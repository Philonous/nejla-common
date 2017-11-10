-- Copyright © 2014-2015 Lambdatrade AB. All rights reserved.

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module NejlaCommon.Persistence
  ( -- * SQL Monad
    Privilege (..)
  , TransactionLevel(..)
  , setTransactionLevel
  , AppState (..)
  , connection
  , userState
  , askState
  , viewState
  , App (..)
  , unprivileged
  , db
  , db'
  , SqlConfig (..)
  , defaultSqlConfig
  , HasNumRetries(..)
  , HasRetryMinDelay(..)
  , HasRetryMaxDelay(..)
  , HasRetryableErrors(..)
  , HasUseTransactionLevels(..)
  , runApp
  , readCommitted
  , serializable
  , repeatableRead
  , runApp'
  , withLevel
  , withReadCommitted
  , withRepeatableRead
  , withSerializable
  , forkApp
  -- * Persistence Helpers
  , checkmarkToBool
  , boolToCheckmark
  , boolCheckmark
  -- * Esqueleto Helpers
  --
  -- ** Functions that work on lists
  , orL
  , andL
  , andLMb
  , whereL
  , whereLMb
  , onL
  , onLMb
  , mbEq
  , offsetLimit
  -- * SQL helpers
  , SV
  , SVM
  , jsonField
  , jsonFieldText
  , jsonFieldUUID
  , array
  , emptyArray
  , arrayAgg'
  , sqlFormatTime
  , deferrConstraints
  , undeferrConstraints
  -- * Not Found
  , notFound
  , getNotFound
  , getByNotFound
  , getByNotFound'
  , fromMaybeNotFound
  , fromListNotFound
  -- * Uniquenes Constraints
  , PersistError(..)
  , responseCode
  , DescribeUnique(..)
  , conflict
  , insertUniqueConflict
  , replaceUniqueConflict
  -- *  Foreign Key Relationships
  , ForeignPair(..)
  , ForeignKey(..)
  , foreignKey
  , foreignKeyL
  , foreignKeyR
  , foreignKeyLR
  , foreignKeyRMaybe
  , onForeignKey
  -- * Human-readable IDs
  , mkRandomHrID
  , mkUniqueRandomHrID
  ) where

import           Control.Concurrent
import qualified Control.Lens                      as L
import           Control.Lens.TH
import           Control.Monad.Base
import qualified Control.Monad.Catch               as Ex
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import qualified Data.Aeson                        as Aeson
import           Data.Aeson                        hiding (Value)
import           Data.ByteString                   (ByteString)
import           Data.Data
import           Data.Default
import qualified Data.Foldable                     as Foldable
import           Data.IORef
import qualified Data.List                         as List
import           Data.Maybe                        (catMaybes, maybeToList)
import           Data.Monoid
import           Data.Singletons
import           Data.Singletons.TH
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import qualified Data.Text.Encoding                as Text
import qualified Data.Text.Encoding.Error          as Text
import           Data.Time
import           Data.UUID                         (UUID)
import           Database.Esqueleto                as E
import           Database.Esqueleto.Internal.Sql
import qualified Database.Esqueleto.PostgreSQL     as Postgres
import qualified Database.PostgreSQL.Simple        as Postgres
import           Database.PostgreSQL.Simple.Errors
import           GHC.Generics
import           System.Random
import           System.Random.Shuffle

--------------------------------------------------------------------------------
-- SQL Monad -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | The Privilege necessary to run an operation
data Privilege = Unprivileged -- ^ Operations that can be run by unprivileged
                              -- users
               | Privileged -- ^ Generally all operations that change data
            deriving (Show, Eq, Ord, Data, Typeable, Generic)

-- | Transaction Level to run a transaction at. Please see your databases
-- documentation for the semantics
data TransactionLevel = ReadCommitted
                      | RepeatableRead
                      | Serializable
            deriving (Show, Eq, Ord, Data, Typeable, Generic)

-- | Set the transaction level of the current transaction. This operation has to
-- be run at the beginning of the transaction
setTransactionLevel :: MonadIO m => TransactionLevel -> ReaderT SqlBackend m ()
setTransactionLevel l = do
    rawExecute ("SET TRANSACTION ISOLATION LEVEL " <> level  l) []
  where
    level Serializable = "SERIALIZABLE"
    level RepeatableRead = "REPEATABLE READ"
    level ReadCommitted = "READ COMMITTED"

genSingletons [''Privilege, ''TransactionLevel]
promoteEqInstances [''Privilege, ''TransactionLevel]
promoteOrdInstances  [''Privilege, ''TransactionLevel]

-- | Application state
data AppState st = AppState { appStateConnection :: !SqlBackend
                            -- ^ The database connection to work with
                            , appStateUserState  :: !st
                            -- ^ User state
                            } deriving ( Typeable, Generic)

L.makeLensesWith L.camelCaseFields ''AppState

-- | Get the Application state
askState :: App st r l st
askState = App $ L.view userState

-- | Access a field of the user state using a lens
--
-- Example:
--
-- Given a user state of
--
-- > data UserState = UserState { userStateFoo :: Bool }
-- > makeLensesWith camelCaseFields ''UserState
--
-- We can access the foo field like this
--
-- > fooValue <- viewState foo
--
viewState :: L.Getting a st a -> App st r l a
viewState f = App . L.view $ userState . f

-- | An App action running in a privilege context @r@ with transaction level @l@
newtype App (st :: *) (r :: Privilege) (l :: TransactionLevel)
            a = App {unApp :: ReaderT (AppState st) IO a}
                   deriving (Functor, Applicative, Monad, MonadIO
                            , Ex.MonadThrow, Ex.MonadCatch, MonadBase IO)

instance MonadBaseControl IO (App st r l) where
  type StM (App st r l) a = a
  liftBaseWith f = App . ReaderT $ \st ->
                    f (\(App m) -> runReaderT m st )
  restoreM       = return
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}

instance MonadLogger (App st r l) where
  monadLoggerLog loc logSource logLevel logStr = do
    -- We use the log function stored in the SqlBackend
    con <- App $ L.view connection
    liftIO $ connLogFunc con loc logSource logLevel $ toLogStr logStr


data SqlConfig = SqlConfig { -- | How often to retry the transaction (0 to
                             -- disable retries completely)
                             --
                             -- Default: 3
                             sqlConfigNumRetries :: !Int
                             -- | Minimum delay in µs before retrying (delay
                             -- will be chosen from a uniform distribution
                             -- between min and max)
                             --
                             -- Default 0
                           , sqlConfigRetryMinDelay :: !Int
                             -- | Maximum delay in µs before retrying
                             --
                             -- Default: 100000 (100ms)
                           , sqlConfigRetryMaxDelay :: !Int
-- | Only retry on the those error codes
--
-- See <https://www.postgresql.org/docs/9.5/static/errcodes-appendix.html> for a
-- list of possible codes
--
-- Default:
-- [ "40001" -- serialization_failure
-- , "40P01" -- deadlock_detected
-- ]
                           , sqlConfigRetryableErrors :: ![ByteString]
                           , sqlConfigUseTransactionLevels :: !Bool
                           }

makeLensesWith camelCaseFields ''SqlConfig

defaultSqlConfig :: SqlConfig
defaultSqlConfig = SqlConfig { sqlConfigNumRetries = 3
                             , sqlConfigRetryMinDelay = 0 -- 0 ms
                             , sqlConfigRetryMaxDelay = 100000 -- 0 ms
                             , sqlConfigRetryableErrors
                               =  [ "40001" -- serialization_failure
                                  , "40P01" -- deadlock_detected
                                  ]
                             , sqlConfigUseTransactionLevels = True
                             }

instance Default SqlConfig where
  def = defaultSqlConfig

-- | run an App transaction
runApp :: Sing l -- ^ mode to run the transaction in (see 'serializable',
                 -- 'repeatableRead' and 'readCommitted')
       -> SqlConfig
       -> ConnectionPool -- ^ Database connection pool to use
       -> st -- ^ User state to pass along
       -> App st p l a
       -> IO a
runApp tLevel conf pool ust ((App m) :: App st p l a) =
  flip runSqlPool pool $ do
    when (conf L.^. useTransactionLevels) $
      setTransactionLevel (fromSing tLevel)
    con <- ask
    let st = AppState { appStateConnection = con
                      , appStateUserState = ust
                      }
    liftIO $ Ex.catch (go con st $ conf L.^. numRetries) $
      \e -> case constraintViolation e of
              Just (ForeignKeyViolation table constr) ->
                Ex.throwM (ForeignKey (utf8 table) (utf8 constr))
              Just (CheckViolation relation constr) ->
                Ex.throwM (ForeignKey (utf8 relation) (utf8 constr))
              Just (NotNullViolation column) ->
                Ex.throwM (ForeignKey ("not null") (utf8 column))
              Just (UniqueViolation column) ->
                Ex.throwM (Conflict (utf8 column) [])
              _ -> Ex.throwM $ DBError (Ex.SomeException e)
  where
    utf8 = Text.decodeUtf8With Text.lenientDecode
    go con st retries = do
        Ex.catch (liftIO $ runReaderT m st) $ \e -> do
          runReaderT transactionUndo con
          case Postgres.sqlState e `elem` (conf L.^. retryableErrors)
               && retries > 0
            of
            True -> do
              delay <- randomRIO ( conf L.^. retryMinDelay
                                 , conf L.^. retryMaxDelay)
              threadDelay delay
              go con st (retries -1)
            False -> Ex.throwM e

-- | Run the transaction in serializable mode
serializable :: Sing 'Serializable
serializable = SSerializable

-- | Run the transaction in repeatable read mode
repeatableRead :: Sing 'RepeatableRead
repeatableRead = SRepeatableRead

-- | Run the transaction in read committed mode
readCommitted :: Sing 'ReadCommitted
readCommitted = SReadCommitted


-- | Like runApp, but derive the mode from the type of the transaction (if it is
-- monomorphic)
runApp' :: SingI l =>
           SqlConfig
        -> ConnectionPool
        -> st
        -> App st p l a
        -> IO a
runApp' = runApp sing


-- | Run any operation in a privileged context
unprivileged :: App st p l a -> App st 'Privileged l a
unprivileged (App m) = App m

-- | Run a db action in an unprivileged context
db :: ReaderT SqlBackend IO b -> App st 'Unprivileged 'ReadCommitted b
db m = do
    con <- App $ L.view connection
    liftIO $ runReaderT m con
{-# INLINE db #-}

-- | Run a db action in a privileged context
db' :: ReaderT SqlBackend IO b -> App st 'Privileged 'ReadCommitted b
db' = unprivileged . db
{-# INLINE db' #-}

-- | Run a lower-loeveled action in a higher-leveled context
withLevel :: ((newLevel :<= oldLevel) ~ 'True) =>
             App st p newLevel a
          -> App st p oldLevel a
withLevel (App m) = App m

-- | Annotate or upgrade an operation as working in Read Committed mode
withReadCommitted :: App st p 'ReadCommitted a
                  -> App st p 'ReadCommitted a
withReadCommitted (App m) = (App m)

-- | Annotate or upgrade an operation as requiring Repeatable Read
withRepeatableRead :: ((l :<= 'RepeatableRead) ~ 'True) =>
                      App st p l a
                   -> App st p 'RepeatableRead a
withRepeatableRead (App m) = App m

-- | Annotate or upgrade an operation as requiring Serializable
withSerializable :: App st p l a -> App st p 'Serializable a
withSerializable (App m) = App m

-- | Run an app action in a new haskell thread
forkApp :: App st p r () -> App st p r ()
forkApp (App m) = do
  st <- App ask
  _ <- liftIO . forkIO $ runReaderT m st
  return ()

--------------------------------------------------------------------------------
-- Errors ----------------------------------------------------------------------
--------------------------------------------------------------------------------

data PersistError = EntityNotFound !Text !Text -- Entity type and name
                  | Conflict !Text ![(Text, Text)] -- entity type and fields
                  | JSONDeserializationError !Text
                  | ValueBound !Text
                  | Policy !Text
                  | ForeignKey !Text !Text -- Table and field
                  | Check !Text !Text
                  | DBError Ex.SomeException
                  deriving (Show, Typeable, Generic)

-- | Operator for setting text-valued JSON object fields (overloaded strings
-- breaks type inference for string literals)
(..=) :: Text -> Text -> (Text, Aeson.Value)
(..=) = (.=)

instance ToJSON PersistError where
    toJSON (Conflict entity fields) =
        object [ "error" ..= "conflict"
               , "entity" ..= entity
               , "fields" .= (fieldToJSON <$> fields)
               ]
      where
        fieldToJSON (fieldname, value') =
            object [ "field" ..= fieldname
                   , "value" ..= value'
                   ]
    toJSON (JSONDeserializationError e) =
        object [ "error" ..= "deserialization error"
               , "message" .= e
               ]
    toJSON (ValueBound e) = object [ "error" ..= "out of bounds"
                                   , "message" .= e
                                   ]
    toJSON (Policy e) = object [ "error" ..= "policy violation"
                               , "message" .= e
                               ]
    toJSON (EntityNotFound tp v) =
        object [ "error" ..= "not found"
               , "type" .= tp
               , "entity" .= v
               ]
    toJSON (ForeignKey table field) =
        object [ "error" ..= "foreign constraint"
               , "table" ..= table
               , "field" ..= field
               ]
    toJSON (Check table constraint) =
        object [ "error" ..= "check constraint"
               , "table" ..= table
               , "constraint" ..= constraint
               ]
    toJSON (DBError e) =
        object [ "error" ..= "database error"
               , "value" ..= Text.pack (show e)
               ]

-- | Response codes for defined errors
responseCode :: PersistError -> Int
responseCode Conflict{}                 = 409
responseCode JSONDeserializationError{} = 400
responseCode ValueBound{}               = 400
responseCode Policy{}                   = 403
responseCode EntityNotFound{}           = 404
responseCode ForeignKey{}               = 409
responseCode Check{}                    = 409
responseCode DBError{}                  = 500

instance Ex.Exception PersistError

--------------------------------------------------------------------------------
-- Persistence Helpers ---------------------------------------------------------
--------------------------------------------------------------------------------

-- | Convert Checkmark to bool ('Active' to 'True'', 'Inactive' to 'False')
checkmarkToBool :: Checkmark -> Bool
checkmarkToBool Active = True
checkmarkToBool Inactive = False

-- | Convert Boolean value to Checkmark ('True' to 'Active', 'False' to
-- 'Inactive')
boolToCheckmark :: Bool -> Checkmark
boolToCheckmark True = Active
boolToCheckmark False = Inactive

-- | An Isomorphism between Booleans and Checkmarks
boolCheckmark :: L.Iso' Bool Checkmark
boolCheckmark = L.iso boolToCheckmark checkmarkToBool

--------------------------------------------------------------------------------
-- Esqueleto Helpers -----------------------------------------------------------
--------------------------------------------------------------------------------

-- | @OR@ a list of predicates. An empty list becomes 'False'
orL :: Esqueleto query expr backend =>
       [expr (Value Bool)]
    -> expr (Value Bool)
orL [] = val False
orL (p:ps) = List.foldl' (||.) p ps

-- | @AND@ a list of predicates. An Empty list becomes 'True'
andL :: Esqueleto query expr backend =>
        [expr (Value Bool)] -> expr (Value Bool)
andL [] = val True
andL (p:ps) = List.foldl' (&&.) p ps

-- | @AND@ a list of predicates (ignoring Nothing values).
andLMb :: Esqueleto query expr backend =>
          [Maybe (expr (Value Bool))]
       -> expr (Value Bool)
andLMb = andL . catMaybes

-- | @WHERE@ on a list of predicates (conjunction)
whereL :: Esqueleto query expr backend => [expr (Value Bool)] -> query ()
whereL [] = return ()
whereL xs = where_ $ andL xs

-- | WHERE on a list of optional predicates (conjunction, ignoring 'Nothing''s)
whereLMb :: Esqueleto query expr backend =>
            [Maybe (expr (Value Bool))] -> query ()
whereLMb = whereL . catMaybes

-- | ON on a list of predicates.
onL :: Esqueleto query expr backend =>
       [expr (Value Bool)]
    -> query ()
-- ON will be preserved even if the list is empty. This is important.
onL = on . andL

-- | ON on a list of optional predicates, ignoring Nothings
onLMb :: Esqueleto query expr backend =>
         [Maybe (expr (Value Bool))]
      -> query ()
onLMb = onL . catMaybes

-- | Set offset and limit for the query.
offsetLimit  :: (Esqueleto m expr backend ) =>
                Maybe Int
             -> Maybe Int
             -> m ()
offsetLimit os l = do
    Foldable.forM_ os $ offset . fromIntegral
    Foldable.forM_ l $ limit . fromIntegral
    return ()



type SV a  = SqlExpr (Entity a)
type SVM a = SqlExpr (Maybe (Entity a))

emptyArray :: SqlExpr (Value [a])
emptyArray = unsafeSqlValue "'{}'"

arrayRemoveNull :: SqlExpr (Value [Maybe a]) -> SqlExpr (Value [a])
arrayRemoveNull x = unsafeSqlFunction "array_remove" (x, unsafeSqlValue "NULL")

arrayAgg' :: PersistField [a] => SqlExpr (Value (Maybe a)) -> SqlExpr (Value [a])
arrayAgg' =  arrayRemoveNull . Postgres.arrayAgg

--------------------------------------------------------------------------------
-- App helpers (Postgres specific) ---------------------------------------------
--------------------------------------------------------------------------------

-- | Class of Haskell types that are represented as json in postgres
class SqlJSON a where

instance SqlJSON Aeson.Value

infixl 5 `jsonField`, `jsonFieldText`

-- | postgresql (->) (object indexing) function
jsonField :: (SqlJSON a, SqlJSON b) =>
             SqlExpr (Value a)
          -> SqlExpr (Value Text)
          -> SqlExpr (Value b)
jsonField = unsafeSqlBinOp "->"

-- | postgresql (->) (object indexing) function
jsonFieldText :: (SqlJSON a) =>
                 SqlExpr (Value a)
              -> SqlExpr (Value Text)
              -> SqlExpr (Value (Maybe Text))
jsonFieldText = unsafeSqlBinOp "->>"

-- | postgresql (->) (object indexing) function
jsonFieldUUID :: SqlJSON a =>
                 SqlExpr (Value a)
              -> SqlExpr (Value Text)
              -> SqlExpr (Value (Maybe UUID))
jsonFieldUUID v i = unsafeSqlBinOp "::"
                      (jsonFieldText v i)
                      (unsafeSqlValue "uuid")

-- | Create a singleton array (postgres)
array :: SqlExpr (Value a) -> SqlExpr (Value [a])
array = unsafeSqlFunction "array"

-- | Format a time value with a format string
sqlFormatTime :: SqlExpr (Value (Maybe UTCTime))
           -> SqlExpr (Value Text)
           -> SqlExpr (Value (Maybe Text))
sqlFormatTime time formatstring = unsafeSqlFunction "to_char" (time, formatstring)

-- | Set constraints to DEFERRED
deferrConstraints :: MonadIO m => ReaderT SqlBackend m ()
deferrConstraints = rawExecute "SET CONSTRAINTS ALL DEFERRED;" []

-- | Set constraints to IMMEDIATE
undeferrConstraints :: MonadIO m => ReaderT SqlBackend m ()
undeferrConstraints = rawExecute "SET CONSTRAINTS ALL IMMEDIATE;" []

--------------------------------------------------------------------------------
-- Uniquenes Constraints -------------------------------------------------------
--------------------------------------------------------------------------------

-- -- | Exception thrown when a data conflict occurs
-- data Conflict = Conflict { conflictType :: !Text
--                            -- ^ The type of the entity producing the context
--                            -- (e.g. the name of the entity)
--                          , conflictFields :: ![(Text, Text)]
--                            -- ^ The fields of the entity that contribute to the
--                            -- conflict
--                          } deriving (Show, Typeable, Data, Generic)

-- instance Ex.Exception Conflict

-- | Describe a Uniqueness constraint. Used e.g. to automatically create
-- Conflict exceptions on insertion
class PersistEntity a => DescribeUnique a where
    -- | The type/name of the uniqueness constraint
    uniqueType :: Unique a -> Text
    -- | The fields that constitute the uniqueness constraint
    uniqueFieldNames :: Unique a -> [(Text, Text)]


-- | Throw a conflict exception calculated from a uniqueness constraint.
conflict :: DescribeUnique a => Unique a -> PersistError
conflict descr = Conflict (uniqueType descr) (uniqueFieldNames descr)

-- | Insert a value, throwing a Conflict exception when a uniqueness constraint
-- is violated
insertUniqueConflict :: (DescribeUnique a, PersistEntityBackend a ~ SqlBackend) =>
                        a
                     -> App st 'Privileged 'ReadCommitted (Key a)
insertUniqueConflict x = do
    mbCfl <- db' $ checkUnique x
    case mbCfl of
     Nothing -> db' $ insert x
     Just cfl -> liftIO . Ex.throwM $ conflict cfl

-- | Replace a value, throwing a Conflict exception when a uniqueness constraint
-- is violated
replaceUniqueConflict :: (Eq a, Eq (Unique a), DescribeUnique a,
                          PersistEntityBackend a ~ SqlBackend) =>
                         Key a
                      -> a
                      -> App st 'Privileged 'ReadCommitted ()
replaceUniqueConflict k v = do
    mbCfl <- db' $ replaceUnique k v
    case mbCfl of
     Nothing -> return ()
     Just cfl -> liftIO . Ex.throwM $ conflict cfl

--------------------------------------------------------------------------------
-- Getters with possible 404s --------------------------------------------------
--------------------------------------------------------------------------------


notFound :: (MonadIO m, Show a) => Text -> a -> m b
notFound entType entName =
    liftIO . Ex.throwM $ EntityNotFound entType (Text.pack $ show entName)

getByNotFound :: (PersistEntity val, Show a,
                  PersistEntityBackend val ~ SqlBackend) =>
                 Text -> a -> Unique val -> App st 'Unprivileged 'ReadCommitted (Entity val)
getByNotFound entType entName p = do
    g <- db $ getBy p
    case g of
        Nothing -> notFound entType entName
        Just e -> return e

getByNotFound' :: (DescribeUnique val, PersistEntityBackend val ~ SqlBackend) =>
                  Unique val
               -> App st 'Unprivileged 'ReadCommitted (Entity val)
getByNotFound' p = getByNotFound (uniqueType p) (uniqueFieldNames p) p

getNotFound :: (PersistEntity b, PersistEntityBackend b ~ SqlBackend) =>
               Text -> Key b -> App st 'Unprivileged 'ReadCommitted b
getNotFound entType p = do
    g <- db $ get p
    case g of
        Nothing -> notFound entType p
        Just e -> return e

-- | Get the first element from a List of results or throw EntityNotFound if the
-- list is Empty
--
-- Throws: ApiFailure
fromListNotFound :: (Ex.MonadThrow m, Show a) => Text -> a -> [b] -> m b
fromListNotFound entType entName [] =
  Ex.throwM $ EntityNotFound entType (Text.pack $ show entName)
fromListNotFound _entType _entName (x:_) = return x

fromMaybeNotFound :: (Show a, Ex.MonadThrow m) => Text -> a -> Maybe b -> m b
fromMaybeNotFound entType entName item' =
  fromListNotFound entType entName (maybeToList item')


--------------------------------------------------------------------------------
-- Foreign Key Relationships ---------------------------------------------------
--------------------------------------------------------------------------------

-- | Describe a Pair of keys that form a foreign key relationship.
--
-- The first element is the entity field that holds the foreign key. The second
-- element is the entity field that holds the references primary key. Type of
-- primary key and foreign key have to coincide.
--
-- For example, give the following Entity definition:
--
-- @
-- Employee
--     num Int
--     Primary numid
--     name Text
--
-- Team
--     teamId Int
--     employee Int
--     Foreign Employee fkEmployee employee
-- @
--
-- The Following ForeignPair would capture the foreign key relationship
--
-- @ForeignPair TeamEmployee EmployeeNum@

data ForeignPair a b where
    ForeignPair :: (PersistEntity a, PersistEntity b, PersistField f) =>
                    EntityField a f
                 -> EntityField b f
                 -> ForeignPair a b


-- | Describe a unique, canonical foreign key relationship between entities,
-- . For example, given the entity definitions from 'ForeignPair', there is
-- exactly one foreign key relationship between Employee and Team, so we can capture it in a type class:
--
-- @
-- instance ForeignKey Team Employee where
--     foreignPair = ForeignPair TeamEmployee EmployeeNum
-- @
--
-- Note that the entity with the foreign key is the _first_ parameter of the
-- type class, the target entity the second
class ForeignKey a b where
    foreignPair :: ForeignPair a b

-- | A foreign key constraint between two entities.
--
-- Example:
--
-- @
-- from $ \(team, employee) ->
--   where_ (foreignKey team employee)
--   [...]
-- @
foreignKey :: (ForeignKey a b, Esqueleto query expr backend) =>
              expr (Entity a) -> expr (Entity b) -> expr (Value Bool)
foreignKey x y =
    case foreignPair of
     (ForeignPair xk yk) -> x ^. xk ==. y ^. yk

-- | Similar to foreignKey, except that the foreign key can be nullable
-- . However, it will only match if the key is actually set
foreignKeyR  :: (ForeignKey a b, Esqueleto query expr backend) =>
               expr (Entity a) -> expr (Maybe (Entity b)) -> expr (Value Bool)
foreignKeyR x y =
    case foreignPair of
     (ForeignPair xk yk) -> just (x ^. xk) ==. y ?. yk

-- | Similar to foreignKey, except that the foreign key can be nullable
-- . However, it will only match if the key is actually set
foreignKeyL  :: (ForeignKey a b, Esqueleto query expr backend) =>
               expr (Maybe (Entity a)) -> expr (Entity b) -> expr (Value Bool)
foreignKeyL x y =
    case foreignPair of
     (ForeignPair xk yk) -> (x ?. xk) ==. just (y ^. yk)

-- | Similar to foreignKey, except that the foreign key can be nullable
-- . However, it will only match if the key is actually set
foreignKeyLR  :: (ForeignKey a b, Esqueleto query expr backend) =>
               expr (Maybe (Entity a)) -> expr (Maybe (Entity b)) -> expr (Value Bool)
foreignKeyLR x y =
    case foreignPair of
     (ForeignPair xk yk) -> (x ?. xk) ==. (y ?. yk)

-- | Compare an entity field to a Haskell 'Maybe' value. NOTE: Simply using
-- @==.@ does __not__ work! @NULL ==. Nothing@ will evaluate to @NULL@!
mbEq :: (PersistField typ, Esqueleto query expr backend) =>
        expr (Value (Maybe typ))
     -> Maybe typ -> expr (Value Bool)
mbEq v1 Nothing  = isNothing v1
mbEq v1 (Just v2)  = v1 ==. just (val v2)


-- | Like foreignKeyR, but also matches if the foreign key field is NULL
foreignKeyRMaybe :: (Esqueleto query expr backend, ForeignKey a b) =>
                    expr (Entity a)
                 -> expr (Maybe (Entity b))
                 -> expr (Value Bool)
foreignKeyRMaybe x y =
    case foreignPair of
     (ForeignPair xk yk) ->
         orL [ isNothing (y ?. yk)
             , just (x ^. xk) ==. y ?. yk
             ]
-- | ON for a foreign key pair
--
-- @onForeignKey a b === on (foreignKey a b)@
--
-- Example:
--
-- @
-- from $ \(team \`InnerJoin\` employee) ->
--   onForeignKey team employee
-- @
onForeignKey :: (Esqueleto query expr backend, ForeignKey a b) =>
                expr (Entity a) -> expr (Entity b) -> query ()
onForeignKey x y = on $ foreignKey x y

--------------------------------------------------------------------------------
-- ID generation ---------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Digits for human-readable ID generation. 0 and 1 are not included to avoid
-- confusion with I and O respectively
hrIDDigits :: [Char]
hrIDDigits = "2345679"

-- | Letters for human-readable ID generation. Vovels are not included to avoid
-- accidentally spelling profanities.
hrIDChars :: [Char]
hrIDChars = "CDFGHJKLMNPQRSTVWXYZ" ++ hrIDDigits

-- | Generate a random human-readable ID.
mkRandomHrID :: Int -> IO Text
mkRandomHrID len = do
    chars <- replicateM (len - 1) $ selectOne hrIDChars
    digit <- selectOne hrIDDigits
    Text.pack <$> shuffleM (digit : chars)
  where
    selectOne xs = do
        i <- randomRIO (0, length xs - 1)
        return $ xs !! i

-- | Generate a random human-readable ID and make sure it doesn't exist in the database
mkUniqueRandomHrID :: ( PersistField typ
                      , PersistEntity val
                      , PersistEntityBackend val ~ SqlBackend) =>
                      (Text -> typ)
                   -> Int
                   -> EntityField val typ
                   -> App st 'Unprivileged 'ReadCommitted typ
mkUniqueRandomHrID fromCandidate len field = do
    candidate <- liftIO $ mkRandomHrID len
    [Value rows] <- db . select . E.from $ \o -> do
        where_ $ o ^. field ==. val (fromCandidate candidate)
        return $ countRows
    if (rows :: Rational) > 0
        then mkUniqueRandomHrID fromCandidate len field
        else return $ fromCandidate candidate
