{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
-- Copyright © 2014-2015 Lambdatrade AB. All rights reserved.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lambdatrade.Persistence
  ( -- * SQL Monad
    Privilege (..)
  , TransactionLevel
  , setTransactionLevel
  , , SQL (..)
  , unprivileged
  , db
  , db'
  , runSQL
  , readCommitted
  , serializeable
  , repeatableRead
  , runSQL'
  , withSerializeable
  , withReadCommited
  -- * Persistence Helpers
  , checkmarkToBool
  , boolToCheckmark
  , boolCheckmark
  -- * Esqueleto Helpers
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
  , jsonField
  , jsonFieldText
  , jsonFieldUUID
  , array
  , sqlFormatTime
  , deferrConstraints
  , undeferrConstraints
  -- * Uniquenes Constraints
  , Conflict(..)
  , DescribeUnique(..)
  , conflict
  , insertUniqueConflict
  , replaceUniqueConflict
  -- *  Foreign Key Relationships
  , ForeignPair(..)
  , ForeignKey(..)
  , foreignKey
  , foreignKeyR
  , foreignKeyRMaybe
  , onForeignKey
  ) where

import qualified Control.Exception as Ex
import qualified Control.Lens as L
import           Control.Monad.Catch
import           Control.Monad.Reader
import qualified Data.Aeson as Aeson
import           Data.Data
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import           Data.Maybe (catMaybes)
import           Data.Monoid
import           Data.Singletons
import           Data.Singletons.TH
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time
import           Data.Typeable
import           Data.UUID (UUID)
import qualified Data.UUID as UUID
import           Database.Esqueleto as E
import           Database.Esqueleto.Internal.Sql
import qualified Database.Persist as P
import qualified Database.Persist.Sql as P
import           GHC.Generics

--------------------------------------------------------------------------------
-- SQL Monad -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | The Privilege necessary to run an operation
data Privilege = Unprivileged -- ^ Operations that can be run by unprivileged
                              -- users
               | Privileged -- ^ Generally all operations that change data
            deriving (Show, Eq, Ord, Data, Typeable, Generic)

data TransactionLevel = Serializeable
                      | RepeatableRead
                      | ReadCommitted
            deriving (Show, Eq, Ord, Data, Typeable, Generic)

setTransactionLevel :: MonadIO m => TransactionLevel -> ReaderT SqlBackend m ()
setTransactionLevel l = do
    rawExecute ("SET TRANSACTION ISOLATION LEVEL" <> level  l) []
  where
    level Serializeable = "SERIALIZEABLE"
    level RepeatableRead = "REPEATABLE READ"
    level ReadCommitted = "READ COMMITED"

genSingletons [''Privilege, ''TransactionLevel]

-- | An SQL action running in a privilege context @r@
newtype SQL (r :: Privilege) (l :: TransactionLevel)
            a = SQL {unSQL :: ReaderT SqlBackend IO a}
                   deriving (Functor, Applicative, Monad, MonadIO
                            , MonadThrow, MonadCatch)

-- | run an SQL transaction
runSQL :: Sing l -- ^ mode to run the transaction in (see 'serializeable',
                 -- 'repeatableRead' and 'readCommitted')
       -> ConnectionPool
       -> SQL p l a
       -> IO a
runSQL tLevel pool ((SQL m) :: SQL p l a) = flip runSqlPool pool $ do
    setTransactionLevel (fromSing tLevel)
    m

-- | Run the transaction in serializeable mode
serializeable :: Sing 'Serializeable
serializeable = SSerializeable

-- | Run the transaction in repeatable read mode
repeatableRead :: Sing 'RepeatableRead
repeatableRead = SRepeatableRead

-- | Run the transaction in read committed mode
readCommitted :: Sing 'ReadCommitted
readCommitted = SReadCommitted


-- | Like runSQL, but derive the mode from the type of the transaction (if it is
-- monomorphic)
runSQL' :: SingI l =>
           ConnectionPool
        -> SQL p l a
        -> IO a
runSQL' = runSQL sing


-- | Run an unprivileged operation in a privileged context
unprivileged :: SQL Unprivileged l a -> SQL Privileged l a
unprivileged (SQL m) = SQL m

-- | Run a db action in a polymorphic context (i.e. it can be run both in
-- privileged and in unprivileged contexts)
db :: ReaderT SqlBackend IO b -> SQL p l b
db m = do
    con <- SQL $ ask
    liftIO $ runReaderT m con
{-# INLINE db #-}

-- | Run a db action in a privileged context
db' :: ReaderT SqlBackend IO b -> SQL Privileged l b
db' = unprivileged . db
{-# INLINE db' #-}

-- | Annotate an operation as requiring serializability
withSerializeable :: SQL p l a -> SQL p Serializeable a
withSerializeable (SQL m) = SQL m

-- | Annotate an operation as not requiring serializability
withReadCommited :: SQL p ReadCommitted a -> SQL p ReadCommitted a
withReadCommited m = m

--------------------------------------------------------------------------------
-- Persistence Helpers ---------------------------------------------------------
--------------------------------------------------------------------------------

checkmarkToBool :: Checkmark -> Bool
checkmarkToBool Active = True
checkmarkToBool Inactive = False

boolToCheckmark :: Bool -> Checkmark
boolToCheckmark True = Active
boolToCheckmark False = Inactive

boolCheckmark :: L.Iso' Bool Checkmark
boolCheckmark = L.iso boolToCheckmark checkmarkToBool

--------------------------------------------------------------------------------
-- Esqueleto Helpers -----------------------------------------------------------
--------------------------------------------------------------------------------

-- | OR a list of predicates
orL :: Esqueleto query expr backend =>
       [expr (Value Bool)]
    -> expr (Value Bool)
orL [] = val False
orL (p:ps) = List.foldl' (||.) p ps

-- | AND a list of predicates
andL :: Esqueleto query expr backend =>
        [expr (Value Bool)] -> expr (Value Bool)
andL [] = val True
andL (p:ps) = List.foldl' (&&.) p ps

-- | AND a list of predicates (ignoring Nothing values)
andLMb :: Esqueleto query expr backend =>
          [Maybe (expr (Value Bool))]
       -> expr (Value Bool)
andLMb = andL . catMaybes

-- | WHERE on a list of predicates (conjunction)
whereL :: Esqueleto query expr backend => [expr (Value Bool)] -> query ()
whereL [] = return ()
whereL xs = where_ $ andL xs

-- | WHERE on a list of optional predicates (conjunction, ignoring Nothings)
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

--------------------------------------------------------------------------------
-- SQL helpers (Postgres specific) ---------------------------------------------
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

-- | Exception thrown when a data conflict occurs
data Conflict = Conflict { conflictType :: !Text
                           -- ^ The type of the entity producing the context
                           -- (e.g. the name of the entity)
                         , conflictFields :: ![(Text, Text)]
                           -- ^ The fields of the entity that contribute to the
                           -- conflict
                         } deriving (Show, Typeable, Data, Generic)

instance Ex.Exception Conflict

-- | Describe a Uniqueness constraint. Used e.g. to automatically create
-- Congflict exceptions on insertion
class PersistEntity a => DescribeUnique a where
    -- | The type/name of the uniqueness constraint
    uniqueType :: Unique a -> Text
    -- | The fields that constitute the uniqueness constraint
    uniqueFieldNames :: Unique a -> [(Text, Text)]


-- | Throw a conflict exception calculated from a uniqueness constraint.
conflict :: DescribeUnique a => Unique a -> Conflict
conflict descr = Conflict (uniqueType descr) (uniqueFieldNames descr)

-- | Insert a value, throwing a Conflict exception when a uniqueness constraint
-- is violated
insertUniqueConflict :: (DescribeUnique a, PersistEntityBackend a ~ SqlBackend) =>
                        a
                     -> SQL 'Privileged l (Key a)
insertUniqueConflict x = do
    mbCfl <- db $ checkUnique x
    case mbCfl of
     Nothing -> db $ insert x
     Just cfl -> liftIO . Ex.throwIO $ conflict cfl

-- | Replace a value, throwing a Conflict exception when a uniqueness constraint
-- is violated
replaceUniqueConflict :: (Eq a, Eq (Unique a), DescribeUnique a,
                          PersistEntityBackend a ~ SqlBackend) =>
                         Key a
                      -> a
                      -> SQL 'Privileged l ()
replaceUniqueConflict k v = do
    mbCfl <- db $ replaceUnique k v
    case mbCfl of
     Nothing -> return ()
     Just cfl -> liftIO . Ex.throwIO $ conflict cfl

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


-- | Describe a unique, caninical foreign key relationship between entities,
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
