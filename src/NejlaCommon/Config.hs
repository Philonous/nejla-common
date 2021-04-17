{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module NejlaCommon.Config
  ( getConfGenericMaybe
  , getConfGeneric
  , getConf'
  , getConfMaybe'
  , getConf
  , getConfMaybe
  , getConfBool
  , getConfBoolMaybe
  , loadConf
  , Conf.Config
  , Conf.Name
  ) where

import           Control.Monad.Logger
import           Control.Monad.Trans
import qualified Data.Char as Char
import qualified Data.Configurator as Conf
import qualified Data.Configurator.Types as Conf
import           Data.Maybe (catMaybes)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as Text
import           System.Environment
import qualified System.Exit as Exit

import           NejlaCommon.Helpers

-- | Generic function to retrieve a configuration option.
--
-- Returns 'Nothing' if the option could not be found
getConfGenericMaybe :: (MonadLogger m, MonadIO m, Conf.Configured a) =>
                       (String -> Maybe a) -- ^ Function to convert from string
                    -> String -- ^ Environment variable to read option from
                    -> Conf.Name -- ^ Config name to read option from
                    -> Conf.Config -- ^ Config object to read option from
                    -> m (Maybe a)
getConfGenericMaybe fromString env confName conf = do
    mbConfVal <- liftIO $ Conf.lookup conf confName
    case mbConfVal of
     Just v -> return $ Just v
     Nothing -> do
       mbVal <- liftIO $ lookupEnv env
       return $ fromString =<< mbVal


-- | Generic function to retrieve a configuration option
getConfGeneric :: (MonadLogger m, MonadIO m, Conf.Configured a) =>
                  (String -> Maybe a) -- ^ Function to convert from string
               -> String -- ^ Environment variable to read option from
               -> Conf.Name -- ^ Config name to read option from
               -> Either Text a -- ^ Default value to use when option could not
                                -- be found or a description of the object to
                                -- display as an error message
               -> Conf.Config -- ^ Config object to read option from
               -> m a
getConfGeneric fromString env confName mbDefault conf = do
    mbC <- getConfGenericMaybe fromString env confName conf
    case mbC of
     Nothing -> case mbDefault of
                 Right d -> return d
                 Left e -> do
                     $logError $ "Configuration of `"  <> e <> "` is required. \n"
                                 <> " Set environment variable "
                                 <> Text.pack env <>
                                 " or configuration variable " <> confName <> "."
                     liftIO $ Exit.exitFailure
     Just v -> return v

-- | Get configuration option based on Read instance
--
-- /NB/: Do NOT use this for boolean options, use 'getConfBool' instead
getConf' :: (Conf.Configured a, MonadLogger m, MonadIO m, Read a) =>
            String -- ^ Environment variable to read option from
         -> Conf.Name -- ^ Config name to read option from
         -> Either Text a -- ^ Default value to use when option could not be
                          -- found or a description of the object to display as
                          -- an error message
         -> Conf.Config
         -> m a
getConf' = getConfGeneric safeRead

-- | Get configuration option based on Read instance
--
-- /NB/: Do NOT use this for boolean options, use 'getConfBoolMaybe' instead
getConfMaybe' :: (Conf.Configured a, MonadLogger m, MonadIO m, Read a) =>
                 String -- ^ Environment variable to read option from
              -> Conf.Name -- ^ Config name to read option from
              -> Conf.Config
              -> m (Maybe a)
getConfMaybe' = getConfGenericMaybe safeRead

-- | Get text config option
getConf :: (MonadLogger m, MonadIO m) =>
            String -- ^ Environment variable to read option from
        -> Conf.Name -- ^ Config name to read option from
        -> Either Text Text -- ^ Default value to use when option could not be
                            -- found or a description of the object to display
                            -- as an error message
        -> Conf.Config
        -> m Text
getConf = getConfGeneric (Just . Text.pack)

-- | Get text config option
getConfMaybe :: (MonadLogger m, MonadIO m) =>
                String -- ^ Environment variable to read option from
             -> Conf.Name -- ^ Config name to read option from
             -> Conf.Config
             -> m (Maybe Text)
getConfMaybe = getConfGenericMaybe (Just . Text.pack)


-- | Get boolean config option
getConfBool :: (MonadIO m, MonadLogger m) =>
               String -- ^ Environment variable to read option from
            -> Conf.Name -- ^ Config name to read option from
            -> Either Text Bool -- ^ Default value to use when option could not
                                -- be found or a description of the object to
                                -- display as an error message
            -> Conf.Config
            -> m Bool
getConfBool = getConfGeneric parseBool
  where
    parseBool str | map Char.toLower str == "true" = Just True
                  | map Char.toLower str == "false" = Just False
                  | otherwise = Nothing

-- | Get boolean config option
getConfBoolMaybe :: (MonadIO m, MonadLogger m) =>
                    String -- ^ Environment variable to read option from
                 -> Conf.Name -- ^ Config name to read option from
                 -> Conf.Config
                 -> m (Maybe  Bool)
getConfBoolMaybe = getConfGenericMaybe parseBool
  where
    parseBool str | map Char.toLower str == "true" = Just True
                  | map Char.toLower str == "false" = Just False
                  | otherwise = Nothing


-- | Load configuration file.
--
-- The file will be read from the location set in CONF_PATH
-- or /data/appname.conf
loadConf :: MonadIO m =>
            String -- ^ config file name (appname)
         -> m Conf.Config
loadConf appname = liftIO $ do
    mbConfPath <- lookupEnv "CONF_PATH"
    Conf.load $ catMaybes [ Just . Conf.Optional $
                             "/data/" <> appname <> ".conf"
                          , Conf.Required <$> mbConfPath
                          ]
