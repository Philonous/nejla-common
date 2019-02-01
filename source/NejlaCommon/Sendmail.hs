{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module NejlaCommon.Sendmail where

import           Control.Exception       as Ex (ErrorCall(..))
import           Control.Lens            hiding (from)
import           Control.Monad.Catch     as Ex
import           Control.Monad.Logger
import           Control.Monad.Trans
import           Data.Aeson              as Aeson
import qualified Data.Configurator       as Conf
import qualified Data.Configurator.Types as Conf
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.Lazy          as LText
import qualified Data.Text.Lazy.IO       as LText
import qualified Network.Mail.Mime       as Mail
import qualified System.Exit             as Exit
import qualified Text.Microstache        as Mustache

import           NejlaCommon.Config
import           NejlaCommon.Helpers     (mustache)

data SendmailConfig = SendmailConfig
  { sendmailConfigPath :: FilePath
  , sendmailConfigArguments :: [String]
  } deriving Show

defaultSendmailConfig :: SendmailConfig
defaultSendmailConfig = SendmailConfig
  { sendmailConfigPath = "/usr/bin/sendmail"
  , sendmailConfigArguments = ["-t"]
  }

makeLensesWith camelCaseFields ''SendmailConfig

data EmailConfig = EmailConfig
  { emailConfigFrom :: Mail.Address
  , emailConfigHost :: Text
  , emailConfigPort :: Int
  , emailConfigUser :: Text
  , emailConfigPassword :: Text
  , emailConfigTls :: Bool
  , emailConfigAuth :: Bool
  , emailConfigSendmail :: SendmailConfig
  } deriving Show

makeLensesWith camelCaseFields ''EmailConfig

-- | Get email settings from configuration environment and write /etc/msmtprc
-- file. This is meant for docker containers where overwriting the global
-- msmtprc file is acceptable
setEmailConf :: (MonadIO m, MonadLogger m) => Conf.Config -> m (Maybe EmailConfig)
setEmailConf conf =
  getConfMaybe "EMAIL_FROM" "email.from" conf >>= \case
    Nothing -> return Nothing
    Just fromAddress -> do
      fromName <- getConfMaybe "EMAIL_FROM_NAME" "email.fromName" conf
      let emailConfigFrom = Mail.Address fromName fromAddress
      emailConfigHost <-
        getConf "EMAIL_SMTP" "email.smtp" (Left "email host") conf
      emailConfigPort <-
        getConf' "EMAIL_PORT" "email.port" (Right 25) conf
      emailConfigUser <-
        getConf "EMAIL_USER" "email.user" (Left "email user") conf
      emailConfigPassword <-
        getConf "EMAIL_PASSWORD" "email.password" (Left "email password") conf
      sendmailCommand <-
        getConfMaybe "SENDMAIL_PROGRAM" "email.sendmail-program" conf
      emailConfigTls <- getConfBool "EMAIL_TLS" "email.tls" (Right True) conf
      emailConfigAuth <- getConfBool "EMAIL_AUTH" "email.auth" (Right True) conf
      let emailConfigSendmail =
            case sendmailCommand of
              Just cmd
                | (prg:args) <- Text.splitOn " " cmd
                , not (Text.null prg) ->
                  SendmailConfig
                  { sendmailConfigPath = Text.unpack $ prg
                  , sendmailConfigArguments = Text.unpack <$> args
                  }
              _ -> defaultSendmailConfig
          ecfg = EmailConfig {..}
      writeMsmtprc ecfg emailConfigTls emailConfigAuth
      return $ Just ecfg

-- | Render email settings and write to /etc/msmtprc. This is meant for docker
-- containers where overwriting the global msmtprc file is acceptable
writeMsmtprc ::
     (MonadLogger m, MonadIO m) => EmailConfig -> Bool -> Bool -> m ()
writeMsmtprc emailConfig tls auth = do
  txt <- renderMsmtprc emailConfig tls auth
  liftIO $ LText.writeFile "/etc/msmtprc" txt

-- | Render an msmtprc.conf file from email config
renderMsmtprc ::
     (MonadIO m, MonadLogger m)
  => EmailConfig
  -> Bool -- ^ Use TLS
  -> Bool -- ^ Use authentication
  -> m LText.Text
renderMsmtprc cfg tls auth =
  let Mail.Address _ fromAddress = (cfg ^. from)
      dt = Aeson.object [ "host" Aeson..= (cfg ^. host)
                        , "port" Aeson..= (cfg ^. port)
                        , "fromAddress" Aeson..= fromAddress
                        , "password" Aeson..= (cfg ^. password)
                        , "tls" Aeson..= tls
                        , "auth" Aeson..= auth
                        , "user" Aeson..= (cfg ^. user)
                        ]
  in case Mustache.renderMustacheW template dt of
       ([], txt) -> return txt
       (warnings, _) -> do
         $logError $ "Could not render msmtprc: " <> (Text.pack $ show warnings)
         liftIO Exit.exitFailure
  where
    template = [mustache|
defaults

{{#tls}}
tls on
tls_trust_file /etc/ssl/certs/ca-certificates.crt
{{/tls}}
{{^tls}}
tls off
{{/tls}}

timeout 30

account defaultaccount
host {{host}}
port {{port}}
from {{fromAddress}}
{{#auth}}
auth on
user {{user}}
password {{password}}
{{/auth}}
{{^auth}}
auth off
{{/auth}}

account default : defaultaccount
|]

-- | Send an html email using the sendmail program.
sendEmail ::
     (MonadLogger m, MonadIO m, MonadThrow m)
  => EmailConfig
  -> Text -- ^ Email Address
  -> Text -- ^ Subject
  -> [Mail.Part] -- ^ Mail parts
  -> m Bool
sendEmail cfg toAddress subject parts = do
  let sendmailCfg = cfg ^. sendmail
      to = Mail.Address Nothing toAddress
  let mail =
        Mail.addPart parts
        (Mail.emptyMail (cfg ^. from))
          {Mail.mailHeaders = [("Subject", subject)], Mail.mailTo = [to]}
  mbError <-
    liftIO . Ex.try $
    Mail.renderSendMailCustom
      (sendmailCfg ^. path)
      (sendmailCfg ^. arguments)
      mail
  case mbError of
    Left (Ex.ErrorCall msg) -> do
      $logError $ "Error sending mail: " <> Text.pack msg
      return False
    Right () -> return True

sendHtmlEmail ::
     (MonadThrow m, MonadIO m, MonadLogger m)
  => EmailConfig
  -> Text -- ^ Address
  -> Text -- ^ Subject
  -> LText.Text -- ^ HTML attachment
  -> m Bool
sendHtmlEmail cfg toAddress subject body =
  let plainBody = "Please see the HTML attachment."
  in sendEmail cfg toAddress subject [Mail.plainPart plainBody, Mail.htmlPart body]

sendPlainEmail ::
     (MonadLogger m, MonadIO m, MonadThrow m)
  => EmailConfig
  -> Text -- ^ Address
  -> Text -- ^ Subject
  -> LText.Text -- ^ Body
  -> m Bool
sendPlainEmail cfg toAddress subject plainBody =
  sendEmail cfg toAddress subject [Mail.plainPart plainBody]
