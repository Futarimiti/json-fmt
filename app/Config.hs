{-# LANGUAGE OverloadedStrings #-}

module Config (getConfig) where

import Control.Applicative         (Alternative, (<|>))
import Control.Monad.Except        (MonadError (throwError), runExceptT)
import Control.Monad.IO.Class      (MonadIO (..))
import Control.Monad.Logger        (MonadLogger, logErrorN, logInfoN)
import Data.Default                (Default (def))
import Data.Either                 (fromRight)
import Data.Text                   qualified as Text
import Read                        (readFileBS)
import System.Environment          (lookupEnv)
import Text.JSON.Pretty.CommaFirst qualified as JSON

-- | Contains a configuration from either user or default
getConfig :: (MonadIO m, MonadLogger m) => m JSON.Config
getConfig = fromRight def <$> runExceptT getUserConfig

getUserConfig :: (Alternative m, MonadError String m, MonadLogger m, MonadIO m)
              => m JSON.Config
getUserConfig = do logInfoN "Trying to get user configuration"
                   envConfig <|> xdgConfig <|> throwError "No user configuration found"

envConfig, xdgConfig :: (MonadError String m, MonadLogger m, MonadIO m) => m JSON.Config
envConfig = do logInfoN "Attempting to read configuration from environment variable"
               destFile <- checkEnv "JSONFMT_CONFIG"
               parseFile destFile

xdgConfig = do logInfoN "Attempting to read configuration from $XDG_CONFIG_HOME/json-fmt/config.json"
               xdg <- checkEnv "XDG_CONFIG_HOME"
               let destFile = xdg <> "/json-fmt/config.json"
               parseFile destFile


-- impl

parseFile :: (MonadError String m, MonadLogger m, MonadIO m) => FilePath -> m JSON.Config
parseFile path = do logInfoN $ "Trying to read configuration from " <> path'
                    content <- readFileBS path
                    case JSON.parseConfigJSON content of
                        Left err -> do logErrorN $ "Error parsing " <> path' <> ": " <> Text.pack err
                                       throwError $ "Error parsing JSON: " <> err
                        Right cfg -> do logInfoN "Successfully parsed JSON"
                                        return cfg
  where path' = Text.pack path

-- | Check out the environment variable
-- returns it if it's set and not empty, otherwise throws an error
checkEnv :: (MonadError String m, MonadLogger m, MonadIO m)
         => String  -- env var name
         -> m String
checkEnv env = do logInfoN $ "Checking env var " <> Text.pack env
                  maybeVal <- liftIO $ lookupEnv env
                  case maybeVal of
                    Nothing -> do logInfoN $ "Env var " <> Text.pack env <> " not set"
                                  throwError $ "Env var not set: " <> env
                    Just val -> do logInfoN $ "Env var " <> Text.pack env <> " is set to, quote: '" <> Text.pack val <> "'"
                                   return val
