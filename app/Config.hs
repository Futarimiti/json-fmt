{-# LANGUAGE OverloadedStrings #-}

module Config (getConfig) where

import           Control.Applicative         ((<|>))
import           Control.Monad.Except        (ExceptT, MonadError (throwError),
                                              MonadIO (liftIO), runExceptT)
import           Control.Monad.Logger        (LoggingT, logErrorN, logInfoN)
import           Data.Default                (Default (def))
import           Data.Either                 (fromRight)
import qualified Data.Text                   as Text
import           Read                        (readFileT)
import           System.Environment          (lookupEnv)
import qualified Text.JSON.Pretty.CommaFirst as JSON

-- | Contains a configuration from either user or default
getConfig :: LoggingT IO JSON.Config
getConfig = fromRight def <$> runExceptT getUserConfig

getUserConfig :: ExceptT String (LoggingT IO) JSON.Config
getUserConfig = do logInfoN "Trying to get user configuration"
                   envConfig <|> xdgConfig <|> throwError "No user configuration found"

envConfig, xdgConfig :: ExceptT String (LoggingT IO) JSON.Config
envConfig = do logInfoN "Attempting to read configuration from environment variable"
               destFile <- checkEnv "JSONFMT_CONFIG"
               parseFile destFile

xdgConfig = do logInfoN "Attempting to read configuration from $XDG_CONFIG_HOME/json-fmt/config.json"
               xdg <- checkEnv "XDG_CONFIG_HOME"
               let destFile = xdg <> "/json-fmt/config.json"
               parseFile destFile


-- impl


parseFile :: FilePath -> ExceptT String (LoggingT IO) JSON.Config
parseFile path = do logInfoN $ "Trying to read configuration from " <> path'
                    content <- readFileT path
                    case JSON.parseConfigJSON content of
                        Left err -> do logErrorN $ "Error parsing " <> path' <> ": " <> Text.pack err
                                       throwError $ "Error parsing JSON: " <> err
                        Right cfg -> do logInfoN "Successfully parsed JSON"
                                        return cfg
  where path' = Text.pack path

-- | Check out the environment variable
-- returns it if it's set and not empty, otherwise throws an error
checkEnv :: String  -- env var name
         -> ExceptT String (LoggingT IO) String
checkEnv env = do logInfoN $ "Checking env var " <> Text.pack env
                  maybeVal <- liftIO $ lookupEnv env
                  case maybeVal of
                    Nothing -> do logInfoN $ "Env var " <> Text.pack env <> " not set"
                                  throwError $ "Env var not set: " <> env
                    Just val -> do logInfoN $ "Env var " <> Text.pack env <> " is set to, quote: '" <> Text.pack val <> "'"
                                   return val
