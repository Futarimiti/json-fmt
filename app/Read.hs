{-# LANGUAGE OverloadedStrings #-}

module Read where

import Control.Monad          (unless)
import Control.Monad.Except   (ExceptT, MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger   (LoggingT, logInfoN)
import Data.ByteString        (ByteString)
import Data.ByteString        qualified as BS
import Data.Text              qualified as Text
import System.Directory       (doesFileExist)

-- T to distinguish from prelude readFile
readFileT :: FilePath -> ExceptT String (LoggingT IO) ByteString
readFileT file = do checkPath file
                    liftIO $ BS.readFile file

checkPath :: FilePath -> ExceptT String (LoggingT IO) ()
checkPath path = do logInfoN $ "Checking file " <> path'
                    exists <- liftIO $ doesFileExist path
                    unless exists $ do logInfoN $ "File " <> path' <> " does not exist"
                                       throwError $ "File not found: " <> path
                    logInfoN $ "File " <> path' <> " exists"
  where path' = Text.pack path


