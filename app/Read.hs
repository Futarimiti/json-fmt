{-# LANGUAGE OverloadedStrings #-}

module Read where

import           Control.Monad.Except (ExceptT, MonadError (..), MonadIO (..),
                                       unless)
import           Control.Monad.Logger (LoggingT, logInfoN)
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as BS
import qualified Data.Text            as Text
import           System.Directory     (doesFileExist)

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


