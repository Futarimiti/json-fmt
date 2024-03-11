{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Action               (Action (..), FormatAction (..))
import           Config               (getConfig)
import           Control.Monad.Except (MonadIO (..), MonadTrans (..), runExcept,
                                       runExceptT)
import           Control.Monad.Logger (LogLevel (..), filterLogger, logErrorN,
                                       runStderrLoggingT)
import           Control.Monad.Reader (ReaderT (..), mapReaderT)
import qualified Data.ByteString      as BS
import qualified Data.Text            as Text
import           Options              (parseArgs)
import           Read                 (readFileT)
import           System.IO            (hPutStr, stderr)
import qualified Text.JSON.Format     as JSON

main :: IO ()
main = do Action {..} <- parseArgs
          let loggedOutcome = runExceptT $ do
                config <- lift getConfig
                input <- case format of
                           FormatStdin     -> liftIO BS.getContents
                           FormatFile path -> readFileT path
                lift $ runReaderT
                  (mapReaderT (either (logErrorN . (("Error formatting " <> (case format of
                                                                               FormatStdin -> "<stdin>"
                                                                               FormatFile path -> Text.pack path) <> ": ") <>) . Text.pack)
                                      (liftIO . BS.putStr) . runExcept) (JSON.format input)) config
          let filterImportant = filterLogger (const (>= LevelWarn))
          outcome <- runStderrLoggingT $ (if verbose then id else filterImportant) loggedOutcome
          case outcome of
            Left err -> hPutStr stderr err
            Right _  -> return ()
