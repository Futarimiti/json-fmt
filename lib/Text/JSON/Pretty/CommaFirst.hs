module Text.JSON.Pretty.CommaFirst
  (format, module Text.JSON.Pretty.CommaFirst.Config) where

import           Control.Monad.Except               (ExceptT)
import           Control.Monad.Reader               (ReaderT (..))
import           Data.ByteString                    (ByteString)
import           Prettyprinter                      (Doc)
import           Text.JSON                          (JSValue (..))
import           Text.JSON.Pretty.CommaFirst.Config

format :: ByteString -> ReaderT Config (ExceptT String m) ByteString
format = undefined ppEntire

ppEntire :: JSValue -> ReaderT Config m (Doc ann)
ppEntire = undefined

