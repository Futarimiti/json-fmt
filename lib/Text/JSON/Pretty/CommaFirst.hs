module Text.JSON.Pretty.CommaFirst
  (format, module Text.JSON.Pretty.CommaFirst.Config) where

import           Control.Monad.Except               (ExceptT)
import           Control.Monad.Reader               (ReaderT (..))
import           Prettyprinter                      (Doc)
import           Text.JSON                          (JSValue (..))
import           Text.JSON.Pretty.CommaFirst.Config

format :: String -> ReaderT Config (ExceptT String m) String
format = undefined ppEntire

ppEntire :: JSValue -> ReaderT Config m (Doc ann)
ppEntire = undefined

