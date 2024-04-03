module Text.JSON.Pretty.CommaFirst where

import           Control.Monad.Reader               (ReaderT)
import           Prettyprinter                      hiding (nest)
import           Text.JSON                          (JSValue (..))
import           Text.JSON.Pretty.CommaFirst.Config

ppValue :: Monad m => Int -> JSValue -> ReaderT Config m (Doc ann)
