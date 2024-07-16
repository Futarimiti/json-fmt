module Text.JSON.Pretty.CommaFirst where

import           Control.Monad.Reader               (MonadReader)
import           Prettyprinter                      hiding (nest)
import           Text.JSON                          (JSValue (..))
import           Text.JSON.Pretty.CommaFirst.Config

ppValue :: MonadReader Config m => Int -> JSValue -> m (Doc ann)
