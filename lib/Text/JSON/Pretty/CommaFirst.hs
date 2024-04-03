module Text.JSON.Pretty.CommaFirst
  (format, ppValue, module Text.JSON.Pretty.CommaFirst.Config) where

import           Control.Lens                       (view)
import           Control.Monad.Except               (ExceptT, MonadError (..))
import           Control.Monad.Reader               (ReaderT (..), asks)
import           Data.Char                          (isSpace)
import           Prettyprinter                      hiding (nest)
import qualified Text.JSON                          as JSON
import           Text.JSON                          (JSValue (..), Result (..))
import           Text.JSON.Pretty.CommaFirst.Array  (ppArr)
import           Text.JSON.Pretty.CommaFirst.Config
import           Text.JSON.Pretty.CommaFirst.Object (ppObj)
import           Text.JSON.Types                    (JSObject (..))

-- Strangely enough JSON.decode does not allow pre-whitespaces
-- so " {\"a\": 1}" will fail
format :: Monad m => String -> ReaderT Config (ExceptT String m) String
format input = do let trimmed = dropWhile isSpace input
                  let mvalue = JSON.decode trimmed
                  case mvalue of
                    Error err -> throwError err
                    Ok value -> do doc <- ppEntire value
                                   pure $ show doc

ppEntire :: Monad m => JSValue -> ReaderT Config m (Doc ann)
ppEntire val = do appendNewline <- asks (view endWithNewline)
                  doc <- ppValue 0 val
                  if appendNewline then pure $ doc <> hardline
                                   else pure doc

ppValue :: Monad m => Int -> JSValue -> ReaderT Config m (Doc ann)
ppValue nest (JSObject (JSONObject o)) = ppObj nest o
ppValue nest (JSArray a)               = ppArr nest a
ppValue _ other                        = pure $ pretty $ JSON.encode other
