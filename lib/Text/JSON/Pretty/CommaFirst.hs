module Text.JSON.Pretty.CommaFirst
  (format, module Text.JSON.Pretty.CommaFirst.Config) where

import           Control.Lens                       (view)
import           Control.Monad.Except               (ExceptT, MonadError (..))
import           Control.Monad.Reader               (ReaderT (..), asks)
import           Data.Char                          (isSpace)
import           Prettyprinter                      hiding (nest)
import qualified Prettyprinter                      as PP
import qualified Text.JSON                          as JSON
import           Text.JSON                          (JSValue (..), Result (..))
import           Text.JSON.Pretty.CommaFirst.Config
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

-- -*- OBJECT PRETTYPRINTERS -*-

ppObj :: Monad m => Int -> [(String, JSValue)] -> ReaderT Config m (Doc ann)
ppObj nest []           = ppEmptyObj nest
ppObj nest [(key, val)] = ppOneEntryObj nest (key, val)
ppObj nest keyvals      = ppMultiEntryObj nest keyvals

ppEmptyObj :: Monad m => Int -> ReaderT Config m (Doc ann)
ppEmptyObj nest = do spaceNumber <- asks (view spaceNInEmptyObj)
                     oneLine <- asks (view oneEntryOneLine)
                     return $ if Empty `elem` oneLine then braces $ padding spaceNumber
                                                      else PP.nest nest $ vsep [lbrace, rbrace]

ppOneEntryObj :: Monad m => Int -> (String, JSValue) -> ReaderT Config m (Doc ann)
ppOneEntryObj nest (key, val) = do oneLine <- asks (view oneEntryOneLine)
                                   if getValueType val `elem` oneLine then ppInlineOneEntryObj (key, val)
                                                                      else ppSepLineOneEntryObj nest (key, val)

ppInlineOneEntryObj :: Monad m => (String, JSValue) -> ReaderT Config m (Doc ann)
ppInlineOneEntryObj entry = do entryDoc <- ppEntry entry
                               pad <- objPadding
                               pure $ braces $ pad <> entryDoc <> pad

ppSepLineOneEntryObj :: Monad m => Int -> (String, JSValue) -> ReaderT Config m (Doc ann)
ppSepLineOneEntryObj nest entry = do entryDoc <- ppEntry entry
                                     pad <- objPadding
                                     pure $ PP.nest nest $ vsep [lbrace <> pad <> entryDoc, rbrace]

-- -- nesting logic:
-- -- nest = comma/brace + objPaddingSpaceN + key length + quotes + spaceNBeforeColon + 1 + spaceNAfterColon
ppEntry :: Monad m => (String, JSValue) -> ReaderT Config m (Doc ann)
ppEntry (key, val) = do spaceNumberBef <- asks (view spaceNBeforeColon)
                        spaceNumberAft <- asks (view spaceNAfterColon)
                        objPaddingSpace <- asks (view objPaddingSpaceN)
                        let befPadding = padding spaceNumberBef
                            aftPadding = padding spaceNumberAft
                            nest = 1 + objPaddingSpace + 2 + length key + spaceNumberBef + 1 + spaceNumberAft
                        valDoc <- ppValue nest val
                        let keyDoc = pretty $ show key  -- what could possibly go wrong?
                        pure $ keyDoc <> befPadding <> colon <> aftPadding <> valDoc

ppMultiEntryObj :: Monad m => Int -> [(String, JSValue)] -> ReaderT Config m (Doc ann)
ppMultiEntryObj nest keymap = do entries <- ppEntries keymap
                                 pad <- objPadding
                                 pure $ PP.nest nest $ vsep [lbrace <> pad <> entries, rbrace]

ppEntries :: Monad m => [(String, JSValue)] -> ReaderT Config m (Doc ann)
ppEntries keymap = do entriesDocs <- traverse ppEntry keymap
                      pad <- objPadding
                      pure $ mconcat $ PP.punctuate (hardline <> comma <> pad) entriesDocs

-- -*- ARRAY PRETTYPRINTERS -*-

ppArr :: Monad m => Int -> [JSValue] -> ReaderT Config m (Doc ann)
ppArr nest []    = ppEmptyArr nest
ppArr nest [val] = ppOneElemArr nest val
ppArr nest vec   = ppMultiElemArr nest vec

ppMultiElemArr :: Monad m => Int -> [JSValue] -> ReaderT Config m (Doc ann)
ppMultiElemArr nest vec = do hasSepLineElem <- checkSepLineElem vec
                             if hasSepLineElem then ppSepLineMultiElemArr nest vec
                                               else ppInlineMultiElemArr vec

ppInlineMultiElemArr :: Monad m => [JSValue] -> ReaderT Config m (Doc ann)
ppInlineMultiElemArr vec = do pad <- arrPadding
                              elems <- ppInlineElems vec
                              pure $ brackets $ pad <> elems <> pad

ppInlineElems :: Monad m => [JSValue] -> ReaderT Config m (Doc ann)
ppInlineElems vec = do spaceBefComma <- asks (view spaceNBeforeArrComma)
                       spaceAftComma <- asks (view spaceNAfterArrComma)
                       docs <- mapM (ppValue 0) vec
                       pure $ mconcat (PP.punctuate ((padding spaceBefComma <> comma <> padding spaceAftComma) <> padding spaceBefComma) docs)

-- -- nesting logic:
-- -- nest = comma/bracket + arrPaddingSpaceN
ppMultilineElems :: Monad m => [JSValue] -> ReaderT Config m (Doc ann)
ppMultilineElems vec = do paddingSpace <- asks (view arrPaddingSpaceN)
                          let pad = padding paddingSpace
                              nest = 1 + paddingSpace
                          docs <- mapM (ppValue nest) vec
                          pure $ mconcat $ PP.punctuate (line <> comma <> pad) docs

ppSepLineMultiElemArr :: Monad m => Int -> [JSValue] -> ReaderT Config m (Doc ann)
ppSepLineMultiElemArr nest vec = do pad <- arrPadding
                                    elems <- ppMultilineElems vec
                                    pure $ PP.nest nest $ vsep [lbracket <> pad <> elems, rbracket]

checkSepLineElem :: Monad m => [JSValue] -> ReaderT Config m Bool
checkSepLineElem vec = do sepLineElems <- asks (view elemsOnSepLine)
                          let types = map getValueType vec
                          pure $ any (`elem` sepLineElems) types

ppOneElemArr :: Monad m => Int -> JSValue -> ReaderT Config m (Doc ann)
ppOneElemArr nest val = do oneLine <- asks (view oneElemOneLine)
                           if getValueType val `elem` oneLine then ppInlineOneElemArr val
                                                              else ppSepLineOneElemArr nest val

ppSepLineOneElemArr :: Monad m => Int -> JSValue -> ReaderT Config m (Doc ann)
ppSepLineOneElemArr nest val = do paddingSpace <- asks (view arrPaddingSpaceN)
                                  doc <- ppValue (paddingSpace + 1) val  -- nest logic: comma/bracket + arrPaddingSpaceN
                                  pad <- arrPadding
                                  pure $ PP.nest nest $ vsep [lbracket <> pad <> doc, rbracket]

ppInlineOneElemArr :: Monad m => JSValue -> ReaderT Config m (Doc ann)
ppInlineOneElemArr val = do doc <- ppValue 0 val  -- val presumed to be one-line hence no nesting, could be broken
                            pad <- arrPadding
                            pure $ brackets $ pad <> doc <> pad

ppEmptyArr :: Monad m => Int -> ReaderT Config m (Doc ann)
ppEmptyArr nest = do spaceNumber <- asks (view spaceNInEmptyArr)
                     oneLine <- asks (view oneElemOneLine)
                     return $ if Empty `elem` oneLine then brackets $ padding spaceNumber
                                                      else PP.nest nest $ vsep [lbracket, rbracket]


-- utils

padding :: Int -> Doc ann
padding n = pretty $ replicate n ' '

objPadding :: Monad m => ReaderT Config m (Doc ann)
objPadding = do spaceNumber <- asks (view objPaddingSpaceN)
                pure $ padding spaceNumber

arrPadding :: Monad m => ReaderT Config m (Doc ann)
arrPadding = do spaceNumber <- asks (view arrPaddingSpaceN)
                pure $ padding spaceNumber

