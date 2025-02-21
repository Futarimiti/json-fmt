module Text.JSON.Pretty.CommaFirst.Array (ppArr) where

import                Control.Lens                       (view)
import                Control.Monad.Reader
import                Prettyprinter                      hiding (nest)
import                Prettyprinter                      qualified as PP
import                Text.JSON
import {-# SOURCE #-} Text.JSON.Pretty.CommaFirst        (ppValue)
import                Text.JSON.Pretty.CommaFirst.Config
import                Text.JSON.Pretty.CommaFirst.Util   (padding)

ppArr :: MonadReader Config m => Int -> [JSValue] -> m (Doc ann)
ppArr nest []    = ppEmptyArr nest
ppArr nest [val] = ppOneElemArr nest val
ppArr nest vec   = ppMultiElemArr nest vec

ppMultiElemArr :: MonadReader Config m => Int -> [JSValue] -> m (Doc ann)
ppMultiElemArr nest vec = do hasSepLineElem <- checkSepLineElem vec
                             if hasSepLineElem then ppSepLineMultiElemArr nest vec
                                               else ppInlineMultiElemArr vec

ppInlineMultiElemArr :: MonadReader Config m => [JSValue] -> m (Doc ann)
ppInlineMultiElemArr vec = do pad <- arrPadding
                              elems <- ppInlineElems vec
                              pure $ brackets $ pad <> elems <> pad

ppInlineElems :: MonadReader Config m => [JSValue] -> m (Doc ann)
ppInlineElems vec = do spaceBefComma <- view spaceNBeforeArrComma
                       spaceAftComma <- view spaceNAfterArrComma
                       docs <- mapM (ppValue 0) vec
                       pure $ mconcat (PP.punctuate ((padding spaceBefComma <> comma <> padding spaceAftComma) <> padding spaceBefComma) docs)

-- nesting logic:
-- nest = comma/bracket + arrPaddingSpaceN
ppMultilineElems :: MonadReader Config m => [JSValue] -> m (Doc ann)
ppMultilineElems vec = do paddingSpace <- view arrPaddingSpaceN
                          let pad = padding paddingSpace
                              nest = 1 + paddingSpace
                          docs <- mapM (ppValue nest) vec
                          pure $ mconcat $ PP.punctuate (line <> comma <> pad) docs

ppSepLineMultiElemArr :: MonadReader Config m => Int -> [JSValue] -> m (Doc ann)
ppSepLineMultiElemArr nest vec = do pad <- arrPadding
                                    elems <- ppMultilineElems vec
                                    pure $ PP.nest nest $ vsep [lbracket <> pad <> elems, rbracket]

checkSepLineElem :: MonadReader Config m => [JSValue] -> m Bool
checkSepLineElem vec = do sepLineElems <- view elemsOnSepLine
                          let types = map getValueType vec
                          pure $ any (`elem` sepLineElems) types

ppOneElemArr :: MonadReader Config m => Int -> JSValue -> m (Doc ann)
ppOneElemArr nest val = do oneLine <- view oneElemOneLine
                           if getValueType val `elem` oneLine then ppInlineOneElemArr val
                                                              else ppSepLineOneElemArr nest val

ppSepLineOneElemArr :: MonadReader Config m => Int -> JSValue -> m (Doc ann)
ppSepLineOneElemArr nest val = do paddingSpace <- view arrPaddingSpaceN
                                  doc <- ppValue (paddingSpace + 1) val  -- nest logic: comma/bracket + arrPaddingSpaceN
                                  pad <- arrPadding
                                  pure $ PP.nest nest $ vsep [lbracket <> pad <> doc, rbracket]

ppInlineOneElemArr :: MonadReader Config m => JSValue -> m (Doc ann)
ppInlineOneElemArr val = do doc <- ppValue 0 val  -- val presumed to be one-line hence no nesting, could be broken
                            pad <- arrPadding
                            pure $ brackets $ pad <> doc <> pad

ppEmptyArr :: MonadReader Config m => Int -> m (Doc ann)
ppEmptyArr nest = do spaceNumber <- view spaceNInEmptyArr
                     oneLine <- view oneElemOneLine
                     return $ if Empty `elem` oneLine then brackets $ padding spaceNumber
                                                      else PP.nest nest $ vsep [lbracket, rbracket]

arrPadding :: MonadReader Config m => m (Doc ann)
arrPadding = do spaceNumber <- view arrPaddingSpaceN
                pure $ padding spaceNumber
