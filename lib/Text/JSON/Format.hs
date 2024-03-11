module Text.JSON.Format (module Text.JSON.Format.Config, format) where

import           Control.Lens              (view)
import           Control.Monad.Except      (ExceptT, MonadError (..))
import           Control.Monad.Reader      (ReaderT, asks)
import qualified Data.Aeson                as Aeson
import qualified Data.Aeson.Key            as Key
import           Data.Aeson.KeyMap         (KeyMap)
import qualified Data.Aeson.KeyMap         as KeyMap
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as BS8
import           Data.Vector               (Vector)
import qualified Data.Vector               as Vec
import           Prettyprinter             (Doc, Pretty (..), braces, brackets,
                                            colon, comma, hardline, lbrace,
                                            lbracket, line, rbrace, rbracket,
                                            vsep)
import qualified Prettyprinter             as PP
import           Prettyprinter.Combinators (ppByteStringLazy)
import           Text.JSON.Format.Config

format :: Monad m => ByteString -> ReaderT Config (ExceptT String m) ByteString
format input = do let mvalue = Aeson.eitherDecodeStrict @Aeson.Value input
                  case mvalue of
                    Left err -> throwError err
                    Right value -> do doc <- ppEntire value
                                      pure $ BS8.pack $ show doc

ppEntire :: Monad m => Aeson.Value -> ReaderT Config m (Doc ann)
ppEntire val = do appendNewline <- asks (view endWithNewline)
                  doc <- ppValue 0 val
                  if appendNewline then pure $ doc <> hardline
                                   else pure doc

ppValue :: Monad m
        => Int  -- nesting
        -> Aeson.Value
        -> ReaderT Config m (Doc ann)
ppValue nest (Aeson.Object keymap) = ppObj nest keymap
ppValue nest (Aeson.Array vec)     = ppArr nest vec
-- all other values take only one line, hence nesting not matter
ppValue _ other                    = pure $ ppByteStringLazy $ Aeson.encode other

-- space padding
padding :: Int -> Doc ann
padding n = pretty $ replicate n ' '

-- -*- OBJECT PRETTYPRINTERS -*-
-- cannot be extracted into another module due to mutual recursion with @ppValue@

ppObj :: Monad m
      => Int  -- nesting as used by @nest@
      -> KeyMap Aeson.Value
      -> ReaderT Config m (Doc ann)
ppObj nest keymap
  | null keymap = ppEmptyObj nest
  | [(key, val)] <- KeyMap.toList keymap = ppOneEntryObj nest (key, val)
  | otherwise = ppMultiEntryObj nest keymap

ppEmptyObj :: Monad m => Int -> ReaderT Config m (Doc ann)
ppEmptyObj nest = do spaceNumber <- asks (view spaceNInEmptyObj)
                     oneLine <- asks (view oneEntryOneLine)
                     return $ if Empty `elem` oneLine then braces $ padding spaceNumber
                                                      else PP.nest nest $ vsep [lbrace, rbrace]

ppOneEntryObj :: Monad m => Int -> (KeyMap.Key, Aeson.Value) -> ReaderT Config m (Doc ann)
ppOneEntryObj nest (key, val) = do oneLine <- asks (view oneEntryOneLine)
                                   if getValueType val `elem` oneLine then ppInlineOneEntryObj (key, val)
                                                                      else ppSepLineOneEntryObj nest (key, val)

ppInlineOneEntryObj :: Monad m => (KeyMap.Key, Aeson.Value) -> ReaderT Config m (Doc ann)
ppInlineOneEntryObj (key, val) = do entry <- ppEntry key val
                                    pad <- objPadding
                                    pure $ braces $ pad <> entry <> pad

ppSepLineOneEntryObj :: Monad m => Int -> (KeyMap.Key, Aeson.Value) -> ReaderT Config m (Doc ann)
ppSepLineOneEntryObj nest (key, val) = do entry <- ppEntry key val
                                          pad <- objPadding
                                          pure $ PP.nest nest $ vsep [lbrace <> pad <> entry, rbrace]

objPadding :: Monad m => ReaderT Config m (Doc ann)
objPadding = do spaceNumber <- asks (view objPaddingSpaceN)
                pure $ padding spaceNumber

-- nesting logic:
-- nest = comma/brace + objPaddingSpaceN + key length + quotes + spaceNBeforeColon + 1 + spaceNAfterColon
ppEntry :: Monad m => KeyMap.Key -> Aeson.Value -> ReaderT Config m (Doc ann)
ppEntry key val = do spaceNumberBef <- asks (view spaceNBeforeColon)
                     spaceNumberAft <- asks (view spaceNAfterColon)
                     objPaddingSpace <- asks (view objPaddingSpaceN)
                     let befPadding = padding spaceNumberBef
                         aftPadding = padding spaceNumberAft
                         nest = 1 + objPaddingSpace + 2 + length (Key.toString key) + spaceNumberBef + 1 + spaceNumberAft
                     valDoc <- ppValue nest val
                     let keyDoc = pretty $ show key  -- what could possibly go wrong?
                     pure $ keyDoc <> befPadding <> colon <> aftPadding <> valDoc

ppMultiEntryObj :: Monad m => Int -> KeyMap Aeson.Value -> ReaderT Config m (Doc ann)
ppMultiEntryObj nest keymap = do entries <- ppEntries keymap
                                 pad <- objPadding
                                 pure $ PP.nest nest $ vsep [lbrace <> pad <> entries, rbrace]

ppEntries :: Monad m => KeyMap Aeson.Value -> ReaderT Config m (Doc ann)
ppEntries keymap = do entriesDocs <- KeyMap.traverseWithKey ppEntry keymap
                      pad <- objPadding
                      let list = map snd $ KeyMap.toList entriesDocs  -- NOTE: unstable order, could be broken later
                      pure $ mconcat $ PP.punctuate (line <> comma <> pad) list

-- -*- ARRAY PRETTYPRINTERS -*-

ppArr :: Monad m => Int -> Vector Aeson.Value -> ReaderT Config m (Doc ann)
ppArr nest vec
  | null vec = ppEmptyArr nest
  | [only] <- Vec.toList vec = ppOneElemArr nest only
  | otherwise = ppMultiElemArr nest vec

ppMultiElemArr :: Monad m => Int -> Vector Aeson.Value -> ReaderT Config m (Doc ann)
ppMultiElemArr nest vec = do hasSepLineElem <- checkSepLineElem vec
                             if hasSepLineElem then ppSepLineMultiElemArr nest vec
                                               else ppInlineMultiElemArr vec

-- TRY to pp elems in one line
-- however some elems may be multiline
ppInlineMultiElemArr :: Monad m => Vector Aeson.Value -> ReaderT Config m (Doc ann)
ppInlineMultiElemArr vec = do pad <- arrPadding
                              elems <- ppInlineElems vec
                              pure $ brackets $ pad <> elems <> pad

ppInlineElems :: Monad m => Vector Aeson.Value -> ReaderT Config m (Doc ann)
ppInlineElems vec = do spaceBefComma <- asks (view spaceNBeforeArrComma)
                       spaceAftComma <- asks (view spaceNAfterArrComma)
                       (Vec.toList -> docs) <- mapM (ppValue 0) vec
                       pure $ mconcat (PP.punctuate ((padding spaceBefComma <> comma <> padding spaceAftComma) <> padding spaceBefComma) docs)

-- nesting logic:
-- nest = comma/bracket + arrPaddingSpaceN
ppMultilineElems :: Monad m => Vector Aeson.Value -> ReaderT Config m (Doc ann)
ppMultilineElems vec = do paddingSpace <- asks (view arrPaddingSpaceN)
                          let pad = padding paddingSpace
                              nest = 1 + paddingSpace
                          (Vec.toList -> docs) <- mapM (ppValue nest) vec
                          pure $ mconcat $ PP.punctuate (line <> comma <> pad) docs

ppSepLineMultiElemArr :: Monad m => Int -> Vector Aeson.Value -> ReaderT Config m (Doc ann)
ppSepLineMultiElemArr nest vec = do pad <- arrPadding
                                    elems <- ppMultilineElems vec
                                    pure $ PP.nest nest $ vsep [lbracket <> pad <> elems, rbracket]

checkSepLineElem :: Monad m => Vector Aeson.Value -> ReaderT Config m Bool
checkSepLineElem (Vec.toList -> vec) = do sepLineElems <- asks (view elemsOnSepLine)
                                          let types = map getValueType vec
                                          pure $ any (`elem` sepLineElems) types

ppOneElemArr :: Monad m => Int -> Aeson.Value -> ReaderT Config m (Doc ann)
ppOneElemArr nest val = do oneLine <- asks (view oneElemOneLine)
                           if getValueType val `elem` oneLine then ppInlineOneElemArr val
                                                              else ppSepLineOneElemArr nest val

ppSepLineOneElemArr :: Monad m => Int -> Aeson.Value -> ReaderT Config m (Doc ann)
ppSepLineOneElemArr nest val = do paddingSpace <- asks (view arrPaddingSpaceN)
                                  doc <- ppValue (paddingSpace + 1) val  -- nest logic: comma/bracket + arrPaddingSpaceN
                                  pad <- arrPadding
                                  pure $ PP.nest nest $ vsep [lbracket <> pad <> doc, rbracket]

ppInlineOneElemArr :: Monad m => Aeson.Value -> ReaderT Config m (Doc ann)
ppInlineOneElemArr val = do doc <- ppValue 0 val  -- val presumed to be one-line hence no nesting, could be broken
                            pad <- arrPadding
                            pure $ brackets $ pad <> doc <> pad

arrPadding :: Monad m => ReaderT Config m (Doc ann)
arrPadding = do spaceNumber <- asks (view arrPaddingSpaceN)
                pure $ padding spaceNumber

ppEmptyArr :: Monad m => Int -> ReaderT Config m (Doc ann)
ppEmptyArr nest = do spaceNumber <- asks (view spaceNInEmptyArr)
                     oneLine <- asks (view oneElemOneLine)
                     return $ if Empty `elem` oneLine then brackets $ padding spaceNumber
                                                      else PP.nest nest $ vsep [lbracket, rbracket]
