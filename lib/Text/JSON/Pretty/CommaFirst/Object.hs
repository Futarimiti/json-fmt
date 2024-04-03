module Text.JSON.Pretty.CommaFirst.Object (ppObj) where

import                          Control.Lens                       (view)
import                          Control.Monad.Reader
import                          Prettyprinter                      hiding (nest)
import                qualified Prettyprinter                      as PP
import                          Text.JSON
import {-# SOURCE #-}           Text.JSON.Pretty.CommaFirst        (ppValue)
import                          Text.JSON.Pretty.CommaFirst.Config
import                          Text.JSON.Pretty.CommaFirst.Util   (padding)

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

-- nesting logic:
-- nest = comma/brace + objPaddingSpaceN + key length + quotes + spaceNBeforeColon + 1 + spaceNAfterColon
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

objPadding :: Monad m => ReaderT Config m (Doc ann)
objPadding = do spaceNumber <- asks (view objPaddingSpaceN)
                pure $ padding spaceNumber

