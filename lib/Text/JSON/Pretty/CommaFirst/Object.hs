module Text.JSON.Pretty.CommaFirst.Object (ppObj) where

import                Control.Lens                       (view)
import                Control.Monad.Reader
import                Prettyprinter                      hiding (nest)
import                Prettyprinter                      qualified as PP
import                Text.JSON
import {-# SOURCE #-} Text.JSON.Pretty.CommaFirst        (ppValue)
import                Text.JSON.Pretty.CommaFirst.Config
import                Text.JSON.Pretty.CommaFirst.Util   (padding)

ppObj :: MonadReader Config m
      -- | the continuation indentation (in spaces) to apply
      -- when this object appears as a nested value. It is computed by the parent
      -- (typically from the width of the key, colon, and surrounding padding), not
      -- the current nesting depth.
      --
      -- For example:
      -- { "abc": { ... } }
      -- ^^^^^^^^^
      -- the inner object would be printed with an indentation of 9
      -- (the width of @'{ "abc": '@).
      -- { "abc":
      --   { ...
      --   }
      -- }
      -- ^^
      -- and here the indentation is then 2.
      => Int
      -> [(String, JSValue)]
      -> m (Doc ann)
ppObj nest []           = ppEmptyObj nest
ppObj nest [(key, val)] = ppOneEntryObj nest (key, val)
ppObj nest keyvals      = ppMultiEntryObj nest keyvals

ppEmptyObj :: MonadReader Config m => Int -> m (Doc ann)
ppEmptyObj nest = do spaceNumber <- view spaceNInEmptyObj
                     oneLine <- view oneEntryOneLine
                     return $ if Empty `elem` oneLine
                                 then braces $ padding spaceNumber
                                 else PP.nest nest $ vsep [lbrace, rbrace]

ppOneEntryObj :: MonadReader Config m => Int -> (String, JSValue) -> m (Doc ann)
ppOneEntryObj nest (key, val) = do oneLine <- view oneEntryOneLine
                                   if getValueType val `elem` oneLine
                                      then ppInlineOneEntryObj (key, val)
                                      else ppSepLineOneEntryObj nest (key, val)

-- { "a": 1 }
ppInlineOneEntryObj :: MonadReader Config m => (String, JSValue) -> m (Doc ann)
ppInlineOneEntryObj entry = do entryDoc <- ppEntry entry
                               pad <- objPadding
                               pure $ braces $ pad <> entryDoc <> pad

-- { "a": 1
-- }
ppSepLineOneEntryObj :: MonadReader Config m => Int -> (String, JSValue) -> m (Doc ann)
ppSepLineOneEntryObj nest entry = do entryDoc <- ppEntry entry
                                     pad <- objPadding
                                     pure $ PP.nest nest $
                                       vsep [lbrace <> pad <> entryDoc, rbrace]

-- nesting logic:
-- nest = comma/brace + objPaddingSpaceN + key length + quotes + spaceNBeforeColon + 1 + spaceNAfterColon
ppEntry :: MonadReader Config m => (String, JSValue) -> m (Doc ann)
ppEntry (key, val) = do spaceNumberBef <- view spaceNBeforeColon
                        spaceNumberAft <- view spaceNAfterColon
                        objPaddingSpace <- view objPaddingSpaceN
                        let befPadding = padding spaceNumberBef
                            aftPadding = padding spaceNumberAft
                            nest = 1 + objPaddingSpace + 2 + length key
                                 + spaceNumberBef + 1 + spaceNumberAft
                        valDoc <- ppValue nest val
                        let keyDoc = pretty $ show key  -- what could possibly go wrong?
                        pure $ keyDoc <> befPadding <> colon <> aftPadding <> valDoc

ppMultiEntryObj :: MonadReader Config m => Int -> [(String, JSValue)] -> m (Doc ann)
ppMultiEntryObj nest keymap = do entries <- ppEntries keymap
                                 pad <- objPadding
                                 pure $ PP.nest nest $
                                   vsep [lbrace <> pad <> entries, rbrace]

ppEntries :: MonadReader Config m => [(String, JSValue)] -> m (Doc ann)
ppEntries keymap = do entriesDocs <- traverse ppEntry keymap
                      pad <- objPadding
                      pure $ mconcat $ PP.punctuate (hardline <> comma <> pad) entriesDocs

objPadding :: MonadReader Config m => m (Doc ann)
objPadding = do spaceNumber <- view objPaddingSpaceN
                pure $ padding spaceNumber

