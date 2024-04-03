module Text.JSON.Pretty.CommaFirst.Util where

import           Prettyprinter

padding :: Int -> Doc ann
padding n = pretty $ replicate n ' '

