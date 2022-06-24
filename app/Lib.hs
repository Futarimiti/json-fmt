module Lib (fmtDefault, fmtWithConf)
where

import           Data.List       (intercalate)
import           FmtConfig
import           GHC.Real        (Ratio ((:%)))
import           Text.JSON       (JSValue (..), Result (..), decode)
import           Text.JSON.Types (JSObject (JSONObject), JSString (JSONString))
import           Text.Printf     (printf)
import           Util

type ErrorMsg = String

-- main func: format a json string with default FmtConfig
fmtDefault :: String -> Either ErrorMsg String
fmtDefault = fmtWithConf defaultConfig

-- main' func: format a json string with specified FmtConfig
fmtWithConf :: FmtConfig -> String -> Either ErrorMsg String
fmtWithConf conf str = maybeAppendNewline <$> (fmtEntire . decode . doubleSlash . trimLead $ str)
  where maybeAppendNewline
          | endWithNewline conf = (++ "\n")
          | otherwise = id

        fmtEntire :: Result JSValue -> Either ErrorMsg String
        fmtEntire (Error msg)                        = Left $ "Invalid JSON string: " ++ msg
        fmtEntire (Ok (JSArray arr))                 = Right $ fmtArr conf 0 arr
        fmtEntire (Ok (JSObject (JSONObject pairs))) = Right $ fmtObj conf 0 pairs
        fmtEntire (Ok basic)                         = Right $ fmtBasic conf basic

        doubleSlash :: String -> String
        doubleSlash = replace '\\' "\\\\"

colonStyle :: FmtConfig -> String
colonStyle conf = padL ++ ":" ++ padR
  where padL = replicate (spaceNBeforeColon conf) ' '
        padR = replicate (spaceNAfterColon conf) ' '

arrayCommaStyle :: FmtConfig -> String
arrayCommaStyle conf = padL ++ "," ++ padR
  where padL = replicate (spaceNBeforeArrayComma conf) ' '
        padR = replicate (spaceNAfterArrayComma conf) ' '

toValueType :: JSValue -> ValueType
toValueType JSNull                     = Null
toValueType (JSBool _)                 = Bool
toValueType (JSRational _ _)           = Number
toValueType (JSString (JSONString "")) = EmptyString
toValueType (JSString (JSONString _))  = NonEmptyString
toValueType (JSArray [])               = EmptyArray
toValueType (JSArray _)                = FilledArray
toValueType (JSObject (JSONObject [])) = EmptyObject
toValueType (JSObject (JSONObject _))  = FilledObject
-- we do not have an Empty here

-- conf is not used, but could be useful in the future
-- so does indent, since at present every basic type only occupies one line
fmtBasic :: FmtConfig -> JSValue -> String
fmtBasic _ JSNull                  = "null"
fmtBasic _ (JSBool True)           = "true"
fmtBasic _ (JSBool False)          = "false"
fmtBasic _ (JSRational _ (a :% 1)) = show a
fmtBasic _ (JSRational _ (a :% b)) = show (fromInteger a / fromInteger b :: Double)
fmtBasic _ (JSString (JSONString str)) = printf "\"%s\"" str
fmtBasic _ (JSObject _) = error "Not basic type: JSObject"
fmtBasic _ (JSArray _) = error "Not basic type: JSArray"

fmtArr :: FmtConfig -> Int -> [JSValue] -> String
-- []
fmtArr conf n []
  | Empty `notElem` oneElemOneLine conf = printf "[%s%s]" (newline conf) (replicate n ' ')
  | otherwise = printf "[%s]" spaceInBrackets
  where spaceInBrackets = replicate (spaceNInEmptyArr conf) ' '

-- one elem only: check if type is in oneElemOneLine
fmtArr conf n [only]
  | toValueType only `notElem` oneElemOneLine conf = printf "[%s%s%s%s]" pad value (newline conf) indent
  | otherwise = printf "[%s%s%s]" pad value pad
  where pad = replicate (arrayPaddingSpaceN conf) ' '
        value = toValue only
        toValue :: JSValue -> String
        toValue (JSArray arr)                 = fmtArr conf newIndent arr
        toValue (JSObject (JSONObject pairs)) = fmtObj conf newIndent pairs -- 1 for {
        toValue basic                         = fmtBasic conf basic
        newIndent = n + 1 + arrayPaddingSpaceN conf
        indent = replicate n ' '

-- multiple elem: check elemsOnSepLine
fmtArr conf n elems
  | elemsOnSepLine conf `disjoint` (toValueType <$> elems) = fmtOnelineArr conf n elems
  | otherwise = fmtMultilineArr conf n elems
  where fmtOnelineArr :: FmtConfig -> Int -> [JSValue] -> String
        fmtOnelineArr conf n elems = printf "[%s%s%s]" pad elemsStr pad
          where pad = replicate (arrayPaddingSpaceN conf) ' '
                elemsStr = intercalate (arrayCommaStyle conf) (strify conf <$> elems)

        fmtMultilineArr :: FmtConfig -> Int -> [JSValue] -> String
        fmtMultilineArr conf n elems = printf "[%s%s%s%s]" pad elemsStr (newline conf) indent
          where pad = replicate (arrayPaddingSpaceN conf) ' '
                elemsStr = intercalate (printf "%s%s,%s" (newline conf) indent pad) (strify conf <$> elems)
                indent = replicate n ' '

        strify :: FmtConfig -> JSValue -> String
        strify conf (JSArray arr)                 = fmtArr conf newIndent arr
        strify conf (JSObject (JSONObject pairs)) = fmtObj conf newIndent pairs
        strify conf basic                         = fmtBasic conf basic
        newIndent = n + 1 + arrayPaddingSpaceN conf
  {-
    { "arr" : [   { "a" : 1
                  }
              ]
    --------------  <-- 14 spaces: 10 from parent n,
                                 1 from [,
                                 3 from arrayPaddingSpaceN
    -}

  {-
    generate something like:
    indent = 4:

    { val1" : "key1"
        , "val2" : "val2"
        , "val3" : "val3"
        }
    ----   <-- 4 spaces
    -}
fmtObj :: FmtConfig -> Int -> [(String, JSValue)] -> String
-- Empty
fmtObj conf n []
  | Empty `notElem` oneEntryOneLine conf = printf "{%s%s}" (newline conf) (replicate n ' ')
  | otherwise = printf "{%s}" spaceInBrace
  where spaceInBrace = replicate (spaceNInEmptyObj conf) ' '

-- one entry only
fmtObj conf n [(str, jsv)]
  | toValueType jsv `notElem` oneEntryOneLine conf = printf "{%s%s%s%s}" pad kvstr (newline conf) indent
  | otherwise = printf "{%s%s%s}" pad kvstr pad
  where pad = replicate (bracePaddingSpaceN conf) ' '
        indent = replicate n ' '
        kvstr = toKVstr (str, jsv)
        toKVstr :: (String, JSValue) -> String
        toKVstr (str, JSArray arr) = printf "\"%s\"%s%s" str (colonStyle conf) (fmtArr conf newIndent arr)
          where newIndent = n + 4 + bracePaddingSpaceN conf + length str + spaceNBeforeColon conf + spaceNAfterColon conf
        toKVstr (str, JSObject (JSONObject o)) = printf "\"%s\"%s%s" str (colonStyle conf) (fmtObj conf newIndent o)
          where newIndent = n + 4 + bracePaddingSpaceN conf + length str + spaceNBeforeColon conf + spaceNAfterColon conf
        toKVstr (str, basic) = printf "\"%s\"%s%s" str (colonStyle conf) (fmtBasic conf basic)
          {-
            factors: bracePaddingSpaceN, colonStyle
            bracePaddingSpaceN = 4, spaceNBeforeColon = 2, spaceNAfterColon = 3
            {    "ao"  :   {    "k"  :   1
                           }
            ---------------   <-- 15 spaces:
                                  1 from {
                                  4 from bracePaddingSpaceN
                                  2 from double quotes
                                  2 from str length
                                  2 from spaceNBeforeColon
                                  1 from :
                                  3 from spaceNAfterColon

                                  overall: 4 + bracePaddingSpaceN + str len + spaceNBeforeColon + spaceNAfterColon
            }
            -}

-- multiple entries
fmtObj conf n pairs = printf "{%s%s%s%s}" pad kvstrsMerged (newline conf) indent
  where pad = replicate (bracePaddingSpaceN conf) ' '
        indent = replicate n ' '
        kvstrsMerged :: String
        kvstrsMerged = intercalate (printf "%s%s,%s" (newline conf) indent pad) kvstrs
        kvstrs = toKVstr <$> pairs
        toKVstr (str, JSArray arr) = printf "\"%s\"%s%s" str (colonStyle conf) (fmtArr conf newIndent arr)
          where newIndent = n + 4 + bracePaddingSpaceN conf + length str + spaceNBeforeColon conf + spaceNAfterColon conf
        toKVstr (str, JSObject (JSONObject o)) = printf "\"%s\"%s%s" str (colonStyle conf) (fmtObj conf newIndent o)
          where newIndent = n + 4 + bracePaddingSpaceN conf + length str + spaceNBeforeColon conf + spaceNAfterColon conf
        toKVstr (str, basic) = printf "\"%s\"%s%s" str (colonStyle conf) (fmtBasic conf basic)

