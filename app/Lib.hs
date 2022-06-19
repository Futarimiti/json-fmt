module Lib (fmt)
where

import           Data.Bifunctor  (second)
import           Data.Char       (isSpace)
import           Data.List       (intercalate)
import           GHC.Real        (Ratio ((:%)))
import           Text.JSON       (JSValue (..), Result (..), decode)
import           Text.JSON.Types (JSObject (JSONObject), JSString (JSONString))
import           Text.Printf     (printf)

-- main func: format a json string
fmt :: String -> String
fmt str = fmt' . decode . trimLead $ str
  where fmt' :: Result JSValue -> String
        fmt' (Error str) = str
        fmt' (Ok jsv)    = toStr jsv

-- weird enough, decode tolerates trailing space but not leading
trimLead :: String -> String
trimLead "" = ""
trimLead (x:xs)
  | isSpace x = trimLead xs
  | otherwise = x:xs

toStr :: JSValue -> String
toStr JSNull                        = "null"
toStr (JSBool True)                 = "true"
toStr (JSBool False)                = "false"
toStr (JSRational _ (a :% 1))       = show a
toStr (JSRational _ (a :% b))       = show (fromInteger a / fromInteger b :: Double)
toStr (JSString (JSONString str))   = printf "\"%s\"" str
toStr (JSArray arr)                 = '[' : values arr ++ "]"
    where values xs = intercalate ", " $ map toStr xs
toStr (JSObject (JSONObject []))    = "{}"
toStr (JSObject (JSONObject pairs)) = _fmtWIndent 0 pairs

-- fmt for JSObject

  {-
    generate something like:
    indent = 4:

    { val1" : "key1"
        , "val2" : "val2"
        , "val3" : "val3"
        }
    ----   <-- 4 spaces
    -}
_fmtWIndent :: Int -> [(String, JSValue)] -> String
_fmtWIndent n pairs = "{ " ++ intercalate delim kvstrs ++ closing
  where indent = replicate n ' '
        delim = '\n' : indent ++ ", "
        kvstrs = _toKVStr n <$> pairs
        closing = '\n' : indent ++ "}"

  {-
    converts a (String, JSValue) aka a single entry in the object into k:v style string

    if the jval is an object, fmt with indent; here first arg is important:
    { "val1" : { "val2" : "key2"
               , "val3" : "key3"
               , "val4" : { "val5" : "key5"
                          , "val6" : "val6"
                          }
               }
    }
    -----------   <-- 11 spaces: 0 from prev objs, 2 from "{ ", 6 from "'val1'", 3 from " : "
    ----------------------   <-- 22 spaces: 11 from prev objs, 2 from ", ", 6 from "'val4'", 3 from " : "

    otherwise simply toStr
    -}
_toKVStr :: Int -> (String, JSValue) -> String
-- keys must be double quoted
_toKVStr _ (str, JSObject (JSONObject [])) = printf "\"%s\" : {}" str
_toKVStr prevIndent (str, JSObject (JSONObject pairs)) = printf "\"%s\" : %s" str (_fmtWIndent (prevIndent + length str + 7) pairs) -- 7 here: 2 from key quotes, 2 from ", " or "{ ", 3 from " , "
_toKVStr _ (str, val) = printf "\"%s\" : %s" str (toStr val)

