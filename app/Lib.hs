{-# LANGUAGE FlexibleInstances #-}
module Lib (JValue(..))
where

import           Data.Bifunctor  (second)
import           Data.List       (intercalate)
import           GHC.Real        (Ratio ((:%)))
import           Text.JSON       (JSValue (..), Result (..), decode)
import           Text.JSON.Types (JSObject (JSONObject), JSString (JSONString))
import           Text.Printf     (printf)

data JValue = JString String
            | JDouble Double
            | JInteger Integer
          -- NOTE: js does not actually distinguish double or int, just 'number'
          -- only used for fmt purpose here
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
            deriving Eq

instance Show JValue where
  show (JString str) = printf "\"%s\"" str
  show (JDouble d) = show d
  show (JInteger i) = show i
  show (JBool True) = "true"
  show (JBool False) = "false"
  show JNull = "null"
  show (JArray arr) = '[' : values arr ++ "]"
    where values xs = intercalate ", " $ map show xs
  show (JObject []) = "{}"
  show (JObject o) = _fmtWIndent 0 o -- do not call show on JObject unless top-level

-- | interoperate with Text.JSON
parse :: JSValue -> JValue
parse JSNull                        = JNull
parse (JSBool True)                 = JBool True
parse (JSBool False)                = JBool False
parse (JSRational True (a :% b))    = error "JSRational turned to be True" -- haven't figured out when JSRational would be True
parse (JSRational False (a :% 1))   = JInteger a
parse (JSRational False (a :% b))   = JDouble $ fromInteger a / fromInteger b
parse (JSString (JSONString str))   = JString str
parse (JSArray arr)                 = JArray (parse <$> arr)
parse (JSObject (JSONObject []))    = JObject []
parse (JSObject (JSONObject pairs)) = JObject (second parse <$> pairs)

instance Read JValue where
  readsPrec _ str = process (decode (replace '\\' "\\\\" str) :: Result JSValue)
    where process :: Result JSValue -> [(JValue, String)]
          process (Error msg) = error msg
          process (Ok jsv)    = [(parse jsv, "")]
          -- replace a single char with string
          -- '\\' must be replaced with '\\\\', otherwise escape sequences will be evaluated
          replace :: Char -> String -> String -> String
          replace src dest [] = []
          replace src dest (x:xs) = if x == src then dest ++ replace src dest xs
                                      else x : replace src dest xs

-- fmt for JObject

  {-
    generate something like:
    indent = 4:

    { val1" : "key1"
        , "val2" : "val2"
        , "val3" : "val3"
        }
    ----   <-- 4 spaces
    -}
_fmtWIndent :: Int -> [(String, JValue)] -> String
_fmtWIndent n pairs = "{ " ++ intercalate delim kvstrs ++ closing
  where indent = replicate n ' '
        delim = '\n' : indent ++ ", "
        kvstrs = _toKVStr n <$> pairs
        closing = '\n' : indent ++ "}"

  {-
    converts a (String, JValue) aka a single entry in the object into k:v style string

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

    otherwise simply show
    -}
_toKVStr :: Int -> (String, JValue) -> String
-- keys must be double quoted
_toKVStr _ (str, JObject []) = printf "\"%s\" : {}" str
_toKVStr n (str, JObject o) = printf "\"%s\" : %s" str (_fmtWIndent (n + length str + 7) o) -- 7 here: 2 from key quotes, 2 from ", " or "{ ", 3 from " , "
_toKVStr _ (str, val)       = printf "\"%s\" : %s" str (show val)

