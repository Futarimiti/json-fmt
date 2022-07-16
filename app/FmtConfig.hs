module FmtConfig ( FmtConfig (..)
                 , ValueType (..)
                 , defaultConfig
                 , parseConfig
                 )
where

import           Data.Char       (isSpace)
import           GHC.Real        (Ratio ((:%)))
import           Logged
import           Prelude         hiding (log)
import           Text.JSON       (JSValue (..), Result (Error, Ok), decode)
import           Text.JSON.Types (JSObject (JSONObject), JSString (JSONString))
import           Text.Read       (readMaybe)
import           Util

data FmtConfig = FmtConfig { spaceNBeforeColon      :: Int
                           , spaceNAfterColon       :: Int
                           , spaceNBeforeArrayComma :: Int
                           , spaceNAfterArrayComma  :: Int
                           , arrayPaddingSpaceN     :: Int
                           , spaceNInEmptyArr       :: Int
                           , spaceNInEmptyObj       :: Int
                           , bracePaddingSpaceN     :: Int
                           , endWithNewline         :: Bool
                           , newline                :: String
                           , oneEntryOneLine        :: [ValueType]
                           , oneElemOneLine         :: [ValueType]
                           , elemsOnSepLine         :: [ValueType]
                           }

data ValueType = Empty | Null | Bool | Number | EmptyString | NonEmptyString | FilledArray | EmptyArray | FilledObject | EmptyObject deriving (Eq, Read)

defaultConfig = FmtConfig { spaceNBeforeColon = 1
                          , spaceNAfterColon = 1
                          , spaceNBeforeArrayComma = 0
                          , spaceNAfterArrayComma = 1
                          , arrayPaddingSpaceN = 1
                          , spaceNInEmptyArr = 0
                          , spaceNInEmptyObj = 0
                          , bracePaddingSpaceN = 1
                          , endWithNewline = True
                          , newline = "\n"
                          , oneEntryOneLine = [ Empty, Null, Bool, Number, EmptyString, NonEmptyString, EmptyArray, EmptyObject ]
                          , oneElemOneLine = [ Empty, Null, Bool, Number, EmptyString, NonEmptyString, EmptyObject ]
                          , elemsOnSepLine = [ FilledObject, FilledArray ]
                          }

-- parse FmtConfig from a JSON string, with logs
parseConfig :: String -> Logged FmtConfig
parseConfig = maybeParseConfig . decode . trimLead
  where maybeParseConfig :: Result JSValue -> Logged FmtConfig
        maybeParseConfig (Error msg) = Logged [msg] defaultConfig
        maybeParseConfig (Ok (JSObject (JSONObject kvpairs))) = foldl iter (Logged [] defaultConfig) kvpairs
          where iter :: Logged FmtConfig -> (String, JSValue) -> Logged FmtConfig
                iter (Logged logs' conf) pair = logs logs' $ maybeSetConfig conf pair
        maybeParseConfig (Ok x) = Logged ["Expecting a json object but got " ++ show x] defaultConfig

        maybeSetConfig :: FmtConfig -> (String, JSValue) -> Logged FmtConfig
        maybeSetConfig conf pair = case pair of
          ("spaceNBeforeColon", JSRational _ (n :% 1))      -> pure conf { spaceNBeforeColon = fromInteger n }
          ("spaceNAfterColon", JSRational _ (n :% 1))       -> pure conf { spaceNAfterColon = fromInteger n }
          ("spaceNBeforeArrayComma", JSRational _ (n :% 1)) -> pure conf { spaceNBeforeArrayComma = fromInteger n }
          ("spaceNAfterArrayComma", JSRational _ (n :% 1))  -> pure conf { spaceNAfterArrayComma = fromInteger n }
          ("arrayPaddingSpaceN", JSRational _ (n :% 1))     -> pure conf { arrayPaddingSpaceN = fromInteger n }
          ("spaceNInEmptyArr", JSRational _ (n :% 1))       -> pure conf { spaceNInEmptyArr = fromInteger n }
          ("spaceNInEmptyObj", JSRational _ (n :% 1))       -> pure conf { spaceNInEmptyObj = fromInteger n }
          ("bracePaddingSpaceN", JSRational _ (n :% 1))     -> pure conf { bracePaddingSpaceN = fromInteger n }
          ("endWithNewline", JSBool b)                      -> pure conf { endWithNewline = b }
          ("newline", JSString (JSONString str))            -> pure conf { newline = str }
          ("oneEntryOneLine", JSArray strs)                 -> Logged logs conf { oneEntryOneLine = vts }
            where Logged logs vts = toVTListLogged strs
          ("oneElemOneLine", JSArray strs)                  -> Logged logs conf { oneElemOneLine = vts }
            where Logged logs vts = toVTListLogged strs
          ("elemsOnSepLine", JSArray strs)                  -> Logged logs conf { elemsOnSepLine = vts }
            where Logged logs vts = toVTListLogged strs
          (str, jsv) -> Logged ["Unrecognised option, or unexpected type: (\"" ++ str ++ "\", " ++ show jsv ++ ")"] conf

        toVTListLogged :: [JSValue] -> Logged [ValueType]
        toVTListLogged = foldr (\x acc -> case x of
          (JSString (JSONString s)) -> case readMaybe s of
                                         Just vt -> (vt:) <$> acc
                                         Nothing -> log ("Unrecognised value type: \"" ++ s ++ "\"") acc
          _                         -> log ("Expecting a string but got " ++ show x) acc) (Logged [] [])
