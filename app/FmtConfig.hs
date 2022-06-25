module FmtConfig ( FmtConfig (..)
                 , ValueType (..)
                 , defaultConfig
                 , parseConfig
                 )
where

import           Data.Char       (isSpace)
import           GHC.Real        (Ratio ((:%)))
import           Text.JSON       (JSValue (..), Result (Error, Ok), decode)
import           Text.JSON.Types (JSObject (JSONObject), JSString (JSONString))
import           Text.Read       (readMaybe)
import           Util

  {-
    spaceNBeforeColon, spaceNAfterColon :: Int
      defines number of spaces before and after a key-value separating colon.
      spaceNBeforeColon = 1, spaceNAfterColon = 1
        { "key" : "value" }
      spaceNBeforeColon = 0, spaceNAfterColon = 1
        { "key": "value" }

    spaceNBeforeArrayComma, spaceNAfterArrayComma :: Int
      defines number of spaces before and after a comma in a one-line array.
      does not affect those in multiline (see below)
      spaceNBeforeArrayComma = 0, spaceNAfterArrayComma = 1
        [1, 2, 3]
      spaceNBeforeArrayComma = 1, spaceNAfterArrayComma = 1
        [1 , 2 , 3]

    arrayPaddingSpaceN :: Int
      defines number of spaces used as paddings for a non-empty array;
      also affects number of spaces after every entry-separating comma, if multilined.
      arrayPaddingSpaceN = 1
        [ 1, 2, 3 ]
        [ "string1"
        , "string2"
        , true
        ]
      arrayPaddingSpaceN = 2
        [  1, 2, 3  ]
        [  "string1"
        ,  "string2"
        ,  true
        ]

    spaceNInEmptyArr :: Int
      defines number of spaces within an empty array.
      spaceNInEmptyArr = 0
        []
      spaceNInEmptyArr = 2
        [  ]

    spaceNInEmptyObj :: Int
      defines number of spaces within an empty obj.
      spaceNInEmptyObj = 0
        {}
      spaceNInEmptyObj = 2
        {  }

    bracePaddingSpaceN :: Int
      defines number of spaces used as paddings for a non-empty object;
      also affects number of spaces after every entry-separating comma.
      bracePaddingSpaceN = 1
        { "key" : "val"
        , "key2" : {}
        }
        { "key" : "val" }
      bracePaddingSpaceN = 0
        {"key" : "val"
        ,"key2" : {}
        }
        {"key" : "val"}
      bracePaddingSpaceN = 6
        {      "key" : "val"
        ,      "key2" : {}
        }
        {      "key" : "val"      }

    endWithNewline :: Bool
      when True, leaves an empty line at the end of document.

    newline :: String
      defines style of newline, usually either "\n" or "\n\r".

    oneEntryOneLine :: [ValueType]
      enclosing every object containing only one entry of ValueType type on the same line as the opening.
      oneEntryOneLine = [Empty, Null, Bool, Number, EmptyString, NonEmptyString, EmptyArray, EmptyObject]
        {}
        { "nil" : null }
        { "bool" : true }
        { "num" : 123 }
        { "str" : "abc" }
        { "emptyArr" : [] }
        { "filledArr" : [1]
        }
        { "emptyObj" : {} }
        { "filledObj" : { "a" : "b" }
        }
      oneEntryOneLine = []
        {
        }
        { "nil" : null
        }
        { "bool" : true
        }
        { "num" : 123
        }
        { "str" : "abc"
        }
        { "emptyArr" : []
        }
        { "filledArr" : [1]
        }
        { "emptyObj" : {}
        }
        { "filledObj" : { "a" : "b"
                        }
        }

    oneElemOneLine :: [ValueType]
      enclosing every array containing only one element of ValueType type on the same line as the opening.
      oneElemOneLine = [Empty, Null, Bool, Number, EmptyString, NonEmptyString, EmptyObject]
        [ 1 ]
        [ "some very long string" ]
        [ { "obj" : {} }
        ]

    elemsOnSepLine :: [ValueType]
      if an array contains one or more elements with types in [ValueType], put each element on separate lines.
      elemsOnSepLine = [FilledObject, FilledArray]
        [ 1, 2 ]
        [ true
        , 1.0
        , { "a" = "b" }
        ]
        [ [ "elem" ]
        ]
    -}

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

maybeSetConf :: FmtConfig -> (String, JSValue) -> FmtConfig
maybeSetConf conf ("spaceNBeforeColon", JSRational _ (n :% 1)) = conf { spaceNBeforeColon = fromInteger n }
maybeSetConf conf ("spaceNAfterColon", JSRational _ (n :% 1)) = conf { spaceNAfterColon = fromInteger n }
maybeSetConf conf ("spaceNBeforeArrayComma", JSRational _ (n :% 1)) = conf { spaceNBeforeArrayComma = fromInteger n }
maybeSetConf conf ("spaceNAfterArrayComma", JSRational _ (n :% 1)) = conf { spaceNAfterArrayComma = fromInteger n }
maybeSetConf conf ("arrayPaddingSpaceN", JSRational _ (n :% 1)) = conf { arrayPaddingSpaceN = fromInteger n }
maybeSetConf conf ("spaceNInEmptyArr", JSRational _ (n :% 1)) = conf { spaceNInEmptyArr = fromInteger n }
maybeSetConf conf ("spaceNInEmptyObj", JSRational _ (n :% 1)) = conf { spaceNInEmptyObj = fromInteger n }
maybeSetConf conf ("bracePaddingSpaceN", JSRational _ (n :% 1)) = conf { bracePaddingSpaceN = fromInteger n }
maybeSetConf conf ("endWithNewline", JSBool b) = conf { endWithNewline = b }
maybeSetConf conf ("newline", JSString (JSONString str)) = conf { newline = str }
maybeSetConf conf ("oneEntryOneLine", JSArray strs) = conf { oneEntryOneLine = toVTList strs }
maybeSetConf conf ("oneElemOneLine", JSArray strs) = conf { oneElemOneLine = toVTList strs }
maybeSetConf conf ("elemsOnSepLine", JSArray strs) = conf { elemsOnSepLine = toVTList strs }
maybeSetConf conf _ = conf  -- ignore unrecognised kvpairs

-- ignore any values of invalid type or invalid ValueType
-- could make it harder to debug, maybe carry an error msg later?
toVTList :: [JSValue] -> [ValueType]
toVTList = foldr (\x acc -> case x of (JSString (JSONString s)) -> case readMaybe s of Just vt -> vt : acc
                                                                                       Nothing -> acc
                                      _ -> acc) []

-- parse FmtConfig from a JSON string
-- left: error msg; right: FmtConfig
parseConfig :: String -> Either String FmtConfig
parseConfig  = maybeParseConfig . decode . trimLead
  where maybeParseConfig :: Result JSValue -> Either String FmtConfig
        maybeParseConfig (Ok (JSObject (JSONObject kvpairs))) = Right $ foldl maybeSetConf defaultConfig kvpairs
        maybeParseConfig (Error msg) = Left msg
        maybeParseConfig _           = Left "Expecting a valid object"

