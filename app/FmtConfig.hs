module FmtConfig ( FmtConfig (..)
                 , ValueType (..)
                 , defaultConfig
                 , parseConfigWithLogs
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

-- parse FmtConfig from a JSON string, with logs
parseConfigWithLogs :: String -> ([Log], FmtConfig)
parseConfigWithLogs = maybeParseConfigWithLogs . decode . trimLead
  where -- parse FmtConfig based on the json and defaultConfig
        -- an Error result will result in defaultConfig being used
        maybeParseConfigWithLogs :: Result JSValue -> ([Log], FmtConfig)
        maybeParseConfigWithLogs (Error msg) = ([msg], defaultConfig)
        maybeParseConfigWithLogs (Ok (JSObject (JSONObject kvpairs))) = foldl iter ([], defaultConfig) kvpairs
          where iter :: ([Log], FmtConfig) -> (String, JSValue) -> ([Log], FmtConfig)
                iter (logs, conf) pair = (logs ++ setConfLogs, newConf)
                  where (setConfLogs, newConf) = maybeSetConfWithLogs conf pair
        maybeParseConfigWithLogs (Ok x) = (["Expecting a json object but got " ++ show x], defaultConfig)

        maybeSetConfWithLogs :: FmtConfig -> (String, JSValue) -> ([Log], FmtConfig)
        maybeSetConfWithLogs conf pair = case pair of
          ("spaceNBeforeColon", JSRational _ (n :% 1))      -> ([], conf { spaceNBeforeColon = fromInteger n })
          ("spaceNAfterColon", JSRational _ (n :% 1))       -> ([], conf { spaceNAfterColon = fromInteger n })
          ("spaceNBeforeArrayComma", JSRational _ (n :% 1)) -> ([], conf { spaceNBeforeArrayComma = fromInteger n })
          ("spaceNAfterArrayComma", JSRational _ (n :% 1))  -> ([], conf { spaceNAfterArrayComma = fromInteger n })
          ("arrayPaddingSpaceN", JSRational _ (n :% 1))     -> ([], conf { arrayPaddingSpaceN = fromInteger n })
          ("spaceNInEmptyArr", JSRational _ (n :% 1))       -> ([], conf { spaceNInEmptyArr = fromInteger n })
          ("spaceNInEmptyObj", JSRational _ (n :% 1))       -> ([], conf { spaceNInEmptyObj = fromInteger n })
          ("bracePaddingSpaceN", JSRational _ (n :% 1))     -> ([], conf { bracePaddingSpaceN = fromInteger n })
          ("endWithNewline", JSBool b)                      -> ([], conf { endWithNewline = b })
          ("newline", JSString (JSONString str))            -> ([], conf { newline = str })
          ("oneEntryOneLine", JSArray strs)                 -> (logs, conf { oneEntryOneLine = vtList })
            where (logs, vtList) = toVTListWithLogs strs
          ("oneElemOneLine", JSArray strs)                  -> (logs, conf { oneElemOneLine = vtList })
            where (logs, vtList) = toVTListWithLogs strs
          ("elemsOnSepLine", JSArray strs)                  -> (logs, conf { elemsOnSepLine = vtList })
            where (logs, vtList) = toVTListWithLogs strs
          (str, jsv) -> (["Unrecognised option, or unexpected type: (\"" ++ str ++ "\", " ++ show jsv ++ ")"], conf)

        toVTListWithLogs :: [JSValue] -> ([Log], [ValueType])
        toVTListWithLogs = foldr (\x (logs, vts) -> case x of
          (JSString (JSONString s)) -> case readMaybe s of
                                         Just vt -> (logs, vt:vts)
                                         Nothing -> (("Unrecognised value type: " ++ s):logs, vts)
          _ -> (("Expecting a string but got " ++ show x):logs, vts)) ([], [])
