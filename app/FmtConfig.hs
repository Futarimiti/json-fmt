module FmtConfig
where

import           Data.Char         (isSpace)
import           GHC.Real          (Ratio ((:%)))
import           Text.JSON         (JSON (readJSON), JSValue (..),
                                    Result (Error, Ok), decode)
import           Text.JSON.Generic (JSON (showJSON))
import           Text.JSON.Types   (JSObject (JSONObject),
                                    JSString (JSONString))

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

data ConfigTerm = SpaceNBeforeColon
                | SpaceNAfterColon
                | SpaceNBeforeArrayComma
                | SpaceNAfterArrayComma
                | ArrayPaddingSpaceN
                | SpaceNInEmptyArr
                | SpaceNInEmptyObj
                | BracePaddingSpaceN
                | EndWithNewline
                | Newline
                | OneEntryOneLine
                | OneElemOneLine
                | ElemsOnSepLine

data FmtConfig = FmtConfig { spaceNBeforeColon'      :: ConfigValue
                           , spaceNAfterColon'       :: ConfigValue
                           , spaceNBeforeArrayComma' :: ConfigValue
                           , spaceNAfterArrayComma'  :: ConfigValue
                           , arrayPaddingSpaceN'     :: ConfigValue
                           , spaceNInEmptyArr'       :: ConfigValue
                           , spaceNInEmptyObj'       :: ConfigValue
                           , bracePaddingSpaceN'     :: ConfigValue
                           , endWithNewline'         :: ConfigValue
                           , newline'                :: ConfigValue
                           , oneEntryOneLine'        :: ConfigValue
                           , oneElemOneLine'         :: ConfigValue
                           , elemsOnSepLine'         :: ConfigValue
                           } deriving Show

data ConfigValue = ConfInt Int | ConfBool Bool | ConfStr String | ConfVTList [ValueType] deriving (Show, Read)
data ValueType = Empty | Null | Bool | Number | EmptyString | NonEmptyString | FilledArray | EmptyArray | FilledObject | EmptyObject deriving (Eq, Read, Show)

defaultConfig = FmtConfig { spaceNBeforeColon' = ConfInt 1
                          , spaceNAfterColon' = ConfInt 1
                          , spaceNBeforeArrayComma' = ConfInt 0
                          , spaceNAfterArrayComma' = ConfInt 1
                          , arrayPaddingSpaceN' = ConfInt 1
                          , spaceNInEmptyArr' = ConfInt 0
                          , spaceNInEmptyObj' = ConfInt 0
                          , bracePaddingSpaceN' = ConfInt 1
                          , endWithNewline' = ConfBool True
                          , newline' = ConfStr "\n"
                          , oneEntryOneLine' = ConfVTList [ Empty, Null, Bool, Number, EmptyString, NonEmptyString, EmptyArray, EmptyObject ]
                          , oneElemOneLine' = ConfVTList [ Empty, Null, Bool, Number, EmptyString, NonEmptyString, EmptyObject ]
                          , elemsOnSepLine' = ConfVTList [ FilledObject, FilledArray ]
                          }

-- getters ig
spaceNBeforeColon = getInt . spaceNBeforeColon'
spaceNAfterColon = getInt . spaceNAfterColon'
spaceNBeforeArrayComma = getInt . spaceNBeforeArrayComma'
spaceNAfterArrayComma = getInt . spaceNAfterArrayComma'
arrayPaddingSpaceN = getInt . arrayPaddingSpaceN'
spaceNInEmptyArr = getInt . spaceNInEmptyArr'
spaceNInEmptyObj = getInt . spaceNInEmptyObj'
bracePaddingSpaceN = getInt . bracePaddingSpaceN'
endWithNewline = getBool . endWithNewline'
newline = getStr . newline'
oneEntryOneLine = getVTList . oneEntryOneLine'
oneElemOneLine = getVTList .  oneElemOneLine'
elemsOnSepLine = getVTList . elemsOnSepLine'

getInt (ConfInt n) = n
getInt _           = undefined
getBool (ConfBool b) = b
getBool _            = undefined
getStr (ConfStr s) = s
getStr _           = undefined
getVTList (ConfVTList l) = l
getVTList _              = undefined

-- unbox config vals from each kvpair
unbox :: (String, JSValue) -> Maybe (ConfigTerm, ConfigValue)
unbox ("spaceNBeforeColon", JSRational _ (n :% 1)) = Just (SpaceNBeforeColon, (ConfInt . fromInteger) n)
unbox ("spaceNAfterColon", JSRational _ (n :% 1))  = Just (SpaceNAfterColon, (ConfInt . fromInteger) n)
unbox ("spaceNBeforeArrayComma", JSRational _ (n :% 1)) = Just (SpaceNBeforeArrayComma, (ConfInt . fromInteger) n)
unbox ("spaceNAfterArrayComma", JSRational _ (n :% 1)) = Just (SpaceNAfterArrayComma, (ConfInt . fromInteger) n)
unbox ("arrayPaddingSpaceN", JSRational _ (n :% 1)) = Just (ArrayPaddingSpaceN, (ConfInt . fromInteger) n)
unbox ("spaceNInEmptyArr", JSRational _ (n :% 1)) = Just (SpaceNInEmptyArr, (ConfInt . fromInteger) n)
unbox ("spaceNInEmptyObj", JSRational _ (n :% 1)) = Just (SpaceNInEmptyObj, (ConfInt . fromInteger) n)
unbox ("bracePaddingSpaceN", JSRational _ (n :% 1)) = Just (BracePaddingSpaceN, (ConfInt . fromInteger) n)
unbox ("endWithNewline", JSBool b) = Just (EndWithNewline, ConfBool b)
unbox ("newline", JSString (JSONString str)) = Just (Newline, ConfStr str)
unbox ("oneEntryOneLine", JSArray strs) = Just (OneEntryOneLine, ConfVTList $ readVTs strs)
unbox ("oneElemOneLine", JSArray strs) = Just (OneElemOneLine, ConfVTList $ readVTs strs)
unbox ("elemsOnSepLine", JSArray strs) = Just (ElemsOnSepLine, ConfVTList $ readVTs strs)
unbox _ = Nothing

readVTs :: [JSValue] -> [ValueType]
readVTs = foldr (\x acc -> case x of (JSString (JSONString s)) -> read s : acc
                                     _                         -> acc) []

-- overriding any configVals in defaultConfig
makeConfig :: [(ConfigTerm, ConfigValue)] -> FmtConfig
makeConfig = foldl setConf defaultConfig
  where setConf :: FmtConfig -> (ConfigTerm, ConfigValue) -> FmtConfig
        setConf conf (SpaceNBeforeColon, ConfInt n) = conf { spaceNBeforeColon' = ConfInt n }
        setConf conf (SpaceNAfterColon, ConfInt n) = conf { spaceNAfterColon' = ConfInt n }
        setConf conf (SpaceNBeforeArrayComma, ConfInt n) = conf { spaceNBeforeArrayComma' = ConfInt n }
        setConf conf (SpaceNAfterArrayComma, ConfInt n) = conf { spaceNAfterArrayComma' = ConfInt n }
        setConf conf (ArrayPaddingSpaceN, ConfInt n) = conf { arrayPaddingSpaceN' = ConfInt n }
        setConf conf (SpaceNInEmptyArr, ConfInt n) = conf { spaceNInEmptyArr' = ConfInt n }
        setConf conf (SpaceNInEmptyObj, ConfInt n) = conf { spaceNInEmptyObj' = ConfInt n }
        setConf conf (BracePaddingSpaceN, ConfInt n) = conf { bracePaddingSpaceN' = ConfInt n }
        setConf conf (EndWithNewline, ConfBool b) = conf { endWithNewline' = ConfBool b }
        setConf conf (Newline, ConfStr s) = conf { newline' = ConfStr s }
        setConf conf (OneEntryOneLine, ConfVTList ls) = conf { oneEntryOneLine' = ConfVTList ls }
        setConf conf (OneElemOneLine, ConfVTList ls) = conf { oneElemOneLine' = ConfVTList ls }
        setConf conf (ElemsOnSepLine, ConfVTList ls) = conf { elemsOnSepLine' = ConfVTList ls }
        setConf _ _ = error "Imcompatible type detected when setting fmt config"

-- parse FmtConfig from a JSON string
-- left: error msg; right: FmtConfig
parseConfig :: String -> Either String FmtConfig
parseConfig  = maybeParseConfig . decode . trimLead
  where maybeParseConfig :: Result JSValue -> Either String FmtConfig
        maybeParseConfig (Ok (JSObject (JSONObject kvpairs))) = Right $ makeConfig configVals
          where configVals = foldl (\acc x -> case unbox x of Just y -> y:acc
                                                              _      -> acc) [] kvpairs :: [(ConfigTerm, ConfigValue)]
        maybeParseConfig (Error msg) = Left msg
        maybeParseConfig _           = Left "Expecting a valid object"

        -- weird enough, decode tolerates trailing space but not leading
        trimLead :: String -> String
        trimLead "" = ""
        trimLead (x:xs)
          | isSpace x = trimLead xs
          | otherwise = x:xs
