# json-fmt

```json
{ "array" : [ 1, 2, 2, 3 ]
, "mixedarr" : [ "str"
               , true
               , [ "innerlist" ]
               ]
, "objarr" : [ { "ab" : "cd"
               , "num" : 0
               }
             ]
, "number" : 123
, "float" : 3.1415926
, "string" : "abcd"
, "subobj" : { "k1" : "v1"
             , "k2" : "v2"
             , "num" : 0.3
             }
}
```

Formatted using this formatter, with default configurations

### Why?

Searched through loads of JSON formatters without finding single one supporting comma-leading style so I wrote this

### Install

To install and use:

1.  `git clone` this repo
2.  Within `json-fmt/`, run `cabal install`; get [Cabal](https://cabal.readthedocs.io/en/3.6/) if you don't have one
3.  `~/.cabal/bin/json-fmt` should've been generated as an executable; if you haven't, I suggest adding `~/.cabal/bin/` to `$PATH`
4.  `json-fmt` accepts input json as either a commandline arg or stdin; I put a `sample.json` for testing:
    ```txt
    cat sample/sample.json | json-fmt
    ```
    The formatted result should go to stdout.

`json-fmt` expects valid JSON input, and will not attempt to format inputs detected to be invalid.

### Configuration

`json-fmt` first try to read json file at environment variable `$JSONFMT_CONFIG`;
if not found, `json-fmt` will look for `$XDG_CONFIG_HOME/json-fmt/config.json`;
if invalid and configurations failed to be parsed, the default configurations will be used.

The config file must be an object, containing any number of key-value pairs defining each option of the config;
default values will be used for any absent options.
`default-config.json` containing the default configurations is provided for reference/template use.

Meanings of all options:

#### `spaceNBeforeColon, spaceNAfterColon :: Int`

defines number of spaces before and after a key-value separating colon.

`spaceNBeforeColon = 1, spaceNAfterColon = 1`

```json
{ "key" : "value" }
```

`spaceNBeforeColon = 0, spaceNAfterColon = 1`

```json
{ "key": "value" }
```

#### `spaceNBeforeArrayComma, spaceNAfterArrayComma :: Int`

defines number of spaces before and after a comma in a one-line array.
does not affect those in multiline (see below)

`spaceNBeforeArrayComma = 0, spaceNAfterArrayComma = 1`

```json
[1, 2, 3]
```

`spaceNBeforeArrayComma = 1, spaceNAfterArrayComma = 1`

```json
[1 , 2 , 3]
```

#### `arrayPaddingSpaceN :: Int`

defines number of spaces used as paddings for a non-empty array;
also affects number of spaces after every entry-separating comma, if multilined.

`arrayPaddingSpaceN = 1`

```json
[ 1, 2, 3 ]

[ "string1"
, "string2"
, true
]
```

`arrayPaddingSpaceN = 2`

```json
[  1, 2, 3  ]
[  "string1"
,  "string2"
,  true
]
```

#### `spaceNInEmptyArr :: Int`

defines number of spaces within an empty array.

`spaceNInEmptyArr = 0`

```json
[]
```

`spaceNInEmptyArr = 2`

```json
[  ]
```

#### `spaceNInEmptyObj :: Int`

defines number of spaces within an empty obj.

`spaceNInEmptyObj = 0`

```json
{}
```

`spaceNInEmptyObj = 2`

```json
{  }
```

#### `bracePaddingSpaceN :: Int`

defines number of spaces used as paddings for a non-empty object;
also affects number of spaces after every entry-separating comma.

`bracePaddingSpaceN = 1`

```json
{ "key" : "val"
, "key2" : {}
}
{ "key" : "val" }
```

`bracePaddingSpaceN = 0`

```json
{"key" : "val"
,"key2" : {}
}
{"key" : "val"}
```

`bracePaddingSpaceN = 6`

```json
{      "key" : "val"
,      "key2" : {}
}
{      "key" : "val"      }
```

#### `endWithNewline :: Bool`

when True, leaves an empty line at the end of document.

#### `newline :: String`

defines style of newline, usually either "\n" or "\n\r".

#### `oneEntryOneLine :: [ValueType]`

enclosing every object containing only one entry of ValueType type on the same line as the opening.

`oneEntryOneLine = [Empty, Null, Bool, Number, EmptyString, NonEmptyString, EmptyArray, EmptyObject]`

```json
{}
{ "nil" : null }
{ "bool" : true }
{ "num" : 123 }
{ "str" : "abc" }
{ "emptyarr" : [] }
{ "filledarr" : [1]
}
{ "emptyobj" : {} }
{ "filledobj" : { "a" : "b" }
}
```

`oneEntryOneLine = []`

```json
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
{ "emptyarr" : []
}
{ "filledarr" : [1]
}
{ "emptyobj" : {}
}
{ "filledobj" : { "a" : "b"
				}
}
```

#### `oneElemOneLine :: [ValueType]`

enclosing every array containing only one element of ValueType type on the same line as the opening.

`oneElemOneLine = [Empty, Null, Bool, Number, EmptyString, NonEmptyString, EmptyObject]`

```json
[ 1 ]
[ "some very long string" ]
[ { "obj" : {} }
]
```

#### `elemsOnSepLine :: [ValueType]`

if an array contains one or more elements with types in \[ValueType], put each element on separate lines.

`elemsOnSepLine = [FilledObject, FilledArray]`

```json
[ 1, 2 ]
[ true
, 1.0
, { "a" = "b" }
]
[ [ "elem" ]
]
```

### Vim integration

Add to ftplugin file for JSON:

```vim
setlocal formatprg=json-fmt
```
