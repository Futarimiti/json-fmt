# json-fmt

```json
{ "array": [ 1, 2, 2, 3 ]
, "mixedarr": [ "str"
              , true
              , [ "innerlist" ]
              ]
, "objarr": [ { "ab": "cd"
              , "num": 0
              }
            ]
, "number": 123
, "float": 3.1415926
, "string": "abcd"
, "subobj": { "k1": "v1"
            , "k2": "v2"
            , "num": 0.3
            }
}
```

### v3

There is a limitation in v2 where the order of fields within an object cannot be preserved.
v3 addresses this by switching from `Aeson` to `json` library for parsing,
nevertheless compromising in performance due to use of less efficient data types.
If you care about performance and don't mind me messing with your object entries, use v2 branch.

### Install

[Cabal](https://www.haskell.org/cabal/) is required. Stack should also do

```sh
git clone 'https://github.com/Futarimiti/json-fmt'
cd json-fmt
cabal install
```

There's a messy JSON [sample](resources/sample.json) for you to try out:

```sh
json-fmt resources/sample.json
```

### Usage

| Command           | Description                     |
|-------------------|---------------------------------|
| `json-fmt`        | Format JSON input from stdin    |
| `json-fmt <FILE>` | Format JSON input from `<FILE>` |

Valid JSON input is expected.
Formatted result will be printed to stdout upon successful parse and formatting,
otherwise error message will be printed to stderr.
Unless `-v` specified, no other output (errors, logs) will be printed.

#### Flags

| Flag | Description |
|------|-------------|
| `-v` | verbose     |

### Configuration

In order, `json-fmt` will look for configurations from:

*   `$JSONFMT_CONFIG`
*   `$XDG_CONFIG_HOME/json-fmt/config.json`

The config file must be a JSON object containing key-value pairs defining each option;
defaults will be used for any absent options.
See [`resources/default-config.json`](resources/default-config.json) for reference.
Upon failure to find or parse configurations, default configuration will be used.
Whenever in doubt, use `-v` to find out which paths have been searched.

Options:

#### `spaceNBeforeColon, spaceNAfterColon :: Int`

number of spaces before and after a key-value separating colon.

`spaceNBeforeColon = 1, spaceNAfterColon = 1`

```json
{ "key" : "value" }
```

`spaceNBeforeColon = 0, spaceNAfterColon = 1`

```json
{ "key": "value" }
```

#### `spaceNBeforeArrComma, spaceNAfterArrComma :: Int`

number of spaces before and after a comma in a one-line array.
does not affect those in multiline (see below)

`spaceNBeforeArrComma = 0, spaceNAfterArrComma = 1`

```json
[1, 2, 3]
```

`spaceNBeforeArrComma = 1, spaceNAfterArrComma = 1`

```json
[1 , 2 , 3]
```

#### `arrPaddingSpaceN :: Int`

number of spaces used as paddings for a non-empty array;
also affects number of spaces after every entry-separating comma, if multilined.

`arrPaddingSpaceN = 1`

```json
[ 1, 2, 3 ]

[ "string1"
, "string2"
, true
]
```

`arrPaddingSpaceN = 2`

```json
[  1, 2, 3  ]
[  "string1"
,  "string2"
,  true
]
```

#### `spaceNInEmptyArr :: Int`

number of spaces within an empty array.

`spaceNInEmptyArr = 0`

```json
[]
```

`spaceNInEmptyArr = 2`

```json
[  ]
```

#### `spaceNInEmptyObj :: Int`

number of spaces within an empty obj.

`spaceNInEmptyObj = 0`

```json
{}
```

`spaceNInEmptyObj = 2`

```json
{  }
```

#### `objPaddingSpaceN :: Int`

number of spaces used as paddings for a non-empty object;
also affects number of spaces after every entry-separating comma.

`objPaddingSpaceN = 1`

```json
{ "key" : "val"
, "key2" : {}
}
{ "key" : "val" }
```

`objPaddingSpaceN = 0`

```json
{"key" : "val"
,"key2" : {}
}
{"key" : "val"}
```

`objPaddingSpaceN = 6`

```json
{      "key" : "val"
,      "key2" : {}
}
{      "key" : "val"      }
```

#### `endWithNewline :: Bool`

leaves an empty line at the end of document when true.

#### `oneEntryOneLine :: [ValueType]`

enclosing every object containing only one entry of ValueType type on the same line as the opening.

Definition of `ValueType`:

```hs
data ValueType = Empty | Null | Bool | Number | EmptyString | NonEmptyString | FilledArray | EmptyArray | FilledObject | EmptyObject
```

Inside the configuration file, use an array of strings to represent a list of value types, e.g.:

```json
{ "oneEntryOneLine" : [ "Empty", "Null", "Bool", "Number", "EmptyString" ]
}
```

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
, { "a" : "b" }
]
[ [ "elem" ]
]
```

### Vim integration

```vim
" ~/.vim/after/ftplugin/json.vim
setlocal formatprg=json-fmt
```
