# json-fmt

```json
{ "array": [ 1, 2, 2, 3 ]
, "float": 3.1415926
, "mixedarr": [ "str"
              , true
              , [ "innerlist" ]
              ]
, "number": 123
, "objarr": [ { "ab": "cd"
              , "num": 0.0
              }
            ]
, "string": "abcd"
, "subobj": { "k1": "v1"
            , "k2": "v2"
            , "num": 0.3
            }
}
```

<!-- ### From the author -->

<!-- Years ago there was no comma-leading JSON formatter -->
<!-- and I ground my teeth and wrote this toy in like 3 days; -->
<!-- Copilot was not a thing yet. -->
<!-- You can tell my immaturity just look at the r/programmerhorror code quality. -->
<!-- (em actually I barely see any haskell there -->
<!-- I guess it's more difficult to be inelegant in haskell) -->
<!-- I'm still active now in 2024, so anything wrong just go open an issue or a PR—you know the flow. -->
<!-- Though I would indeed take some time to understand my spaghetti to get back on track. -->
<!-- Some time in the future may I carry out a refactor -->
<!-- or more realistically some better tool will come out and -->
<!-- I'll just put a "GO USE \[insert repo here] INSTEAD" banner here. -->
<!-- But before that, hope you could at least have some good experience with my creation. -->

This branch is a rewrite of the original spaghetti,
notably specialised libraries are used instead of concatenating strings.
Lots of breaking changes happened.
You should be fine if you are new to this project,
otherwise do not casually migrate; read the updated doc below.

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
* `$JSONFMT_CONFIG`
* `$XDG_CONFIG_HOME/json-fmt/config.json`

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

### Bugs/features

* Fields within an object will be ordered alphabetically.
  Normally there should be an option for that
  but I can't yet find a way to parse JSON with field order preserved with Aeson.

### Vim integration

```vim
" ~/.vim/after/ftplugin/json.vim
setlocal formatprg=json-fmt
```
