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

### From the author

Years ago there was no comma-leading JSON formatter
and I ground my teeth and wrote this toy in like 3 days;
Copilot was not a thing yet.
You can tell my immaturity just look at the r/programmerhorror code quality.
(em actually I barely see any haskell there
I guess it's more difficult to be inelegant in haskell)
I'm still active now in 2024, so anything wrong just go open an issue or a PR—you know the flow.
Though I would indeed take some time to understand my spaghetti to get back on track.
Some time in the future may I carry out a refactor
or more realistically some better tool will come out and
I'll just put a "GO USE \[insert repo here] INSTEAD" banner here.
But before that, hope you could at least have some good experience with my creation.

### Install

[Cabal](https://www.haskell.org/cabal/) is required. Stack should also work I think

```sh
git clone 'https://github.com/Futarimiti/json-fmt'
cd json-fmt
cabal install
```

There's a messy JSON sample for you to try out:

```sh
json-fmt sample/sample.json
```

### Usage

| Command           | Description                     |
|-------------------|---------------------------------|
| `json-fmt`        | Format JSON input from stdin    |
| `json-fmt <FILE>` | Format JSON input from `<FILE>` |

Valid JSON input is expected.
Regardless of success or failure, the formatted result will be printed to stdout.
Unless `-v` flag is used, no other output (errors, logs) will be printed.

#### Flags

| Flag | Description |
|------|-------------|
| `-v` | verbose     |

Sorry, I didn't know optparse was a thing back then.

### Configuration

In order, `json-fmt` will look for configurations from:
* `$JSONFMT_CONFIG`
* `$XDG_CONFIG_HOME/json-fmt/config.json`

The config file must be a JSON object containing key-value pairs defining each option;
defaults will be used for any absent options.
See `default-config.json` for reference.

Upon failure to find or parse configurations, default configuration will be used.
Again see `default-config.json` for reference.

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

#### `spaceNBeforeArrayComma, spaceNAfterArrayComma :: Int`

number of spaces before and after a comma in a one-line array.
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

number of spaces used as paddings for a non-empty array;
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

#### `bracePaddingSpaceN :: Int`

number of spaces used as paddings for a non-empty object;
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

leaves an empty line at the end of document when true.

#### `newline :: String`

style of newline, usually either "\n" or "\n\r".

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
