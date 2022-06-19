# json-fmt

Searched throught loads of JSON formatters without finding single one supporting comma-leading style so I wrote this

To install and use:

1.  `git clone` this repo
2.  Within `json-fmt/`, run `cabal install`; get [Cabal](https://cabal.readthedocs.io/en/3.6/) if you don't have one
3.  `~/.cabal/bin/json-fmt` should've been generated as an executable; if you haven't, I suggest adding `~/.cabal/bin/` to `$PATH`
4.  `json-fmt` accepts input json as either a commandline arg or stdin; I put a `sample.json` for testing:
    ```txt
    cat sample/sample.json | json-fmt
    ```
    The result should be go to stdout unless otherwise redirected.

`json-fmt` expects valid JSON input, and will not attempt to format inputs detected to be invalid.

### Vim integration

Add to ftplugin file for JSON:

```vim
setlocal formatprg=json-fmt
```
