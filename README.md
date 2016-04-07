Aeson Diff
==========

[![Build Status][badge]][status]

This is a small library for working with changes to JSON documents. It
includes a library and two executables in the style of diff(1) and
patch(1). Patches are themselves JSON Patch documents as specified in
[RFC 6902][3].

Installing
----------

The `aeson-diff` package is written in Haskell and can be installed using the
[Cabal][1] package management tool, [stack][2], or something similar.

````bash
stack install aeson-diff
````

The command-line tools can then be executed using stack:

````
stack exec json-diff -- ....
stack exec json-patch -- ....
````

If you prefer to use Cabal, something like this might do the trick:

````bash
cd aeson-diff/
cabal sandbox init
cabal install --dependencies-only
cabal build
sudo mkdir -p /usr/local/bin
sudo cp dist/build/json-*/json-{diff,patch} /usr/local/bin/
````

Usage
-----

### Patch format

`aeson-diff` supports the JSON Patch format described in
[RFC 6902][3].

### json-diff command

The `json-diff` command compares two JSON documents and extracts a patch
describing the differences between the first document and the second.

````
Usage: json-diff [-o|--output OUTPUT] FROM TO
Generate a patch between two JSON documents.

Available options:
    -h,--help                Show this help text
````

### json-patch command

The `json-patch` command applies a patch describing changes to be made to
a JSON document.

````
Usage: json-patch [-o|--output OUTPUT] PATCH FROM
Generate a patch between two JSON documents.

Available options:
  -h,--help                Show this help text
  -o,--output OUTPUT       Destination for patched JSON.
  PATCH                    Patch to apply.
  FROM                     JSON file to patch.
````

### aeson-diff library

The `aeson-diff` library exports as single module: `Data.Aeson.Diff`. This
exports `diff` and `patch` functions which do exactly what might be expected:

- `diff :: Value -> Value -> Patch` examines source and target JSON `Value`s
and constructs a new `Patch` describing the changes.

- `patch' :: Patch -> Value -> Result Value` applies the changes in a
`Path` to a JSON `Value`.

- `patch :: Patch -> Value -> Value` applies the changes in a `Patch` to a JSON
`Value`. If an error results then an exception is thrown.

For more complete information, see the [documentation][docs].

[badge]: https://travis-ci.org/thsutton/aeson-diff.svg?branch=master
[status]: https://travis-ci.org/thsutton/aeson-diff
[docs]: https://hackage.haskell.org/package/aeson-diff/docs/Data-Aeson-Diff.html
[1]: https://wiki.haskell.org/Cabal-Install
[2]: http://haskellstack.org/
[3]: http://tools.ietf.org/html/rfc6902
