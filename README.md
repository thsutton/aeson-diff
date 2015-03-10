Aeson Diff
==========

[![Build Status][badge]][status]

This is a small library for working with changes to JSON documents. It includes
a library and two executables in the style of diff(1) and patch(1).

Installing
----------

The `aeson-diff` package is written in Haskell and can be installed using the
Cabal package management tool.

For the command-line tools only, I recommend using Cabal's sandbox
functionality to avoid installing the associated libraries globally on your
system. This approach might look something like the following:

````bash
cd aeson-diff/
cabal sandbox init
cabal install --dependencies-only
cabal build
sudo mkdir -p /usr/local/bin
sudo cp dist/build/json-*/json-{diff,patch} /usr/local/bin/
````

If you want to use the library, use Cabal in a sandbox or not according to your
preference.

````bash
cabal sandbox init
cabal sandbox install ~/Downloads/aeson-diff/
````

Usage
-----

### `json-diff` command

The `json-diff` command compares two JSON documents and extracts a patch
describing the differences between the first document and the second.

### `json-patch` command

The `json-patch` command applies a patch describing changes to be made to
a JSON document.

### `aeson-diff` library

The `aeson-diff` library exports as single module: `Data.Aeson.Diff`. This
exports `diff` and `patch` functions which do exactly what might be expected:

- `diff :: Value -> Value -> Patch` examines source and target JSON `Value`s
and constructs a new `Patch` describing the changes.

- `patch :: Patch -> Value -> Value` applies the changes in a `Patch` to a JSON
`Value`.

[badge]: https://travis-ci.org/thsutton/aeson-diff.svg?branch=master
[status]: https://travis-ci.org/thsutton/aeson-diff
