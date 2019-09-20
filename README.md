# urbit-hob

[![Build Status](https://travis-ci.org/urbit/urbit-hob.svg?branch=master)](https://travis-ci.org/urbit/urbit-hob)
[![Hackage Version](https://img.shields.io/hackage/v/urbit-hob.svg)](http://hackage.haskell.org/package/urbit-hob)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

Utilities for phonetic base wrangling.

## What

Here you can primarily find functions for dealing with the "patp" and "patq"
*phonetic bases* used by Urbit.  The `@p` encoding is used for naming ships,
whereas the `@q` encoding is used for arbitrary data; they both uniquely
represent nonnegative integers (i.e. *atoms*) in a memorable and pronounceable
fashion.

The `@p` encoding is an *obfuscated* representation of an underlying atom, in
particular, hence the 'ob' in the library's name.

## Usage

The library exposes two families of functions, `patp` and `fromPatp`, and then
`patq` and `fromPatq`, for converting between representations appropriately.
You can render `{patp, patq}` values via the `render{Patp, Patq}` functions,
and parse them from Text via `parse{Patp, Patq}` respectively.

The useful `clan` and `sein` functions, for determining a ship's class and
(default) parent, are also exposed.

Here are some quick examples:

```
> :set -XOverloadedStrings
> import qualified Urbit.Ob as Ob
> let nidsut = Ob.patp 15663360
> let marzod = Ob.patq (Ob.fromPatp nidsut)
> Ob.renderPatp nidsut
"~nidsut-tomdun"
> Ob.renderPatq marzod
"~mun-marzod"
> Ob.fromPatp nidsut
15663360
> Ob.parsePatp "~nidsut-tomdun"
Right ~nidsut-tomdun
> Ob.clan nidsut
Planet
> Ob.sein nidsut
~marzod
```

## See also

* [urbit-ob](https://github.com/urbit/urbit-ob) -- JavaScript bindings
