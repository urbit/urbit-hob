# urbit-hob

[![Build Status](https://travis-ci.org/urbit/urbit-hob.svg?branch=master)](https://travis-ci.org/urbit/urbit-hob)
[![Hackage Version](https://img.shields.io/hackage/v/urbit-hob.svg)](http://hackage.haskell.org/package/urbit-hob)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

Utilities for phonetic base wrangling.

## What

Here you can primarily find functions for dealing with the "patp" *phonetic
base* used by Urbit.  The `@p` encoding is used for naming ships; it uniquely
represents a nonnegative integer (i.e. an *atom*) in a memorable and
pronounceable fashion.

The `@p` encoding is an *obfuscated* representation of an underlying atom, in
particular, hence the 'ob' in the library's name.

## Usage

The library exposes two functions, `patp` and `fromPatp`, for converting
between representations.  You can render a `patp` value via the `render`
function, and parse one from Text via `parse`.

Here's a quick example:

```
> :set -XOverloadedStrings
> import qualified Urbit.Ob as Ob
> let nidsut = Ob.patp 15663360
> Ob.render nidsut
"~nidsut-tomdun"
> Ob.fromPatp nidsut
15663360
> Ob.parse "~nidsut-tomdun"
Right ~nidsut-tomdun
```

## See also

* [urbit-ob](https://github.com/urbit/urbit-ob) -- JavaScript bindings
