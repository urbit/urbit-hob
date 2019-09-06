# urbit-hob

[![Build Status](https://secure.travis-ci.org/urbit/urbit-hob.png)](http://travis-ci.org/urbit/urbit-hob)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Utilities for phonetic base wrangling.

## What

Here you can primarily find functions for dealing with the *phonetic bases*
used by Urbit.  The `@p` encoding is used for naming ships, while the `@q`
encoding is used for representing arbitrary data in a memorable and
pronounceable fashion.

The `@p` encoding is an *obfuscated* representation of an underlying 32-bit
number, in particular, hence the 'ob' in the library's name.

