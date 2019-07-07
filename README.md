A set of tools to work with the [ZM](http://quid2.org/docs/ZhengMing.pdf) data definition language.

ZM types and values can be represented in a number of ways:
  * free form text
  * canonical text
  * canonical binary
  * equivalent types and values in a number of programming languages (Haskell, Typescript, etc.)

This package provides a set of tools to convert across some of these representation:
  * Parser (Free form textual representation of ZM types and values to their Haskell model)
  * Encoder (Free form textual representation of ZM values of a known type into their canonical binary representation in [Flat](http://quid2.org/docs/Flat.pdf)
  * Code generators (Internal Haskell model of ZM types to Haskell, TypeScript and ZM source code)
