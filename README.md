## querystring-pickle

[![Build Status](https://secure.travis-ci.org/brendanhay/querystring-pickle.png)](http://travis-ci.org/brendanhay/querystring-pickle)


## Table of Contents

* [Introduction](#introduction)
* [Examples](#examples)
* [Caveats](#caveats)
* [Compatibility](#compatibility)
* [Contributing](#contributing)
* [Licence](#licence)


## Introduction

> TODO


## Examples

> TODO


## Caveats

An `IsQuery` instance for `Maybe a` is supplied and it is left
up to the consumer of the library to implement them. (Apologies for forcing
orphan-instances on anybody.)

The reasoning is the desired behaviour of the de/serialisers for both types is
ambiguous, take the following naive instance declarations:

```haskell
instance IsQuery a => IsQuery (Maybe a) where
    queryPickler = qpOption queryPickler

data A = A { aInt :: Int } deriving (Show, Generic)
data B = B { bA :: Maybe A } deriving (Show, Generic)
data C = C { cB :: B } deriving (Show, Generic)

instance IsQuery A
instance IsQuery B
instance IsQuery C

let b = C $ B Nothing
```

Running `toQuery` / `fromQuery` on the example yields:

```haskell
ghci: let bQry = toQuery b
[]

ghci: fromQuery bQry :: Either String C
Left "qpElem: non-locatable - B - List []"
```

If data type `B` has a second non-optional field, the
de/serialisation will succeed.

This is due to the overly simple underlying rose tree used
as the intermediate data structure for query transforms.
Something that will hopefully be fixed in a future release.


## Compatibility

Due to the dependency on `GHC.Generics` a version of `base 4.6` or higher is required.


## Contributing

For any problems, comments or feedback please create an issue [here on GitHub](github.com/brendanhay/querystring-pickle/issues).


## Licence

querystring-pickle is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/)
