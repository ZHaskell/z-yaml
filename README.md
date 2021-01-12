## Z-IO

This package is part of [Z](https://github.com/haskell-Z/Z) project, provides YAML reading & writing tools.

## Requirements

* A working haskell compiler system, GHC(>=8.6), cabal-install(>=2.4), hsc2hs.

* Tests need [hspec-discover](https://hackage.haskell.org/package/hspec-discover).

## Example usage

```haskell
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingStrategies, TypeApplication #-}

import           GHC.Generics
import qualified Z.Data.YAML as YAML
import           Z.Data.YAML (JSON)
import qualified Z.Data.Text as T

data Person = Person
    { name  :: T.Text
    , age   :: Int
    , magic :: Bool
    }
  deriving (Show, Generic)
  deriving anyclass JSON

> YAML.decode @[Person] "- name: Erik Weisz\n  age: 52\n  magic: True\n"
> Right [Person {name = "Erik Weisz", age = 52, magic = True}]
```
