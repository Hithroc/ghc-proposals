---
author: Artyom Kuznetsov
date-accepted: ""
proposal-number: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/270).

# Add unified namespace

Currently, in GHC we have separate namespaces for types and terms. This proposal suggests to add a new extension that will add support for a single, unified namespace. Having all of the identifiers in one namespace allows referring to them on any level (i.e. on type level, or term level).

## Motivation

Currently terms and types have their own namespaces. Because of that we can write code like:

```
id :: forall a. a -> a
id a = a
```

Here `a` on type level and `a` on term level are two different variables and they don't interact with each other in any way because they live in different namespaces.

For the same reason we can define constructors which have the same name as their
type:

```
data T = T
```

And we actually have a lot of built-in types that rely on this:

```
data [a] = [] | a : [a]
data (a, b) = (a, b)
data () = ()
data Proxy a = Proxy
newtype Identity a = Identity a
```

While this seems very convinient, this behavior actually introduces a lot of
problems:

- This is often confusing for beginners. They have to understand the difference between `[Int]` and `[5]`. Same with `data T = T`, the difference between the `T`s has to be understood.
- Type-level programming with `-XDataKinds` introduces namespace ambiguity which has to be resolved with `'` prefix, for example `[]` and `'[]`. In addition to that, `'` syntax is already used by Template Haskell.
- We don't currently have a way to refer to type constructors in terms which would be useful for Dependent Haskell.
  For example, consider this case with visible dependent quantification:
  ```
  data Proxy k where
    ProxyV :: forall a -> Proxy a

  data T = T

  p = ProxyV T
  ```
  Does `T` on the last line refer to the data constructor `T` or the type constructor `T`?

## Proposed Change Specification

* A new extension is added: `-XUnifiedNamespace`
* With the extension enabled, all new identifiers will belong to both both type and term namespaces.
* To disambiguate namespaces from modules that do not use `-XUnifiedNamespace` the extension will introduce a new syntax:
  ```
  impdecl   -> import [qualified] modid [as modid] [impspec]
             |
  ```
  is changed to
  ```
  impdecl   -> import [qualified] modid [namespace] [as modid] [impspec]
             |

  namespace -> data
             | type
  ```
   * With `data` specified in the import, only identifiers belonging to terms will be brought into the scope.
   * With `type` specified in the import, only identifiers belonging to types will be brought into the scope.
* With `-XUnifiedNamespace` enabled, `'` prefix syntax will no longer be used in `-XDataKinds` and will become Template Haskell exclusive and `''` prefix syntax will be removed.
* Built-in ambigous syntax constructs, such as lists, tuples and unit will be assumed as data constructors by default, with a new module defining unambiguous type definitions for them: `List`, `Tuple` and `Unit`:
  ```
  type List = []
  type Unit = ()
  type Tuple2 = (,)
  type Tuple3 = (,,)
  type Tuple4 = (,,,)
  {- ... -}

  type family Tuple (xs :: [Type]) :: Type where
    Tuple [] = Unit
    Tuple [a] = GHC.Tuple.Unit a
    Tuple [a,b] = Tuple2 a b
    Tuple [a,b,c] = Tuple3 a b c
    {- ... -}
  ```
* With `-XUnifiedNamespace` the renamer will look up the identifier in all of the namespaces. If an identifier is found in more than one namespace, then an ambigous namespaces error is reported.
* Without the extension, whenever the renamer looks up an identifier, it will look only in namespaces that the identifier belongs to.

## Examples

Here are some examples of the new syntax:

```
import Data.Proxy type as T
import Data.Proxy data as D

type T = T.Proxy Int

f :: T
f = D.Proxy

id :: forall a. a -> a
id = \a -> a -- this 'a' shadows the type level 'a'.
```

## Effect and Interactions

* With this proposal after enabling `-XUnifiedNamespace` any identifier can be referenced on any level, for example, this code becomes valid (it does not type check, however):
```
data T = MkT

f = T
```

## Costs and Drawbacks

- This proposal introduces new, potentially confusing for newcomers syntax
- Lookup logic becomes more complex and can become potentially slower because
  we have to look up a name in unified namespace first and then fall back to
  other namespaces
- It becomes impossible to define types which have constructors with the same
  name (e. g. `data T = T`) with the extension enabled.

## Alternatives

* Instead of changing the look up logic a new syntax could be introduced to specify whether an identifier should be looked up on terms or types. In fact, this [has been proposed before](https://github.com/ghc-proposals/ghc-proposals/pull/214). However, this results in noisy source code that is hard to read.

## Unresolved Questions

None

## Implementation Plan

I (Artyom Kuznetsov) will implement the change.

There's a work in progress prototype that can be found [here](https://gitlab.haskell.org/hithroc/ghc/tree/wip/unified-namespaces)
