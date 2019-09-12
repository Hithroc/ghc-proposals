---
author: Artyom Kuznetsov
date-accepted: ""
proposal-number: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/270).

# Add unified namespace

Currently, in GHC we have separate namespaces for types and terms. This proposal suggests adding a new non-intrusive extension that will change lookup rules allowing users to refer to terms or types equally as if they were in one unified namespace (i.e. on type level, or term level).

## Motivation

Currently terms and types have their own namespaces. Because of that we can write code like:

```haskell
id :: forall a. a -> a
id a = a
```

Here `a` on type level and `a` on term level are two different variables and they don't interact with each other in any way because they live in different namespaces.

For the same reason we can define constructors which have the same name as their
type:

```haskell
data T = T
```

And we actually have a lot of built-in types that rely on this:

```haskell
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
  ```haskell
  data Proxy k where
    ProxyV :: forall a -> Proxy a

  data T = T

  p = ProxyV T
  ```
  Does `T` on the last line refer to the data constructor `T` or the type constructor `T`?

  Here's a more real-world example, let's look at `sizeOf` in `Foreign.Storable`:
  ```haskell
  sizeOf :: Storable a => a -> Int
  ```
  This function takes an `Storable a => a` value as an argument when in reality it doesn't really need an actual value, it only cares about the type of it. Currently to use it we have to provide it with a value, and the most convinient way to do it is to use `undefined`:
  ```
  sizeOf (undefined :: MyType)
  ```
  Ideally, we want to be able to do something like this instead:
  ```haskell
  sizeOf :: forall a -> Storable a => Int

  sizeOf MyType

  -- or even

  f :: forall a. something
  f = ... sizeOf a ...
  ```
  Of course right now we have an option to use `@` syntax to do that:
  ```haskell
  sizeOf :: forall a. Storable a => Int

  sizeOf @MyType
  ```
  But that argument is implicit and not required. If the user forgets to specify it, GHC will try to infer it which will lead to bad error messages like "could not deduce" that do not really hint to the problem is and how to fix it.

These problems could be solved by introducing a new extension that would change the identifier lookup rules: all identifiers no matter where they are encountered would be looked up in all namespaces. New identifiers would appear as if they're in both types and terms namespaces. To keep compatibility with modules that do not use the extension and allow punning, a new syntax would be introduced allowing to import only terms or only types from a module.

The code that doesn't use the extension is not affected and will not perceive any changes.

## Proposed Change Specification

* A new extension is added: `-XUnifiedNamespace`
* With the extension enabled, all new identifiers will belong to both type and term namespaces.
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
* Built-in ambiguous syntax constructs, such as lists, tuples and unit will be assumed as data constructors by default, with a new module defining unambiguous type definitions for them: `List`, `Tuple` and `Unit`:
  ```haskell
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
* Without the extension, the names are looked up in the namespace that corresponds to the occurence context (type or term), same as today.

## Examples

Here are some examples of the new syntax:

```haskell
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
```haskell
data T = MkT

f = T
```

## Costs and Drawbacks

- This proposal introduces new, potentially confusing for newcomers syntax, however, the newcomers can just look up the syntax.
- Lookup logic becomes more complex and can become potentially slower because
  we have to look up a name in unified namespace first and then fall back to
  other namespaces
- It becomes impossible to define types which have constructors with the same
  name (e. g. `data T = T`) with the extension enabled.

## Alternatives

* Instead of changing the lookup logic a new syntax could be introduced to specify whether an identifier should be looked up on terms or types. In fact, this [has been proposed before](https://github.com/ghc-proposals/ghc-proposals/pull/214). However, this results in noisy source code that is hard to read. For example: `a data.: b data.: data.[]`.

* [Dependent Haskell page on GHC wiki](https://gitlab.haskell.org/ghc/ghc/wikis/dependent-haskell#parsingnamespace-resolution) suggest to use `'` to "flip" the default namespace from one to another, for example on types it would mean terms namespace and on terms it would mean type namespace. While this is not as noisy as the previous alternative, context-dependent syntax is generally more confusing to read and it still conflicts with Template Haskell.

## Unresolved Questions

None

## Implementation Plan

I (Artyom Kuznetsov) will implement the change.

There's a work in progress prototype that can be found [here](https://gitlab.haskell.org/hithroc/ghc/tree/wip/unified-namespaces)
