---
author: Artyom Kuznetsov
date-accepted: ""
proposal-number: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/270).

# Extend term-level lookup rules

Currently, when GHC looks up a name, it only looks it up in certain namespace depending on the context: in the type namespace for types and the data namespace for terms. There is `-XDataKinds` that alters this behavior for types, allowing users to refer to data constructors at the type level. This proposal suggests adding similar behavior for terms, allowing users to refer to type-level identifiers in terms.

## Motivation

Currently, terms and types have their own namespaces. Because of that, we can write code like:

```haskell
id :: forall a. a -> a
id a = a
```

Here `a` at the type level and `a` at the term level are two different variables and they don't interact with each other in any way because they live in different namespaces.

For the same reason, we can define data constructors and type constructors whose names coincide:

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

While this behavior seems very convenient, it introduces a lot of problems:

* Beginners need to learn that while `[Int]` and `[5]` look similar, they denote completely different concepts. Same with `data T = T`, the difference between the `T`s has to be understood.
* Type-level programming with `-XDataKinds` introduces namespace ambiguity which has to be resolved with the `'` prefix, for example `[]` and `'[]`. In addition to that, the `'` syntax is already used by Template Haskell.
 We don't currently have a way to refer to type constructors in terms which would be useful for Dependent Haskell.
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
  This function takes a `Storable a => a` value as an argument when in reality it doesn't really need an actual value, it only cares about its type. Currently, to use it we have to provide it with a value, and the most convenient way to do it is to use `undefined`:
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
  But that argument is implicit and not required. If the user forgets to specify it, GHC will try to infer it which will lead to bad error messages like "could not deduce" that does not really hint to what the problem is and how to fix it.

These problems could be solved by changing the identifier lookup rules in a non-intrusive way: whenever we lookup an identifier at the term level we can first search for it in the data namespace, and if it wasn't found there, we could search in other namespaces as a fall-back. To keep compatibility with modules that use punning, a new syntax could be introduced allowing to import only terms or only types from a module.

## Proposed Change Specification

* The name lookup rules are extended: when looking up term-level identifier fails, look for a type-level identifier as a fall-back.
* To disambiguate the namespaces of identifiers from modules that use punning, the `-XExplicitNamespaces` extension is extended with new syntax:
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
   * With `data` specified in the import, only identifiers belonging to the data namespace will be brought into the scope.
   * With `type` specified in the import, only identifiers belonging to the type namespace will be brought into the scope.
* A new module `Data.Type` is introduced which defines unambiguous type definitions for built-in syntax constructors: `List`, `Tuple` and `Unit`:
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
* The `[a]` syntax means `[] a` when the `[]` type constructor is in scope, and a singleton list otherwise.
* The `(a,b)` syntax means `(,) a b`, where `(,)` is either the type constructor or the data constructor according to the name lookup rules in the given context. This also applies to tuples of other arities.
* `[]` and `()`, `(,)`, `(,,)`, ..., type constructors are no longer visible by default and instead are exported in a new module `Data.BuiltInTypes` which is imported by default like `Prelude`. `-XNoImplicitBuiltInTypes` is introduced to change this behavior.
* Deprecate the `'` syntax of `-XDataKinds`, reserving this syntax for Template Haskell name quotation.
* Deprecate the `''` syntax in Template Haskell.
* Introduce a new warning, `-Wpun-declaration`, which warns the user when a declaration introduces a type constructor, type variable or a data constructor that clashes with an identifier in another namespace.
* Introduce a new warning, `-Wpuns`, which warns the user of using an identifier that is found in both namespaces.
## Examples

Here are some examples of the new syntax:

```haskell
import Data.Proxy type as T
import Data.Proxy data as D

type T = T.Proxy Int

f :: T
f = D.Proxy
```

Warning examples:

```haskell
data T

data A = T -- triggers -Wpun-declaration

f = T -- triggers -Wpuns because T exists both in the type and the data namespaces
```

## Effect and Interactions

* There is an asymmetry with what `-XDataKinds` does, as `-XDataKinds` only promotes data constructors to the type level and doesn't promote variables. On the contrary, new lookup rules let users reference type variables at the term-level. 
  ```haskell
  f :: forall a. a -> Int
  f _ = sizeOf a -- valid code
  ```
  Compare this to `-XDataKinds`:
  ```haskell
  {-# LANGUAGE DataKinds #-}
  a = 5
  f :: Proxy a -> Proxy a -- 'a' here does not refer to the term-level 'a' and is implicitly quantified 
  ```

## Costs and Drawbacks

* This proposal introduces new, potentially confusing for newcomers syntax, however, the newcomers can just look up the syntax.
* It becomes impossible to refer to a punned name on type level:
  ```haskell
  data T = T
  
  f :: T -- Will always refer to the type constructor
  f = T -- Will always refer to the data constructor
  ```
  The user can circumvent this by either not using punning, or if the punned name is imported, using a namespace qualified import:
  ```haskell
  import Data.Proxy type as T
  import Data.Proxy data as D
  
  f = T.Proxy -- Refers to the type constructor
  h :: D.Proxy -- Refers to the data constructor
  ```

## Alternatives

* Instead of changing the lookup logic a new syntax could be introduced to specify whether an identifier should be looked up on terms or types. In fact, this [has been proposed before](https://github.com/ghc-proposals/ghc-proposals/pull/214). However, this results in noisy source code that is hard to read. For example: `a data.: b data.: data.[]`.

* [Dependent Haskell page on GHC wiki](https://gitlab.haskell.org/ghc/ghc/wikis/dependent-haskell#parsingnamespace-resolution) suggest using `'` to "flip" the default namespace from one to another, for example on types it would mean terms namespace and on terms it would mean type namespace. While this is not as noisy as the previous alternative, context-dependent syntax is generally more confusing to read and it still conflicts with Template Haskell.

## Unresolved Questions

None

## Implementation Plan

I (Artyom Kuznetsov) will implement the change.

There's a work in progress prototype that can be found [here](https://gitlab.haskell.org/hithroc/ghc/tree/wip/unified-namespaces)
