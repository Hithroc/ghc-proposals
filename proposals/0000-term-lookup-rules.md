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

  There's ["Visible 'forall' in types of terms" proposal](https://github.com/ghc-proposals/ghc-proposals/pull/281) that describes this in more detail and depends on this proposal.

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
* When looking in the type-level namespace, `[a]` syntax means `[] a` when the `[]` type constructor is in scope, and a singleton list otherwise.
* The `(a,b)` syntax means `(,) a b`, where `(,)` is either the type constructor or the data constructor according to the name lookup rules in the given context. This also applies to tuples of other arities.
* `[]` and `()`, `(,)`, `(,,)`, ..., type constructors are no longer always visible and instead are exported in a new module `Data.BuiltInTypes` which is imported by default like `Prelude`. `-XNoImplicitBuiltInTypes` is introduced to change this behavior.
* Deprecate the `'` syntax of `-XDataKinds`, reserving this syntax for Template Haskell name quotation. Introduce a new warning, `-Wprime-data-kinds` which warns of the usage of the syntax.
* Deprecate the `''` syntax in Template Haskell. Introduce a new warning, `-Wdouble-prime-template-haskell` which warns of the usage of the syntax.
* Introduce a new warning, `-Wpun-bindings`, which warns the user when a binding binds a name  that would clash with another identifier if Haskell had a single unified namespace.
* Introduce a new warning, `-Wpuns`, which warns the user of using an identifier that would be referred to another identifier if Haskell had a single unified namespace.
* The deprecation strategy for the `'` syntax in `-XDataKinds` and `''` syntax in `-XTemplateHaskell` is the following:
  * In the next release add `-Wpun-bindings`, `-Wpuns`, `-Wprime-data-kinds` and `-Wdouble-prime-template-haskell` to `-Wcompat`.
  * Three releases from after this proposal is implemented add all four warnings to `-Wall`.
  * Seven releases from after this proposal is implemented deprecate the syntax and enable `-Wprime-data-kinds` and `-Wdouble-prime-template-haskell` by default.
  * Fifteen releases from after this proposal is implemented reject the syntax.

### Extended Proposed Change Specification

#### Unify Namespace Qualifiers

* Change `pattern` syntax in `-XPatternSynonyms` to use `data` keyword instead. Introduce `-Wpattern-import` warning that warns when `pattern` syntax is used. Add it to `-Wcompat`.
* If this proposal is accepted before the [fixities proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0008-type-infix.rst) is implemented, amend that proposal to use `data` instead of `value`.

#### Add `~` operator

* Remove `a ~ b` as a special syntax and introduce a `a ~ b` type operator and add it to `Data.BuiltInTypes`

## Examples

Here are some examples of the new syntax:

```haskell
import Data.Proxy type as T
import Data.Proxy data as D

type T = T.Proxy Int

f :: T
f = D.Proxy
```

In the examples on how warnings work, we will consider a currently working scenario and view it as if Haskell had a single unified namespace.

`-Wpuns` example #1:
```haskell
module A where { data A = T }
module B where { data T = X }

module C where

import A
import B

f = T
```

If Haskell had a single unified namespace referring to `T` would result in ambiguity (is it `A.T` or `B.T`?), thus this should trigger the warning.

`-Wpuns` example #2:
```haskell
a = 15
f :: a -> a
```
If Haskell had a single unified namespace, `a` instead of referring to implicitly bound type-variable `a` would refer to `a` on the type-level. This means that punning is used and should trigger the warning.

On the contrary:
```haskell
a = 15

f :: forall a. a -> a
```
Does not use punning because if Haskell had a single unified namespace, explicitly bound type variable `a` would shadow the top-level `a`.

`-Wpuns` example #3:
```haskell
{-# LANGUAGE ScopedTypeVariabels #-}
a = 15

f :: forall a. a -> a
f = \a -> (a :: a)
--              ^ warning here
```
In all of the `a` uses except for the last one there is no punning, because if Haskell had a single unified namespace, in the type signature, top-level `a` would be shadowed by explicitly bound type variable `a`, and in the expression `a` variable bound in the lambda would shadow the type variable. In the very last case, however, currently the `a` would refer to the type variable, but if Haskell had a single namespace it would refer to the data variable. Thus the warning is triggered.

`-Wpun-bindings` example #1:
```haskell
id :: t -> t
id @a a = a
```
Here, when term level `a` is bound it would conflict with the type level `a` if Haskell had a single namespace, thus triggering the warning. This behavior is similar to conflicting definition error for `f b b = ...`

```haskell
Test.hs:1:3: error:
    • Conflicting definitions for 'b'
      Bound at: Test.hs:1:3
                Test.hs:1:5
    • In an equation for 'f'
```

On the contrary, the code below is fine, similarly to `-Wpuns` example #2, the `a` is shadowed instead.
```haskell
f :: t -> ()
f @a = \a -> ()
```

`-Wpun-bindings` example #2:
```haskell
data T = T
```
If Haskell had a single unified namespace, type constructor `T` and data constructor `T` would conflict, thus this should trigger the warning.

`-Wpun-bindings`, example #3
```haskell
data T = MkT
data B = T | F
```
Even though type constructor `T` and data constructor `T` are defined in different definitions, they would still cause a conflict, same as example #2.

`-Wpun-bindings`, example #4
```haskell
data J = Bool
```
This should not cause the warning, because `Bool` defined here would not conflict with `Bool` imported from `Prelude`, it would shadow it instead:
```haskell
import Prelude (Bool)
data Bool -- no conflict
```

## Effect and Interactions

* There is an asymmetry with what `-XDataKinds` does, as `-XDataKinds` only promotes data constructors to the type level and doesn't promote variables. On the contrary, new lookup rules let users reference type variables at the term-level.
  ```haskell
  f :: forall a. a -> Int
  f _ = sizeOf a -- passes the renamer
  ```
  Compare this to `-XDataKinds`:
  ```haskell
  {-# LANGUAGE DataKinds #-}
  a = 5
  f :: Proxy a -> Proxy a -- 'a' here does not refer to the term-level 'a' and is implicitly quantified 
  ```
  In this example `a` in `f`'s type signature does not refer to the `a` defined above and instead (implicitly) becomes a type variable:
  ```haskell
  f :: forall a. Proxy a -> Proxy a
  ```
 * This code will now pass the renamer, but will still be rejected by the type checker:
   ```haskell
   data T = MkT
   f = T
   ```

## Costs and Drawbacks

* This proposal introduces new, potentially confusing for newcomers syntax, however, the newcomers can just look up the syntax.
* After `'` syntax in `-XDataKinds` is deprecated it becomes impossible to refer to a punned name on type level:
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

There's a merge request with `-Wpuns` warning implementation which can be found here [here](https://gitlab.haskell.org/ghc/ghc/merge_requests/2044).
