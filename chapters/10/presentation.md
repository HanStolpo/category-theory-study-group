---
title: "Category theory study group - Chapter 10"
author: Handré Stolp
date: May 15, 2018
slideLevel: 1
incremental: true
slideVariant: RevealJsSlides
designWidth: 1024
designHeight: 768
margin: 0.1
minScale: 0.1
maxScale: 3
incremental: false
---
<style>
  .reveal section {
    font-size: 0.65em;
  }
  .reveal h1{
    font-size: 1.5em;
    margin-top: -30px;
  }
</style>

Natural transformation
============
<div style="width: 50%;float: left;">
* Map functors to functors </br>
  $\alpha_{a} = Fa \to Fa$
* Preserving **natuarality condition**</br>
  $G⨍∘α_{a} = α_{b}∘F⨍$</br>
  where</br>
    $⨍ = α \to β$</br>
    $F⨍ = Fa \to Fb$</br>
    $G⨍ = Ga \to Gb$</br>
    $α_{a} = Fa \to Gb$</br>
    $α_{b} = Fb \to Gb$</br>
</div>
<div style="width: 50%;float: right;">
![](/images/naturality.jpg)
</div>

Natural transformation
============
<div style="width: 50%;float: left;">
Its **stringent**. 

If $F⨍$ is invertible then $α_{b}$ is determined in terms of $α_{a}$ </br>
$α_{b} = (G⨍)∘α_{a}∘(F⨍)^{-1}$
</div>
<div style="width: 50%;float: right;">
![](/images/transport.jpg)
</div>

Natural transformation
============
<div style="width: 50%;float: left;">
**Viewed componentwise** could say:

  * maps objects to morphisms
  * maps morphisms to commuting squares
</div>
<div style="width: 50%;float: right;">
![](/images/comuting-square.jpg)
</div>

Polymorphic Functions
============

With **parametric polymorphism** as in Haskell any function between functors
polymorphic in the element is a natural transformation.

```haskell
-- Always a natural transformation
alpha :: F a -> G a
-- naturality always holds
(fmap @ G) f . alpha = alpha . (fmap @ F) f
```

This is due to *"theoroms for free"*


Even functions to values can be seen as natural transformations

```haskell
-- Normal version
length :: [a] -> Int

-- Version recast as natural transformation
lengthF :: [a] -> Const Int a
```

Contravariant Functors
==================

* Equivalent to covariant functors in the opposite category
* Not natural transformations in **Hask**
* They do satisfy the opposite naturality condition in **Hask**
  ```haskell
  (contramap @ G) . alpha = alpha . (contramap @ F)
  ```

Functor Category
================

<div style="width: 50%;float: left;">
* Called $[C,D]$ or $D^C$
* One category of functors for each pair of categories $C$ an $D$
* Objects are functors from $C$ to $D$
* Morphism are natural transformations between the functors</br>
  $α_{a} = Fa \to Ga$</br>
  $β_{a} = Ga \to Ha$</br>
  $β_{a}∘α_{a} = Fa \to Ha$</br>
  $(β⋅α) _{a} = β_{a}∘α_{a}$</br>
</div>

<div style="width: 50%;float: right;">
  ![](/images/vertical.jpg)
</div>

Functor Category
================
<div style="width: 50%;float: left;">
* composition of natural transformation is associative because
  there component morphisms are asociative.
* Naturality holds for the composition</br>
  $H⨍ ∘ (β⋅α)_{a} = (β⋅α)_{b} ∘ F⨍$</br>
* Indentity natural transformation $1_F$ whose components
  are the identity morphisms.</br>
  $id_{F_a} = Fa \to Fa$
</div>

<div style="width: 50%;float: right;">
![](/images/verticalnaturality.jpg)
</div>

Vertical composition
================
<div style="width: 50%;float: left;">
* $β⋅α$
* composition of natural transformation within the same functor category $D^C$
</div>

<div style="width: 50%;float: right;">
![](/images/vertical2.jpg)
</div>

2-Categories
=============

<div style="width: 50%;float: left;">
* **Cat** category of *small* categories
* Hom-set in **Cat** set of functors
* Functors form a category
* In **Cat** as 2-category
  * Objects: *Small* categories
  * 1-morphisms: Functors between categories
  * 2-morphisms: Natural transformations between Functors (morphisms of morphisms)
* Hom-category $D^C$ instead of Hom-set between categories $C$ and $D$
  * Regular functor composition</br>
    $F$ from $D^C$</br>
    $G$ from $E^D$</br>
    $G∘F$ in $E^C$
  * Also composition inside each Hom-category, vertical composition of natural transformations
</div>

<div style="width: 50%;float: right;">
![](/images/8_cat-2-cat.jpg)
</div>

Horizontal Composition
=======================

* 2 Functors 1-morhpisms in **Cat** </br>
  $F = C \to D$</br>
  $G = D \to E$
* their composition </br>
  $G∘F= C \to E$
* 2 Natural transformations $α$ and $β$ acting on $F$ and $G$ </br>
  $α = F \to F'$</br>
  $β = G \to G'$</br>
  we can't apply vertical composition to $α$ and $β$

Horizontal Composition </br> $β∘α = G∘F \to G'∘F'$
=======================
<div style="width: 50%;float: left;">
Can we construct natural transformation between $G∘F$ and $G'∘F'$ ?

* $a$ splits into $Fa$ and $F'a$
* $α_a$ connects $Fa$ and $F'a$ </br>
  $α_a = Fa \to F'a$
* two objects split further into 4 objects </br>
  $G(Fa), G'(Fa), G(F'a), G'(F'a)$
* 4 Morhpisms forming a square
  * 2 from natural transfrom $β$ </br>
    $β_{Fa} = G(Fa) \to G'(Fa)$ </br>
    $β_{F'a} = G(F'a) \to G'(F'a)$ </br>
  * other 2 are images of $α$ under $G$ and $G'$ </br>
    $G_{α_{a}} = G(Fa) \to G(F'a)$</br>
    $G'_{α_{a}} = G'(Fa) \to G'(F'a)$</br>
</div>

<div style="width: 50%;float: right;">
* Need to find path $G(Fa) \to G'(F'a)$ mapping $G∘F \to G'∘F'$
  * there are two paths </br>
    $G'α_a∘β_{Fa}$ </br>
    $β_{F'a}∘Gα_a$ </br>
  * but they are equal because of the naturality condition of $β$
![](/images/9_horizontal.jpg)
</div>


Horizontal Composition
=======================

<div style="width: 50%;float: left;">
* ?? Natural transformations collapse all equivalent functors between categories `C` and `D`
  and horizontal composition is the composition of these collapsed functors ??
* **Interchage law** for horizontal composition </br>
  $(β' ⋅ α') ∘ (β ⋅ α) = (β' ∘ β) ⋅ (α' ∘ α)$
</div>

<div style="width: 50%;float: right;">
![](/images/sideways.jpg)
</div>

Question 1
============

Define a natural transformation from the Maybe functor to the list functor.
Prove the naturality condition for it.

Answer 1
============

Given the following

```haskell
-- definition of Maybe functor
data Maybe a = Maybe a | Nothing
fmapMaybe _ Nothing = Nothing
fmapMaybe f (Just a) = Just (f a)
instance Functor Maybe where fmap = fmapMaybe

-- definition of list functor in pseudo haskell
data [a] = [] | a : [a]
fmapList _ [] = []
fmapList f (a:tail) = f a : fmapList f tail
instance Functor [] where fmap = fmapList
```

Answer 1
============

```haskell
-- Natural transform Maybe to list
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- Natuarality condition
fmapList f . maybeToList = maybeToList . fmapMaybe f
-- LHS at Nothing
LHS  = (fmapList f . maybeToList) Nothing
     = fmapList f (maybeToList Nothing)   -- function compisition
     = fampList f []                      -- maybeToList
     = []                                 -- fmapList
-- RHS at Nothing
RHS  = (maybeToList f . fmapMaybe) Nothing
     = maybeToList (fmapMaybe f Nothing)   -- function composition
     = maybeToList Nothing                 -- fmapMaybe
     = []                                  -- maybeToList
-- Naturality hodlds at Nothing
LHS = RHS
-- LHS at Just a
LHS  = (fmapList f . maybeToList) (Just a)
     = fmapList f (maybeToList (Just a))   -- function compisition
     = fampList f [a]                      -- maybeToList
     = [f a]                               -- fmapList
-- RHS at (Just a)
RHS  = (maybeToList f . fmapMaybe) (Just a)
     = maybeToList (fmapMaybe f (Just a))   -- function composition
     = maybeToList (Just (f a))             -- fmapMaybe
     = [f a]                                -- maybeToList
-- Naturality hodlds at Just a
RHS = LHS
```

Question 2
===========

Define at least two different natural transformations between Reader () and
the list functor. How many different lists of () are there?

Answer 2
===========

```haskell
newtype Reader e a = Reader (e -> a)

readerToList1 :: Reader () a -> [a]
readerToList1 (Reader g) = [g ()]

readerToList2 :: Reader () a -> [a]
readerToList2 (Reader g) = [g (), g ()]

readerToListN :: Reader () a -> [a]
readerToListN (Reader g) = g () : readerToListN (Reader g)
```

There are infinitely many lists of `()` since lists could be infinitely long and
each different length list of `()` is a different list.

Question 3
===========

Continue the previous exercise with Reader Bool and Maybe.

Answer 3
==========

```haskell
readerToMaybeTrue :: Reader Bool a -> Maybe a
readerToMaybeTrue (Reader g) = Just (g True)

readerToMaybeFalse :: Reader Bool a -> Maybe a
readerToMaybeFalse (Reader g) = Just (g False)

readerToMaybeNothing :: Reader Bool a -> Maybe a
readerToMaybeNothing _ = Nothing
```

There are three natural transformations from `Reader Bool` to `Maybe`

Question 4
==========

Show that horizontal composition of natural transformation satisfies the naturality
condition (hint: use components). It’s a good exercise in diagram chasing.

Answer 4
=========

<div style="width: 35%;float: left;font-size:0.6em;">
* red is  $(β∘α)_a = G_a∘F_a \to G'_a∘F'_a$
* blue is $(β∘α)_b = G_b∘F_b \to G'_b∘F'_b$
* khaki is $G∘F_F$
* green is $G'∘F'_F$
* diagram chasing shows
  * $(G'∘F)' ∘ (β∘α) = (β∘α) ∘ (G∘F)$
</div>

<div style="width: 65%;float: right;">
![](/images/horizontal-naturality.svg)
</div>

Question 5
==========

Write a short essay about how you may enjoy writing down the evident diagrams needed to
prove the interchange law.

Answer 5
=========

I would enjoy writing a program that generates and checks the diagrams needed to prove the interchange law.

Question 6
=========

Create a few test cases for the opposite naturality condition of transformations between different
Op functors. Here’s one choice:

```haskell
  op :: Op Bool Int
  op = Op (\x -> x > 0)
```

and

```haskell
  f :: String -> Int
  f x = read x
```

Answer 6
========

```haskell
module Main where

class Contravariant f where
    contramap :: (b -> a) -> f a -> f b

newtype Op r a = Op (a -> r)
runOp (Op f) = f

instance Contravariant (Op r) where
    contramap f (Op g) = Op (g . f)


op :: Op Bool Int
op = Op (\x -> x > 0)

f :: String -> Int
f x = read x

alpha :: Op Bool a -> Op String a
alpha (Op f) = Op (const "2")

main = do
  putStrLn $ "(contramap f . alpha) \"\" == 2" ++
    show (runOp((contramap f . alpha) op) "")
  putStrLn $ "(alpha . contramap f) \"\" == 2" ++
    show (runOp((alpha . contramap f ) op) "")
  putStrLn $ "contramap f . alpha  = alpha . contramap f " ++
    show ((runOp((contramap f . alpha) op) "") == (runOp((alpha . contramap f ) op) "") )
```

```
> (contramap f . alpha) "" == 2"2"
> (alpha . contramap f) "" == 2"2"
> contramap f . alpha  = alpha . contramap f True
```
