---
title: "Category theory study group - Chapter 13"
author: Handré Stolp
date: Oktober 17, 2018
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
  .reveal code {
    background: antiquewhite;
  }
</style>

Monoid Set Theoretic
============

<div style="width: 50%;float: left;">
* Binary operator that map pairs of elements to other elements. </br>
  `u :: (m,m) -> m` </br>
        or </br>
  `* :: m -> m -> m`
* There is an identity element `e` combined with any other element maps to
  that element. </br>
  `e * a = a * e = a`
* The operations is associative, the order of reducing multiple oprations
  does not matter. </br>
  `(a * b) * c = a * (b * c)`

Another way to see it is, given any two elements map them to another element
satisfying the laws. This identification is monoid dependent.
</div>

<div style="width: 50%;float: right;">
* Multiple pairs could map to the same element </br>
  *Natural numbers under summation* </br>
  <span style="font-size: smaller"> `3 = 0 + 3 = 1 + 2 = 2 + 1 = 3 + 0` </span>
* Some identifications are bound to the specific monoid. </br>
  `0 + 3 = 2 + 1`
* Other identifications are necessary to satisfy the laws. </br>
  `0 + 3 = 3 + 0`
* Free monoid is the monoid where the minimum amount of identifications are
  required to satisfy the laws.</br>
       or</br>
  The least amount of structure required to satisfy the monoid laws.
</div>

Constructing the free monoid
============

<div style="width: 48%;float: left;">
* Start with a set of generators `{a,b}`
* monoid requires an identity element so add it to the set `{a,b,e}`
* the possible pairs are then</br>
  `e * a`       `a * b`</br>
  `b * e`       `b * a`</br>
  `a * e`</br>
* need to identify the results of `e` pairs for monoid laws</br>
  `e * a = a`       `a * e = a`</br>
  `b * e = b`       `e * b = b`</br>
* but the results of the other pairs are not important so extend the set
  with them </br>
  `{a, b, e, aa, bb, ab, ba}`
* the new set excluding the identity element combinations have the following
  new pairs. </br>
  `a * aa`       `b * ab`</br>
  `a * bb`       `b * ba`</br>
  `a * ab`       `ab * a`</br>
  `a * ba`       `ba * a`</br>
  `b * aa`       `ab * b`</br>
  `b * bb`       `ba * b`</br>
</div><div style="width: 52%;float: right;">
* because of associativity law we have to identify some of the new pairs as
  the same </br>
  `a * bb = ab * b`     `b * aa = ba * a` </br>
  `a * ba = ab * a`     `b * ab = ba * b` </br>
* extend the set by adding all the unique pairs</br>
  <span style="font-size: 50%"> `{a, b, e, aa, bb, ab, ba, aaa, bbb, aab, aba, bba, baa, abb}` </span>
* repeat infinitely building the set
</br>
</br>
</br>
</br>
* This set is the set of lists of all the possible combinations of the
  original generators, where
  * identity element mean the empty list and
  * associativity means it does not matter which
    order the list is built up.
</br>
</br>
</br>
* Basically list is the free monoid.
</div>


Monoid Category
=====================

* Category of monoids `Mon`
* Objects in the category of monoids are monoids
* Morphisms in the category of monoids must preserve the underlying structure
  or must be *homomorphisms*
    * Needs to map identity `e₁` in first monoid to `e₂` in second monoid.
    * needs to map pairs in the first monoid to pairs of mapping in the second
      monoid </br>`h(a *₁ b) -> h a *₂ h b`

Free Monoid Category Theoretic
=====================

* Free monoid relied on starting with a set of generators, but that is rooted
  in the `Set` category.
* Can't uniquely map the category of sets to the category of monoids, because
  for each set there are possibly multiple monoids differing in the binary
  oprator
* Can uniquely map the category of monoids to the category of sets
* Functor `U` assigning every monoid to its unique underlying set
  `U :: Mon -> Set`
* `U` is a forgetful functor because it forgets the structure in `Mon`
  going to `Set`

![](/images/free-monoid-category-theory-Mon-to-Set.svg){width=500 }

Free Monoid Category Universal Construction
=====================

<div style="width: 65%;float: left;">
* First part pick a pattern
  * For some generator set `x` in `Set` map through some function `p` to
    the mapping of the monoid `m` in `Mon` to the set `Um` in `Set` ,</br>
    `given x choose (m, p :: x -> Um)`
* Second part define a ranking between choices of your pattern
  * `m₁` is better than `m₂` </br>
    if there is a moprhism `h` from `m₁` to `m₂` </br> 
    and functions `p₁` from `x` to `Um₁` and `p₂` from `x` to `Um₂` </br>
    such that `U h . p₁ = p₂` (diagram commutes)
* Final step select the best one
  * according to the ranking it has to be the initial object in `Mon` since
  the best one will have a unique mapping to every other object in `Mon`
* So every monoid can be derived from the free monoid by identifying extra
  pairs.
* Free monoid is the list since any other monoid can be derived by collapsing
  adjacent elements.

</div><div style="width: 35%;float: right;">
![](/images/free-monoid-category-theory-construct.svg){width=500 }
</div>


Question 1
============

<div style="width: 60%;float: left; text-align: left;">
You might think (as I did, originally) that the requirement that a homomorphism
of monoids preserve the unit is redundant. After all, we know that for all a
</br>
</br>
`h a * h e = h (a * e) = h a`
</br>
</br>
So `h` `e` acts like a right unit (and, by analogy, as a left unit). The
problem is that `h` `a`, for all `a` might only cover a sub-monoid of the
target monoid. There may be a “true” unit outside of the image of `h`. Show
that an isomorphism between monoids that preserves multiplication must
automatically preserve unit.

</div><div style="width: 40%;float: right;">

Proof in psuedo Haskell.

```{.haskell style="font-size:150%"}
-- Given
  h (a₁ * b₁) = h a₁ * h b₁
-- h maps e₁ to anything
h (a₁ * e₁)
  = h a₁ * h e₁
  = h a₁ * c₂
-- a₁ * e₁ is identity
h (a₁ * e₁)
  = h a₁
-- iif c₂ = e₂
h a₁ = h a₁ * c₂
-- therefore
h e₁ = e₂
```

</div>

Question 2
============

<div style="width: 40%;float: left; text-align: left;">
Consider a monoid homomorphism from lists of integers with concatenation to
integers with multiplication. </br>
Assume that all singleton lists are mapped to the integers they contain, 
that is `[3]` is mapped to 3, etc. </br>

* What is the image of the empty list `[]`? </br>
* What’s the image of `[1, 2, 3, 4]`? </br>
* How many different lists map to the integer 12? </br>
* Is there any other homomorphism between the two monoids? </br>

</div><div style="width: 60%;float: right;">

*  
  ```
   identity maps to identity
   [] -> 1
   1 is the identity of multiplication
   1 * a = a * 1 = a
  ```
*  
  ```
   [1, 2, 3, 4] -> 24
   because
   1*(2*(3*4))
    = 1*((2*3)*4)
    = ((1*2)*3)*4
    = 24
  ```

* Double the factor pairs of 12 so 6 lists.
  ```
    [1, 12] -> 12
    [12, 1] -> 12
    [2, 6] -> 12
    [6, 2] -> 12
    [3, 4] -> 12
    [4, 3] -> 12
  ```

* No

</div>

Question 3
============

<div style="text-align: left;">
* What is the free monoid generated by a one-element set?
* Can you see what it’s
isomorphic to?</br>

* * * *

</div>
<div><div style="width: 45%;float: left; text-align: left;">
Given any 1 element set `{a}` the free monoid is `[()]` , or list of unit.</br>

```haskell
-- any value in a generator set gets mapped
-- to unit its unique since there is only one
-- element in the set
p :: a -> ()
p _ = ()
-- `h` maps from any monoid generated from a
-- singleton set to list of unit
h :: Monoid m => m -> [()]
-- monoid empty maps to empty list
h mempty = []
-- monoid binary operator maps to list concat
h (a <> b) = ++
-- any other value maps to unit
h _ = ()
-- `U` maps the monoid to its generator set
U :: (Set s, Monoid m) => m -> s
U ????
```
</div><div style="width: 50%;float: right; text-align: left;">
It is isomorphic to `Bool` under `Or` / `||`

```haskell
-- h is a homomorphism
h :: [()] -> Bool
-- empty list maps to False which is identiy under Or
False || True
  = True | False
  = True
h [] = False
-- one element list maps to True
h [()] = True
--- any list combined with unit list is True
h ([()], [])
  = h [] ++ h [()]
  = False || True
  = True
  = h [()]
h ([], [()])
  = h [()] ++ h []
  = True || False
  = True
  = h [()]
-- List of multiple elements map to True
h [(), (), ...]
  = h ([()] ++ [()] ++ ...)
  = h [()] ++ h [()] ++ ...
  = True || True || ...
  = True
  = h [()]
```
</div>
</div>
