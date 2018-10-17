Free monoids
===================

Monoid Set Theoretic
--------------------

* definition:
  * Binary operator that map pairs of elements to other elements.
    * `u :: (m,m) -> m` or `* :: m -> m -> m`
  * There is an identity element `e` combined with any other element maps to
    that element.
    * `e * a = a * e = a`
  * The operations is associative, the order of reducing multiple oprations
    does not matter.
    * `(a * b) * c = a * (b * c)`


* Given any two elements identify another element satisfying the laws.
  * The identification is monoid dependent.
  * Multiple pairs could map to the same element
  * Natural numbers under summation
    * `3 = 0 + 3 = 1 + 2 = 2 + 1 = 3 + 0`
    * All the identifications are not important and are specific to the
      specific monoid
    * some of the identifications are necessary to satisfy the laws.
    * Free monoid is the monoid where the minimum of identifications are
      required to satisfy the laws.

<!-- -->

* Construct free monoid:
  * Start with a set of generators `{a,b}`
  * monoid requires an identity element so add it to the set `{a,b,e}`
  * the possible pairs are then
    * `e * a`
    * `b * e`
    * `a * e`
    * `a * b`
    * `b * a`
  * need to identify the results of `e` pairs for monoid laws
    * `e * a = a`
    * `b * e = b`
    * `a * e = a`
    * `e * b = b`
  * but the results of the other pairs are not important so extend the set
    with them `{a, b, e, aa, bb, ab, ba}`
  * the new set excluding the identity element combinations have the following
    new pairs.
    * `a * aa`
    * `a * bb`
    * `a * ab`
    * `a * ba`
    * `b * aa`
    * `b * bb`
    * `b * ab`
    * `b * ba`
    * `ab * a`
    * `ba * a`
    * `ab * b`
    * `ba * b`
  * because of associativity law we have to identify some of the new pairs as
    the same
    * `a * bb = ab * b`
    * `a * ba = ab * a`
    * `b * aa = ba * a`
    * `b * ab = ba * b`
  * we extend the set by adding all the unique pairs to it
    `{a, b, e, aa, bb, ab, ba, aaa, bbb, aab, aba, bba, baa, abb}`
  * we infinitly continue recursively buiding up the set.
  * This set is the set of lists of all the possible combinations of the
  original generators, where
    * identity element means we could have an empty list
    * associativity means it does not matter which order the list is built up.
  * So basically list is the free monoid.



Monoid Category Theoretic
--------------------

* Category of monoids **Mon**
  * Objects in the category of monoids would be monoids
  * Morphisms in the category of monoids
    * Need to preserver the underlying structure which is called a
    **homomorphism**
      * Needs to map identity `e₁` in first monoid to `e₂` in second monoid.
      * needs to map pairs in the first monoid to pairs of mapping in the second
        monoid `h(a *₁ b) -> h a *₂ h b`


* Constructing free Monoid
  * relied on starting with a set of generators
  * can't uniquely map the category of sets to the category of monoids, because
    for each set there are possibly multiple monoids differing in the binary 
    oprator
  * can uniquely map the category of monoids to the category of sets
  * Functor **U** assigning every monoid to its unique underlying set
    `U :: Mon -> Set`
  * **U** is a forgetful functor because it forgets the structure in **Mon**
    going to **Set**

* Universal construction
  * First part pick a pattern
    * Given some generator set `x` in `Set` we map through some function `p` to
      the mapping of the monoid `m` in `Mon` to the set `Um` in `Set` ,
      `given x choose (m, p :: x -> Um)`
  * Second part define a ranking between choices of your pattern
    * `m₁` is better than `m₂` if there is a moprhism `h` from `m₁` to `m₂`
      and functions `p₁` from `x` to `U m₁` and `p₂` from `x` to `U m₂` such
      that `U h . p₁ = p₂` (diagram commutes)
  * Final step select the best one 
    * according to the ranking it has to be the initial object in `Mon` since 
    the best one will have a unique mapping to every other object in `Mon`
  * So every monoid can be derived from the free monoid by identifying extra
    pairs.
      * If the other monoid is bigger than the free monoid then the free monoid
        can be embedded in it through `h`.
      * If the other monoid is small than the free monoid then the free monoid
        can be projected 

Challanges
============================

Question 1
-----------------------
You might think (as I did, originally) that the requirement that a homomorphism
of monoids preserve the unit is redundant. After all, we know that for all a

```
h a * h e = h (a * e) = h a
```

So `h` `e` acts like a right unit (and, by analogy, as a left unit). The
problem is that `h` `a`, for all `a` might only cover a sub-monoid of the 
target monoid. There may be a “true” unit outside of the image of `h`. Show
that an isomorphism between monoids that preserves multiplication must
automatically preserve unit.

Answer 1
----------------------

Given

````
  h (a₁ * b₁) = h a₁ * h b₁
```

Then

```
  -- h maps e₁ to anything
  h (a₁ * e₁)
    = h a₁ * h e₁
    = h a₁ * c₂
  -- a₁ * e₁ is identity
  h (a₁ * e₁)
    = h a₁
    = h a₁
 -- iif c₂ = e₂
  h a₁ = h a₁ * c₂
  -- therefore
  h e₁ = e₂
```

Question 2
---------------------
Consider a monoid homomorphism from lists of integers with concatenation to
integers with multiplication. What is the image of the empty list `[]`? Assume
that all singleton lists are mapped to the integers they contain, that is `[3]`
is mapped to 3, etc. What’s the image of `[1, 2, 3, 4]`? How many different lists
map to the integer 12? Is there any other homomorphism between the two monoids?

Answer 2
--------------------

### 1

```
 identity maps to identity
 [] -> 1
 1 is the identity of multiplication
 1 * a = a * 1 = a
```

### 2

```
 [1, 2, 3, 4] -> 24
 because
 1*(2*(3*4)) = 1*((2*3)*4) = ((1*2)*3)*4 = 24
```

### 3
Double the factor pairs of 12 so 6 lists.

```
  [1, 12] -> 12
  [12, 1] -> 12
  [2, 6] -> 12
  [6, 2] -> 12
  [3, 4] -> 12
  [4, 3] -> 12
```

Question 3
--------------

What is the free monoid generated by a one-element set? Can you see what it’s
isomorphic to?

Answer 3
-------------

Given any 1 element set `{a}` the free monoid is `[()]` , or list of unit.

```
  p :: a -> ()
  p _ = ()
  h :: Monoid m => m -> [()]
  h mempty = []
  h _ = ()
  U :: (Set s, Monoid m) => m -> s
  ????
```

It is isomorphic to `Bool` under `Or` / '||'

```
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
