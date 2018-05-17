1
======

Question
-------

Define a natural transformation from the Maybe functor to the list functor.
Prove the naturality condition for it.

Answer
-------

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

2
======

Question
-------

Define at least two different natural transformations between Reader () and
the list functor. How many different lists of () are there?

Answer
-------

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

3
======

Question
-------

Continue the previous exercise with Reader Bool and Maybe.

Answer
-------

```haskell
readerToMaybeTrue :: Reader Bool a -> Maybe a
readerToMaybeTrue (Reader g) = Just (g True)

readerToMaybeFalse :: Reader Bool a -> Maybe a
readerToMaybeFalse (Reader g) = Just (g False)

readerToMaybeNothing :: Reader Bool a -> Maybe a
readerToMaybeNothing _ = Nothing
```

There are three natural transformations from `Reader Bool` to `Maybe`

4
======

Question
-------

Show that horizontal composition of natural transformation satisfies the naturality
condition (hint: use components). It’s a good exercise in diagram chasing.

5
======

Question
-------

Write a short essay about how you may enjoy writing down the evident diagrams needed to
prove the interchange law.

6
======

Question
-------

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


