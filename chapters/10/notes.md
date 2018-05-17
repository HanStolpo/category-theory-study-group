Natural Transformations
======================

| Natural transformation maps fucntors to functors by picking
  for every object $a$ in the source category a morphism in
  the target category to do the mapping.
|   $\alpha_{a} = Fa \to Ga$
| The mapping must also preserve the **natuarality condition**
|   $G⨍∘α_{a} = α_{b}∘F⨍$
|   where
|     $⨍ = α \to β$
|     $F⨍ = Fa \to Fb$
|     $G⨍ = Ga \to Gb$
|     $α_{a} = Fa \to Ga$
|     $α_{b} = Fb \to Gb$
![](/images/naturality.jpg){height=150}

| Its **stringent**. If $F⨍$ is invertible then $α_{b}$ is determined in terms
  of $α_{a}$
|   $α_{b} = (G⨍)∘α_{a}∘(F⨍)^{-1}$
![](/images/transport.jpg){height=150}

**Viewed componentwise** could say:

  * maps objects to morphisms
  * maps morphisms to comuting squares
    ![](/images/comuting-square.jpg){height=150}

Polymorphic Functions
======================

With **parametric polymorphism** as in Haskell any function between functors
polymorphic in the element is a natural transformation.

```haskell
-- Always a natural transformation
alpha :: F a -> G a
-- naturality always holds
(fmap @ G) f . alpha = alpha . (fmap @ F) f
```

This is due to *"theoroms for free"**


Even functions to values can be seen as natural transformations

```haskell
-- Normal version
length :: [a] -> Int

-- Version cast recast as natural transformation
lengthF :: [a] -> Const Int a
```

Contravariant Functors
==================

* Equavilent to covariant functors in the opposite category
* Not natural transformations in **Hask**
* They do satisfy the opposite naturailty condition in **Hask**
  ```haskell
  (contramap @ G) . alpha = alpha . (contramap @ F)
  ```

Functor Category
================

* One category of functors for each pair of categories $C$ an $D$
* Objects are functors from $C$ to $D$
* Morphism are natural transformations between the functors</br>
  $α_{a} = Fa \to Fb$</br>
  $β_{a} = Ga \to Ha$</br>
  $β_{a}∘α_{a} = Fa \to Ha$</br>
  $(β⋅α) _{a} = β_{a}∘α_{a}$</br>
  ![](/images/vertical.jpg){height=150}
* composition of natural transformation is associative because
  there component morphisms are asociative.
* Naturality holds for the composition</br>
  $H⨍ ∘ (β⋅α)_{a} = (β⋅α)_{b} ∘ F⨍$</br>
  ![](/images/verticalnaturality.jpg){height=150}
* Indentity natural transformation $1_F$ whose components
  are the identity morphisms.</br>
  $id_{F_a} = Fa \to Fa$
* Called $[C,D]$ or $D^C$

Vertical composition
--------------------
* composition used above known as vertical composition
* composition of natural transformation within the same category
* $β⋅α$</br>
  ![](/images/vertical2.jpg){height=150}

2-Categories
=============

* **Cat** category of *small* categories
* Hom-set in **Cat** set of functors
* Functors form a category
* In **Cat** as 2-category
  * Objects: *Small* categories
  * 1-morphisms: Functors between categories
  * 2-morphisms: Natural transformations between Functors (morphisms of morphisms)
* Hom-category $D^C$ instead of Hom-set between categories $C$ and $D$
  * Regular functor compistion</br>
    $F$ from $D^C$</br>
    $G$ from $E^D$</br>
    $G∘F$ in $E^C$
  * Also composition inside each Hom-category, vertical composition of natural transformations</br>
   ![](/images/8_cat-2-cat.jpg){height=150}

Horizontal Composition
------------------------
* 2 Functors 1-morhpisms in **Cat** </br>
  $F = C \to D$
  $G = D \to E$
* their composition </br>
  $G∘F= C \to E$
* 2 Natural transformations $α$ and $β$ acting on $F$ and $G </br>
  $α = F \to F'$</br>
  $β = G \to G'$</br>
  we can't apply vertical composition to $α$ and $β$
* Can we construct natural transformation between $G∘F$ and $G'∘F'$ ? Yes. </br>
  ![](/images/9_horizontal.jpg){height=150} </br>
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
  * Need to find path $G(Fa) \to G'(F'a)$ mapping $G∘F \to G'∘F'$
    * there are two paths </br>
      $G'α_a∘β_{Fa}$ </br>
      $β_{F'a}∘Gα_a$ </br>
    * but they are equal because of the naturality condition of $β$
  * This is called the horizontal composition of $α$ and $β$ </br>
    $β∘α = G∘F \to G'∘F'$
  * ?? Natural transofrmations collapse all equivelant functors between categories `C` and `D`
    and horizontal compisition is the composition of these collapsed functors ??
    ![](/images/sideways.jpg){height=150}
  * **Interchage law** for horizontal composition </br>
    $(β' ⋅ α') ∘ (β ⋅ α) = (β' ∘ β) ⋅ (α' ∘ α)$
