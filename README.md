# Pólya.hs

Journey into [Pólya's enumeration theorem](https://en.wikipedia.org/wiki/P%C3%B3lya_enumeration_theorem) using [Haskell](https://www.haskell.org/).

It's my first Haskell project in a decade so don't expect any fancy code or good use of libraries. Feel free to tidy things up if you like.

Features a polynomial type bound on variables of some Eq or Ord type, and scalars of any [commutative ring](https://en.wikipedia.org/wiki/Commutative_ring). For nerd points made a genuine [monad](https://en.wikipedia.org/wiki/Monad_(functional_programming)) out of it, see [this branch](https://github.com/PenguinF/polya/tree/feature/polynomial_monad).

To play around with the code:

- Install the [Haskell platform](https://www.haskell.org/platform/)
- Clone this repository, for example to `C:\Dev\polya`
- Start (Win)GHCi

To bring all functions into scope, use these commands:
```
Prelude> :cd C:/Dev/polya
Prelude> :add util.hs
[1 of 1] Compiling Eutherion.Utilities ( util.hs, interpreted )
Ok, one module loaded.
*Eutherion.Utilities> :add combinatorics.hs
[2 of 2] Compiling Eutherion.Combinatorics ( combinatorics.hs, interpreted )
Ok, two modules loaded.
*Eutherion.Combinatorics> :add ring.hs
[3 of 3] Compiling Eutherion.CommutativeRing ( ring.hs, interpreted )
Ok, three modules loaded.
*Eutherion.CommutativeRing> :add polynomial.hs
[4 of 4] Compiling Eutherion.Polynomial ( polynomial.hs, interpreted )
Ok, four modules loaded.
*Eutherion.Polynomial> :add polya.hs
[5 of 5] Compiling Eutherion.Polya  ( polya.hs, interpreted )
Ok, five modules loaded.
*Eutherion.Polya> :add main.hs
[6 of 6] Compiling Main             ( main.hs, interpreted )
Ok, six modules loaded.
*Main>
```

Example questions this code can answer for you:

1) How many different non-isomorphic graphs exist on 4 vertices?
```
-- Show the Cayley table representation of the graph group:
*Main> cayleyTable $ graphPolyaGroup 4
    | 1 b c d e f g h i j k l m n o p q r s t u v w x | ~
  --+-------------------------------------------------+---
  1 | 1 b c d e f g h i j k l m n o p q r s t u v w x | 1
  b | b 1 d c f e h g l k j i n m r q p o t s x w v u | b
  c | c e 1 f b d l k j i h g r q p o n m x w v u t s | c
  d | d f b e 1 c i j k l g h o p q r m n u v w x s t | e
  e | e c f 1 d b k l g h i j q r m n o p w x s t u v | d
  f | f d e b c 1 j i h g l k p o n m r q v u t s x w | f
  g | g v m s p j 1 o n f u t c i h e x w d l k b r q | g
  h | h w n t q k b r m e x s d l g f u v c i j 1 o p | v
  i | i x o u r l d q p c w v b k j 1 t s e h g f n m | p
  j | j s p v m g f n o 1 t u e h i c w x b k l d q r | j
  k | k t q w n h e m r b s x f g l d v u 1 j i c p o | s
  l | l u r x o i c p q d v w 1 j k b s t f g h e m n | m
  m | m p g j v s t u f n o 1 w x e h i c q r b k l d | l
  n | n q h k w t s x e m r b v u f g l d p o 1 j i c | u
  o | o r i l x u v w c p q d s t 1 j k b m n f g h e | o
  p | p m j g s v u t 1 o n f x w c i h e r q d l k b | i
  q | q n k h t w x s b r m e u v d l g f o p c i j 1 | x
  r | r o l i u x w v d q p c t s b k j 1 n m e h g f | r
  s | s j v p g m n f u t 1 o h e x w c i k b r q d l | k
  t | t k w q h n m e x s b r g f u v d l j 1 o p c i | t
  u | u l x r i o p c w v d q j 1 t s b k g f n m e h | n
  v | v g s m j p o 1 t u f n i c w x e h l d q r b k | h
  w | w h t n k q r b s x e m l d v u f g i c p o 1 j | w
  x | x i u o l r q d v w c p k b s t 1 j h e m n f g | q
-- Show the expanded characteristic polynomial of this group,
-- in which non-present and present edges are represented by 'x' and 'v' respectively:
*Main> characteristicPolynomial (graphPolyaGroup 4) "xv"
v^6 + v^5x + 2v^4x^2 + 3v^3x^3 + 2v^2x^4 + vx^5 + x^6
-- Substituting 1 for each variable in this polynomial gives the correct answer, 11.
*Main> substitute (characteristicPolynomial (graphPolyaGroup 4) "xv") (\var -> mp "1")
11
-- So, how many different non-isomorphic graphs are there for 5, or 6 vertices?
*Main> substitute (characteristicPolynomial (graphPolyaGroup 5) "xv") (\var -> mp "1")
34
*Main> substitute (characteristicPolynomial (graphPolyaGroup 6) "xv") (\var -> mp "1")
156
*Main>
```
