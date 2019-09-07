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

1) How many different tic-tac-toe positions are there?
```
-- Show the expanded characteristic polynomial of the tic-tac-toe board group,
-- in which an empty square is represented by an 'e', and non-empty squares by 'o' and 'x'.
*Main> characteristic (sqBoardPolyaGroup 3) "exo"
(2(e^4 + o^4 + x^4)^2(e + o + x) + (e^2 + o^2 + x^2)^4(e + o + x) + 4(e^2 + o^2 + x^2)^3(e + o + x)^3 + (e + o + x)^9) / 8
*Main> expand $ characteristic (sqBoardPolyaGroup 3) "exo"
e^9 + 3e^8o + 3e^8x + 8e^7o^2 + 12e^7ox + 8e^7x^2 + 16e^6o^3 + 38e^6o^2x + 38e^6ox^2 + 16e^6x^3 + 23e^5o^4 + 72e^5o^3x + 108e^5o^2x^2 + 72e^5ox^3 + 23e^5x^4 + 23e^4o^5 + 89e^4o^4x + 174e^4o^3x^2 + 174e^4o^2x^3 + 89e^4ox^4 + 23e^4x^5 + 16e^3o^6 + 72e^3o^5x + 174e^3o^4x^2 + 228e^3o^3x^3 + 174e^3o^2x^4 + 72e^3ox^5 + 16e^3x^6 + 8e^2o^7 + 38e^2o^6x + 108e^2o^5x^2 + 174e^2o^4x^3 + 174e^2o^3x^4 + 108e^2o^2x^5 + 38e^2ox^6 + 8e^2x^7 + 3eo^8 + 12eo^7x + 38eo^6x^2 + 72eo^5x^3 + 89eo^4x^4 + 72eo^3x^5 + 38eo^2x^6 + 12eox^7 + 3ex^8 + o^9 + 3o^8x + 8o^7x^2 + 16o^6x^3 + 23o^5x^4 + 23o^4x^5 + 16o^3x^6 + 8o^2x^7 + 3ox^8 + x^9
-- Simplify by setting 'e' to 1.
*Main> substVarWith1 'e' $ expand $ characteristic (sqBoardPolyaGroup 3) "exo"
o^9 + 3o^8x + 3o^8 + 8o^7x^2 + 12o^7x + 8o^7 + 16o^6x^3 + 38o^6x^2 + 38o^6x + 16o^6 + 23o^5x^4 + 72o^5x^3 + 108o^5x^2 + 72o^5x + 23o^5 + 23o^4x^5 + 89o^4x^4 + 174o^4x^3 + 174o^4x^2 + 89o^4x + 23o^4 + 16o^3x^6 + 72o^3x^5 + 174o^3x^4 + 228o^3x^3 + 174o^3x^2 + 72o^3x + 16o^3 + 8o^2x^7 + 38o^2x^6 + 108o^2x^5 + 174o^2x^4 + 174o^2x^3 + 108o^2x^2 + 38o^2x + 8o^2 + 3ox^8 + 12ox^7 + 38ox^6 + 72ox^5 + 89ox^4 + 72ox^3 + 38ox^2 + 12ox + 3o + x^9 + 3x^8 + 8x^7 + 16x^6 + 23x^5 + 23x^4 + 16x^3 + 8x^2 + 3x + 1
-- As there are no positions with e.g. 9 x's some terms can be eliminated. Let's assume that 'x' always gets
-- the first turn. Then we want coefficients of x, x and o, x² and o, x² and o², and so on.
-- Generate the list of coefficient combinations.
*Main> [[('o',o),('x',x)] | x<-[0..5], o<-[x-1..x]]
[[('o',-1),('x',0)],[('o',0),('x',0)],[('o',0),('x',1)],[('o',1),('x',1)],[('o',1),('x',2)],[('o',2),('x',2)],[('o',2),('x',3)],[('o',3),('x',3)],[('o',3),('x',4)],[('o',4),('x',4)],[('o',4),('x',5)],[('o',5),('x',5)]]
-- This generates a few coefficients too many, but they don't show up in the characteristic polynomial anyway.
*Main> [coefficient [('o',o),('x',x)] (substVarWith1 'e' $ characteristic (sqBoardPolyaGroup 3) "exo") | x<-[0..5], o<-[x-1..x]]
[0,1,3x,12ox,38ox^2,108o^2x^2,174o^2x^3,228o^3x^3,174o^3x^4,89o^4x^4,23o^4x^5,0]
-- One empty position, 3 with a single cross, 12 with one cross and one circle, and so on.
-- Add them all together and substitute 1 again for each variable to get the total number
-- of truly different tic-tac-toe positions.
*Main> substAllVarsWith1 $ addPoly [coefficient [('o',o),('x',x)] (substVarWith1 'e' $ characteristic (sqBoardPolyaGroup 3) "exo") | x<-[0..5], o<-[x-1..x]]
850
-- 85 of these 850 positions are illegal or unreachable, some examples:
-- xoo xxx ooo
-- xxo .o. xx.
-- xxo o.o .xx
-- The number of non-isomorphic legal positions therefore is 850 - 85 = 765.
*Main>
```

2) How many different non-isomorphic graphs exist on 4 vertices?
```
-- Show the characteristic polynomial of this group,
-- in which non-present and present edges are represented by 'x' and 'v' respectively.
*Main> characteristic (graphPolyaGroup 4) "xv"
(6(v^4 + x^4)(v^2 + x^2) + 8(v^3 + x^3)^2 + 9(v^2 + x^2)^2(v + x)^2 + (v + x)^6) / 24
*Main> expand $ characteristic (graphPolyaGroup 4) "xv"
v^6 + v^5x + 2v^4x^2 + 3v^3x^3 + 2v^2x^4 + vx^5 + x^6
-- Substituting 1 for each variable in this polynomial gives the correct answer, 11.
*Main> substAllVarsWith1 $ characteristic (graphPolyaGroup 4) "xv"
11
-- So, how many different non-isomorphic graphs are there for 5, 6, or 7 vertices?
*Main> substAllVarsWith1 $ characteristic (graphPolyaGroup 5) "xv"
34
*Main> substAllVarsWith1 $ characteristic (graphPolyaGroup 6) "xv"
156
*Main> substAllVarsWith1 $ characteristic (graphPolyaGroup 7) "xv"
1044
*Main>
```
