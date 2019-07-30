# Pólya.hs

Journey into [Pólya's enumeration theorem](https://en.wikipedia.org/wiki/P%C3%B3lya_enumeration_theorem) using [Haskell](https://www.haskell.org/).

It's my first Haskell project in a decade so don't expect any fancy code or good use of libraries. Feel free to tidy things up if you like.

Features a polynomial type bound on variables of some Eq or Ord type, and scalars of any [commutative ring](https://en.wikipedia.org/wiki/Commutative_ring). For nerd points made a genuine [monad](https://en.wikipedia.org/wiki/Monad_(functional_programming)) out of it.

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
*Eutherion.Combinatorics> :add main.hs
[3 of 3] Compiling Main             ( main.hs, interpreted )
Ok, three modules loaded.
*Main>
```
