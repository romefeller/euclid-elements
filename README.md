# Euclid in Haskell ✨📐

This project is a playful **formalization of Euclid’s *Elements*** using the Haskell type system, GADTs, and type-level symbols.  
The goal is to see how far we can push the typechecker to mimic Euclid’s constructions, assumptions, and proofs.

---

## Highlights

- **Points, Lines, Circles, Triangles**  
  Basic geometric entities are encoded as *generalized algebraic data types* (GADTs).  
  For example:
  ```haskell
  data Point (a :: Symbol) where
      Pt    :: KnownSymbol a => Proxy a -> Point a
      Inter :: Circle '[a, b] -> Circle '[b, a] -> Point p

  The hidden axioms are also explicit.
