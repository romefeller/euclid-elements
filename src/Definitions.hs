{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
module Definitions where

import GHC.TypeLits
import Data.Proxy

-- Point exists
data Point (a :: Symbol) where
    Pt :: KnownSymbol a => Proxy a -> Point a
    -- Circle–circle intersection point.
    -- This is assumed in Euclid I.1 but never proved.
    -- Later commentators note this is a hidden axiom.
    -- Reference: Euclid, Elements, Book I, Prop. 1.
    Inter :: Circle '[a, b] -> Circle '[b, a] -> Point p

-- Line between points
data Line s where
    Ln :: Point a -> Point b -> Line '[a, b]
    -- Needed to change the order. Not mentioned in the book,
    -- but required in the formalization to handle symmetry of a line.
    -- Compare with Euclid’s unstated assumption: "the straight line AB
    -- is the same as BA."
    Chg :: Line '[a, b] -> Line '[b, a]
    -- You can always extend a line.
    -- Euclid uses this principle in I.2 and elsewhere.
    -- Book I, Postulate 2: "to produce a finite straight line continuously in a straight line."
    Ex :: Line '[a, b]
       -> Point c
       -> Line ('[a, c])

data Circle s where
    -- A circle with center a and radius AB.
    -- Reference: Euclid, Elements, Book I, Def. 15 ("A circle is a plane figure
    -- contained by one line such that all straight lines falling upon it
    -- from one point among those lying within the figure equal one another").
    Cr :: Point a -> Line '[a, b] -> Circle '[a, b]

data Triangle s where
    -- An equilateral triangle.
    -- Reference: Euclid, Elements, Book I, Def. 20 ("Of trilateral figures,
    -- an equilateral triangle is that which has its three sides equal").
    Equil :: Line '[a, b]
       -> Line '[b, c]
       -> Line '[a, c]
       -> Line '[a, c] :=: Line '[a, b]
       -> Line '[a, b] :=: Line '[b, c]
       -> Triangle '[a, b, c]

-- Equality witnesses
data (:=:) s r where
    Refl :: a :=: a
    Sym :: a :=: b -> b :=: a
    Trans :: a :=: b -> b :=: c -> a :=: c

    -- Axiom: a line is the same regardless of direction.
    -- Euclid assumes "the straight line AB is the same as BA".
    Comm   :: Line '[a,b] :=: Line '[b,a]

    -- Axiom: all radii of a circle are equal.
    -- This is implicit in Book I, Def. 15 and used in Prop. 1.
    Radii  :: Circle '[a, b] -> Point c -> Line '[a, c] :=: Line '[a, b]

-- Helper to avoid boilerplate. Can be useful in the future.
mkLn :: (KnownSymbol a, KnownSymbol b) => Proxy a -> Proxy b -> Line '[a, b]
mkLn x y = Ln (Pt x) (Pt y)

-- Euclid I.1 (Book 1): “On a given finite straight line to construct an equilateral triangle.”
-- Steps:
-- 1. Take AB as the given finite line.
-- 2. Construct circle with center A, radius AB (circle ca).
-- 3. Construct circle with center B, radius AB (circle cb).
-- 4. Assume the circles intersect in a point C (pc).
-- 5. Draw lines AC and BC.
-- 6. Use the "all radii equal" axiom to conclude AC = AB and BC = AB.
-- 7. Thus, triangle ABC is equilateral.
equilTri
  :: Line '["A", "B"]
  -> Triangle '["A", "B", "C"]
equilTri ab =
  let ca = Cr (Pt (Proxy @"A")) ab
      cb = Cr (Pt (Proxy @"B")) (Chg ab)
      pc = Inter ca cb
      lineac = (Ln (Pt (Proxy @"A")) pc)
      linebc = (Ln (Pt (Proxy @"B")) pc)
      eq1 = Radii ca pc
      eq2 = Trans Comm (Sym (Radii cb pc))
  in Equil ab linebc lineac eq1 eq2
