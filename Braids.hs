{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

data Nat = Z | S Nat

type family Plus (m :: Nat) (n :: Nat) :: Nat
type instance Plus Z n = n
type instance Plus (S m) n = S (Plus m n)

data Braid (strands :: Nat) where
  Unit   :: Braid (S Z)
  Over   :: Braid (S (S Z))
  Under  :: Braid (S (S Z))
  (:--:) :: Braid n -> Braid n -> Braid n
  (:=:)  :: Braid m -> Braid n -> Braid (Plus m n)

plus :: Diagram Cairo R2
plus = vrule 1 <> square 0.1 # fc white # lw 0 <> hrule 1 

drawBraid :: Braid n -> Diagram Cairo R2
drawBraid Unit = hrule 1 <> strutY 1
drawBraid Over = plus # rotateBy (-1/8) # sized (Width 1)
               <> strutY 2
drawBraid Under = plus # rotateBy (1/8) # sized (Width 1) <> strutY 2
drawBraid (b1 :--: b2) = (drawBraid b1 ||| drawBraid b2) # centerXY
drawBraid (b1 :=: b2) = (d1' === d2') # centerXY
  where
    d1, d2 :: Diagram Cairo R2
    d1 = drawBraid b1
    d2 = drawBraid b2
    w1 = width d1
    w2 = width d2
    d1' = if w1 < w2 then d1 # scaleX (w2 / w1) else d1
    d2' = if w2 < w1 then d2 # scaleX (w1 / w2) else d2

b = (Under :=: Unit) :--: (Unit :=: Under) :--: (Unit :=: Unit :=: Unit) :--: (Unit :=: Under) :--: (Over :=: Unit)

main = defaultMain (drawBraid b)
