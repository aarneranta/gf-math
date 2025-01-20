{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Core2ForthelPlus where

import ForthelPlus

nlg :: Tree a -> [Tree a]
nlg t = [
  t,
  aggregate t
  ]

aggregate :: Tree a -> Tree a
aggregate t = case t of
  GAndProp (GListProp [GAdjProp a x, GAdjProp b y]) | x == y ->
    GAdjProp (GAndAdj (GListAdj [a, b])) x
  GOrProp (GListProp [GAdjProp a x, GAdjProp b y]) | x == y ->
    GAdjProp (GOrAdj (GListAdj [a, b])) x
  _ -> composOp aggregate t

