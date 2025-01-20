{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module ForthelPlus2Core where

import ForthelPlus

data SEnv = SEnv {varlist :: [String]}
initSEnv = SEnv {varlist = []}

semantics :: Tree a -> Tree a
semantics = sem initSEnv

sem :: SEnv -> Tree a -> Tree a
sem env t = case t of
  GAdjProp (GAndAdj (GListAdj [a, b])) x ->
    let sx = sem env x
    in GAndProp (GListProp [GAdjProp a sx, GAdjProp b sx])
  GAdjProp (GOrAdj (GListAdj [a, b])) x ->
    let sx = sem env x
    in GOrProp (GListProp [GAdjProp a sx, GAdjProp b sx])
  _ -> composOp (sem env) t

