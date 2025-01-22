{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Informath2Core where

import Informath

data SEnv = SEnv {varlist :: [String]}
initSEnv = SEnv {varlist = []}

semantics :: Tree a -> Tree a
semantics = sem initSEnv

sem :: SEnv -> Tree a -> Tree a
sem env t = case t of
  GPropJmt hypos prop ->
    GAxiomJmt (sem env hypos) (GStrLabel (GString "")) (sem env prop) 
  GAdjProp (GAndAdj (GListAdj [a, b])) x ->
    let sx = sem env x
    in GAndProp (GListProp [GAdjProp a sx, GAdjProp b sx])
  GAdjProp (GOrAdj (GListAdj [a, b])) x ->
    let sx = sem env x
    in GOrProp (GListProp [GAdjProp a sx, GAdjProp b sx])
  _ -> composOp (sem env) t

