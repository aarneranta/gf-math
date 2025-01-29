{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Informath2Core where

import Informath

data SEnv = SEnv {varlist :: [String]}
initSEnv = SEnv {varlist = []}

semantics :: Tree a -> Tree a
semantics = addCoercions . sem initSEnv

addCoercions :: Tree a -> Tree a
addCoercions t = case t of
  GAxiomJmt label hypos prop -> GAxiomJmt label hypos (GProofProp prop)
  {-
  AxiomPropJmt : Label -> ListHypo -> Prop -> Jmt ;
  DefPropJmt : Label -> ListHypo -> Prop -> Prop -> Jmt ;
  SuchThatKind : Ident -> Kind -> Prop -> Kind ;
  ThmJmt : Label -> ListHypo -> Prop -> Proof -> Jmt ;
  AxiomExpJmt : Label -> ListHypo -> Exp -> Kind -> Jmt ;
  AxiomKindJmt : Label -> ListHypo -> Kind -> Jmt ;
  DefExpJmt : Label -> ListHypo -> Exp -> Kind -> Exp -> Jmt ;
  DefKindJmt : Label -> ListHypo -> Kind -> Kind -> Jmt ;
  ElemKind : Kind -> Kind ;
  IdentsArgKind : Kind -> ListIdent -> ArgKind ;
  KindArgKind : Kind -> ArgKind ;
  TypedExp : Exp -> Kind -> Exp ;
  -}
  GVarsHypo idents kind -> GVarsHypo idents (GElemKind kind)
  _ -> composOp addCoercions t


sem :: SEnv -> Tree a -> Tree a
sem env t = case t of
  GAdjProp (GAndAdj (GListAdj adjs)) x ->
    let sx = sem env x
    in GAndProp (GListProp [GAdjProp adj sx | adj <- adjs])
  GAdjProp (GOrAdj (GListAdj adjs)) x ->
    let sx = sem env x
    in GOrProp (GListProp [GAdjProp adj sx | adj <- adjs])
  GAdjProp a (GAndExp (GListExp exps)) ->
    let sa = sem env a
    in GAndProp (GListProp [GAdjProp sa exp | exp <- exps])
  GAdjProp a (GOrExp (GListExp exps)) ->
    let sa = sem env a
    in GOrProp (GListProp [GAdjProp sa exp | exp <- exps])

  GLetFormulaHypo formula ->
    GPropHypo (sem env (GFormulaProp (sem env formula)))

  GFormulaProp (GFEquation equation) -> case equation of
    GEBinary (GComparEqsign compar) term1 term2 ->
      GAdjProp (GComparAdj compar (sem env (GTermExp term2))) (sem env (GTermExp term1))
    _ -> composOp (sem env) t

  GTermExp (GConstTerm const) -> GConstExp const
      
  _ -> composOp (sem env) t

