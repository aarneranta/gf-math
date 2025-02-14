{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Informath2Core where

import Informath

data SEnv = SEnv {varlist :: [String]}
initSEnv = SEnv {varlist = []}

semantics :: Tree a -> Tree a
semantics = addCoercions . addParenth . sem initSEnv

addCoercions :: Tree a -> Tree a
addCoercions t = case t of
  GAxiomJmt label hypos prop -> GAxiomJmt label hypos (proofProp prop)
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
 where
   proofProp prop = case prop of
     GProofProp _ -> prop
     _ -> GProofProp prop

addParenth :: Tree a -> Tree a
addParenth t = case t of
  GSimpleAndProp (GListProp props) -> GAndProp (GListProp (map addParenth props))
  GSimpleOrProp (GListProp props) -> GOrProp (GListProp (map addParenth props))
  GSimpleIfProp a b -> GIfProp (addParenth a) (addParenth b)
  GSimpleIffProp a b -> GIffProp (addParenth a) (addParenth b)
  _ -> composOp addParenth t
  
sem :: SEnv -> Tree a -> Tree a
sem env t = case t of
{-
  GAxiomJmt label (GListHypo hypos) prop@(SimpleIfProp _ _) ->
    let (hs, p) = ifs2hypos hypos prop
    in GAxiomJmt label (GListHypo (map (sem env) (hypos ++ hs))) (sem env p)
-}

  GTextbfTerm term -> sem env term

  GLetFormulaHypo formula -> case (sem env formula) of
    GFElem (GListTerm terms) (GSetTerm set) ->
      GVarsHypo (GListIdent [x | GTIdent x <- terms]) (GSetKind set) ---- TODO: check that all terms are idents

    _ -> GPropHypo (sem env (GFormulaProp (sem env formula)))

  GSimpleIfProp (GKindProp (GTermExp (GTIdent x)) kind) prop ->
    sem env (GAllProp (GListArgKind [GIdentsArgKind kind (GListIdent [x])]) prop)

  GPostQuantProp prop exp -> case exp of
    GEveryIdentKindExp ident kind ->
      sem env (GAllProp (GListArgKind [GIdentsArgKind kind (GListIdent [ident])]) prop)
    GIndefIdentKindExp ident kind ->
      sem env (GExistProp (GListArgKind [GIdentsArgKind kind (GListIdent [ident])]) prop)
    GSomeArgKindExp argkind ->
      sem env (GExistProp (GListArgKind [argkind]) prop)
    GAllArgKindExp argkind ->
      sem env (GAllProp (GListArgKind [argkind]) prop)

  GAllProp argkinds prop -> case argkinds of
    GListArgKind [GIdentsArgKind (GAdjKind adj kind) vars@(GListIdent [x])] ->
      GAllProp
        (GListArgKind [GIdentsArgKind kind vars])
        (GSimpleIfProp (sem env (GAdjProp adj (GTermExp (GTIdent x)))) (sem env prop))
    akinds -> GAllProp akinds (sem env prop)
    
  GKindProp exp (GAdjKind adj kind_) ->
    sem env (GAdjProp adj exp) --- ignoring kind, if not in hypothesis position

  GAdjKind adj kind ->
    let (var, nenv) = newVar env
    in GSuchThatKind var (sem nenv kind) (sem nenv (GAdjProp adj (GTermExp (GTIdent var))))

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
  GNotAdjProp adj exp -> GNotProp (sem env (GAdjProp adj exp))

  GFormulaProp (GFEquation (GEBinary (GComparEqsign compar) term1 term2)) ->
    GAdjProp (GComparAdj compar (sem env (GTermExp term2))) (sem env (GTermExp term1))

  GFormulaProp (GFEquation equation@(GEChain _ _ _)) -> case chainedEquations equation of
    triples -> GAndProp (GListProp
      [sem env (GFormulaProp (GFEquation (GEBinary eqsign x y))) | (eqsign, x, y) <- triples])
    
  GTermExp (GConstTerm const) -> GConstExp const
  GTermExp (GAppOperTerm oper x y) ->
    GOperListExp oper (GAddExps (sem env (GTermExp x)) (GOneExps (sem env (GTermExp y))))
  GTermExp (GTTimes x y) -> sem env (GTermExp (GAppOperTerm (LexOper "times_Oper") x y))
  GTermExp (GTExp x y) -> sem env (GTermExp (GAppOperTerm (LexOper "pow_Oper") x y))
---  GTFrac x y -> sem env (GAppOperTerm (LexOper "div_Oper") x y)
  GTParenth term -> sem env term
      
  _ -> composOp (sem env) t

chainedEquations :: GEquation -> [(GEqsign, GTerm, GTerm)]
chainedEquations equation = case equation of
  GEChain eqsign term equ ->
    let triples@((_, x, _):_) = chainedEquations equ
    in (eqsign, term, x) : triples
  GEBinary eqsign term1 term2 ->
    [(eqsign, term1, term2)]

{-
ifs2hypos :: [GHypo] -> GProp -> ([GHypo], GProp)
ifs2hypos hs prop = case prop of
  GSimpleIfProp p q -> 
-}


hypoVars :: GHypo -> [GIdent]
hypoVars hypo = case hypo of
  GVarsHypo (GListIdent idents) _ -> idents
  _ -> []

newVar :: SEnv -> (GIdent, SEnv)
newVar env = (GStrIdent (GString "h_"), env) ---- TODO fresh variables

