{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Dedukti2Core where

import Dedukti.AbsDedukti
import Dedukti.PrintDedukti
import Core
import CommonConcepts

import Data.Char

jmt2core :: Jmt -> GJmt
jmt2core jmt = case jmt of
  JStatic ident typ -> case splitType typ of
    (hypos, kind) -> 
      GAxiomExpJmt
        (GListHypo (map hypo2core hypos)) (ident2coreExp ident)
        (exp2coreKind kind)
  JDef ident (MTExp typ) (MEExp exp) -> case splitType typ of
    (hypos, kind) -> 
      GDefExpJmt
        (GListHypo (map hypo2core hypos)) (ident2coreExp ident)
        (exp2coreKind kind) (exp2coreExp exp)
  JDef ident (MTExp typ) MENone -> case splitType typ of
    (hypos, kind) -> 
      GAxiomExpJmt
        (GListHypo (map hypo2core hypos)) (ident2coreExp ident)
        (exp2coreKind kind)
  JThm ident (MTExp typ) (MEExp exp) -> case splitType typ of
    (hypos, prop) -> 
      GThmJmt
        (GListHypo (map hypo2core hypos)) (ident2coreExp ident)
        (exp2coreProp prop) (exp2coreProof exp)
  JThm ident (MTExp typ) MENone -> case splitType typ of
    (hypos, prop) -> 
      GAxiomJmt
        (GListHypo (map hypo2core hypos)) (ident2coreExp ident)
        (exp2coreProp prop)
  JRules rules -> GRewriteJmt (GListRule (map rule2core rules))
  
  _ -> error ("not yet: " ++ printTree jmt)
{-
  GAxiomJmt exp (GListHypo hypos) prop ->
    JStatic
      (exp2coreIdent exp)
      (foldr EFun (prop2core prop) (concatMap hypo2core hypos))
  GThmJmt exp (GListHypo hypos) prop proof ->
    JDef
      (exp2coreIdent exp)
      (MTExp (foldr EFun (prop2core prop) (concatMap hypo2core hypos)))
      (MEExp (proof2core proof))
  GDefPropJmt (GListHypo hypos) prop df ->
    JDef
      (prop2coreIdent prop)
      (MTExp (foldr EFun typeProp (concatMap hypo2core hypos)))
      (MEExp (prop2core df))
  GDefKindJmt (GListHypo hypos) kind df ->
    JDef
      (kind2coreIdent kind)
      (MTExp (foldr EFun typeType (concatMap hypo2core hypos)))
      (MEExp (kind2core df))
  GDefExpJmt (GListHypo hypos) exp kind df ->
    JDef
      (exp2coreIdent exp)
      (MTExp (foldr EFun (kind2core kind) (concatMap hypo2core hypos)))
      (MEExp (exp2core df))
  GAxiomPropJmt (GListHypo hypos) prop ->
    JStatic
      (prop2coreIdent prop)
      (foldr EFun typeProp (concatMap hypo2core hypos))
  GAxiomKindJmt (GListHypo hypos) kind ->
    JStatic
      (kind2coreIdent kind)
      (foldr EFun typeType (concatMap hypo2core hypos))
-}

hypo2core :: Hypo -> GHypo
hypo2core hypo = case hypo of
  HVarExp var kind -> 
    GVarsHypo (GListIdent [var2core var]) (exp2coreKind kind)
  HParVarExp var kind -> 
    GVarsHypo (GListIdent [var2core var]) (exp2coreKind kind)
  HExp prop -> 
    GPropHypo (exp2coreProp prop)

hypo2coreArgKind :: Hypo -> GArgKind
hypo2coreArgKind hypo = case hypo of
  HVarExp var kind -> 
    GIdentsArgKind (exp2coreKind kind) (GListIdent [var2core var]) 
  HParVarExp var kind -> 
    GIdentsArgKind (exp2coreKind kind) (GListIdent [var2core var])
  HExp kind -> 
    GKindArgKind (exp2coreKind kind)

rule2core :: Rule -> GRule
rule2core rule = case rule of
  RRule [] patt exp ->
    GNoVarRewriteRule (patt2coreExp patt) (exp2coreExp exp)
  RRule idents patt exp ->
    GRewriteRule
      (GListIdent (map ident2core idents)) (patt2coreExp patt) (exp2coreExp exp)

exp2coreKind :: Exp -> GKind
exp2coreKind exp = case exp of
  EIdent ident -> GFormalKind (ident2coreFormal ident)
  EApp _ _ -> case splitApp exp of
    (fun, args) -> case fun of
      EIdent ident ->
        GAppKind (ident2coreFormal ident) (GListExp (map exp2coreExp args))
  EFun _ _ -> 
    case splitType exp of
      (hypos, valexp) ->
        GFunKind (GListArgKind (map hypo2coreArgKind hypos)) (exp2coreKind valexp)

exp2coreProp :: Exp -> GProp
exp2coreProp exp = case exp of
  EIdent ident -> GFormalProp (ident2coreFormal ident)
  _ | exp == propFalse -> GFalseProp
  EApp _ _ -> case splitApp exp of
    (fun, args) -> case fun of
      EIdent ident ->
        GAppProp (ident2coreFormal ident) (GListExp (map exp2coreExp args))
  EFun _ _ -> case splitType exp of
    (hypos, exp) ->
      GAllProp (GListArgKind (map hypo2coreArgKind hypos)) (exp2coreProp exp)
{-
  GAndProp (GListProp props) -> foldl1 propAnd (map prop2core props)
  GOrProp (GListProp props) -> foldl1 propOr (map prop2core props)
  GIfProp a b -> propImp (prop2core a) (prop2core b)
  GNotProp a -> propNot (prop2core a)
  GIffProp a b -> propEquiv (prop2core a) (prop2core b)
  GAllProp (GListIdent idents) kind prop ->
    foldr
      (\x y ->
        propPi (kind2core kind) (EAbs (BVar (VIdent (ident2core x))) y))
      (prop2core prop)
      idents
  GExistProp (GListIdent idents) kind prop ->
    foldr
      (\x y ->
        propSigma (kind2core kind) (EAbs (BVar (VIdent (ident2core x))) y))
      (prop2core prop)
      idents
  GEqProp a b -> propEquals (exp2core a) (exp2core b)
-}

exp2coreExp :: Exp -> GExp
exp2coreExp exp = case exp of
  EIdent ident -> GFormalExp (ident2coreFormal ident)
  EApp _ _ -> case splitApp exp of
    (fun, args) -> GAppExp (exp2coreExp fun) (GListExp (map exp2coreExp args))
  EAbs _ _ -> case splitAbs exp of
    (binds, body) -> GAbsExp (GListIdent (map bind2coreIdent binds)) (exp2coreExp body)

exp2coreProof :: Exp -> GProof
exp2coreProof exp = case exp of
  EIdent ident -> GAppProof (GListProof []) (GFormalExp (ident2coreFormal ident))
  EApp _ _ -> case splitApp exp of
    (fun, args) ->
      GAppProof (GListProof (map exp2coreProof args)) (exp2coreExp fun)
  EAbs _ _ -> case splitAbs exp of
    (binds, body) -> GAbsProof (GListHypo (map (hypo2core . bind2hypo) binds)) (exp2coreProof body)

patt2coreExp :: Patt -> GExp
patt2coreExp patt = case patt of
  PVar (VIdent ident) -> GFormalExp  (ident2coreFormal ident)
  PVar _ -> GFormalExp wildFormal
  PApp _ _ -> case splitPatt patt of
    (fun, args) -> case fun of
      PVar (VIdent ident) ->
        GAppExp (GFormalExp (ident2coreFormal ident)) (GListExp (map patt2coreExp args))

ident2core :: QIdent -> GIdent
ident2core ident = case ident of
  QIdent s -> GStrIdent (GString s)

ident2coreFormal :: QIdent -> GFormal
ident2coreFormal ident = case ident of
  QIdent s -> GStrFormal (GString s)

ident2coreExp :: QIdent -> GExp
ident2coreExp ident = case ident of
  QIdent s -> GFormalExp (GStrFormal (GString s))

bind2coreIdent :: Bind -> GIdent
bind2coreIdent bind = case bind of
  BVar var -> var2core var
  BTyped var exp -> var2core var ---- add typed binding to Core?

var2core :: Var -> GIdent
var2core var = case var of
  VIdent s -> ident2core s
  VWild -> ident2core (QIdent "_") ----

splitType :: Exp -> ([Hypo], Exp)
splitType exp = case exp of
  EFun hypo body -> case splitType body of
    ([], _) -> ([hypo], body)        
    (hypos, rest) -> (hypo:hypos, rest)
  _ -> ([], exp)

splitApp :: Exp -> (Exp, [Exp])
splitApp exp = case exp of
  EApp fun arg -> case splitApp fun of
    (_, [])   -> (fun, [arg])
    (f, args) -> (f, args ++ [arg])
  _ -> (exp, [])

splitAbs :: Exp -> ([Bind], Exp)
splitAbs exp = case exp of
  EAbs bind body -> case splitAbs body of
    ([], _) -> ([bind], body)        
    (binds, rest) -> (bind:binds, rest)
  _ -> ([], exp)

splitPatt :: Patt -> (Patt, [Patt])
splitPatt patt = case patt of
  PApp fun arg -> case splitPatt fun of
    (_, [])   -> (fun, [arg])
    (f, args) -> (f, args ++ [arg])
  _ -> (patt, [])

wildIdent :: GIdent
wildIdent = GStrIdent (GString "_") ---- to be eliminated?

wildFormal :: GFormal
wildFormal = GStrFormal (GString "_") ---- to be eliminated?

bind2hypo :: Bind -> Hypo
bind2hypo bind = case bind of
  BTyped VWild exp -> HExp exp
  BTyped var exp -> HVarExp var exp
  _ -> error ("cannot infer type of binding")
