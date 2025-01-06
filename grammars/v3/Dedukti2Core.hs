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
----  JRewr rules ->
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

exp2coreKind :: Exp -> GKind
exp2coreKind exp = case exp of
  EIdent ident -> GFormalKind (ident2coreFormal ident)
  EApp _ _ -> case splitApp exp of
    (fun, args) -> case fun of
      EIdent ident ->
        GAppKind (ident2coreFormal ident) (GListExp (map exp2coreExp args))

exp2coreProp :: Exp -> GProp
exp2coreProp exp = case exp of
  EIdent ident -> GFormalProp (ident2coreFormal ident)
  _ | exp == propFalse -> GFalseProp
  EApp _ _ -> case splitApp exp of
    (fun, args) -> case fun of
      EIdent ident ->
        GAppProp (ident2coreFormal ident) (GListExp (map exp2coreExp args))
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

{-
proof2core :: GProof -> Exp
proof2core proof = case proof of
  GAppProof (GListProof proofs) exp prop ->
    expTyped
      (foldl1 EApp (exp2core exp : map proof2core proofs))
      (prop2core prop)
-}

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

