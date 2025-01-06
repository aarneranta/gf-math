{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Core2Dedukti where

import Dedukti.AbsDedukti
import Core
import CommonConcepts

import Data.Char


jmt2dedukti :: GJmt -> Jmt
jmt2dedukti jment = case jment of
  GAxiomJmt exp (GListHypo hypos) prop ->
    JStatic
      (exp2deduktiIdent exp)
      (foldr EFun (prop2dedukti prop) (concatMap hypo2dedukti hypos))
  GThmJmt exp (GListHypo hypos) prop proof ->
    JDef
      (exp2deduktiIdent exp)
      (MTExp (foldr EFun (prop2dedukti prop) (concatMap hypo2dedukti hypos)))
      (MEExp (proof2dedukti proof))
  GDefPropJmt (GListHypo hypos) prop df ->
    JDef
      (prop2deduktiIdent prop)
      (MTExp (foldr EFun typeProp (concatMap hypo2dedukti hypos)))
      (MEExp (prop2dedukti df))
  GDefKindJmt (GListHypo hypos) kind df ->
    JDef
      (kind2deduktiIdent kind)
      (MTExp (foldr EFun typeType (concatMap hypo2dedukti hypos)))
      (MEExp (kind2dedukti df))
  GDefExpJmt (GListHypo hypos) exp kind df ->
    JDef
      (exp2deduktiIdent exp)
      (MTExp (foldr EFun (kind2dedukti kind) (concatMap hypo2dedukti hypos)))
      (MEExp (exp2dedukti df))
  GAxiomPropJmt (GListHypo hypos) prop ->
    JStatic
      (prop2deduktiIdent prop)
      (foldr EFun typeProp (concatMap hypo2dedukti hypos))
  GAxiomKindJmt (GListHypo hypos) kind ->
    JStatic
      (kind2deduktiIdent kind)
      (foldr EFun typeType (concatMap hypo2dedukti hypos))
  GAxiomExpJmt (GListHypo hypos) exp kind ->
    JStatic
      (exp2deduktiIdent exp)
      (foldr EFun (kind2dedukti kind) (concatMap hypo2dedukti hypos))

prop2dedukti :: GProp -> Exp
prop2dedukti prop = case prop of
  GFalseProp -> propFalse
  GFormalProp formal -> formal2dedukti formal
  GAndProp (GListProp props) -> foldl1 propAnd (map prop2dedukti props)
  GOrProp (GListProp props) -> foldl1 propOr (map prop2dedukti props)
  GIfProp a b -> propImp (prop2dedukti a) (prop2dedukti b)
  GNotProp a -> propNot (prop2dedukti a)
  GIffProp a b -> propEquiv (prop2dedukti a) (prop2dedukti b)
  GAllProp (GListIdent idents) kind prop ->
    foldr
      (\x y ->
        propPi (kind2dedukti kind) (EAbs (BVar (VIdent (ident2dedukti x))) y))
      (prop2dedukti prop)
      idents
  GExistProp (GListIdent idents) kind prop ->
    foldr
      (\x y ->
        propSigma (kind2dedukti kind) (EAbs (BVar (VIdent (ident2dedukti x))) y))
      (prop2dedukti prop)
      idents
  GEqProp a b -> propEquals (exp2dedukti a) (exp2dedukti b)

hypo2dedukti :: GHypo -> [Hypo]
hypo2dedukti hypo = case hypo of
  GVarsHypo (GListIdent idents) kind ->
    [HVarExp (VIdent (ident2dedukti ident)) (kind2dedukti kind) | ident <- idents]
  GPropHypo prop ->
    [HExp (prop2dedukti prop)]

kind2dedukti :: GKind -> Exp
kind2dedukti kind = case kind of
  GFormalKind formal -> formal2dedukti formal
  GSuchThatKind ident kind prop ->
    propSigma
      (kind2dedukti kind)
      (EAbs (BVar (VIdent (ident2dedukti ident)))
            (prop2dedukti prop))

exp2dedukti :: GExp -> Exp
exp2dedukti exp = case exp of
  GFormalExp formal -> formal2dedukti formal
  GAppExp exp (GListExp exps) ->
    foldl1 EApp (map exp2dedukti (exp : exps))
  GAbsExp (GListIdent idents) exp ->
    foldr
      (\x y -> EAbs (BVar (VIdent (ident2dedukti x))) y)
      (exp2dedukti exp)
      idents

proof2dedukti :: GProof -> Exp
proof2dedukti proof = case proof of
  GAppProof (GListProof proofs) exp prop ->
    expTyped
      (foldl1 EApp (exp2dedukti exp : map proof2dedukti proofs))
      (prop2dedukti prop)

ident2dedukti :: GIdent -> QIdent
ident2dedukti ident = case ident of
  GStrIdent (GString s) -> QIdent s

formal2dedukti :: GFormal -> Exp
formal2dedukti formal = case formal of
  GStrFormal (GString s) -> EIdent (QIdent s)

exp2deduktiIdent :: GExp -> QIdent
exp2deduktiIdent exp = case exp of
  GFormalExp (GStrFormal (GString s)) -> QIdent s
  _ -> QIdent (takeWhile isAlpha (show (gf exp))) ---- TODO

kind2deduktiIdent :: GKind -> QIdent
kind2deduktiIdent kind = case kind of
  GFormalKind (GStrFormal (GString s)) -> QIdent s
  _ -> QIdent (takeWhile isAlpha (show (gf kind))) ---- TODO

prop2deduktiIdent :: GProp -> QIdent
prop2deduktiIdent prop = case prop of
  GFormalProp (GStrFormal (GString s)) -> QIdent s
  _ -> QIdent (takeWhile isAlpha (show (gf prop))) ---- TODO







