{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Core2Dedukti where

import Dedukti.AbsDedukti
import Core
import CommonConcepts

import Data.Char


jmt2dedukti :: GJmt -> Jmt
jmt2dedukti jment = case jment of
  GAxiomJmt (GListHypo hypos) exp prop ->
    JStatic
      (exp2deduktiIdent exp)
      (foldr EFun (prop2dedukti prop) (concatMap hypo2dedukti hypos))
  GThmJmt (GListHypo hypos) exp prop proof ->
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
---  GRewriteJmt patt exp -> case exp2deduktiPatt patt of 
---    (idents, patt) -> JRewr [RRule idents patt (exp2dedukti exp)]

prop2dedukti :: GProp -> Exp
prop2dedukti prop = case prop of
  GFalseProp -> propFalse
  GFormalProp formal -> formal2dedukti formal
  GAndProp (GListProp props) -> foldl1 propAnd (map prop2dedukti props)
  GOrProp (GListProp props) -> foldl1 propOr (map prop2dedukti props)
  GIfProp a b -> propImp (prop2dedukti a) (prop2dedukti b)
  GNotProp a -> propNeg (prop2dedukti a)
  GIffProp a b -> propEquiv (prop2dedukti a) (prop2dedukti b)
----  GAllProp argkinds prop ->
----  GExistProp (GListIdent idents) kind prop ->
----    foldr
----      (\x y ->
----        propSigma (kind2dedukti kind) (EAbs (BVar (VIdent (ident2dedukti x))) y))
----      (prop2dedukti prop)
----      idents
  GEqProp a b -> propEq (exp2dedukti a) (exp2dedukti b)
  GAppProp formal (GListExp exps) ->
    foldl1 EApp (formal2dedukti formal : map exp2dedukti exps)
  ---- still assuming GF fun is Dedukti ident
  GAdjProp (LexAdj adj) exp ->
    EApp (EIdent (QIdent adj)) (exp2dedukti exp) 

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
  GAppKind formal (GListExp exps) ->
    foldl1 EApp (formal2dedukti formal : map exp2dedukti exps)
  ---- still assuming GF fun is Dedukti ident
  GNounKind (LexNoun noun) ->
    EIdent (QIdent noun)

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
  ---- still assuming GF fun is Dedukti ident
  GNameExp (LexName name) ->
    EIdent (QIdent name)

proof2dedukti :: GProof -> Exp
proof2dedukti proof = case proof of
  GAppProof (GListProof proofs) exp ->
    foldl1 EApp (exp2dedukti exp : map proof2dedukti proofs)

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

{-
exp2deduktiPatt :: GExp -> ([QIdent], Patt)
exp2deduktiPatt exp = ([], getPatt (exp2dedukti exp)) ---- TODO
  where
    getPatt :: Exp -> Patt
    getPatt dexp = case dexp of
      EApp fun arg -> PApp (getPatt fun) (getPatt arg)
      EIdent ident -> PVar (VIdent ident)
      ---- TODO
-}




