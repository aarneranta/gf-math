{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Core2Dedukti where

import Dedukti.AbsDedukti
import Informath -- superset of MathCore
import CommonConcepts

import Data.Char


jmt2dedukti :: GJmt -> Jmt
jmt2dedukti jment = case jment of
  GAxiomJmt label (GListHypo hypos) prop ->
    JStatic
      (label2ident label)
      (foldr EFun (prop2dedukti prop) (concatMap hypo2dedukti hypos))
  GThmJmt label (GListHypo hypos) prop proof ->
    JDef
      (label2ident label)
      (MTExp (foldr EFun (prop2dedukti prop) (concatMap hypo2dedukti hypos)))
      (MEExp (proof2dedukti proof))
  GDefPropJmt label_ (GListHypo hypos) prop df ->
    JDef
      (prop2deduktiIdent prop)
      (MTExp (foldr EFun typeProp (concatMap hypo2dedukti hypos)))
      (MEExp (prop2dedukti df))
  GDefKindJmt label_ (GListHypo hypos) kind df ->
    JDef
      (kind2ident kind)
      (MTExp (foldr EFun typeType (concatMap hypo2dedukti hypos)))
      (MEExp (kind2dedukti df))
  GDefExpJmt label_ (GListHypo hypos) exp kind df ->
    JDef
      (exp2ident exp)
      (MTExp (foldr EFun (kind2dedukti kind) (concatMap hypo2dedukti hypos)))
      (MEExp (exp2dedukti df))
  GAxiomPropJmt label_ (GListHypo hypos) prop ->
    JStatic
      (prop2deduktiIdent prop)
      (foldr EFun typeProp (concatMap hypo2dedukti hypos))
  GAxiomKindJmt label_ (GListHypo hypos) kind ->
    JStatic
      (kind2ident kind)
      (foldr EFun typeType (concatMap hypo2dedukti hypos))
  GAxiomExpJmt label_ (GListHypo hypos) exp kind ->
    JStatic
      (exp2ident exp)
      (foldr EFun (kind2dedukti kind) (concatMap hypo2dedukti hypos))
  GRewriteJmt (GListRule rules) -> JRules (map rule2dedukti rules)

rule2dedukti :: GRule -> Rule
rule2dedukti rule = case rule of
  GRewriteRule (GListIdent idents) patt exp ->
    RRule (map ident2ident idents) (exp2deduktiPatt patt) (exp2dedukti exp)
  GNoVarRewriteRule patt exp ->
    rule2dedukti (GRewriteRule (GListIdent []) patt exp)

prop2dedukti :: GProp -> Exp
prop2dedukti prop = case prop of
  GFalseProp -> propFalse
  GIdentProp ident -> EIdent (ident2ident ident)
  GAndProp (GListProp props) -> foldl1 propAnd (map prop2dedukti props)
  GOrProp (GListProp props) -> foldl1 propOr (map prop2dedukti props)
  GIfProp a b -> propImp (prop2dedukti a) (prop2dedukti b)
  GNotProp a -> propNeg (prop2dedukti a)
  GIffProp a b -> propEquiv (prop2dedukti a) (prop2dedukti b)
  GAllProp (GListArgKind argkinds) prop ->
    foldr
      (\ (exp, vars) y ->
        propPi exp
          (foldr (\x z -> EAbs (BVar x) z) y vars))
        (prop2dedukti prop)
        (map argkind2dedukti argkinds)
  GExistProp (GListArgKind argkinds) prop ->
    foldr
      (\ (exp, vars) y ->
        propSigma exp
          (foldr (\x z -> EAbs (BVar x) z) y vars))
        (prop2dedukti prop)
        (map argkind2dedukti argkinds)
  GAppProp ident exps ->
    foldl1 EApp ((EIdent (ident2ident ident)) : map exp2dedukti (exps2list exps))
  GAdjProp (GRelAdj (LexRel rel) b) a ->
    foldl EApp (EIdent (QIdent (undk rel))) (map exp2dedukti [a, b])
  GAdjProp (GComparAdj (LexCompar rel) b) a ->
    foldl EApp (EIdent (QIdent (undk rel))) (map exp2dedukti [a, b])
  GAdjProp (LexAdj adj) exp ->
    EApp (EIdent (QIdent (undk adj))) (exp2dedukti exp) 
  _ -> eUndefined ---- TODO complete Informath2Core

hypo2dedukti :: GHypo -> [Hypo]
hypo2dedukti hypo = case hypo of
  GVarsHypo (GListIdent idents) kind ->
    [HVarExp (VIdent (ident2ident ident)) (kind2dedukti kind) | ident <- idents]
  GPropHypo prop ->
    [HExp (prop2dedukti prop)]

argkind2dedukti :: GArgKind -> (Exp, [Var])
argkind2dedukti argkind = case argkind of
  GIdentsArgKind kind (GListIdent idents) ->
    (kind2dedukti kind, map ident2var idents)

kind2dedukti :: GKind -> Exp
kind2dedukti kind = case kind of
  GTermKind (GTIdent ident) -> EIdent (ident2ident ident)
  GSuchThatKind ident kind prop ->
    propSigma
      (kind2dedukti kind)
      (EAbs (BVar (VIdent (ident2ident ident)))
            (prop2dedukti prop))
  GAppKind ident exps ->
    foldl1 EApp (EIdent (ident2ident ident) : map exp2dedukti (exps2list exps))
  ---- still assuming GF fun is Dedukti ident
  GNounKind (LexNoun noun) ->
    EIdent (QIdent (undk noun))
  _ -> eUndefined ---- TODO

exp2dedukti :: GExp -> Exp
exp2dedukti exp = case exp of
  GTermExp (GTIdent ident) -> EIdent (ident2ident ident)
  GAppExp exp exps ->
    foldl1 EApp (map exp2dedukti (exp : (exps2list exps)))
  GAbsExp (GListIdent idents) exp ->
    foldr
      (\x y -> EAbs (BVar (VIdent (ident2ident x))) y)
      (exp2dedukti exp)
      idents
  ---- still assuming GF fun is Dedukti ident
  GNameExp (LexName name) ->
    EIdent (QIdent (undk name))
  _ -> eUndefined ---- TODO

exp2deduktiPatt :: GExp -> Patt
exp2deduktiPatt exp = case exp of
  GTermExp (GTIdent ident) -> PVar (ident2var ident)
{- ----
  GAppExp exp (GListExp exps) ->
    foldl1 EApp (map exp2dedukti (exp : exps))
  GAbsExp (GListIdent idents) exp ->
    foldr
      (\x y -> EAbs (BVar (VIdent (ident2ident x))) y)
      (exp2dedukti exp)
      idents
  GNameExp (LexName name) ->
    EIdent (QIdent (undk name))
-}

proof2dedukti :: GProof -> Exp
proof2dedukti proof = case proof of
  GAppProof (GListProof proofs) exp ->
    foldl1 EApp (exp2dedukti exp : map proof2dedukti proofs)

ident2ident :: GIdent -> QIdent
ident2ident ident = case ident of
  GStrIdent (GString s) -> QIdent s

ident2var :: GIdent -> Var
ident2var ident = case ident of
  GStrIdent (GString "_") -> VWild
  GStrIdent (GString s) -> VIdent (QIdent s) 

exp2ident :: GExp -> QIdent
exp2ident exp = case exp of
  GTermExp (GTIdent ident) -> ident2ident ident
  _ -> QIdent (takeWhile isAlpha (show (gf exp))) ---- TODO

label2ident :: GLabel -> QIdent
label2ident label = case label of
  LexLabel s -> QIdent (undk s)
  GStrLabel (GString s) -> QIdent s

kind2ident :: GKind -> QIdent
kind2ident kind = case kind of
  GTermKind (GTIdent ident) -> ident2ident ident
  _ -> QIdent (takeWhile isAlpha (show (gf kind))) ---- TODO

prop2deduktiIdent :: GProp -> QIdent
prop2deduktiIdent prop = case prop of
  GIdentProp (GStrIdent (GString s)) -> QIdent s
  _ -> QIdent (takeWhile isAlpha (show (gf prop))) ---- TODO

eUndefined :: Exp
eUndefined = EIdent (QIdent "UNDEFINED")

exps2list :: GExps -> [GExp]
exps2list exps = case exps of
  GOneExps exp -> [exp]
  GAddExps exp exps -> exp : exps2list exps

