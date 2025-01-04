{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Core2Dedukti where

import Dedukti.AbsDedukti
import Core


type CTree a = Core.Tree a
type DTree a = Dedukti.AbsDedukti.Tree a

-- logical constants in Dedukti are taken from the library
-- https://github.com/Deducteam/Dedukti/blob/master/libraries/theories/fol.dk
propFalse = EIdent (QIdent "false")
propAnd x y = EApp (EApp (EIdent (QIdent "and")) x) y
propOr x y = EApp (EApp (EIdent (QIdent "or")) x) y
propImp x y = EApp (EApp (EIdent (QIdent "imp")) x) y
propEquiv x y = EApp (EApp (EIdent (QIdent "equiv")) x) y
propNot x = EApp (EIdent (QIdent "not")) x
propEquals x y = EApp (EApp (EIdent (QIdent "equals")) x) y

-- these are sorted variants of qualtifiers
propPi kind pred = EApp (EApp (EIdent (QIdent "Pi")) kind) pred
propSigma kind pred = EApp (EApp (EIdent (QIdent "Sigma")) kind) pred


--- needed for typing conclusions of proofs
expTyped x t = EApp (EApp (EIdent (QIdent "typed")) x) t


jmt2dedukti :: String -> GJmt -> Jmt
jmt2dedukti name jment = case jment of
  GThmJmt (GListHypo hypos) prop ->
    JThm
      (QIdent name)
      (MTExp (foldr EFun (prop2dedukti prop) (concatMap hypo2dedukti hypos)))
      MENone
  GThmProofJmt (GListHypo hypos) prop proof ->
    JThm
      (QIdent name)
      (MTExp (foldr EFun (prop2dedukti prop) (concatMap hypo2dedukti hypos)))
      (MEExp (proof2dedukti proof))
  GDefPropJmt (GListHypo hypos) prop df ->
    JThm
      (QIdent name)
      (MTExp (foldr EFun (prop2dedukti prop) (concatMap hypo2dedukti hypos)))
      (MEExp (prop2dedukti df))
  GDefKindJmt (GListHypo hypos) kind df ->
    JThm
      (QIdent name)
      (MTExp (foldr EFun (kind2dedukti kind) (concatMap hypo2dedukti hypos)))
      (MEExp (kind2dedukti df))
  GDefExpJmt (GListHypo hypos) exp df ->
    JThm
      (QIdent name)
      (MTExp (foldr EFun (exp2dedukti exp) (concatMap hypo2dedukti hypos)))
      (MEExp (exp2dedukti df))

prop2dedukti :: GProp -> Exp
prop2dedukti prop = case prop of
  GFalseProp -> propFalse
  GFormalProp formal -> formal2dedukti formal
  GAndProp (GListProp props) -> foldl1 propAnd (map prop2dedukti props)
  GOrProp (GListProp props) -> foldl1 propOr (map prop2dedukti props)
  GIfProp a b -> propImp (prop2dedukti a) (prop2dedukti b)
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
exp2dedukti kind = case kind of
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








