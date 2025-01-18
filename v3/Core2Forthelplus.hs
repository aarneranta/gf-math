{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Core2Forthelplus where

import Core
import qualified Forthel as F


jmt2toplevel :: GJmt -> F.GToplevel
jmt2toplevel jmt = case jmt of
  GAxiomJmt (GListHypo []) (GLabelExp (LexLabel label)) prop ->
      F.GSectionToplevel
        (F.GLabelHeader (F.LexLabel label))
        (F.GStatementSection (prop2statement prop) F.GEmptySection)
{-
  GAxiomJmt (GListHypo hypos) exp prop ->
    F.GAssumptionsSection
      (exp2termIdent exp)
      (foldr EFun (prop2statement prop) (concatMap hypo2dedukti hypos))
  GThmJmt (GListHypo hypos) exp prop proof ->
    JDef
      (exp2termIdent exp)
      (MTExp (foldr EFun (prop2statement prop) (concatMap hypo2dedukti hypos)))
      (MEExp (proof2dedukti proof))
  GDefPropJmt (GListHypo hypos) prop df ->
    JDef
      (prop2statementIdent prop)
      (MTExp (foldr EFun typeProp (concatMap hypo2dedukti hypos)))
      (MEExp (prop2statement df))
  GDefKindJmt (GListHypo hypos) kind df ->
    JDef
      (kind2deduktiIdent kind)
      (MTExp (foldr EFun typeType (concatMap hypo2dedukti hypos)))
      (MEExp (kind2dedukti df))
  GDefExpJmt (GListHypo hypos) exp kind df ->
    JDef
      (exp2termIdent exp)
      (MTExp (foldr EFun (kind2dedukti kind) (concatMap hypo2dedukti hypos)))
      (MEExp (exp2term df))
  GAxiomPropJmt (GListHypo hypos) prop ->
    JStatic
      (prop2statementIdent prop)
      (foldr EFun typeProp (concatMap hypo2dedukti hypos))
  GAxiomKindJmt (GListHypo hypos) kind ->
    JStatic
      (kind2deduktiIdent kind)
      (foldr EFun typeType (concatMap hypo2dedukti hypos))
  GAxiomExpJmt (GListHypo hypos) exp kind ->
    JStatic
      (exp2termIdent exp)
      (foldr EFun (kind2dedukti kind) (concatMap hypo2dedukti hypos))
  GRewriteJmt (GListRule rules) -> JRules (map rule2dedukti rules)
-}

label2label :: GLabel -> F.GLabel
label2label label = case label of
  LexLabel s -> F.LexLabel s

{-
rule2dedukti :: GRule -> Rule
rule2dedukti rule = case rule of
  GRewriteRule (GListIdent idents) patt exp ->
    RRule (map ident2dedukti idents) (exp2termPatt patt) (exp2term exp)
  GNoVarRewriteRule patt exp ->
    rule2dedukti (GRewriteRule (GListIdent []) patt exp)
-}

prop2statement :: GProp -> F.GStatement
prop2statement prop = case prop of
--  GFalseProp -> propFalse
--  GFormalProp formal -> formal2dedukti formal
  GAndProp (GListProp props) -> foldl1 F.GAndStatement (map prop2statement props)
  GOrProp (GListProp props) -> foldl1  F.GOrStatement (map prop2statement props)
  GIfProp a b -> F.GIfStatement (prop2statement a) (prop2statement b)
--  GNotProp a -> propNeg (prop2statement a)
  GIffProp a b -> F.GIffStatement (prop2statement a) (prop2statement b)
{-
  GAllProp (GListArgKind argkinds) prop ->
    foldr
      (\ (exp, vars) y ->
        propPi exp
          (foldr (\x z -> EAbs (BVar x) z) y vars))
        (prop2statement prop)
        (map argkind2dedukti argkinds)
  GExistProp (GListArgKind argkinds) prop ->
    foldr
      (\ (exp, vars) y ->
        propSigma exp
          (foldr (\x z -> EAbs (BVar x) z) y vars))
        (prop2statement prop)
        (map argkind2dedukti argkinds)
-}
--  GAppProp formal (GListExp exps) ->
--    foldl1 EApp (formal2dedukti formal : map exp2term exps)
  GAdjProp (LexAdj adj) exp ->
    F.GSimpleStatement (F.GOneTerms (exp2term exp))
      (F.GPosOnePredicates (F.GIsPredicate (F.GAdjAdjective (F.LexAdj adj))))
--  GRelProp (LexRel rel) a b ->
--    foldl EApp (EIdent (QIdent (undk rel))) (map exp2term [a, b]) 

{-
hypo2dedukti :: GHypo -> [Hypo]
hypo2dedukti hypo = case hypo of
  GVarsHypo (GListIdent idents) kind ->
    [HVarExp (VIdent (ident2dedukti ident)) (kind2dedukti kind) | ident <- idents]
  GPropHypo prop ->
    [HExp (prop2statement prop)]
-}

{-
argkind2dedukti :: GArgKind -> (Exp, [Var])
argkind2dedukti argkind = case argkind of
  GIdentsArgKind kind (GListIdent idents) ->
    (kind2dedukti kind, map ident2deduktiVar idents)
-}

{-
kind2dedukti :: GKind -> Exp
kind2dedukti kind = case kind of
  GFormalKind formal -> formal2dedukti formal
  GSuchThatKind ident kind prop ->
    propSigma
      (kind2dedukti kind)
      (EAbs (BVar (VIdent (ident2dedukti ident)))
            (prop2statement prop))
  GAppKind formal (GListExp exps) ->
    foldl1 EApp (formal2dedukti formal : map exp2term exps)
  ---- still assuming GF fun is Dedukti ident
  GNounKind (LexNoun noun) ->
    EIdent (QIdent (undk noun))
-}

exp2term :: GExp -> F.GTerm
exp2term exp = case exp of
  GFormalExp formal -> F.GSymbolicTerm (formal2symbterm formal)
{-
  GAppExp exp (GListExp exps) ->
    foldl1 EApp (map exp2term (exp : exps))
  GAbsExp (GListIdent idents) exp ->
    foldr
      (\x y -> EAbs (BVar (VIdent (ident2dedukti x))) y)
      (exp2term exp)
      idents
-}
  GNameExp (LexName name) ->
    F.GNameTerm (F.LexName name)


{- ----
exp2termPatt :: GExp -> Patt
exp2termPatt exp = case exp of
  GFormalExp formal -> PVar (formal2deduktiVar formal)
  GAppExp exp (GListExp exps) ->
    foldl1 EApp (map exp2term (exp : exps))
  GAbsExp (GListIdent idents) exp ->
    foldr
      (\x y -> EAbs (BVar (VIdent (ident2dedukti x))) y)
      (exp2term exp)
      idents
  GNameExp (LexName name) ->
    EIdent (QIdent (undk name))
-}

{-
proof2dedukti :: GProof -> Exp
proof2dedukti proof = case proof of
  GAppProof (GListProof proofs) exp ->
    foldl1 EApp (exp2term exp : map proof2dedukti proofs)

ident2dedukti :: GIdent -> QIdent
ident2dedukti ident = case ident of
  GStrIdent (GString s) -> QIdent s

ident2deduktiVar :: GIdent -> Var
ident2deduktiVar ident = case ident of
  GStrIdent (GString "_") -> VWild
  GStrIdent (GString s) -> VIdent (QIdent s) 
-}

formal2symbterm :: GFormal -> F.GSymbTerm
formal2symbterm formal = case formal of
  GStrFormal (GString s) -> F.GLatexExpSymbTerm (F.GTVar (F.GstringVar (F.GString s)))

{-
formal2deduktiVar :: GFormal -> Var
formal2deduktiVar formal = case formal of
  GStrFormal (GString s) -> VIdent (QIdent s)

exp2termIdent :: GExp -> QIdent
exp2termIdent exp = case exp of
  GFormalExp (GStrFormal (GString s)) -> QIdent s
  _ -> QIdent (takeWhile isAlpha (show (gf exp))) ---- TODO

kind2deduktiIdent :: GKind -> QIdent
kind2deduktiIdent kind = case kind of
  GFormalKind (GStrFormal (GString s)) -> QIdent s
  _ -> QIdent (takeWhile isAlpha (show (gf kind))) ---- TODO

prop2statementIdent :: GProp -> QIdent
prop2statementIdent prop = case prop of
  GFormalProp (GStrFormal (GString s)) -> QIdent s
  _ -> QIdent (takeWhile isAlpha (show (gf prop))) ---- TODO

-}


