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
  EIdent ident@(QIdent s) -> case lookupConstant s of  ---- TODO: more high level
    Just "Noun" -> GNounKind (LexNoun (dk s))
    _ -> GFormalKind (ident2coreFormal ident)
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
      EIdent conn | conn == identConj -> case splitIdent conn exp of
        exps -> GAndProp (GListProp (map exp2coreProp exps))
      EIdent conn | conn == identDisj -> case splitIdent conn exp of
        exps -> GOrProp (GListProp (map exp2coreProp exps))
      EIdent conn | conn == identImpl -> case args of
        [a, b] -> GIfProp (exp2coreProp a) (exp2coreProp b) 
      EIdent conn | conn == identEquiv -> case args of
        [a, b] -> GIffProp (exp2coreProp a) (exp2coreProp b) 
      EIdent conn | conn == identEq -> case args of
        [a, b] -> GEqProp (exp2coreExp a) (exp2coreExp b) 
      EIdent conn | conn == identNeg -> case args of
        [a] -> GNotProp (exp2coreProp a)
      EIdent ident@(QIdent pred) -> case (lookupConstant pred, args) of
        (Just "Adj", [a]) -> GAdjProp (LexAdj (dk pred)) (exp2coreExp a)     
        _  ->
          GAppProp (ident2coreFormal ident) (GListExp (map exp2coreExp args))
  EFun _ _ -> case splitType exp of
    (hypos, exp) ->
      GAllProp (GListArgKind (map hypo2coreArgKind hypos)) (exp2coreProp exp)
{-
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
-}

exp2coreExp :: Exp -> GExp
exp2coreExp exp = case exp of
  EIdent ident@(QIdent s) -> case lookupConstant s of  ---- TODO: more high level
    Just "Name" -> GNameExp (LexName (dk s))
    _ -> GFormalExp (ident2coreFormal ident)
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

splitIdent :: QIdent -> Exp -> [Exp]
splitIdent conn exp = case splitApp exp of
  (EIdent fun, [a, b]) | fun == conn -> case splitIdent conn a of
    [] -> [a, b]
    cs -> cs ++ [b]
  _ -> []

wildIdent :: GIdent
wildIdent = GStrIdent (GString "_") ---- to be eliminated?

wildFormal :: GFormal
wildFormal = GStrFormal (GString "_") ---- to be eliminated?

bind2hypo :: Bind -> Hypo
bind2hypo bind = case bind of
  BTyped VWild exp -> HExp exp
  BTyped var exp -> HVarExp var exp
  _ -> error ("cannot infer type of binding")
