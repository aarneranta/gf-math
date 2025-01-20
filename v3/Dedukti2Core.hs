{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Dedukti2Core where

import Dedukti.AbsDedukti
import Dedukti.PrintDedukti
import ForthelPlus -- superset of Core
import CommonConcepts

import Data.Char

jmt2core :: Jmt -> GJmt
jmt2core jmt = case jmt of
  JDef ident (MTExp typ) meexp ->
    let mexp = case meexp of
          MEExp exp -> Just exp
          _ -> Nothing
    in case (splitType typ, cat ident) of
      ((hypos, kind), "Label") -> 
        (maybe GAxiomJmt (\exp x y z -> GThmJmt x y z (exp2proof exp)) mexp)
          (GListHypo (map hypo2core hypos))
          (ident2label ident)
          (exp2prop kind)
      ((hypos, kind), "Noun") -> 
          (maybe GAxiomKindJmt (\exp x y -> GDefKindJmt x y (exp2kind exp)) mexp)
            (GListHypo (map hypo2core hypos))
	    (ident2coreKindExp ident)
      ((hypos, kind), "Name") ->
          (maybe GAxiomExpJmt (\exp x y z -> GDefExpJmt x y z (exp2exp exp)) mexp)
            (GListHypo (map hypo2core hypos)) (ident2exp ident)
            (exp2kind kind)
      ((hypos, kind), "Fun") ->
        let chypos = map hypo2core hypos
        in (maybe GAxiomExpJmt (\exp x y z -> GDefExpJmt x y z (exp2exp exp)) mexp)
             (GListHypo chypos)
	     (funListExp ident (map identExp (concatMap hypoIdents chypos)))
             (exp2kind kind)
      ((hypos, kind), "Rel") ->
        let chypos = map hypo2core hypos
        in (maybe GAxiomPropJmt (\exp x y -> GDefPropJmt x y (exp2prop exp)) mexp)
             (GListHypo chypos)
	     (funListProp ident (map identExp (concatMap hypoIdents chypos)))
      ((hypos, kind), _) -> 
        GAxiomExpJmt
          (GListHypo (map hypo2core hypos)) (ident2exp ident)
          (exp2kind kind)
  JStatic ident typ ->
    jmt2core (JDef ident (MTExp typ) MENone)
  JInj ident mtyp mexp ->
    jmt2core (JDef ident mtyp mexp)
  JThm ident mtyp mexp ->
    jmt2core (JDef ident mtyp mexp) 
  JRules rules -> GRewriteJmt (GListRule (map rule2rule rules))  
  _ -> error ("not yet: " ++ printTree jmt)

cat :: QIdent -> String
cat ident@(QIdent c) = maybe "Label" id (lookupConstant c) 

funListExp :: QIdent -> [GExp] -> GExp
funListExp ident exps = case ident of
  QIdent s -> case lookupConstant s of
    Just "Fun" ->
      GFunListExp (LexFun (dk s)) (GListExp exps)
    _ -> GFormalExp (GStrFormal (GString s))

funListProp :: QIdent -> [GExp] -> GProp
funListProp ident exps = case ident of
  QIdent s -> case lookupConstant s of
    Just "Adj" | length exps == 1 ->
      GAdjProp (LexAdj (dk s)) (exps !! 0)
    Just "Rel" | length exps == 2 ->
      GRelProp (LexRel (dk s)) (exps !! 0) (exps !! 1)
    _ -> GFormalProp (GStrFormal (GString s))

identExp :: GIdent -> GExp
identExp (GStrIdent gs) = GFormalExp (GStrFormal gs)

hypoIdents :: GHypo -> [GIdent]
hypoIdents hypo = case hypo of
  GVarsHypo (GListIdent idents) kind_ -> idents
  GBareVarsHypo (GListIdent idents) -> idents
  GBareVarsHypo _ -> []

hypo2core :: Hypo -> GHypo
hypo2core hypo = case hypo of
  HVarExp var kind -> 
    GVarsHypo (GListIdent [var2core var]) (exp2kind kind)
  HParVarExp var kind -> 
    GVarsHypo (GListIdent [var2core var]) (exp2kind kind)
  HExp prop -> 
    GPropHypo (exp2prop prop)

hypo2coreArgKind :: Hypo -> GArgKind
hypo2coreArgKind hypo = case hypo of
  HVarExp var kind -> 
    GIdentsArgKind (exp2kind kind) (GListIdent [var2core var]) 
  HParVarExp var kind -> 
    GIdentsArgKind (exp2kind kind) (GListIdent [var2core var])
  HExp kind -> 
    GKindArgKind (exp2kind kind)

rule2rule :: Rule -> GRule
rule2rule rule = case rule of
  RRule [] patt exp ->
    GNoVarRewriteRule (patt2exp patt) (exp2exp exp)
  RRule idents patt exp ->
    GRewriteRule
      (GListIdent (map ident2core idents)) (patt2exp patt) (exp2exp exp)

exp2kind :: Exp -> GKind
exp2kind exp = case exp of
  EIdent ident@(QIdent s) -> case lookupConstant s of  ---- TODO: more high level
    Just "Noun" -> GNounKind (LexNoun (dk s))
    _ -> GFormalKind (ident2coreFormal ident)
  EApp _ _ -> case splitApp exp of
    (fun, args) -> case fun of
      EIdent ident ->
        GAppKind (ident2coreFormal ident) (GListExp (map exp2exp args))
  EFun _ _ -> 
    case splitType exp of
      (hypos, valexp) ->
        GFunKind (GListArgKind (map hypo2coreArgKind hypos)) (exp2kind valexp)

exp2prop :: Exp -> GProp
exp2prop exp = case exp of
  _ | exp == propFalse -> GFalseProp
  EIdent ident -> GFormalProp (ident2coreFormal ident)
  EApp _ _ -> case splitApp exp of
    (fun, args) -> case fun of
      EIdent conn | conn == identConj -> case splitIdent conn exp of
        exps -> GAndProp (GListProp (map exp2prop exps))
      EIdent conn | conn == identDisj -> case splitIdent conn exp of
        exps -> GOrProp (GListProp (map exp2prop exps))
      EIdent conn | conn == identImpl -> case args of
        [a, b] -> GIfProp (exp2prop a) (exp2prop b) 
      EIdent conn | conn == identEquiv -> case args of
        [a, b] -> GIffProp (exp2prop a) (exp2prop b) 
      EIdent conn | conn == identSigma -> case args of
        [kind, EAbs bind prop] ->
          GExistProp
            (GListArgKind [hypo2coreArgKind (HVarExp (bind2var bind) kind)])
            (exp2prop prop)
      EIdent conn | conn == identPi -> case args of
        [kind, EAbs bind prop] ->
          GAllProp
            (GListArgKind [hypo2coreArgKind (HVarExp (bind2var bind) kind)])
            (exp2prop prop)
      EIdent conn | conn == identNeg -> case args of
        [a] -> case exp2prop a of
          GAdjProp adj x -> GNotAdjProp adj x
	  GRelProp rel x y -> GNotRelProp rel x y
          p -> GNotProp p
      EIdent ident@(QIdent pred) -> case (lookupConstant pred, args) of
        (Just "Adj", [a]) -> GAdjProp (LexAdj (dk pred)) (exp2exp a)     
        (Just "Rel", [a, b]) -> GRelProp (LexRel (dk pred)) (exp2exp a) (exp2exp b)
        _  ->
          GAppProp (ident2coreFormal ident) (GListExp (map exp2exp args))
  EFun _ _ -> case splitType exp of
    (hypos, exp) ->
      GAllProp (GListArgKind (map hypo2coreArgKind hypos)) (exp2prop exp)
  EAbs _ _ -> case splitAbs exp of
    (binds, body) -> (exp2prop body) ---- TODO find way to express binds here

exp2exp :: Exp -> GExp
exp2exp exp = case exp of
  EIdent ident@(QIdent s) -> case lookupConstant s of  ---- TODO: more high level
    Just "Name" -> GNameExp (LexName (dk s))
    _ -> GFormalExp (ident2coreFormal ident)
  EApp _ _ -> case splitApp exp of
    (fun, args) -> case fun of
      EIdent ident@(QIdent f) -> case (lookupConstant f, args) of
        (Just "Fun", exps) -> GFunListExp (LexFun (dk f)) (GListExp (map exp2exp exps))     
        _ -> GAppExp (exp2exp fun) (GListExp (map exp2exp args))
      _ -> GAppExp (exp2exp fun) (GListExp (map exp2exp args))
  EAbs _ _ -> case splitAbs exp of
    (binds, body) -> GAbsExp (GListIdent (map bind2coreIdent binds)) (exp2exp body)

exp2proof :: Exp -> GProof
exp2proof exp = case exp of
  EIdent ident -> GAppProof (GListProof []) (GFormalExp (ident2coreFormal ident))
  EApp _ _ -> case splitApp exp of
    (fun, args) ->
      GAppProof (GListProof (map exp2proof args)) (exp2exp fun)
  EAbs _ _ -> case splitAbs exp of
    (binds, body) -> GAbsProof (GListHypo (map bind2coreHypo binds)) (exp2proof body)

patt2exp :: Patt -> GExp
patt2exp patt = case patt of
  PVar (VIdent ident) -> ident2exp ident
  PVar _ -> GFormalExp wildFormal
  PApp _ _ -> case splitPatt patt of
    (fun, args) -> case fun of
      PVar (VIdent ident) ->
        funListExp ident (map patt2exp args)

ident2core :: QIdent -> GIdent
ident2core ident = case ident of
  QIdent s -> GStrIdent (GString s)

ident2coreFormal :: QIdent -> GFormal
ident2coreFormal ident = case ident of
  QIdent s -> GStrFormal (GString s)

ident2exp :: QIdent -> GExp
ident2exp ident = case ident of
  QIdent s -> case lookupConstant s of
    Just "Name" -> GNameExp (LexName (dk s))
    _ -> GFormalExp (GStrFormal (GString s))

ident2label :: QIdent -> GLabel
ident2label ident = case ident of
  QIdent s -> case lookupConstant s of
    Just "Label" -> LexLabel (dk s)
    _ -> GStrLabel (GString s)

ident2coreKindExp :: QIdent -> GKind
ident2coreKindExp ident = case ident of
  QIdent s -> case lookupConstant s of
    Just "Noun" -> GNounKind (LexNoun (dk s))
    _ -> GFormalKind (ident2coreFormal ident)

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

-- needed in proofs by abstraction
bind2coreHypo :: Bind -> GHypo
bind2coreHypo bind = case bind of
  BTyped VWild exp ->
    GPropHypo (exp2prop exp)  
  BTyped var exp ->
    GVarsHypo (GListIdent [var2core var]) (exp2kind exp)  
  BVar var ->  
    GBareVarsHypo (GListIdent [var2core var])

-- used in quantified propositions
bind2var :: Bind -> Var
bind2var bind = case bind of
  BVar v -> v
  BTyped v _ -> v

