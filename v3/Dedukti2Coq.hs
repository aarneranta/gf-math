{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Dedukti2Coq where

import Dedukti.AbsDedukti
import qualified Coq.AbsCoq as C

import qualified Coq.PrintCoq as PrC

import qualified Dedukti.ParDedukti as PD

import qualified Dedukti.ErrM as E

import DeduktiOperations
import CommonConcepts

import System.Environment (getArgs)

-- skeleton copied from bnfc-generated SkelDedukti

import Dedukti.ErrM

baseconstants = "BaseConstants"

type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transModule :: Module -> C.Module
transModule t = case t of
  MJmts jmts -> C.MJmts (map transJmt jmts)

transJmt :: Jmt -> C.Jmt
transJmt t = case t of
  JStatic qident exp ->
    C.JAxiom (transQIdent qident) (transExpProp exp)
  JDef qident (MTExp typ) (MEExp exp) ->
    let (hypos, vtyp) = splitType typ
    in C.JDef (transQIdent qident) (transHypos hypos) (transExp vtyp) (transExp (stripAbs hypos exp))
  JThm qident (MTExp typ) (MEExp exp) ->
    let (hypos, vtyp) = splitType typ
    in C.JThm (transQIdent qident) (transHypos hypos) (transExp vtyp) (transExp (stripAbs hypos exp))
  JDef qident (MTExp typ) MENone ->
    transJmt (JStatic qident typ)
  JInj qident mtyp mexp -> transJmt (JDef qident mtyp mexp)  
---  JRules rules -> map transRule rules

---transRule :: Rule -> C.Jmt
---transRule t = case t of
---  RRule qidents_ patt exp -> C.JDef (transPatt patt) (transExp exp)

transExp :: Exp -> C.Exp
transExp t = case t of
  EIdent qident -> C.EIdent (transQIdent qident)
  EApp exp0 exp1 -> case splitApp t of
    (fun@(EIdent (QIdent c)), [arg]) | elem c ["Elem", "Proof"] -> transExp arg
    (fun@(EIdent (QIdent n)), args) | elem n ["nn", "nd"] -> case getNumber fun args of
        Just s -> C.EInt (read s) --- no C.EInt
	_ -> C.EApp (transExp exp0) (transExp exp1)
    (EIdent c, [a, b]) | c == identConj -> C.EAnd (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identDisj -> C.EOr (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identImpl -> C.EIf (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identEquiv -> C.EIff (transExp a) (transExp b)
    (EIdent c, [a])    | c == identNeg -> C.ENot (transExp a)
    (EIdent c, [a, b]) | c == identEq -> C.EEq (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identLt -> C.ELt (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identGt -> C.EGt (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identPlus -> C.EPlus (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identMinus -> C.EMinus (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identTimes -> C.ETimes (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identDiv -> C.EDiv (transExp a) (transExp b)
    (EIdent c, [a, b]) | c == identPi -> case b of
      EAbs bind body -> C.EAll [transBind bind] (transExp a) (transExp body)
      _ -> C.EApp (transExp exp0) (transExp exp1)
    (EIdent c, [a, b]) | c == identSigma -> case b of
      EAbs bind body -> C.EExist [transBind bind] (transExp a) (transExp body)
      _ -> C.EApp (transExp exp0) (transExp exp1)
    _ -> C.EApp (transExp exp0) (transExp exp1)
  EAbs bind exp -> C.EAbs [transBind bind] (transExp exp)
  EFun hypo@(HVarExp _ _) exp -> C.EFunDep (transHypo hypo) (transExp exp)
  EFun hypo@(HParVarExp _ _) exp -> C.EFunDep (transHypo hypo) (transExp exp)
  EFun hypo@(HExp typ) exp -> C.EIf (transExp typ) (transExp exp)

transExpProp :: Exp -> C.Exp
transExpProp t = case t of
  EFun (HVarExp var typ) exp -> C.EAll [transVar var] (transExp typ) (transExpProp exp)
  EFun (HParVarExp var typ) exp -> transExpProp (EFun (HVarExp var typ) exp) 
  _ -> transExp t

transBind :: Bind -> C.CIdent
transBind t = case t of
  BVar var -> transVar var
  BTyped var exp -> transVar var

transVar :: Var -> C.CIdent
transVar t = case t of
  VIdent ident -> transQIdent ident
  VWild -> C.CIdent "x_" --- ?

transHypos :: [Hypo] -> [C.Hypo]
transHypos hypos = compress (map transHypo vhypos)
  where
    vhypos = addVarsToHypos hypos
    
    compress :: [C.Hypo] -> [C.Hypo]
    compress hs = case hs of
      h@(C.HVarExp vs exp) : hh -> case span ((== exp) . hypoType) hh of
        (hh1@(_:_), hh2) -> C.HVarExp (vs ++ concatMap hypoVars hh1) exp : compress hh2
	([], _) -> h : compress hh
      [] -> []

    hypoType :: C.Hypo -> C.Exp
    hypoType (C.HVarExp _ exp) = exp
    
    hypoVars :: C.Hypo -> [C.CIdent]
    hypoVars (C.HVarExp vars _ ) = vars

transHypo :: Hypo -> C.Hypo
transHypo t = case t of
--  HExp exp -> C.HExp (transExp exp) -- not reached due to addVarsToHypos
  HVarExp var exp -> C.HVarExp [transVar var] (transExp exp)
  HParVarExp var exp -> C.HVarExp [transVar var] (transExp exp)

transQIdent :: QIdent -> C.CIdent
transQIdent t = case t of
  c | c == identPi -> C.CIdent "All" 
  c | c == identSigma -> C.CIdent "Exist"
  c | c == identNat -> C.CIdent "nat" -- "ℕ" ---- TODO find out how to make Coq recognize these
  QIdent str -> C.CIdent str ---- not quite the same ident syntax ; reserved idents in Coq!

processDeduktiModule :: String -> IO ()
processDeduktiModule s = do
  case PD.pModule (PD.myLexer s) of
    E.Bad e -> putStrLn ("error: " ++ e)
    E.Ok (MJmts jmts) -> do
--      putStrLn ("Require Import Arithm.\n") --- need to paste with BaseConstants.v
      flip mapM_ jmts processDeduktiJmtTree

processDeduktiJmtTree :: Jmt -> IO ()
processDeduktiJmtTree t = do 
  putStrLn $
    map (\c -> if c==';' then '\n' else c) $  --- to remove bnfc layout artefact ;
      PrC.printTree $ transJmt t

-- when used stand-alone
main = do
  xx <- getArgs
  case xx of
    filename:_ -> do
      s <- readFile filename
      processDeduktiModule s


