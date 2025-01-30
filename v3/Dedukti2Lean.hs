{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Dedukti2Lean where

import Dedukti.AbsDedukti
import qualified Lean.AbsLean as A

import qualified Lean.PrintLean as PrA

import qualified Dedukti.ParDedukti as PD

import qualified Dedukti.ErrM as E

import Dedukti2Core (getNumber, splitApp)

import System.Environment (getArgs)

-- skeleton copied from bnfc-generated SkelDedukti

import Dedukti.ErrM

baseconstants = "BaseConstants"

type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transModule :: Module -> L.Module
transModule t = case t of
  MJmts jmts -> L.MJmts (concatMap transJmt jmts)

transJmt :: Jmt -> [L.Jmt]
transJmt t = case t of
  JStatic qident exp ->
    [L.JAxiom (transQIdent qident) (transExp exp)]
  JDef qident (MTExp typ) (MEExp exp) ->
    [L.JTyp (transQIdent qident) [] (transExp typ),
     L.JDef (L.PVar (L.VIdent (transQIdent qident))) (transExp exp)]
  JDef qident (MTExp typ) MENone ->
    [L.JPost (transQIdent qident) (transExp typ)]
  JInj qident mtyp mexp -> transJmt (JDef qident mtyp mexp)  
  JThm qident mtyp mexp -> transJmt (JDef qident mtyp mexp)  
  JRules rules -> map transRule rules

transRule :: Rule -> L.Jmt
transRule t = case t of
  RRule qidents_ patt exp -> L.JDef (transPatt patt) (transExp exp)

transExp :: Exp -> L.Exp
transExp t = case t of
  EIdent qident -> L.EIdent (transQIdent qident)
  EApp exp0 exp1 -> case splitApp t of
    (fun@(EIdent (QIdent c)), [arg]) | elem c ["Elem", "Proof"] -> transExp arg
    (fun@(EIdent (QIdent n)), args) | elem n ["nn", "nd"] -> case getNumber fun args of
        Just s -> L.EIdent (L.LIdent s) --- no L.EInt
	_ -> L.EApp (transExp exp0) (transExp exp1)
    _ -> L.EApp (transExp exp0) (transExp exp1)
  EAbs bind exp -> L.EAbs (transBind bind) (transExp exp)
  EFun hypo exp -> L.EFun (transHypo hypo) (transExp exp)

transBind :: Bind -> L.Bind
transBind t = case t of
  BVar var -> L.BVar [transVar var]
--  BTyped var exp -> failure t

transVar :: Var -> L.Var
transVar t = case t of
  VIdent qident -> L.VIdent (transQIdent qident)
  VWild  -> L.VWild

transHypo :: Hypo -> L.Hypo
transHypo t = case t of
  HExp exp -> L.HExp (transExp exp)
  HVarExp var exp -> L.HVarExp [transVar var] (transExp exp)
  HParVarExp var exp -> L.HVarExp [transVar var] (transExp exp)

transQIdent :: QIdent -> L.LIdent
transQIdent t = case t of
  QIdent "forall" -> L.LIdent "all" -- reserved word
  QIdent "Type" -> L.LIdent "Set" ---
  QIdent str -> L.LIdent str --- so far the same Ident syntax


-- when used stand-alone
main = do
  xx <- getArgs
  case xx of
    filename:_ -> do
      s <- readFile filename
      processDeduktiModule s

processDeduktiModule :: String -> IO ()
processDeduktiModule s = do
  case PD.pModule (PD.myLexer s) of
    E.Bad e -> putStrLn ("error: " ++ e)
    E.Ok (MJmts jmts) -> do
      putStrLn ("open import " ++ baseconstants ++ "\n") 
      flip mapM_ jmts processDeduktiJmtTree

processDeduktiJmtTree :: Jmt -> IO ()
processDeduktiJmtTree t = do 
  putStrLn $
    map (\c -> if c==';' then '\n' else c) $  --- to remove bnfc layout artefact ;
      PrL.printTree $ transJmt t
  
