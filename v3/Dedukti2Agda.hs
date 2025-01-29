{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Dedukti2Agda where

import Dedukti.AbsDedukti
import qualified Agda.AbsAgda as A

import qualified Agda.PrintAgda as PrA

import qualified Dedukti.ParDedukti as PD

import qualified Dedukti.ErrM as E

import System.Environment (getArgs)

-- skeleton copied from bnfc-generated SkelDedukti

import Dedukti.ErrM

type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transModule :: Module -> A.Module
transModule t = case t of
  MJmts jmts -> A.MJmts (concatMap transJmt jmts)

transJmt :: Jmt -> [A.Jmt]
transJmt t = case t of
  JStatic qident exp ->
    [A.JPost (transQIdent qident) (transExp exp)]
  JDef qident (MTExp typ) (MEExp exp) ->
    [A.JTyp (transQIdent qident) [] (transExp typ),
     A.JDef (A.PVar (A.VIdent (transQIdent qident))) (transExp exp)]
  JDef qident (MTExp typ) MENone ->
    [A.JPost (transQIdent qident) (transExp typ)]
  JInj qident mtyp mexp -> transJmt (JDef qident mtyp mexp)  
  JThm qident mtyp mexp -> transJmt (JDef qident mtyp mexp)  
  JRules rules -> map transRule rules

transRule :: Rule -> A.Jmt
transRule t = case t of
  RRule qidents_ patt exp -> A.JDef (transPatt patt) (transExp exp)

transExp :: Exp -> A.Exp
transExp t = case t of
  EIdent qident -> A.EIdent (transQIdent qident)
  EApp exp0 exp1 -> A.EApp (transExp exp0) (transExp exp1)
  EAbs bind exp -> A.EAbs (transBind bind) (transExp exp)
  EFun hypo exp -> A.EFun (transHypo hypo) (transExp exp)

transBind :: Bind -> A.Bind
transBind t = case t of
  BVar var -> A.BVar [transVar var]
--  BTyped var exp -> failure t

transVar :: Var -> A.Var
transVar t = case t of
  VIdent qident -> A.VIdent (transQIdent qident)
  VWild  -> A.VWild

transHypo :: Hypo -> A.Hypo
transHypo t = case t of
  HExp exp -> A.HExp (transExp exp)
  HVarExp var exp -> A.HVarExp [transVar var] (transExp exp)
  HParVarExp var exp -> A.HVarExp [transVar var] (transExp exp)

transPatt :: Patt -> A.Patt
transPatt t = case t of
  PVar var -> A.PVar (transVar var)
  PBracket patt -> transPatt patt ---
  PApp patt0 patt1 -> A.PApp (transPatt patt0) (transPatt patt1)
--  PBind bind patt -> failure t

transQIdent :: QIdent -> A.AIdent
transQIdent t = case t of
  QIdent "Type" -> A.AIdent "Set" ---
  QIdent str -> A.AIdent str --- so far the same Ident syntax


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
    E.Ok (MJmts jmts) -> flip mapM_ jmts processDeduktiJmtTree

processDeduktiJmtTree :: Jmt -> IO ()
processDeduktiJmtTree t = do 
  putStrLn $
    map (\c -> if c==';' then '\n' else c) $  --- to remove bnfc layout artefact ;
      PrA.printTree $ transJmt t
  
