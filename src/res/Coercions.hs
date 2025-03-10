{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Coercions where

import Dedukti.AbsDedukti
import Dedukti.ParDedukti
import Dedukti.PrintDedukti
import Dedukti.ErrM

import qualified Data.Map as M
import System.Environment

baseconstants_file = "BaseConstants.dk"

data Env = Env {
  types :: M.Map QIdent Exp,
  coercions :: M.Map (Exp, Exp) QIdent
  }

lookupType :: Env -> QIdent -> Err Exp
lookupType env ident = maybe (Bad ("lookupType " ++ show ident)) Ok (M.lookup ident (types env))

lookupCoercion :: Env -> (Exp, Exp) -> Err QIdent
lookupCoercion env exps = maybe (Bad ("lookupCoercions " ++ show exps)) Ok (M.lookup exps (coercions env))

informathCoercions ::  M.Map (Exp, Exp) QIdent
informathCoercions = M.fromList [
  ((typeDig, typeNat), QIdent "nd"),
  ((typeNat, typeInt), QIdent "nat2int"),
  ((typeNat, typeReal), QIdent "nat2real"),
  ((typeInt, typeReal), QIdent "int2real"),
  ((typeRat, typeReal), QIdent "rat2real"),
  ((typeSet, typeType), QIdent "Elem"),
  ((typeProp, typeType), QIdent "Proof")
  ]

typeDig = EApp (EIdent (QIdent "Elem")) (EIdent (QIdent "Dig"))
typeNat = EApp (EIdent (QIdent "Elem")) (EIdent (QIdent "Nat"))
typeInt = EApp (EIdent (QIdent "Elem")) (EIdent (QIdent "Int"))
typeRat = EApp (EIdent (QIdent "Elem")) (EIdent (QIdent "Rat"))
typeReal = EApp (EIdent (QIdent "Elem")) (EIdent (QIdent "Real"))
typeType = EIdent (QIdent "Type")
typeSet = EIdent (QIdent "Set")
typeProp = EIdent (QIdent "Prop")

collectTypes :: Module -> M.Map QIdent Exp
collectTypes mo@(MJmts jmts) = M.fromList (concatMap getType jmts)
  where
    getType :: Jmt -> [(QIdent, Exp)]
    getType jmt = case jmt of
      JStatic ident exp -> [(ident, exp)]
      JDef ident (MTExp exp) _ ->  [(ident, exp)]
      JInj ident (MTExp exp) _ ->  [(ident, exp)]
      JThm ident (MTExp exp) _ ->  [(ident, exp)]
      _ -> []

-- partial type inference; we assume that the file is well-typed except for coercions
inferType :: Env -> Exp -> Err Exp
inferType env exp = case exp of
  EIdent ident -> lookupType env ident
  EApp f x -> do
    (c, _) <- unApp f
    typ <- lookupType env c
    return (snd (unType typ))
  EFun _ _ -> return typeType
  _ -> Bad $ "cannot infer type of " ++ printTree exp

unApp :: Tree a -> Err (QIdent, [Exp])
unApp exp = case exp of
  EApp f x -> do
    (c, xx) <- unApp x
    return (c, xx ++ [x])
  EIdent c -> return (c, [])
  _ -> Bad $ "cannot get ident+args from " ++ show exp

unType :: Exp -> ([Hypo], Exp)
unType exp = case exp of
  EFun hypo val -> case unType val of
    (hypos, v) -> (hypo:hypos, v)
  _ -> ([], exp)

{-
insertCoercions :: Env -> Tree a -> Err (Tree a)
insertCoercions env t = case t of
  EApp f x -> do
    (c, xx) <- unApp t
    typ <- lookupType env c
    let (hypos, val) = unType typ
-}    



-- to test stand-alone

main = do
  filename:_ <- getArgs
  s <- readFile baseconstants_file
  case pModule (myLexer s) of
    Bad e -> putStrLn ("error: " ++ e)
    Ok mo -> mapM_ (putStrLn . showType) (M.toList (collectTypes mo))

showType (ident, exp) = printTree (JStatic ident exp)



    
