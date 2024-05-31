{-# LANGUAGE GADTs, FlexibleInstances, KindSignatures, RankNTypes, TypeSynonymInstances #-}
module Main where

import AbsLean
import ParLean (pCode, pExp, myLexer)
import LayoutLean (resolveLayout)
import MathText
import PGF
import ErrM


nlGrammar = "../grammars/MathText.pgf"

main = do
    gr <- readPGF nlGrammar
    interact (lean2text gr)


lean2text :: PGF -> String -> String
lean2text pgf s =
  let
      tokens = myLexer s
--      tokens = resolveLayout True $ myLexer s
      etree = pExp tokens -- pCode
  in case etree of
      Ok tree -> unlines [
          show tree,
	  linearizations pgf tree
	  ]
      Bad err -> err


linearizations :: PGF -> Exp -> String
linearizations pgf exp =
    unlines [
        linearize pgf cnc tree | cnc <- languages pgf
    ]
  where
    tree = lean2pgf exp


lean2pgf :: AbsLean.Exp -> PGF.Expr
lean2pgf = (gf :: GProp -> Expr) . optimizeGF . leanExp2gfProp


leanExp2gfProp :: AbsLean.Exp -> MathText.GProp
leanExp2gfProp exp = case exp of
    ENot e ->
        GPNeg (leanExp2gfProp e)
    EAnd p q ->
        GPConj GCAnd (leanExp2gfProp p) (leanExp2gfProp q)
    EOr p q ->
        GPConj GCOr (leanExp2gfProp p) (leanExp2gfProp q)
    EIf p q ->
        GPImpl (leanExp2gfProp p) (leanExp2gfProp q)
    EApp (EVar (VId (Id s))) (AExp e) ->
        GPAtom (GAPred1 (LexPred1 s) (leanExp2gfInd e))
    _ -> error $ "sorry, not yet " ++ show exp


leanExp2gfInd :: AbsLean.Exp -> MathText.GInd
leanExp2gfInd exp = case exp of
    EInt i -> GIInt (GInt (fromInteger i))
    EVar (VId (Id x)) -> GIVar (GVString (GString x))
    _ -> error $ "sorry, not yet " ++ show exp


optimizeGF :: MathText.Tree a -> MathText.Tree a
optimizeGF tree = case tree of
    GPNeg (GPAtom a) ->
        GPNegAtom a
    GPConj conj (GPAtom (GAPred1 p x)) (GPAtom (GAPred1 q y)) | p == q ->
        GPAtom (GAPred1 p (GConjInd conj (GListInd [x, y])))  
    GPConj conj (GPAtom (GAPred1 p x)) (GPAtom (GAPred1 q y)) | x == y ->
        GPAtom (GAPred1 (GConjPred1 conj (GListPred1 [p, q])) x)
    _ -> MathText.composOp optimizeGF tree
    

-- Lean: EApp (EVar (VId (Id "Even"))) (AExp (EInt 42))
-- GF:   PAtom (APred1 Even (IInt 42))