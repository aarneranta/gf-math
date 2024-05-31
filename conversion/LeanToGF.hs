{-# LANGUAGE GADTs, FlexibleInstances, KindSignatures, RankNTypes, TypeSynonymInstances #-}
module Main where

import AbsLean
import ParLean (pCode, pExp, myLexer)
import LayoutLean (resolveLayout)
import MathText
import PGF
import ErrM --- in some bnfc
import System.Environment (getArgs)


nlGrammar = "../grammars/MathText.pgf"

main = do
    xx <- getArgs
    gr <- readPGF nlGrammar
    interact (lean2text xx gr)


lean2text :: [String] -> PGF -> String -> String
lean2text args pgf s =
  let inp = case args of
        "Exp":_ -> "#check " ++ s ++ ";"
        _ -> s
  in processCode pgf (pCode (resolveLayout True (myLexer s)))


processCode pgf etree =
    case etree of
        Ok code ->
            unlines $
                map (\expr -> 
                    unlines [
                        show code,
	                linearizations pgf expr
	                ])
                    (lean2pgf code)
        Bad err -> err


linearizations :: PGF -> PGF.Expr -> String
linearizations pgf expr =
    unlines [
        linearize pgf cnc expr | cnc <- languages pgf
    ]


lean2pgf :: AbsLean.Code -> [PGF.Expr]
lean2pgf = map (gf . optimizeGF) . leanCode2gfParagraphs


leanCode2gfParagraphs :: AbsLean.Code -> [MathText.GParagraph]
leanCode2gfParagraphs code = case code of
    LeanCode cs -> map leanCmd2gfParagraph cs
    _ -> error $ "sorry, not yet " ++ show code


leanCmd2gfParagraph :: AbsLean.Cmd -> MathText.GParagraph
leanCmd2gfParagraph cmd = case cmd of
    CCheck e -> GParStatement (GListHypothesis []) (leanExp2gfProp e)
    _ -> error $ "sorry, not yet " ++ show cmd


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
----    EUnivVars vs typ body ->
----        GPUnivs (leanListVar2gfListVar vs) (leanExp2gfKind typ) (leanExp2gfProp body)
    EApp (EVar (VId (Id s))) (AExp e) ->
        GPAtom (GAPred1 (LexPred1 s) (leanExp2gfInd e))
    _ -> error $ "sorry, not yet " ++ show exp


leanExp2gfKind :: AbsLean.Exp -> MathText.GKind
leanExp2gfKind exp = case exp of
    EVar (VId (Id "Nat")) ->
        LexKind "Nat" ----
    EVar (VId (Id s)) ->
        GKindQN (LexQN s) ----
    _ -> error $ "sorry, not yet " ++ show exp


leanExp2gfInd :: AbsLean.Exp -> MathText.GInd
leanExp2gfInd exp = case exp of
    EInt i ->
        GIInt (GInt (fromInteger i))
    EVar (VId (Id x)) ->
        GIVar (GVString (GString x))
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
    
