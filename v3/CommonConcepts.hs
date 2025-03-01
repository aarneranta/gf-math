{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module CommonConcepts where

import Constants (constants)
import Dedukti.AbsDedukti
import Informath
import qualified Data.Map as M
import Data.List (isSuffixOf)

type CTree a = Informath.Tree a
type DTree a = Dedukti.AbsDedukti.Tree a

-- referring to mathbase.dk

identConj = QIdent "and"
identDisj = QIdent "or"
identImpl = QIdent "if"
identNeg = QIdent "not"
identEquiv = QIdent "iff"
identPi = QIdent "forall"
identSigma = QIdent "exists"

identNat =  QIdent "Nat"
identInt =  QIdent "Int"
identRat =  QIdent "Rat"
identReal =  QIdent "Real"
identPlus =  QIdent "plus"
identMinus =  QIdent "minus"
identTimes =  QIdent "times"
identDiv =  QIdent "div"
identEq =  QIdent "Eq"
identLt =  QIdent "Lt"
identGt =  QIdent "Gt"
identNeq =  QIdent "Neq"
identLeq =  QIdent "Leq"
identGeq =  QIdent "Geq"

-- these are to be peeled away
identProof = QIdent "Proof"
identElem = QIdent "Elem"

identSuchThat = QIdent "suchthat"

-- logical constants in base.dk
propFalse = EIdent (QIdent "false")
propAnd x y = EApp (EApp (EIdent identConj) x) y
propOr x y = EApp (EApp (EIdent identDisj) x) y
propImp x y = EApp (EApp (EIdent identImpl) x) y
propEquiv x y = EApp (EApp (EIdent identEquiv) x) y
propNeg x = EApp (EIdent identNeg) x

propPi kind pred = EApp (EApp (EIdent identPi) kind) pred
propSigma kind pred = EApp (EApp (EIdent identSigma) kind) pred

-- built-in types
typeProp = EIdent (QIdent "Prop")
typeType = EIdent (QIdent "Type")
typeSet = EIdent (QIdent "Set")

--- needed for typing conclusions of proofs
expTyped x t = EApp (EApp (EIdent (QIdent "typed")) x) t
expNegated x = EApp (EIdent (QIdent "neg")) x



-- constants in lexicon
---- TODO: now hardcoded, should be dynamically generated and loaded

constantMap :: M.Map String (String, String)
constantMap = M.fromList [(c, (cat, fun)) | (c, cat, fun) <- constants]

lookupConstant :: String -> Maybe (String, String)
lookupConstant c = M.lookup c constantMap

constantMapBack :: M.Map String String
constantMapBack = M.fromList [(fun, c) | (c, _, fun) <- constants]

lookupConstantBack :: String -> Maybe String
lookupConstantBack c = M.lookup c constantMapBack

-- Dedukti representation of digits
digitFuns :: [String]
digitFuns = [nn, nd]
nn = "nn"
nd = "nd"

-- Dedukti ident X becomes GF ident Dk_X
dk :: String -> String
dk s = "Dk_" ++ s

undk :: String -> String
undk s = case s of
  'D':'k':'_':c -> c
  _ -> s

