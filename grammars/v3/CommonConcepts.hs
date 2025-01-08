{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module CommonConcepts where

import Constants (constants)
import Dedukti.AbsDedukti
import Core
import qualified Data.Map as M

type CTree a = Core.Tree a
type DTree a = Dedukti.AbsDedukti.Tree a

identConj = QIdent "Conj"
identDisj = QIdent "Disj"
identImpl = QIdent "Impl"
identNeg = QIdent "Neg"
identEquiv = QIdent "Equiv"

-- logical constants in base.dk
propFalse = EIdent (QIdent "False")
propAnd x y = EApp (EApp (EIdent identConj) x) y
propOr x y = EApp (EApp (EIdent identDisj) x) y
propImp x y = EApp (EApp (EIdent identImpl) x) y
propEquiv x y = EApp (EApp (EIdent identEquiv) x) y
propNeg x = EApp (EIdent identNeg) x

propPi kind pred = EApp (EApp (EIdent (QIdent "Pi")) kind) pred
propSigma kind pred = EApp (EApp (EIdent (QIdent "Sigma")) kind) pred

-- built-in types
typeProp = EIdent (QIdent "Prop")
typeType = EIdent (QIdent "Type")

--- needed for typing conclusions of proofs
expTyped x t = EApp (EApp (EIdent (QIdent "typed")) x) t


-- constants in lexicon
---- TODO: now hardcoded, should be dynamically generated and loaded

constantMap :: M.Map String String
constantMap = M.fromList constants

lookupConstant :: String -> Maybe String
lookupConstant c = M.lookup c constantMap

-- Dedukti ident X becomes GF ident Dk_X
dk :: String -> String
dk s = "Dk_" ++ s

undk :: String -> String
undk s = case s of
  'D':'k':'_':c -> c
  _ -> s



