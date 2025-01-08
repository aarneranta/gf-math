{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module CommonConcepts where

import Dedukti.AbsDedukti
import Core

type CTree a = Core.Tree a
type DTree a = Dedukti.AbsDedukti.Tree a

identConj = QIdent "Conj"
identDisj = QIdent "Disj"
identImpl = QIdent "Impl"
identNeg = QIdent "Neg"
identEquiv = QIdent "Equiv"
identEq = QIdent "Eq"

-- logical constants in base.dk
propFalse = EIdent (QIdent "False")
propAnd x y = EApp (EApp (EIdent identConj) x) y
propOr x y = EApp (EApp (EIdent identDisj) x) y
propImp x y = EApp (EApp (EIdent identImpl) x) y
propEquiv x y = EApp (EApp (EIdent identEquiv) x) y
propNeg x = EApp (EIdent identNeg) x
propEq x y = EApp (EApp (EIdent identEq) x) y

propPi kind pred = EApp (EApp (EIdent (QIdent "Pi")) kind) pred
propSigma kind pred = EApp (EApp (EIdent (QIdent "Sigma")) kind) pred

-- built-in types
typeProp = EIdent (QIdent "Prop")
typeType = EIdent (QIdent "Type")

--- needed for typing conclusions of proofs
expTyped x t = EApp (EApp (EIdent (QIdent "typed")) x) t
