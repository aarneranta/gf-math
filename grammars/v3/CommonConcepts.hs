{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module CommonConcepts where

import Dedukti.AbsDedukti
import Core

type CTree a = Core.Tree a
type DTree a = Dedukti.AbsDedukti.Tree a

-- logical constants in base.dk
propFalse = EIdent (QIdent "False")
propAnd x y = EApp (EApp (EIdent (QIdent "Conj")) x) y
propOr x y = EApp (EApp (EIdent (QIdent "Disj")) x) y
propImp x y = EApp (EApp (EIdent (QIdent "Impl")) x) y
propEquiv x y = EApp (EApp (EIdent (QIdent "Equiv")) x) y
propNot x = EApp (EIdent (QIdent "Neg")) x
propEquals x y = EApp (EApp (EIdent (QIdent "Eq")) x) y

propPi kind pred = EApp (EApp (EIdent (QIdent "Pi")) kind) pred
propSigma kind pred = EApp (EApp (EIdent (QIdent "Sigma")) kind) pred

-- built-in types
typeProp = EIdent (QIdent "Prop")
typeType = EIdent (QIdent "Type")

--- needed for typing conclusions of proofs
expTyped x t = EApp (EApp (EIdent (QIdent "typed")) x) t
