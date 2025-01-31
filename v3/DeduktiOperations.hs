{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module DeduktiOperations where

import Dedukti.AbsDedukti
import Dedukti.PrintDedukti
import CommonConcepts

import Data.Char


splitType :: Exp -> ([Hypo], Exp)
splitType exp = case exp of
  EFun hypo body -> case splitType body of
    ([], _) -> ([hypo], body)        
    (hypos, rest) -> (hypo:hypos, rest)
  _ -> ([], exp)

addVarsToHypos :: [Hypo] -> [Hypo]
addVarsToHypos = adds 0 where
  adds :: Int -> [Hypo] -> [Hypo]
  adds i hypos = case hypos of
    HExp exp : hh -> HVarExp (vars !! i) exp : adds (i+1) hh
    hypo : hh -> hypo : adds i hh
    _ -> []
  vars = [VIdent (QIdent s) |
           s <- ["x", "y", "z", "u", "v", "w"] ++ ["X"  ++ show i | i <- [1..]]]   

splitApp :: Exp -> (Exp, [Exp])
splitApp exp = case exp of
  EApp fun arg -> case splitApp fun of
    (_, [])   -> (fun, [arg])
    (f, args) -> (f, args ++ [arg])
  _ -> (exp, [])

splitAbs :: Exp -> ([Bind], Exp)
splitAbs exp = case exp of
  EAbs bind body -> case splitAbs body of
    ([], _) -> ([bind], body)        
    (binds, rest) -> (bind:binds, rest)
  _ -> ([], exp)

splitPatt :: Patt -> (Patt, [Patt])
splitPatt patt = case patt of
  PApp fun arg -> case splitPatt fun of
    (_, [])   -> (fun, [arg])
    (f, args) -> (f, args ++ [arg])
  _ -> (patt, [])

splitIdent :: QIdent -> Exp -> [Exp]
splitIdent conn exp = case splitApp exp of
  (EIdent fun, [a, b]) | fun == conn -> case splitIdent conn a of
    [] -> [a, b]
    cs -> cs ++ [b]
  _ -> []

getNumber :: Exp -> [Exp] -> Maybe String
getNumber fun args =
  case (fun, args) of
    (EIdent (QIdent n), [x]) | n == nd -> getDigit x
    (EIdent (QIdent n), [x, y]) | n == nn -> do
      d <- getDigit x
      n <- uncurry getNumber (splitApp y) 
      return (d ++ n)
    _ -> Nothing
 where
   getDigit :: Exp -> Maybe String
   getDigit x = case x of
     EIdent (QIdent [d]) | elem d "0123456789" -> return [d]
     _ -> Nothing

-- used in quantified propositions
bind2var :: Bind -> Var
bind2var bind = case bind of
  BVar v -> v
  BTyped v _ -> v
