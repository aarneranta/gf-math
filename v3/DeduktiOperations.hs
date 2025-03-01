{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module DeduktiOperations where

import Dedukti.AbsDedukti
import Dedukti.PrintDedukti
import CommonConcepts

import Data.Char
import qualified Data.Map as M

-- frequency map of identifiers in code, excluding bound variables

identsInTypes :: Tree a -> M.Map QIdent Int
identsInTypes t = M.fromListWith (+) [(x, 1) | x <- ids t] where
  ids :: Tree a -> [QIdent]
  ids tree = case tree of
    EIdent qident -> [qident]
    EAbs bind exp -> [x | x <- ids exp, VIdent x /= bind2var bind]
    EFun (HVarExp var exp) body -> ids exp ++ [x | x <- ids body, VIdent x /= var]
    EFun (HParVarExp var exp) body -> ids  (EFun (HVarExp var exp) body)
    RRule pattbinds patt exp ->
      [x | x <- ids patt ++ ids exp, notElem x (pattbindIdents pattbinds)] ---- types in pattbinds
    JStatic qident typ -> qident : ids typ
    JDef qident typ exp -> qident : ids typ ++ ids exp
    JThm qident typ exp -> qident : ids typ ++ ids exp
    JInj qident typ exp -> qident : ids typ ++ ids exp
    
    _ -> composOpMPlus ids tree


-- consider only typings, for instance when generating natural language
dropDefinitions :: Module -> Module
dropDefinitions (MJmts jmts) = MJmts (concatMap drops jmts) where
  drops :: Jmt -> [Jmt]
  drops jmt = case jmt of
    JDef qident (MTExp typ) _ -> [JStatic qident typ]
    JThm qident (MTExp typ) _ -> [JStatic qident typ]
    JInj qident (MTExp typ) _ -> [JStatic qident typ]
    JStatic _ _ -> [jmt]
    _ -> []

splitType :: Exp -> ([Hypo], Exp)
splitType exp = case exp of
  EFun hypo body -> case splitType body of
    ([], _) -> ([hypo], body)        
    (hypos, rest) -> (hypo:hypos, rest)
  _ -> ([], exp)

addVarsToHypos :: [Hypo] -> [Hypo]
addVarsToHypos = adds vars where
  adds :: [Var] -> [Hypo] -> [Hypo]
  adds vs hypos = case hypos of
    HExp exp : hh -> HVarExp (head vs) exp : adds (tail vs) hh
    hypo@(HVarExp var _) : hh -> hypo : adds (filter (/= var) vs) hh
    hypo@(HParVarExp var _) : hh -> hypo : adds (filter (/= var) vs) hh
    _ -> []
  vars = [VIdent (QIdent s) |
           s <- ["x", "y", "z", "u", "v", "w"] ++ ["X"  ++ show i | i <- [1..11]]]
	 --- finite list so that filter works

-- strip abstraction when function type arguments are moved to hypos, as in Lean
stripAbs :: [Hypo] -> Exp -> Exp
stripAbs hypos exp = case (hypos, exp) of
  (h:hs, EAbs _ body) -> stripAbs hs body
  _ -> exp

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

pattbindIdents :: [Pattbind] -> [QIdent]
pattbindIdents = concatMap bident where
  bident :: Pattbind -> [QIdent]
  bident pattbind = case pattbind of
    PBVar (VIdent x) -> [x]
    PBTyped (VIdent x) _ -> [x]
    _ -> []
    