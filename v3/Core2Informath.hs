{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Core2Informath where

import Informath

nlg :: Tree a -> [Tree a]
nlg t = [viaft] --- [t, ft, aft, iaft, viaft]
 where
   ut = uncoerce t
   ft = formalize ut
   aft = aggregate (flatten ft)
   iaft = insitu aft
   viaft = varless iaft

uncoerce :: Tree a -> Tree a
uncoerce t = case t of
  GProofProp prop -> uncoerce prop
  GElemKind kind -> uncoerce kind
  GCoercionExp coercion_ exp -> uncoerce exp
  _ -> composOp uncoerce t

formalize :: Tree a -> Tree a
formalize t = case t of
  GAdjProp (GComparAdj compar y) x -> case (getTerm x, getTerm y) of
    (Just tx, Just ty) ->
      GFormulaProp (GFEquation (GEBinary (GComparEqsign compar) tx ty))
    _ -> GAdjProp (GComparAdj compar (formalize x)) (formalize y)
  GOperListExp oper xy@(GAddExps x (GOneExps y)) -> case (getTerm x, getTerm y) of
    (Just tx, Just ty) -> GTermExp (GAppOperTerm oper tx ty)
    _ -> GOperListExp oper (formalize xy)
  GConstExp const -> GTermExp (GConstTerm const)
  _ -> composOp formalize t

getTerm :: GExp -> Maybe GTerm
getTerm t = case t of
  GConstExp const -> return (GConstTerm const)
  GOperListExp oper (GAddExps x (GOneExps y)) -> do
    tx <- getTerm x
    ty <- getTerm y
    return (GAppOperTerm oper tx ty)
  GTermExp term -> return term
  _ -> Nothing

aggregate :: Tree a -> Tree a
aggregate t = case t of
  GNotProp prop -> case aggregate prop of
    GAdjProp adj x -> GNotAdjProp adj x
    aprop -> GNotProp aprop
  GAndProp (GListProp pp@(GAdjProp a x : props)) -> case getAdjs props x of
    Just adjs -> GAdjProp (GAndAdj (GListAdj (a:adjs))) x
    _ -> case getAdjArgs props a of
      Just exps -> GAdjProp a (GAndExp (GListExp (x:exps)))
      _ -> GAndProp (GListProp (map aggregate pp))
  GOrProp (GListProp pp@(GAdjProp a x : props)) -> case getAdjs props x of
    Just adjs -> GAdjProp (GOrAdj (GListAdj (a:adjs))) x
    _ -> case getAdjArgs props a of
      Just exps -> GAdjProp a (GOrExp (GListExp (x:exps)))
      _ -> GOrProp (GListProp (map aggregate pp))

  GListHypo hypos -> case hypos of
    h@(GVarsHypo (GListIdent xs) kind) : hh -> case getHypos hh kind of
      Just ([], resthypos) -> case aggregate (GListHypo resthypos) of
        GListHypo aresthypos -> GListHypo (h : aresthypos)
      Just (ys, resthypos) -> case aggregate (GListHypo resthypos) of
        GListHypo aresthypos -> GListHypo (GVarsHypo (GListIdent (xs ++ ys)) kind : aresthypos)
    h : hh -> case aggregate (GListHypo hh) of
        GListHypo aresthypos -> GListHypo (h : aresthypos)
    _ -> t
	
  _ -> composOp aggregate t

getAdjs :: [GProp] -> GExp -> Maybe [GAdj]
getAdjs props x = case props of
  GAdjProp adj y : pp | x == y -> do
    adjs <- getAdjs pp x
    return (adj : adjs)
  prop : _ -> Nothing
  _ -> return []

getAdjArgs :: [GProp] -> GAdj -> Maybe [GExp]
getAdjArgs props a = case props of
  GAdjProp b y : pp | a == b -> do
    exps <- getAdjArgs pp a
    return (y : exps)
  prop : _ -> Nothing
  _ -> return []

getHypos :: [GHypo] -> GKind -> Maybe ([GIdent], [GHypo])
getHypos hypos kind = case hypos of
  GVarsHypo (GListIdent xs) k : hh | k == kind -> do
    (ys, hs) <- getHypos hh kind
    return (xs ++ ys, hs)
  hypo : hh -> return ([], hh)
  _ -> return ([], [])


flatten :: Tree a -> Tree a
flatten t = case t of
  GAndProp (GListProp props) -> case getAndProps props of
    Just ps -> GAndProp (GListProp ps)
    _ -> GAndProp (GListProp (map flatten props))
  GOrProp (GListProp props) -> case getOrProps props of
    Just ps -> GOrProp (GListProp ps)
    _ -> GOrProp (GListProp (map flatten props))
  _ -> composOp flatten t

getAndProps :: [GProp] -> Maybe [GProp]
getAndProps props = case props of
  GAndProp (GListProp ps):qs -> do
    pss <- getAndProps ps
    qss <- getAndProps qs
    return (pss ++ qss)
  prop : qs -> do
    qss <- getAndProps qs
    return (prop : qss)
  _ -> return []

getOrProps :: [GProp] -> Maybe [GProp]
getOrProps props = case props of
  GOrProp (GListProp ps):qs -> do
    pss <- getOrProps ps
    qss <- getOrProps qs
    return (pss ++ qss)
  prop : qs -> do
    qss <- getOrProps qs
    return (prop : qss)
  _ -> return []

---- a very simple special case of in situ so far
insitu :: Tree a -> Tree a
insitu t = case t of
  GAllProp (GListArgKind [argkind]) (GAdjProp adj exp) -> case subst argkind exp of
    True -> GAdjProp adj (GAllArgKindExp argkind)
    _ -> t
  _ -> composOp insitu t

subst :: GArgKind -> GExp -> Bool
subst argkind exp = case (argkind, exp) of
  (GIdentsArgKind _ (GListIdent [x]), GTermExp (GTIdent y)) -> x == y
  _ -> False

varless :: Tree a -> Tree a
varless t = case t of
  GAllArgKindExp (GIdentsArgKind kind (GListIdent [_])) -> GEveryKindExp kind
  _ -> composOp varless t

{-
> x : Pi Nat (n => Disj (Even n) (Odd n)).
Axiom x . for all natural numbers $ n $ , ( $ n $ is even or $ n $ is odd ) .
Axiom x . for all natural numbers $ n $ , $ n $ is even or odd .
Axiom x . all natural numbers $ n $ are even or odd .
Axiom x . every natural number is even or odd .
-}
