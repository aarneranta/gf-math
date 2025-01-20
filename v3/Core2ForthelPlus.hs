{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Core2ForthelPlus where

import ForthelPlus

nlg :: Tree a -> [Tree a]
nlg t = [
  t,
  aggregate (flatten t)
  ]

aggregate :: Tree a -> Tree a
aggregate t = case t of
  GAndProp (GListProp pp@(GAdjProp a x : props)) -> case getAdjs props x of
    Just adjs -> GAdjProp (GAndAdj (GListAdj (a:adjs))) x
    _ -> GAndProp (GListProp (map aggregate pp))
  GOrProp (GListProp pp@(GAdjProp a x : props)) -> case getAdjs props x of
    Just adjs -> GAdjProp (GOrAdj (GListAdj (a:adjs))) x
    _ -> GOrProp (GListProp (map aggregate pp))
  _ -> composOp aggregate t

getAdjs :: [GProp] -> GExp -> Maybe [GAdj]
getAdjs props x = case props of
  GAdjProp adj y : pp | x == y -> do
    adjs <- getAdjs pp x
    return (adj : adjs)
  prop : _ -> Nothing
  _ -> return []


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
    
