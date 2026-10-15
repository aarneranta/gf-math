module Ranking (rankTreesAndStrings, rankTreesAndLins, Scores) where

import PGF
import Environment
import Lexing (lextex, indexTex)
import ParseInformath
import Data.List (sortOn)

-- ranking trees and their linearizations with a set of scores

data Scores = Scores {
  tree_length :: Int,
  tree_depth :: Int,
  characters :: Int,
  tokens :: Int,
  subsequent_dollars :: Int,
  initial_dollars :: Int,
  extra_parses :: Int
  } deriving Show

-- for cases where (possibly expensive) scoring is not needed
noScores :: Scores
noScores = Scores 0 0 0 0 0 0 0

scoreString :: Env -> String -> Scores
scoreString env s = scoreTokens env (words (lextex s)) s

-- the scores of a string, given its tokens
scoreTokens :: Env -> [String] -> String -> Scores
scoreTokens env toks s = Scores {
  tree_length = 0, -- computed separately
  tree_depth = 0,  -- 
  characters = length s,
  tokens = length toks,
  subsequent_dollars = subdollars toks,
  initial_dollars = initdollars toks + if take 1 toks == ["$"] then 1 else 0,
  extra_parses =  -- can be expensive; return #parses -1 
    if (isFlag "-test-ambiguity" env)
    then maybe 0 ((+ (-1)) . length . take 3)
           (fst (parseJmt env jmt inds))
    else 1
  }
 where
   (inds, _) = indexTex (lextex s)
   Just jmt = readType "Jmt"
   subdollars ts = case ts of
     "$":"$":tt -> 1 + subdollars tt
     "$":",":"$":tt -> 1 + subdollars tt
     "$":".":"$":tt -> 1 + subdollars tt
     _:tt -> subdollars tt
     _ -> 0
   initdollars ts = case ts of
     ".":"$":tt -> 1 + initdollars tt
     _:tt -> initdollars tt
     _ -> 0

-- returns all scores and their weighted sum
scoreTreeAndString :: Env -> (Expr, String) -> (Scores, Int)
scoreTreeAndString env (t, s) =
  let scores = (scoreString env s){
        tree_length = treeLength t, tree_depth = treeDepth t}
  in (scores, totalScore env scores)
       

-- the total score of the scores
totalScore :: Env -> Scores -> Int
totalScore env scores =
      sum (map (\ (w, f) -> w * (f scores))
               (zip
            (scoreWeights env)
            [tree_length, tree_depth, characters, tokens,
                 subsequent_dollars, initial_dollars, extra_parses]))

-- as rankTreesAndStrings, but from linearizations whose words are the tokens,
-- so that they need not be lexed again; unl gives the string returned
rankTreesAndLins :: Env -> (String -> String) -> [(Expr, String)] -> [((Expr, String), (Scores, Int))]
rankTreesAndLins env unl tls =
  if isFlag "-no-ranking" env
  then [((t, unl l), (noScores, 0)) | (t, l) <- tls]
  else sortOn (snd . snd) [((t, s), (scores, totalScore env scores)) |
    (t, l) <- tls,
    let s = unl l,
    let scores = (scoreTokens env (words l) s){tree_length = treeLength t, tree_depth = treeDepth t}]

-- sorts trees from lowest to highest total score
rankTreesAndStrings :: Env -> [(Expr, String)] -> [((Expr, String), (Scores, Int))] 
rankTreesAndStrings env tss =
  if isFlag "-no-ranking" env
  then [(ts, (noScores, 0)) | ts <- tss]
  else sortOn (snd . snd) [(ts, scoreTreeAndString env ts) | ts <- tss]


