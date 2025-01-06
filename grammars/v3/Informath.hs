{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Core2Dedukti
import Dedukti.PrintDedukti
import Core
import Lexing

import PGF

import System.Random

corePGFFile = "Core.pgf"
Just english = readLanguage "CoreEng"
Just jmt = readType "Jmt"

main = do
  corepgf <- readPGF corePGFFile
  loop corepgf

loop :: PGF -> IO ()
loop gr = do
  putStr "> "
  s <- getLine
  let ts = parse gr english jmt (lextex s)
  case ts of
    t:tt -> do
      putStrLn $ showExpr [] t
      let d = jmt2dedukti (fg t)
      putStrLn $ printTree d
    _ -> case s of
      "gr" -> do
        t <- genRandom gr
        putStrLn $ showExpr [] t
        putStrLn $ linearize gr english t
        let d = jmt2dedukti (fg t)
        putStrLn $ printTree d
      _ -> putStrLn "no parse"
  loop gr


genRandom gr = do
  g <- getStdGen
  let t = head (generateRandom g gr jmt)
  return t

   

