{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Core2Dedukti
import Dedukti.PrintDedukti
import Core

import PGF

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
  let ts = parse gr english jmt s
  case ts of
    t:tt -> do
      putStrLn $ showExpr [] t
      let d = jmt2dedukti "Theorem42" (fg t)
      putStrLn $ printTree d
    _ -> do
      putStrLn "no parse"
  loop gr

   

