{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Core2Dedukti
import Dedukti2Core
import Dedukti.PrintDedukti
import Dedukti.ParDedukti
import Dedukti.AbsDedukti
import Core
import Lexing

import PGF

import System.Random
import System.Environment (getArgs)

corePGFFile = "Core.pgf"
Just english = readLanguage "CoreEng"
Just jmt = readType "Jmt"

main = do
  xx <- getArgs
  corepgf <- readPGF corePGFFile
  case xx of
    filename:_ -> do
      s <- readFile filename
      processDeduktiModule corepgf s
    _ -> do
      g <- getStdGen
      let randoms = generateRandomDepth g corepgf jmt (Just 4)
      loop corepgf randoms 0

loop :: PGF -> [Expr] -> Int -> IO ()
loop gr randoms n = do
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
        let t = randoms !! n
        putStrLn $ showExpr [] t
        putStrLn $ linearize gr english t
        let d = jmt2dedukti (fg t)
        putStrLn $ printTree d
      '!':cs -> do
        case pJmt (myLexer cs) of
          Left e -> putStrLn ("error: " ++ e)
          Right t -> do
            putStrLn $ show t
            let gft = gf $ jmt2core t
            putStrLn $ showExpr [] gft
            putStrLn $ linearize gr english gft
      _ -> putStrLn "no parse"
  loop gr randoms (n+1)


processDeduktiModule gr s = do
  case pModule (myLexer s) of
    Left e -> putStrLn ("error: " ++ e)
    Right (MJmts jmts) -> flip mapM_ jmts $ \t -> do
      putStrLn $ show t
      let gft = gf $ jmt2core t
      putStrLn $ showExpr [] gft
      putStrLn $ linearize gr english gft
