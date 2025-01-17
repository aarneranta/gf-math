{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Core2Dedukti
import Dedukti2Core
import Dedukti.PrintDedukti
import Dedukti.ParDedukti
import Dedukti.AbsDedukti
import Dedukti.ErrM
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
  ss <- getLine
  case ss of
    '?':s -> processCoreJmt gr s
    "gr"  -> processCoreJmtTree gr (randoms !! n)
    '=':s -> roundtripDeduktiJmt gr s   
  loop gr randoms (n+1)

processDeduktiModule :: PGF -> String -> IO ()
processDeduktiModule gr s = do
  case pModule (myLexer s) of
    Bad e -> putStrLn ("error: " ++ e)
    Ok (MJmts jmts) -> flip mapM_ jmts $ processDeduktiJmtTree gr

processDeduktiJmt :: PGF -> String -> IO ()
processDeduktiJmt gr cs = case pJmt (myLexer cs) of
  Bad e -> putStrLn ("error: " ++ e)
  Ok t -> processDeduktiJmtTree gr t

roundtripDeduktiJmt :: PGF -> String -> IO ()
roundtripDeduktiJmt gr cs = case pJmt (myLexer cs) of
  Bad e -> putStrLn ("error: " ++ e)
  Ok t -> do
    putStrLn $ "#Dedukti: " ++ show t
    let gft = gf $ jmt2core t
    putStrLn $ "#Core: " ++ showExpr [] gft
    let lin = linearize gr english gft
    processCoreJmt gr lin

processDeduktiJmtTree :: PGF -> Jmt -> IO ()
processDeduktiJmtTree gr t = do 
  putStrLn $ "#Dedukti: " ++ show t
  let gft = gf $ jmt2core t
  putStrLn $ "#Core: " ++ showExpr [] gft
  putStrLn $ linearize gr english gft

processCoreJmt :: PGF -> String -> IO ()
processCoreJmt gr s = do
  let ls = lextex s
  let ts = parse gr english jmt ls
  case ts of
    [] -> putStrLn ("NO PARSE: " ++ ls)
    _:tt -> do
      if (length tt > 0) then (putStrLn "#AMBIGUOUS") else return ()
      flip mapM_ ts $ processCoreJmtTree gr

processCoreJmtTree :: PGF -> Expr -> IO ()
processCoreJmtTree gr t = do
  putStrLn $ "#Core: " ++ showExpr [] t
  putStrLn $ linearize gr english t
  let d = jmt2dedukti (fg t)
  putStrLn $ printTree d
