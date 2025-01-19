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
import qualified Forthel as F
import Core2Forthelplus (jmt2toplevel)
import Lexing

import PGF

import Data.List (partition)
import System.Random
import System.Environment (getArgs)

corePGFFile = "Core/Core.pgf"
Just english = readLanguage "CoreEng"
Just jmt = readType "Jmt"

forthelPGFFile = "forthelplus/Forthel.pgf"
Just f_english = readLanguage "ForthelEng"


data Env = Env {
 flags :: [String],
 cpgf :: PGF,
 fpgf :: PGF
--- rands :: [Expr],
--- itr :: Int
 }
--- random generation disabled for the time being

ifFlag x env = elem x (flags env)

main = do
  xx <- getArgs
  let (ff, yy) = partition ((== '-') . head) xx
  corepgf <- readPGF corePGFFile
  forthelpgf <-
    if elem "-forthel" ff
      then readPGF forthelPGFFile
      else return corepgf
  let env = Env{flags = ff, cpgf = corepgf, fpgf = forthelpgf} ---, rands = [], itr = 0}
  case yy of
    filename:_ -> do
      s <- readFile filename
      processDeduktiModule env s
    _ -> do
---      g <- getStdGen
  ---    let rs = generateRandomDepth g corepgf jmt (Just 4)
    ---  let env = env{rands = rs}
      loop env

loop :: Env -> IO ()
loop env = do
  putStr "> "
  ss <- getLine
  case ss of
    '?':s -> processCoreJmt env s
---    "gr"  -> processCoreJmtTree env (rands env !! itr env)
    '=':s -> roundtripDeduktiJmt env s
    _     -> processDeduktiJmt env ss
---  let env = env{itr = itr env + 1}
  loop env

processDeduktiModule :: Env -> String -> IO ()
processDeduktiModule env s = do
  case pModule (myLexer s) of
    Bad e -> putStrLn ("error: " ++ e)
    Ok (MJmts jmts) -> flip mapM_ jmts $ processDeduktiJmtTree env

processDeduktiJmt :: Env -> String -> IO ()
processDeduktiJmt env cs = do
  case pJmt (myLexer cs) of
    Bad e -> putStrLn ("error: " ++ e)
    Ok t -> processDeduktiJmtTree env t

roundtripDeduktiJmt :: Env -> String -> IO ()
roundtripDeduktiJmt env cs = do
  let gr = cpgf env
  case pJmt (myLexer cs) of
    Bad e -> putStrLn ("error: " ++ e)
    Ok t -> do
      putStrLn $ "#Dedukti: " ++ show t
      let gft = gf $ jmt2core t
      putStrLn $ "#Core: " ++ showExpr [] gft
      let lin = linearize gr english gft
      processCoreJmt env lin

processDeduktiJmtTree :: Env -> Jmt -> IO ()
processDeduktiJmtTree env t = do
  let gr = cpgf env
  putStrLn $ "#Dedukti: " ++ show t
  let ct = jmt2core t
  let gft = gf ct
  putStrLn $ "#Core: " ++ showExpr [] gft
  putStrLn $ linearize gr english gft
  if ifFlag "-forthel" env
    then convertCoreToForthel env ct
    else return ()

convertCoreToForthel :: Env -> GJmt -> IO ()
convertCoreToForthel env ct = do
  let fgr = fpgf env
  let ft = jmt2toplevel ct
  let gfft = F.gf ft
  putStrLn $ "#ForthelPlus: " ++ showExpr [] gfft
  putStrLn $ linearize fgr f_english gfft

processCoreJmt :: Env -> String -> IO ()
processCoreJmt env s = do
  let gr = cpgf env
  let ls = lextex s
  print ls
  let ts = parse gr english jmt ls
  print (length ts)
  case ts of
    [] -> putStrLn ("NO PARSE: " ++ ls)
    _:tt -> do
      if (length tt > 0) then (putStrLn "#AMBIGUOUS") else return ()
      flip mapM_ ts $ processCoreJmtTree env

processCoreJmtTree :: Env -> Expr -> IO ()
processCoreJmtTree env t = do
  let gr = cpgf env
  putStrLn $ "#Core: " ++ showExpr [] t
  putStrLn $ linearize gr english t
  let d = jmt2dedukti (fg t)
  putStrLn $ printTree d
