{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Core2Dedukti
import Dedukti2Core
import Dedukti.PrintDedukti
import Dedukti.ParDedukti
import Dedukti.AbsDedukti
import Dedukti.ErrM
import Informath -- superset of Core
import Core2Informath (nlg)
import Informath2Core (semantics)
import ParseInformath (parseJmt)
import Lexing
import MkConstants (mkConstants)
import qualified Dedukti2Agda as DA

import PGF

import Data.List (partition, isSuffixOf)
----import System.Random
import System.Environment (getArgs)
import System.IO

informathPGFFile = "grammars/Informath.pgf"
Just english = readLanguage "InformathEng"
Just jmt = readType "Jmt"

data Env = Env {
 flags :: [String],
 cpgf :: PGF
 }
--- random generation disabled for the time being

ifFlag x env = elem x (flags env)

main = do
  xx <- getArgs
  let (ff, yy) = partition ((== '-') . head) xx
  corepgf <- readPGF informathPGFFile
  let env = Env{flags = ff, cpgf = corepgf} ---, rands = [], itr = 0}
  case yy of
    filename:_ | isSuffixOf ".dkgf" filename -> do
      mkConstants filename
    filename:_ | isSuffixOf ".dk" filename -> do
      s <- readFile filename
      if ifFlag "-to_agda" env
        then DA.processDeduktiModule s
        else processDeduktiModule env s
    filename:_  -> do
      s <- readFile filename
      mapM_ (processInformathJmt env) (filter (not . null) (lines s))
    _ -> do
----      g <- getStdGen
      let rs = [] ---- generateRandomDepth g corepgf jmt (Just 4)
      loop env rs 0 ---- storing rs in env causes an infinite loop

loop :: Env -> [Expr] -> Int -> IO ()
loop env rs i = do
  putStr "> "
  hFlush stdout
  ss <- getLine
  case ss of
    '?':s -> processInformathJmt env s
    "gr"  -> processCoreJmtTree env (rs !! i)
    '=':s -> roundtripDeduktiJmt env s
    _     -> processDeduktiJmt env ss
  loop env rs (i + 1)

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
      let gft = gf $ jmt2jmt t
      putStrLn $ "#Core: " ++ showExpr [] gft
      let lin = linearize gr english gft
      processCoreJmt env lin

processDeduktiJmtTree :: Env -> Jmt -> IO ()
processDeduktiJmtTree env t = do
  let gr = cpgf env
  putStrLn $ "#Dedukti: " ++ show t
  let ct = jmt2jmt t
  let gft = gf ct
  putStrLn $ "#Core: " ++ showExpr [] gft
  putStrLn $ linearize gr english gft
  convertCoreToInformath env ct

convertCoreToInformath :: Env -> GJmt -> IO ()
convertCoreToInformath env ct = do
  let fgr = cpgf env
  let fts = nlg ct
  let gffts = map gf fts
  flip mapM_ gffts $ \gfft -> do
    putStrLn $ "#Informath: " ++ showExpr [] gfft
    putStrLn $ linearize fgr english gfft

processCoreJmt :: Env -> String -> IO ()
processCoreJmt env s = do
  let gr = cpgf env
  let ls = lextex s
  putStrLn ls
  let (mts, msg) = parseJmt gr english jmt ls
  putStrLn msg
  case mts of
    Just ts@(_:_) -> do
      flip mapM_ ts $ processCoreJmtTree env
    _ -> putStrLn ("NO PARSE: " ++ ls)


processCoreJmtTree :: Env -> Expr -> IO ()
processCoreJmtTree env t = do
  let gr = cpgf env
  putStrLn $ "#Core: " ++ showExpr [] t
  putStrLn $ linearize gr english t
  let tr = fg t
  let str = semantics tr
  let st = gf str
  putStrLn $ showExpr [] st
  putStrLn $ linearize gr english st
  let d = jmt2dedukti str
  putStrLn $ printTree d
  convertCoreToInformath env str

processInformathJmt :: Env -> String -> IO ()
processInformathJmt env s = do
  let gr = cpgf env
  let ls = lextex s
  putStrLn ls
  let (mts, msg) = parseJmt gr english jmt ls
  putStrLn msg
  case mts of
    Just ts@(_:_) -> do
      flip mapM_ ts $ processInformathJmtTree env
    _ -> putStrLn ("NO PARSE: " ++ ls)

processInformathJmtTree :: Env -> Expr -> IO ()
processInformathJmtTree env t = do
  let gr = cpgf env
  putStrLn $ "#Informath: " ++ showExpr [] t
  let tr = fg t
  let str = semantics tr
  let st = gf str
  putStrLn $ "#Core     : " ++ showExpr [] st
  putStrLn $ linearize gr english st
  let d = jmt2dedukti str
  putStrLn $ printTree d


