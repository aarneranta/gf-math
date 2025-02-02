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
import qualified Dedukti2Coq as DC
import qualified Dedukti2Lean as DL

import PGF

import Data.List (partition, isSuffixOf, isPrefixOf)
----import System.Random
import System.Environment (getArgs)
import System.IO

helpMsg = unlines [
  "usage: RunInformath <flag>* <file>?",
  "without arguments or flags, start interactive session",
  "  ? <string>  translate from natural language to Dedukti",
  "  <string>    translate from Dedukti to natural language",
  "with file argument: depending on file suffix,",
  "  .dk    read Dedukti file and convert to natural language of Agda",
  "  .dkgf  create UserConstants files to map Dedukti identifiers",
  "  .txt   (or any other) parse as natural language, convert to Dedukti",
  "flags:",
  "  -help         print this message",
  "  -to-agda      convert to Agda (with <file>.dk as argument)",
  "  -to-coq       convert to Coq (with <file>.dk as argument)",
  "  -to-lean      convert to Lean (with <file>.dk as argument)",
  "  -lang=<lang>  natural language to be targeted; Eng (default), Swe, Fre,...",
  "  -v            verbose output, e.g. syntax trees and intermediate results",
  "output is to stdout and can be redirected to a file to check with Dedukti or Agda."
  ]

informathPrefix = "Informath"
informathPGFFile = "grammars/" ++ informathPrefix ++ ".pgf"
Just jmt = readType "Jmt"

data Env = Env {
 flags :: [String],
 cpgf :: PGF,
 lang :: Language
 }

ifFlag x env = elem x (flags env)
ifv env act = if (ifFlag "-v" env) then act else return ()

flagValue flag dfault ff = case [f | f <- ff, isPrefixOf flag (tail f)] of
  f:_ -> drop (length flag + 2) f   -- -<flag>=<value>
  _ -> dfault

main = do
  xx <- getArgs
  let (ff, yy) = partition ((== '-') . head) xx
  corepgf <- readPGF informathPGFFile
  let Just lan = readLanguage (informathPrefix ++ (flagValue "lang" "Eng" ff))
  let env = Env{flags = ff, cpgf = corepgf, lang=lan}
  case yy of
    _ | ifFlag "-help" env -> do
      putStrLn helpMsg
    filename:_ | isSuffixOf ".dkgf" filename -> do
      mkConstants (lang env) filename
    filename:_ | isSuffixOf ".dk" filename -> do
      s <- readFile filename
      case s of
        _ | ifFlag "-to-agda" env -> DA.processDeduktiModule s
        _ | ifFlag "-to-coq" env -> DC.processDeduktiModule s
        _ | ifFlag "-to-lean" env -> DL.processDeduktiModule s
	_ -> processDeduktiModule env s
    filename:_  -> do
      s <- readFile filename
      ss <- mapM (processInformathJmt env) (filter (not . null) (lines s))
      case s of
        _ | ifFlag "-to-agda" env -> DA.processDeduktiModule (unlines ss)
        _ | ifFlag "-to-coq" env -> DC.processDeduktiModule (unlines ss)
        _ | ifFlag "-to-lean" env -> DL.processDeduktiModule (unlines ss)
	_ -> mapM_ putStrLn ss
    _ -> do
      loop env

loop :: Env -> IO ()
loop env = do
  putStr "> "
  hFlush stdout
  ss <- getLine
  case ss of
    '?':s -> processInformathJmt env s >>= putStrLn
    '=':s -> roundtripDeduktiJmt env s >> return ()
    _     -> processDeduktiJmt env ss >> return ()
  loop env

processDeduktiModule :: Env -> String -> IO ()
processDeduktiModule env s = do
  case pModule (myLexer s) of
    Bad e -> putStrLn ("error: " ++ e)
    Ok (MJmts jmts) -> flip mapM_ jmts $ processDeduktiJmtTree env

processDeduktiJmt :: Env -> String -> IO ()
processDeduktiJmt env cs = do
  case pJmt (myLexer cs) of
    Bad e -> putStrLn ("## error: " ++ e)
    Ok t -> processDeduktiJmtTree env t

roundtripDeduktiJmt :: Env -> String -> IO ()
roundtripDeduktiJmt env cs = do
  let gr = cpgf env
  case pJmt (myLexer cs) of
    Bad e -> putStrLn ("error: " ++ e)
    Ok t -> do
      ifv env $ putStrLn $ "## Dedukti: " ++ show t
      let gft = gf $ jmt2jmt t
      ifv env $ putStrLn $ "## MathCore: " ++ showExpr [] gft
      let lin = unlextex $ linearize gr (lang env) gft
      putStrLn lin
      processCoreJmt env lin

processDeduktiJmtTree :: Env -> Jmt -> IO ()
processDeduktiJmtTree env t = do
  let gr = cpgf env
  ifv env $ putStrLn $ "#Dedukti: " ++ show t
  let ct = jmt2jmt t
  let gft = gf ct
  ifv env $ putStrLn $ "## MathCore: " ++ showExpr [] gft
  ifv env $ putStrLn $ "# MathCoreEng: " ++ unlextex (linearize gr (lang env) gft)
  convertCoreToInformath env ct

convertCoreToInformath :: Env -> GJmt -> IO ()
convertCoreToInformath env ct = do
  let fgr = cpgf env
  let fts = nlg ct
  let gffts = map gf fts
  flip mapM_ gffts $ \gfft -> do
    ifv env $ putStrLn $ "## Informath: " ++ showExpr [] gfft
    putStrLn $ unlextex $ linearize fgr (lang env) gfft

processCoreJmt :: Env -> String -> IO ()
processCoreJmt env s = do
  let gr = cpgf env
  let ls = lextex s
  ifv env $ putStrLn ls
  let (mts, msg) = parseJmt gr (lang env) jmt ls
  ifv env $ putStrLn msg
  case mts of
    Just ts@(_:_) -> do
      flip mapM_ ts $ processCoreJmtTree env
    _ -> putStrLn ("NO PARSE: " ++ ls)


processCoreJmtTree :: Env -> Expr -> IO ()
processCoreJmtTree env t = do
  let gr = cpgf env
  ifv env $ putStrLn $ "## Informath: " ++ showExpr [] t
  ifv env $ putStrLn $ "# InformathEng: " ++ unlextex (linearize gr (lang env) t)
  let tr = fg t
  let str = semantics tr
  let st = gf str
  ifv env $ putStrLn $ "## MathCore: " ++ showExpr [] st
  ifv env $ putStrLn $ "# MathCoreEng: " ++ unlextex (linearize gr (lang env) st)
  let d = jmt2dedukti str
  putStrLn $ printTree d
---  convertCoreToInformath env str

processInformathJmt :: Env -> String -> IO String
processInformathJmt env s = do
  let gr = cpgf env
  let ls = lextex s
  ifv env $ putStrLn $ "# LEXED: " ++ ls
  let (mts, msg) = parseJmt gr (lang env) jmt ls
  ifv env $ putStrLn msg
  case mts of
    Just ts@(t:_) -> do
      s:_ <- flip mapM ts $ processInformathJmtTree env
      return s
    _ -> do
      ifv env $ putStrLn ("NO PARSE: " ++ ls)
      return ""

processInformathJmtTree :: Env -> Expr -> IO String
processInformathJmtTree env t = do
  let gr = cpgf env
  ifv env $ putStrLn $ "## Informath: " ++ showExpr [] t
  let tr = fg t
  let str = semantics tr
  let st = gf str
  ifv env $ putStrLn $ "## Core     : " ++ showExpr [] st
  ifv env $ putStrLn $ unlextex (linearize gr (lang env) st)
  let d = jmt2dedukti str
  let dt = printTree d
  ifv env $ putStrLn $ dt
  return dt



