module ParseInformath where

import PGF
import Data.Char(isAlpha)
import qualified Data.Map

main_pgf = "grammars/Informath.pgf"
max_number = 9999 -- number of trees considered with checkVariables

-- quick stand-alone test: runghc ParseInformath.hs <test/gflean-data.txt

-- this is the function to be exported to other modules

parseJmt :: PGF -> Language -> Type -> String -> (Maybe [Expr], String)
parseJmt gr eng cat s =
  case fst (parse_ gr eng cat (Just 4) s) of  --- Just 4 is default in PGF.parse
    ParseOk ps -> 
         let trees = [t | t <- take max_number ps, checkVariables t]
         in
	 if not (null trees)
            then (Just trees, "# SUCCESS " ++ show (length trees))
            else (Just [], "# FAILURE VARCHECK")
    ParseFailed pos -> 
         (Nothing, "# FAILURE AT " ++ show pos)
    ParseIncomplete -> 
         (Nothing, "# FAILURE INCOMPLETE")

-- a quick way to test e.g. on a file with jments line by line

main = do
  gr <- readPGF main_pgf
  let Just eng = readLanguage "InformathEng"
  doParse gr eng (startCat gr) (0, 0)

doParse gr eng cat (success, failure) = do
  s <- getLine
  putStrLn s
  if not (null s)
    then do
      let (mtree, msg) = parseJmt gr eng cat s
      putStrLn msg
      case mtree of
        Just (tree:_) -> do
	  putStrLn $ showExpr [] tree
	  putStrLn $ "# SUCCEED " ++ show (success + 1) ++ " FAIL " ++ show failure
          doParse gr eng cat (success + 1, failure)
	_ -> do
	  putStrLn $ "# SUCCEED " ++ show (success) ++ " FAIL " ++ show (failure + 1)
          doParse gr eng cat (success, failure + 1)
    else
      doParse gr eng cat (success, failure)

-- quick hack to get the effect of a callback: check that variables consist of one letter
-- and that numbers don't overshadow Dedukti digits

checkVariables :: Expr -> Bool
checkVariables expr = case unApp expr of
  Just (f, [x]) | showCId f == "StrIdent" -> case showExpr [] x of
    [_,c,_] | isAlpha c -> True
    _ -> False
  Just (_, args) -> all checkVariables args
  _ -> True


{-
variableCallback :: (AbsName,[(Cat,LiteralCallback)])
variableCallBack = ("Informath", [("Var", pvar)])

-- = PGF -> (ConcName,Concr) -> String -> String -> Int -> Maybe (Expr,Float,Int)

pvar :: LiteralCallback
pvar pgf (lang, concr) sentence lin_idx offset = undefined ---- TODO: recognize variables
-}
