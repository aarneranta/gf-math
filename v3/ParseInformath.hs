module ParseInformath where

import PGF2
import Data.Char(isAlpha)
import qualified Data.Map

main_pgf = "grammars/Informath.pgf"
max_number = 999 -- number of trees considered with checkVariables

-- quick stand-alone test: runghc ParseInformath.hs <test/gflean-data.txt

-- this is the function to be exported to other modules

parseJmt :: Concr -> Type -> String -> (Maybe [Expr], String)
parseJmt eng cat s =
  case parse eng cat s of
    ParseOk ps@((e, p): rest) -> 
         let trees = [t | (t, _) <- take max_number ps, checkVariables t]
         in
	 if not (null trees)
            then (Just trees, "# SUCCESS " ++ show (length trees))
            else (Just [], "# FAILURE VARCHECK")
    ParseFailed pos tok -> 
         (Nothing, "# FAILURE TOKEN " ++ show pos ++ " " ++ tok)
    ParseIncomplete -> 
         (Nothing, "# FAILURE INCOMPLETE")

-- a quick way to test e.g. on a file with jments line by line

main = do
  gr <- readPGF main_pgf
  let Just eng = Data.Map.lookup "InformathEng" (languages gr)
  doParse eng (startCat gr) (0, 0)

doParse eng cat (success, failure) = do
  s <- getLine
  putStrLn s
  if not (null s)
    then do
      let (mtree, msg) = parseJmt eng cat s
      putStrLn msg
      case mtree of
        Just (tree:_) -> do
	  print tree
	  putStrLn $ "# SUCCEED " ++ show (success + 1) ++ " FAIL " ++ show failure
          doParse eng cat (success + 1, failure)
	_ -> do
	  putStrLn $ "# SUCCEED " ++ show (success) ++ " FAIL " ++ show (failure + 1)
          doParse eng cat (success, failure + 1)
    else
      doParse eng cat (success, failure)

-- quick hack to get the effect of a callback: check that variables consist of one letter

checkVariables :: Expr -> Bool
checkVariables expr = case unApp expr of
  Just ("stringVar", [str]) -> case unStr str of
    Just [x] | isAlpha x -> True
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
