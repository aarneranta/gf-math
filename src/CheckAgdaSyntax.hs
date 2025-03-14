{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

-- check syntax errors in Agda judgements line by line
-- usage: runghc CheckAgdaSyntax.hs <exx.agda | grep ERROR

module CheckAgdaSyntax where

import Agda.ParAgda
import Agda.ErrM

main = interact (unlines . map parseAgdaJmt . filter (not . null) . lines)

parseAgdaJmt :: String -> String
parseAgdaJmt s = do
  case pJmt (myLexer (s ++ " ;")) of
    Bad e -> "ERROR: " ++ s
    Ok mo -> "OK: " ++ s


