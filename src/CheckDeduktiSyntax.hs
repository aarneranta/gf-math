{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

-- check syntax errors in Dedukti judgements line by line
-- usage: runghc CheckDeduktiSyntax.hs <test/exx.dk | grep ERROR

module CheckDeduktiSyntax where

import Dedukti.ParDedukti
import Dedukti.ErrM

main = interact (unlines . map parseDeduktiJmt . filter (not . null) . lines)

parseDeduktiJmt :: String -> String
parseDeduktiJmt s = do
  case pJmt (myLexer s) of
    Bad e -> "ERROR: " ++ s
    Ok mo -> "OK: " ++ s


