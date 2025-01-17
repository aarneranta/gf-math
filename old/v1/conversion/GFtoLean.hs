module Main where

import AbsLean
import ParLean (pCode, myLexer)
import LayoutLean (resolveLayout)
import LexLean
import MathText
import PGF


grammarNL = "../grammars/MathText.pgf"

main = do
    gr <- readPGF grammarNL
    interact (lean2text gr)


lean2text :: PGF -> String -> String
lean2text pgf s =
  let
      tokens = resolveLayout True $ myLexer s
      etree = pCode tokens
  in case etree of
      Right tree -> show tree
      Left err -> err


