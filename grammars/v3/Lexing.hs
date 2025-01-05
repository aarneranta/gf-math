module Lexing where

lextex :: String -> String
lextex s = case s of
  c:cs | elem c "$*.,()" -> ' ':c:' ': lextex cs
  c:cs -> c : lextex cs
  _ -> s
