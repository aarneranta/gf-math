concrete ForthelTermLatex of ForthelTerm =
  ForthelTermAscii - [ENeq, ELe, EGe, ESim, FElem]
  ** open Formal, Prelude in {

lin
  ENeq = "\\neq" ; 
  ELe = "\\le" ; 
  EGe = "\\ge" ; 
  ESim = "\\sim" ;
  FElem es e = constant (es.s ++ "\\in" ++ top e) ;

  LTExp a b = tinfixl 3 "^" (top a) (curlyStr (top b)) ;

oper
  -- to be usable at runtime, therefore ++
  mathEnvStr : Str -> Str = \s -> "$" ++ s ++ "$" ;
  curlyStr : Str -> Str = \s -> "{" ++ s ++ "}" ;


}