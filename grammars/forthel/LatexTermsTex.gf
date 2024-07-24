concrete LatexTermsTex of LatexTerms =
  ForthelTermsAscii - [ENeq, ELe, EGe, ESim, FElem]
  ** open Formal, Prelude in {

lin
  LENeq = "\\neq" ; 
  LELe = "\\le" ; 
  LEGe = "\\ge" ; 
  LESim = "\\sim" ;
  LFElem es e = constant (es.s ++ "\\in" ++ top e) ;

  LTExp a b = tinfixl 3 "^" a (b ** {s = curlyStr b.s}) ;

oper
  -- to be usable at runtime, therefore ++
  mathEnvStr : Str -> Str = \s -> "$" ++ s ++ "$" ;
  curlyStr : Str -> Str = \s -> "{" ++ s ++ "}" ;

}