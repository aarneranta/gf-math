concrete LatexTermsTex of LatexTerms =
  ForthelTermsAscii - [ENeq, ELe, EGe, ESim, FElem]
  ** open Formal, Prelude in {

lin
  LENeq = "\\neq" ; 
  LELe = "\\leq" ; 
  LEGe = "\\geq" ; 
  LESim = "\\sim" ;
  LFElem es e = constant (es.s ++ "\\in" ++ top e) ;

  LTPower a b = tinfixl 3 "^" a (b ** {s = curlyStr b.s}) ;
  
  LTPositive c = tinfixl 3 "^" c (tconstant (curlyStr "+")) ;
  LTNegative c = tinfixl 3 "^" c (tconstant (curlyStr "-")) ;

  LTFrac a b = tconstant (macroApp "frac" (top a) (top b)) ;
  
  LTAbsolute a = tconstant ("|" ++ (top a) ++ "|") ;
  
  LTComprehension a b f =
    tconstant ("\\{" ++ top a ++ "\\in" ++ top b ++
                ":" ++ top f ++ "\\}") ;

  LTextbfExp e = e ** {s = macroApp "textbf" (top e)} ;

oper
  -- to be usable at runtime, therefore ++
  mathEnvStr : Str -> Str = \s -> "$" ++ s ++ "$" ;
  curlyStr : Str -> Str = \s -> "{" ++ s ++ "}" ;

  macroApp = overload {
    macroApp : (f : Str) -> Str = \f -> "\\" + f ;
    macroApp : (f, x : Str) -> Str = \f, x -> "\\" + f ++ "{" ++ x ++ "}" ;
    macroApp : (f, x, y : Str) -> Str = \f, x, y ->
      "\\" + f ++ "{" ++ x ++ "} {" ++ y ++ "}" ;
   } ;

}