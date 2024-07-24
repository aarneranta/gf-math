concrete ForthelTermLatex of ForthelTerm =
  ForthelTermAscii - [ENeq, ELe, EGe, ESim, FElem]
  ** open Formal, Prelude in {

lin
  ENeq = "\\neq" ; 
  ELe = "\\le" ; 
  EGe = "\\ge" ; 
  ESim = "\\sim" ;
  FElem es e = constant (es.s ++ "\\in" ++ top e) ;



}