abstract ForthelPlus = Core, Terms ** {

cat
  [Adj] {2} ;
---  [Exp] {2} ;

fun
  TermExp : Term -> Exp ;
  FormulaProp : Formula -> Prop ;

  AndAdj : [Adj] -> Adj ;
  OrAdj : [Adj] -> Adj ;
  
---  AndExp : [Exp] -> Exp ;
---  OrExp : [Exp] -> Exp ;

}