abstract ForthelPlus = Core, Terms ** {

cat
  [Adj] {2} ;
---  [Exp] {2} ;

fun
  TermExp : Term -> Exp ;
  FormulaProp : Formula -> Prop ;

  ConstTerm : Const -> Term ;
  ComparEquation : Compar -> Term -> Term -> Equation ;
  AppOperTerm : Oper -> Term -> Term -> Term ;

  AndAdj : [Adj] -> Adj ;
  OrAdj : [Adj] -> Adj ;
  
---  AndExp : [Exp] -> Exp ;
---  OrExp : [Exp] -> Exp ;

  EveryKindExp : Kind -> Exp ;
  AllArgKindExp : ArgKind -> Exp ;

}