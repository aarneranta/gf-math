abstract Informath = MathCore, Terms ** {

flags startcat=Jmt ;

cat
  [Adj] {2} ;
---  [Exp] {2} ;

fun
  TermExp : Term -> Exp ;
  FormulaProp : Formula -> Prop ;

  SetTerm : Set -> Term ;
  ConstTerm : Const -> Term ;
  ComparEqsign : Compar -> Eqsign ;
  AppOperTerm : Oper -> Term -> Term -> Term ;

  AndAdj : [Adj] -> Adj ;
  OrAdj : [Adj] -> Adj ;
  
---  AndExp : [Exp] -> Exp ;
---  OrExp : [Exp] -> Exp ;

  EveryKindExp : Kind -> Exp ;
  AllArgKindExp : ArgKind -> Exp ;

-- for Pathak's examples

  LetFormulaHypo : Formula -> Hypo ;
  PropJmt : [Hypo] -> Prop -> Jmt ;
  DefinedAdjJmt : [Hypo] -> Exp -> Adj -> Prop -> Jmt ;
  WeDefineAdjJmt : [Hypo] -> Exp -> Adj -> Prop -> Jmt ;

  AdjKind : Adj -> Kind -> Kind ;
  KindProp : Exp -> Kind -> Prop ;

  SomeKindExp : Kind -> Exp ;
  SomeArgKindExp : ArgKind -> Exp ;
  PostQuantProp : Prop -> Exp -> Prop ;
  IndefKindExp : Kind -> Exp ;
  IndefIdentKindExp : Ident -> Kind -> Exp ;
  EveryIdentKindExp : Ident -> Kind -> Exp ;

}