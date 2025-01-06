abstract Core = {

flags startcat = Jmt ;

cat
  Jmt ;
  Exp ;
  [Exp] {1} ;
  Prop ;
  [Prop] {2} ;
  Kind ;
  Hypo ;
  [Hypo] ;
  Ident ;
  [Ident] {1} ;
  Formal ;
  Proof ;
  [Proof] {0} ;

fun
  ThmJmt : Exp -> [Hypo] -> Prop -> Proof -> Jmt ;
  AxiomJmt : Exp -> [Hypo] -> Prop -> Jmt ;
  
  DefPropJmt : [Hypo] -> Prop -> Prop -> Jmt ;
  DefKindJmt : [Hypo] -> Kind -> Kind -> Jmt ;
  DefExpJmt  : [Hypo] -> Exp -> Kind -> Exp -> Jmt ;
  
  AxiomPropJmt : [Hypo] -> Prop -> Jmt ;
  AxiomKindJmt : [Hypo] -> Kind -> Jmt ;
  AxiomExpJmt  : [Hypo] -> Exp -> Kind -> Jmt ;

  PropHypo : Prop -> Hypo ;
  VarsHypo : [Ident] -> Kind -> Hypo ;

  AppExp : Exp -> [Exp] -> Exp ;
  AbsExp : [Ident] -> Exp -> Exp ;
  FormalExp : Formal -> Exp ;
  TypedExp : Exp -> Kind -> Exp ;

  AndProp : [Prop] -> Prop ;
  OrProp : [Prop] -> Prop ;
  IfProp : Prop -> Prop -> Prop ;
  IffProp : Prop -> Prop -> Prop ;
  NotProp : Prop -> Prop ;
  FalseProp : Prop ;
  AllProp : [Ident] -> Kind -> Prop -> Prop ;
  ExistProp : [Ident] -> Kind -> Prop -> Prop ;
  FormalProp : Formal -> Prop ;

  FormalKind : Formal -> Kind ;
  SuchThatKind : Ident -> Kind -> Prop -> Kind ;

  EqProp : Exp -> Exp -> Prop ;

  StrIdent : String -> Ident ;
  StrFormal : String -> Formal ;

  AppProof : [Proof] -> Exp -> Prop -> Proof ;

}