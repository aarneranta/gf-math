abstract Core = {

flags startcat = Jmt ;

cat
  Jmt ;
  Exp ;
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
  ThmProofJmt : [Hypo] -> Prop -> Proof -> Jmt ;
  ThmJmt : [Hypo] -> Prop -> Jmt ;
  
  DefPropJmt : [Hypo] -> Prop -> Prop -> Jmt ;
  DefKindJmt : [Hypo] -> Kind -> Kind -> Jmt ;
  DefExpJmt  : [Hypo] -> Exp -> Exp -> Jmt ;

  PropHypo : Prop -> Hypo ;
  VarsHypo : [Ident] -> Kind -> Hypo ;

  AppExp : Exp -> Exp -> Exp ;
  AbsExp : [Ident] -> Exp -> Exp ;
  NumExp : Float -> Exp ;
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
  SuchThatKind : Kind -> Prop -> Prop ;

  EqProp : Exp -> Exp -> Prop ;
  LtProp : Exp -> Exp -> Prop ;
  GtProp : Exp -> Exp -> Prop ;

  StrIdent : String -> Ident ;
  StrFormal : String -> Formal ;

  AppProof : [Proof] -> Exp -> Prop -> Proof ;

}