abstract Core = Constants ** {

flags startcat = Jmt ;

cat
  Jmt ;
  Exp ;
  [Exp] {1} ;
  Prop ;
  [Prop] {2} ;
  Kind ;
  ArgKind ;
  [ArgKind] {1} ;
  Hypo ;
  [Hypo] ;
  Ident ;
  [Ident] {1} ;
  Formal ;
  Proof ;
  [Proof] {0} ;
  Rule ;
  [Rule] {1} ;

fun
  ThmJmt : [Hypo] -> Exp -> Prop -> Proof -> Jmt ;
  AxiomJmt : [Hypo] -> Exp -> Prop -> Jmt ;
  
  DefPropJmt : [Hypo] -> Prop -> Prop -> Jmt ;
  DefKindJmt : [Hypo] -> Kind -> Kind -> Jmt ;
  DefExpJmt  : [Hypo] -> Exp -> Kind -> Exp -> Jmt ;
  
  AxiomPropJmt : [Hypo] -> Prop -> Jmt ;
  AxiomKindJmt : [Hypo] -> Kind -> Jmt ;
  AxiomExpJmt  : [Hypo] -> Exp -> Kind -> Jmt ;

  RewriteJmt : [Rule] -> Jmt ;
  RewriteRule : [Ident] -> Exp -> Exp -> Rule ; ---- generalize to [] and x:A
  NoVarRewriteRule : Exp -> Exp -> Rule ;

  PropHypo : Prop -> Hypo ;
  VarsHypo : [Ident] -> Kind -> Hypo ;
  BareVarsHypo : [Ident] -> Hypo ;  -- needed in proofs: let x be arbitrary

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
  AllProp : [ArgKind] -> Prop -> Prop ;
  ExistProp : [ArgKind] -> Prop -> Prop ; 
  FormalProp : Formal -> Prop ;
  AppProp : Formal -> [Exp] -> Prop ;

  FormalKind : Formal -> Kind ;
  SuchThatKind : Ident -> Kind -> Prop -> Kind ;
  AppKind : Formal -> [Exp] -> Kind ;
  FunKind : [ArgKind] -> Kind -> Kind ;

  KindArgKind : Kind -> ArgKind ;
  IdentsArgKind : Kind -> [Ident] -> ArgKind ;

  StrIdent : String -> Ident ;
  StrFormal : String -> Formal ;

  AppProof : [Proof] -> Exp -> Proof ;
  AbsProof : [Hypo] -> Proof -> Proof ;

-- using Constants

  AdjProp : Adj -> Exp -> Prop ;
  NotAdjProp : Adj -> Exp -> Prop ;
  RelProp : Rel -> Exp -> Exp -> Prop ;
  NotRelProp : Rel -> Exp -> Exp -> Prop ;
  NounKind : Noun -> Kind ;
  NameExp : Name -> Exp ;
  FunExp : Fun -> Exp -> Exp ;
  Fun2Exp : Fun -> Exp -> Exp -> Exp ;
  LabelExp : Label -> Exp ;

}