abstract MathCore =
  Notations,  -- basic notations, always available
  Constants   -- Dedukti-specific notations, user-defined with MkConstants.hs
  ** {

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
  Proof ;
  [Proof] {0} ;
  Rule ;
  [Rule] {1} ;

fun
  ThmJmt : Label -> [Hypo] -> Prop -> Proof -> Jmt ;
  AxiomJmt : Label -> [Hypo] -> Prop -> Jmt ;
  
  DefPropJmt : Label -> [Hypo] -> Prop -> Prop -> Jmt ;
  DefKindJmt : Label -> [Hypo] -> Kind -> Kind -> Jmt ;
  DefExpJmt  : Label -> [Hypo] -> Exp -> Kind -> Exp -> Jmt ;
  
  AxiomPropJmt : Label -> [Hypo] -> Prop -> Jmt ;
  AxiomKindJmt : Label -> [Hypo] -> Kind -> Jmt ;
  AxiomExpJmt  : Label -> [Hypo] -> Exp -> Kind -> Jmt ;

  RewriteJmt : [Rule] -> Jmt ;
  RewriteRule : [Ident] -> Exp -> Exp -> Rule ; ---- generalize to [] and x:A
  NoVarRewriteRule : Exp -> Exp -> Rule ;

  PropHypo : Prop -> Hypo ;
  VarsHypo : [Ident] -> Kind -> Hypo ;
  BareVarsHypo : [Ident] -> Hypo ;  -- needed in proofs: let x be arbitrary

  AppExp : Exp -> [Exp] -> Exp ;
  AbsExp : [Ident] -> Exp -> Exp ;
  IdentExp : Ident -> Exp ;
  TypedExp : Exp -> Kind -> Exp ;

  AndProp : [Prop] -> Prop ;
  OrProp : [Prop] -> Prop ;
  IfProp : Prop -> Prop -> Prop ;
  IffProp : Prop -> Prop -> Prop ;
  NotProp : Prop -> Prop ;
  FalseProp : Prop ;
  AllProp : [ArgKind] -> Prop -> Prop ;
  ExistProp : [ArgKind] -> Prop -> Prop ; 
  IdentProp : Ident -> Prop ;
  AppProp : Ident -> [Exp] -> Prop ;

  IdentKind : Ident -> Kind ;
  SuchThatKind : Ident -> Kind -> Prop -> Kind ;
  AppKind : Ident -> [Exp] -> Kind ;
  FunKind : [ArgKind] -> Kind -> Kind ;

  KindArgKind : Kind -> ArgKind ;
  IdentsArgKind : Kind -> [Ident] -> ArgKind ;

  StrIdent : String -> Ident ;
  StrLabel : String -> Label ; -- to deal with Dedukti labels not in grammar
  noLabel : Label ; -- to deal with unlabelled statements
  axiomLabel : Label ;
  theoremLabel : Label ;
  definitionLabel : Label ;

  AppProof : [Proof] -> Exp -> Proof ;
  AbsProof : [Hypo] -> Proof -> Proof ;

-- using Constants

  AdjProp : Adj -> Exp -> Prop ;
  NotAdjProp : Adj -> Exp -> Prop ;
  RelProp : Rel -> Exp -> Exp -> Prop ;
  NotRelProp : Rel -> Exp -> Exp -> Prop ;
  NounKind : Noun -> Kind ;
  SetKind : Set -> Kind ;
  NameExp : Name -> Exp ;
  FunListExp : Fun -> [Exp] -> Exp ;
  LabelExp : Label -> Exp ;
  ConstExp : Const -> Exp ;
  OperListExp : Oper -> [Exp] -> Exp ; -- binary operation applied collectively
  ComparProp : Compar -> Exp -> Exp -> Prop ;
  NotComparProp : Compar -> Exp -> Exp -> Prop ;
}