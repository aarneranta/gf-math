concrete PropLean of Prop = open Prelude, Formal in {

lincat
  Prop = TermPrec ;
  Atom = Bool => Str ; -- positive and negated forms
  Pred1 = Bool => Str ;
  Pred2 = Bool => Str ;
  Var,
  Conj
    = Str ;
  Ind = TermPrec ;
  Fun1 = TermPrec ;
  Fun2 = TermPrec ;

lin
  PAtom a = constant (a ! True) ;
  PNeg = prefix 3 "¬" ;
  PConj = infixl 1 ;
  PImpl = infixr 0 "→" ;
  PUniv v = prefix 3 (parenth ("∀" ++ v) ++ ",") ;
  PExist v = prefix 3 (parenth ("∃" ++ v) ++ ",") ;

  APred1 p x = \\b => appLatex (p ! b) (top x) ;
  APred2 p x y = \\b => appLatex (p ! b) (top x) (top y) ;

  IVar x = constant x ;
  IFun1 f x = constant (appLatex (top f) (top x)) ;
  IFun2 f x y = constant (appLatex (top f) (top x) (top y)) ;

  VString s = s.s ;

  CAnd = "∧" ;
  COr = "∨" ;

-- supplementary

lincat
  Kind,
  [Prop],
  [Pred1], 
  [Ind], 
  [Var]
    = Str ;

lin
  AKind k x = table {True => top x ++ "∈" ++ k ; False => top x ++ "∉" ++ k} ;

  PConjs c ps = constant (c ++ "[" ++ ps ++ "]") ;
  PUnivs vs k = prefix 4 (parenth ("∀" ++ vs ++ ":" ++ k)) ;
  PExists vs k = prefix 4 (parenth ("∃" ++ vs ++ ":" ++ k)) ;

  PNegAtom a = constant (a ! False) ;

  BaseProp a b = top a ++ "," ++ top b ;
  ConsProp a as = top a ++ "," ++ as ;

  BaseVar a = a ;
  ConsVar a as = a ++ as ;

  BaseInd a b = top a ++ "," ++ top b ;
  ConsInd a as = top a ++ "," ++ as ;

  BasePred1 a b = (a ! True) ++ "," ++ (b ! True) ;
  ConsPred1 a as = (a ! True) ++ "," ++ as ;

lin
  ConjPred1 c ps = \\_ => c ++ bracket ps ; ---- neg

  APredColl f ps = \\b => appLatex (f ! b) ps ;
  APredRefl f x  = \\b => appLatex (f ! b) (top x) (top x) ;

  IFunC f ps = constant (appLatex (top f) (bracket ps)) ;

  IUniv k = constant (parenth ("∀" ++ k)) ;
  IExist k = constant (parenth ("∃" ++ k)) ;

  ConjInd c ps = constant (c ++ bracket ps) ;

--  ModKind k m = "{x :" ++ k ++ "//" ++ m ++ "x}" ;

  PartPred f y = \\b => appLatex (f ! b) (top y) ;

-- test lexicon

lin
  Vertical = slash "vertical" ;
  Horizontal = slash "horizontal" ;
  Parallel = slash "parallel" ;
  Equal = slash "equal" "notequal" ;
  Line = slash "line" ;
  Point = slash "point" ;
  Centre = slash  "centre" ;
  Intersection = slash  "intersection" ;

  Set k = appLatex ("set") k ;

  Even = slash "even" ;
  Odd = slash "odd" ;
  Square = slash "square" ;
  Sum = slash "sum" ;
  Product = slash "product" ;
  Nat = "N" ;

  IInt i = constant i.s ;

oper
  appLatex = overload {
    appLatex : Str -> Str -> Str = \f,x -> curly (f ++ x) ;
    appLatex : Str -> Str -> Str -> Str = \f,x,y -> curly (f ++ x ++ y) ;
    } ;
  slash = overload {
    slash : Str -> Str = \f -> f ;
    slash : Str -> TermPrec = \f -> constant (f) ;
    slash : Str -> Bool => Str = \f -> \\_ => f ;
    slash : Str -> Str -> Bool => Str = \f,g -> table {True => f ; False => g} ; 
    } ;

  curly   : Str -> Str = \s -> "(" ++ s  ++")" ;
  bracket : Str -> Str = \s -> "[" ++ s ++ "]" ;

}
