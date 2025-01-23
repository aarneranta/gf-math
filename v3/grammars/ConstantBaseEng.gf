concrete ConstantBaseEng of ConstantBase =

open
  SyntaxEng,
  ParadigmsEng,
  SymbolicEng,
  Formal,
  Prelude

in {

lincat
  Noun = CN ;
  Set = SetT ;
  Adj = AP ;
  Rel = RelationT ;
  Name = NP ;
  Fun = FunctionT ;
  Label = LabelT ;
  Const = ConstantT ;
  Oper = OperatorT ;
  Compar = ComparisonT ; 

oper
  RelationT : Type = {ap : AP ; prep : Prep} ;
  FunctionT : Type = {cn : CN ; prep : Prep} ;
  ConstantT : Type = {np : NP ; c : Str} ;
  OperatorT : Type = {f : FunctionT ; op : Str ; p : Prec} ; -- infixl p c
  ComparisonT : Type = {rel : RelationT ; op :  Str} ;
  SetT : Type = {cn : CN ; c : Str} ;
  LabelT = {np : NP ; isEmpty : Bool} ;

  mkNoun = overload {
    mkNoun : Str -> CN
      = \s -> mkCN (mkN s) ;
    mkNoun : Str -> Str -> CN
      = \a, n -> mkCN (mkA a) (mkN n) ;
    } ;
    
  mkSet = overload {
    mkSet : Str -> Str -> SetT
      = \s, c -> {cn = mkCN (mkN s) ; c = c} ;
    mkSet : Str -> Str -> Str -> SetT
      = \a, n, c -> {cn = mkCN (mkA a) (mkN n) ; c = c} ;
    } ;
    
  mkFun = overload {
    mkFun : Str -> FunctionT
      = \s -> {cn = mkCN (mkN s) ; prep = possess_Prep} ;
    mkFun : (a, n : Str) -> FunctionT
      = \a, n -> {cn = mkCN (mkA a) (mkN n) ; prep = possess_Prep} ;
    mkFun2 : (a, b, n : Str) -> FunctionT
      = \a, b, n -> {cn = mkCN (mkA a) (mkCN (mkA b) (mkN n)) ; prep = possess_Prep} ;
    } ;
    
  mkAdj = overload {
    mkAdj : Str -> AP
      = \s -> mkAP (mkA s) ;
    } ;
    
  mkRel = overload {
    mkRel : Str -> Str -> {ap : AP ; prep : Prep}
      = \s, p -> {ap = mkAP (mkA s) ; prep = mkPrep p}
    } ;
    
  mkName = overload {
    mkName : Str -> NP
      = \s -> mkNP (mkPN s)
    } ;

  mkLabel = overload {
    mkLabel : Str -> LabelT
      = \s -> {np = mkNP (mkPN s) ; isEmpty = False}
    } ;

  mkConst = overload {
    mkConst : Str -> Str -> ConstantT
      = \w, c -> {np = mkName w ; c = c}
    } ;
    
  mkOper = overload {
    mkOper : Str -> Str -> OperatorT
      = \w, c -> {f = mkFun w ; op = c ; p = 0} ; -- lowest Prec
    mkOper : Str -> Str -> Prec -> OperatorT
      = \w, c, p -> {f = mkFun w ; op = c ; p = p}
    } ;

  mkCompar = overload {
    mkCompar : Str -> Str -> Str -> ComparisonT
      = \s, p, op -> {rel = mkRel s p ; op = op} ;
    } ;

  latexName : Str -> NP
      = \s -> symb (mkSymb ("$" ++ s ++ "$")) ;


}