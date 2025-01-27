concrete BaseConstantsEng of BaseConstants =

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

lin type_Noun = mkNoun "type";
lin natural_Set = mkSet "natural" "number" "N" ;
lin integer_Set = mkSet "integer" "Z";
lin rational_Set = mkSet "rational" "number" "Q";
lin real_Set = mkSet "real" "number" "R";
lin complex_Set = mkSet "complex" "number" "C";
lin set_Noun = mkNoun "set" ;
lin even_Adj = mkAdj "even" ;
lin odd_Adj = mkAdj "odd" ;
lin prime_Adj = mkAdj "prime" ;
lin divisible_Rel = mkRel "divisible" "by" ;
lin eq_Compar = mkCompar "equal" "to" "=" ;
lin neq_Compar = mkCompar "not equal" "to" "\\neq" ;
lin lt_Compar = mkCompar "less" "than" "<" ;
lin leq_Compar = mkCompar "less than or equal" "to" "\\leq" ;
lin gt_Compar = mkCompar "greater" "than" ">" ;
lin geq_Compar = mkCompar "greater than or equal" "to" "\\geq" ;
lin sim_Compar = mkCompar "similar" "to" "\\sim" ;
lin sum_Oper = mkOper "sum" "+" <1 : Prec> ;
lin subtraction_Oper = mkOper "subtraction" "-" <1 : Prec> ;
lin product_Oper = mkOper "product" "\\times" <2 : Prec> ;
lin division_Oper = mkOper "division" "\\div" <2 : Prec> ;
lin gcd_Fun = mkFun "greatest" "common" "divisor" ;

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