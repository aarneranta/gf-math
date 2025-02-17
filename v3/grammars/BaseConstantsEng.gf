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

lin
  type_Noun = mkNoun "type" ;
  set_Noun = mkNoun "set" ;
  proposition_Noun = mkNoun "proposition" ;

  elements_Fun = mkFun "type of elements" ;
  proofs_Fun = mkFun  "type of proofs" ;

  absurdity_Name = mkName "absurdity" ;
  conjunction_Fun = mkFun "conjunction" ;
  disjunction_Fun = mkFun "disjunction" ;
  implication_Fun = mkFun "implication" ;
  universal_Fun = mkFun "universal" "quanfication" ;
  existential_Fun = mkFun "existential" "quanfication" ;
  negation_Fun = mkFun "negation" ;
  equivalence_Fun = mkFun "equivalence" ;

  number_Noun = mkNoun "Number" ;
  natural_Set = mkSet "natural" "number" "N" ;
  integer_Set = mkSet "integer" "Z";
  rational_Set = mkSet "rational" "number" "Q";
  real_Set = mkSet "real" "number" "R";
  complex_Set = mkSet "complex" "number" "C";

  Eq_Compar = mkCompar "equal" "to" "=" ;
  Lt_Compar = mkCompar "less" "than" "<" ;
  Gt_Compar = mkCompar "greater" "than" ">" ;
  Neq_Compar = mkCompar "not equal" "to" "\\neq" ;
  Leq_Compar = mkCompar "less than or equal" "to" "\\leq" ;
  Geq_Compar = mkCompar "greater than or equal" "to" "\\geq" ;

  positive_Adj = mkAdj "positive" ;
  negative_Adj = mkAdj "negative" ;

  plus_Oper = mkOper "sum" "+" <1 : Prec> ;
  minus_Oper = mkOper "difference" "-" <1 : Prec> ; 
  times_Oper = mkOper "product" "\\times" <2 : Prec> ;
  div_Oper = mkOper "quotient" "\\div" <2 : Prec> ; ---
  pow_Oper = mkOper "exponentiation" "^" <2 : Prec> ; ---
  neg_Oper = mkOper "negation" "\\negated" ;

  factorial_Fun = mkFun "factorial" ;
  gcd_Fun = mkFun "greatest" "common" "divisor" ;

  even_Adj = mkAdj "even" ;
  odd_Adj = mkAdj "odd" ;
  divisible_Rel = mkRel "divisible" "by" ;
  prime_Adj = mkAdj "prime" ;

  function_Oper = mkOper "function" "\\rightarrow" ; ---
  union_Oper = mkOper "union" "\\cup" ;
  intersection_Oper = mkOper "intersection" "\\cap" ;
  difference_Oper = mkOper "difference" "\\setminus" ;
  powerset_Oper = mkOper "power set" "\\wp" ;

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