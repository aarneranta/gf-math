concrete BaseConstantsSwe of BaseConstants =

open
  SyntaxSwe,
  ParadigmsSwe,
  SymbolicSwe,
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
  type_Noun = mkNoun (mkN "typ" "typer") ;
  set_Noun = mkNoun (mkN "mängd" "mängder") ;
  proposition_Noun = mkNoun "påstående" ;

  elements_Fun = mkFun "elementtyp" ;
  proofs_Fun = mkFun  "bevistyp" ;

  absurdity_Name = mkName (mkN "kontradiktion" "kontradiktioner") ;
  conjunction_Fun = mkFun (mkN "konjunktion" "konjunktioner") ;
  disjunction_Fun = mkFun (mkN "disjunktion" "disjunktioner") ;
  implication_Fun = mkFun (mkN "implikation" "inmplikationer") ;
  universal_Fun = mkFun "universal" "kvantifikation" ; ---- plural
  existential_Fun = mkFun "existentiell" "kvantifikation" ;
  negation_Fun = mkFun (mkN "negation" "negationer") ;
  equivalence_Fun = mkFun (mkN "ekvivalens" "ekvivalenser") ;

  number_Noun = mkNoun tal_N ;
  natural_Set = mkSet "naturlig" (tal_N) "N" ;
  integer_Set = mkSet (mkN "hel" tal_N) "Z";
  rational_Set = mkSet "rationell" tal_N "Q";
  real_Set = mkSet "reell" tal_N "R";
  complex_Set = mkSet "komplex" tal_N "C";

  Eq_Compar = mkCompar "lika" "med" "=" ;
  Lt_Compar = mkCompar "mindre" "än" "<" ;
  Gt_Compar = mkCompar "större" "än" ">" ;
  Neq_Compar = mkCompar "inte lika" "med" "\\neq" ;
  Leq_Compar = mkCompar "mindre än eller lika" "med" "\\leq" ;
  Geq_Compar = mkCompar "större än eller lika" "med" "\\geq" ;

  positive_Adj = mkAdj "positiv" ;
  negative_Adj = mkAdj "negativ" ;

  plus_Oper = mkOper "summa" "+" <1 : Prec> ;
  minus_Oper = mkOper (mkN "skillnad" "skillnader") (mkPrep "mellan") "-" <1 : Prec> ; 
  times_Oper = mkOper "produkt" "\\times" <2 : Prec> ;
  div_Oper = mkOper "kvot" "\\div" <2 : Prec> ; ---
  pow_Oper = mkOper "potens" "^" <2 : Prec> ; ---
  neg_Oper = mkOper "negation" "\\negated" ; --- to be avoided in parsing

  factorial_Fun = mkFun "fakultet" ;
  gcd_Fun = mkFun "störst" "gemensam" "delare" ;

  even_Adj = mkAdj "jämn" ;
  odd_Adj = mkAdj "udda" ;
  divisible_Rel = mkRel "delbar" "med" ;
  prime_Adj = mkAdj "prim" ;

  function_Oper = mkOper (mkN "funktion" "funktioner") "\\rightarrow" ; ---
  union_Oper = mkOper "union" "\\cup" ;
  intersection_Oper = mkOper (mkN "snitt" "snittet") "\\cap" ;
  difference_Oper = mkOper "differens" "\\setminus" ;
  powerset_Oper = mkOper "potensmängd" "\\wp" ;

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
    mkNoun : N -> CN
      = \n -> mkCN n ;
    mkNoun : Str -> Str -> CN
      = \a, n -> mkCN (mkA a) (mkN n) ;
    } ;
    
  mkSet = overload {
    mkSet : Str -> Str -> SetT
      = \s, c -> {cn = mkCN (mkN s) ; c = c} ;
    mkSet : N -> Str -> SetT
      = \n, c -> {cn = mkCN n ; c = c} ;
    mkSet : Str -> Str -> Str -> SetT
      = \a, n, c -> {cn = mkCN (mkA a) (mkN n) ; c = c} ;
    mkSet : Str -> N -> Str -> SetT
      = \a, n, c -> {cn = mkCN (mkA a) n ; c = c} ;
    } ;
    
  mkFun = overload {
    mkFun : Str -> FunctionT
      = \s -> {cn = mkCN (mkN s) ; prep = possess_Prep} ;
    mkFun : N -> FunctionT
      = \n -> {cn = mkCN n ; prep = possess_Prep} ;
    mkFun : N -> Prep -> FunctionT
      = \n, p -> {cn = mkCN n ; prep = p} ;
    mkFun : (a, n : Str) -> FunctionT
      = \a, n -> {cn = mkCN (mkA a) (mkN n) ; prep = possess_Prep} ;
    mkFun : (a, b, n : Str) -> FunctionT
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
      = \s -> mkNP (mkPN s) ;
    mkName : N -> NP
      = \n -> mkNP n
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
    mkOper : N -> Str -> OperatorT
      = \w, c -> {f = mkFun w ; op = c ; p = 0} ; -- lowest Prec
    mkOper : N -> Prep -> Str -> OperatorT
      = \w, prep, c -> {f = mkFun w prep ; op = c ; p = 0} ; -- lowest Prec
    mkOper : N -> Prep -> Str -> Prec -> OperatorT
      = \w, prep, c, p -> {f = mkFun w prep ; op = c ; p = p} ; -- lowest Prec
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

  tal_N : N = mkN "tal" "tal" ;


}