concrete BaseConstantsFre of BaseConstants =

open
  SyntaxFre,
  ParadigmsFre,
  (P=ParadigmsFre),
  SymbolicFre,
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
  type_Noun = mkNoun type_N ;
  set_Noun = mkNoun (mkN "ensemble" masculine) ;
  proposition_Noun = mkNoun "proposition" ;

  elements_Fun = mkFun type_N "des éléments" ;
  proofs_Fun = mkFun  type_N "des preuves" ;

  absurdity_Name = mkName "contradiction" ;
  conjunction_Fun = mkFun "conjonction" ;
  disjunction_Fun = mkFun "disjonction" ;
  implication_Fun = mkFun "implication" ;
  universal_Fun = mkFun "universel" "quantification" ;
  existential_Fun = mkFun "existentiel" "quantification" ;
  negation_Fun = mkFun "négation" ;
  equivalence_Fun = mkFun "équivalence" ;

  number_Noun = mkNoun nombre_N ;
  natural_Set = mkSet "naturel" nombre_N "N" ;
  integer_Set = mkSet "entier" "Z";
  rational_Set = mkSet "rationnel" nombre_N "Q";
  real_Set = mkSet "réel" nombre_N "R";
  complex_Set = mkSet "complexe" nombre_N "C";

  Eq_Compar = mkCompar (mkAP (mkA "égal")) dative "=" ;
  Lt_Compar = mkCompar (mkAP (mkA "inférieur")) dative "<" ;
  Gt_Compar = mkCompar (mkAP (mkA "supërieur")) dative ">" ;
  Neq_Compar = mkCompar (mkAP (mkA "inégal")) dative "\\neq" ; ---- ?
  Leq_Compar = mkCompar (mkAP or_Conj (mkAP (mkA "inférieur")) (mkAP (mkA "ágal"))) dative "\\leq" ;
  Geq_Compar =  mkCompar (mkAP or_Conj (mkAP (mkA "supérieur")) (mkAP (mkA "ágal"))) dative "\\leq" ;

  positive_Adj = mkAdj "positif" ;
  negative_Adj = mkAdj "negatif" ;

  plus_Oper = mkOper "somme" "+" <1 : Prec> ;
  minus_Oper = mkOper "différence" "-" <1 : Prec> ; 
  times_Oper = mkOper "produit" "\\times" <2 : Prec> ;
  div_Oper = mkOper "quotient" "\\div" <2 : Prec> ; ---
  pow_Oper = mkOper "puissance" "^" <2 : Prec> ; ---
  neg_Oper = mkOper "négation" "\\negated" ;

  absolute_value_Fun = mkFun (mkCN (mkA "absolu") (mkN "valeur" feminine)) ;
  factorial_Fun = mkFun "factorielle" ;
  gcd_Fun = mkFun "plus grand" "commun" "diviseur" ; ---- should be in this order

  even_Adj = mkAdj "pair" ;
  odd_Adj = mkAdj "impair" ;
  divisible_Rel = mkRel "divisible" "par" ;
  prime_Adj = mkAdj "premier" ;

  function_Oper = mkOper "fonction" "\\rightarrow" ; ---
  union_Oper = mkOper "union" "\\cup" ;
  intersection_Oper = mkOper "intersection" "\\cap" ;
  difference_Oper = mkOper (mkN "différence") (mkPrep "entre") "\\setminus" ;
  powerset_Oper = mkOper "puissance" "\\wp" ; ----

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
    mkFun : CN -> FunctionT
      = \n -> {cn = n ; prep = possess_Prep} ;
    mkFun : N -> Prep -> FunctionT
      = \n, p -> {cn = mkCN n ; prep = p} ;
    mkFun : N -> Str -> FunctionT
      = \n, s -> {cn = mkCN (mkCN n) (P.mkAdv s) ; prep = possess_Prep} ;
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
      = \s, p -> {ap = mkAP (mkA s) ; prep = mkPrep p} ;
    mkRel : AP -> Prep -> {ap : AP ; prep : Prep}
      = \ap, prep -> {ap = ap ; prep = prep}
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
    mkCompar : AP -> Prep -> Str -> ComparisonT
      = \ap, prep, op -> {rel = mkRel ap prep ; op = op} ;
    } ;

  latexName : Str -> NP
      = \s -> symb (mkSymb ("$" ++ s ++ "$")) ;

  nombre_N : N = mkN "nombre" masculine ;
  type_N = mkN "type" masculine ;


}