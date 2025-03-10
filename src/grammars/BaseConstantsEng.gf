concrete BaseConstantsEng of BaseConstants =

open
  UtilitiesEng,
  SyntaxEng,
  ParadigmsEng,
  SymbolicEng,
  (L=BaseConstantsLatex),
  Formal,
  Prelude

in {

lincat
  Noun = CN ;
  Fam = CN ;
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
  boolean_Noun = mkNoun "boolean" ;
  list_Fam = mkNoun "list" ;

  natural_Set = mkSet L.natural_Set "natural" "number" ;
  integer_Set = mkSet L.integer_Set "integer" ;
  rational_Set = mkSet L.rational_Set "rational" "number" ;
  real_Set = mkSet L.real_Set "real" "number" ;
  complex_Set = mkSet  L.complex_Set "complex" "number" ;

  Eq_Compar = mkCompar L.Eq_Compar "equal" "to" ;
  Lt_Compar = mkCompar L.Lt_Compar "less" "than" ;
  Gt_Compar = mkCompar L.Gt_Compar "greater" "than" ;
  Neq_Compar = mkCompar L.Neq_Compar "not equal" "to" ;
  Leq_Compar = mkCompar L.Leq_Compar "less than or equal" "to" ;
  Geq_Compar = mkCompar L.Geq_Compar "greater than or equal" "to" ;

  positive_Adj = mkAdj "positive" ;
  negative_Adj = mkAdj "negative" ;

  plus_Oper = mkOper L.plus_Oper "sum" ;
  minus_Oper = mkOper L.minus_Oper "difference" ;
  times_Oper = mkOper L.times_Oper "product" ;
  div_Oper = mkOper L.div_Oper "quotient" ;
  pow_Oper = mkOper L.pow_Oper "exponentiation" ;
  neg_Oper = mkOper L.neg_Oper "negation" ;
  logarithm_Oper = mkOper L.logarithm_Oper "logarithm" ; ----
  square_root_Oper = mkOper L.square_root_Oper "square root" ;

  successor_Fun = mkFun "successor" ;
  absolute_value_Fun = mkFun "absolute" "value" ;
  factorial_Fun = mkFun "factorial" ;
  gcd_Fun = mkFun "greatest" "common" "divisor" ;

  even_Adj = mkAdj "even" ;
  odd_Adj = mkAdj "odd" ;
  divisible_Rel = mkRel "divisible" "by" ;
  prime_Adj = mkAdj "prime" ;

  function_Oper = mkOper L.function_Oper "function" ;
  union_Oper = mkOper L.union_Oper "union" ;
  intersection_Oper = mkOper L.intersection_Oper "intersection" ;
  difference_Oper = mkOper L.difference_Oper "difference" ;
  powerset_Oper = mkOper L.powerset_Oper "power set" ;

}