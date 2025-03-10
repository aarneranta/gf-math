concrete BaseConstantsSwe of BaseConstants =

open
  UtilitiesSwe,
  SyntaxSwe,
  ParadigmsSwe,
  SymbolicSwe,
  (L=BaseConstantsLatex),
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
  natural_Set = mkSet L.natural_Set "naturlig" tal_N ;
  integer_Set = mkSet L.integer_Set (mkN "hel" tal_N) ;
  rational_Set = mkSet L.rational_Set "rationell" tal_N ;
  real_Set = mkSet L.real_Set "reell" tal_N ;
  complex_Set = mkSet L.complex_Set "komplex" tal_N ;

  Eq_Compar = mkCompar L.Eq_Compar "lika" "med" ;
  Lt_Compar = mkCompar L.Lt_Compar "mindre" "än" ; 
  Gt_Compar = mkCompar L.Gt_Compar "större" "än" ; 
  Neq_Compar = mkCompar L.Neq_Compar "inte lika" "med" ; ---- 
  Leq_Compar = mkCompar L.Leq_Compar "mindre än eller lika" "med" ; 
  Geq_Compar =  mkCompar L.Geq_Compar "större än eller lika" "med" ;

  positive_Adj = mkAdj "positiv" ;
  negative_Adj = mkAdj "negativ" ;

  plus_Oper = mkOper L.plus_Oper "summa" ;
  minus_Oper = mkOper L.minus_Oper (mkN "skillnad" "skillnader") (mkPrep "mellan") ;
  times_Oper = mkOper L.times_Oper "produkt" ;
  div_Oper = mkOper L.div_Oper "kvot" ;
  pow_Oper = mkOper L.pow_Oper "potens" ; ----
  neg_Oper = mkOper L.neg_Oper "negation" ;

  successor_Fun = mkFun (mkN "efterföljare" neutrum) ;
  absolute_value_Fun = mkFun (mkN "absolutbelopp" neutrum) ;
  factorial_Fun = mkFun "fakultet" ;
  gcd_Fun = mkFun "störst" "gemensam" "delare" ;

  even_Adj = mkAdj "jämn" ;
  odd_Adj = mkAdj "udda" ;
  divisible_Rel = mkRel "delbar" "med" ;
  prime_Adj = mkAdj "prim" ;

  function_Oper = mkOper L.function_Oper (mkN "funktion" "funktioner") ;
  union_Oper = mkOper L.union_Oper "union" ;
  intersection_Oper = mkOper L.intersection_Oper (mkN "snitt" "snittet") ;
  difference_Oper = mkOper L.difference_Oper (mkN "differens") (mkPrep "mellan") ;
  powerset_Oper = mkOper L.powerset_Oper "potensmängd" ;

}