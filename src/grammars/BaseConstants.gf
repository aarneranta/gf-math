abstract BaseConstants = {

cat
  Noun ; -- Kind
  Fam ; -- Kind -> Kind
  Set ;  -- Kind + symbol
  Adj ;  -- Exp -> Prop
  Rel ;  -- Exp -> Exp -> Prop
  Name ; -- Exp
  Fun ;  -- [Exp] -> Exp
  Label ; -- Exp
  Const ; -- Exp + symbol
  Oper ;  -- Exp -> Exp -> Exp + symbol
  Compar ; -- Exp -> Exp -> Prop + symbol

fun
  type_Noun : Noun ;
  set_Noun : Noun ;
  proposition_Noun : Noun ;

  elements_Fun : Fun ;
  proofs_Fun : Fun ;

  absurdity_Name : Name ;
  conjunction_Fun : Fun ;
  disjunction_Fun : Fun ;
  implication_Fun : Fun ;
  universal_Fun : Fun ;
  existential_Fun : Fun ;
  negation_Fun : Fun ;
  equivalence_Fun : Fun ;

  number_Noun : Noun ;
  boolean_Noun : Noun ;
  list_Fam : Fam ;

  natural_Set : Set ;
  integer_Set : Set ;
  rational_Set : Set ;
  real_Set : Set ;
  complex_Set : Set ;

  Eq_Compar : Compar ;
  Lt_Compar : Compar ;
  Gt_Compar : Compar ;
  Neq_Compar : Compar ;
  Leq_Compar : Compar ;
  Geq_Compar : Compar ;

  positive_Adj : Adj ;
  negative_Adj : Adj ;

  plus_Oper : Oper ;
  minus_Oper : Oper ;
  times_Oper : Oper ;
  div_Oper : Oper ;
  pow_Oper : Oper ;
  neg_Oper : Oper ;
  logarithm_Oper : Oper ;
  square_root_Oper : Oper ;
  
  successor_Fun : Fun ;
  absolute_value_Fun : Fun ;
  factorial_Fun : Fun ;
  gcd_Fun : Fun ;

  even_Adj : Adj ;
  odd_Adj : Adj ;
  divisible_Rel : Rel ;
  prime_Adj : Adj ;

  function_Oper : Oper ;
  union_Oper : Oper ;
  intersection_Oper : Oper ;
  difference_Oper : Oper ;
  powerset_Oper : Oper ;

}