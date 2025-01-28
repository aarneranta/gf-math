abstract BaseConstants = {

cat
  Noun ; -- Kind
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
  
  natural_Set : Set ;
  integer_Set : Set ;
  rational_Set : Set ;
  real_Set : Set ;
  complex_Set : Set ;

{-
  n0_Const : Const ;
  n1_Const : Const ;
  n2_Const : Const ;
  n3_Const : Const ;
  n4_Const : Const ;
  n5_Const : Const ;
  n6_Const : Const ;
  n7_Const : Const ;
  n8_Const : Const ;
  n9_Const : Const ;
-}

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