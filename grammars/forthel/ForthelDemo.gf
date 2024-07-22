abstract ForthelDemo = Forthel ** {

-- demo lexicon for Forthel

fun

  set_PrimClass : PrimClass ; -- set (A, B, C)
  element_PrimClass : Term -> PrimClass ;
  function_PrimClass : Term -> Term -> PrimClass ;

  zero_DefiniteNoun : DefiniteNoun ;
  order_DefiniteNoun : PlainTerm -> DefiniteNoun ;

  converge_Verb : Verb ;
  divide_Verb : Term -> Verb ;   --- V2
  belong_Verb : Term -> Verb ;
  join_Verb : Term -> Term -> Verb ; --- V3

  prime_Adjective : Adjective ;
  dividing_Adjective : Term -> Adjective ; -- A2
  equal_Adjective : Term -> Adjective ;
  less_Adjective : Term -> Adjective ;
  greater_Adjective : Term -> Adjective ;

  thesis_Constant : Constant ;
  contrary_Constant : Constant ;
  contradiction_Constant : Constant ;

-- from GFLean

  rational_Adjective : Adjective ;
  odd_Adjective : Adjective ;
  integer_PrimClass : PrimClass ;
  number_PrimClass : PrimClass ;
  real_Adjective : Adjective ;
  even_Adjective : Adjective ;
  positive_Adjective : Adjective ;
  nonnegative_Adjective : Adjective ;
  negative_Adjective : Adjective ;
  less_or_equal_Adjective : Term -> Adjective ;
  greater_or_equal_Adjective : Term -> Adjective ;

} 