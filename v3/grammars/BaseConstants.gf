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

fun type_Noun : Noun ;
fun natural_Set : Set ;
fun integer_Set : Set ;
fun rational_Set : Set ;
fun real_Set : Set ;
fun complex_Set : Set ;
fun set_Noun : Noun ;
fun even_Adj : Adj ;
fun odd_Adj : Adj ;
fun prime_Adj : Adj ;
fun divisible_Rel : Rel ;
fun eq_Compar : Compar ;
fun neq_Compar : Compar ;
fun leq_Compar : Compar ;
fun geq_Compar : Compar ;
fun lt_Compar : Compar ;
fun gt_Compar : Compar ;
fun sim_Compar : Compar ;
fun sum_Oper : Oper ;
fun subtraction_Oper : Oper ;
fun product_Oper : Oper ;
fun division_Oper : Oper ;
fun gcd_Fun : Fun ;

}