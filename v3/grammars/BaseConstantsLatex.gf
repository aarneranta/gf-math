resource BaseConstantsLatex = 

open
  Formal,
  Prelude

in {

oper
  natural_Set = "N" ;
  integer_Set = "Z";
  rational_Set = "Q";
  real_Set = "R";
  complex_Set = "C";

  Eq_Compar = "=" ;
  Lt_Compar = "<" ;
  Gt_Compar = ">" ;
  Neq_Compar = "\\neq" ;
  Leq_Compar = "\\leq" ;
  Geq_Compar = "\\geq" ;
  
  plus_Oper : OperT = mkOper "+" <1 : Prec> ;
  minus_Oper : OperT = mkOper "-" <1 : Prec> ; 
  times_Oper : OperT = mkOper "\\times" <2 : Prec> ;
  div_Oper : OperT = mkOper "\\div" <2 : Prec> ; ---
  pow_Oper : OperT = mkOper "^" <2 : Prec> ; ---
  neg_Oper : OperT = mkOper "\\negated" ;

  function_Oper : OperT = mkOper "\\rightarrow" ; ---
  union_Oper : OperT = mkOper "\\cup" ;
  intersection_Oper : OperT = mkOper "\\cap" ;
  difference_Oper : OperT = mkOper "\\setminus" ;
  powerset_Oper : OperT = mkOper "\\wp" ;

oper
  OperT : Type = {op : Str ; p : Prec} ;
  
  mkOper = overload {
    mkOper : Str -> OperT
      = \c -> {op = c ; p = 0} ; -- lowest Prec
    mkOper : Str -> Prec -> OperT
      = \c, p -> {op = c ; p = p}
    } ;

}