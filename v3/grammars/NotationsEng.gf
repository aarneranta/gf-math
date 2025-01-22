concrete NotationsEng of Notations = ConstantBaseEng ** 

open
  SyntaxEng,
  ParadigmsEng,
  SymbolicEng,
  Formal

in {

lin type_Noun = mkNoun "type";
lin natural_Set = mkSet "natural" "number" "N" ;
lin integer_Set = mkSet "integer" "Z";
lin rational_Set = mkSet "rational" "number" "Q";
lin real_Set = mkSet "real" "number" "R";
lin complex_Set = mkSet "complex" "number" "C";
lin set_Noun = mkNoun "set" ;
lin even_Adj = mkAdj "even" ;
lin odd_Adj = mkAdj "odd" ;
lin prime_Adj = mkAdj "prime" ;
lin divisible_Rel = mkRel "divisible" "by" ;
lin eq_Compar = mkCompar "equal" "to" "=" ;
lin neq_Compar = mkCompar "not equal" "to" "\\neq" ;
lin lt_Compar = mkCompar "less" "than" "<" ;
lin leq_Compar = mkCompar "less than or equal" "to" "\\leq" ;
lin gt_Compar = mkCompar "greater" "than" ">" ;
lin geq_Compar = mkCompar "greater than or equal" "to" "\\geq" ;
lin sim_Compar = mkCompar "similar" "to" "\\sim" ;
lin sum_Oper = mkOper "sum" "+" <1 : Prec> ;
lin subtraction_Oper = mkOper "subtraction" "-" <1 : Prec> ;
lin product_Oper = mkOper "product" "\\times" <2 : Prec> ;
lin division_Oper = mkOper "division" "\\div" <2 : Prec> ;
lin gcd_Fun = mkFun "greatest" "common" "divisor" ;

}
