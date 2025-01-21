concrete NotationsEng of Notations = ConstantBaseEng ** 

open
  SyntaxEng,
  ParadigmsEng,
  SymbolicEng,
  Formal

in {

lin type_Noun = mkNoun "type";
lin nat_Set = mkSet "natural" "number" "N" ;
lin int_Set = mkSet "integer" "Z";
lin set_Noun = mkNoun "set" ;
lin even_Adj = mkAdj "even" ;
lin odd_Adj = mkAdj "odd" ;
lin prime_Adj = mkAdj "prime" ;
lin divisible_Rel = mkRel "divisible" "by" ;
lin eq_Compar = mkCompar "equal" "to" "=" ;
lin lt_Compar = mkCompar "less" "than" "<" ;
lin gt_Compar = mkCompar "greater" "than" ">" ;
lin sum_Oper = mkOper "sum" "+" ;
lin product_Oper = mkOper "product" "\\times" <2 : Prec> ;
lin gcd_Fun = mkFun "greatest" "common" "divisor" ;

}
