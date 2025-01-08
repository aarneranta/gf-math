concrete ConstantsEng of Constants = ConstantBaseEng **

open
  SyntaxEng,
  ParadigmsEng,
  SymbolicEng

in {

-- replace by a dynamically generated file

lin
  Dk_Type = mkNoun "type" ;
  Dk_Nat = mkNoun "natural" "number" ;
  Dk_Set = mkNoun "set" ;
  Dk_Even = mkAdj "even" ;
  Dk_Odd = mkAdj "odd" ;
  Dk_Prime = mkAdj "prime" ;
  Dk_Zero = latexName "0" ;
  Dk_Div = mkRel "divisible" "by" ;
  Dk_Eq = mkRel "equal" "to" ;
  Dk_Lt = mkRel "less" "than" ;
  Dk_Gt = mkRel "greater" "than" ;
  Dk_Succ = mkFun "successor" ;
  Dk_sum = mkFun "sum" ;
  Dk_prod = mkFun "product" ;
  Dk_gcd = mkFun "greatest" "common" "divisor" ;
  Dk_eqZero = mkLabel "equality of zero" ;
  Dk_eqSucc = mkLabel "equality of successors" ;

}