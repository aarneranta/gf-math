concrete ConstantsEng of Constants = ConstantBaseEng **

open
  SyntaxEng,
  ParadigmsEng,
  SymbolicEng

in {

-- replace by a dynamically generated file

lin
  DkNat = mkNoun "natural" "number" ;
  DkSet = mkNoun "set" ;
  DkEven = mkAdj "even" ;
  DkOdd = mkAdj "odd" ;
  DkPrime = mkAdj "prime" ;
  DkZero = latexName "0" ;
  DkDiv = mkRel "divisible" "by" ;


}