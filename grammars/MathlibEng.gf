concrete MathlibEng of Mathlib = CategoriesEng **

open
  UtilitiesEng, IrregEng, SyntaxEng, ParadigmsEng

in {

oper
  -- "of" and a plural, as in MytextWordsEng: the set of divisors
  setOfPl : CN -> Adv = \cn -> SyntaxEng.mkAdv possess_Prep (mkNP aPl_Det cn) ;

lin smallest_Adj = mkAdj "smallest" ;
lin descending_Adj = mkAdj "descending" ;
lin finitely_supported_Adj = mkAdj "finitely supported" ;
lin bounded_above_Adj = mkAdj "bounded above" ;
lin bounded_below_Adj = mkAdj "bounded below" ;
lin ceiling_Noun = mkNoun "ceiling" ;
lin partially_ordered_Adj = mkAdj "partially ordered" ;
lin linearly_ordered_Adj = mkAdj "linearly ordered" ;
lin trichotomous_Adj = mkAdj "trichotomous" ;
lin lower_Adj = mkAdj "lower" ;

lin set_of_divisors_Noun = mkCN (mkCN (mkN "set")) (setOfPl (mkCN (mkN "divisor"))) ;
lin set_of_proper_divisors_Noun = mkCN (mkCN (mkN "set")) (setOfPl (mkCN (mkA "proper") (mkN "divisor"))) ;
lin set_of_prime_factors_Noun = mkCN (mkCN (mkN "set")) (setOfPl (mkCN (mkA "prime") (mkN "factor"))) ;

}
