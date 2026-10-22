concrete MathlibCze of Mathlib = CategoriesCze **

open
  UtilitiesCze, SyntaxCze, ParadigmsCze

in {

oper
  -- the genitive plural after "množina": množina dělitelů
  setOfPl : CN -> Adv = \cn -> SyntaxCze.mkAdv possess_Prep (mkNP aPl_Det cn) ;
  divisor_N : N = mkN "dělitel" "dělitele" mascAnimate ;

lin smallest_Adj = mkAdj (mkA "nejmenší") ;
lin descending_Adj = mkAdj (mkA "klesající") ;
lin finitely_supported_Adj = mkAdj (mkA "konečně nesený") ;
lin bounded_above_Adj = mkAdj (mkA "shora omezený") ;
lin bounded_below_Adj = mkAdj (mkA "zdola omezený") ;
lin ceiling_Noun = mkNoun (mkN "strop" "stropu" mascInanimate) ;
lin partially_ordered_Adj = mkAdj (mkA "částečně uspořádaný") ;
lin linearly_ordered_Adj = mkAdj (mkA "lineárně uspořádaný") ;
lin trichotomous_Adj = mkAdj (mkA "trichotomický") ;
lin lower_Adj = mkAdj (mkA "dolní") ;

lin set_of_divisors_Noun = mkNoun (mkCN (mkCN mnozina_N) (setOfPl (mkCN divisor_N))) ;
lin set_of_proper_divisors_Noun = mkNoun (mkCN (mkCN mnozina_N) (setOfPl (mkCN (mkA "vlastní") divisor_N))) ;
lin set_of_prime_factors_Noun = mkNoun (mkCN (mkCN mnozina_N) (setOfPl (mkCN (mkA "prvočíselný") (mkN "faktor" "faktoru" mascInanimate)))) ;

}
