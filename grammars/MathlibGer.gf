concrete MathlibGer of Mathlib = CategoriesGer **

open
  UtilitiesGer, SyntaxGer, ParadigmsGer

in {

oper
  -- the genitive plural after "Menge": die Menge der Teiler
  setOfPl : CN -> Adv = \cn -> SyntaxGer.mkAdv possess_Prep (mkNP thePl_Det cn) ;
  teiler_N : N = mkN "Teiler" "Teiler" masculine ;

lin smallest_Adj = mkAdj (mkA "kleinst") ;
lin descending_Adj = mkAdj (mkA "fallend") ;
lin finitely_supported_Adj = mkAdj (mkA "endlich getragen") ;
lin bounded_above_Adj = mkAdj (mkA "nach oben beschränkt") ;
lin bounded_below_Adj = mkAdj (mkA "nach unten beschränkt") ;
lin ceiling_Noun = mkNoun (mkN "Decke" "Decken" feminine) ;
lin partially_ordered_Adj = mkAdj (mkA "halbgeordnet") ;
lin linearly_ordered_Adj = mkAdj (mkA "linear geordnet") ;
lin trichotomous_Adj = mkAdj (mkA "trichotom") ;
lin lower_Adj = mkAdj (mkA "unter") ;

lin set_of_divisors_Noun = mkNoun (mkCN (mkCN menge_N) (setOfPl (mkCN teiler_N))) ;
lin set_of_proper_divisors_Noun = mkNoun (mkCN (mkCN menge_N) (setOfPl (mkCN (mkA "echt") teiler_N))) ;
lin set_of_prime_factors_Noun = mkNoun (mkCN (mkCN menge_N) (setOfPl (mkCN (mkN "Primfaktor" "Primfaktoren" masculine)))) ;

}
