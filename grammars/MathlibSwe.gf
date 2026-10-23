concrete MathlibSwe of Mathlib = CategoriesSwe **

open
  UtilitiesSwe, SyntaxSwe, ParadigmsSwe

in {

oper
  -- mängden av delare
  setOfPl : CN -> SyntaxSwe.Adv = \cn -> SyntaxSwe.mkAdv (mkPrep "av") (mkNP aPl_Det cn) ;
  delare_N : N = mkN "delare" "delaren" "delare" "delarna" utrum ;
  -- the participles: ordnad, ordnat, ordnade
  partA : Str -> Str -> Str -> A = \ordnad,ordnat,ordnade ->
    mkA ordnad ordnat ordnade ("mer" ++ ordnad) ("mest" ++ ordnad) ;

lin smallest_Adj = mkAdj (mkA "minst" "minst" "minsta" "minst" "minst") ;
lin descending_Adj = mkAdj (mkA "fallande" "fallande" "fallande" "fallande" "fallande") ;
lin finitely_supported_Adj = mkAdj (partA "ändligt stödd" "ändligt stött" "ändligt stödda") ;
lin bounded_above_Adj = mkAdj (partA "uppåt begränsad" "uppåt begränsat" "uppåt begränsade") ;
lin bounded_below_Adj = mkAdj (partA "nedåt begränsad" "nedåt begränsat" "nedåt begränsade") ;
lin ceiling_Noun = mkNoun (mkN "tak" "taket" "tak" "taken" neutrum) ;
lin partially_ordered_Adj = mkAdj (partA "partiellt ordnad" "partiellt ordnat" "partiellt ordnade") ;
lin linearly_ordered_Adj = mkAdj (partA "linjärt ordnad" "linjärt ordnat" "linjärt ordnade") ;
lin trichotomous_Adj = mkAdj (mkA "trikotom") ;
lin lower_Adj = mkAdj (mkA "undre" "undre" "undre" "undre" "understa") ;

lin set_of_divisors_Noun = mkNoun (mkCN (mkCN mängd_N) (setOfPl (mkCN delare_N))) ;
lin set_of_proper_divisors_Noun = mkNoun (mkCN (mkCN mängd_N) (setOfPl (mkCN (mkA "äkta" "äkta" "äkta" "äkta" "äkta") delare_N))) ;
lin set_of_prime_factors_Noun = mkNoun (mkCN (mkCN mängd_N) (setOfPl (mkCN (mkN "primfaktor" "primfaktorer")))) ;

}
