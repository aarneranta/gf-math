concrete MathlibFre of Mathlib = CategoriesFre **

open
  UtilitiesFre, SyntaxFre, ParadigmsFre

in {

oper
  -- "des" and a plural: l'ensemble des diviseurs
  setOfPl : CN -> Adv = \cn -> SyntaxFre.mkAdv possess_Prep (mkNP thePl_Det cn) ;
  -- an adjective phrase that is not inflected
  fixedA : Str -> A = \s -> mkA s s s s ;

lin smallest_Adj = mkAdj (prefixA (mkA "plus petit" "plus petite" "plus petits" "plus petites")) ;
lin descending_Adj = mkAdj "décroissant" ;
lin finitely_supported_Adj = mkAdj (fixedA "à support fini") ;
lin bounded_above_Adj = mkAdj "majoré" ;
lin bounded_below_Adj = mkAdj "minoré" ;
lin ceiling_Noun = mkNoun (mkN "plafond" masculine) ;
lin partially_ordered_Adj = mkAdj "partiellement ordonné" ;
lin linearly_ordered_Adj = mkAdj "totalement ordonné" ;
lin trichotomous_Adj = mkAdj "trichotome" ;
lin lower_Adj = mkAdj (mkA "inférieur" "inférieure") ;

lin set_of_divisors_Noun = mkNoun (mkCN (mkCN (mkN "ensemble" masculine)) (setOfPl (mkCN (mkN "diviseur" masculine)))) ;
lin set_of_proper_divisors_Noun = mkNoun (mkCN (mkCN (mkN "ensemble" masculine)) (setOfPl (mkCN (mkA "propre") (mkN "diviseur" masculine)))) ;
lin set_of_prime_factors_Noun = mkNoun (mkCN (mkCN (mkN "ensemble" masculine)) (setOfPl (mkCN (mkA "premier" "première") (mkN "facteur" masculine)))) ;

}
