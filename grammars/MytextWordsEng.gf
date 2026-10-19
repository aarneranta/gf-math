concrete MytextWordsEng of MytextWords = CategoriesEng **
open UtilitiesEng, SyntaxEng, ParadigmsEng, (G = GrammarEng) in {
oper
  ofPl : CN -> Adv = \cn -> SyntaxEng.mkAdv possess_Prep (mkNP aPl_Det cn) ;
  subsets : Adv = ofPl (mkCN (mkN "subset")) ;
lin sigma_algebra_Noun = mkCN (mkN "σ-algebra") ;
lin sigma_ring_Noun = mkCN (mkN "σ-ring") ;
lin ring_of_subsets_Noun = mkCN (mkCN (mkN "ring")) subsets ;
lin algebra_of_subsets_Noun = mkCN (mkCN (mkN "algebra")) subsets ;
lin sigma_ring_of_subsets_Noun = mkCN (mkCN (mkN "σ-ring")) subsets ;
lin sigma_algebra_of_subsets_Noun = mkCN (mkCN (mkN "σ-algebra")) subsets ;
lin class_of_subsets_Noun = mkCN (mkCN (mkN "class")) subsets ;
lin sequence_of_subsets_Noun = mkCN (mkCN (mkN "sequence")) subsets ;
lin sequence_of_elements_Noun = mkCN (mkCN (mkN "sequence")) (ofPl (mkCN (mkN "element"))) ;
lin family_of_classes_of_subsets_Noun = mkCN (mkCN (mkN "family")) (ofPl (mkCN (mkCN (mkN "class")) subsets)) ;
oper closedUnder : CN -> AP = \cn -> G.AdvAP (mkAP (mkA "closed")) (SyntaxEng.mkAdv under_Prep (mkNP aPl_Det cn)) ;
lin closed_under_differences_Adj = closedUnder (mkCN (mkN "difference")) ;
lin closed_under_unions_Adj = closedUnder (mkCN (mkN "union")) ;
lin closed_under_intersections_Adj = closedUnder (mkCN (mkN "intersection")) ;
lin closed_under_symmetric_differences_Adj = closedUnder (mkCN (mkA "symmetric") (mkN "difference")) ;
lin closed_under_complements_Adj = closedUnder (mkCN (mkN "complement")) ;
lin closed_under_countable_unions_Adj = closedUnder (mkCN (mkA "countable") (mkN "union")) ;
}
