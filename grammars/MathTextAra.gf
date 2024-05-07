--# -path=.:present:prop:wikidata

concrete MathTextAra of MathText = MathWikidataAra, PropAra **
  MathTextFunctor - [Fun1QN]  
  with
    (Syntax = SyntaxAra),
    (Extend = ExtendAra)
    **
  open ParadigmsAra in {

lin Fun1QN qn = mkFun1 ((mkUtt qn).s ! masc) qn possess_Prep ;
  
-- lexical items not in Wikidata
lin
  commutative_Property = mkAP (mkA "تبادلي") ;
---  associative_Property = mkAP (mkA "associative") ;
  reflexive_Property = mkAP (mkA "انعكاسي") ;
  symmetric_Property = mkAP (mkA "تناظري") ;
  transitive_Property = mkAP (mkA "متعدي") ;

  } 

