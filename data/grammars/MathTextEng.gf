--# -path=.:prop

concrete MathTextEng of MathText = MathWikidataEng, PropEng **
  MathTextFunctor with
    (Syntax = SyntaxEng),
    (Extend = ExtendEng)
    **
  open ParadigmsEng in {

  
-- lexical items not in Wikidata
lin
  commutative_Property = mkAP (mkA "commutative") ;
  associative_Property = mkAP (mkA "associative") ;
  reflexive_Property = mkAP (mkA "reflexive") ;
  symmetric_Property = mkAP (mkA "symmetric") ;
  transitive_Property = mkAP (mkA "transitive") ;

  } 

