--# -path=.:present:prop:wikidata

concrete MathTextFin of MathText = MathWikidataFin, PropFin **
  MathTextFunctor with
    (Syntax = SyntaxFin),
    (Extend = ExtendFin) **
  open ParadigmsFin in {
  
-- lexical items not in Wikidata
lin
  commutative_Property = mkAP (mkA "kommutatiivinen") ;
  associative_Property = mkAP (mkA "assosiatiivinen") ;
  reflexive_Property = mkAP (mkA "refleksiivinen") ;
  symmetric_Property = mkAP (mkA "symmetrinen") ;
  transitive_Property = mkAP (mkA "transitiivinen") ;

  } 


