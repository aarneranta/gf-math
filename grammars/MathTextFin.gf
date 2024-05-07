--# -path=.:present:prop:term:wikidata

concrete MathTextFin of MathText = MathWikidataFin, PropFin, TermLatex **
  MathTextFunctor with
    (Syntax = SyntaxFin),
    (Extend = ExtendFin),
    (Symbolic = SymbolicFin)
    **
  open ParadigmsFin in {
  
-- lexical items not in Wikidata
lin
  commutative_Property = mkAP (mkA "kommutatiivinen") ;
  associative_Property = mkAP (mkA "assosiatiivinen") ;
  reflexive_Property = mkAP (mkA "refleksiivinen") ;
  symmetric_Property = mkAP (mkA "symmetrinen") ;
  transitive_Property = mkAP (mkA "transitiivinen") ;

  } 


