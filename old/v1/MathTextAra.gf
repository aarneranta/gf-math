--# -path=.:present:prop:term:wikidata

concrete MathTextAra of MathText = MathWikidataAra, PropAra, TermLatex **
  MathTextFunctor - [Fun1QN, HypTyping]  
  with
    (Syntax = SyntaxAra),
    (Extend = ExtendAra),
    (Grammar = GrammarAra),
    (Symbolic = SymbolicAra)
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

