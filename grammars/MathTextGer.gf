--# -path=.:present:prop:term:wikidata

concrete MathTextGer of MathText = MathWikidataGer, PropGer, TermLatex **
  
  MathTextFunctor - [Agreement, refPron, kindAgr]
  with
    (Syntax = SyntaxGer),
    (Extend = ExtendGer),
    (Symbolic = SymbolicGer)
    **
    
  open ParadigmsGer, (R=ResGer) in {

-- functor exceptions

oper
  Agreement = R.Gender ;
  refPron : Agreement -> Pron = \a -> case a of {
    R.Masc  => he_Pron ;
    R.Fem => she_Pron ;
    R.Neutr  => it_Pron
    } ;
    
  kindAgr : CN -> Agreement = \cn -> cn.g ;  


-- lexical items not in Wikidata
lin
  commutative_Property = mkAP (mkA "kommutativ") ;
  associative_Property = mkAP (mkA "assoziativ") ;
  reflexive_Property = mkAP (mkA "reflexiv") ;
  symmetric_Property = mkAP (mkA "symmetrisch") ;
  transitive_Property = mkAP (mkA "transitiv") ;

  } 

