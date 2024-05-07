--# -path=.:present:prop:term:wikidata

concrete MathTextFre of MathText = MathWikidataFre, PropFre, TermLatex **
  MathTextFunctor - [Agreement, refPron, kindAgr]
  
  with
    (Syntax = SyntaxFre),
    (Extend = ExtendFre),
    (Symbolic = SymbolicFre)
    **
    
  open ParadigmsFre, (R=CommonRomance) in {

-- functor exceptions

oper
  Agreement = R.Gender ;
  refPron : Agreement -> Pron = \a -> case a of {
    R.Masc  => he_Pron ;
    R.Fem => she_Pron 
    } ;
    
  kindAgr : CN -> Agreement = \cn -> cn.g ;  

-- lexical items not in Wikidata
lin
  commutative_Property = mkAP (mkA "commutatif") ;
  associative_Property = mkAP (mkA "associatif") ;
  reflexive_Property = mkAP (mkA "réflexif") ;
  symmetric_Property = mkAP (mkA "symétrique") ;
  transitive_Property = mkAP (mkA "transitif") ;

  } 

