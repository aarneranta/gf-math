--# -path=.:present:prop:wikidata

concrete MathTextPor of MathText = MathWikidataPor, PropPor **
  MathTextFunctor - [Agreement, refPron, kindAgr]
  
  with
    (Syntax = SyntaxPor),
    (Extend = ExtendPor) **
    
  open ParadigmsPor, (R=CommonRomance) in {

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
  commutative_Property = mkAP (mkA "comutativo") ;
  associative_Property = mkAP (mkA "associativo") ;
  reflexive_Property = mkAP (mkA "reflexivo") ;
  symmetric_Property = mkAP (mkA "simétrico") ;
  transitive_Property = mkAP (mkA "transitivo") ;

  } 


