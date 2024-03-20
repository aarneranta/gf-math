concrete MathTextIta of MathText = MathWikidataIta **
  MathTextFunctor - [Agreement, refPron, kindAgr]
  
  with
    (Syntax = SyntaxIta) **
    
  open ParadigmsIta, (R=CommonRomance) in {

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
  commutative_Property = mkAP (mkA "commutativo") ;
  associative_Property = mkAP (mkA "associativo") ;
  reflexive_Property = mkAP (mkA "riflessivo") ;
  symmetric_Property = mkAP (mkA "simmetrica") ;
  transitive_Property = mkAP (mkA "transitivo") ;

  } 


