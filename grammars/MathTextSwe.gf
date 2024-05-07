--# -path=.:present:prop:wikidata

concrete MathTextSwe of MathText = MathWikidataSwe, PropSwe **
  MathTextFunctor - [Agreement, refPron, kindAgr]
  
  with
    (Syntax = SyntaxSwe),
    (Extend = ExtendSwe) **
    
  open ParadigmsSwe, (R=CommonScand), (X=ExtraSwe) in {

-- functor exceptions

oper
  Agreement = R.Gender ;
  refPron : Agreement -> Pron = \a -> case a of {
    R.Utr  => X.it8utr_Pron ;
    R.Neutr => it_Pron 
    } ;
    
  kindAgr : CN -> Agreement = \cn -> cn.g ;  


-- lexical items not in Wikidata
lin
  commutative_Property = mkAP (mkA "kommutativ") ;
  associative_Property = mkAP (mkA "associativ") ;
  reflexive_Property = mkAP (mkA "reflexiv") ;
  symmetric_Property = mkAP (mkA "symmetrisk") ;
  transitive_Property = mkAP (mkA "transitiv") ;

  } 

