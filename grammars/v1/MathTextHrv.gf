--# -path=.:present:prop:term:wikidata

concrete MathTextHrv of MathText = MathWikidataHrv, PropHrv, TermLatex **
  
  MathTextFunctor - [
    DefIsASuch, HypTyping, PEquation, -- missing
    Agreement, refPron, kindAgr
    ]
  with
    (Syntax = SyntaxHrv),
    (Extend = ExtendHrv),
    (Grammar = GrammarHrv),
    (Symbolic = SymbolicHrv)
    **
    
  open ParadigmsHrv, (R=ResHrv) in {

-- functor exceptions

oper
  Agreement = R.Gender ;
  refPron : Agreement -> Pron = \a -> case a of {
    R.Masc _  => he_Pron ;
    R.Fem => she_Pron ;
    R.Neutr  => it_Pron
    } ;
    
  kindAgr : CN -> Agreement = \cn -> cn.g ;  


-- lexical items not in Wikidata
lin
  commutative_Property = mkAP (mkA "komutativni") ;
  associative_Property = mkAP (mkA "asocijativni") ;
  reflexive_Property = mkAP (mkA "reflexivni") ;
  symmetric_Property = mkAP (mkA "simetrični") ;
  transitive_Property = mkAP (mkA "transitivni") ;

  } 

