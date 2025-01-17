--# -path=.:present

concrete PropFin of Prop = PropFunctor - [PUniv, PNeg]  with 
  (Syntax = SyntaxFin), 
  (Symbolic = SymbolicFin), 
  (Sentence = SentenceFin)
--  (Lang = LangFin) 
   ** open ParadigmsFin, Prelude in {

flags coding = utf8 ;

-- exception

lin
  PUniv v p = {s = ExtAdvS (mkAdv for_Prep (mkNP everybody_NP (lin Adv v))) p.s ; c = False} ;

  PNeg p = {
    s = mkS negativePol (mkCl 
      (mkVP (mkVP (mkVP (mkV "pitää")) (ParadigmsFin.mkAdv "paikkaansa")) (SyntaxFin.mkAdv that_Subj p.s))) ; 
    c = False
    } ;

-- instance of interface

oper
  case_N = mkN "tapaus" ;
  such_A = mkA "sellainen" ;
  then_Adv = ParadigmsFin.mkAdv "niin" ;
  element_N = mkN "alkio" "alkioita" ;
  set_N2 = mkN2 (mkN "joukko") possess_Prep ;
  hold_V = mkV "päteä" ;

  singular = ParadigmsFin.singular ; ---

-- test lexicon

lin
  Vertical = mkAP (mkA "pystysuora") ;
  Horizontal = mkAP (mkA "vaakasuora") ;
  Parallel = mkA2 (mkA "yhdensuuntainen") with_Prep ;
  Equal = mkA2 (mkA (mkN "sama")) (prePrep nominative "kuin") ;
  Line = mkCN (mkN "suora") ;
  Point = mkCN (mkN "piste") ;
  Centre = mkFun1 "centre" (mkCN (mkN "keskipiste")) possess_Prep ;
  Intersection = mkFun2 "intersection" (mkCN (mkN "leikkaus")) possess_Prep ;

  Set k = mkCN set_N2 (mkNP a_Art plNum k) ; 

  Even = mkAP (mkA "parillinen") ;
  Odd = mkAP (mkA "pariton") ;
  Square = mkFun1 "square" (mkCN (mkN "neliö" "neliöitä")) possess_Prep ;
  Sum = mkFun2 "sum" (mkCN (mkN "summa")) possess_Prep ;
  Product = mkFun2 "product" (mkCN (mkN "tulo")) possess_Prep ;
  Nat = mkCN (mkN "luku") ;

}
