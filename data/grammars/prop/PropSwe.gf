--# -path=.:present

concrete PropSwe of Prop = PropFunctor - [PUniv]  with 
  (Syntax = SyntaxSwe), 
  (Symbolic = SymbolicSwe), 
  (Symbol = SymbolSwe),
  (Sentence = SentenceSwe)
   ** open (P = ParadigmsSwe), Prelude in {

flags coding = utf8 ;

-- exception

lin
  PUniv v p = {s = ExtAdvS (mkAdv for_Prep (mkNP everybody_NP (lin Adv v))) p.s ; c = False} ;

-- instance of interface

oper
  case_N = P.mkN "fall" "fall" ;
  such_A = P.mkA "sådan" ;
  then_Adv = P.mkAdv "så" ;
  element_N = P.mkN "element" "element" ;
  set_N2 = P.mkN2 (P.mkN "mängd" "mängder") possess_Prep ;
  hold_V = P.mkV "gäller" ;

  singular = P.singular ; ---


-- test lexicon

lin
  Vertical = mkAP (P.mkA "vertikal") ;
  Horizontal = mkAP (P.mkA "horisontal") ;
  Parallel = P.mkA2 (P.mkA "parallell") to_Prep ;
  Equal = P.mkA2 (P.mkA "lika") with_Prep ;
  Line = mkCN (P.mkN "linje" "linjer") ;
  Point = mkCN (P.mkN "punkt" "punkter") ;
  Centre = mkFun1 "centre" (mkCN (P.mkN "mittpunkt" "mittpunkter")) possess_Prep ;
  Intersection = mkFun1 "intersection" (mkCN (P.mkN "snitt" "snitt")) possess_Prep ;

  Set k = mkCN set_N2 (mkNP a_Art plNum k) ; 

  Even = mkAP (P.mkA "jämn") ;
  Odd = mkAP (P.mkA "udda") ;
  Square = mkFun1 "square" (mkCN (P.mkN "kvadrat" "kvadrater")) possess_Prep ;
  Sum = mkFun1 "sum" (mkCN (P.mkN "summa")) possess_Prep ;
  Product = mkFun1 "product" (mkCN (P.mkN "produkt" "produkter")) possess_Prep ;
  Nat = mkCN (P.mkN "tal" "tal") ;

}
