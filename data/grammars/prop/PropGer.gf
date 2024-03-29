--# -path=.:present

concrete PropGer of Prop = PropFunctor - [PUniv]  with 
  (Syntax = SyntaxGer), 
  (Symbolic = SymbolicGer), 
  (Symbol = SymbolGer),
  (Sentence = SentenceGer)
   ** open ParadigmsGer, IrregGer, Prelude in {

flags coding = utf8 ;

-- exception

lin
  PUniv v p = 
    {s = ExtAdvS (mkAdv for_Prep (mkNP (mkNP (mkPN "alle")) (lin Adv v))) p.s ; c = False} ;

-- instance of interface

oper
  case_N = mkN "Fall" "Fälle" masculine ;
  such_A = invarA "derart" ; ---
  then_Adv = ParadigmsGer.mkAdv "dann" ;
  element_N = mkN "Element" "Elemente" neuter ;
  set_N2 = mkN2 (mkN "Menge") possess_Prep ;
  hold_V = gelten_V ;

  singular = ParadigmsGer.singular ; ---

-- test lexicon

lin
  Vertical = mkAP (mkA "vertikal") ;
  Horizontal = mkAP (mkA "horizontal") ;
  Parallel = mkA2 (mkA "parallel") zu_Prep ;
  Equal = mkA2 (mkA "gleich") datPrep ;
  Line = mkCN (mkN "Gerade") ;
  Point = mkCN (mkN "Punkt" "Punkte" masculine) ;
  Centre = mkFun1 "centre" (mkCN (mkN "Mittelpunkt" "Mittelpunkte" masculine)) possess_Prep ;
  Intersection = mkFun2 "intersection" (mkCN (mkN "Schnitt" "Schnitte" masculine)) possess_Prep ;

  Set k = mkCN set_N2 (mkNP a_Art plNum k) ; 

  Even = mkAP (mkA "gerade") ;
  Odd = mkAP (mkA "ungerade") ;
  Square = mkFun1 "square" (mkCN (mkN "Quadrat" "Quadrate" neuter)) possess_Prep ;
  Sum = mkFun2 "sum" (mkCN (mkN "Summe")) possess_Prep ;
  Product = mkFun2 "product" (mkCN (mkN "Produkt" "Produkte" neuter)) possess_Prep ;
  Nat = mkCN (mkN "Zahl" "Zahlen" feminine) ;

}
