--# -path=.:present

concrete PropHrv of Prop = PropFunctor -
  [PExists, PExist, APredRefl] -- not yet

with 
  (Syntax = SyntaxHrv), 
  (Symbolic = SymbolicHrv), 
  (Symbol = SymbolHrv),
  (Sentence = SentenceHrv)
   ** open ParadigmsHrv, Prelude in {

flags coding = utf8 ;

-- instance of interface

oper
  case_N = mkN "slučaj" ;
  such_A = invarA "derart" ; ---
  then_Adv = ParadigmsHrv.mkAdv "onda" ;
  element_N = mkN "element" ;
--  set_N2 = mkN2 (mkN "Menge") possess_Prep ;
  hold_V = mkV "stoji" ;

  singular = ParadigmsHrv.singular ; ---

-- test lexicon
{-
lin
  Vertical = mkAP (mkA "vertikal") ;
  Horizontal = mkAP (mkA "horizontal") ;
  Parallel = mkA2 (mkA "parallel") to_Prep ;
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
-}

}
