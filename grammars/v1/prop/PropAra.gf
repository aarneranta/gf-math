--# -path=.:present

concrete PropAra of Prop = PropFunctor - [

  COr, --- implementation missing
  app1, app2 --- type mismatch
  ]
  
 with 
  (Syntax = SyntaxAra), 
  (Symbolic = SymbolicAra),
  (Sentence = SentenceAra)
   ** open (P = ParadigmsAra), ParadigmsAra, Prelude in {

-- exceptions

oper
  app1 : Symb -> NP -> NP = \f,x -> symbNP (f.s ++ "{" ++ (mkUtt x).s ! masc ++ "}") ; 

  app2 : Symb -> NP -> NP -> NP = \f,x,y -> 
    symbNP (f.s ++ "{" ++ (mkUtt x).s ! masc ++ "}" ++ "{" ++ (mkUtt y).s ! masc ++ "}") ; 

-- instance of interface

oper
  case_N = P.mkN "case" ;
  such_A = P.mkA "such" ;
  then_Adv = P.mkAdv "then" ;
  element_N = P.mkN "element" ;
  set_N2 = P.mkN2 (P.mkN "set") ;
  hold_V =  regV "يَمسِك" ;

  singular = P.sg ; ---

{-
-- test lexicon

lin
  Vertical = mkAP (P.mkA "vertical") ;
  Horizontal = mkAP (P.mkA "horizontal") ;
  Parallel = P.mkA2 (P.mkA "parallel") to_Prep ;
  Equal = P.mkA2 (P.mkA "equal") to_Prep ;
  Line = mkCN (P.mkN "line") ;
  Point = mkCN (P.mkN "point") ;
  Centre = mkFun1 "centre" (mkCN (mkN "centre")) possess_Prep ;
  Intersection = mkFun2 "intersection" (mkCN (mkN "intersection")) possess_Prep ;

  Set k = mkCN set_N2 (mkNP a_Art plNum k) ; 

  Even = mkAP (P.mkA "even") ;
  Odd = mkAP (P.mkA "odd") ;
  Square = mkFun1 "square" (mkCN (mkN "square")) possess_Prep ;
  Sum = mkFun2 "sum" (mkCN (mkN "sum")) possess_Prep ;
  Product = mkFun2 "product" (mkCN (mkN "product")) possess_Prep ;
  Nat = mkCN (P.mkN "number") ;
-}

}
