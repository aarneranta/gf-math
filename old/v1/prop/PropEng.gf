--# -path=.:present

concrete PropEng of Prop = PropFunctor - [PNeg, PNegAtom] with 
  (Syntax = SyntaxEng), 
  (Symbolic = SymbolicEng),
  (Sentence = SentenceEng)
   ** open (P = ParadigmsEng), ParadigmsEng, ExtraEng, Prelude in {

-- exceptions

lin
  PNeg p = { 
    s = mkS ExtraEng.UncNeg (mkCl 
          (mkVP (mkNP the_Quant (mkCN case_N (mkAdv that_Subj p.s))))) ; 
    c = False ---- ?
    } ;
  PNegAtom a = {
    s = mkS ExtraEng.UncNeg a ;
    c = False
    } ;

-- instance of interface

oper
  case_N = P.mkN "case" ;
  such_A = P.mkA "such" ;
  then_Adv = P.mkAdv "then" ;
  element_N = P.mkN "element" ;
  set_N2 = P.mkN2 (P.mkN "set") ;
  hold_V = P.mkV "hold" "held" "held" ;

  singular = P.singular ; ---


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

}
