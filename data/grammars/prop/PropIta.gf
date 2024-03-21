--# -path=.:present

concrete PropIta of Prop = PropFunctor with --- - [PUnivs,IUniv]  with 
  (Syntax = SyntaxIta), 
  (Symbolic = SymbolicIta), 
  (Sentence = SentenceIta)
   ** open (P = ParadigmsIta), IrregIta, Prelude, ExtraIta in {

flags coding = utf8 ;

-- exception
{-
lin
  PUnivs vs k p = 
    {s = ExtAdvS (mkAdv for_Prep (mkNP all_Predet (mkNP thePl_Det (mkCN k vs)))) p.s ; c = False} ;

  IUniv k = {s = mkNP tout_Det k ; isSymbolic = False} ;
-}

-- instance of interface

oper
  case_N = P.mkN "caso" ;
  such_A = P.mkA "tale" ;
  then_Adv = P.mkAdv "allora" ;
  element_N = P.mkN "elemento" ;
  set_N2 = P.mkN2 (P.mkN "insieme" P.masculine) possess_Prep ;
  hold_V = valere_V ;

  singular = P.singular ; ---

-- test lexicon

lin
  Vertical = mkAP (P.mkA "verticalw") ;
  Horizontal = mkAP (P.mkA "horizzontale") ;
  Parallel = P.mkA2 (P.mkA "parallelo") to_Prep ;
  Equal = P.mkA2 (P.mkA "uguale") P.dative ;
  Line = mkCN (P.mkN "linea") ;
  Point = mkCN (P.mkN "punto") ;
  Centre = mkFun1 "centre" (P.mkN "centro" P.masculine) possess_Prep ;
  Intersection = mkFun2 "intersection" (P.mkN "intersezione" P.feminine) possess_Prep ;

  Set k = mkCN set_N2 (mkNP a_Art plNum k) ; 

  Even = mkAP (P.mkA "pari") ;
  Odd = mkAP (P.mkA "dispari") ;
  Square = mkFun1 "square" (P.mkN "quadrato") possess_Prep ;
  Sum = mkFun2 "sum" (P.mkN "somma") possess_Prep ;
  Product = mkFun2 "product" (P.mkN "prodotto") possess_Prep ;
  Nat = mkCN  (P.mkA "naturale") (P.mkN "numero") ;

oper
  mkFun1, mkFun2 : Str -> N -> Prep -> {s : Symb ; v : N2} = \s,n,p -> 
    {s = mkSymb ("\\" + s) ; v = P.mkN2 n p} ;

}
