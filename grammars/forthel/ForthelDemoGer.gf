concrete ForthelDemoGer of ForthelDemo = ForthelGer **

open
  SyntaxGer,
  SymbolicGer,
  (Extend=ExtendGer),
  (Grammar=GrammarGer),
  (Markup=MarkupGer),
  Prelude,

  (P=ParadigmsGer),
  (M=MakeStructuralGer),
  (R=ResGer),
  (I=IrregGer)
  
in {


lin
  set_PrimClass = {
    cn = mkCN set_N ;
    adv = emptyAdv
    } ;

  element_PrimClass term = {
    cn = mkCN element_N ;
    adv = mkAdv possess_Prep term
    } ;

  function_PrimClass fromterm toterm = {
    cn = mkCN function_N ;
    adv = concatAdv (mkAdv from_Prep fromterm) (mkAdv to_Prep fromterm)
    } ;

  zero_DefiniteNoun = mkCN zero_N ;
  order_DefiniteNoun x = mkCN (mkCN order_N) (mkAdv possess_Prep x) ;

  converge_Verb = mkVP converge_V ;
  divide_Verb t = mkVP divide_V2 t ;
  belong_Verb t = mkVP belong_V2 t ;
----  join_Verb t u = mkVP join_V3 t u ;

  prime_Adjective = mkAP prime_A ;
  dividing_Adjective t = mkAP dividing_A2 t ;
  equal_Adjective t = mkAP equal_A2 t ;
  less_Adjective t = mkAP less_A2 t ; --- a comparative
  greater_Adjective t = mkAP great_A t ; 

  thesis_Constant = mkNP the_Det thesis_N ;
  contrary_Constant = mkNP the_Det contrary_N ;
  contradiction_Constant = mkNP a_Det contradiction_N ; --- a/an in spec

oper
  
  set_N : N = P.mkN "set" ;
  element_N : N = P.mkN "element" ;
  function_N : N = P.mkN "function" ;
  zero_N = P.mkN "zero" ;
  order_N = P.mkN "order" ;

  converge_V : V = P.mkV "konvergieren" ;
  divide_V2 : V2 = P.mkV2 (P.mkV "teinen") ;
  belong_V2 : V2 = P.mkV2 (P.mkV "gehören") to_Prep ;
----  join_V3 : V3 = P.mkV3 (P.mkV "join") P.noPrep with_Prep ;

  prime_A : A = P.mkA "unteilbar" ;
  dividing_A2 : A2 = P.mkA2 (P.mkA "teilend") P.accPrep ;
  less_A2 : A2 = P.mkA2 (P.mkA "kleiner") (P.mkPrep "als" P.nominative) ; ---
  great_A : A = P.mkA "gross" ; 
  
  thesis_N = P.mkN "Thesis" ;
  contrary_N = P.mkN "Gegenteil" ;
  contradiction_N = P.mkN "Widerspruch" ;
}