concrete ForthelDemoGer of ForthelDemo = ForthelGer **

open
  SyntaxGer,
  (S=SyntaxGer),
  SymbolicGer,
  (Extend=ExtendGer),
  (Grammar=GrammarGer),
  (Markup=MarkupGer),
  Prelude,

  ParadigmsGer,
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
    adv = S.mkAdv possess_Prep term
    } ;

  function_PrimClass fromterm toterm = {
    cn = mkCN function_N ;
    adv = concatAdv (S.mkAdv from_Prep fromterm) (S.mkAdv to_Prep fromterm)
    } ;

  zero_DefiniteNoun = mkCN zero_N ;
  order_DefiniteNoun x = mkCN (mkCN order_N) (S.mkAdv possess_Prep x) ;

  converge_Verb = mkVP converge_V ;
  divide_Verb t = mkVP divide_V2 t ;
  belong_Verb t = mkVP belong_V2 t ;
  join_Verb t u = mkVP join_V3 t u ;

  prime_Adjective = mkAP prime_A ;
  dividing_Adjective t = mkAP dividing_A2 t ;
  equal_Adjective t = mkAP equal_A2 t ;
  less_Adjective t = mkAP less_A2 t ; --- a comparative
  greater_Adjective t = mkAP great_A t ; 

  thesis_Constant = mkNP the_Det thesis_N ;
  contrary_Constant = mkNP the_Det contrary_N ;
  contradiction_Constant = mkNP a_Det contradiction_N ; --- a/an in spec

oper
  
  set_N : N = mkN "Menge" ;
  element_N : N = mkN "Element" neuter ;
  function_N : N = mkN "Funktion" ;
  zero_N = mkN "Null" ;
  order_N = mkN "Ordnung" ;

  converge_V : V = mkV "konvergieren" ;
  divide_V2 : V2 = mkV2 (mkV "teilen") ;
  belong_V2 : V2 = mkV2 (mkV "gehören") to_Prep ;
  join_V3 : V3 = mkV3 (fixprefixV "ver" I.binden_V) accPrep with_Prep ;

  prime_A : A = mkA "unteilbar" ;
  dividing_A2 : A2 = mkA2 (mkA "teilend") accPrep ; ----
  less_A2 : A2 = mkA2 (mkA "kleiner") (mkPrep "als" nominative) ; ---
  great_A : A = mkA "gross" ; 
  
  thesis_N = mkN "Thesis" ;
  contrary_N = mkN "Gegenteil" ;
  contradiction_N = mkN "Widerspruch" ;
}