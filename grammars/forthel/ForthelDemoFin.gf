concrete ForthelDemoFin of ForthelDemo = ForthelFin **

open
  SyntaxFin,
  (S=SyntaxFin),
  SymbolicFin,
  (Extend=ExtendFin),
  (Grammar=GrammarFin),
  (Markup=MarkupFin),
  Prelude,

  ParadigmsFin,
  (P=ParadigmsFin),
  (M=MakeStructuralFin),
  (R=ResFin)
  
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
  equal_Adjective t = mkAP (mkA2 equal_A kuin_Prep) t ;
  less_Adjective t = mkAP small_A t ; --- a comparative
  greater_Adjective t = mkAP great_A t ; 

  thesis_Constant = mkNP the_Det thesis_N ;
  contrary_Constant = mkNP the_Det contrary_N ;
  contradiction_Constant = mkNP a_Det contradiction_N ; --- a/an in spec

-- from GFLean

  rational_Adjective = mkAP (mkA "rationaalinen") ;
  odd_Adjective = mkAP (mkA "pariton") ;
  integer_PrimClass = mkPrimClass (mkN "kokonais" (mkN "luku")) ;
  number_PrimClass = mkPrimClass (mkN "luku") ;
  real_Adjective = mkAP (mkA "reaalinen") ;
  even_Adjective = mkAP (mkA "pariton") ;
  positive_Adjective = mkAP (mkA "positiivinen") ;
  nonnegative_Adjective = mkAP (mkA "ei-negatiivinen") ;
  negative_Adjective = mkAP (mkA "negatiivinen") ;
  less_or_equal_Adjective t =
    Grammar.AdvAP
      (mkAP or_Conj (comparAP small_A) (mkAP equal_A)) --- hack
      (S.mkAdv kuin_Prep t) ;
  greater_or_equal_Adjective t =
    Grammar.AdvAP
      (mkAP or_Conj (comparAP great_A) (mkAP equal_A)) --- hack
      (S.mkAdv kuin_Prep t) ;


oper
  set_N : N = mkN "joukko" ;
  element_N : N = mkN "alkio" ;
  function_N : N = mkN "fuktio" ;
  zero_N = mkN "nolla" ;
  order_N = mkN "järjestys" ;

  converge_V : V = mkV "supeta" ;
  divide_V2 : V2 = mkV2 "jakaa" ;
  belong_V2 : V2 = mkV2 (mkV "kuulua") to_Prep ;
  join_V3 : V3 = mkV3 (mkV "yhdistää") accusative to_Prep ;

  prime_A : A = mkA "jaoton" ;
  dividing_A2 : A2 = mkA2 (mkA "jakava") accusative ;
  small_A = mkA (mkN "pieni" "pieniä") ;
  great_A : A = mkA (mkN "suuri" "suuria") ;
  equal_A : A = mkA (mkN "yhtä" (mkN "suuri" "suuria")) ;
 
  thesis_N = mkN "väite" ;
  contrary_N = mkN "vasta" (mkN "kohta") ;
  contradiction_N = mkN "risti" (mkN "riita") ;

  over_Prep = mkPrep "yli" genitive ;
  kuin_Prep = mkPrep "kuin" nominative ;

-- Kohlhase

lin
  general_linear_group_Notion ord set = {
    cn = mkCN
           (mkCN 
             (mkCN
	       (mkCN general_A (mkCN linear_A group_N.cn))
	       <symb (mkSymb ("G_" ++ ord.sym.s ++ set.sym.s)) : NP>)
             (possessAdv (ord.np)))
          (S.mkAdv over_Prep set.np) ;
   isPlur = False
    } ;

  general_A = mkAP (mkA "yleinen") ;
  linear_A = mkAP (mkA "lineaarinen") ;
  group_N = {cn = mkCN (mkN "ryhmä") ; adv = emptyAdv} ;
  order_PrimClass = mkPrimClass (mkN "kertaluku") ; --- ?

}
