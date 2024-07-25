concrete ForthelDemoEng of ForthelDemo = ForthelEng **

open
  SyntaxEng,
  (S=SyntaxEng),
  SymbolicEng,
  (Extend=ExtendEng),
  (Grammar=GrammarEng),
  (Markup=MarkupEng),
  Prelude,

  ParadigmsEng,
  (P=ParadigmsEng),
  (M=MakeStructuralEng),
  (R=ResEng),
  (I=IrregEng)
  
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

-- from GFLean

  rational_Adjective = mkAP (mkA "rational") ;
  odd_Adjective = mkAP (mkA "odd") ;
  integer_PrimClass = mkPrimClass (mkN "integer") ;
  number_PrimClass = mkPrimClass (mkN "number") ;
  real_Adjective = mkAP (mkA "real") ;
  even_Adjective = mkAP (mkA "even") ;
  positive_Adjective = mkAP (mkA "positive") ;
  nonnegative_Adjective = mkAP (mkA "nonnegative") ;
  negative_Adjective = mkAP (mkA "negative") ;
  less_or_equal_Adjective t =
    Grammar.AdvAP
      (mkAP or_Conj (mkAP (mkA "less than")) (mkAP (mkA "equal"))) --- hack
      (S.mkAdv to_Prep t) ;
  greater_or_equal_Adjective t =
    Grammar.AdvAP
      (mkAP or_Conj (mkAP (mkA "less than")) (mkAP (mkA "equal"))) --- hack
      (S.mkAdv to_Prep t) ;


oper
  set_N : N = mkN "set" ;
  element_N : N = mkN "element" ;
  function_N : N = mkN "function" ;
  zero_N = mkN "zero" ;
  order_N = mkN "order" ;

  converge_V : V = mkV "converge" ;
  divide_V2 : V2 = mkV2 "divide" ;
  belong_V2 : V2 = mkV2 (mkV "belong") to_Prep ;
  join_V3 : V3 = mkV3 (mkV "join") noPrep with_Prep ;

  prime_A : A = mkA "prime" ;
  dividing_A2 : A2 = mkA2 (mkA "dividing") noPrep ;
  less_A2 : A2 = mkA2 (mkA "less") (mkPrep "than") ; ---
  great_A : A = mkA "great" ;
 
  thesis_N = mkN "thesis" ;
  contrary_N = mkN "contrary" ;
  contradiction_N = mkN "contradiction" ;

  over_Prep = mkPrep "over" ;

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

  general_A = mkAP (mkA "general") ;
  linear_A = mkAP (mkA "linear") ;
  group_N = {cn = mkCN (mkN "group") ; adv = emptyAdv} ;
  order_PrimClass = mkPrimClass (mkN "order") ;

}
