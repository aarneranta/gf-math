concrete ForthelDemoFre of ForthelDemo = ForthelFre **

open
  SyntaxFre,
  (S=SyntaxFre),
  SymbolicFre,
  (Extend=ExtendFre),
  (Grammar=GrammarFre),
---  (Markup=MarkupFre),
  Prelude,

  ParadigmsFre,
  (P=ParadigmsFre),
  (M=MakeStructuralFre),
  (R=ResFre),
  (I=IrregFre)
  
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
  less_Adjective t = mkAP (mkA2 (mkA "inférieur") dative) t ; 
  greater_Adjective t = mkAP (mkA2 (mkA "supérieur") dative) t ; 

  thesis_Constant = mkNP the_Det thesis_N ;
  contrary_Constant = mkNP the_Det contrary_N ;
  contradiction_Constant = mkNP a_Det contradiction_N ; --- a/an in spec

-- from GFLean

  rational_Adjective = mkAP (mkA "rationnel") ;
  odd_Adjective = mkAP (mkA "impair") ;
  integer_PrimClass = mkPrimClass (mkN "entier") ; ----
  number_PrimClass = mkPrimClass (mkN "nombre" masculine) ;
  real_Adjective = mkAP (mkA "réel") ;
  even_Adjective = mkAP (mkA "pair") ;
  positive_Adjective = mkAP (mkA "positif") ;
  nonnegative_Adjective = mkAP (mkA "non-négatif") ;
  negative_Adjective = mkAP (mkA "négatif") ;
  less_or_equal_Adjective t =
     mkAP or_Conj
        (mkAP (mkA "inférieur"))
	(mkAP equal_A2 t) ;
        
  greater_or_equal_Adjective t =
     mkAP or_Conj
        (mkAP (mkA "supérieur"))
	(mkAP equal_A2 t) ;

oper
  mkPrimClass = overload {
    mkPrimClass : N -> PrimClass
      = \n -> lin PrimClass {cn = mkCN n ; adv = emptyAdv}
    } ;


  set_N : N = mkN "ensemble" masculine ;
  element_N : N = mkN "élément" ;
  function_N : N = mkN "fonction" ;
  zero_N = mkN "zéro" ;
  order_N = mkN "ordre" masculine ;

  converge_V : V = mkV "converger" ;
  divide_V2 : V2 = mkV2 (mkV "diviser") ;
  belong_V2 : V2 = mkV2 I.appartenir_V to_Prep ;
  join_V3 : V3 = mkV3 I.joindre_V accusative with_Prep ;

  prime_A : A = mkA "premier" ;
  dividing_A2 : A2 = mkA2 (mkA "diviseur") genitive ; ----
  less_A2 : A2 = mkA2 (mkA "inférieur") dative ;
  
  thesis_N = mkN "thèse" ;
  contrary_N = mkN "contraire" masculine ;
  contradiction_N = mkN "contradiction" ;

  over_Prep = on_Prep ;

-- Kohlhase

lin
  general_linear_group_Notion ord set = {
    cn = mkCN
           (mkCN 
             (mkCN
	       (mkCN general_A (mkCN linear_A group_N.cn))
	       <symb (mkSymb ("G_" ++ ord.sym.s ++ set.sym.s)) : NP>)
             (possessAdv ord.np))
          (S.mkAdv over_Prep set.np) ;
   isPlur = False
    } ;

  general_A = mkAP (mkA "général") ;
  linear_A = mkAP (mkA "linéaire") ;
  group_N = {cn = mkCN (mkN "Groupe" masculine) ; adv = emptyAdv} ;
  order_PrimClass = mkPrimClass (mkN "ordre" masculine) ;

}