-- based on http://nevidal.org/download/forthel.pdf

concrete ForthelGer of Forthel =

  ForthelTermsAscii **
  ForthelFunctor with
    (Syntax=SyntaxGer),
    (Symbolic=SymbolicGer),
    (Extend=ExtendGer),
    (Grammar=GrammarGer),
    (Markup=MarkupGer)

** open

  (P=ParadigmsGer),
  (M=MakeStructuralGer),
  (R=ResGer),
  (I=IrregGer),
  Prelude
  
in {

oper
-- functor exceptions



-- words etc

  denote_V2 : V2 =
    P.mkV2 (P.mkV "bedeuten") ;

  any_Quant = a_Quant ; ---- TODO
  each_Det = every_Det ; ---- TODO
  such_that_Subj = M.mkSubj "so dass" ;

  iff_Conj = M.mkConj [] "wenn und nur wenn" P.singular ;

  equal_A2 : A2 = P.mkA2 (P.mkA "gleich") with_Prep ;
}
