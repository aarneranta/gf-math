concrete MathCoreEng of MathCore =
  TermsLatex, 
  UserConstantsEng **
  MathCoreFunctor - [negPol] with
    (Syntax=SyntaxEng),
    (Grammar=GrammarEng),
    (Markup=MarkupEng),
    (Extend=ExtendEng),
    (Symbolic=SymbolicEng)
  ** open
    Prelude,
    ParadigmsEng,
    (I=IrregEng)

in {

oper
-- override
  negPol : Pol = Extend.UncontractedNeg ;

  define_V2 : V2 = mkV2 (mkV "define") ;
  assume_VS : VS = mkVS (mkV "assume") ;
  element_N : N = mkN "element" ;
  case_N : N = mkN "case" ;
  contradiction_N : N = mkN "contradiction" ;
  then_Adv : Adv = ParadigmsEng.mkAdv "then" ;
  thenText_Adv : Adv = ParadigmsEng.mkAdv "then" ;
  such_that_Subj : Subj = mkSubj "such that" ;
  applied_to_Prep : Prep = mkPrep "applied to" ;
  defined_as_Prep : Prep = mkPrep "defined as" ;
  function_N : N = mkN "function" ;
  basic_type_CN : CN = mkCN (mkA "basic") (mkN "type") ;
  map_V3 = mkV3 (mkV "map") noPrep to_Prep ;
  say_VS = mkVS I.say_V ;
  hold_V2 = mkV2 I.hold_V for_Prep ;
  arbitrary_A = mkA "arbitrary" ;

  iff_Subj : Subj = mkSubj "if and only if" ;

  basic_concept_Str = "basic concept" ;
  by_cases_Str = "by cases:" ;
  proof_Str = "proof" ;
  axiom_Str = "axiom" ;
  theorem_Str = "theorem" ;
  definition_Str = "definition" ;

  instance_N = mkN "instance" ;
  prove_VS = mkVS (mkV "prove") ;

  as_Prep : Prep = mkPrep "as" ;

  let_Str = "let" ;

}