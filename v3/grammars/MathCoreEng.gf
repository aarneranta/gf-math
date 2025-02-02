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
  such_that_Subj : Subj = mkSubj "such that" ;
  applied_to_Prep : Prep = mkPrep "applied to" ;
  defined_as_Prep : Prep = mkPrep "defined as" ;
  function_N : N = mkN "function" ;
  basic_type_CN : CN = mkCN (mkA "basic") (mkN "type") ;
  map_V3 = mkV3 (mkV "map") noPrep to_Prep ;
  say_VS = mkVS I.say_V ;
  hold_V2 = mkV2 I.hold_V for_Prep ;
  arbitrary_A = mkA "arbitrary" ;

  equal_A2 : A2 = mkA2 (mkA "equal") to_Prep ;
  less_A2 : A2 = mkA2 (mkA "less") than_Prep ;
  greater_A2 : A2 = mkA2 (mkA "greater") than_Prep ;
  than_Prep : Prep = mkPrep "than" ;
  iff_Subj : Subj = mkSubj "if and only if" ;

  basic_concept_Str = "basic concept" ;
  by_cases_Str = "by cases:" ;
  proof_Str = "proof" ;
  axiom_Str = "axiom" ;
  theorem_Str = "theorem" ;
  definition_Str = "definition" ;

  instance_N = mkN "instance" ;
  prove_VS = mkVS (mkV "prove") ;
  number_N = mkN "number" ;
  integer_CN = mkCN (mkN "integer") ;
  
  natural_number_CN = mkCN (mkA "natural") number_N ;
  rational_number_CN = mkCN (mkA "rational") number_N ;
  real_number_CN = mkCN (mkA "real") number_N ;
  bare_element_CN = mkCN (mkA "bare") element_N ;
  as_Prep : Prep = mkPrep "as" ;

  let_Str = "let" ;

}