concrete CoreEng of Core =
  open
    SyntaxEng,
    (S=SyntaxEng),
    (G=GrammarEng),
    (M=MarkupEng),
    (E=ExtendEng),
    SymbolicEng,
    Prelude,
    ParadigmsEng

in {

lincat
  Jmt = Text ;
  Exp = NP ;
  [Exp] = {np : NP ; isPl : Bool} ;
  Prop = S ;
  [Prop] = [S] ;
  Kind = {cn : CN ; adv : Adv} ;
  Hypo = Utt ;
  [Hypo] = {text : Text ; isEmpty : Bool} ;
  Ident = Symb ;
  [Ident] = {np : NP ; isPl : Bool} ;
  Formal = Symb ;
  Proof = Text ;
  [Proof] = Text ;

lin
  AxiomJmt exp hypos prop =
    labelText (axiom_Label ++ (mkUtt exp).s)
      (mkText hypos.text (mkText prop)) ;
  ThmJmt exp hypos prop proof =
    labelText (theorem_Label ++ (mkUtt exp).s)
      (mkText hypos.text (mkText (mkText prop)
        (labelText proof_Label proof))) ;
  DefPropJmt hypos prop df =
    labelText definition_Label
      (mkText hypos.text (mkText (G.SSubjS prop if_Subj df))) ;
  DefKindJmt hypos kind df =
    labelText definition_Label
      (mkText hypos.text (mkText
        (mkS (mkCl (mkNP a_Det (useKind kind)) (mkNP a_Det (useKind df)))))) ;
  DefExpJmt hypos exp kind df =
    labelText definition_Label
      (mkText hypos.text (mkText (mkS (mkCl exp (definedCN (useKind kind) df))))) ;
  AxiomPropJmt hypos prop =
    labelText basic_concept_Label
      (mkText hypos.text (mkText (mkS (mkCl we_NP can_VV (mkVP say_VS prop))))) ;
  AxiomKindJmt hypos kind =
    labelText basic_concept_Label
      (mkText hypos.text (mkText
        (mkS (mkCl (mkNP a_Det (useKind kind)) (mkNP a_Det basic_type_CN))))) ;
  AxiomExpJmt hypos exp kind =
    labelText basic_concept_Label
      (mkText hypos.text (mkText (mkS (mkCl exp (useKind kind))))) ;

  PropHypo prop = mkUtt (mkImp (mkVP assume_VS prop)) ; 
  VarsHypo idents kind = G.ImpP3 idents.np (mkVP (useKind kind)) ; 

  AppExp exp exps = mkNP exp (S.mkAdv applied_to_Prep exps.np) ;
  AbsExp idents exp =
    mkNP the_Det (mkCN function_N (mkRS (mkRCl which_RP map_V3 idents.np exp))) ; 
  FormalExp f = latexNP f ;
  TypedExp exp kind = mkNP the_Det (mkCN (mkCN kind.cn exp) kind.adv) ;

  AndProp props = mkS and_Conj props ;
  OrProp props = mkS or_Conj props ;
  IfProp A B = G.ExtAdvS (S.mkAdv if_Subj A) (mkS then_Adv B) ;
  IffProp A B = G.SSubjS A iff_Subj B ;
  NotProp prop =
    mkS E.UncontractedNeg (mkCl 
          (mkVP (mkNP the_Quant (mkCN case_N (S.mkAdv that_Subj prop))))) ;
  AllProp idents kind prop =
    G.ExtAdvS
      (S.mkAdv for_Prep
         (mkNP all_Predet
	    (mkNP aPl_Det (mkCN (mkCN kind.cn idents.np) kind.adv)))) prop ;
  ExistProp idents kind prop =
    G.SSubjS (mkS (E.ExistsNP (notionNP idents kind))) such_that_Subj prop ;
  FormalProp f = latexS f ;
  FalseProp = mkS (mkCl we_NP have_V2 (mkNP a_Det contradiction_N)) ;

  EqProp x y = mkS (mkCl x equal_A2 y) ;

  FormalKind formal = {cn = mkCN element_N ; adv = S.mkAdv possess_Prep (latexNP formal)} ;
  SuchThatKind ident kind prop = {
    cn = mkCN kind.cn <symb ident : NP> ;
    adv = ccAdv kind.adv (S.mkAdv such_that_Subj prop)
    } ;

  StrIdent s = mkSymb s.s ;
  StrFormal s = mkSymb s.s ;

  BaseIdent ident =
    {np = latexNP ident ; isPl = False} ;
  ConsIdent ident idents =
    {np = mkNP and_Conj (latexNP ident) idents.np ; isPl = True} ;

  BaseExp exp =
    {np = exp ; isPl = False} ;
  ConsExp exp exps =
    {np = mkNP and_Conj exp exps.np ; isPl = True} ;

  BaseHypo = {text = emptyText ; isEmpty = True} ;
  ConsHypo hypo hypos = {text = mkText hypo hypos.text ; isEmpty = False} ;
  
  BaseProp a b = mkListS a b ;
  ConsProp a bs = mkListS a bs ;

  AppProof proofs exp prop =
    mkText proofs
      (mkText (G.ExtAdvS (S.mkAdv by_Prep exp) prop)) ;

  BaseProof = emptyText ;
  ConsProof proof proofs = mkText proof proofs ;

oper
  labelText : Str -> Text -> Text = \label, text ->
    lin Text {s = label ++ "." ++ text.s} ;

  notionNP : {np : NP ; isPl : Bool} -> {cn : CN ; adv : Adv} -> NP = \idents, kind ->
    let det = case idents.isPl of {
      True => aPl_Det ; 
      False => a_Det
      }
    in mkNP det (mkCN (mkCN kind.cn idents.np) kind.adv) ;

  definedCN : CN -> NP -> CN = \cn, np ->
    mkCN cn (S.mkAdv defined_as_Prep np) ;

  useKind : {cn : CN ; adv : Adv} -> CN = \kind -> mkCN kind.cn kind.adv ;

  latexNP : Symb -> NP = \x ->
    symb (mkSymb ("$" ++ x.s ++ "$")) ;
  latexS : Symb -> S = \x ->
    symb (mkSymb ("$" ++ x.s ++ "$")) ;

  by_Prep : Prep = by8means_Prep ;

  ccAdv : Adv -> Adv -> Adv = \x, y -> lin Adv {s = x.s ++ y.s} ;

-- non-functor

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
  basic_type_CN : CN = mkCN (mkA "basic") (mkN "basic") ;
  map_V3 = mkV3 (mkV "map") noPrep to_Prep ;
  say_VS = mkVS (mkV "say" "said" "said") ;

  equal_A2 : A2 = mkA2 (mkA "equal") to_Prep ;
  less_A2 : A2 = mkA2 (mkA "less") than_Prep ;
  greater_A2 : A2 = mkA2 (mkA "greater") than_Prep ;
  than_Prep : Prep = mkPrep "than" ;
  iff_Subj : Subj = mkSubj "if and only if" ;

  definition_Label : Str = "Definition" ;
  theorem_Label : Str = "Theorem" ;
  proof_Label : Str = "Proof" ;
  axiom_Label : Str = "Axiom" ;
  basic_concept_Label : Str = "Basic Concept" ;

}