concrete MathCoreEng of MathCore = ConstantsEng ** 
  open
    SyntaxEng,
    (S=SyntaxEng),
    (G=GrammarEng),
    (M=MarkupEng),
    (E=ExtendEng),
    SymbolicEng,
    Prelude,
    ParadigmsEng,
    (I=IrregEng)

in {

lincat
  Exp = NP ;
  Kind = {cn : CN ; adv : Adv} ;
  Prop = Proposition ;
  Jmt = Text ;
  [Exp] = {np : NP ; isPl : Bool} ;
  [Prop] = [S] ;
  ArgKind = {cn : CN ; adv : Adv} ;
  [ArgKind] = NP ;
  Hypo = Utt ;
  [Hypo] = {text : Text ; isEmpty : Bool} ;
  Ident = Symb ;
  [Ident] = {np : NP ; isPl : Bool} ;
  Proof = Text ;
  [Proof] = Text ;
  Rule = Utt ;
  [Rule] = Text ;

lin
  AxiomJmt hypos label prop =
    labelText (axiom_Label ++ (mkUtt label).s)
      (mkText hypos.text (mkText (topProp prop))) ;
  ThmJmt hypos label prop proof =
    labelText (theorem_Label ++ (mkUtt label).s)
      (mkText hypos.text (mkText (mkText (topProp prop))
        (labelText proof_Label proof))) ;
  DefPropJmt hypos prop df =
    labelText definition_Label
      (mkText hypos.text (mkText (G.SSubjS (partProp prop) if_Subj (partProp df)))) ;
  DefKindJmt hypos kind df =
    labelText definition_Label
      (mkText hypos.text (mkText
        (mkS (mkCl (mkNP a_Det (useKind kind)) (mkNP a_Det (useKind df)))))) ;
  DefExpJmt hypos exp kind df =
    labelText definition_Label
      (mkText hypos.text (mkText (mkS (mkCl exp (definedCN (useKind kind) df))))) ;
  AxiomPropJmt hypos prop =
    labelText basic_concept_Label
      (mkText hypos.text (mkText (mkS (mkCl we_NP can_VV (mkVP say_VS (topProp prop)))))) ;
  AxiomKindJmt hypos kind =
    labelText basic_concept_Label
      (mkText hypos.text (mkText
        (mkS (mkCl (mkNP aPl_Det (useKind kind)) (mkNP a_Det basic_type_CN))))) ;
  AxiomExpJmt hypos exp kind =
    labelText basic_concept_Label
      (mkText hypos.text (mkText (mkS (mkCl exp (useKind kind))))) ;

  RewriteJmt rules = labelText by_cases_Label rules ;
  RewriteRule idents patt exp =
    mkUtt (G.ExtAdvS (S.mkAdv for_Prep idents.np) (mkS (mkCl patt exp))) ;
  NoVarRewriteRule patt exp =
    mkUtt (mkS (mkCl patt exp)) ;

  PropHypo prop = mkUtt (mkImp (mkVP assume_VS (topProp prop))) ; 
  VarsHypo idents kind = G.ImpP3 idents.np (mkVP (useKind kind)) ; 
  BareVarsHypo idents = G.ImpP3 idents.np (mkVP arbitrary_A) ;

  AppExp exp exps = mkNP exp (S.mkAdv applied_to_Prep exps.np) ;
  AbsExp idents exp =
    mkNP the_Det (mkCN function_N (mkRS (mkRCl which_RP map_V3 idents.np exp))) ; 
  IdentExp f = latexNP f ;
  TypedExp exp kind = mkNP the_Det (mkCN (mkCN kind.cn exp) kind.adv) ;

  AndProp props = complexProp (mkS and_Conj props) ;
  OrProp props = complexProp (mkS or_Conj props) ;
  IfProp A B = complexProp (G.ExtAdvS (S.mkAdv if_Subj (partProp A)) (mkS then_Adv (partProp B))) ;
  IffProp A B = complexProp (G.SSubjS (partProp A) iff_Subj (partProp B)) ;
  NotProp prop =
    simpleProp (mkS negPol (mkCl 
          (mkVP (mkNP the_Quant (mkCN case_N (S.mkAdv that_Subj (partProp prop))))))) ;
  AllProp argkinds prop =
    simpleProp (G.ExtAdvS (S.mkAdv for_Prep (mkNP all_Predet argkinds)) (partProp prop)) ;
  ExistProp argkinds prop =
    simpleProp (G.SSubjS (mkS (E.ExistsNP argkinds)) such_that_Subj (partProp prop)) ; ---- TODO: sg/pl correctly
  IdentProp f = simpleProp (latexS f) ;
  FalseProp = simpleProp (mkS (mkCl we_NP have_V2 (mkNP a_Det contradiction_N))) ;
  AppProp f exps = simpleProp (mkS (mkCl (latexNP f) hold_V2 exps.np)) ;

  IdentKind formal = {
    cn = mkCN element_N ;
    adv = S.mkAdv possess_Prep (latexNP formal)
    } ;
  SuchThatKind ident kind prop = {
    cn = mkCN kind.cn (latexNP ident) ;
    adv = ccAdv kind.adv (S.mkAdv such_that_Subj (partProp prop))
    } ;
  AppKind formal exps = {
    cn = mkCN element_N ;
    adv = S.mkAdv possess_Prep (mkNP (latexNP formal) (S.mkAdv possess_Prep exps.np))
    } ;
  FunKind argkinds kind = {
    cn = mkCN function_N ;
    adv = ccAdv (S.mkAdv from_Prep argkinds) (S.mkAdv to_Prep (mkNP aPl_Det (useKind kind)))
    } ;

  KindArgKind kind = kind ;
  IdentsArgKind kind idents = {cn = mkCN kind.cn idents.np ; adv = kind.adv} ;

  StrIdent s = mkSymb s.s ;
  StrLabel s = symb (mkSymb s.s) ;
  
  AppProof proofs exp =
    mkText proofs
      (mkText (mkUtt (S.mkAdv by_Prep exp))) ;
      
  AbsProof hypos proof =
    mkText hypos.text proof ;

  BaseIdent ident =
    {np = latexNP ident ; isPl = False} ;
  ConsIdent ident idents =
    {np = mkNP and_Conj (latexNP ident) idents.np ; isPl = True} ;

  BaseExp exp =
    {np = exp ; isPl = False} ;
  ConsExp exp exps =
    {np = mkNP and_Conj exp exps.np ; isPl = True} ;

  BaseArgKind kind =
    mkNP aPl_Det (useKind kind) ;
  ConsArgKind kind kinds =
    mkNP and_Conj (mkNP aPl_Det (useKind kind)) kinds ;

  BaseHypo = {text = emptyText ; isEmpty = True} ;
  ConsHypo hypo hypos = {text = mkText hypo hypos.text ; isEmpty = False} ;
  
  BaseProp a b = mkListS (partProp a) (partProp b) ;
  ConsProp a bs = mkListS (partProp a) bs ;

  BaseProof = emptyText ;
  ConsProof proof proofs = mkText proof proofs ;

  BaseRule rule = labelText item_Label (mkText rule) ;
  ConsRule rule rules = mkText (labelText "\\item" (mkText rule)) rules ;

-- using Constants

  AdjProp adj exp = simpleProp (mkS (mkCl exp adj)) ;
  NotAdjProp adj exp = simpleProp (mkS negPol (mkCl exp adj)) ;
  RelProp rel x y = simpleProp (mkS (mkCl x (mkVP (mkVP rel.ap) (S.mkAdv rel.prep y)))) ;
  NotRelProp rel x y = simpleProp (mkS negPol (mkCl x (mkVP (mkVP rel.ap) (S.mkAdv rel.prep y)))) ;
  NounKind noun = {cn = noun ; adv = lin Adv {s = []}} ;
  NameExp name = name ;
  FunListExp f exps = mkNP the_Det (mkCN f.cn (S.mkAdv f.prep exps.np)) ;
  LabelExp label = label ;
  ConstExp const = const.np ;
  OperListExp op exps = mkNP the_Det (mkCN op.f.cn (S.mkAdv op.f.prep exps.np)) ;
  ComparProp co x y = simpleProp (mkS (mkCl x (mkVP (mkVP co.rel.ap) (S.mkAdv co.rel.prep y)))) ;
  NotComparProp co x y = simpleProp (mkS negPol (mkCl x (mkVP (mkVP co.rel.ap) (S.mkAdv co.rel.prep y)))) ;

oper
  labelText : Str -> Text -> Text = \label, text ->
    lin Text {s = label ++ "." ++ text.s} ;

  Proposition : Type = {s : S ; isComplex : Bool} ;

  simpleProp : S -> Proposition = \s -> {s = s ; isComplex = False} ;
  complexProp : S -> Proposition = \s -> {s = s ; isComplex = True} ;

  topProp : Proposition -> S = \prop -> prop.s ;
  partProp : Proposition -> S = \prop -> case prop.isComplex of {
    True => parenthS prop.s ;
    False => prop.s
    } ;

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

  parenthS : S -> S = \s -> M.MarkupS (lin Mark {begin = "(" ; end = ")"}) s ;

  by_Prep : Prep = by8means_Prep ;

  ccAdv : Adv -> Adv -> Adv = \x, y -> lin Adv {s = x.s ++ y.s} ;

  item_Label : Str = "\\item" ;

-- non-functor
  negPol : Pol = E.UncontractedNeg ;

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

  definition_Label : Str = "Definition" ;
  theorem_Label : Str = "Theorem" ;
  proof_Label : Str = "Proof" ;
  axiom_Label : Str = "Axiom" ;
  basic_concept_Label : Str = "Basic Concept" ;
  by_cases_Label : Str = "By cases:" ;

}