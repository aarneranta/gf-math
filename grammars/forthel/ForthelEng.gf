-- based on http://nevidal.org/download/forthel.pdf

concrete ForthelEng of Forthel =

open
  SyntaxEng,
  SymbolicEng,
  (Extend=ExtendEng),
  Prelude,

  (P=ParadigmsEng),
  (M=MakeStructuralEng),
  (R=ResEng)
  
in {

lincat
  Definition = S ;
  Statement = S ;
  Predicate = VP ;
  Term = NP ;
  Notion = {cn : CN ; isPlur : Bool} ;

  Terms = NP ;
  Predicates = Extend.VPS ;
  Notions = NP ;
  PrimClass = {cn : CN ; adv : Adv} ; -- element (x) , of X

  ClassNoun = CN ;
  DefiniteNoun = CN ;
  QuantifiedNotion = NP ;
  QuantifiedNotions = NP ;
  PlainTerm = NP ;
  Adjective = AP ;  --- these two are one cat in 1.3.1 but separated in 1.3.4
  Verb = VP ;
  Constant = NP ;

  PredicateHead = S ;
  
  Operator = Symb ; -- symbolic
  Relation = Symb ;
  SymbTerm = Symb ;
  Name = Symb ;
  [Name] = {s : Str ; isPlur : Bool} ;

lin
-- 1.3.2
  PrimClassNotion pc = {
    cn = mkCN pc.cn pc.adv ;
    isPlur = False
    } ;
    
  PrimClassNamesNotion pc xs = {
    cn = mkCN (mkCN pc.cn (<symb (mkSymb xs.s) : NP>)) pc.adv ;
    isPlur = xs.isPlur
    } ;
    
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

  NameSymbTerm n = n ;

  StringName s = mkSymb s.s ;
  
  BaseName x = {s = x.s ; isPlur = False} ;
  ConsName x xs = {s = x.s ++ "," ++ xs.s ; isPlur = True} ;

  AdjNotion notion adjective =
    notion ** {cn = mkCN adjective notion.cn} ;
  RelNotion notion predicates =
    notion ** {cn = mkCN notion.cn (Extend.RelVPS which_RP predicates)} ;
  StatNotion notion statement =
    notion ** {cn = mkCN notion.cn (mkAdv such_that_Subj statement)} ;

-- 1.3.3
  EveryTerm notion = mkNP every_Det notion.cn ;  --- overgenerates "every set A, B"
  
  EachTerm notion = mkNP each_Det notion.cn ;  --- overgenerates "each set A, B"
  
  AllTerm notion = mkNP all_Predet (mkNP aPl_Det notion.cn) ;

  SomeTerm notion = case notion.isPlur of {
    True => mkNP somePl_Det notion.cn ;
    False => mkNP someSg_Det notion.cn
    } ;

  AnyTerm notion = case notion.isPlur of {
    True => mkNP any_Quant pluralNum notion.cn ;
    False => mkNP any_Quant notion.cn
    } ;
    
  NoTerm notion = case notion.isPlur of {
    True => mkNP no_Quant pluralNum notion.cn ;
    False => mkNP no_Quant notion.cn
    } ;

  OneQuantifiedNotions qn = qn ;
  AddQuantifiedNotions qn qns = mkNP and_Conj qn qns ;

  QuantifiedTerm qn = qn ;

  PlainTermTerm t = t ;

  SymbPlainTerm t = symb t ;

  DefiniteSgNounTerm n = mkNP the_Det n ;
  DefinitePlNounTerm n = mkNP thePl_Det n ;

  zero_DefiniteNoun = mkCN zero_N ;
  order_DefiniteNoun x = mkCN (mkCN order_N) (mkAdv possess_Prep x) ;

  DoesPredicate verb = verb ;
  IsPredicate adjective = mkVP adjective ;
  IsaPredicate notion = mkVP notion.cn ;
  HasPredicate notion = mkVP have_V2 (mkNP a_Det notion.cn) ;
  HasNoPredicate notion = mkVP have_V2 (mkNP no_Quant notion.cn) ;

  converge_Verb = mkVP converge_V ;
  divide_Verb t = mkVP divide_V2 t ;
  belong_Verb t = mkVP belong_V2 t ;
  join_Verb t u = mkVP join_V3 t u ;

  prime_Adjective = mkAP prime_A ;
  dividing_Adjective t = mkAP dividing_A2 t ;
  equal_Adjective t = mkAP equal_A2 t ;
  less_Adjective t = mkAP less_A2 t ; --- a comparative
  greater_Adjective t = mkAP great_A t ; 

-- 1.3.5
  SimpleStatement terms predicates = Extend.PredVPS terms predicates ;
  WeHaveSymbStatement sym = mkS (mkCl we_NP have_V2 <symb sym : NP>) ;
  WeHaveConstStatement const = mkS (mkCl we_NP have_V2 const) ;
  
  ThereIsStatement notions = mkS (Extend.ExistsNP notions) ;
  ThereIsNoStatement notion = mkS (Extend.ExistsNP (mkNP no_Quant notion.cn)) ;

  PosOnePredicates pred =
    Extend.MkVPS (mkTemp presentTense simultaneousAnt) positivePol pred ;
  NegOnePredicates pred = 
    Extend.MkVPS (mkTemp presentTense simultaneousAnt) negPol pred ;
  PosAddPredicates pred preds =
    Extend.ConjVPS and_Conj
      (Extend.BaseVPS
        (Extend.MkVPS (mkTemp presentTense simultaneousAnt) positivePol pred) preds) ;
  NegAddPredicates pred preds =
    Extend.ConjVPS and_Conj
      (Extend.BaseVPS
        (Extend.MkVPS (mkTemp presentTense simultaneousAnt) negPol pred) preds) ;

  OneTerms term = term ;
  AddTerms term terms = mkNP and_Conj term terms ; --- commas in spec

  OneNotions notion =
    mkNP a_Det notion.cn ;
  AddNotions notion notions =
    mkNP and_Conj (mkNP a_Det notion.cn) notions ; --- commas in spec

  thesis_Constant = mkNP the_Det thesis_N ;
  contrary_Constant = mkNP the_Det contrary_N ;
  contradition_Constant = mkNP a_Det contradiction_N ; --- a/an in spec


---- symbolic statements TODO

  ForStatement qns s = mkS (mkAdv for_Prep qns) s ;

--- simplicied from spec, which uses many levels
--- to resolve ambiguities: that can be misleading to an innocent
--- reader (and the use of primaryStatement seems to compromise this)

  AndStatement s t = mkS and_Conj s t ;
  OrStatement s t = mkS or_Conj s t ;
  IfStatement s t = mkS if_then_Conj s t ;
  IffStatement s t = mkS iff_Conj s t ;

-- 1.3.6

  NotionDefinition definiendum definiens =
    mkS (mkCl (mkNP a_Det definiendum.cn) definiens.cn) ;
  FunctionDefinition definiendum definiens =
    mkS (mkCl (mkNP the_Det definiendum) definiens) ;
  FunctionIsEqualDefinition definiendum definiens =
    mkS (mkCl (mkNP the_Det definiendum) (mkAP equal_A2 definiens)) ;
  PredicateDefinition predhead statement =
    mkS iff_Conj predhead statement ;

  AdjectivePredicateHead adjective names =
    mkS (mkCl (namesNP names) adjective) ;
  VerbPredicateHead verb names =
    mkS (mkCl (namesNP names) verb) ;




--------------------------

oper
--- these work for many languages but can be overridden
  emptyAdv : Adv = lin Adv {s = ""} ;
  concatAdv : Adv -> Adv -> Adv = \a, b -> lin Adv {s = a.s ++ b.s} ;


-- to be functorized
  negPol = Extend.UncontractedNeg ; --- should be negativePol in functor, but isn't 

  namesNP : [Name] -> NP = \xs -> case xs.isPlur of {
    True => symb xs.s ** {n = R.Pl} ;
    False => symb xs.s
    } ;

  set_N : N = P.mkN "set" ;
  element_N : N = P.mkN "element" ;
  function_N : N = P.mkN "function" ;
  zero_N = P.mkN "zero" ;
  order_N = P.mkN "order" ;
  any_Quant = P.mkQuant "any" "any" ;
  each_Det = M.mkDet "each" ;
  such_that_Subj = P.mkSubj "such that" ;

  iff_Conj = P.mkConj "iff" ;

  converge_V : V = P.mkV "converge" ;
  divide_V2 : V2 = P.mkV2 "divide" ;
  belong_V2 : V2 = P.mkV2 (P.mkV "belong") to_Prep ;
  join_V3 : V3 = P.mkV3 (P.mkV "join") P.noPrep with_Prep ;

  prime_A : A = P.mkA "prime" ;
  dividing_A2 : A2 = P.mkA2 (P.mkA "dividing") P.noPrep ;
  equal_A2 : A2 = P.mkA2 (P.mkA "equal") to_Prep ;
  less_A2 : A2 = P.mkA2 (P.mkA "less") (P.mkPrep "than") ; ---
  great_A : A = P.mkA "great" ;
  
  thesis_N = P.mkN "thesis" ;
  contrary_N = P.mkN "contrary" ;
  contradiction_N = P.mkN "contradiction" ;

}