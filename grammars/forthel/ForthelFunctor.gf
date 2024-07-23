-- based on http://nevidal.org/download/forthel.pdf

incomplete concrete ForthelFunctor of Forthel = ForthelTerms **

open
  Syntax,
  Symbolic,
  Extend,
  Grammar,
---  Markup,
  Prelude

in {

lincat
  Toplevel = Text ;
  Section = Text ;
  Header = Text ;
  Sentence = Text ;
  Assumption = Text ;
  Synonym = Text ;
  Definition = S ;
  Statement = S ;
  Predicate = VP ;
  Term = NP ;
  Notion = {cn : CN ; isPlur : Bool} ;

  Terms = NP ;
  Predicates = Extend.VPS ;
  Notions = NP ;
  PrimClass = {cn : CN ; adv : Adv} ; -- element (x) , of X

  ClassNoun = {cn : CN ; adv : Adv} ;
  DefiniteNoun = CN ;
  QuantifiedNotion = NP ;
  QuantifiedNotions = NP ;
  PlainTerm = NP ;
  Adjective = AP ;  --- these two are one cat in 1.3.1 but separated in 1.3.4
  Verb = VP ;
  Constant = NP ;

  PredicateHead = S ;
  
---  Operator = Symb ; -- symbolic
---  Relation = Symb ;
  SymbTerm = Symb ;
  Name = Symb ;
  [Name] = {s : Str ; isPlur : Bool} ;

lin
-- importing from ForthelTerms

  FormulaSymbTerm formula = mkSymb formula.s ;
  ExpressionSymbTerm exp = mkSymb exp.s ;
  VarName v = mkSymb v ;


-- 1.3.2
  PrimClassNoun pc = pc ;
  
  ClassNounNotion pc = {
    cn = mkCN pc.cn pc.adv ;
    isPlur = False
    } ;
    
  ClassNounNamesNotion pc xs = {
    cn = mkCN (mkCN pc.cn (<symb (mkSymb xs.s) : NP>)) pc.adv ;
    isPlur = xs.isPlur
    } ;
    
---  NameSymbTerm n = n ;

----  StringName s = mkSymb s.s ; ---- replaced by VariableName
  
  BaseName x = {s = x.s ; isPlur = False} ;
  ConsName x xs = {s = x.s ++ "," ++ xs.s ; isPlur = True} ;

  AdjClassNoun noun adjective =
    noun ** {cn = mkCN adjective noun.cn} ;
  RelClassNoun noun predicates =
    noun ** {cn = mkCN noun.cn (Extend.RelVPS which_RP predicates)} ; --- place of rel?
  StatClassNoun noun statement =
    noun ** {adv = concatAdv noun.adv (Syntax.mkAdv such_that_Subj statement)} ;

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

  DoesPredicate verb = verb ;
  IsPredicate adjective = mkVP adjective ;
  IsaPredicate notion = mkVP notion.cn ;
  HasPredicate notion = mkVP have_V2 (mkNP a_Det notion.cn) ;
  HasNoPredicate notion = mkVP have_V2 (mkNP no_Quant notion.cn) ;

-- 1.3.5
  SimpleStatement terms predicates = Extend.PredVPS terms predicates ;
  WeHaveSymbStatement sym = mkS (mkCl we_NP have_V2 <symb sym : NP>) ;
  WeHaveConstStatement const = mkS (mkCl we_NP have_V2 const) ;
  FormulaStatement formula = <symb (mkSymb formula.s) : S> ;
  
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

---- symbolic statements TODO

  ForStatement qns s = Grammar.ExtAdvS (Syntax.mkAdv for_Prep qns) s ;

--- simplicied from spec, which uses many levels
--- to resolve ambiguities: that can be misleading to an innocent
--- reader (and the use of primaryStatement seems to compromise this)

  AndStatement s t = mkS and_Conj s t ;
  OrStatement s t = mkS or_Conj s t ;
  IfStatement s t = Grammar.ExtAdvS (mkAdv if_Subj s) (mkS then_Adv t) ;
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

  NotionSynonym head target =
    letSynonym (mkNP a_Det head.cn) (mkNP a_Det target.cn) ;
  FunctionSynonym head target =
    letSynonym (symb head) target ;
---  PredicateSynonym head target = 
---    letSynonym (s2np head) (s2np (parenthS target)) ;
  

-- 1.5.1
  NamesAssumption names classnoun =
    mkText 
      (mkUtt (mkImp (Extend.ComplBareVS assume_VS
         (mkS (mkCl (namesNP names) (mkCN classnoun.cn classnoun.adv))))))
      fullStopPunct ;
      
  LetNamesAssumption names classnoun =
    mkText 
      (mkUtt (ImpP3 (namesNP names) (mkVP (mkCN classnoun.cn classnoun.adv))))
      fullStopPunct ;

  StatementAssumption stat =
    mkText 
      (mkUtt (mkImp (Extend.ComplBareVS assume_VS stat))) fullStopPunct ;

  FormulaAssumption formula =
    lin Text {s = let_Str ++ formula.s ++ "."} ;

  SectionToplevel header section = mkText header section ;

  EmptySection = emptyText ;
  AssumptionSection assumption section = mkText assumption section ;
  StatementSection statement section = mkText (mkUtt statement) section ;
  ThenStatementSection statement section = mkText (mkUtt (mkS then_Adv statement)) section ;
  DefinitionSection definition section = mkText (mkUtt definition) section ;

  ex_Header = lin Text {s = "ex ."} ; --- GFLean specific ?


--------------------------

oper
--- these work for many languages but can be overridden
  emptyAdv : Adv = lin Adv {s = ""} ;
  concatAdv : Adv -> Adv -> Adv = \a, b -> lin Adv {s = a.s ++ b.s} ;

  negPol = negativePol ;

  namesNP : [Name] -> NP = \xs -> case xs.isPlur of {
    True => pluralNP (symb xs.s) ;
    False => symb xs.s
    } ;

  pluralNP : NP -> NP = \np -> np ;  --- to be overridden for languages

  letSynonym : NP -> NP -> Text = \dum, dens ->
    mkText (Grammar.ImpP3 dum (mkVP denote_V2 dens)) ;

  s2np : S -> NP = \s -> symb (mkSymb (mkUtt s).s) ; --- hack; Forthel is not quite grammatical here

---  parenthS : S -> S = \s -> Markup.MarkupS (lin Mark {begin = "(" ; end = ")"}) s ;

}