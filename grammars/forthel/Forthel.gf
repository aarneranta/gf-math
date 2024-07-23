-- based on http://nevidal.org/download/forthel.pdf

abstract Forthel = ForthelTerms ** {

cat
  Toplevel ;
  Section ;
  Header ;
  Sentence ;
  Assumption ;
  Synonym ;
  Definition ; -- separate from Statement, to avoid ambiguities
  Statement ;
  Predicate ;
  Term ;
  Notion ;

  Terms ; -- subject of primary statement
  Predicates ;
  Notions ; -- complement of there exist(s)
  PrimClass ;

  ClassNoun ;
  DefiniteNoun ;
  QuantifiedNotion ;
  QuantifiedNotions ;
  PlainTerm ;  -- no quantifiers
  Adjective ;  --- these two are one cat in 1.3.1
  Verb ;
  Constant ; -- constant statement

  PredicateHead ;

---  Operator ; -- symbolic
---  Relation ;
  SymbTerm ;
  Name ;
  [Name] {1} ; 

fun
-- importing from ForthelTerms

  FormulaSymbTerm : Formula -> SymbTerm ;
  ExpressionSymbTerm : Exp -> SymbTerm ;
  VarName : Var -> Name ;


-- 1.3.2
  PrimClassNoun : PrimClass -> ClassNoun ;
  ClassNounNotion : ClassNoun -> Notion ;
  ClassNounNamesNotion : ClassNoun -> [Name] -> Notion ;

---  NameSymbTerm : Name -> SymbTerm ;
----  StringName : String -> Name ; ---- replaced by VariableName

  AdjClassNoun : ClassNoun -> Adjective -> ClassNoun ; -- both left and right
  RelClassNoun : ClassNoun -> Predicates -> ClassNoun ; -- some of right
  StatClassNoun : ClassNoun -> Statement -> ClassNoun ;

-- 1.3.3
  EveryTerm : Notion -> QuantifiedNotion ;  --- can only take Sg
  EachTerm  : Notion -> QuantifiedNotion ;

  AllTerm   : Notion -> QuantifiedNotion ;  -- can take both Sg and Pl
  AnyTerm   : Notion -> QuantifiedNotion ;
  SomeTerm  : Notion -> QuantifiedNotion ;
  NoTerm    : Notion -> QuantifiedNotion ;

  OneQuantifiedNotions : QuantifiedNotion -> QuantifiedNotions ;
  AddQuantifiedNotions : QuantifiedNotion -> QuantifiedNotions -> QuantifiedNotions ;
  
  QuantifiedTerm : QuantifiedNotions -> Term ;

  PlainTermTerm : PlainTerm -> Term ; -- no quantifiers
  
  SymbPlainTerm : SymbTerm -> PlainTerm ;

  DefiniteSgNounTerm : DefiniteNoun -> PlainTerm ;
  DefinitePlNounTerm : DefiniteNoun -> PlainTerm ;

-- 1.3.4
-- reading nonterminals doesPredicate etc as funs, not cats

  DoesPredicate : Verb -> Predicate ;
  IsPredicate : Adjective -> Predicate ;
  IsaPredicate : Notion -> Predicate ; --- not notion in doc 
  HasPredicate : Notion -> Predicate ; --- hence we may overgenerate
  HasNoPredicate : Notion -> Predicate ;


-- 1.3.5
--- intervening categories as functions again

  SimpleStatement : Terms -> Predicates -> Statement ;

  ThereIsStatement : Notions -> Statement ;
  ThereIsNoStatement : Notion -> Statement ;
  WeHaveSymbStatement : SymbTerm -> Statement ;
  WeHaveConstStatement : Constant -> Statement ;
  FormulaStatement : Formula -> Statement ; --- own shortcut

  PosOnePredicates : Predicate -> Predicates ;
  NegOnePredicates : Predicate -> Predicates ;
  PosAddPredicates : Predicate -> Predicates -> Predicates ;
  NegAddPredicates : Predicate -> Predicates -> Predicates ;

  OneTerms : Term -> Terms ;
  AddTerms : Term -> Terms -> Terms ;

  OneNotions : Notion -> Notions ;
  AddNotions : Notion -> Notions -> Notions ;

---- symbolic statements TODO

  ForStatement : QuantifiedNotions -> Statement -> Statement ;

--- simplicied from spec, which uses many levels
--- to resolve ambiguities: that can be misleading to an innocent
--- reader (and the use of primaryStatement seems to compromise this)

  AndStatement : Statement -> Statement -> Statement ;
  OrStatement : Statement -> Statement -> Statement ;
  IfStatement : Statement -> Statement -> Statement ;
  IffStatement : Statement -> Statement -> Statement ;

-- 1.3.6

  NotionDefinition :  Notion -> Notion -> Definition ;
  FunctionDefinition : DefiniteNoun -> PlainTerm -> Definition ;
  FunctionIsEqualDefinition : DefiniteNoun -> PlainTerm -> Definition ;
  PredicateDefinition : PredicateHead ->  Statement -> Definition ;

  AdjectivePredicateHead : Adjective -> [Name] -> PredicateHead ;
  VerbPredicateHead : Verb -> [Name] -> PredicateHead ;

--- spec p. 15: "Note that synonym declarations are not statements but instructions to the
--- parser, just like token groups (see Section 1.3.1). Roughly, synonyms relate
--- to defined symbols as preprocessor macros relate to subroutines
--- in a programming language."

  NotionSynonym : Notion -> PrimClass -> Synonym ;  --- in spec: notionPattern classNoun
  FunctionSynonym : SymbTerm -> PlainTerm -> Synonym ; -- in spec: functionPattern
  PredicateSynonym : PredicateHead -> Statement -> Synonym ;


-- 1.5.1
  NamesAssumption : [Name] -> ClassNoun -> Assumption ; --- cannot find a rule, only examples
  LetNamesAssumption : [Name] -> ClassNoun -> Assumption ; --- cannot find a rule, only examples
  StatementAssumption : Statement -> Assumption ;
  FormulaAssumption : Formula -> Assumption ;  --- own shortcut

  SectionToplevel : Header -> Section -> Toplevel ;

  EmptySection : Section ;
  AssumptionSection : Assumption -> Section -> Section ;
  StatementSection : Statement -> Section -> Section ;
  ThenStatementSection : Statement -> Section -> Section ;
  DefinitionSection : Definition -> Section -> Section ;

  ex_Header : Header ;  --- GFLean specific ?

}
