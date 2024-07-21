--# -path=.:prop

abstract MathText =
  Prop - [VSymb, IUniv, IExist],
  Term,
  MathWikidata
  ** {

flags startcat = Paragraph ;

cat
  Paragraph ;
  Definition ;
  Condition ;
  Hypothesis ;
  [Hypothesis] ;

fun
  ParDefinition : [Hypothesis] -> Definition -> Paragraph ;
  ParStatement  : [Hypothesis] -> Prop -> Paragraph ;

  DefIsA     : QN -> Kind -> Definition ;               -- an abelian grouo is a group
  DefIsASuch : QN -> Kind -> Condition -> Definition ;  -- an ab. grouo is a group such that...
  DefIsAIf   : QN -> Kind -> Condition -> Definition ;  -- a group is an ab. group if...
  DefWhose   : QN -> Kind -> Fun1 -> Pred1 -> Definition ; -- an ab. group is a group whose...

  CondIsA : Kind -> Condition ;
  CondPred1 : Pred1 -> Condition ;
  CondItsFun1 : Fun1 -> Pred1 -> Condition ;

  HypTyping : [Var] -> Kind -> Hypothesis ;
  
-- using Wikidata
  KindQN : QN -> Kind ;
  Fun1QN : QN -> Fun1 ;

-- using Term
  PEquation : Equation -> Prop ;
  ITerm : Term -> Ind ;


-- lexical items not in Wikidata

  commutative_Property : Pred1 ;
  associative_Property : Pred1 ;
  reflexive_Property : Pred1 ;
  symmetric_Property : Pred1 ;
  transitive_Property : Pred1 ;

}