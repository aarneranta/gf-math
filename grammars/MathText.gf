--# -path=.:prop

abstract MathText =
  Prop - [VSymb],
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
  [Variable] {1} ;

fun
  ParDefinition : [Hypothesis] -> Definition -> Paragraph ;
  ParStatement  : [Hypothesis] -> Prop -> Paragraph ;

  DefIsA     : Kind -> Kind -> Definition ;               -- an abelian grouo is a group
  DefIsASuch : Kind -> Kind -> Condition -> Definition ;  -- an ab. grouo is a group such that...
  DefIsAIf   : Kind -> Kind -> Condition -> Definition ;  -- a group is an ab. group if...
  DefWhose   : Kind -> Kind -> Fun1 -> Pred1 -> Definition ; -- an ab. group is a group whose...

  CondIsA : Kind -> Condition ;
  CondPred1 : Pred1 -> Condition ;
  CondItsFun1 : Fun1 -> Pred1 -> Condition ;

  HypTyping : [Variable] -> Kind -> Hypothesis ;
  
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