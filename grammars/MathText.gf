--# -path=.:prop

abstract MathText = Prop, MathWikidata ** {

flags startcat = Paragraph ;

cat
  Paragraph ;
  Definition ;
  Condition ;

fun
  ParDefinition : Definition -> Paragraph ;
  ParProp : Prop -> Paragraph ;

  DefIsA     : Kind -> Kind -> Definition ;               -- an abelian grouo is a group
  DefIsASuch : Kind -> Kind -> Condition -> Definition ;  -- an ab. grouo is a group such that...
  DefIsAIf   : Kind -> Kind -> Condition -> Definition ;  -- a group is an ab. group if...
  DefWhose   : Kind -> Kind -> Fun1 -> Pred1 -> Definition ; -- an ab. group is a group whose...

  CondIsA : Kind -> Condition ;
  CondPred1 : Pred1 -> Condition ;
  CondItsFun1 : Fun1 -> Pred1 -> Condition ;
  
-- using Wikidata
  KindQN : QN -> Kind ;
  Fun1QN : QN -> Fun1 ;


-- lexical items not in Wikidata

  commutative_Property : Pred1 ;
  associative_Property : Pred1 ;
  reflexive_Property : Pred1 ;
  symmetric_Property : Pred1 ;
  transitive_Property : Pred1 ;

}