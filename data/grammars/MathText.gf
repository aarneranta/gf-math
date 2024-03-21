--# -path=.:prop

abstract MathText = MathWikidata, Prop ** {

flags startcat = Paragraph ;

cat
  Paragraph ;
  Definition ;
--  Sentence ;
  Condition ;
  Object ;
  Reference ;
--  Property ;
--  [Property] {2} ;
--  Conjunction ;

fun
  ParDefinition : Definition -> Paragraph ;
--  ParSentence : Sentence -> Paragraph ;

  DefIsA     : Kind -> Kind -> Definition ;
  DefIsASuch : Kind -> Kind -> Condition -> Definition ;
  DefIsAIf   : Kind -> Kind -> Condition -> Definition ;
  DefWhose   : Kind -> Kind -> Kind -> Pred1 -> Definition ;

  CondIsA : Reference -> Kind -> Condition ;
  CondHasProp : Reference -> Pred1 -> Condition ;
  
--  SentIsA : Object -> Kind -> Sentence ;
--  SentHasProp : Object -> Pred1 -> Sentence ;

  RefIt : Reference ;
  RefIts : Kind -> Reference ;

---  ObjIts : Kind -> Object ;

--  PropConj : Conjunction -> [Property] -> Property ;

--  and_Conjunction : Conjunction ;
--  or_Conjunction : Conjunction ;

-- using Wikidata
  KindQN : QN -> Kind ;

-- using Prop
  ParProp : Prop -> Paragraph ;

-- lexical items not in Wikidata

  commutative_Property : Pred1 ;
  associative_Property : Pred1 ;
  reflexive_Property : Pred1 ;
  symmetric_Property : Pred1 ;
  transitive_Property : Pred1 ;

}