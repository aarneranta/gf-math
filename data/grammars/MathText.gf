abstract MathText = MathWikidata ** {

flags startcat = Paragraph ;

cat
  Paragraph ;
  Definition ;
  Sentence ;
  Condition ;
  Kind ;
  Object ;
  Reference ;
  Property ;
  [Property] {2} ;
  Conjunction ;

fun
  ParDefinition : Definition -> Paragraph ;
  ParSentence : Sentence -> Paragraph ;

  DefIsA     : Kind -> Kind -> Definition ;
  DefIsASuch : Kind -> Kind -> Condition -> Definition ;
  DefIsAIf   : Kind -> Kind -> Condition -> Definition ;
  DefWhose   : Kind -> Kind -> Kind -> Property -> Definition ;

  CondIsA : Reference -> Kind -> Condition ;
  CondHasProp : Reference -> Property -> Condition ;
  
  SentIsA : Object -> Kind -> Sentence ;
  SentHasProp : Object -> Property -> Sentence ;

  RefIt : Reference ;
  RefIts : Kind -> Reference ;

---  ObjIts : Kind -> Object ;

  PropConj : Conjunction -> [Property] -> Property ;

  and_Conjunction : Conjunction ;
  or_Conjunction : Conjunction ;

-- using Wikidata
  KindQN : QN -> Kind ;


-- lexical items not in Wikidata

  commutative_Property : Property ;
  associative_Property : Property ;
  reflexive_Property : Property ;
  symmetric_Property : Property ;
  transitive_Property : Property ;

}