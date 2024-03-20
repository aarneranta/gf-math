abstract MathText = MathWikidata ** {

flags startcat = Paragraph ;

cat
  Paragraph ;
  Definition ;
  Sentence ;
  Object ;
  

fun
  ParDefinition : Definition -> Paragraph ;

  DefIsA     : QN -> QN -> Definition ;
  DefIsASuch : QN -> QN -> Sentence -> Definition ;
  DefIsAIf   : QN -> QN -> Sentence -> Definition ;

  SentIsA : Object -> QN -> Sentence ;

  ObjIts : QN -> Object ;
  
}