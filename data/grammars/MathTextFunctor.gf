incomplete concrete MathTextFunctor of MathText = MathWikidata **

open
  Syntax

in {

lincat
  Paragraph = Text ;
  Definition = S ;
  Sentence = Cl ;
  Object = NP ;
  

lin
  ParDefinition d = mkText d ;

  DefIsA a b = mkS (mkCl (mkNP a_Det a) b) ;
  DefIsASuch a b c = mkS (mkCl (mkNP a_Det a) (mkCN b (mkRS (mkRCl c)))) ;
  DefIsAIf a b c = mkS (mkCl (mkNP a_Det a) (mkCN b (mkAdv if_Subj (mkS c)))) ;

  SentIsA a b = mkCl a b ;

  ObjIts a = mkNP it_Pron a ;
  
}