--# -path=.:present:prop:term:wikidata

concrete MathTextLean of MathText = PropLean - [IUniv, IExist], MathWikidataCore, TermAscii **

open
  Prelude, Formal

in {

lincat
  Paragraph, 
  Condition,
  Hypothesis,
  [Hypothesis],
  [Variable]
    = Str ;
  Definition = {dfum, dfens : Str} ;

oper
  suchThat : (x, a, b : Str) -> Str = \x, a, b ->
    "{" ++ x ++ ":" ++ a ++ "//" ++ b ++ "}" ;

  apply : Str -> Str -> Str = \f, a -> "(" ++ f ++ a ++ ")" ;

lin
  ParDefinition hs d = "def" ++ d.dfum ++ hs ++ ":=" ++ d.dfens ;
  ParStatement hs d = "example" ++ hs ++ ":" ++ top d ++ ":=" ++ "sorry" ;

  DefIsA a b = {dfum = a.s ; dfens = b} ;
  DefIsASuch a b c = {dfum = a.s ; dfens = suchThat "x" b (apply c "x")} ;
  DefIsAIf a b c = {dfum = a.s ; dfens = suchThat "x" b (apply c "x")} ;
  DefWhose a b f c = {dfum = a.s ; dfens = suchThat "x" b (apply (c ! True) (appLatex (top f) "x"))} ;

  CondIsA b = b ;
  CondPred1 b = b ! True;
  CondItsFun1 f b = "(" ++ b ! True ++ "∘" ++ top f ++ ")" ;

  HypTyping xs k = parenth (xs ++ ":" ++ k) ;
  
  BaseHypothesis = "" ;
  ConsHypothesis h hs = h ++ hs ; 

  BaseVariable x = x ;
  ConsVariable x xs = x ++ "," ++ xs ;

-- using Wikidata
  KindQN qn = qn.s ;
  Fun1QN qn = constant qn.s ;

-- using Term
  PEquation e = constant e.s ;
  ITerm t = t ;

-- lexical items not in Wikidata
lin
  commutative_Property = slash "Commutative" ;
  associative_Property = slash "Associative" ;
  reflexive_Property = slash "Reflexive" ;
  symmetric_Property = slash "Symmetric" ;
  transitive_Property = slash "Transitive" ;

  
}