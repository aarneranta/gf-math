--# -path=.:present:prop:term:wikidata

concrete MathTextCore of MathText = PropCore, MathWikidataCore, TermAscii **

open
  Prelude, Formal

in {

lincat
  Paragraph, 
  Definition,
  Condition,
  Hypothesis,
  [Hypothesis],
  [Variable]
    = Str ;

oper
  suchThat : (x, a, b : Str) -> Str = \x, a, b ->
    "(Σ" ++ x ++ ":" ++ a ++ ")" ++ b ;

  apply : Str -> Str -> Str = \f, a -> f ++ "(" ++ a ++ ")" ;

lin
  ParDefinition hs d = hs ++ "|-" ++ d ;
  ParStatement hs d = hs ++ "|-" ++ top d ;

  DefIsA a b = a ++ ":=" ++ b ;
  DefIsASuch a b c = a ++ ":=" ++ suchThat "x" b (apply c "x") ;
  DefIsAIf a b c = a ++ ":=" ++ suchThat "x" b (apply c "x") ;
  DefWhose a b f c = a ++ ":=" ++ suchThat "x" b (apply (c ! True) (appLatex (top f) "x")) ;

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