concrete MathTextCore of MathText = MathWikidataCore **

open
  Prelude

in {

lincat
  Paragraph = {s : Str} ;
  Definition =  {s : Str} ;
  Sentence = {s : Str} ;
  Condition =  {s : Str} ;
  Kind =  {s : Str} ;
  Object = {s : Str} ;
  Reference = {s : Str} ; 
  Property =  {s : Str} ;
  [Property] =  {s : Str} ;
  Conjunction =  {s : Str} ;

oper
  suchThat : (x, a, b, e : Str) -> Str = \x, a, b, e ->
    "(Σ" ++ x ++ ":" ++ a ++ ")" ++ apply b e ;

  apply : Str -> Str -> Str = \f, a -> "(" ++ f ++ a ++ ")" ;

  proj : Str -> Str -> Str = \f, a -> f ++ "." ++ a ;

  list : Str -> Str = \s -> "[" ++ s ++ "]" ;

lin
  ParDefinition d = {s = "def" ++ d.s} ;
  ParSentence d = {s = "theorem" ++ d.s} ;

  DefIsA a b = {s = a.s ++ ":=" ++ b.s} ;
  DefIsASuch a b c = {s = a.s ++ ":=" ++ suchThat "x" b.s c.s "x"} ;
  DefIsAIf a b c = {s = b.s ++ ":=" ++ suchThat "x" a.s c.s "x"} ;
  DefWhose a b c d =  {s = b.s ++ ":=" ++ suchThat "x" a.s d.s (apply (proj b.s c.s) "x")} ;

  CondIsA r b = {s = r.s ++ b.s} ;
  CondHasProp r b = {s = r.s ++ b.s} ;

  RefIt = {s = ""} ;
  RefIts k =  {s = ""} ;

--  SentIsA a b = mkCl a b ;
--  SentHasProp a b = mkCl a b ;

  PropConj conj props = {s = apply conj.s (list props.s)} ;

  and_Conjunction = {s = "∧"} ;
  or_Conjunction = {s = "∨"} ;

  BaseProperty a b = {s = a.s ++ "," ++  b.s} ;
  ConsProperty a bs = {s = a.s ++ "," ++  bs.s} ;

-- using Wikidata
  KindQN qn = qn ;

-- lexical items not in Wikidata
lin
  commutative_Property = {s = "Commutative"} ;
  associative_Property = {s = "Associative"} ;
  reflexive_Property = {s = "Reflexive"} ;
  symmetric_Property = {s = "Symmetric"} ;
  transitive_Property = {s = "Transitive"} ;

  
}