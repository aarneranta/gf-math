incomplete concrete MathTextFunctor of MathText = MathWikidata, Prop **

open
  Syntax,
  Extend

in {

lincat
  Paragraph = Text ;
  Definition = S ;
--  Sentence = Cl ;
  Condition = {s : Agreement => Cl} ;
  Object = NP ;
  Reference = {s : Agreement => NP} ;
--  Property = AP ;
--  [Property] = [AP] ;
--  Conjunction = Conj ;

oper
  Agreement : PType = {} ;
  refPron : Agreement -> Pron = \_ -> it_Pron ;
  kindAgr : CN -> Agreement = \_ -> <> ;

  genRP : CN -> RP = \cn -> GenRP singularNum cn ;

lin
  ParDefinition d = mkText d ;
--  ParSentence d = mkText (mkS d) ;

  DefIsA a b = mkS (mkCl (mkNP a_Det a) b) ;
  DefIsASuch a b c = mkS (mkCl (mkNP a_Det a) (mkCN b (mkRS (mkRCl (c.s ! kindAgr b))))) ;
  DefIsAIf a b c = mkS (mkCl (mkNP a_Det b) (mkCN a (Syntax.mkAdv if_Subj (mkS (c.s ! kindAgr b))))) ;
  DefWhose a b c d = mkS (mkCl (mkNP a_Det a) (mkCN b (mkRS (mkRCl (genRP c) (mkVP d))))) ;

  CondIsA r b = {s = \\a => mkCl (r.s ! a) b} ;
  CondHasProp r b = {s = \\a => mkCl (r.s ! a) b} ;

  RefIt = {s = \\a => mkNP (refPron a)} ;
  RefIts k =  {s = \\a => mkNP (refPron a) k} ;

--  SentIsA a b = mkCl a b ;
--  SentHasProp a b = mkCl a b ;

--  PropConj conj props = mkAP conj props ;

--  and_Conjunction = and_Conj ;
--  or_Conjunction = or_Conj ;

--  BaseProperty a b = mkListAP a b ;
--  ConsProperty as = mkListAP as ;

-- using Wikidata
  KindQN qn = qn ;

-- using Prop
  ParProp p = mkText p.s ;

}