--# -path=.:morphodict

-- make a symlink to morphodict

concrete ExtractGer of ExtractGerAbs =
  MorphoDictGer,
  MathWordsGer,
  StructuralGer

** open
  SyntaxGer,
  SymbolicGer,
  (E=ExtendGer),
  (P=ParadigmsGer)

  in {

lincat
  Term = Utt ;

lin
  NPTerm np = mkUtt np ;
  APTerm ap = mkUtt ap ;
  AdvTerm adv = mkUtt adv ;

  UseN n = mkCN n ;
  AdjCN ap cn = mkCN ap cn ;
  CompoundN x y = E.CompoundN x y ;
  IntCompoundCN i x = mkCN 
    (hyphenPrefixA (i.s)) x ;
  NameCompoundCN n x = mkCN
    (hyphenPrefixA (mkUtt (mkNP n)).s) x ;
  NounIntCN cn i = mkCN cn (symb i) ;
  NounPrepCN cn adv = mkCN cn adv ;
  NounGenCN cn np = mkCN cn (mkAdv P.genPrep np) ;
  
  DefCN cn = mkNP the_Det cn ;
  DefPluralCN cn = mkNP thePl_Det cn ;
  IndefCN cn = mkNP a_Det cn ;
  IndefPluralCN cn = mkNP aPl_Det cn ;
  BareCN cn = mkNP cn ;

  PositA a = mkAP a ;
  AdAP ad ap = mkAP ad ap ;
  AAdAP a ap = mkAP (lin AdA (mkAdv a)) ap ;

  PrepNP prep np = mkAdv prep np ;

oper
  nounStrCN : CN -> Str -> CN = \cn, s -> mkCN cn (symb s) ;
  strCompoundCN : Str -> CN -> CN = \s, cn -> mkCN (hyphenPrefixA s) cn ;

  hyphenPrefixA : Str -> A = \s ->
--    P.invarA (s ++ Predef.BIND ++ "-" ++ Predef.BIND) ;
    P.invarA (s ++ "-") ; -- needs insertion of spaces 

}