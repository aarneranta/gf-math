--# -path=.:morphodict

-- make a symlink to morphodict

concrete ExtractGer of ExtractGerAbs =
  MorphoDictGer,
  MathWordsGer,
  StructuralGer

** open
  SyntaxGer,
  (E=ExtendGer)
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

  DefCN cn = mkNP the_Det cn ;
  DefPluralCN cn = mkNP thePl_Det cn ;
  IndefCN cn = mkNP a_Det cn ;
  IndefPluralCN cn = mkNP aPl_Det cn ;
  BareCN cn = mkNP cn ;

  PositA a = mkAP a ;
  AdAP ad ap = mkAP ad ap ;

  PrepNP prep np = mkAdv prep np ;

}