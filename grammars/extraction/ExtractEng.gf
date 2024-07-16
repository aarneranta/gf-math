--# -path=.:morphodict

-- make a symlink to morphodict

concrete ExtractEng of ExtractEngAbs =
  MorphoDictEng,
  MathWordsEng
--  StructuralEng

** open
  SyntaxEng,
  SymbolicEng,
  (E=ExtendEng),
  (P=ParadigmsEng),
  (R=ResEng)
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
  IntCompoundCN i x = prefixCN (i.s ++ hyphen) x ;
  NameCompoundCN n x = prefixCN (mkUtt (mkNP n)).s x ;
  NounIntCN cn i = mkCN cn (symb i) ;
  NounPrepCN cn adv = mkCN cn adv ;
  NounGenCN cn np = prefixCN (mkUtt (mkNP (E.GenNP np))).s cn ;

  DefCN cn = mkNP the_Det cn ;
  DefPluralCN cn = mkNP thePl_Det cn ;
  IndefCN cn = mkNP a_Det cn ;
  IndefPluralCN cn = mkNP aPl_Det cn ;
  BareCN cn = mkNP cn ;

  PositA a = mkAP a ;
  AdAP ad ap = mkAP ad ap ;

  PrepNP prep np = mkAdv prep np ;

oper
  prefixCN : Str -> CN -> CN = \s, cn ->
    cn ** {s = \\n, c => s ++ cn.s ! n ! c} ;

  hyphen : Str = Predef.BIND ++ "-" ++ Predef.BIND ;

}