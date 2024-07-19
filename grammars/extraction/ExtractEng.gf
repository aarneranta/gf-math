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
  (R=ResEng),
  Prelude
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
  AAdAP a ap = mkAP (lin AdA (mkAdv a)) ap ;
  PastPartAP v = mkAP (lin A {s = table (R.AForm) {_ => v.s ! R.VPPart} ; isPre = True ; isMost = True}) ;

  PrepNP prep np = mkAdv prep np ;

oper
  -- not used for parsing, but as replacements of Int-functions when used as lin terms, as a work-around to GF type checking
  nounStrCN : CN -> Str -> CN = \cn, s -> mkCN cn (symb s) ;
  strCompoundCN : Str -> CN -> CN = \s, cn -> prefixCN (s ++ hyphen) cn ;

  -- a back-up oper for unanalysed terms
  rawTerm : Str -> Term = \s ->
    lin Term (lin Utt {s = s}) ; ---

  prefixCN : Str -> CN -> CN = \s, cn ->
    cn ** {s = \\n, c => s ++ cn.s ! n ! c} ;

  hyphen : Str = Predef.BIND ++ "-" ++ Predef.BIND ;

}