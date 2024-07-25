--# -path=../extraction:../forthel:../mathterms:../extraction/morphodict

concrete WikiMathEng of WikiMath =
  ForthelEng,
  MathTermsEng ** {

lin
  WikiPrimClass cn = mkPrimClass cn ;
  WikiAdjective ap = ap ;

}