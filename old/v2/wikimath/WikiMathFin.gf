--# -path=../extraction:../forthel:../mathterms:../extraction/morphodict

concrete WikiMathFin of WikiMath =
  ForthelFin,
  MathTermsFin,
  DerivedMathTermsFin ** {

lin
  WikiPrimClass cn = mkPrimClass cn ;
  WikiAdjective ap = ap ;

}