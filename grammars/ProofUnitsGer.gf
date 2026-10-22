concrete ProofUnitsGer of ProofUnits = CategoriesGer **

-- the functions that proof texts (-proof-text) are made of

open
  SyntaxGer,
  ParadigmsGer,
  (P=ParadigmsGer),
  UtilitiesGer

in {

lin
  HyposUnit hypos = hypos.text ;
  ConclusionUnit prop = mkText (topProp prop) ;

lincat
  [Unit] = Text ;
  Hence = Adv ;

lin
  noHence = P.mkAdv "" ;
  henceHence = P.mkAdv "also" ;
  thusHence = P.mkAdv "somit" ;
  thenHence = P.mkAdv "dann" ;
  altogetherHence = P.mkAdv "insgesamt" ;
  afortioriHence = P.mkAdv "erst recht" ;
  inParticularHence = P.mkAdv "insbesondere" ;
  weConcludeHence = P.mkAdv "wir schließen, dass" ;

  BaseUnit = emptyText ;
  ConsUnit unit units = mkText unit units ;

}
