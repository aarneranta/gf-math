concrete ProofUnitsCze of ProofUnits = CategoriesCze **

-- the functions that proof texts (-proof-text) are made of

open
  SyntaxCze,
  ParadigmsCze,
  (P=ParadigmsCze),
  UtilitiesCze

in {

lin
  HyposUnit hypos = hypos.text ;
  ConclusionUnit prop = mkText (topProp prop) ;

lincat
  [Unit] = Text ;
  Hence = Adv ;

lin
  noHence = P.mkAdv "" ;
  henceHence = P.mkAdv "tedy" ;
  thusHence = P.mkAdv "tudíž" ;
  thenHence = P.mkAdv "pak" ;
  altogetherHence = P.mkAdv "celkem" ;
  afortioriHence = P.mkAdv "tím spíše" ;
  inParticularHence = P.mkAdv "zejména" ;
  weConcludeHence = P.mkAdv "dostáváme, že" ;

  BaseUnit = emptyText ;
  ConsUnit unit units = mkText unit units ;

}
