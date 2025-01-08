concrete ConstantBaseEng of ConstantBase =

open
  SyntaxEng,
  ParadigmsEng,
  SymbolicEng

in {

lincat
  Noun = CN ;
  Adj = AP ;
  Rel = {ap : AP ; prep : Prep} ;
  Name = NP ;

oper
  mkNoun = overload {
    mkNoun : Str -> CN
      = \s -> mkCN (mkN s) ;
    mkNoun : Str -> Str -> CN
      = \a, n -> mkCN (mkA a) (mkN n) ;
    } ;
    
  mkAdj = overload {
    mkAdj : Str -> AP
      = \s -> mkAP (mkA s) ;
    } ;
    
  mkRel = overload {
    mkRel : Str -> Str -> {ap : AP ; prep : Prep}
      = \s, p -> {ap = mkAP (mkA s) ; prep = mkPrep p}
    } ;
    
  mkName = overload {
    mkName : Str -> NP
      = \s -> mkNP (mkPN s)
    } ;

  latexName : Str -> NP
      = \s -> symb (mkSymb ("$" ++ s ++ "$")) ;


}