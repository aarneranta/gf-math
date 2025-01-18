concrete ConstantBaseEng of ConstantBase =

open
  SyntaxEng,
  ParadigmsEng,
  SymbolicEng

in {

lincat
  Noun = CN ;
  Adj = AP ;
  Rel = RelationT ;
  Name = NP ;
  Fun = FunctionT ;
  Label = NP ;

oper
  RelationT : Type = {ap : AP ; prep : Prep} ;
  FunctionT : Type = {cn : CN ; prep : Prep} ;

  mkNoun = overload {
    mkNoun : Str -> CN
      = \s -> mkCN (mkN s) ;
    mkNoun : Str -> Str -> CN
      = \a, n -> mkCN (mkA a) (mkN n) ;
    } ;
    
  mkFun = overload {
    mkFun : Str -> FunctionT
      = \s -> {cn = mkCN (mkN s) ; prep = possess_Prep} ;
    mkFun : (a, n : Str) -> FunctionT
      = \a, n -> {cn = mkCN (mkA a) (mkN n) ; prep = possess_Prep} ;
    mkFun2 : (a, b, n : Str) -> FunctionT
      = \a, b, n -> {cn = mkCN (mkA a) (mkCN (mkA b) (mkN n)) ; prep = possess_Prep} ;
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

  mkLabel = overload {
    mkLabel : Str -> NP
      = \s -> mkNP (mkPN s)
    } ;

  latexName : Str -> NP
      = \s -> symb (mkSymb ("$" ++ s ++ "$")) ;


}