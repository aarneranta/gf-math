concrete ConstantsEng of Constants =

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


-- lexicon
---- TODO: dynamically generated file

lin
  DkNat = mkNoun "natural" "number" ;
  DkSet = mkNoun "set" ;
  DkEven = mkAdj "even" ;
  DkOdd = mkAdj "odd" ;
  DkPrime = mkAdj "prime" ;
  DkZero = latexName "0" ;
  DkDiv = mkRel "divisible" "by" ;


}