concrete ConstantsEng of Constants =

open
  SyntaxEng,
  ParadigmsEng,
  SymbolicEng

in {

lincat
  Exp = NP ;
  Kind = {cn : CN ; adv : Adv} ;
  Prop = S ;
  Noun = CN ;
  Adj = AP ;
  Name = NP ;

lin
  AdjProp adj exp = mkS (mkCl exp adj) ;
  NounKind noun = {cn = noun ; adv = lin Adv {s = []}} ;
  NameExp name = name ;

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
    
  mkName = overload {
    mkName : Str -> NP
      = \s -> mkNP (mkPN s)
    } ;

  latexName : Str -> NP
      = \s -> symb (mkSymb ("$" ++ s ++ "$")) ;


-- lexicon
---- TODO: dynamically generated file

lin
  Nat = mkNoun "natural" "number" ;
  Set = mkNoun "set" ;
  Even = mkAdj "even" ;
  Odd = mkAdj "odd" ;
  Prime = mkAdj "prime" ;
  Zero = mkName "0" ;


}