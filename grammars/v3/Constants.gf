abstract Constants = {

cat
  Exp ;
  Kind ;
  Prop ;
  Noun ;
  Adj ;
  Name ;

fun
  AdjProp : Adj -> Exp -> Prop ;
  NounKind : Noun -> Kind ;
  NameExp : Name -> Exp ;

-- lexicon
---- TODO: dynamically generated file

fun
  DkNat : Noun ;
  DkSet : Noun ;
  DkEven : Adj ;
  DkOdd : Adj ;
  DkPrime : Adj ;
  DkZero : Name ;

}