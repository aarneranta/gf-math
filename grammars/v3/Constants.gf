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
  Nat : Noun ;
  Set : Noun ;
  Even : Adj ;
  Odd : Adj ;
  Prime : Adj ;
  Zero : Name ;

}