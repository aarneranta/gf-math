abstract ExtractEngAbs =
  MorphoDictEngAbs,
  Cat [NP, CN, AP, AdA]
--  Structural -- covered in MorphoDict

** {

flags startcat = Term ;

cat
  Term ;

fun
  NPTerm : NP -> Term ;
  APTerm : AP -> Term ;
  AdvTerm : Adv -> Term ;

  UseN : N -> CN ;
  AdjCN : AP -> CN -> CN ;
  CompoundN : N -> N -> N ;

  DefCN : CN -> NP ;
  DefPluralCN : CN -> NP ;
  IndefCN : CN -> NP ;
  PluralCN : CN -> NP ;
  BareCN : CN -> NP ;

  PositA : A -> AP ;
  AdAP : AdA -> AP -> AP ;

  PrepNP : Prep -> NP -> Adv ;

}