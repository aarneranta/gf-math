--# -path=.:morphodict

abstract ExtractFreAbs =
  MorphoDictFreAbs,
  MathWordsFreAbs, -- initially empty
  Structural

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
  IntCompoundCN : Int -> CN -> CN ;
  NameCompoundCN : PN -> CN -> CN ;
  NounIntCN : CN -> Int -> CN ;
  NounPrepCN : CN -> Adv -> CN ;
  NounGenCN : CN -> NP -> CN ;

  DefCN : CN -> NP ;
  DefPluralCN : CN -> NP ;
  IndefCN : CN -> NP ;
  IndefPluralCN : CN -> NP ;
  BareCN : CN -> NP ;

  PositA : A -> AP ;
  AdAP : AdA -> AP -> AP ;
  AAdAP : A -> AP -> AP ;

  PrepNP : Prep -> NP -> Adv ;

}