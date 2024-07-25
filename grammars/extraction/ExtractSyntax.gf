abstract ExtractSyntax = Structural ** {

flags startcat = MT ;

cat
  MT ;

fun
  NPMT : NP -> MT ;
  APMT : AP -> MT ;
  AdvMT : Adv -> MT ;

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
  PastPartAP : V -> AP ;

  PrepNP : Prep -> NP -> Adv ;

}