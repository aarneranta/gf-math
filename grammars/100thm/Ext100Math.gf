abstract Ext100Math =
  WikiMath
  ** {

flags startcat = Toplevel ;

------------ extensions for the 100 theorems ----------------

fun
  PostForStatement : Terms -> Statement -> Statement ;
  LatexIndexedStatement : Int -> Statement ;
  ClassNounIndexedNotion : ClassNoun -> Int -> Notion ;
  PrimClassOfDefNoun : PrimClass -> ClassNoun -> DefiniteNoun ;
  ClassOfDefiniteNoun : CN -> Terms -> DefiniteNoun ;
  ClassOfClassNoun : CN -> Term -> ClassNoun ;
  ClassFromToClassNoun : CN -> Term -> Term -> ClassNoun ;
  ClassFromOntoClassNoun : CN -> Term -> Term -> ClassNoun ;
  LatexNamesAssumption : [Name] -> ClassNoun -> Assumption ;
  AllSymbTerm : SymbTerm -> Term ;
  IsThePredicate : DefiniteNoun -> Predicate ;

  equinumerous_AP : AP ;
  surjection_CN : CN ;
  powerset_CN : CN ;

}