-- mathematical terms as they appear in "normal" mathematical text

abstract Terms = {

cat
  Formula ;
  Equation ;
  Eqsign ;
  Term ;
  [Term] {1} ;
  Ident ;
  Function ;

fun
  FEquation : Equation -> Formula ;
  FElem : [Term] -> Term -> Formula ;

  EChain : Eqsign -> Term -> Equation -> Equation ;
  EBinary : Eqsign -> Term -> Term -> Equation ;

  TParenth : Term -> Term ; -- extra parentheses

  TTimes, TExp : Term -> Term -> Term ;
  TNeg : Term -> Term ;
  TApp : Function -> [Term] -> Term ;

  TIdent : Ident -> Term ;
  TNumber : Float -> Term ;

  FIdent : Ident -> Function ;
  FDerivative : Function -> Function ;

  StrIdent : String -> Ident ;
  
  TFrac : Term -> Term -> Term ;
  TAbsolute : Term -> Term ;
  TComprehension : Term -> Term -> Formula -> Term ;
  TPositive : Term -> Term ; -- R^+
  TNegative : Term -> Term ;

  TextbfTerm : Term -> Term ;
}
