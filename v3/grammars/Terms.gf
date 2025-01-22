-- mathematical terms as they appear in "normal" mathematical text

abstract Terms = {

cat
  Formula ;
  Equation ;
  Eqsign ;
  Term ;
  [Term] {1} ;
  Var ;
  Constant ;
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

  TVar : Var -> Term ;
  TConstant : Constant -> Term ;
  TNumber : Float -> Term ;

  FVar : Var -> Function ;
  FDerivative : Function -> Function ;

  N_Constant, Z_Constant, Q_Constant, R_Constant, C_Constant : Constant ;

  stringVar : String -> Var ;
  
  TFrac : Term -> Term -> Term ;
  TAbsolute : Term -> Term ;
  TComprehension : Term -> Term -> Formula -> Term ;
  TPositive : Term -> Term ; -- R^+
  TNegative : Term -> Term ;

  TextbfTerm : Term -> Term ;
}
