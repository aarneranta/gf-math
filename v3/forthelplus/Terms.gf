-- mathematical terms as they appear in "normal" mathematical text

abstract Terms = {

cat
  Formula ;
  Equation ;
  Eqsign ;
  Term ;
  [Term] {1} ;
  Var ;
  Const ;
  Function ;

fun
  FEquation : Equation -> Formula ;
  FElem : [Term] -> Term -> Formula ;

  EChain : Eqsign -> Term -> Equation -> Equation ;
  EBinary : Eqsign -> Term -> Term -> Equation ;

  EEq, ENeq, ELt, EGt, ELe, EGe, ESim : Eqsign ;

  TParenth : Term -> Term ; -- extra parentheses

  TPlus, TMinus, TTimes, TDiv, TExp : Term -> Term -> Term ;
  TNeg : Term -> Term ;
  TApp : Function -> [Term] -> Term ;

  TVar : Var -> Term ;
  TConst : Const -> Term ;
  TNumber : Float -> Term ;

  FVar : Var -> Function ;
  FDerivative : Function -> Function ;

  N_Const, Z_Const, Q_Const, R_Const, C_Const : Const ;

  stringVar : String -> Var ;
  
  TFrac : Term -> Term -> Term ;
  TAbsolute : Term -> Term ;
  TComprehension : Term -> Term -> Formula -> Term ;
  TPositive : Term -> Term ; -- R^+
  TNegative : Term -> Term ;

  TextbfTerm : Term -> Term ;
}
