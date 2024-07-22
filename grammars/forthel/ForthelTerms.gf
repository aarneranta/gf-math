-- mathematical terms as they appear in "normal" mathematical text

abstract ForthelTerms = {

cat
  Formula ;
  Equation ;
  Eqsign ;
  Exp ;
  [Exp] {1} ;
  Variable ;
  Function ;

fun
  FEquation : Equation -> Formula ;

  EChain : Eqsign -> Exp -> Equation -> Equation ;
  EBinary : Eqsign -> Exp -> Exp -> Equation ;

  EEq, ENeq, ELt, EGt, ELe, EGe, ESim : Eqsign ;

  TPlus, TMinus, TTimes, TDiv, TExp : Exp -> Exp -> Exp ;
  TNeg : Exp -> Exp ;
  TApp : Function -> [Exp] -> Exp ;

  TVariable : Variable -> Exp ;
  TNumber : Float -> Exp ;

  FVariable : Variable -> Function ;
  FDerivative : Function -> Function ;


  x_Variable, y_Variable, z_Variable, u_Variable : Variable ; 
  a_Variable, b_Variable, c_Variable, d_Variable : Variable ;
  f_Variable, g_Variable : Variable ;

}