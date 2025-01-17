-- mathematical terms as they appear in "normal" mathematical text

abstract ForthelTerms = {

cat
  Formula ;
  Equation ;
  Eqsign ;
  Exp ;
  [Exp] {1} ;
  Var ;
  Const ;
  Function ;

fun
  FEquation : Equation -> Formula ;
  FElem : [Exp] -> Exp -> Formula ;

  EChain : Eqsign -> Exp -> Equation -> Equation ;
  EBinary : Eqsign -> Exp -> Exp -> Equation ;

  EEq, ENeq, ELt, EGt, ELe, EGe, ESim : Eqsign ;

  TParenth : Exp -> Exp ; -- extra parentheses

  TPlus, TMinus, TTimes, TDiv, TExp : Exp -> Exp -> Exp ;
  TNeg : Exp -> Exp ;
  TApp : Function -> [Exp] -> Exp ;

  TVar : Var -> Exp ;
  TConst : Const -> Exp ;
---  TFloat : Float -> Exp ;
  TNumber : Int -> Exp ;

  FVar : Var -> Function ;
  FDerivative : Function -> Function ;

  N_Const, Z_Const, Q_Const, R_Const : Const ;

  x_Var, y_Var, z_Var, u_Var : Var ; 
  a_Var, b_Var, c_Var, d_Var : Var ;
  f_Var, g_Var : Var ;
  k_Var, m_Var, n_Var, p_Var : Var ;
  q_Var, r_Var : Var ;
  s_Var, t_Var : Var ;
  A_Var, B_Var, C_Var, K_Var : Var ;
  L_Var, M_Var, S_Var, T_Var : Var ;

}

-- rf -file=lexed.txt -lines | p -lang=Eng | l -lang=Ger -bind -unlexmixed | ? sort -u

-- cat out/terms_Fre.conllu | deptreepy/deptreepy.py visualize_conllu >out/terms_Fre.html