concrete ForthelTermsAscii of ForthelTerms = open Formal, Prelude in {

lincat
  Formula = TermPrec ;
  Equation = {s : Str} ;
  Eqsign = Str ;
  Exp = TermPrecNum ;
  [Exp] = {s : Str} ;
  Var = Str ;
  Const = Str ;
  Function = Str ;

lin
  FEquation eq = constant eq.s ;
  FElem es e = constant (es.s ++ "∈" ++ top e) ;

  EChain op x eq = {s = top x ++ op ++ eq.s} ;
  EBinary op x y = {s = top x ++ op ++ top y} ;

  EEq = "=" ; 
  ENeq = "≠" ; 
  ELt = "<" ; 
  EGt = ">" ; 
  ELe = "≤" ; 
  EGe = "≥" ; 
  ESim = "~" ;

  TParenth t = constant (parenth (top t)) ** {isNumber = False} ;

  TPlus = tinfixl 1 "+" ;
  TMinus = tinfixl 1 "-" ;
  TTimes x y = case <x.isNumber, y.isNumber> of {
     <True, True> => infixl 2 "*" x y ** {isNumber = True} ;
     _ => tinfixl 2 "" x y
     } ;
  TDiv = tinfixl 2 "/" ;
  TExp = tinfixl 3 "^" ;
  TNeg x = prefix 2 "-" x ** {isNumber = x.isNumber} ;
  TApp f xs = constant (f ++ parenth xs.s) ** {isNumber = False} ;

  TVar x =  constant x ** {isNumber = False} ;
  TConst c =  constant c ** {isNumber = False} ;
---  TFloat n = constant n.s ** {isNumber = True} ;
  TNumber n = constant n.s ** {isNumber = True} ;

  BaseExp x = {s = top x} ;
  ConsExp x xs = {s = top x ++ "," ++ xs.s} ;

  N_Const = "N" ;
  Z_Const = "Z" ;
  Q_Const = "Q" ;
  R_Const = "R" ;

  FVar v = v ;
  FDerivative f = f ++ "'" ;

  x_Var = "x" ;
  y_Var = "y" ;
  z_Var = "z" ;
  u_Var = "u" ;
  a_Var = "a" ;
  b_Var = "b" ;
  c_Var = "c" ;
  d_Var = "d" ;
  f_Var = "f" ;
  g_Var = "g" ;
  k_Var = "k" ;
  n_Var = "n" ;
  m_Var = "m" ;
  p_Var = "p" ;
  q_Var = "q" ;
  r_Var = "r" ;
  s_Var = "s" ;
  t_Var = "t" ;

  A_Var = "A" ;
  B_Var = "B" ;
  C_Var = "C" ;
  K_Var = "K" ;
  L_Var = "L" ;
  M_Var = "M" ;
  S_Var = "S" ;
  T_Var = "T" ;

oper
  TermPrecNum = TermPrec ** {isNumber : Bool} ;

  tinfixl : Prec -> Str -> (_,_ : TermPrecNum) -> TermPrecNum = \p, op, x, y ->
    infixl p op x y ** {isNumber = False} ;
  tconstant : Str -> TermPrecNum = \s ->
    constant s ** {isNumber = False} ;

}