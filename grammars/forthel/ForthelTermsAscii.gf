concrete ForthelTermsAscii of ForthelTerms = open Formal, Prelude in {

lincat
  Formula = TermPrec ;
  Equation = {s : Str} ;
  Eqsign = Str ;
  Exp = TermPrecNum ;
  [Exp] = {s : Str} ;
  Variable = Str ;
  Function = Str ;

lin
  FEquation eq = constant eq.s ;

  EChain op x eq = {s = top x ++ op ++ eq.s} ;
  EBinary op x y = {s = top x ++ op ++ top y} ;

  EEq = "=" ; 
  ENeq = "≠" ; 
  ELt = "<" ; 
  EGt = ">" ; 
  ELe = "≤" ; 
  EGe = "≥" ; 
  ESim = "~" ;

  TPlus = tinfixl 1 "+" ;
  TMinus = tinfixl 1 "-" ;
  TTimes x y = case <x.isNumber, y.isNumber> of {
     <True, True> => infixl 2 "*" x y ** {isNumber = True} ;
     _ => tinfixl 2 "" x y
     } ;
  TDiv = tinfixl 2 "/" ;
  TExp = tinfixl 3 "^" ;
  TNeg x = prefix 3 "-" x ** {isNumber = x.isNumber} ;
  TApp f xs = constant (f ++ parenth xs.s) ** {isNumber = False} ;

  TVariable x =  constant x ** {isNumber = False} ;
  TNumber n = constant n.s ** {isNumber = True} ;

  BaseExp x = {s = top x} ;
  ConsExp x xs = {s = top x ++ "," ++ xs.s} ;

  FVariable v = v ;
  FDerivative f = f ++ "'" ;

  x_Variable = "x" ;
  y_Variable = "y" ;
  z_Variable = "z" ;
  u_Variable = "u" ;
  a_Variable = "a" ;
  b_Variable = "b" ;
  c_Variable = "c" ;
  d_Variable = "d" ;
  f_Variable = "f" ;
  g_Variable = "g" ;
  k_Variable = "k" ;
  n_Variable = "n" ;
  m_Variable = "m" ;
  p_Variable = "p" ;


oper
  TermPrecNum = TermPrec ** {isNumber : Bool} ;

  tinfixl : Prec -> Str -> (_,_ : TermPrecNum) -> TermPrecNum = \p, op, x, y ->
    infixl p op x y ** {isNumber = False} ;

}