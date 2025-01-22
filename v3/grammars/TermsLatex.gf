concrete TermsLatex of Terms =
  open Formal, Prelude in {

lincat
  Formula = TermPrec ;
  Equation = {s : Str} ;
  Eqsign = Str ;
  Term = TermPrecNum ;
  [Term] = {s : Str} ;
  Var = Str ;
  Constant = Str ;
  Function = Str ;

lin
  FEquation eq = constant eq.s ;
  FElem es e = constant (es.s ++ "\\in" ++ top e) ;

  EChain op x eq = {s = top x ++ op ++ eq.s} ;
  EBinary op x y = {s = top x ++ op ++ top y} ;

  TParenth t = constant (parenth (top t)) ** {isNumber = False} ;

  TTimes x y = case <x.isNumber, y.isNumber> of {
     <True, True> => infixl 2 "\\times" x y ** {isNumber = True} ;
     _ => tinfixl 2 "" x y
     } ;
  TExp a b = tinfixl 3 "^" a (b ** {s = curlyStr b.s}) ;

  TNeg x = prefix 2 "-" x ** {isNumber = x.isNumber} ;
  TApp f xs = constant (f ++ parenth xs.s) ** {isNumber = False} ;

  TVar x =  constant x ** {isNumber = False} ;
  TConstant c =  constant c ** {isNumber = False} ;
  TNumber n = constant n.s ** {isNumber = True} ;

  BaseTerm x = {s = top x} ;
  ConsTerm x xs = {s = top x ++ "," ++ xs.s} ;

  N_Constant = "N" ;
  Z_Constant = "Z" ;
  Q_Constant = "Q" ;
  R_Constant = "R" ;
  C_Constant = "C" ;

  stringVar s = s.s ;

  FVar v = v ;
  FDerivative f = f ++ "'" ;

  
  TPositive c = tinfixl 3 "^" c (tconstant (curlyStr "+")) ;
  TNegative c = tinfixl 3 "^" c (tconstant (curlyStr "-")) ;

  TFrac a b = tconstant (macroApp "frac" (top a) (top b)) ;
  
  TAbsolute a = tconstant ("|" ++ (top a) ++ "|") ;
  
  TComprehension a b f =
    tconstant ("\\{" ++ top a ++ "\\in" ++ top b ++
                ":" ++ top f ++ "\\}") ;

  TextbfTerm e = e ** {s = macroApp "textbf" (top e)} ;

oper
  TermPrecNum = TermPrec ** {isNumber : Bool} ;

  tinfixl : Prec -> Str -> (_,_ : TermPrecNum) -> TermPrecNum = \p, op, x, y ->
    infixl p op x y ** {isNumber = False} ;
  tconstant : Str -> TermPrecNum = \s ->
    constant s ** {isNumber = False} ;

  -- to be usable at runtime, therefore ++
  mathEnvStr : Str -> Str = \s -> "$" ++ s ++ "$" ;
  curlyStr : Str -> Str = \s -> "{" ++ s ++ "}" ;

  macroApp = overload {
    macroApp : (f : Str) -> Str = \f -> "\\" + f ;
    macroApp : (f, x : Str) -> Str = \f, x -> "\\" + f ++ "{" ++ x ++ "}" ;
    macroApp : (f, x, y : Str) -> Str = \f, x, y ->
      "\\" + f ++ "{" ++ x ++ "} {" ++ y ++ "}" ;
   } ;

}