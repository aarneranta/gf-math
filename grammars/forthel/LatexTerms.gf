-- mathematical terms as they appear in "normal" mathematical text

abstract LatexTerms =

  ForthelTerms - [ENeq, ELe, EGe, ESim, FElem, TExp] ** {
  
    fun
      LFElem : [Exp] -> Exp -> Formula ;
      LENeq, LELe, LEGe, LESim : Eqsign ;

      LTPower : Exp -> Exp -> Exp ;
      LTFrac : Exp -> Exp -> Exp ;
      LTAbsolute : Exp -> Exp ;
      LTComprehension : Exp -> Exp -> Formula -> Exp ;
      LTPositive : Exp -> Exp ; -- R^+
      LTNegative : Exp -> Exp ;

      LTextbfExp : Exp -> Exp ;
}
