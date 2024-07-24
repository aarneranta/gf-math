-- mathematical terms as they appear in "normal" mathematical text

abstract LatexTerms =

  ForthelTerms - [ENeq, ELe, EGe, ESim, FElem, TExp] ** {
  
    fun
      LFElem : Exp -> Exp -> Formula ;

      LENeq, LELe, LEGe, LESim : Eqsign ;

      LTExp : Exp -> Exp -> Exp ;

}
