incomplete concrete InformathFunctor of Informath =
  MathCore **

open
  Syntax,
  Symbolic,
  Grammar,
  Formal,
  Prelude
in {

lincat
  [Adj] = Syntax.ListAP ;
  [Exp] = Syntax.ListNP ;

lin
  FormulaProp formula = simpleProp (latexS (mkSymb formula.s)) ;

  SetTerm set = constant set.c ** {isNumber = False} ;
  ConstTerm const = constant const.c ** {isNumber = False} ;
  ComparEqsign compar = compar.op ;
  AppOperTerm op x y = infixl op.p op.op x y ** {isNumber = False} ;

  AndAdj adjs = mkAP both7and_DConj adjs ;
  OrAdj adjs = mkAP or_Conj adjs ;

  AndExp exps = mkNP both7and_DConj exps ;
  OrExp exps = mkNP or_Conj exps ;

  EveryKindExp kind = mkNP every_Det (mkCN kind.cn kind.adv) ;
  AllArgKindExp kind = mkNP all_Predet (mkNP aPl_Det (mkCN kind.cn kind.adv)) ;

  BaseAdj a b = mkListAP a b ;
  ConsAdj a bs = mkListAP a bs ;

  BaseExp a b = mkListNP a b ;
  ConsExp a bs = mkListNP a bs ;

-- Pathak's

  LetFormulaHypo formula = lin Utt {s = let_Str ++ "$" ++ top formula ++ "$"} ;

  DefinedAdjJmt label hypos exp adj prop =
    labelText (label)
      (thenText hypos (
        mkS (mkCl exp (mkVP (mkVP (passiveVP define_V2)
          <lin Adv (mkUtt (mkVP adj)) : Adv>) (Syntax.mkAdv if_Subj prop.s))))) ; 
  WeDefineAdjJmt label hypos exp adj prop =
    labelText (label)
      (thenText hypos (
        mkS (mkCl we_NP (mkVP (mkVP (mkVP define_V2 exp)
          <lin Adv (mkUtt (mkVP adj)) : Adv>) (Syntax.mkAdv if_Subj prop.s))))) ; 

  AdjKind adj kind = kind ** {cn = mkCN adj kind.cn} ;
  KindProp exp kind = simpleProp (mkS (mkCl exp kind.cn)) ;

  SomeKindExp kind = mkNP someSg_Det (mkCN kind.cn kind.adv) ;
  SomeArgKindExp kind = mkNP somePl_Det (mkCN kind.cn kind.adv) ;
  PostQuantProp prop exp =
    simpleProp (postAdvS prop.s (Syntax.mkAdv for_Prep exp)) ; --- no complexProp in Informath
  IndefKindExp kind = mkNP a_Det (mkCN kind.cn kind.adv) ;
  IndefIdentKindExp ident kind = mkNP a_Det (mkCN (mkCN kind.cn (latexNP (mkSymb ident))) kind.adv) ;
  EveryIdentKindExp ident kind = mkNP every_Det (mkCN (mkCN kind.cn (latexNP (mkSymb ident))) kind.adv) ;


oper
  postAdvS : S -> Adv -> S = \s, adv -> lin S {s = s.s ++ adv.s} ;
}