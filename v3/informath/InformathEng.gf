--# -path=.:../mathcore

concrete InformathEng of Informath = MathCoreEng, TermsLatex **

open
  SyntaxEng,
  (Syntax = SyntaxEng),
  SymbolicEng,
  ParadigmsEng,
  Formal,
  Prelude
in {

lincat
  [Adj] = [AP] ;
---  [Exp] = [NP] ;

lin
  TermExp term = latexNP (mkSymb term.s) ;
  FormulaProp formula = simpleProp (latexS (mkSymb formula.s)) ;

  ConstTerm const = constant const.c ** {isNumber = False} ;
  ComparEquation compar x y = {s = top x ++ compar.op ++ top y} ;
  AppOperTerm op x y = infixl op.p op.op x y ** {isNumber = False} ;

  AndAdj adjs = mkAP and_Conj adjs ;
  OrAdj adjs = mkAP or_Conj adjs ;

---  AndExp exps = mkNP and_Conj exps ;
---  OrExp exps = mkNP or_Conj exps ;

  EveryKindExp kind = mkNP every_Det (mkCN kind.cn kind.adv) ;
  AllArgKindExp kind = mkNP all_Predet (mkNP aPl_Det (mkCN kind.cn kind.adv)) ;

  BaseAdj a b = mkListAP a b ;
  ConsAdj a bs = mkListAP a bs ;

---  BaseExp a b = mkListExp a b ;
---  ConsExp a bs = mkListExp a bs ;

-- Pathak's

  LetFormulaHypo formula = lin Utt {s = let_Str ++ "$" ++ top formula ++ "$"} ;
  PropJmt hypos prop = mkText hypos.text (mkText (topProp prop)) ;
  DefinedAdjJmt hypos exp adj prop =
    thenText hypos (mkText (
      mkS (mkCl exp (mkVP (mkVP (passiveVP define_V2)
        (lin Adv (mkSC (mkVP adj)))) (Syntax.mkAdv if_Subj prop.s))))) ; 
  WeDefineAdjJmt hypos exp adj prop =
    thenText hypos (mkText (
      mkS (mkCl we_NP (mkVP (mkVP (mkVP define_V2 exp)
        (lin Adv (mkSC (mkVP adj)))) (Syntax.mkAdv if_Subj prop.s))))) ; 

  AdjKind adj kind = kind ** {cn = mkCN adj kind.cn} ;
  KindProp exp kind = simpleProp (mkS (mkCl exp kind.cn)) ;

  SomeKindExp kind = mkNP someSg_Det (mkCN kind.cn kind.adv) ;
  SomeArgKindExp kind = mkNP somePl_Det (mkCN kind.cn kind.adv) ;
  PostQuantProp prop exp =
    simpleProp (postAdvS prop.s (Syntax.mkAdv for_Prep exp)) ; --- no complexProp in Informath
  IndefKindExp kind = mkNP a_Det (mkCN kind.cn kind.adv) ;
  IndefIdentKindExp ident kind = mkNP a_Det (mkCN (mkCN kind.cn (latexNP ident)) kind.adv) ;
  EveryIdentKindExp ident kind = mkNP every_Det (mkCN (mkCN kind.cn (latexNP ident)) kind.adv) ;


oper
  postAdvS : S -> Adv -> S = \s, adv -> lin S {s = s.s ++ adv.s} ;

  let_Str = "let" ;
}