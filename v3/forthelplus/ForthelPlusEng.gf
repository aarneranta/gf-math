--# -path=.:../Core

concrete ForthelPlusEng of ForthelPlus = CoreEng, TermsLatex **

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

}