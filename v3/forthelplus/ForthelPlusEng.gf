--# -path=.:../Core

concrete ForthelPlusEng of ForthelPlus = CoreEng, TermsLatex **

open
  SyntaxEng,
  (Syntax = SyntaxEng),
  SymbolicEng,
  ParadigmsEng
in {

lincat
  [Adj] = [AP] ;
---  [Exp] = [NP] ;

lin
  TermExp term = latexNP (mkSymb term.s) ;
  FormulaProp formula = simpleProp (latexS (mkSymb formula.s)) ;

  AndAdj adjs = mkAP and_Conj adjs ;
  OrAdj adjs = mkAP or_Conj adjs ;

---  AndExp exps = mkNP and_Conj exps ;
---  OrExp exps = mkNP or_Conj exps ;

  BaseAdj a b = mkListAP a b ;
  ConsAdj a bs = mkListAP a bs ;

---  BaseExp a b = mkListExp a b ;
---  ConsExp a bs = mkListExp a bs ;

}