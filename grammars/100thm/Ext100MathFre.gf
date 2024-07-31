--# -path=../wikimath:../extraction:../forthel:../mathterms:../extraction/morphodict

concrete Ext100MathFre of Ext100Math =
  WikiMathFre - [
    injective_module_Q2716519_der_AP,
    collection_Q18029547_CN,
    injective_object_Q1625040_der_AP,
    differentiable_map_Q77989741_der_CN,
    natural_number_Q21199_CN
  ]
  **
  open
    SyntaxFre,
    SymbolicFre,
    GrammarFre,
    Prelude,
    (P=ParadigmsFre)
  in {

------------ extensions for the 100 theorems ----------------

lin
  PostForStatement qns s = lin S {s = \\o => s.s ! o ++ (mkAdv for_Prep qns).s} ;
  LatexIndexedStatement n = lin S {s = \\_ => (indexedSymb n.s).s} ;
  ClassNounIndexedNotion pc x = {
    cn = mkCN (mkCN pc.cn (<symb (indexedSymb x.s) : NP>)) pc.adv ;
    isPlur = False
    } ;
  PrimClassOfDefNoun pc cn =
    mkCN pc.cn (mkAdv part_Prep (mkNP aPl_Det (mkCN cn.cn cn.adv))) ;

  ClassOfDefiniteNoun A B = mkCN A (possessAdv B) ;

ClassOfClassNoun A B = {
    cn = A ;
    adv = possessAdv B
    } ;
  ClassFromToClassNoun A B C = {
    cn = A ;
    adv = concatAdv (mkAdv from_Prep B) (mkAdv to_Prep C)
    } ;
  ClassFromOntoClassNoun A B C = {
    cn = A ;
    adv = concatAdv (mkAdv from_Prep B) (mkAdv onto_Prep C)
    } ;

  LatexNamesAssumption names classnoun =
    mkPhr
      (mkUtt (ImpP3 (namesNP (names ** {s = mathEnvStr names.s}))
             (mkVP (mkCN classnoun.cn classnoun.adv)))) ;

  AllSymbTerm st =  mkNP all_Predet <symb st : NP> ;

  IsThePredicate defnoun = mkVP (mkNP the_Det defnoun) ;

  equinumerous_AP = mkAP (P.mkA "équinombreux") ;
  surjection_CN = mkCN (P.mkN "surjection") ;
  powerset_CN = mkCN (P.mkN "ensemble" P.masculine) (P.mkAdv "puissance") ;

oper
  indexedSymb : Str -> Symb = \n ->  mkSymb (mathEnvStr (macroApp "INDEXEDTERM" n)) ;

  onto_Prep : Prep = on_Prep ;


lin
  injective_module_Q2716519_der_AP = mkAP (P.mkA "injectif") ;
  collection_Q18029547_CN = mkCN (P.mkN "collection") ;
  injective_object_Q1625040_der_AP = mkAP (P.mkA "injectif") ;
  differentiable_map_Q77989741_der_CN = mkCN (P.mkN "application") ;
  natural_number_Q21199_CN = mkCN (P.mkA "naturel")  (P.mkN "entier") ;


}