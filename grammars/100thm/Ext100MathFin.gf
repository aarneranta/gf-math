--# -path=../wikimath:../extraction:../forthel:../mathterms:../extraction/morphodict

concrete Ext100MathFin of Ext100Math =
  WikiMathFin - [
   positive_feedback_Q918004_der_AP,
   rational_number_Q1244890_CN,
   collection_q18029547_cn,  --- what is this??
   collection_Q18029547_CN,
   prime_power_Q1667469_der_AP,
   infinite_group_Q374268_der_AP,
   injective_object_Q1625040_der_AP,
   additive_map_Q22963169_der_CN,
   injective_module_Q2716519_der_AP,
   differentiable_map_Q77989741_der_CN,
   finite_abelian_group_Q3117606_der_AP,
   positive_real_number_Q3176558_der_CN,
   negative_real_number_Q200227_der_CN,
   greatest_common_divisor_Q131752_CN 
   ]
  **
  open
    SyntaxFin,
    SymbolicFin,
    GrammarFin,
    Prelude,
    (P=ParadigmsFin)
  in {

------------ extensions for the 100 theorems ----------------

lin
  PostForStatement qns s = lin S {s = s.s ++ (mkAdv for_Prep qns).s} ;
  LatexIndexedStatement n = indexedSymb n.s ;
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

  equinumerous_AP = mkAP (P.mkA "yhtämahtava") ;
  surjection_CN = mkCN (P.mkN "surjektio") ;
  powerset_CN = mkCN (P.mkN "potenssijoukko") ;


   positive_feedback_Q918004_der_AP = mkAP (P.mkA "positiivinen") ;
   rational_number_Q1244890_CN = mkCN (P.mkN "luku") ;
   collection_q18029547_cn = mkCN (P.mkN "kokoelma") ;
   collection_Q18029547_CN = mkCN (P.mkN "kokoelma") ;
   prime_power_Q1667469_der_AP = mkAP (P.mkA "jaoton") ;
   infinite_group_Q374268_der_AP = mkAP (P.mkA "ääretön") ;
   injective_object_Q1625040_der_AP = mkAP (P.mkA "injektiivinen") ;
   additive_map_Q22963169_der_CN = mkCN (P.mkN "kuvaus") ;
   injective_module_Q2716519_der_AP = mkAP (P.mkA "moduli") ;
   differentiable_map_Q77989741_der_CN = mkCN (P.mkN "kuvaus" "kuvauksen") ;
   finite_abelian_group_Q3117606_der_AP = mkAP (P.mkA "äärellinen") ;
   positive_real_number_Q3176558_der_CN = mkCN (P.mkN "reaaliluku") ;
   negative_real_number_Q200227_der_CN = mkCN (P.mkN "reaaliluku") ;
   greatest_common_divisor_Q131752_CN =
      mkCN (P.mkA (P.mkN "suurin" "suurimman")) (mkCN (P.mkA "yhteinen") (P.mkN "tekijä")) ;


oper
  indexedSymb : Str -> Symb = \n ->  mkSymb (mathEnvStr (macroApp "INDEXEDTERM" n)) ;

  onto_Prep : Prep = for_Prep ;


}