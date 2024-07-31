--# -path=../wikimath:../extraction:../forthel:../mathterms:../extraction/morphodict

concrete Ext100MathGer of Ext100Math =
  WikiMathGer - [
    greatest_common_divisor_Q131752_CN, ---- variants {}
    finite_abelian_group_Q3117606_der_AP,
    additive_map_Q22963169_der_CN,
    infinite_group_Q374268_der_AP,
    collection_Q18029547_CN,
    prime_power_Q1667469_der_AP
    ]
  **
  open
    SyntaxGer,
    SymbolicGer,
    GrammarGer,
    Prelude,
    (P=ParadigmsGer)
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

  equinumerous_AP = mkAP (P.mkA "gleichzahlig") ;
  surjection_CN = mkCN (P.mkN "Surjektion") ;
  powerset_CN = mkCN (P.mkN "Potenzmenge") ;

oper
  indexedSymb : Str -> Symb = \n ->  mkSymb (mathEnvStr (macroApp "INDEXEDTERM" n)) ;

  onto_Prep : Prep = P.aufAcc_Prep ;

lin ---- not in MathTermsGer
  greatest_common_divisor_Q131752_CN = mkCN (P.mkA "größt") (mkCN (P.mkA "gemeinsam") (P.mkN "Teiler")) ;
  finite_abelian_group_Q3117606_der_AP = mkAP (P.mkA "endlich") ;
  additive_map_Q22963169_der_CN = mkCN (P.mkN "Abbildung") ;
  infinite_group_Q374268_der_AP = mkAP (P.mkA "unendlich") ;
  collection_Q18029547_CN = mkCN (P.mkN "Sammlung") ;
  prime_power_Q1667469_der_AP = mkAP (P.mkA "unteilbar") ;



}