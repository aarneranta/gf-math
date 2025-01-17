--# -path=../wikimath:../extraction:../forthel:../mathterms:../extraction/morphodict

concrete Ext100MathIta of Ext100Math =
  WikiMathIta
   - [
    positive_feedback_Q918004_der_AP, 
    collection_Q18029547_CN,
    prime_power_Q1667469_der_AP,
   infinite_group_Q374268_der_AP,
   additive_map_Q22963169_der_CN,
   injective_object_Q1625040_der_AP,
   differentiable_map_Q77989741_der_CN,
   finite_abelian_group_Q3117606_der_AP

  ]
 
  **
  open
    SyntaxIta,
    SymbolicIta,
    GrammarIta,
    Prelude,
    (P=ParadigmsIta)
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

  equinumerous_AP = mkAP (P.mkA "equinumeroso") ;
  surjection_CN = mkCN (P.mkN "suriezione") ;
  powerset_CN = mkCN (P.mkN "insieme" P.masculine) (P.mkAdv "delle parti") ;

oper
  indexedSymb : Str -> Symb = \n ->  mkSymb (mathEnvStr (macroApp "INDEXEDTERM" n)) ;

  onto_Prep : Prep = P.su_Prep ;


lin
  positive_feedback_Q918004_der_AP =  mkAP (P.mkA "positivo") ;
  collection_Q18029547_CN = mkCN (P.mkN "collezione") ;
  prime_power_Q1667469_der_AP = mkAP (P.mkA "primo") ;

   infinite_group_Q374268_der_AP = mkAP (P.mkA "infinito") ;
   additive_map_Q22963169_der_CN = mkCN (P.mkN "mappa") ;
   injective_object_Q1625040_der_AP = mkAP (P.mkA "iniettivo") ;
   differentiable_map_Q77989741_der_CN = mkCN (P.mkN "mappa") ;
   finite_abelian_group_Q3117606_der_AP = mkAP (P.mkA "finito") ;

}

