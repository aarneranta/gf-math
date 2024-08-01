--# -path=../wikimath:../extraction:../forthel:../mathterms:../extraction/morphodict

concrete Ext100MathPor of Ext100Math =
  WikiMathPor
   - [
    positive_feedback_Q918004_der_AP, 
    collection_Q18029547_CN,
    prime_power_Q1667469_der_AP 
  ]
 
  **
  open
    SyntaxPor,
    SymbolicPor,
    GrammarPor,
    Prelude,
    (P=ParadigmsPor)
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

  equinumerous_AP = mkAP (P.mkA "equinumeiro") ;
  surjection_CN = mkCN (P.mkN "sobrejecção") ;
  powerset_CN = mkCN (P.mkN "conjunto" P.masculine) (P.mkAdv "de potência") ;

oper
  indexedSymb : Str -> Symb = \n ->  mkSymb (mathEnvStr (macroApp "INDEXEDTERM" n)) ;

  onto_Prep : Prep = P.mkPrep "sobre" ;


lin
  positive_feedback_Q918004_der_AP =  mkAP (P.mkA "positivo") ;
  collection_Q18029547_CN = mkCN (P.mkN "collection") ;
  prime_power_Q1667469_der_AP = mkAP (P.mkA "primo") ;

}