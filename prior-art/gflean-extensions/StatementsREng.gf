--# -path=.:simplifiedForThel:present

concrete StatementsREng of Statements =  PredicatesREng ** open Prelude, Formal, Utils, SyntaxEng, ParadigmsEng, SymbolicEng in {
   
    lin
        andStm s1 s2 = mkS and_Conj s1 s2 ;
        orStm s1 s2 = mkS or_Conj s1 s2 ;
        ifThenStm s1 s2 = mkS (SyntaxEng.mkAdv if_Subj s1) (mkS (ParadigmsEng.mkAdv "then") s2) ;
        iffStm s1 s2 = mkS (mkConj "iff") s1 s2 ;
        notStm stm = mkS negativePol (mkCl (mkVP (SyntaxEng.mkAdv that_Subj stm))) ;

        qNotStmToStm qn stm = mkS (SyntaxEng.mkAdv for_Prep qn) stm ;
        termDoesPredToStm t dp = mkS (mkCl t dp) ;

        notionsToStm not = mkS (mkCl (mkNP a_Det not)) ; --- pl variants
        notionNoToStm not = mkS (mkCl (mkNP no_Quant not)) ; -- pl variants
}
