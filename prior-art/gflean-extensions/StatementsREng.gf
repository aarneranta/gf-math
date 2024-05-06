    concrete StatementsEng of Statements =  PredicatesEng ** open Prelude, Formal in {
   
    lin
        andStm s1 s2 = mkPrec 3 (usePrec 4 s1 ++ ("and" | ",") ++ usePrec 3 s2) ;
        orStm s1 s2 = mkPrec 2 (usePrec 3 s1 ++ "or" ++ usePrec 2 s2) ;
        ifThenStm s1 s2 = mkPrec 1 ("if" ++ usePrec 2 s1 ++ "then" ++ usePrec 1 s2) ;
        iffStm s1 s2 = mkPrec 0 (usePrec 1 s1 ++ "iff" ++ usePrec 0 s2) ;
        notStm stm = mkPrec 4 ("it's not that" ++ stm.s)  ;

        qNotStmToStm qn stm = mkPrec 4 ( "for" ++ qn.s ++ "," ++ stm.s );
        termDoesPredToStm t dp = mkPrec 4 (t.s ++ dp.s) ;

        notionsToStm not = mkPrec 4 (("there exist" | "there exists a" | "there exists an") ++ not.s) ;
        notionNoToStm not = mkPrec 4 (("there exists no" | "there exist no") ++ not.s) ;
}
