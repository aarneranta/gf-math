concrete TermsEng of Terms = NotionsEng ** open Prelude, Formal in {
    lincat
        term = TermPrec ;
        quantifiedNotion = TermPrec ;
        definiteTerm = TermPrec ;
        
        primDefiniteNoun = TermPrec ;

    lin 
        rN2ToPDNoun r t1 t2 = {s =  usePrec r.p t1 ++ r.s ++ usePrec (nextPrec r.p) t2 ; p = r.p} ;

    lin
        qNotionToTerm = id TermPrec ;

        allNotion n = mkPrec 4 ("every" ++ n.s) ;
        someNotion n = mkPrec 4 ("some" ++ n.s) ;
        noNotion n = mkPrec 4 ("no" ++ n.s) ;

        prDefNounToDefTerm = id TermPrec ;
        
        varToDefTerm v = mkPrec 4 v.s ;
        intToDefTerm i = mkPrec 4 i.s ;

        defTermToTerm = id TermPrec ;
        
}