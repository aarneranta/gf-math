--# -path=.:simplifiedForThel:present

concrete TermsREng of Terms = NotionsREng ** open Prelude, Formal, Utils, SyntaxEng, ParadigmsEng, SymbolicEng in {
    lincat
        term = NP ;
        quantifiedNotion = NP ;
        definiteTerm = NP ;
        
        primDefiniteNoun = NP ;

    lin 
        rN2ToPDNoun r t1 t2 = symb ((mkUtt t1).s ++ r.s ++ (mkUtt t2).s) ; ---- creates ambiguity

        qNotionToTerm np = np ;

        allNotion n = mkNP every_Det n ;
        someNotion n = mkNP someSg_Det n ;
        noNotion n = mkNP no_Quant n ;

        prDefNounToDefTerm np = np ;
        
        varToDefTerm v = symb v ;
        intToDefTerm i = symb i ;

        defTermToTerm np = np ;
        
}