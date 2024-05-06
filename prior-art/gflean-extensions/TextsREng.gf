concrete TextsEng of Texts = StatementsEng ** open Prelude in {
    lincat
        example = SS ;
        assumption = SS ;
        text = SS ;

        Lassumption = SS ;

    lin
        Bassumption = ss "";
        Cassumption as ls = prefixSS "assume" (cc2 as ls);

        assToExm las st  = {s = "ex ." ++ las.s ++ (optStr "then") ++ st.s ++ "."} ;

        stmToAssumption = postfixSS "." ;

        thmToText = id SS;


}
