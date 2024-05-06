--# -path=.:simplifiedForThel:present

concrete TextsREng of Texts = StatementsREng ** open Prelude, Formal, Utils, SyntaxEng, ParadigmsEng, SymbolicEng, (G=GrammarEng) in {
    lincat
        example = Text ;
        assumption = Text ;
        text = Text ;

        Lassumption = Text ;

    lin
        Bassumption = emptyText ;
        Cassumption as ls = mkText (strText "assume") (mkText as ls) ; ---

        assToExm las st  = mkText (mkText (strText "ex .") las) (mkText (strText "then") (mkText st)) ;

        stmToAssumption s = mkText s ;

        thmToText e = e ;


    oper
      strText : Str -> Text = \s -> lin Text {s = s} ; ---


}
