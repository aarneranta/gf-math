--# -path=.:simplifiedForThel:present

concrete TextsREng of Texts = StatementsREng ** open Prelude, Formal, Utils, SyntaxEng, ParadigmsEng, SymbolicEng, (G=GrammarEng) in {
    lincat
        example = Text ;
        assumption = Text ;
        text = Text ;

        Lassumption = {t : Text ; isEmpty : Bool} ;

    lin
        Bassumption = {t = emptyText ; isEmpty = True} ;
        Cassumption as ls = {t = mkText (strText "assume") (mkText as ls.t) ; isEmpty = False} ;

        assToExm las st =
	  mkText (mkText (strText "ex .") las.t)
	         (mkText (strText (if_then_Str las.isEmpty "" "then")) (mkText st)) ;

        stmToAssumption s = mkText s ;

        thmToText e = e ;


    oper
      strText : Str -> Text = \s -> lin Text {s = s} ; ---


}
