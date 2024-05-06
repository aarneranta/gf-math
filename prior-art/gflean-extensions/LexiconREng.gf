--# -path=.:simplifiedForThel:present

concrete LexiconREng of Lexicon = open Prelude, Formal, Utils, SyntaxEng, ParadigmsEng, SymbolicEng in {
    lincat

        variable = Symb ;
        rawNoun0 = {cn : CN ; s : Symb} ;
        rawNoun2 = TermPrec ;

        rawAdjective0 = {ap : AP ; s : Str} ;
        rawAdjective1 = {ap : AP ; p : Prep ; s : Str} ;


    lin
        A_VAR = mkSymb "a" ;
        B_VAR = mkSymb "b" ;
        C_VAR = mkSymb "c" ;
        K_VAR = mkSymb "k" ;
        M_VAR = mkSymb "m" ;
        N_VAR = mkSymb "n" ;
        R_VAR = mkSymb "r" ;
        X_VAR = mkSymb "x" ;
        Y_VAR = mkSymb "y" ;
        Z_VAR = mkSymb "z" ;

        REAL_NUMBER = {cn = mkCN (mkA "real") (mkN "number") ; s = mkSymb "ℝ"} ;
        INTEGER = {cn = mkCN (mkN "integer") ; s = mkSymb "ℤ"} ;
        RATIONAL = {cn = mkCN (mkA "rational") (mkN "number") ; s =  mkSymb "ℚ"} ;
 
        LESS_THAN = {ap = mkAP (mkA "less") ; p = mkPrep "than" ; s = "<"} ;
        LESS_TE = {ap = mkAP or_Conj (mkAP (mkA "less than")) (mkAP (mkA "equal")) ; p = to_Prep ; s =  "≥"} ; ---
        GREATER_THAN =  {ap = mkAP (mkA "greater") ; p = mkPrep "than" ; s = ">"} ;
        GREATER_TE = {ap = mkAP or_Conj (mkAP (mkA "greater than")) (mkAP (mkA "equal")) ; p = to_Prep ; s = "≥"} ; ---
        NOT_EQUAL = {ap = mkAP (mkAdA "not") (mkA "equal") ; p = to_Prep ; s = "≠"} ; ---
        EQUAL = {ap = mkAP (mkA "equal") ; p = to_Prep ; s = "="} ;
        --BETWEEN  = mkSS3L "between" "and" "" ;

        POSITIVE = {ap = mkAP (mkA "positive") ; s = "pos"} ;
        ODD = {ap = mkAP (mkA "odd") ; s = "odd"} ;
        EVEN = {ap = mkAP (mkA "even") ; s = "even"} ;
        NONNEGATIVE = {ap = mkAP (mkA "nonnegative") ; s = "nneg"} ;
        NEGATIVE = {ap = mkAP (mkA "negative") ; s = "neg"} ;

        --SAMEPARITY = mkSSL "have same parity" "samePar" ;
        --OPPPARITY = mkSSL "have opposite parity" "oppPar" ;

        EXP = mkPrec 4 "^";
        SUM = mkPrec 0 "+";
        MINUS = mkPrec 1 "-";
        PRODUCT = mkPrec 2 "*";
        FRAC = mkPrec 3 "/";
}
