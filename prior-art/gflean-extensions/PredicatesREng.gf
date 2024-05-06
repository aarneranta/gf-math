concrete PredicatesEng of Predicates = TermsEng ** open Prelude in {
   lincat
        polarity = SS ;

        --hasPredicate = SS ;
        is_aPredicate = SS ;

        --possessedNoun = SS ;

        primAdjective = SS ;
        --primAdjectiveM = SS ;
        --primVerb = SS ;
        --primVerbM = SS ;
        --primPossessedNoun = SS ;

lin
        
        rA0ToPAdj rA0 = rA0!E  ;
        rA1ToPAdj r t = {s = (r!E).s1 ++ t.s ++ (r!E).s2} ;
        --rA2ToPAdj r t1 t2 = {s = (r!E).s1 ++ t1.s ++ (r!E).s2 ++ t2.s ++ (r!E).s3} ;
        
lin 
        pos = ss "" ;
        neg = ss "not" ;

        --prVerbToDPred = cc3 (ss "does") ;
        --prVerbMToDPred = cc3 (ss "do") ;
        --hasPredToDPred = cc2 (ss ("has" | "have")) ;
        isPredToDPred = cc2 (ss ("is" | "are" )) ;
        isAPredToDPred = cc2 (ss ( "is" | "are")) ;

        primAdjToIsPred = cc2 ;

        clNounToIs_aPred pol = cc3 pol (ss ("a" | "an" | "")) ;
        deftrmToIs_aPred = cc2 ;

        --possNTohasPred = cc2 (ss ("a" | "an")) ;
        --possNnotTohasPred = cc2 (ss "no") ;

        --primPosNounToPosNoun = cc2 ;
        --primPosNounRAttrToPosNoun = cc3 ;
}