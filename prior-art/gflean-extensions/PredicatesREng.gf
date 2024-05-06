--# -path=.:simplifiedForThel:present

concrete PredicatesREng of Predicates = TermsREng ** open Prelude, Formal, Utils, SyntaxEng, ParadigmsEng, SymbolicEng, (G=GrammarEng) in {
   lincat
        polarity = Pol ;

        --hasPredicate = SS ;
        is_aPredicate = VP ;

        --possessedNoun = SS ;

        primAdjective = AP ;
        --primAdjectiveM = SS ;
        --primVerb = SS ;
        --primVerbM = SS ;
        --primPossessedNoun = SS ;

lin
        
        rA0ToPAdj rA0 = rA0.ap  ;
        rA1ToPAdj r t = G.AdvAP r.ap (SyntaxEng.mkAdv r.p t) ;
        --rA2ToPAdj r t1 t2 = {s = (r!E).s1 ++ t1.s ++ (r!E).s2 ++ t2.s ++ (r!E).s3} ;
        
lin 
        pos = positivePol ;
        neg = negativePol ;

        --prVerbToDPred = cc3 (ss "does") ;
        --prVerbMToDPred = cc3 (ss "do") ;
        --hasPredToDPred = cc2 (ss ("has" | "have")) ;
        isPredToDPred p = mkVP p ;
        isAPredToDPred p = p ;

        primAdjToIsPred pol ap = ap ;

        clNounToIs_aPred pol cn = mkVP cn ; ---- polarity
        deftrmToIs_aPred pol np = mkVP np ;

        --possNTohasPred = cc2 (ss ("a" | "an")) ;
        --possNnotTohasPred = cc2 (ss "no") ;

        --primPosNounToPosNoun = cc2 ;
        --primPosNounRAttrToPosNoun = cc3 ;
}