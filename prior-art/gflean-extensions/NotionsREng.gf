--# -path=.:simplifiedForThel:present

concrete NotionsREng of Notions = LexiconREng ** open SyntaxEng, ParadigmsEng, SymbolicEng, (R=ResEng), Prelude, Formal in {
    lincat
        notion = CN ;
        names = NP ;
        
        leftAttribute = AP ;
        rightAttribute = {ap : AP ; rs : RS ; isAP : Bool} ;
        isPredicate =  {ap : AP ; pol : Pol} ;
        doesPredicate = {vp : VP ; pol : Pol} ;
        statement = S ;
        primSimpleAdjective = AP ;
        primClassNoun = CN ;

    lin

        rA0ToPSAdj rAdj = rAdj.ap ;
        rN0ToPcNoun rN0 nam = mkCN rN0.cn nam ;
        
    lin
        listVarToName x = symb x ;
        knownName n = mkSymb ("x" ++ n.s) ; ---- | ss "" ;
        

        prSimpAdjToLAttrib ap = ap ;

        isPrToRAttr pred =
	  let ap = case pred.pol.p of {
             R.CPos => pred.ap ;
	     R.CNeg _ => mkAP (mkAdA "not") pred.ap
          }
	  in {
	  ap = ap ;
	  rs = mkRS (mkRCl which_RP ap) ; isAP = True
	  } ;
        doesPrToRAttr pred = {ap = variants {} ; rs = mkRS pred.pol (mkRCl which_RP pred.vp) ; isAP = False} ; 
        stmToRAttr s = {ap = variants {} ; rs = lin RS {s = \\_ => "such that" ++ s.s ; c = R.npNom} ; isAP = False} ; ---- 


        prClNounToNotion cn = cn ;
        prClNounRAttrToNotion cn attr = case attr.isAP of {
	  True => mkCN attr.ap cn ;
	  False => mkCN cn attr.rs
          } ;
        prLAttrClNounToNotion ap cn = mkCN ap cn ;
        prLAttrClNounRAttrToNotion ap cn attr = case attr.isAP of {
	  True => mkCN attr.ap (mkCN ap cn) ;
	  False => mkCN (mkCN ap cn) attr.rs
          } ;

}