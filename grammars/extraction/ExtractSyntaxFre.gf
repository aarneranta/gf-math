concrete ExtractSyntaxFre of ExtractSyntax =
  StructuralFre **
    ExtractSyntaxFunctor - [pmkV2, prefixCN] with
      (Syntax=SyntaxFre),
      (Symbolic=SymbolicFre),
      (Extend=ExtendFre) **
      open (P=ParadigmsFre) in {

   oper
     pmkV2 : V -> V2 = P.mkV2 ;
     prefixCN : Str -> CN -> CN = \s, cn ->
        cn ** {s = \\n => s ++ cn.s ! n} ;

     }
