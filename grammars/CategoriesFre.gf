concrete CategoriesFre of Categories =
  IdentifiersLatex **
  CategoriesFunctor - [BaseArgKind, ConsArgKind]
  with
    (Utilities = UtilitiesFre),
    (Syntax = SyntaxFre),
    (Symbolic = SymbolicFre),
    (Markup = MarkupFre) **
  open
    ParadigmsFre
  in {

lin
  BaseArgKind kind = {
    sg = case kind.isPl of {
      True => toArgKindNP (mkNP aPl_Det (useKind kind)) ;
      False => toArgKindNP (mkNP aSg_Det (useKind kind))
      } ;
    neg = case kind.isPl of {
      True => toArgKindNP (mkNP no_Quant (useKind kind)) ;
      False => toArgKindNP (mkNP no_Quant (useKind kind))
      } ;
    pl = toArgKindNP (mkNP thePl_Det (useKind kind))
    } ;
    
  ConsArgKind kind kinds = {
    sg = case kind.isPl of {
      True => toArgKindNP (mkNP and_Conj (mkNP aPl_Det (useKind kind)) (fromArgKindNP kinds.sg)) ;
      False => toArgKindNP (mkNP and_Conj (mkNP aSg_Det (useKind kind)) (fromArgKindNP kinds.sg))
      } ;
    neg = case kind.isPl of {
      True => toArgKindNP (mkNP and_Conj (mkNP no_Quant (useKind kind)) (fromArgKindNP kinds.sg)) ;
      False => toArgKindNP (mkNP and_Conj (mkNP no_Quant (useKind kind)) (fromArgKindNP kinds.sg))
      } ;
    pl = toArgKindNP (mkNP and_Conj (mkNP thePl_Det (useKind kind)) (fromArgKindNP kinds.pl)) 
    } ;


}
