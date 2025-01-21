abstract ConstantBase = {

cat
  Noun ; -- Kind
  Set ;  -- Kind + symbol
  Adj ;  -- Exp -> Prop
  Rel ;  -- Exp -> Exp -> Prop
  Name ; -- Exp
  Fun ;  -- [Exp] -> Exp
  Label ; -- Exp
  Const ; -- Exp + symbol
  Oper ;  -- Exp -> Exp -> Exp + symbol
  Compar ; -- Exp -> Exp -> Prop + symbol

}