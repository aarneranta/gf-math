{-# LANGUAGE GADTs, KindSignatures, DataKinds, Rank2Types #-}
{-# LANGUAGE LambdaCase, PatternSynonyms #-}

module AnnotateDedukti where

import BuildConstantTable
import Dedukti.AbsDedukti
import DeduktiOperations
import CommonConcepts (lookupConstant, symbolicCats, verbalCats, mainCats)
import Environment

import PGF hiding (Hypo)

import Data.List (intersperse, sortOn)
import Data.Monoid (Sum(..))
import Control.Monad.Writer (WriterT(..), tell, censor, lift)
import qualified Data.Map as M
import qualified Data.Set as S


type DkTree a = Dedukti.AbsDedukti.Tree a

-- annotate Dk idents with cats and funs ; used only internally
annotateDkIdents :: Env -> DkTree a -> [DkTree a]
annotateDkIdents env =
  if force_symbolic || symbolic_latex
  then annot []
  else checkSymbolics . annot []
  

 where
  table = constantTable (symbolTable env)
  drops = dropTable (symbolTable env)
  msyns = ifvar $ argValueMaybeInt "-synonyms" (flags env)
  msymbs = ifvar $ argValueMaybeInt "-symbolics" (flags env)
  ifvar mi = if (isFlag "-variations" env) || (isFlag "-more-variants" env) then mi else Just 1
  force_symbolic = isFlag "-force-symbolic" env
  symbolic_latex = isFlag "-to-symbolic-latex" env

  -- don't annotate bound variables: they override constants
  annot :: forall a. [QIdent] -> DkTree a -> [DkTree a]
  annot bounds = map fst . runWriterT . annotV bounds

  -- each alternative comes with its verbality, the number of verbal constants
  -- in it, counted as it is built, as verbality would count them
  annotV :: forall a. [QIdent] -> DkTree a -> Annots (DkTree a)
  annotV bounds t = WriterT (spread (runWriterT (annotV0 bounds t)))

  annotV0 :: forall a. [QIdent] -> DkTree a -> Annots (DkTree a)
  annotV0 bounds t = case t of
    EApp fun arg -> case splitApp t of
      (EIdent c, args) | notElem c bounds -> do
        (f, p) <- lift (annotId c)
        case p of
          -- HOAS profiles take arguments apart, so they are annotated first,
          -- and the result is counted afresh, as some arguments may be dropped
          HoasProfile _ -> do
            aargs <- censor (const 0) (mapM (annotV bounds) args)
            let r = appProfile p (foldl EApp (EIdent f) aargs)
            tell (Sum (verbality r))
            return r
          -- otherwise only the arguments the profile keeps are annotated,
          -- and under a symbolic head only with nonverbal alternatives
          _ -> case splitApp (appProfile p (foldl EApp (EIdent f) args)) of
            (hd, kept) -> do
              tell (verbalIdent f)
              aargs <- mapM (prune f . annotV bounds) kept
              return (foldl EApp hd aargs)
      _ -> EApp <$> annotV bounds fun <*> annotV bounds arg
    EIdent c -> do
      c2 <- annotV bounds c
      tell (verbalIdent c2)
      return (EIdent c2)
    -- a pattern in a rewrite rule shows the arguments an expression shows
    PApp _ _ -> case splitPatt t of
      (PVar c, args) | notElem c bounds -> do
        (f, p) <- lift (annotId c)
        aargs <- mapM (annotV bounds) args
        return (appPattProfile p (foldl PApp (PVar f) aargs))
      _ -> composOpM (annotV bounds) t
    QIdent _ | notElem t bounds -> lift (map fst (annotId t))
    EAbs b exp -> EAbs <$> censor (const 0) (annotV bounds b)
                       <*> annotV (bind2ident b : bounds) exp
    EFun h exp -> EFun <$> annotV bounds h <*> annotV (hypo2topvars h ++ bounds) exp
    BVar _ -> return t
    BTyped v ty -> BTyped v <$> annotV bounds ty
    HVarExp v ty -> HVarExp v <$> annotV bounds ty
    HParVarExp v ty -> HParVarExp v <$> annotV bounds ty
    HLetExp v ty -> HLetExp v <$> annotV bounds ty
    HLetTyped v ty exp -> HLetTyped v <$> annotV bounds ty <*> annotV bounds exp
    -- the name of a judgement counts as verbality counts it
    JStatic _ _ -> named
    JDef _ _ _ -> named
    JThm _ _ _ -> named
    JInj _ _ _ -> named
    _ -> composOpM (annotV bounds) t
   where
     named = do
       r <- composOpM (annotV bounds) t
       tell (verbalIdent (jmtName r))
       return r
     jmtName :: DkTree b -> QIdent
     jmtName j = case j of
       JStatic c _ -> c
       JDef c _ _ -> c
       JThm c _ _ -> c
       JInj c _ _ -> c
       _ -> QIdent ""

  -- the alternatives of an argument of f that checkSymbolics would accept
  prune :: QIdent -> Annots Exp -> Annots Exp
  prune (QIdent f) us
    | force_symbolic || symbolic_latex = us
    | otherwise = case lookupConstant f of
        Just (cat, _) | S.member cat symbolicCats ->
          WriterT (filter ((== 0) . snd) (runWriterT us))
        _ -> us

  verbalIdent :: QIdent -> Sum Int
  verbalIdent (QIdent k) = case lookupConstant k of
    Just (kat, _) | S.member kat verbalCats -> 1
    _ -> 0

  -- the number of verbal constants in an annotated tree
  verbality :: DkTree a -> Int
  verbality u = length [k | QIdent k <- identsInTree u,
                            Just (kat, _) <- [lookupConstant k], S.member kat verbalCats]

  -- the alternatives of a subtree multiply up the tree, so a subtree keeps
  -- at most maxAnnots of them (0 = all): the most symbolic, the most verbal
  -- and ones spread evenly between them; -variations keeps all by default
  maxAnnots = case argValueMaybeInt "-max-annotations" (flags env) of
    Just k -> k
    _ | isFlag "-variations" env || isFlag "-more-variants" env -> 0
    _ -> defaultMaxAnnotations

  spread :: [(DkTree a, Sum Int)] -> [(DkTree a, Sum Int)]
  spread us
    | maxAnnots <= 0 || null (drop maxAnnots us) = us
    | maxAnnots == 1 = take 1 (sortOn snd us)
    | otherwise =
        let vs = sortOn snd us
            n = length vs
            picks = S.fromList [(i * (n-1)) `div` (maxAnnots-1) | i <- [0 .. maxAnnots-1]]
        in [u | (j, u) <- zip [0..] vs, S.member j picks]

  tkSyns = maybe id take msyns
  tkSymbs = maybe id take msymbs

  annotId c = case M.lookup c table of
    Just entry | symbolic_latex -> symbolicLatexId c entry
    Just entry -> [annotIdent c (maybe 0 id (M.lookup c drops)) fpt |
                       let symbs = tkSymbs (symbolics entry),
                       fpt <- symbs ++
		              if not (null symbs) && force_symbolic
			      then []
			      else tkSyns (primary entry : synonyms entry)
                       ]
    _ -> [(c, NoProfile)]

  -- in symbolic LaTeX, a constant is shown with its symbolic notation, or
  -- else with its core function (such as and, forall); failing both, it is
  -- left as it is, to be shown as \mathrm{c}, with all of its arguments but
  -- the ones #DROP hides: the verbal entries may leave out arguments that
  -- their wording implies, which a formula must show
  symbolicLatexId c entry = case tkSymbs (symbolics entry) of
    symbs@(_:_) -> [annotIdent c ndrop fpt | fpt <- symbs]
    _ -> case primary entry of
      fpt@(_, typ) | S.member (showCId (valCat typ)) mainCats -> [annotIdent c ndrop fpt]
      _ -> [(c, maybe NoProfile DropProfile (M.lookup c drops))]
   where
     ndrop = maybe 0 id (M.lookup c drops)

  checkSymbolics :: [DkTree a] -> [DkTree a]
  checkSymbolics ts = [t | t <- ts, not (badSymb t)]

  -- bad symbolics are subtrees with symbolic root and at least one verbal subtree
  badSymb :: DkTree a -> Bool
  badSymb t = case t of
    EApp _ _ -> case splitApp t of
      (EIdent (QIdent c), ts) -> case lookupConstant c of
        Just (cat, _) | S.member cat symbolicCats ->
          not (null
           [u | u <- ts, not (null [k | QIdent k <- identsInTree u,
            Just (kat, _) <- [lookupConstant k], S.member kat verbalCats])])
        _  -> any badSymb ts
      (f, ts) -> any badSymb (f : ts)
    EAbs b exp -> badSymb b || badSymb exp
    EFun h exp -> badSymb h || badSymb exp
    BTyped _ ty -> badSymb ty
    HVarExp _ ty -> badSymb ty
    HParVarExp _ ty -> badSymb ty
    HLetExp _ ty -> badSymb ty
    HLetTyped _ ty exp -> badSymb ty || badSymb exp
    JDef _ mt me -> badSymb mt || badSymb me
    JStatic _ ty -> badSymb ty
    JThm _ mt me -> badSymb mt || badSymb me
    JInj _ mt me -> badSymb mt || badSymb me
    JRules rules -> any badSymb rules
    RRule pattbinds patt exp -> any badSymb pattbinds || badSymb patt || badSymb exp
    MTExp exp -> badSymb exp
    MEExp exp -> badSymb exp
    ---- TODO patt
    _ -> False


  badSymbolics :: DkTree a -> [DkTree a]
  badSymbolics t = case t of
    EApp _ _ -> case splitApp t of
      (EIdent (QIdent c), ts) -> case lookupConstant c of
        Just (cat, _) | S.member cat symbolicCats ->
          [u | u <- ts, not (null [k | QIdent k <- identsInTree u,
            Just (kat, _) <- [lookupConstant k], S.member kat verbalCats])]
        _  -> concatMap badSymbolics ts
      (f, ts) -> concatMap badSymbolics (f : ts)
    _ -> composOpM badSymbolics t

-- alternative annotations, each with its verbality
type Annots a = WriterT (Sum Int) [] a

-- the default of -max-annotations
defaultMaxAnnotations :: Int
defaultMaxAnnotations = 256

annotIdent :: QIdent -> Int -> (FunProfile, Type) -> (QIdent, Profile)
annotIdent (QIdent s) d ((f, p), t) =
  (QIdent $ concat $ intersperse "#" $ [s, dk (valCat t), dkp f] ++ map dk (argCats t) ++ [showProfile p], p)
    where
      dk c = showCId c
      dkp f = showGFTree f

harmonizeJmt :: Jmt -> Jmt
harmonizeJmt jmt = case jmt of
  JDef ident (MTExp typ) meexp@(MEExp exp) ->
    let
       (hypos, kind) = splitType typ
       ahypos = unifyVars meexp hypos
       hvars = concatMap hypo2vars ahypos
       dexp = etaAppExp exp hvars
    in JDef ident (MTExp (foldr EFun kind ahypos)) (MEExp dexp)
  JDef ident (MTExp typ) MENone ->
    let
       (hypos, kind) = splitType typ
       ahypos = unifyVars MENone hypos
    in JDef ident (MTExp (foldr EFun kind ahypos)) MENone
  JStatic ident typ -> harmonizeJmt (JDef ident (MTExp typ) MENone)
  JThm ident mtyp mexp -> harmonizeJmt (JDef ident mtyp mexp)
  JInj ident mtyp mexp -> harmonizeJmt (JDef ident mtyp mexp)
  _ -> jmt


etaAppExp :: Exp -> [QIdent] -> Exp
etaAppExp fun args = foldr EAbs (foldl EApp (subst (zip bindvars (map EIdent args)) [] body) rest) ebinds
 where
   (binds, body) = splitAbs fun
   restargs = drop (length binds) args
   ebinds = binds ++ [BVar x | x <- restargs] ---- possible that x overshadow binds ? 
   bindvars = map bind2var ebinds
   rest = map EIdent restargs
   

subst :: [(QIdent, Exp)] -> [QIdent] -> Exp -> Exp
subst gamma bs e = case e of
  EIdent x {- | notElem x bs -} -> case lookup x gamma of
    Just v -> v
    _ -> e
  EApp f a -> case subst gamma bs f of
    EAbs bind body -> subst [(bind2ident bind, subst gamma bs a)] bs body -- beta-reduce redex from substituted higher-order arg
    f' -> EApp f' (subst gamma bs a)
  EAbs (BTyped x ty) a -> EAbs (BTyped x (subst gamma bs ty)) (subst gamma (x : bs) a)
  EAbs b a -> EAbs b (subst gamma (bind2ident b : bs) a)
  EFun (HVarExp x ty) a -> EFun (HVarExp x (subst gamma bs ty)) (subst gamma (x : bs) a)
  EFun (HParVarExp x ty) a -> EFun (HParVarExp x (subst gamma bs ty)) (subst gamma (x : bs) a)
  EFun h a -> EFun h (subst gamma (hypo2vars h ++ bs) a)
  _ -> e

-- add vars to hypos from abstraction or new vars if not yet given
unifyVars :: MExp -> [Hypo] -> [Hypo]
unifyVars mexp hypos = adds (zip vars hypos) where

  hypovars :: [Maybe QIdent]
  hypovars = map getVar hypos

  getVar :: Hypo -> Maybe QIdent
  getVar hypo = case hypo of
    HExp _ -> Nothing
    HVarExp x _  -> Just x
    HParVarExp x _ -> Just x
    HLetExp x _ -> Just x
    HLetTyped x _ _ -> Just x

  absvars :: [QIdent]
  absvars = case mexp of
    MEExp exp -> absIdents exp
    _ -> []

  allvars = absvars ++ [x | Just x <- hypovars]

  genvars = [QIdent s | s <- ["x", "y", "z", "u", "v", "w"] ++ ["X"  ++ show i | i <- [1..]]]

  vars = absvars ++ filter (\x -> notElem x allvars) genvars 
  
  adds :: [(QIdent, Hypo)] -> [Hypo]
  adds vshypos = case vshypos of
      (v, HExp exp) : vshs -> HVarExp v exp : adds vshs
      (_, h) : vshs -> h : adds vshs
      _ -> []


