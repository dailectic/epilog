{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Syntax
   ( Term(..,Var0)
   , Clause(..), rhs
   , VariableName(..), Atom(..), Goal, Program
   , cons, nil, foldr_pl
   , hierarchy
   , Operator(..), Assoc(..)
   , Display(..), DisplayShow(..),ShowDisplay(..)
   )
where

import Data.Generics (Data(..), Typeable(..))
import Data.List (intercalate)
import Data.Char (isLetter)
import GHC.Generics
import Data.String (IsString(..))


data Term = Struct Atom [Term] -- ^ Atomic application "foo(x,y,_,Z)"
          | Var VariableName -- ^ Named variable "X"
          | Wildcard -- ^ Unnamed variable "_"
          | Cut Int -- ^ Cut with backtracking limit (usually 0) "!" 
      deriving (Eq, Data, Typeable,Show)

-- | Create a named variable
pattern Var0 a = Var (VariableName 0 a)

data Clause = Clause   { lhs :: Term, rhs_ ::           [Goal] }
            | ClauseFn { lhs :: Term, fn   :: [Term] -> [Goal] }
      deriving (Data, Typeable,Generic)
-- | Extract the RHS function from a clause
rhs :: Clause -> [Term] -> [Goal]
rhs (Clause   _ rhs) = const rhs
rhs (ClauseFn _ fn ) = fn
instance Show Clause where
  show = \case
    Clause lhs rhs_ -> "Clause " ++ show (lhs,rhs_)
    ClauseFn lhs fn -> "ClauseFn " ++ show (lhs,"<<<fn>>>")

data VariableName = VariableName Int String
      deriving (Eq, Data, Typeable, Ord,Show)

newtype Atom         = Atom {unAtom :: String}
  deriving newtype (Show,Eq,IsString,Ord)
  deriving stock (Data,Typeable,Generic)
type Goal         = Term
type Program      = [Clause]

instance Ord Term where
   (<=) = wildcards <=! variables <=! atoms <=! compound_terms <=! error "incomparable"
     where wildcards = \case {Wildcard -> Just (); _ -> Nothing}
           variables = \case {Var v    -> Just v; _  -> Nothing}
           atoms = \case {Struct a [] -> Just [a]; _ -> Nothing}
           compound_terms = \case {Struct a ts -> Just (length ts, a, ts); _ -> Nothing}
           numbers   = \case Struct (Atom (reads @Integer -> [(n,"")])) [] -> Just n
                             _ -> Nothing

infixr 4 <=!
(q <=! _) (q->Just l) (q->Just r) = l <= r
(q <=! _) (q->Just _) _ = True
(q <=! _) _ (q->Just _) = False
(_ <=! c) x y = c x y

class Display a where display :: a -> String
newtype DisplayShow a = DisplayShow a
newtype ShowDisplay a = ShowDisplay a
instance Show a => Display (DisplayShow a) where display (DisplayShow a) = show a
instance Display a => Show (ShowDisplay a) where show (ShowDisplay a) = display a
deriving via DisplayShow Int instance Display Int
instance {-# overlapping #-} Display String where display = id
deriving via DisplayShow (Either (ShowDisplay a) (ShowDisplay b))
  instance (Display a, Display b) => Display (Either a b)
deriving via DisplayShow [ShowDisplay a]
  instance Display a => Display [a]

instance Display Term where display = prettyPrint False 0

-- | Display a term, optionally surrounding it in parentheses and specified nesting depth
prettyPrint :: Bool -> Int -> Term -> String
prettyPrint True _ t@(Struct (Atom ",") [_,_]) =
   "(" ++ prettyPrint False 0 t ++  ")"

prettyPrint f n (Struct (flip lookup operatorTable->Just (p,InfixOp assoc name)) [l,r]) =
   parensIf (n >= p) $ prettyPrint f n_l l ++ spaced name ++ prettyPrint f n_r r
     where (n_l,n_r) = case assoc of
                           AssocLeft  -> (p - 1, p)
                           AssocRight -> (p, p - 1)

prettyPrint f n (Struct (flip lookup operatorTable->Just (p,PrefixOp name)) [r]) =
   parensIf (n >= p) $ name ++ prettyPrint f (p {- Non-associative -}) r

prettyPrint _ _ t@(Struct (Atom ".") [_,_]) =
   let (ts,rest) = g [] t in
      --case guard (isNil rest) >> sequence (map toChar ts) of
      --   Just str -> prettyPrint str
      --   Nothing  ->
            "[" ++ intercalate "," (map (prettyPrint True 0) ts) ++ (if isNil rest then "" else "|" ++ (prettyPrint True 0) rest) ++  "]"
   where g ts (Struct (Atom ".") [h,t]) = g (h:ts) t
         g ts t = (reverse ts, t)
         isNil (Struct (Atom "[]") []) = True
         isNil _                = False

prettyPrint _ _ (Struct (Atom a) [])   = a
prettyPrint _ _ (Struct (Atom a) ts)   = a ++ "(" ++ intercalate ", " (map (prettyPrint True 0) ts) ++ ")"
prettyPrint _ _ (Var v)         = show v
prettyPrint _ _ Wildcard        = "_"
prettyPrint _ _ (Cut _)         = "!"

-- | Add preceeding and trailing spaces to words, or trailing commas
spaced :: String -> String
spaced s = spaceIf (isLetter h) ++ s ++ spaceIf (isLetter l || ',' == l)
 where h = head s; l = last s
       spaceIf p = if p then " " else ""

-- | Enclose a string in parents if the predicate holds
parensIf :: Bool -> String -> String
parensIf p s = if p then "(" ++ s ++")" else s


-- | Lookup table for Operators with precidence
operatorTable :: [(Atom, (Int,Operator))]
operatorTable = concat $ zipWith (map . g) [1..] $ hierarchy False
 where g p op@(InfixOp _ name) = (Atom name,(p,op))
       g p op@(PrefixOp name)  = (Atom name,(p,op))

instance Display VariableName where
   display (VariableName 0 v) = v
   display (VariableName i v) = v ++ "#" ++  display i

instance Display Clause where
   display (Clause   lhs [] ) = display $ display lhs
   display (Clause   lhs rhs) = display $ display lhs ++ " :- " ++ intercalate ", " (map display rhs)
   display (ClauseFn lhs _  ) = display $ display lhs ++ " :- " ++ "<Haskell function>"

-- | Fold over a list of terms
foldr_pl :: (Term -> t -> t) -> t -> Term -> t
foldr_pl f t0 = \case Struct (Atom ".") [first,rest] -> f first $ foldr_pl f t0 rest
                      Struct (Atom "[]") []          ->  t0
                      t                       -> error $ "foldr_pl: Term " ++ show t ++ " is not a list"

-- | Concatenate Terms in a list
cons :: Term -> Term -> Term
cons t1 t2 = Struct (Atom ".")  [t1,t2]
-- | An empty list of Terms
nil :: Term
nil        = Struct (Atom "[]") []

data Operator = PrefixOp String | InfixOp Assoc String
data Assoc = AssocLeft | AssocRight

-- | heirarchy of operator precidence. Descending lists bind more tightly
hierarchy :: Bool -> [[Operator]]
hierarchy ignoreConjunction =
   [ [ infixR ";" ] ] ++
   (if ignoreConjunction then [] else [ [ infixR "," ] ])  ++
   [ [ PrefixOp "\\+" ]
   , map infixL ["<", "=..", "=:=", "=<", "=", ">=", ">", "\\=", "is", "==", "@<", "@=<", "@>=", "@>"]
   , map infixL ["+", "-", "\\"]
   , [ infixL "*"]
   , [ infixL "mod" ]
   , [ infixL "div" ]
   , [ PrefixOp "-" ]
   , [ PrefixOp "$" ] -- used for quasi quotation
   ]
 where infixL = InfixOp AssocLeft; infixR = InfixOp AssocRight
