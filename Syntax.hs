module Syntax
   ( Term(..), var, cut
   , Atom(..)
   , Clause(..), rhs
   , VariableName(..), Atom, Goal, Program
   , cons, nil, foldr_pl
   , arguments -- FIXME Should not be exposed
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


data Term = Struct Atom [Term]
          | Var VariableName
          | Wildcard
          | Cut Int
      deriving (Eq, Data, Typeable,Show, Generic)

var = Var . VariableName 0
cut = Cut 0

data Clause = Clause { lhs :: Term, rhs_ :: [Goal] }
            | ClauseFn { lhs :: Term, fn :: [Term] -> [Goal] }
      deriving (Data, Typeable,Generic)
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

infixr 4 <=!
(q <=! _) (q->Just l) (q->Just r) = l <= r
(q <=! _) (q->Just _) _ = True
(q <=! _) _ (q->Just _) = False
(_ <=! c) x y = c x y

wildcards Wildcard = Just ()
wildcards _        = Nothing

variables (Var v) = Just v
variables _       = Nothing

numbers (Struct (reads . unAtom -> [(n :: Integer,"")]) []) = Just n
numbers _                                        = Nothing

atoms (Struct a []) = Just [a]
atoms _             = Nothing

compound_terms (Struct a ts) = Just (length ts, a, ts)
compound_terms _             = Nothing


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


prettyPrint True _ t@(Struct "," [_,_]) =
   "(" ++ prettyPrint False 0 t ++  ")"

prettyPrint f n (Struct (flip lookup operatorTable->Just (p,InfixOp assoc name)) [l,r]) =
   parensIf (n >= p) $ prettyPrint f n_l l ++ spaced name ++ prettyPrint f n_r r
     where (n_l,n_r) = case assoc of
                           AssocLeft  -> (p - 1, p)
                           AssocRight -> (p, p - 1)

prettyPrint f n (Struct (flip lookup operatorTable->Just (p,PrefixOp name)) [r]) =
   parensIf (n >= p) $ name ++ prettyPrint f (p {- Non-associative -}) r

prettyPrint _ _ t@(Struct "." [_,_]) =
   let (ts,rest) = g [] t in
      --case guard (isNil rest) >> sequence (map toChar ts) of
      --   Just str -> prettyPrint str
      --   Nothing  ->
            "[" ++ intercalate "," (map (prettyPrint True 0) ts) ++ (if isNil rest then "" else "|" ++ (prettyPrint True 0) rest) ++  "]"
   where g ts (Struct "." [h,t]) = g (h:ts) t
         g ts t = (reverse ts, t)
         isNil (Struct "[]" []) = True
         isNil _                = False

prettyPrint _ _ (Struct (Atom a) [])   = a
prettyPrint _ _ (Struct (Atom a) ts)   = a ++ "(" ++ intercalate ", " (map (prettyPrint True 0) ts) ++ ")"
prettyPrint _ _ (Var v)         = show v
prettyPrint _ _ Wildcard        = "_"
prettyPrint _ _ (Cut _)         = "!"
--prettyPrint _ _ ((==cut)->True) = "!"
--prettyPrint _ _ (Cut n)         = "!^" ++ show n


spaced s = let h = head s
               l = last s
           in spaceIf (isLetter h) ++ s ++ spaceIf (isLetter l || ',' == l)

spaceIf True  = " "
spaceIf False = ""

parensIf :: Bool -> String -> String
parensIf True  s = "(" ++ s ++")"
parensIf False s = s


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




foldr_pl f k (Struct "." [h,t]) = f h (foldr_pl f k t)
foldr_pl _ k (Struct "[]" [])   = k

cons t1 t2 = Struct "."  [t1,t2]
nil        = Struct "[]" []

data Operator = PrefixOp String
              | InfixOp Assoc String
data Assoc = AssocLeft
           | AssocRight

hierarchy :: Bool -> [[Operator]]
hierarchy ignoreConjunction =
   --[ [ InfixOp NonAssoc "-->", InfixOp NonAssoc ":-" ]
   [ [ infixR ";" ] ] ++
   (if ignoreConjunction then [] else [ [ infixR "," ] ])  ++
   [ [ prefix "\\+" ]
   , map infixL ["<", "=..", "=:=", "=<", "=", ">=", ">", "\\=", "is", "==", "@<", "@=<", "@>=", "@>"]
   , map infixL ["+", "-", "\\"]
   , [ infixL "*"]
   , [ infixL "mod" ]
   , [ infixL "div" ]
   , [ prefix "-" ]
   , [ prefix "$" ] -- used for quasi quotation
   ]
 where
   prefix = PrefixOp
   infixL = InfixOp AssocLeft
   infixR = InfixOp AssocRight


--infix 6 \\
--x \\ y = Struct "\\" [x,y]

arguments ts xs ds = ts ++ [ xs, ds ]
-- arguments ts xs ds = [ xs \\ ds ] ++ ts


