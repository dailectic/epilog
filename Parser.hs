
module Parser
   ( consult, consultString, parseQuery
   , program, whitespace, comment, clause, terms, term, bottom, vname
   ) where
import Text.Parsec
import Text.Parsec.Expr hiding (Assoc(..))
import qualified Text.Parsec.Expr as Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Control.Applicative ((<$>),(<*>),(<$),(<*))
import Control.Exception (Exception,throwIO)

import Syntax

deriving via DisplayShow ParseError instance Display ParseError
type Parser = Parsec String ()

instance Exception ParseError
-- | Read a program database from a file. Throws 'ParseError'
consult :: FilePath -> IO Program
consult fp = either throwIO pure . consultString =<< readFile fp

-- | Read a program database from a string
consultString :: String -> Either ParseError Program
consultString = parse (do whitespace; p <- program; eof; return p) "(input)"

-- | Parse an interactive query
parseQuery :: String -> Either ParseError [Term]
parseQuery = parse (whitespace >> terms <* eof) "(query)"

-- | Parse a complete program
program :: Parser [Clause]
program = many (clause <* char '.' <* whitespace)

whitespace, comment :: Parser ()
whitespace = skipMany (comment <|> skip space <?> "")
comment = skip $ choice
   [ string "/*" >> (manyTill anyChar $ try $ string "*/")
   , char '%' >> (manyTill anyChar $ try $ skip newline <|> eof)
   ]
skip :: Parser a -> Parser ()
skip = (() <$)

-- | Parse a single Clause
clause :: Parser Clause
clause = do t <- struct <* whitespace
            dcg t <|> normal t
   where
      -- |Parse a normal Clause
      normal :: Term -> Parser Clause
      normal t = do
            ts <- option [] $ do string ":-" <* whitespace
                                 terms
            return (Clause t ts)

      -- | Parse a Definite Clause Grammar
      dcg :: Term -> Parser Clause
      dcg t = do string "-->"; whitespace
                 translate . (t,) <$> terms

      -- | Translate a Definite Clause Grammar spec into a Clause
      translate :: (Term,[Term]) -> Clause
      translate ((Struct aa@(Atom a) ts), rhs) =
         let lhs' = Struct aa (arguments ts (head vars) (last vars))
             vars = map (Var0.("d_"++).(a++).show) [0..length rhs] -- We explicitly choose otherwise invalid variable names
             rhs' = zipWith3 translate' rhs vars (tail vars)
         in Clause lhs' rhs'

      translate' :: Term -> Term -> Term -> Term
      translate' t s s0 | isList t   = Struct "=" [ s, foldr_pl cons s0 t ]     -- Terminal
      translate' t@(Struct "{}" ts) s s0 = foldr and (Struct "=" [ s, s0 ]) ts  -- Braced terms
      translate' (Struct a ts)  s s0 = Struct a (arguments ts s s0)             -- Non-Terminal

      and x y = Struct "," [x,y]
      
      isList = \case Struct "." [_,_] -> True
                     Struct "[]" []   -> True
                     _                -> False

      arguments :: [a] -> a -> a -> [a]
      arguments ts xs ds = ts ++ [ xs, ds ]

-- | Parse a somma seperated list of terms
terms :: Parser [Term]
terms = sepBy1 termWithoutConjunction (charWs ',')

-- | Parse a single Term
term, termWithoutConjunction :: Parser Term
term                   = term' False
termWithoutConjunction = term' True

-- | Parse a single Term possibly rejecting conjunctions
term' :: Bool -> Parser Term
term' ignoreConjunction = buildExpressionParser (reverse $ map (map toParser) $ hierarchy ignoreConjunction) (bottom <* whitespace)

-- | Parse a raw term without any 
bottom :: Parser Term
bottom = variable
      <|> struct
      <|> list
      <|> stringLiteral
      <|> Cut 0 <$ char '!'
      <|> Struct "{}" <$> between (charWs '{') (char '}') terms
      <|> between (charWs '(') (char ')') term

toParser (PrefixOp name)      = Prefix (reservedOp name >> return (\t -> Struct (Atom name) [t]))
toParser (InfixOp assoc name) = Infix  (reservedOp name >> return (\t1 t2 -> Struct (Atom name) [t1, t2]))
                                       (case assoc of AssocLeft  -> Parsec.AssocLeft
                                                      AssocRight -> Parsec.AssocRight)
-- | Parse a reserved keyword operation
reservedOp :: String -> Parser ()
reservedOp = P.reservedOp $ P.makeTokenParser $ emptyDef
   { P.opStart = oneOf ";,<=>\\i*+m@"
   , P.opLetter = oneOf "=.:<+"
   , P.reservedOpNames = operatorNames
   , P.caseSensitive = True
   }

-- | Parse a Char and strip trailing whitespace
charWs :: Char -> Parser Char
charWs c = char c <* whitespace

-- | List of all reserved operators
operatorNames :: [String]
operatorNames = [ ";", ",", "<", "=..", "=:=", "=<", "=", ">=", ">", "\\=", "is", "*", "+", "-", "\\", "mod", "div", "\\+" ]

-- | Parse an unnamed or named variable
variable :: Parser Term
variable = (Wildcard <$ try (char '_' <* notFollowedBy (alphaNum <|> char '_')))
       <|> Var <$> vname
       <?> "variable"

-- | Parse a legal variable name
vname :: Parser VariableName
vname = VariableName 0 <$> ((:) <$> upper    <*> many  (alphaNum <|> char '_') <|>
                            (:) <$> char '_' <*> many1 (alphaNum <|> char '_'))
atom :: Parser Atom
atom = Atom <$> ((:) <$> lower <*> many (alphaNum <|> char '_')
   <|> many1 digit
   <|> choice (map string operatorNames)
   <|> many1 (oneOf "#$&*+/.<=>\\^~")
   <|> between (char '\'') (char '\'') (many (noneOf "'"))
   <?> "atom")


-- | Parse a Struct Term
struct :: Parser Term
struct = do a <- atom
            ts <- option [] $ between (charWs '(') (char ')') terms
            return (Struct a ts)

-- | Parse a list of Terms
list :: Parser Term
list = between (charWs '[') (char ']') $
         flip (foldr cons) <$> option []  terms
                           <*> option nil (charWs '|' >> term)

-- | Parse a string literal
stringLiteral :: Parser Term
stringLiteral = foldr cons nil . map representChar <$> between (char '"') (char '"') (try (many (noneOf "\"")))

-- | Represent a character literal.
-- This is the classical Prolog representation of chars as code points.
representChar :: Char -> Term
representChar c = Struct (Atom $ show (fromEnum c)) [] 
