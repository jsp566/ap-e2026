import Control.Monad (ap, liftM)
import Data.Char (isSpace, isAlpha)

data Parser a
  = Parser (String -> Maybe (a, String))

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure x = Parser (\s -> Just (x, s))
  (<*>) = ap

instance Monad Parser where
  Parser x' >>= f =
    Parser
      ( \s ->
          case x' s of
            Nothing -> Nothing
            Just (x'', s') ->
              let Parser f' = f x''
               in f' s'
      )

instance MonadFail Parser where
  fail _ = Parser (\_ -> Nothing)

runParser :: Parser a -> String -> Maybe (a, String)
runParser (Parser f) s = f s

next :: Parser Char
next = Parser $ \s ->
  case s of
    "" -> Nothing
    c : cs -> Just (c, cs)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  c <- next
  if p c
    then
      pure c
    else fail ("invalid character: " ++ [c])

eof :: Parser ()
eof =
  Parser
    ( \s ->
        case s of
          "" -> Just ((), s)
          _ -> Nothing
    )

notFollowedBy :: Parser a -> Parser ()
notFollowedBy (Parser p2) =
  Parser (\s ->
             case p2 s of
               Nothing -> Just ((), s)
               Just _ -> Nothing
            )

choice :: [Parser a] -> Parser a
choice [] = fail "no option succeeded"
choice (c : cs) =
  Parser
    ( \s ->
        let Parser c' = c
         in case c' s of
              Just (x, s') -> Just (x, s')
              Nothing ->
                let Parser cs' = choice cs
                 in cs' s
    )

many :: Parser a -> Parser [a]
many p =
  choice
    [ do
        x <- p
        xs' <- many p
        pure (x : xs'),
      pure []
    ]

some :: Parser a -> Parser [a]
some p = do
  x <- p
  xs <- many p
  pure (x : xs)

data BExp = Lit Bool
          | Not BExp
          | Var String
          | And BExp BExp
          | Or BExp BExp
          deriving (Eq, Ord, Show)

-- var = ? any nonempty sequence of alphabetic chars ?;
-- bexp ::= "true"
--        | "false"
--        | var
--        | "not" bexp
--        | bexp "and" bexp
--        | bexp "or" bexp;
--
-- not binds tightest, and binds tighter than or.
--
-- all operators are left-associative.
--
-- Allows arbitrary whitespace between tokens.

-- Examples:
--
-- true
-- true and false
-- x and y or z
-- not x and y

spaces :: Parser ()
spaces = do _ <- many (satisfy isSpace)
            pure ()

lexeme :: Parser a -> Parser a
lexeme p = do x <- p
              spaces
              pure x

lKeyword :: String -> Parser ()
lKeyword s = lexeme $ do
  loop s
  notFollowedBy (satisfy isAlpha)
  where loop "" = pure ()
        loop (c:cs) = do _ <- satisfy (==c)
                         loop cs

pTrue :: Parser ()
pTrue = lKeyword "true"

pFalse :: Parser ()
pFalse = lKeyword "false"

pAnd :: Parser ()
pAnd = lKeyword "and"

pOr :: Parser ()
pOr = lKeyword "or"

pNot :: Parser ()
pNot = lKeyword "not"

lLeftParen :: Parser ()
lLeftParen = lexeme (do _ <- (satisfy (=='('))
                        pure () )

lRightParen :: Parser ()
lRightParen = lexeme (do _ <- (satisfy (==')'))
                         pure ())

parens :: Parser a -> Parser a
parens p = lLeftParen *> p <* lRightParen

keywords :: [String]
keywords = ["false", "true", "and", "or", "not"]

lVar :: Parser String
lVar = lexeme (do s <- some (satisfy isAlpha)
                  if elem s keywords
                    then fail "keyword found"
                    else pure s)

pBool :: Parser Bool
pBool = choice [ do pTrue
                    pure True,
                 do pFalse
                    pure False
               ]

-- modified to remove left recursion:
--
-- atom ::= "true"
--        | "false"
--        | var
--        | "not" atom
--        | "(" bexp1 ")"
--
--
-- bexp2' ::= "and" atom bexp2'
--        |
-- bexp2 :: = atom bexp2'
--
-- bexp1' ::= "or" bexp2 bexp1'
--          |
-- bexp1 :: = bexp2 bexp1'


pAtom :: Parser BExp
pAtom = choice [ do v <- lVar
                    pure (Var v),
                 do pNot
                    x <- pAtom
                    pure (Not x),
                 do x <- pBool
                    pure (Lit x),
                 parens pBExp1
               ]

pBExp2' :: BExp -> Parser BExp
pBExp2' x = choice [ do pAnd
                        y <- pAtom
                        pBExp2' (And x y),
                     pure x
                   ]

pBExp2 :: Parser BExp
pBExp2 = do x <- pAtom
            pBExp2' x

pBExp1' :: BExp -> Parser BExp
pBExp1' x = choice [ do pOr
                        y <- pBExp2
                        pBExp1' (Or x y),
                    pure x]

pBExp1 :: Parser BExp
pBExp1 = do x <- pBExp2
            pBExp1' x
