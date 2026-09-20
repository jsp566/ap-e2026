import Data.Char (isDigit, ord, isSpace)
import Control.Monad (ap, liftM, void)

stringToInt1 :: String -> Int
stringToInt1 s = loop 0 s
  where
    loop acc [] = acc
    loop acc (c : cs) =
      let c' = ord c - ord '0'
       in loop (acc * 10 + c') cs

stringToInt2 :: String -> Maybe Int
stringToInt2 "" = Nothing
stringToInt2 s = loop 0 s
  where
    loop acc [] = Just acc
    loop acc (c : cs) =
      if isDigit c
        then
          let c' = ord c - ord '0'
           in loop (acc * 10 + c') cs
        else Nothing

stringToInt3 :: String -> Maybe Int
stringToInt3 "" = Nothing
stringToInt3 s = loop 0 s
  where
    loop acc [] = Just acc
    loop acc (c : cs) =
      if isDigit c
        then
          let c' = ord c - ord '0'
           in loop (acc * 10 + c') cs
        else Just acc

stringToInt4 :: String -> Maybe (Int, String)
stringToInt4 "" = Nothing
stringToInt4 (c:cs) =
  if isDigit c
  then loop 0 (c:cs)
  else Nothing
  where
    loop acc "" = Just (acc, "")
    loop acc (c : cs) =
      if isDigit c
        then
          let c' = ord c - ord '0'
           in loop (acc * 10 + c') cs
        else Just (acc, c:cs)

hasLeadingSpace :: String -> Maybe ((), String)
hasLeadingSpace (' ' : cs) = Just ((), cs)
hasLeadingSpace _ = Nothing

stringToInts1 :: String -> Maybe ((Int, Int), String)
stringToInts1 s =
  case stringToInt4 s of
    Nothing -> Nothing
    Just (x, s') ->
      case hasLeadingSpace s' of
        Just ((), s'') ->
          case stringToInt4 s'' of
            Just (y, s''') -> Just ((x,y), s''')
            Nothing -> Nothing
        Nothing ->
          Nothing

stringToInts2 :: String -> Maybe ((Int, Int), String)
stringToInts2 s = do
  (x, s') <- stringToInt4 s
  ((), s'') <- hasLeadingSpace s'
  (y, s''') <- stringToInt4 s''
  Just ((x,y), s''')

data Parser a =
  Parser ((Int,String) ->
           Either (Int, String)
           (a, (Int,String)))

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  -- pure :: a -> Parser a
  -- x :: a
  -- s :: String
  pure x = Parser (\s -> Right (x,s))
  (<*>) = ap

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  -- x' :: String -> Maybe (a, String)
  -- f :: a -> Parser b
  -- s :: String
  -- x'' :: a
  -- s' :: String
  Parser x' >>= f =
    Parser (\s ->
              case x' s of
                Left e -> Left e
                Right (x'', s') ->
                  -- f' :: String -> Maybe (b, String)
                  let Parser f' = f x''
                  in f' s')

instance MonadFail Parser where
  fail e = Parser (\(o, s) -> Left (o, e))

runParser :: Parser a -> String -> Either String a
runParser (Parser f) s =
  case f (0, s) of
    Left (o, e) ->
      Left ("Parse error at position " ++ show o ++ ": " ++ e)
    Right (x, _) -> Right x

next :: Parser Char
next = Parser $ \(o,s) ->
  case s of
    "" -> Left (o, s)
    c:cs -> pure (c, (o+1, cs))

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = try (do
  c <- next
  if p c then
    pure c
    else fail ("invalid character: " ++ [c]))

eof :: Parser ()
eof = Parser (\(o,s) ->
  case s of
    "" -> Right ((), (o, s))
    _ -> Left (o, "expected EOF"))

choice :: [Parser a] -> Parser a
choice [] = fail "no option succeeded"
choice (c:cs) = Parser (\s ->
                          let Parser c' = c
                          in case c' s of
                            Right (x, s') -> Right (x,s')
                            Left e ->
                              let Parser cs' = choice cs
                              in cs' s)

try :: Parser a -> Parser a
try (Parser p ) = Parser (\(o,s) ->
                            case p (o,s) of
                              Left (_, e) ->
                                Left (o, e)
                              Right x ->
                                Right x)

(<?>) :: Parser a -> String -> Parser a
Parser p <?> e = Parser (\(o,s) ->
                           case p (o,s) of
                             Left (o', _) ->
                               Left (o', e)
                             Right x -> Right x)

-- a*
many :: Parser a -> Parser [a]
many p = choice [do x <- p
                    xs' <- many p
                    pure (x:xs'),
                 pure []]

-- a+
some :: Parser a -> Parser [a]
some p = do x <- p
            xs <- many p
            pure (x:xs)

threeDigits :: Parser (Char, Char, Char)
threeDigits = do
  x <- satisfy isDigit
  y <- satisfy isDigit
  z <- satisfy isDigit
  pure (x,y,z)

parseDigit :: Parser Int
parseDigit = do
  c <- satisfy isDigit <?> "expected digit"
  pure (ord c - ord '0')

parseInt :: Parser Int
parseInt = do
  xs <- some parseDigit <?> "expected integer"
  pure (loop 0 xs)
  where loop acc (x:xs) = loop (acc * 10 + x) xs
        loop acc [] = acc

skipSpaces :: Parser ()
skipSpaces = void $ many $ satisfy isSpace

parseTwoInts :: Parser (Int, Int)
parseTwoInts = do
  x <- parseInt -- >>=
  skipSpaces
  y <- parseInt
  pure (x,y)

parseInts :: Parser [Int]
parseInts = many (parseInt <* skipSpaces)

-- This is what <* does
postfix :: Parser a -> Parser b -> Parser a
postfix p1 p2 = do
  x <- p1
  y <- p2
  pure x

getTwoInts :: String -> Either String (Int, Int)
getTwoInts s =
  runParser (skipSpaces *> parseTwoInts) s
