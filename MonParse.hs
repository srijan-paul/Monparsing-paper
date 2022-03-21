-- https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf
-- Haskell implementation of the monadic parser combination strategy
-- presented in the paper above. For simplicity, and to avoid subclassing
-- `MonadZero`, `Monad`, `Applicative` and `Functor`,
-- we use a custom operator `>>>` as if it were the monadic bind, and corresponding
-- equivalent functions to act as `return`, `zero` etc.

import Data.Char (isDigit, isLower, isUpper, ord)

-- Empty list -> Parse error
-- [(x, xs)] -> x is the parse tree and `xs` is the rest of the unparsed string.
type Parser a = String -> [(a, String)]

-- Produces the parse tree `v` and leaves the input string untouched.
result :: a -> Parser a
result v inp = [(v, inp)]

-- A parser that always fails
zero :: Parser a
zero _ = []

-- Consumes a single character from the input string
item :: Parser Char
item [] = []
item (x : xs) = [(x, xs)]

-- The `bind` operation.
-- If our Parser type was an actual Monad instance, this would be `>>=`
( >>> ) :: Parser a -> (a -> Parser b) -> Parser b
p >>> f = \inp -> concat [f v inp' | (v, inp') <- p inp]

-- Takes a predicate (a Boolean valued function),
-- and yields a parser that consumes a single character if it
-- satisfies the predicate, and fails otherwise
sat :: (Char -> Bool) -> Parser Char
sat p =
  item >>> \x ->
    if p x
      then result x
      else zero

-- Matches a specific character
char :: Char -> Parser Char
char x = sat (== x)

-- Matches a digit
digit :: Parser Char
digit = sat isDigit

-- Matches a lowercase character
lower :: Parser Char
lower = sat isLower

-- Matches a lowercase character
upper :: Parser Char
upper = sat isUpper

-- Applies two parsers to the same input, then returns a list
-- containing results returned by both of them.
plus :: Parser a -> Parser a -> Parser a
p `plus` q = \inp -> p inp ++ q inp

-- Consumes any letter.
letter :: Parser Char
letter = lower `plus` upper

-- Consumes a letter or a digit.
alphanum :: Parser Char
alphanum = letter `plus` digit

-- Consumes a string (no escape characters)
string :: String -> Parser String
string "" = result ""
string (x : xs) =
  char x >>> \_ ->
    string xs >>> \_ ->
      result (x : xs)

-- Applies two parsers `p` and `q` to the input string.
-- If `p` parses the string into something successful, its parse result is returned.
-- If `p` fails but `q` parses successfully, the parse result of `q` is returned.
-- If both fail, an empty list is returned.
(+++) :: Parser a -> Parser a -> Parser a
(p +++ q) inp = case (p `plus` q) inp of
  [] -> []
  (x : xs) -> [x]

-- Applies a parser `p` as many times as possible to the input string.
-- e.g - `many digit "123ABC"` returns [("123", "ABC")]
many :: Parser a -> Parser [a]
many p =
  ( p >>> \x -> -- Start by applying p once
      many p >>> \xs -> -- Then recursive apply `p` many times.
        result (x : xs) -- Finally, combine the results
  )
    +++ result [] -- In case `p` fails to apply either in the initial call, or in one of the
    -- recursive calls to itself, we return an empty list instead.

-- Applies 
word :: Parser String
word = many letter

-- Consumes a pattern that matches `[a-zA-Z][a-zA-Z0-9]*`
ident :: Parser String
ident =
  letter >>> \x -> -- one letter, followed by...
    many alphanum >>> \xs -> -- zero or more alphanumeric chars
      result (x : xs)

-- Not in the paper, just something I was playing around with.
-- Applies `p` first, then `q`, and returns the results in a 2-tuple.
then' :: Parser a -> Parser b -> Parser (a, b)
then' p q =
  p >>> \x ->
    q >>> \xs ->
      result (x, xs)

-- Same as many, but only works if `p` can be applied at least once. (`+` in regex)
many1 :: Parser a -> Parser [a]
many1 p =
  p >>> \x ->
    many p >>> \xs ->
      result (x : xs)

-- Consumes a natural number
nat :: Parser Int
nat =
  many1 digit >>> eval
  where
    eval xs = result $ foldl1 op [ord x - ord '0' | x <- xs]
    m `op` n = 10 * m + n

-- Takes two parsers `p` and `sep`.
-- Then applies `p` and `seq` alternatively, returning a list that
-- contains the parse results of `p`, separated by the parse results of `sep`.
sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep =
  p >>> \x ->  -- first, apply `p`
    many (sep >>> const p) -- To the rest of the string, apply `many (sep >>> const p)`
      >>> \xs -> result (x : xs) --  Then combine the two results into a list and return it.
