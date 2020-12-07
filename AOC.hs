{-# LANGUAGE ExistentialQuantification, OverloadedStrings, DeriveFunctor #-}
module AOC
  ( Solution (..)
  , runSolution
  , execSolution

  , Error (..)
  , err
  , withAttributes

  , AOCM

  -- * Input
  , readLinesFromFile
  , parseDecimal


  -- Parse text with a quick applicative parser
  , Parser ()
  , parse

  , failure
  , success
  , anyChar
  , satisfy
  , charIs
  , textIs
  , eof
  , alternatives
  , sepBy0
  , sepBy1
  , many0
  , many1
  , word
  , newline
  , space
  , digit
  , hexDigit
  )
  where

import Data.Text (Text)
import qualified Data.Text      as Text
import qualified Data.Text.IO   as Text
import qualified Data.Text.Read as Text

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Applicative

-- | An Advent Of Code computation Monad is either a structured Error or a
-- result 'a'.
type AOCM a = Either Error a

-- | All(?) solutions take the form of:
data Solution one two = forall input. Solution
  { _parse   :: [Text] -> AOCM input  -- ^ Some input lines parsed from a file.
  , _partOne :: input  -> AOCM one    -- ^ A function that solves part one, given the parsed input.
  , _partTwo :: input  -> AOCM two    -- ^ A function that solves part two, given the parsed input.
  }

-- | Errors consist of an informative message and a map of metadata.
--
-- You may want to construct with 'err' for plain messages.
-- Add attributes to an 'Error' with 'withAttributes'.
data Error = Error
  { _message    :: Text
  , _attributes :: Map Text Text
  }
  deriving (Eq, Ord)

-- | Pretty-print errors by displaying their message, then attempting to pad out
-- the error attributes like:
--
-- ERROR: Failed to parse number
--  expected: an numeric character
--  got:      end of input
instance Show Error where
  show (Error msg attrs) =
    let keyVals :: [(Text,Text)]
        keyVals = Map.toList attrs

        longestKey :: Int
        longestKey = maximum . map (Text.length . fst) $ keyVals

        formattedAttributes :: [[Text]]
        formattedAttributes = map (\(key, val)
                                    -> [ " "
                                       , key
                                       , ":"
                                       , Text.replicate (longestKey - Text.length key) " "
                                       , val
                                       , "\n"
                                       ]
                                  )
                                  keyVals
     in Text.unpack
      . mconcat
      $ (["ERROR: "
         , msg
         , "\n"
         ]
         <> concat formattedAttributes)

-- | Construct an 'Error' with a message (and no attributes).
err :: Text -> Error
err msg = Error msg Map.empty

-- | Add a collection of attributes to an 'Error'. Attribute keys that collide
-- have their value replaced by the newer association.
withAttributes :: Error -> [(Text,Text)] -> Error
withAttributes (Error msg attr) newAttr = Error msg (Map.union attr (Map.fromList newAttr))

{- Some manual parsing functions, for transforming file input for use in
 - solutions. More complete 'Parser's come later.
 -}

-- | Read newline separated lines of text from a file name.
readLinesFromFile :: Text -> IO [Text]
readLinesFromFile filename = Text.lines <$> Text.readFile (Text.unpack filename)

-- | Text must parse as a valid decimal Int.
parseDecimal :: Text -> AOCM Int
parseDecimal txt = case Text.decimal txt of
  Left err
    -> Left $ Error (Text.pack err) Map.empty

  Right (d, leftovers)
    | (not . Text.null $ leftovers)
    -> Left $ err "Leftovers parsing decimal" `withAttributes`
              [("leftovers", leftovers)]

    | otherwise
    -> Right d

-- | To run a solution, the input lines are parsed and supplied to the two parts
-- in order.
-- If part one fails, part two will not be attempted.
runSolution :: [Text] -> Solution one two -> AOCM (one,two)
runSolution inputText (Solution parse runOne runTwo) = do
  input <- parse inputText
  one   <- runOne input
  two   <- runTwo input
  pure (one,two)

-- | Run a solution, and then print the Error/ successful results to stdout.
execSolution :: (Show one, Show two) => [Text] -> Solution one two -> IO ()
execSolution inputText solution = case runSolution inputText solution of
  Left err
    -> print err

  Right (part1, part2)
      -> do putStrLn "Part 1"
            print part1

            putStrLn "Part 2"
            print part2

-- | A 'Parser' reads 'Text' with:
-- - Possible leftovers
-- - And either:
--   - Fails with an 'Error'
--   - Or sucessfully parses a value 'a'.
--
-- Parser can be:
--
-- - Mapped over as a functor
-- > length <$> word
-- 10
--
-- - Combined sequentially with applicative functors
-- > (\word1 word2 -> (word1, word2)) <$> word <*> (space *> word)
-- ("foo", "bar")
--
-- - Sequenced Monadically with do notation
-- > do word1 <- word
--      space
--      if length word1 == 3
--        then do word2 <- word
--                return (word1, word2)
--        else return (word1, "nope")
--  ("foo","nope")
--
-- - Given alternatives with backtracking
-- > word <|> digit
-- 1
newtype Parser a = Parser {_runParser :: Text -> (Text, AOCM a)}
  deriving Functor

-- | Combine 'Parser's sequentially like:
--
-- Combined sequentially with applicative functors
-- > (\word1 word2 -> (word1, word2)) <$> word <*> (space *> word)
-- ("foo", "bar")
instance Applicative Parser where
  pure = success

  (Parser f) <*> (Parser a) = Parser $ \txt ->
    let (txt', eF) = f txt
     in case eF of
          Left err
            -> (txt', Left err)
          Right f
            -> let (txt'', eA) = a txt'
                in case eA of
                     Left err
                       -> (txt'', Left err)

                     Right a
                       -> (txt'', Right $ f a)

-- | Combine alternatives with backtracking
--
-- I.E. If the first 'Parser' consumes input but fails, pretend that input was
-- not consumed when trying the second.
--
-- > word <|> digit
-- 1
instance Alternative Parser where
  empty = failure (err "failure")

  (Parser l) <|> (Parser r) = Parser $ \txt ->
    case l txt of
      (txt', Right a)
        -> (txt', Right a)
      _
        -> r txt

-- - Sequenced 'Parser's Monadically, such that subsequent 'Parser's can depend
--   on the value of previous parsers.
--
-- > do word1 <- word
--      space
--      if length word1 == 3
--        then do word2 <- word
--                return (word1, word2)
--        else return (word1, "nope")
--  ("foo","nope")
instance Monad Parser where
  return = pure
  (Parser a) >>= fab = Parser $ \txt -> case a txt of
    (txt', Right a')
      -> let Parser f = fab a'
          in f txt'

    (txt', Left err)
      -> (txt', Left err)

-- | Consume no input but fail with an Error message.
failure :: Error -> Parser a
failure err = Parser $ \txt -> (txt, Left err)

-- | Consume no input but succeed with a value.
success :: a -> Parser a
success a = Parser $ \txt -> (txt, Right a)

-- | Run a Parser that only succeeds if:
-- - A value is parsed.
-- - There are no trailing leftovers.
parse :: Parser a -> Text -> AOCM a
parse (Parser f) txt = case f txt of
  ("", Right a)
    -> Right a

  (leftovers, Right _)
    -> Left $ err "parsed successfully but leftover input"
            `withAttributes` [("leftovers", "\""<>leftovers<>"\"")]

  (leftovers, Left err)
    -> Left $ err
            `withAttributes` [("leftovers", leftovers)]

-- | Return any single character - unless end of input.
anyChar :: Parser Char
anyChar = Parser $ \txt -> case Text.uncons txt of
  Nothing
    -> (txt, Left . err $ "no input remaining, required any character")

  Just (c,txt')
    -> (txt', Right c)

-- | Only succeed if there is no remaining input.
eof :: Parser ()
eof = Parser $ \txt -> case Text.uncons txt of
  Nothing
    -> ("", Right ())

  Just _
    -> (txt, Left . err $ "expected eof but there was more input")

-- | Only succeed if the next character matches the one provided.
charIs :: Char -> Parser ()
charIs target = do
  c <- anyChar
  if c == target
    then pure ()
    else failure (err "character is not expected" `withAttributes` [("expected", Text.pack . show $ target)])

-- | Only succeed if the next character matches the predicate.
satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser $ \txt -> case Text.uncons txt of
  Nothing
    -> ("", Left $ err "looking for a character to satisfy a predicate, but no more are available")

  Just (c,txt')
    | pred c
     -> (txt', Right c)

    | otherwise
     -> (txt', Left $ err "Character does not satisfy predicate")

-- | Match a simple newline character.
newline :: Parser ()
newline = charIs '\n'

-- | Match a single regular space character.
space :: Parser ()
space = charIs ' '

-- | Succeed if the next input exactly matches a string of Text.
--
-- If it doesn't match then backtrack.
textIs :: Text -> Parser ()
textIs target = Parser $ \txt ->
  if Text.isPrefixOf target txt
    then (Text.drop (Text.length target) txt, Right ())
    else (txt, Left $ err "expected text was not found"
                    `withAttributes` [("expected", target)])

-- | Try alternatives one by one, backtracking on individual failure and
-- returning the first match.
alternatives :: [Parser a] -> Parser a
alternatives = foldr (<|>) (failure (err "no alternatives matched"))

-- | Match 0 or many 'a's with some separator.
sepBy0 :: Parser a -> Parser () -> Parser [a]
sepBy0 p sep = sepBy1 p sep <|> pure []

-- | Match 1 or many 'a's with some separator.
sepBy1 :: Parser a -> Parser () -> Parser [a]
sepBy1 p sep = do
  a  <- p
  as <- many (sep *> p)
  pure (a:as)

-- | Match 0 or many 'a's.
many0 :: Parser a -> Parser [a]
many0 = many

-- | Match 1 or many 'a's.
many1 :: Parser a -> Parser [a]
many1 p = do
  a  <- p
  as <- many p
  pure (a:as)

-- | A word is a many characters, terminated by a space or newline.
word :: Parser Text
word = Parser $ \txt ->
  let res = Text.takeWhile (not . (`elem` [' ', '\n'])) txt
   in (Text.drop (Text.length res) txt, Right res)

-- | A single digit 0..9.
digit :: Parser Int
digit = alternatives
  [charIs '0' *> pure 0
  ,charIs '1' *> pure 1
  ,charIs '2' *> pure 2
  ,charIs '3' *> pure 3
  ,charIs '4' *> pure 4
  ,charIs '5' *> pure 5
  ,charIs '6' *> pure 6
  ,charIs '7' *> pure 7
  ,charIs '8' *> pure 8
  ,charIs '9' *> pure 9
  ]

-- | A single hex-digit 0..9,a..f
hexDigit :: Parser Char
hexDigit = alternatives
  [charIs '0' *> pure '0'
  ,charIs '1' *> pure '1'
  ,charIs '2' *> pure '2'
  ,charIs '3' *> pure '3'
  ,charIs '4' *> pure '4'
  ,charIs '5' *> pure '5'
  ,charIs '6' *> pure '6'
  ,charIs '7' *> pure '7'
  ,charIs '8' *> pure '8'
  ,charIs '9' *> pure '9'

  ,charIs 'a' *> pure 'a'
  ,charIs 'b' *> pure 'b'
  ,charIs 'c' *> pure 'c'
  ,charIs 'd' *> pure 'd'
  ,charIs 'e' *> pure 'e'
  ,charIs 'f' *> pure 'f'
  ]

