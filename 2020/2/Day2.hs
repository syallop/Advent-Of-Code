{-# LANGUAGE OverloadedStrings #-}
{-
TODO: Still manually splitting Text in this part. Using 'Parser' would be slightly nicer.
-}
module Main where

import AOC

import Data.Text (Text)
import qualified Data.Text as Text

solution :: Solution Int Int
solution = Solution
  { _parse   = mapM parsePassword
  , _partOne = partOne
  , _partTwo = partTwo
  }

main :: IO ()
main = do
  let inputFile   = "2020/2/passwords"

  input <- readLinesFromFile inputFile
  execSolution input solution

-- | A Password is a rule the password must adhear to, and the password text
-- itself.
--
-- How the rule is interpreted is.. flexible.
data Password = Password
  { _rule     :: Rule
  , _password :: Text
  }
  deriving Show

-- | A Rule has a constraint on a single Character with some 'lower' and 'upper'
-- property that is open to interpretation.
--
-- In one case this is a lower and upper character count.
-- In another context this could be a requirement that the character exist at
-- XOR each index.
data Rule = Rule
  { _lower     :: Int
  , _upper     :: Int
  , _character :: Char
  }
  deriving Show

-- | Break a piece of Text in two on a given needle which is NOT included in
-- either result part.
breakOn :: Text -> Text -> AOCM (Text, Text)
breakOn break input =
  let (left, breakAndRight) = Text.breakOn break input
      right = Text.drop (Text.length break) breakAndRight
   in pure (left, right)

-- | Require a single character of any kind can be parsed.
parseChar :: Text -> AOCM Char
parseChar txt = case Text.uncons txt of
  Nothing
    -> Left $ err "Cannot parse a single character - none remaining"

  Just (c,remainder)
    | (not . Text.null $ remainder)
    -> Left $ err "Cannot parse ONLY a single character - some remaining after"

    | otherwise
    -> Right c

-- | To parse a password we can break apart the text on the various delimiting
-- symbols.
parsePassword :: Text -> AOCM Password
parsePassword passwordLine = do
  (ruleFragment, password) <- breakOn ":" passwordLine
  (lower, rest)            <- breakOn "-" ruleFragment
  (upper, charText)        <- breakOn " " rest

  rule <- Rule <$> parseDecimal lower <*> parseDecimal upper <*> parseChar charText
  Password <$> pure rule <*> pure password

-- | An interpretation of a 'Password' rule where the 'Rule' specifies that it's
-- character must appear between 'lower' and 'upper' times in the text.
validByCount :: Password -> Bool
validByCount (Password (Rule lower upper character) password) = (between lower upper) . Text.length . Text.filter (== character) $ password

-- | With an inclusive lower and upper bound, is a number contained between
-- them?
between :: Int -> Int -> Int -> Bool
between lower upper n = and [ lower <= n
                            , n     <= upper
                            ]

-- | Part one validates passwords by counting the number of times characters
-- appear.
partOne :: [Password] -> AOCM Int
partOne = pure . length . filter validByCount


-- | An interpretation of a 'Password' rule where the 'Rule' specified that it's
-- character must appear either in the lower position or the upper but not both.
validByIndex :: Password -> Bool
validByIndex (Password (Rule indexA indexB character) password) = let charA = Text.index password indexA
                                                                      charB = Text.index password indexB
                                                                   in xor (charA == character) (charB == character)

-- | Exclusive OR.
xor :: Bool -> Bool -> Bool
xor False False = False
xor False True  = True
xor True  False = True
xor True  True  = False

-- | Part two validates passwords by xor of whether a character is present in
-- positions.
partTwo :: [Password] -> AOCM Int
partTwo = pure . length . filter validByIndex

