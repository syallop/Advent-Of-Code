{-# LANGUAGE OverloadedStrings, DeriveAnyClass #-}
module Main where

import AOC

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Data.List (groupBy)
import Control.Applicative
import Control.Monad

import Data.Foldable

import Data.Maybe

solution = Solution
  { _parse   = parseRules
  , _partOne = partOne
  , _partTwo = partTwo
  }

-- | A Bag (represented by it's Color) may be contained in many other colors of
-- bags.
-- These themselves may be contained within another bag.
-- Count the number of bags which may contain the target bag at some level of
-- depth.
partOne :: [Rule] -> AOCM Int
partOne rules = pure $ length $ containedTransitivelyIn (Color "shiny gold") (containedBy rules)

-- | A Bag (represented by it's Color) has rules describing how many, and of
-- which color other bags it must contain.
-- Count each nested required bag to complete a full target bag.
partTwo :: [Rule] -> AOCM Int
partTwo rules = pure $ countTransitivelyIn (Color "shiny gold") (ruleMap rules)
  where
    -- Extract the raw definition of a rule map
    ruleMap :: [Rule] -> Map Color (Map Color Int)
    ruleMap = Map.fromList . map (\(Rule color contains) -> (color, contains))

main :: IO ()
main = do
  let inputFile   = "2020/7/bagRules"

  input <- readLinesFromFile inputFile
  execSolution input solution

-- | A Color is an adjective and a color, which we'll represent as the text.
newtype Color = Color Text
  deriving (Eq, Ord, Show)

-- | A 'Contains' relation Map says that each key Color should be present value
-- Int times.
type Contains = Map Color Int

-- | A Rule associates Colors to 'Contains' rules which describe how many and of
-- what Color bags they must themselves contain.
data Rule = Rule Color Contains
  deriving Show

-- | Parse many lines of rules describing how colored bags must nest inside each
-- other.
parseRules :: [Text] -> AOCM [Rule]
parseRules = parse rules . Text.unlines

-- | Rules are separated by a newline.
rules :: Parser [Rule]
rules = sepBy1 rule newline <* newline

-- | A rule apply's to a bag color, and specified which other bag colors it must
-- contain, and how many of each.
rule :: Parser Rule
rule = Rule <$> bagsColor
            <*> (textIs " contain " *> contains <* charIs '.')

-- | A Bag is refered to in singular and plural form, depending on ENGLISH.
bag :: Parser ()
bag =  textIs "bags"
   <|> textIs "bag"

-- | A Bag's color is an adjective, it's color and a bag-like suffix.
--
-- E.G. matches:
-- "[light lime] bags"
bagsColor :: Parser Color
bagsColor = (\adjective color -> Color $ Text.unwords [adjective, color])
         <$> word
         <*> (space *> word <* space <* bag)

-- | A quantity of colored bag(s) may only be a single digit.
--
-- E.G. matches:
-- "[5] [pale red] bags"
containBagsOfColor :: Parser (Color, Int)
containBagsOfColor = flip (,) <$> digit <*> (space *> bagsColor)

-- | Describe the 'Contains' rules of a bag which is either:
-- - A collection of bags and their numbers like:
-- "[5] [pale red] bags, [2] [light lime] bags, [5] clear indigo bags"
--
-- - Or:
-- "no other bags"
contains :: Parser Contains
contains = Map.fromList <$> ((sepBy1 containBagsOfColor (textIs ", ")) <|> (textIs "no other bags" *> pure []))

-- Count the number of bags transitively contained within one top-level bag.

-- | A Bag (represented by it's Color) has rules describing how many, and of
-- which color other bags it must contain.
-- Count each nested required bag to complete a full target bag.
countTransitivelyIn :: Color -> Map Color (Map Color Int) -> Int
countTransitivelyIn color relations = case Map.lookup color relations of
  -- A Bag with no rules requires only itself.
  Nothing
    -> 1

  -- Otherwise we require:
  -- - Each of the number of the various child bags
  -- - Each of the child bags own contained bags, for each of the required
  --   number.
  Just contains
    -> sum . fmap (\(color,count) -> count + count * countTransitivelyIn color relations)
           . Map.toList
           $ contains

-- | A Bag (represented by it's Color) may be contained in many other colors of
-- bags.
-- These themselves may be contained within another bag.
--
-- Given a relation which must describe what other Colors a bag is contained
-- within (the inverse of the Rules), calculate the total variety of bags which
-- may contain the target bag at some level of depth.
containedTransitivelyIn :: Color -> Map Color (Set Color) -> Set Color
containedTransitivelyIn color relations = containedTransitivelyIn' color Set.empty
  where
    containedTransitivelyIn' :: Color -> Set Color -> Set Color
    containedTransitivelyIn' color accSet =
      let containedDirectly :: Set Color
          containedDirectly = containedIn color relations

          containedIndirectly :: Set (Set Color)
          containedIndirectly = Set.map (\color -> containedTransitivelyIn color relations) containedDirectly

       in Set.unions (containedDirectly:(Set.toList containedIndirectly))

-- | Given a Color and some relations which describe:
-- "in what color bags may I be contained in"
-- Return each of these bags, or an empty set if none.
containedIn :: Color -> Map Color (Set Color) -> Set Color
containedIn color relations = case Map.lookup color relations of
  Nothing
    -> Set.empty

  Just containedIn
    -> containedIn

-- | Given a collection of rules:
-- - Which specify which colors contain what other colors
--
-- Invert the relationship to instead describe:
-- - The Set of Colors each Color may be contained withing.
containedBy :: [Rule] -> Map Color (Set Color)
containedBy rules =
  -- TODO: Highly likely can be more efficient that this recursive unions of
  -- singletons.
  foldl (\acc (Rule container contains)
          -> let additionalMap :: Map Color (Set Color)
                 additionalMap = eachContainedIn container (Map.keysSet contains)
              in Map.unionWith Set.union acc additionalMap
        )
        (Map.empty)
        rules

  where
    -- | Given a containing color and a set of contained colors, invert the
    -- relationship and produce a map which describes:
    -- - Each Color is contained within the singleton set of the container
    -- color.
    -- TODO: The way to improve this is probably to make the key type a plain
    -- color.
    eachContainedIn :: Color -> Set Color -> Map Color (Set Color)
    eachContainedIn container contains = foldl (\accMap contained -> Map.insertWith Set.union contained (Set.singleton container) accMap)
                                 Map.empty
                                 contains

