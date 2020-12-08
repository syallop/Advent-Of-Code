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

solution = Solution
  { _parse   = parseDeclarations
  , _partOne = partOne
  , _partTwo = partTwo
  }

-- | Across each group, sum every single unique declaration.
partOne :: [Declaration] -> AOCM Int
partOne = pure . sum . map (Set.size . Set.unions . _unDeclaration)

-- | Across each group, sum every single declaration each individual has in
-- common with their group.
partTwo :: [Declaration] -> AOCM Int
partTwo = pure . sum . map (Set.size . intersections . _unDeclaration)
  where
    intersections :: Ord a => Set (Set a) -> Set a
    intersections = foldl1 Set.intersection

main :: IO ()
main = do
  let inputFile   = "2020/6/customsDeclarations"

  input <- readLinesFromFile inputFile
  execSolution input solution

-- | There are A..Z kinds of Declaration.
data DeclarationName
  = DeclarationA
  | DeclarationB
  | DeclarationC
  | DeclarationD
  | DeclarationE
  | DeclarationF
  | DeclarationG
  | DeclarationH
  | DeclarationI
  | DeclarationJ
  | DeclarationK
  | DeclarationL
  | DeclarationM
  | DeclarationN
  | DeclarationO
  | DeclarationP
  | DeclarationQ
  | DeclarationR
  | DeclarationS
  | DeclarationT
  | DeclarationU
  | DeclarationV
  | DeclarationW
  | DeclarationX
  | DeclarationY
  | DeclarationZ
  deriving (Eq, Ord, Show, Enum)

-- | Declarations cover a group of people, each has their own set of Declarations that they admit to.
newtype Declaration = Declaration {_unDeclaration :: Set (Set DeclarationName)}
  deriving Show

-- | Parse many Declarations (which themselves are sets (the group) of sets (the
-- person) of individual declarations.
parseDeclarations :: [Text] -> AOCM [Declaration]
parseDeclarations = parse declarations . Text.unlines

-- | Declarations are separated by double newlines.
declarations :: Parser [Declaration]
declarations = sepBy1 declaration (newline *> newline) <* newline

-- | A declaration is one or more lines, separated by a newline.
declaration :: Parser Declaration
declaration = (Declaration . Set.fromList) <$> sepBy1 declarationNames newline

-- | A line of declarations from a single individual has at least one
-- declaration.
declarationNames :: Parser (Set DeclarationName)
declarationNames = Set.fromList <$> many1 declarationName

-- | A single declaration is a letter a..z.
declarationName :: Parser DeclarationName
declarationName = alternatives
      . map (\(name,decl) -> (textIs name *> pure decl))
      . zip (map Text.singleton ['a'..'z'])
      $ [DeclarationA .. DeclarationZ]


