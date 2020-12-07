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

solution :: Solution Int Int
solution = Solution
  { _parse   = parsePassports
  , _partOne = partOne
  , _partTwo = partTwo
  }

partOne :: [Passport] -> AOCM Int
partOne = pure . length . filter hasRequiredFields

partTwo :: [Passport] -> AOCM Int
partTwo passports = do
  validatedPassports <- mapM allFieldsValid . filter hasRequiredFields $ passports
  pure . length . filter (== True) $ validatedPassports

main :: IO ()
main = do
  let inputFile   = "2020/4/passports"

  input <- readLinesFromFile inputFile
  execSolution input solution

-- | All possible names of fields.
data FieldName
  = FieldBirthYear
  | FieldIssueYear
  | FieldExpirationYear
  | FieldHeight
  | FieldHairColor
  | FieldEyeColor
  | FieldPassportID
  | FieldCountryID
  deriving (Eq, Ord, Show)

-- | A Passport maps field names to their unvalidated textual values - if they
-- need to be parsed that's somebody elses problem.
newtype Passport = Passport {_unPassport :: Map FieldName Text}
  deriving Show

-- | Parse a collection of passports.
parsePassports :: [Text] -> AOCM [Passport]
parsePassports = parse passports . Text.unlines

-- | A collection of Passports is separated by double newlines.
passports :: Parser [Passport]
passports = sepBy1 passport (newline *> newline) <* newline

-- | A passports has at least one line of fields, possibly more separated by a
-- newline.
passport :: Parser Passport
passport = (Passport . Map.unions) <$> sepBy1 fields newline

-- | A line of fields maps a field name to an unparsed value.
fields :: Parser (Map FieldName Text)
fields = Map.fromList <$> sepBy1 field space

-- | A FieldName and its unparsed value is separated by a ':'.
field :: Parser (FieldName,Text)
field = alternatives
      . map (\(keyName,field) -> (,) <$> (textIs keyName *> pure field)
                                     <*> (charIs ':' *> word))
      $ [ ("byr", FieldBirthYear)
        , ("iyr", FieldIssueYear)
        , ("eyr", FieldExpirationYear)
        , ("hgt", FieldHeight)
        , ("hcl", FieldHairColor)
        , ("ecl", FieldEyeColor)
        , ("pid", FieldPassportID)
        , ("cid", FieldCountryID)
        ]

-- | Validate that a Passport has all of the required fields.
-- NOTE: This does NOT include CountryID.
hasRequiredFields :: Passport -> Bool
hasRequiredFields = Set.isSubsetOf requiredFields . Map.keysSet . _unPassport
  where
    requiredFields :: Set FieldName
    requiredFields = Set.fromList [FieldBirthYear,FieldIssueYear,FieldExpirationYear,FieldHeight,FieldHairColor,FieldEyeColor,FieldPassportID]

-- | Height may be provided in two units.
data HeightUnit = Centimeter | Inch
  deriving (Eq,Ord,Show)

-- | Accept height in two units.
heightUnit :: Parser HeightUnit
heightUnit =  (textIs "cm" *> pure Centimeter)
          <|> (textIs "in" *> pure Inch)

-- | Height has a unit and a value.
data Height = Height Int HeightUnit
  deriving (Eq,Ord,Show)

-- | A Height has lower and upper limits depending on the unit used.
height :: Parser Height
height = do
  digits <- many digit
  let (_,h) = foldr (\d (exp,acc) -> (exp*10,acc+ d*exp)) (1,0) digits
  unit <- heightUnit
  case unit of
    Inch
      -> do when (h < 59) $ failure $ err "Height in inches must be at least 59"
            when (76 < h) $ failure $ err "Height in inches must be less than 76"

    Centimeter
      -> do when (h < 150) $ failure $ err "Height in centimeter must be at least 150"
            when (193 < h) $ failure $ err "Height in centimeter must be less than 193"
  pure $ Height h unit

-- | HairColor is a hexidecimal string.
data HairColor = HairColor Text
  deriving (Eq,Ord,Show)

-- | Hair color begins with a # and has exactly 6 hexidecimal characters.
hairColor :: Parser HairColor
hairColor = do
  charIs '#'
  color <- many hexDigit
  if length color /= 6
    then failure (err "hair color must be exactly 6 characters")
    else pure $ HairColor $ Text.pack $ color

-- | A Fixed set of possible EyeColors.
data EyeColor
  = Amber
  | Blue
  | Brown
  | Gray
  | Green
  | Hazel
  | Other
  deriving (Eq,Ord,Show)

-- | Eye color is one of several alternatives.
eyeColor :: Parser EyeColor
eyeColor = alternatives
          . map (\(name,field) -> textIs name *> pure field)
          $ [("amb",Amber)
            ,("blu",Blue)
            ,("brn",Brown)
            ,("gry",Gray)
            ,("grn",Green)
            ,("hzl",Hazel)
            ,("oth",Other)
            ]

-- | A PassportID is represented as text - as it may use leading 0's.
data PassportID = PassportID Text
  deriving (Eq,Ord,Show)

-- | A PassportID is exactly 9 characters 0..9
passportID :: Parser PassportID
passportID = do
  digits <- many (satisfy (`elem` ("0123456789"::String)))
  if length digits /= 9
    then failure (err "passport did not have exactly 9 digits" `withAttributes` [("digits",Text.pack . show $ length digits)])
    else pure $ PassportID $ Text.pack digits

-- | A Year is a subset of an Int.
data Year = Year Int
  deriving (Eq, Ord, Show)

-- | A year must have exactly 4 digits, because who cares about being
-- future-proof?
year :: Parser Year
year = do
  digits <- many digit
  case digits of
    (a:b:c:d:[])
      -> pure $ Year $ sum [d * 1, c * 10, b * 100 , a * 1000]

    _ -> failure $ err "Incorrect number of digits in year"

data BirthYear = BirthYear Year
  deriving (Eq,Ord,Show)

-- | A Year with some lower and upper bounds.
birthYear :: Parser BirthYear
birthYear = do
  y <- year
  when (y    < Year 1920) $ failure $ err "Year must be greater than 1920"
  when (Year 2002 < y) $ failure $ err "Year must be less than 2002"
  pure $ BirthYear y

data IssueYear = IssueYear Year
  deriving (Eq,Ord,Show)

-- | A Year with some lower and upper bounds.
issueYear :: Parser IssueYear
issueYear = do
  y <- year
  when (y < Year 2010) $ failure $ err "Year must be greater than 2010"
  when (Year 2020 < y) $ failure $ err "Year must be less than 2020"
  pure $ IssueYear y

data ExpirationYear = ExpirationYear Year
  deriving (Eq,Ord,Show)

-- | A Year with some lower and upper bounds.
expirationYear :: Parser ExpirationYear
expirationYear = do
  y <- year
  when (y < Year 2020) $ failure $ err "Year must be greater than 2020"
  when (Year 2030 < y) $ failure $ err "Year must be less than 2030"
  pure $ ExpirationYear y

-- | One of any validated field value.
data ValidField
  = ValidBirthYear      BirthYear
  | ValidIssueYear      IssueYear
  | ValidExpirationYear ExpirationYear
  | ValidHeight         Height
  | ValidHairColor      HairColor
  | ValidEyeColor       EyeColor
  | ValidPassportID     PassportID
  | ValidCountryID      Text
  deriving (Eq, Ord)

-- | Check whether a single field is valid by consulting an associated Parser.
validField :: FieldName -> Parser ValidField
validField field = case field of
  FieldBirthYear
    -> ValidBirthYear <$> birthYear

  FieldIssueYear
   -> ValidIssueYear <$> issueYear

  FieldExpirationYear
    -> ValidExpirationYear <$> expirationYear

  FieldHeight
    -> ValidHeight <$> height

  FieldHairColor
    -> ValidHairColor <$> hairColor

  FieldEyeColor
    -> ValidEyeColor <$> eyeColor

  FieldPassportID
    -> ValidPassportID <$> passportID

  FieldCountryID
    -> ValidCountryID <$> word

  _ -> failure $ err "No validating parser for field"
               `withAttributes` [("field", Text.pack . show $ field)]


-- | A ValidPassport is a set of fields that have been parsed and their values
-- successfully validated.
--
-- Construct with 'mkValidPassport'.
newtype ValidPassport = ValidPassport {_unValidPassport :: Set ValidField}

-- | Attempt to validate a passport by parsing all of the present fields.
mkValidPassport :: Passport -> AOCM ValidPassport
mkValidPassport passport = do
  validFields <- mapM (\(field,value) -> parse (validField field) value) . Map.toList . _unPassport $ passport
  pure . ValidPassport . Set.fromList $ validFields

-- | Can we construct a valid passport, or is there a validation failure?
allFieldsValid :: Passport -> AOCM Bool
allFieldsValid passport = do
  case mkValidPassport passport of
    Left err
      -> pure False

    Right _
      -> pure True

