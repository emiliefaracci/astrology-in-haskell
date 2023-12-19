module Parser where

import Data.Aeson hiding ((.=))
import Data.Text (Text, pack) 
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BC
import GHC.Generics
import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Data.Csv hiding (Parser, runParser)
import qualified Data.Vector as V
import Text.Read

import Compatibility

---------------------------------------------------------------------------------
-- data type with information from the API JSON file ----------------------------

data Planet = Planet
  { name       :: Text
  , fullDegree :: Double
  , normDegree :: Double
  , speed      :: Value
  , isRetro    :: Value
  , sign       :: Text
  , house      :: Int
  } deriving (Show, Generic)

data AstroData = AstroData [Planet]
  deriving (Show, Generic)

instance FromJSON Planet
instance FromJSON AstroData

---------------------------------------------------------------------------------
-- decoding JSON to Chart data type format --------------------------------------

parsedAstroData :: B.ByteString -> String -> Either String Chart
parsedAstroData json n =
  let astroDataResult = eitherDecode json :: Either String AstroData 
      in 
        case astroDataResult of
          Left err -> Left $ "Error parsing JSON: " ++ err
          Right (AstroData planets) ->
            let sunSign = sign $ head $ filter (\p -> name p == pack "Sun") planets
                moonSign = sign $ head $ filter (\p -> name p == pack "Moon") planets
                ascendantSign = sign $ head $ filter (\p -> name p == pack "Ascendant") planets
              in
                Right $ Chart n (tail (init (show sunSign))) (tail (init (show moonSign))) (tail (init (show ascendantSign)))


newtype Parser a =
  Parser { runParser :: String -> [(a, String)] }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f pa = pure f <*> pa

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \s0 -> [(a, s0)]

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) = ap

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  pa >>= f = Parser $ \s0 ->
    [ (b, s2) | (a, s1) <- runParser pa s0
              , (b, s2) <- runParser (f a) s1 ]

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \s -> []

  (<|>) :: Parser a -> Parser a -> Parser a
  (Parser f) <|> (Parser g) = Parser $ \s -> f s ++ g s

----------------------------------------------------------------------

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
  []     -> []
  (a:as) -> [ (a,as) | p a ]

char c = satisfy (c==)
alpha  = satisfy isAlpha
digit  = satisfy isDigit
space  = satisfy isSpace

string :: String -> Parser String
string str = Parser $ \s ->
  [ (pre, suf) | let (pre, suf) = splitAt (length str) s, str == pre ]

token :: String -> a -> Parser a
token str a =
  const a <$> string str

data Date =
    Date { yy :: Int, mm :: Int, dd :: Int }
    deriving (Show)

instance Read Date where
    readsPrec _ = runParser parseDate

parseMonth :: Parser Int
parseMonth = token "January" 1
        <|> token "February" 2
        <|> token "March" 3
        <|> token "April" 4
        <|> token "May" 5
        <|> token "June" 6
        <|> token "July" 7
        <|> token "August" 8
        <|> token "September" 9
        <|> token "October" 10
        <|> token "November" 11
        <|> token "December" 12
      
munch :: (Char -> Bool) -> Parser String
munch pred = Parser $ \s ->
  [span pred s]

skipSpaces :: Parser ()
skipSpaces =
  const () <$> munch isSpace

parseInt :: Parser Int
parseInt = read <$> some digit

parseDate :: Parser Date
parseDate = do
    month <- parseMonth
    skipSpaces
    date <- parseInt
    char ','
    skipSpaces
    year <- parseInt
    pure $ Date year month date

data Time =
    Time { hh :: Int, mn :: Int, zone :: Double }
      deriving (Show)

instance Read Time where
  readsPrec _ = runParser parseTime

parseUTC :: Parser Double
parseUTC = token "GMT" 0
      <|> token "UTC" 0
      <|> token "ECT" 0
      <|> token "EET" 2
      <|> token "ART" 2
      <|> token "EAT" 3
      <|> token "MET" 3.5
      <|> token "NET" 4
      <|> token "PLT" 5
      <|> token "IST" 5.5
      <|> token "BST" 6
      <|> token "VST" 7
      <|> token "CTT" 8
      <|> token "JST" 9
      <|> token "ACT" 9.5
      <|> token "AET" 10
      <|> token "SST" 11
      <|> token "NST" 12
      <|> token "MIT" (-11)
      <|> token "HST" (-10)
      <|> token "AST" (-9)
      <|> token "PST" (-8)
      <|> token "PNT" (-7)
      <|> token "MST" (-7)
      <|> token "CST" (-6)
      <|> token "EST" (-5)
      <|> token "IET" (-5)
      <|> token "PRT" (-4)
      <|> token "CNT" (-3.5)
      <|> token "AGT" (-3)
      <|> token "BET" (-3)
      <|> token "CAT" (-1)

parseTime :: Parser Time
parseTime = do
    hour <- parseInt
    char ':'
    min <- parseInt
    skipSpaces
    zone <- parseUTC
    pure $ Time hour min zone

checkDate :: Date -> Bool
checkDate (Date year month date) 
  | year > 2023 = False
  | mod year 4 /= 0 && month == 2 && date > 28 = False
  | mod year 4 == 0 && month == 2 && date > 29 = False
  | month < 8 && even month && date > 30 = False
  | month > 8 && odd month && date > 30 = False
  | date > 31 = False
  | otherwise = True

---------------------------------------------------------------------------------
-- check if user input is in the correct format ---------------------------------

readDate :: IO Date
readDate = do
  day <- getLine
  case (readMaybe day :: Maybe Date) of
    Just dd -> 
      if not (checkDate dd) then do
        putStrLn "\nIncorrect Date format. Try again.\n"
        readDate
      else pure dd
    Nothing -> do
      putStrLn "\nIncorrect Date format. Try again.\n"
      readDate

readTime :: IO Time
readTime = do
  time <- getLine
  case (readMaybe time :: Maybe Time) of
    Just tt@(Time hh mm tzone) ->
      if hh > 23 || mm > 59 then do
        putStrLn "\nIncorrect Time format. Try again.\n"
        readTime
      else pure tt
    Nothing -> do
      putStrLn "\nIncorrect Time format. Try again.\n"
      readTime
  