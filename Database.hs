module Database where 

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

import Parser
import Compatibility

---------------------------------------------------------------------------------
-- CityRecord type parses city coordinates using CSV file -----------------------

data CityRecord = CityRecord
  { cityId :: !Int
  , country :: !Text
  , cityName :: !Text
  , latitude :: !Double
  , longitude :: !Double
  , elevation :: !Double
  } deriving (Show, Eq, Generic)

instance FromRecord CityRecord
instance FromNamedRecord CityRecord

type ErrorMsg = String
type CityCSV = V.Vector CityRecord
type UserCSV = V.Vector Chart

parseCSVCity :: FilePath -> IO (Either ErrorMsg CityCSV)
parseCSVCity filePath = decodeWith decodeOptions HasHeader <$> B.readFile filePath
  where
    decodeOptions = defaultDecodeOptions { decDelimiter = fromIntegral $ ord ';' }

parseCSVUser :: FilePath -> IO (Either ErrorMsg UserCSV)
parseCSVUser filePath = decodeWith decodeOptions HasHeader <$> B.readFile filePath
  where
    decodeOptions = defaultDecodeOptions { decDelimiter = fromIntegral $ ord ',' }

findCoordinates :: V.Vector CityRecord -> Text -> Maybe (Double, Double)
findCoordinates records cityNameToFind = do
  cityRecord <- V.find (\record -> cityName record == cityNameToFind) records
  return (latitude cityRecord, longitude cityRecord)

findChart :: V.Vector Chart -> String -> Maybe Chart
findChart records n = do
  userRecord <- V.find (\record -> username record == n) records
  return userRecord

---------------------------------------------------------------------------------
-- instances for the Chart data type, to record users into the csv database -----

instance ToNamedRecord Chart where
  toNamedRecord (Chart username' sun' moon' rising') =
    namedRecord
      [ BC.pack "username" .= username'
      , BC.pack "sun" .= sun'
      , BC.pack "moon" .= moon'
      , BC.pack "rising" .= rising'
      ]

instance FromRecord Chart
instance FromNamedRecord Chart

instance DefaultOrdered Chart where
  headerOrder _ =
    header [ BC.pack "username", BC.pack "sun", BC.pack "moon", BC.pack "rising"]
