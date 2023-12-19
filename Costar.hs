module Costar where

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


import Prelude hiding (min)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status
import Network.HTTP.Types (hAuthorization, hContentType)
import qualified Data.ByteString.Lazy.Char8 as L
import Network.HTTP.Types.Method (methodPost)
import Network.HTTP.Types.Header 
import qualified Data.ByteString.Char8 as BC
import Data.Aeson
import Data.Text (Text, pack) 
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Control.Applicative
import Control.Monad
import qualified Data.Vector as V
import Data.Csv hiding (defaultOptions, encode)
import GHC.Generics

import Compatibility
import Parser
import Database

---------------------------------------------------------------------------------
-- data type with information for the API call ----------------------------------

data Birth = 
  Birth { day :: Int, 
          month :: Int, 
          year :: Int, 
          hour :: Int, 
          min :: Int, 
          lat :: Double,
          lon :: Double,
          tzone :: Double 
        } deriving (Generic, Show)
          
instance ToJSON Birth where
    toEncoding = genericToEncoding defaultOptions

---------------------------------------------------------------------------------
-- answer formatting ------------------------------------------------------------

spaceSequence :: String
spaceSequence = replicate 30 '\n'

lineSequence :: String 
lineSequence = replicate 13 '-'

formatChart :: Chart -> String 
formatChart (Chart n s m r) = lineSequence ++ "\nSun 𖤓 " ++ s ++ "\n" ++
                  lineSequence ++ "\nMoon ☾ " ++ m ++ "\n" ++ lineSequence ++ 
                  "\nRising ↑ " ++ r ++ "\n" ++ lineSequence ++ "\n"

extraInfo :: String 
extraInfo = 
  (replicate 85 '-') ++ 
  "\n𖤓 Your sun sign is your core identity.\n" ++
  "☾ Your moon sign governs your emotional nature.\n" ++
  "↑ Your rising influences how people see you.\n" ++ 
  (replicate 85 '-') ++ "\n"

---------------------------------------------------------------------------------
---------------------------------------------------------------------------------

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  csvData <- parseCSVUser "users.csv" 
  case csvData of
    Left err -> putStrLn "Sorry, our astrologers are currently busy looking at the moon. Try again later."
    Right userRecords -> do 
      putStrLn $ spaceSequence ++ "★★★ Welcome to Co-Star! Let's get started. ★★★\n\nWhat's your name?\n"
      name <- getLine
      -- checks the user database for name
      case findChart userRecords name of 
        Just (Chart n s m r) -> do 
          putStrLn $ spaceSequence ++ "Welcome back " ++ name ++ "! ☾ \n\n"
          putStrLn "Would you like to see your chart (Press 'a' key) or check compatability (Press 'b' key)?\n"
          answer <- getLine 
          case answer of 
            "a" -> do putStrLn $ formatChart (Chart n s m r)
            "b" -> do 
              putStrLn "\nGreat! What's the person's name?\n"  
              secondUser <- getLine 
              -- checks if second name is in database
              case findChart userRecords secondUser of 
                Just (Chart n' s' m' r') -> do putStrLn (testCompatibility (Chart n s m r) (Chart n' s' m' r'))
                Nothing -> do putStrLn "\nSorry, that person isn't in our records. Have them sign up first!\n"
            _ -> do putStrLn "Oh no! Please try running again\n"
        
        -- if user is not in database, ask info to make a Chart
        Nothing -> do 
          putStrLn $ spaceSequence ++ "Hi " ++ name ++ "! ☾ \n\n"
          putStrLn "★★★ What is your birth date? (ex: May 02, 2002) ★★★\n"
          date <- readDate
          putStrLn "\n★★★ How about birth time? (ex: 15:07 GMT) ★★★\n"
          tt <- readTime
          putStrLn "\n★★★ How about birth City (just the city or closest city)? ★★★\n"
          city <- getLine 
          csvData_ <- parseCSVCity "WorldCityLocations/World_Cities_Location_table.csv"
          case csvData_ of
            Left err -> putStrLn $ "Error decoding CSV: " ++ err
            Right cityRecords -> do
              let result = findCoordinates cityRecords (pack city)
                  (lati, longi) = case result of
                                    Just (latitude, longitude) -> (latitude, longitude)
                                    Nothing                     -> (39.952, -75.165) -- base case, in case there's an error

              putStrLn $ spaceSequence ++ "Cool. We're working to find the coordinates now. Sit tight!\n"

              -- sending API request using data gathered from IO interaction
              let url = "https://json.astrologyapi.com/v1/planets/tropical" 
              let test = Birth {day = dd date, month = mm date, year = yy date, hour = hh tt, min = mn tt, lat = lati, lon = longi, tzone = zone tt}
              let requestBody = encode test
              let auth = BC.pack "Basic NjI3MDE3OmUyYTA5Yjc4MTQ2YjFhZTBhY2QxOWU0YjU4NGI3ZWU5" 
              let content = BC.pack "application/json" 

              request <- parseRequest url
              let requestWithBody = request
                    { method = methodPost
                    , requestBody = RequestBodyLBS requestBody
                    , requestHeaders = [ (hAuthorization, auth), (hContentType, content) ]
                    }

              response <- httpLbs requestWithBody manager
              let json = responseBody response
              putStrLn $ spaceSequence ++ "We've compiled a custom chart for you! ⋆｡°✩\n"
              case parsedAstroData json name of
                Left err -> putStrLn "Couldn't parse, sorry."
                Right chart -> do
                  let updatedCsvData = V.toList userRecords ++ [chart]
                  B.writeFile "users.csv" $ encodeDefaultOrderedByName updatedCsvData
                  putStrLn $ formatChart chart 
                  putStrLn "Would you like more info about deciphering your chart? (press 'y' key)\n"
                  response <- getLine 
                  case response of 
                    "y" -> do putStrLn $ "\n" ++ extraInfo
                    _ -> do putStrLn "Thank you! Come again soon :)"

          