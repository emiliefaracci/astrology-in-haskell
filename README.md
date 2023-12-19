# astrology-in-haskell

Hi and welcome to our project, Co-Star Haskell Copy! 

**Project Description: 
**

The astrology app, Co-Star, enables users to understand their birth chart, connect with friends, and discover where their compatibility lies. 
Most importantly (to us), Co-Star boasts their use of the Haskell language, and even have a whole page on their website dedicated to it: 
“https://www.costarastrology.com/why-haskell”. Although Co-Star only has two backend engineers, they provide astrology to approx 25% of 
young women in the US. They say it’s because of Haskell. So why not give it a go? 

Elizabeth Li and I recreated a user interface with the main functionalities of Co-Star, incorporating a user database to keep track of users. Functionalities 
include adding your user and seeing your chart (Sun sign, Moon sign, Rising sign), getting extra info about your chart, and comparing compatability 
with other users. 

**High Level Overview of Code Base: 
**

Costar.hs : This file deals with the I/O with user for the interface through its main. It references Compatibility, 
            Parser, and Database modules to perform outside data storage, computation and parsing.

Compatibility.hs: Given two user charts, this file will check sign/elements for compatability and provide the 
                  messages to the users. 

Parser.hs : This file works to parse the input from the user, through a custom built Parser, and make sure that the 
            input is of the correct format for the API call. It also parses the JSON API call output. 

Database.hs : This file deals with CSV and Database actions, both for the WorldCityLocations CSV file and our custom
              user database CSV file users.csv

WorldCityLocations: World City Locations Database which we cloned is an open source CSV file with major world cities
                    and their respective coordinates (latitude, longitude, and altitude). See the 
                    repository README. Database.hs decodes the csv to find the user input's city and 
                    parses it into a CityRecord custom type to be used in the API call. We cloned this repo from                                                 https://github.com/bahar/WorldCityLocations


**Instructions on interacting with the project: ** 

We made a cabal file with costar executable. Simply type cabal run 
                                              and follow the instructions on the screen! The CSV file with current 
                                              users will be updated locally while playing. Users will have to be added 
                                              in a seperate run-through, before compatibility can be checked.


**Notes:**

- We are using a free trial for our API key. Unfortunately, the trial ends on December 17, 2023.
- We included a populated `users.csv` file for your convenience of testing. However, if you wish to start with a fresh csv, the file must contain one newline/end of line character at the start. 
