module Compatibility where

import GHC.Generics

spacesSequence :: String
spacesSequence = replicate 35 '\n'

linesSequence :: String 
linesSequence = replicate 78 '-'

spaceandLine :: String 
spaceandLine = spacesSequence ++ linesSequence

data Chart = Chart
  { username :: String,
    sun :: String,
    moon   :: String,
    rising :: String
  } deriving (Show, Generic)

data Element = Water
    | Earth
    | Fire
    | Air
    deriving (Show, Eq)

signToElement :: String -> Element
signToElement sign 
    | sign == "Taurus" || sign == "Virgo" || sign == "Capricorn" = Earth
    | sign == "Cancer" || sign == "Scorpio" || sign == "Pisces" = Water
    | sign == "Leo" || sign == "Sagittarius" || sign == "Aries" = Fire
    | sign == "Gemini" || sign == "Libra" || sign == "Aquarius" = Air

testCompatibility :: Chart -> Chart -> String
testCompatibility (Chart name1 sun1 _ _) (Chart name2 sun2 _ _) =
    let element1 = signToElement sun1
        element2 = signToElement sun2
        in
            if element1 == element2 then 
                spaceandLine ++ "\n\nYou and " ++ name2 ++ " are both " ++ show element1 
                ++ " signs! Therefore, you are compatible ༄˖°.ೃ࿔*:･\n\n" ++ linesSequence ++ "\n\n"
            else case element1 of 
                    Water -> case element2 of
                        Earth -> spaceandLine ++ "\n\nWater and earth signs are complementary astrological elements. ༄\n"
                                ++ "Since you are a Water sign, and " ++ name2 ++ " is an Earth sign, \n" 
                                ++ "you'll do a great job nourishing " ++ name2 ++ "'s needs \n\n" ++ linesSequence ++ "\n"
                        _ -> spaceandLine ++ "\n\nYou and " ++ name2 ++ " are not compatible because of your sun sign element.\n"
                                ++ "Maybe you should reconsider your relationship... ₊˚.༄\n\n" ++ linesSequence ++ "\n"
                    Earth -> case element2 of
                        Water -> spaceandLine ++ "\n\nAs an Earth sign, you love are grounded and down-to-earth. ₊˚.\nAs a Water sign,"
                                ++ name2 ++ " will balance you out. ༄ You are very compatible.\n\n" ++ linesSequence ++ "\n"
                        _ -> spaceandLine ++ "\n\nYou and " ++ name2 ++ " are not compatible because of your sun sign element.\n" 
                                ++ "Don't worry. You can still be friends! ₊˚.༄\n\n" ++ linesSequence ++ "\n"
                    Fire -> case element2 of
                        Air -> spaceandLine ++ "\n\nFire needs air to survive, so might we say you need " ++ name2 ++ " to thrive.\n\n" ++ linesSequence ++ "\n"
                        _ -> spacesSequence ++ linesSequence ++ "\n\nYou and " ++ name2 ++ " are not compatible because of your sun sign element.\n"
                                ++ "Are you sure you are actually friends?\n\n" ++ linesSequence ++ "\n"
                    Air -> case element2 of
                        Fire -> spaceandLine ++ "\n\nWe think you and " ++ name2 ++ " will make sparks. As an Air and Fire duo,\n "
                                ++ "you two are very compatible.\n\n" ++ linesSequence ++ "\n"
                        _ -> spaceandLine ++ "\n\nYou and " ++ name2 ++ " are not compatible because of your sun sign element. ༄˖°.ೃ࿔* \n"
                                ++ "But maybe opposites attract!\n\n" ++ linesSequence ++ "\n"