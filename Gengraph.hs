{- Seth Brown
www.drbunsen.org
sethbrown@drbunsen.org
Copyright 2012 Seth Brown. All rights reserved.
http://www.census.gov/geo/www/gazetteer/gazetteer2010.html
 -}

module Gengraph where
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.List as DL
import qualified Data.GPS as GPS

main :: IO()
main = do 
    let zips = ["98109", "98404", "99362"]
    bytes <- BS.readFile "zip-code-data.txt"
    print $  genGraph zips bytes

-- | Build a graph from a list of zipcodes
genGraph :: [String] -> BC.ByteString -> [(String, String, Float)]
genGraph zipcodes bytes = filter (\(x,y,z) -> z < 1000) 
                          $ map zipDist . edges $ filterByZips' zipcodes bytes
    where 
        filterByZips' zips b = filterZips zips $ parseBytes b
        parseBytes dat = map BC.words . BC.lines $ dat

-- |collect matching zipcode and location data 
filterZips :: [String] -> [[BC.ByteString]] -> [[String]]
filterZips       _ [] = []
filterZips       [] _ = []
filterZips (x:xs) dat = elimEmpty $ map (filterByzip x) 
                        dat ++ filterZips xs dat
    where 
        elimEmpty = filter (\i -> i /= [])
        filterByzip z lst =  matchHead z $ map BC.unpack lst
        matchHead match lst 
            | match == head lst = [head lst, lst!!7, lst!!8]
            | otherwise = []

-- | assemble all edges in the graph
edges :: [[String]] -> [[Waypoint]]
edges xss = [[i,j] | i <- map go xss, 
                     j <- map go xss, 
                     lon i < lon j,
                     zipcode i /= zipcode j]
                         where
                             go [x,y,z] = Waypoint x
                                                   (read y :: Double) 
                                                   (read z :: Double)

-- |find distances in meters from 2 lat/long coordinates
zipDist :: [Waypoint] -> (String, String, Float)
zipDist [cs,cs'] = ((zipcode cs),(zipcode cs'),(mDist cs cs'))
    where 
        mDist xs xs' = fromIntegral (round $ GPS.distance (dms xs) (dms xs') / 1000) :: Float
        dms p = GPS.degreePairToDMS (lat p, lon p)

data Waypoint = Waypoint { zipcode :: String
                         , lat :: Double 
                         , lon :: Double
                         } deriving (Read, Show, Eq, Ord)  
