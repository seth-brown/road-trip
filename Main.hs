{- Seth Brown
www.drbunsen.org
sethbrown@drbunsen.org
Copyright 2012 Seth Brown. All rights reserved.
http://www.census.gov/geo/www/gazetteer/gazetteer2010.html
 -}

module Main where
import qualified Data.List as DL
import qualified Data.ByteString as BS
import qualified Gengraph as GG
import qualified Dijkstra as DJ

main :: IO()
main = do
   let zips = ["98109", -- Seattle, WA
               "84148", -- Salt Lake, UT
               "89503", -- Reno, NV
               "89311", -- Great Basin, NV
               "83704", -- Boise, ID
               "59434", -- Glacier, MT
               "82190", -- Yellowstone, WY
               "57750", -- Interior, SD
               "80002", -- Arvada, CO
               "66605", -- Topeka, KS
               "52245", -- Iowa City, IA
               "55405", -- Mineapolis, MN
               "63101", -- St. Louis, MO
               "44105", -- Cleveland, OH
               "16254", -- Shippenville, PA
               "19099", -- Philadelphia, PA
               "15201", -- Pittsburgh, PA
               "60563", -- Naperville, IL
               "02109"] -- Boston, MA

   bytes <- BS.readFile "zip-code-data.txt"
   let graph = GG.genGraph zips bytes
   print $ roadTrip 6 "98109" "02109" graph

-- | Calculated a shortest-path solution through a given number of zipcodes
roadTrip :: Ord a => Int -> a -> a -> [(a, a, Float)] -> [[a]]
roadTrip n s t g = filter (\path -> length path == n) $ shPaths s t g
    where
        shPaths source target graph = filter (\path -> length path == n) $ DL.nub $ map (optpath source target) $ permute graph  
        optpath source target dat = DJ.shortestPath source target $ DJ.buildGraph dat
        permute xs = [beginning ++ end | (beginning, ignored:end) <- div xs]
            where
                div lst = zip (DL.inits lst) (DL.tails lst)
