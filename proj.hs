import qualified Data.List
import qualified Data.Array
import qualified Data.Bits


-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

{-///////////////////////////////////////////////////////////////////////////////////////////////
-- 1 (could add a sort for pretty conus) DONE
///////////////////////////////////////////////////////////////////////////////////////////////-}
cities :: RoadMap -> [City]
cities roadmap = Data.List.nub [city | (start, end, _) <- roadmap, city <- [start, end]]


{-///////////////////////////////////////////////////////////////////////////////////////////////
-- 2 DONE
///////////////////////////////////////////////////////////////////////////////////////////////-}
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent roadmap start_city end_city = Data.List.any (\(start, end, _) -> start == start_city && end == end_city) roadmap

{-///////////////////////////////////////////////////////////////////////////////////////////////
-- 3 DONE
///////////////////////////////////////////////////////////////////////////////////////////////-}
distance :: RoadMap -> City -> City -> Maybe Distance
distance roadmap city1 city2
 | not (null l) = Just (Data.List.head l)
 | otherwise = Nothing
    where l = [distance |(start, end, distance) <- roadmap, (start == city1 && end == city2) || (start == city2 && end == city1)]


{-///////////////////////////////////////////////////////////////////////////////////////////////
-- 4 DONE-- podemos encontrar soulçoes mais simples
///////////////////////////////////////////////////////////////////////////////////////////////-}
adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent  roadmap start_city = [(end,distance)| (start, end, distance) <- roadmap ,start == start_city] ++ [(start,distance)| (start, end, distance) <- roadmap ,end == start_city]


{-///////////////////////////////////////////////////////////////////////////////////////////////
-- 5 DONE
///////////////////////////////////////////////////////////////////////////////////////////////-}
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0
pathDistance r [a,b] = distance r a b 
pathDistance r (h:x:t) 
    | areAdjacent r h x = addMaybe (distance r h x)  ( pathDistance r (x:t))
    | otherwise = Nothing

{-///////////////////////////////////////////////////////////////////////////////////////////////
-- 6 DONE
///////////////////////////////////////////////////////////////////////////////////////////////-}
rome :: RoadMap -> [City]
rome rodemap = Data.List.map fst pair_list
    where
        adj_list = cityAdjacencyList rodemap
        maxValue = Data.List.maximum (map snd adj_list)
        pair_list = Data.List.filter (\(a,b) -> b == maxValue) adj_list

{-///////////////////////////////////////////////////////////////////////////////////////////////
-- 7 DONE--começar por o 1 do grafo, e ir procurar todos a quem ele esta conectado; criar uma especie de queue em que guardamos 
///////////////////////////////////////////////////////////////////////////////////////////////-}

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadmap = Data.List.length (cities roadmap) == Data.List.length (bfsCities roadmap) 


{-///////////////////////////////////////////////////////////////////////////////////////////////
-- 8 
///////////////////////////////////////////////////////////////////////////////////////////////-}




--memo: during dijkstra's we choose the samallest path with an unvisited node
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadmap c_start c_end =  getPaths (auxShortestPath roadmap c_start c_end (setinitialDistance roadmap c_start))


auxShortestPath :: RoadMap -> City -> City -> [(City,Distance)] -> [Path] --distanceList ja presume visited
auxShortestPath roadmap c1 c2 distanceList  
    | c1 == c2 = []
    | otherwise = []





 -- Initialize distances to infinity for all nodes except the start node
setinitialDistance :: RoadMap -> City -> [(City,Distance)]
setinitialDistance roadmap start_city = [(city, distance) | city <- cities roadmap, let distance = if city == start_city then 0 else maxBound :: Int]

-- update paths
updateDistance :: City -> Distance -> [(City,Distance)] -> [(City,Distance)]
updateDistance city newDist [] = [(city, newDist)]  
updateDistance city newDist ((c,d): rest)
    |city == c && newDist <= d = (c,newDist) : rest
    |city == c && newDist > d = (c,d): rest
    |otherwise = (c, d) : updateDistance city newDist rest 

currentDistance :: City -> [(City,Distance)] -> Distance
currentDistance city ((c,d): rest) 
    | city == c = d
    | otherwise = currentDistance city rest

setPrev :: City -> City -> [(City, City)] -> [(City, City)] 
setPrev start end prevs = (end,start) : prevs

getPaths :: City -> [(City, City)] -> [Path]
getPaths city_start city_pairs
    | null nextCities = [[city_start]]
    | otherwise = concatMap (\(city_end, _) -> map (city_start :) (getPaths city_end city_pairs)) nextCities
  where
    nextCities = findNextCities city_start city_pairs  

findNextCities :: City -> [(City, City)] -> [(City, City)]
findNextCities city_start city_pairs = Data.List.filter (\(_, city_from) -> city_from == city_start) city_pairs

































{-///////////////////////////////////////////////////////////////////////////////////////////////
-- 9 
///////////////////////////////////////////////////////////////////////////////////////////////-}
travelSales :: RoadMap -> Path
travelSales = undefined


{-///////////////////////////////////////////////////////////////////////////////////////////////
-- auxiliars
///////////////////////////////////////////////////////////////////////////////////////////////-}
addMaybe :: Num a => Maybe a -> Maybe a -> Maybe a
addMaybe (Just x) (Just y) = Just (x + y)
addMaybe _ _ = Nothing 

cityAdjacencyList :: RoadMap -> [(City, Int)] --cidade e o numero de cidades que la ligam
cityAdjacencyList roadmap = [(city, length (adjacent roadmap city)) | city <- city_list]
    where 
        city_list = cities roadmap  
    
--useless as of yet
maxBySecond :: Ord b => [(a, b)] -> (a, b)
maxBySecond = Data.List.maximumBy (\(_, b1) (_, b2) -> compare b1 b2)

bfsCities :: RoadMap -> [City]
bfsCities ((start_city,end_city,dist) : rest) = Data.List.nub( auxBfsCities ((start_city,end_city, dist) : rest) [start_city] [] )
 
auxBfsCities :: RoadMap -> [City] -> [City] -> [City]
auxBfsCities _ [] visited = visited
auxBfsCities roadmap (current:rest) visited 
    | Data.List.elem current visited = auxBfsCities roadmap rest visited
    | otherwise = 
        let neighbors = map fst (adjacent roadmap current)
        in auxBfsCities roadmap (rest ++ neighbors) (current : visited)















{-///////////////////////////////////////////////////////////////////////////////////////////////
-NA
///////////////////////////////////////////////////////////////////////////////////////////////-}
tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function


{-///////////////////////////////////////////////////////////////////////////////////////////////
-- Some graphs to test your work
///////////////////////////////////////////////////////////////////////////////////////////////-}
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

--added 
gTest4 :: RoadMap
gTest4 = [("A", "B", 5), ("C", "D", 3), ("E", "F", 7)]



