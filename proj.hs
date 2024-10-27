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





shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadmap c_start c_end =  getPaths c_start c_end (auxShortestPath roadmap c_start c_end [c_start] (setinitialDistance roadmap c_start)) -- vai fazer o get paths que vai lidar com os prevs encontrados no auxhortest paths

--graf, startCity,endCity,queue,cityDistanceList,current_previousList -> final_previousList
auxShortestPath :: RoadMap -> City -> City -> [City] -> [(City,Distance)] -> [(City,City)]-> [(City,City)] -- vai ter como input roadmap cidade inicial, final, queue, a distancias atuais das cidades e vai dar return dos prevs de cada um.
auxShortestPath roadmap startCity endCity (current:rest) distanceList current_previousList  
    | startCity == endCity = [(startCity,startCity)] 
    | cond 
    
    auxShortestPath roadmap c1 c2 (rest ++ neighbors) (updateDistance neighbours) -- aqui no in e so chamar a funçao utra vez mas com os inputs das listas atualizadas
        where
            neighbours = map fst (adjacent roadmap current)
            currentDistance = getDist_fromDistanceList current distanceList
            
            filteredNeighboursList = [ usefullNeighbour | usefullNeighbour <- neighbors, (getDist_fromDistanceList usefullNeighbour distanceList) >= (currentDistance) + (distance roadmap current usefullNeighbour)]



updateDistance :: City -> Distance -> [(City,Distance)] -> [(City,Distance)]
updateDistance city newDist [] = [(city, newDist)]  
updateDistance city newDist ((c,d): rest)
    |city == c && newDist <= d = (c,newDist) : rest
    |city == c && newDist > d = (c,d): rest
    |otherwise = (c, d) : updateDistance city newDist rest 

getDist_fromDistanceList :: City -> [(City,Distance)] -> Distance
getDist_fromDistanceList city ((c,d): rest) 
    | city == c = d
    | otherwise = getDist_fromDistanceList city rest

setPrev :: City -> City -> [(City, City)] -> [(City, City)] 
setPrev start end prevs = (end,start) : prevs


findPrevCities :: City -> [(City, City)] -> [(City, City)]
findPrevCities city_start city_pairs = filter (\(city, _) -> city == city_start) city_pairs


-- ////////////////////////////////// DONT WORRY WITH THIS ////////////////////////////

 -- Initialize distances to infinity for all nodes except the start node
 -- so serve para a chamada da aux
setinitialDistance :: RoadMap -> City -> [(City,Distance)]
setinitialDistance roadmap start_city = [(city, distance) | city <- cities roadmap, let distance = if city == start_city then 0 else maxBound :: Int]

-- vai buscar os paths melhores para a cidade que queremos.
getPaths :: City -> City -> [(City, City)] -> [Path]
getPaths city_start city_end city_pairs
    | city_start == city_end = [[city_end]]
    | null prevCities = []
    | otherwise = concatMap (\(current_city, prev_city) -> map (city_start :) (getPaths prev_city city_end city_pairs)) prevCities
    where
    prevCities = findPrevCities city_start city_pairs 






























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



