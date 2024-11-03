import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

-- AUX Types
type AdjacencyMatrix = Data.Array.Array (Int,Int)  (Maybe Distance)
type MemoTable = Data.Array.Array (Int, Int)  (Maybe Distance)


-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.


{-///////////////////////////////////////////////////////////////////////////////////////////////
-- 1 DONE
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
-- 8 
///////////////////////////////////////////////////////////////////////////////////////////////-}


shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadmap c_start c_end =  getPaths c_end c_start (auxShortestPath roadmap c_start c_end [c_start] (setinitialDistance roadmap c_start) []) -- vai fazer o get paths que vai lidar com os prevs encontrados no auxhortest paths


--graf, startCity,endCity,queue,cityDistanceList,current_previousList -> final_previousList
auxShortestPath :: RoadMap -> City -> City -> [City] -> [(City,Maybe Distance)] -> [(City,City)]-> [(City,City)] -- vai ter como input roadmap cidade inicial, final, queue, a distancias atuais das cidades e vai dar return dos prevs de cada um.
auxShortestPath roadmap _ _ [] _ current_previousList = current_previousList
auxShortestPath roadmap startCity endCity (current:rest) distanceList current_previousList  
    | startCity == endCity = [(startCity,startCity)] 
    | otherwise = auxShortestPath roadmap startCity endCity (rest ++ filteredNeighboursList) new_dist new_prev_list -- aqui no in e so chamar a funçao outra vez mas com o a queue, prev atualizados
        where
            neighbours = map fst (adjacent roadmap current)
            currentDistance = getDist_fromDistanceList current distanceList
            
            --get usefull neighbours: neighbours whose distances can be made smaller
            filteredNeighboursList = [ usefullNeighbour | usefullNeighbour <- neighbours, getDist_fromDistanceList usefullNeighbour distanceList > addMaybe currentDistance (distance roadmap current usefullNeighbour)]
            
            --update all distances of the usefull neighbours in distance list
            new_dist = foldl (\newDistList usefulNeighbour ->
                    let newDist = addMaybe currentDistance  (distance roadmap current usefulNeighbour)
                    in updateDistance usefulNeighbour newDist newDistList
                ) distanceList filteredNeighboursList
                
            new_prev_list = foldl (\newPrevList filtered_neighbour -> updatePrevList roadmap distanceList current filtered_neighbour newPrevList) current_previousList filteredNeighboursList


updateDistance :: City -> Maybe Distance -> [(City, Maybe Distance)] -> [(City, Maybe Distance)]
updateDistance city newDist cityDistances = map (\(c, d) -> if c == city then (c, newDist) else (c, d)) cityDistances



getDist_fromDistanceList :: City -> [(City,Maybe Distance)] -> Maybe Distance
getDist_fromDistanceList city ((c,d): rest) 
    | city == c = d --found what we wanted, return it
    | otherwise = getDist_fromDistanceList city rest--go through the list


updatePrevList :: RoadMap -> [(City, Maybe Distance)] -> City -> City -> [(City, City)] -> [(City, City)]
updatePrevList roadmap distanceList current neighbour prevList =
    let
        currentDistance = getDist_fromDistanceList current distanceList-- calculate the distance from the starting city to the current city
        distanceToNeighbour = distance roadmap current neighbour -- calculate the distance from current to neighbour
        newDistance = addMaybe currentDistance distanceToNeighbour -- calculate the new potential distance to neighbour through current
        neighbourDistance = getDist_fromDistanceList neighbour distanceList -- get the neighbour's current known distance from distanceList
    in
        case (newDistance, neighbourDistance) of
        (Just dNew, Just dNeighbour)
            | dNew < dNeighbour -> 
                -- if new distance is shorter, remove previous entries for neighbour and add the new one
                (neighbour, current) : filter ((/= neighbour) . fst) prevList

            | dNew == dNeighbour -> 
                -- if new distance is equal, add the new predecessor without removing existing entries
                (neighbour, current) : prevList

        _ -> prevList  -- case of Nothing, return prevList unchanged


-- initialize distances to infinity for all nodes except the start node
-- so serve para a chamada da aux
setinitialDistance :: RoadMap -> City -> [(City,Maybe Distance)]
setinitialDistance roadmap start_city = [(city, distance) | city <- cities roadmap, let distance = if city == start_city then Just 0 else Just (maxBound :: Int)]


getPaths :: City -> City -> [(City, City)] -> [Path]
getPaths city_end city_start city_pairs
    | city_end == city_start = [[city_end]]  -- Base case: reached the destination
    | null prevCities = []                     -- No paths if no predecessors
    | otherwise = (concatMap (\prev_city ->
        let paths = getPaths prev_city city_start city_pairs
        in map (city_end :) paths) prevCities)
    where
        prevCities = findPrevCities city_end city_pairs

-- Find predecessor cities of `city_start` in the list of city pairs
findPrevCities :: City -> [(City, City)] -> [City]
findPrevCities city city_pairs = [prev_city | (current_city, prev_city) <- city_pairs, current_city == city]


{-///////////////////////////////////////////////////////////////////////////////////////////////
-- 9 
///////////////////////////////////////////////////////////////////////////////////////////////-}



travelSales :: RoadMap -> Path
travelSales roadmap
   | isStronglyConnected roadmap = auxTravelSales roadmap
   | otherwise = []

--auxTravelSales :: RoadMap -> MemoTable
--auxTravelSales roadmap = solvedMemoTable
auxTravelSales :: RoadMap -> Path
auxTravelSales roadmap = tour_path
  where
    -- Step 1: Convert roadmap to city indices and adjacency matrix - DONE
    cityIndicesList = cityIndices roadmap
    numCities = length cityIndicesList --N
    startCity = firstCity roadmap
    startIndex = getCityIndex cityIndicesList startCity
    adjacencyMatrix = toAdjacencyMatrix roadmap

    --Step 2: Initialize memo table - DONE 
    initialMemoTable = Data.Array.array ((0, 0), (numCities - 1, Data.Bits.shiftL 1 numCities - 1))[((i, j), Just maxBound) | i <- [0..numCities-1], j <- [0..Data.Bits.shiftL 1 numCities - 1]]
    memoTable = initMemoTable adjacencyMatrix initialMemoTable startCity cityIndicesList

    --Step 3: updates memo with the optimal values of all neighbour/start edges - DONE
    set_up = setup adjacencyMatrix memoTable startIndex numCities -- set up da memo table
   
    --Step 4: calculate all permutaions of tours & partial tours, add them to the memoTable -D ONE
    solvedMemoTable = solveTsp adjacencyMatrix set_up startIndex numCities

    --Step 5: Reconstruct path from memo table using indeces
    optimal = findOptimalTour adjacencyMatrix solvedMemoTable startIndex numCities

    --Step 6: convert to path - DONE
    tour_path = toPath optimal cityIndicesList


--calculates all the combinations of paths with DP
solveTsp :: AdjacencyMatrix -> MemoTable -> Int -> Int -> MemoTable
solveTsp matrix memoTable startNode numCities = foldl updateMemo memoTable [1..(2 ^ numCities - 1)]
  where
    -- update memo table for each subset
    updateMemo :: MemoTable -> Int -> MemoTable
    updateMemo currentMemo subset
      | not (isInSubset startNode subset) = currentMemo  -- Skip subsets not containing the start node
      | otherwise = foldl (processEndCity currentMemo subset) currentMemo [0..numCities-1]

    -- process the end city for a given subset
    processEndCity :: MemoTable -> Int -> MemoTable -> Int -> MemoTable
    processEndCity currentMemo subset currentMemo' endCity
        | endCity == startNode || not (isInSubset endCity subset) = currentMemo'
        | otherwise = 
            let state = subset `Data.Bits.xor` (1 `Data.Bits.shiftL` endCity)
                validPreviousCities = [prevCity | prevCity <- [0..numCities-1], prevCity /= startNode, prevCity /= endCity, isInSubset prevCity subset]
                newDistances = [ let prevDist = memo currentMemo prevCity state
                                in case prevDist of
                                    Just mv -> let edgeDist = matrix Data.Array.! (prevCity, endCity)
                                        in case edgeDist of
                                                    Just mx -> max 0 (mv + mx)  -- Ensure we do not get negative
                                                    Nothing -> maxBound
                                    Nothing -> maxBound
                                | prevCity <- validPreviousCities]
                minDistance = if null newDistances then Nothing else Just (minimum newDistances)
                currentMinDist = memo currentMemo' endCity subset
            in case (minDistance, currentMinDist) of
                (Just d, Nothing) -> currentMemo' Data.Array.// [((endCity, subset), Just d)]
                (Just d, Just cm) | d < cm -> currentMemo' Data.Array.// [((endCity, subset), Just d)]
                _ -> currentMemo'


-- Finds optimal tour and adds all the indexes of cities said tour into a list, in order
findOptimalTour :: AdjacencyMatrix -> MemoTable -> Int -> Int -> [Int]
findOptimalTour matrix memoTable startNode numCities = reverse $ reconstructPath finalCity finalState []
  where
    finalState = (2 ^ numCities - 1)
    finalCity = startNode

    -- reconstruct the path by backtracking
    reconstructPath :: Int -> Int -> [Int] -> [Int]
    reconstructPath city state path
      | state == 0 = path  -- If we exhausted the state, return the path
      | otherwise =
          let previousCity = Data.List.minimumBy (\c1 c2 -> compare (costTo c1) (costTo c2)) validPreviousCities
              newPath = previousCity : path
              newState = state `Data.Bits.xor` (1 `Data.Bits.shiftL` previousCity)
          in reconstructPath previousCity newState newPath
      where
        -- get all valid previous cities that are in the current subset
        validPreviousCities = [prevCity | prevCity <- [0..numCities-1], prevCity /= city, isInSubset prevCity state]

        -- calculate the cost to reach the current city from a previous city
        costTo prevCity = case memoTable Data.Array.! (prevCity, state `Data.Bits.xor` (1 `Data.Bits.shiftL` city)) of
            Just prevCost -> case matrix Data.Array.! (prevCity, city) of
                Just edgeCost -> prevCost + edgeCost
                Nothing -> maxBound
            Nothing -> maxBound            
-- helper for findOptimalTour
isInSubset :: Int -> Int -> Bool
isInSubset city subset = subset `Data.Bits.testBit` city


-- Converts a list of indices into a path using a city index list
toPath::[Int] -> [(City, Int)] -> Path
toPath optimalPathIndexList cityIndicesList = [fst cityTuple | index <- optimalPathIndexList,  index >= 0, index < length cityIndicesList, let cityTuple = cityIndicesList !! index]




-- creates a adj Matrix out of a roadmap
toAdjacencyMatrix :: RoadMap -> AdjacencyMatrix
toAdjacencyMatrix roadmap = Data.Array.array bounds elements
  where
    -- generate city indices for consistent indexing
    indices = cityIndices roadmap
    numCities = length indices
    bounds = ((0, 0), (numCities - 1, numCities - 1))
    
    -- create all pairs with `Nothing` initially to indicate no path
    allPairs = [((i, j), if i == j then Just 0 else Nothing) | i <- [0 .. numCities - 1], j <- [0 .. numCities - 1]]
    
    -- convert roadmap into pairs with actual distances, ensuring symmetry
    roadPairs = [((getCityIndex indices c1, getCityIndex indices c2), Just d) | (c1, c2, d) <- roadmap]
    symmetricPairs = roadPairs ++ [((j, i), Just d) | ((i, j), Just d) <- roadPairs, i /= j]

    -- merge default pairs with roadPairs, ensuring symmetry
    elements = foldl (\acc (pos, dist) -> (pos, dist) : filter ((/= pos) . fst) acc) allPairs symmetricPairs


--initiates the memo table with infinite values
setup :: AdjacencyMatrix -> MemoTable -> Int -> Int -> MemoTable
setup matrix memoTable startNode numCities =
    memoTable Data.Array.// updates
  where
    -- Generate updates for each city except the start node
    updates = 
      [ ((i, (1 `Data.Bits.shiftL` startNode) Data.Bits..|. (1 `Data.Bits.shiftL` i)), matrix Data.Array.! (startNode, i))
      | i <- [0..numCities-1], i /= startNode, 
        matrix Data.Array.! (startNode, i) /= (Just maxBound)  -- Ensure only valid paths are updated
      ]

cityIndices :: RoadMap -> [(City, Int)]
cityIndices roadmap = zip (cities roadmap) [0..]

-- Get the first city from roadmap to start TSP
firstCity :: RoadMap -> City
firstCity [] = error "No cities in this roadmap"
firstCity ((city, _, _):_) = city


-- Map cities to indices for easier array operations
getCityFromIndice :: Int -> [(City, Int)] -> City
getCityFromIndice id ((city, idx):rest)
    | id == idx = city
    | otherwise = getCityFromIndice id rest

-- Get city index by name
getCityIndex :: [(City, Int)] -> City -> Int
getCityIndex ((currentCity, idx):rest) city
    | city == currentCity = idx
    | otherwise = getCityIndex rest city
    
-- Initialize memo table with distances from the start city
initMemoTable :: AdjacencyMatrix -> MemoTable -> City -> [(City, Int)] -> MemoTable
initMemoTable matrix memoTable startCity cityIndices = memoTable Data.Array.// updates
  where
    startIndex = getCityIndex cityIndices startCity
    updates = [((i, Data.Bits.shiftL 1 startIndex Data.Bits..|. Data.Bits.shiftL 1 i), matrix Data.Array.! (startIndex, i)) | (_, i) <- cityIndices, i /= startIndex]

notIn :: Int -> Int -> Bool
notIn node visited = (visited Data.Bits..&. Data.Bits.shiftL 1 node) == 0
    
generateSubsets :: Int -> Int -> [Int]
generateSubsets r n = filter (\s -> Data.Bits.popCount s == r) [1..(2^n - 1)]

memo :: MemoTable -> Int -> Int -> Maybe Distance
memo table city subset = table Data.Array.! (city, subset)

{-///////////////////////////////////////////////////////////////////////////////////////////////
-- some auxiliars
///////////////////////////////////////////////////////////////////////////////////////////////-}
addMaybe :: Num a => Maybe a -> Maybe a -> Maybe a
addMaybe (Just x) (Just y) = Just (x + y)
addMaybe _ _ = Nothing 

cityAdjacencyList :: RoadMap -> [(City, Int)] --cidade e o numero de cidades que la ligam
cityAdjacencyList roadmap = [(city, length (adjacent roadmap city)) | city <- city_list]
    where 
        city_list = cities roadmap  

------------------------------------------------------
-- Funcs used to TO TEST 
-- printMemoTable (checkSetUp gTest5)
-- testar o print printAdjacencyMatrix (toAdjacencyMatrix gTeste1) (cityIndices gTeste1)
------------------------------------------------------
--------------------
--CHECKS /TESTING AREA
-------------------
checkSolve :: RoadMap -> MemoTable
checkSolve roadmap =  solveTsp adjm new_memoTable start_city_index numCities
    where
        numCities = Data.List.length (cities roadmap)
        start_city_index = getCityIndex cityIndicesList startCity
        startCity = firstCity roadmap
        cityIndicesList = cityIndices roadmap
        adjm = toAdjacencyMatrix roadmap
        
        new_memoTable = setup adjm initialMemoTable start_city_index numCities
        
        initialMemoTable = Data.Array.array ((0, 0), (numCities - 1, Data.Bits.shiftL 1 numCities - 1)) [((i, j), Just maxBound) | i <- [0..numCities-1], j <- [0..Data.Bits.shiftL 1 numCities - 1]]
        memoTable = initMemoTable adjm initialMemoTable startCity cityIndicesList

printMemoTable' :: MemoTable -> IO ()
printMemoTable' memoTable = mapM_ print [((i, bitmask), dist) | ((i, bitmask), dist) <- Data.Array.assocs memoTable]

printAdjacencyMatrix :: AdjacencyMatrix -> [(City, Int)] -> IO ()
printAdjacencyMatrix matrix cityIndices = do
    let ((_, _), (numCitiesMinus1, _)) = Data.Array.bounds matrix
        numCities = numCitiesMinus1 + 1
    putStrLn "Adjacency Matrix:"
    putStrLn "-----------------"
    mapM_ (printRow numCities) [0 .. numCities - 1]
  where
    printRow numCities i = do
        let row = [if (i, j) `elem` Data.Array.indices matrix then matrix Data.Array.! (i, j) else Just maxBound | j <- [0 .. numCities - 1]]
        putStrLn $ show (fst (cityIndices !! i)) ++ " | " ++ show row
    
-------------------------------------------------------------------------------------------------------

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
gTest4 = [("A", "B", 5), ("C", "B", 5), ("C", "D", 3), ("E", "F", 7), ("F", "A", 1), ("D", "E", 2)]

gTest5 :: RoadMap
gTest5 = [("A", "B", 2), ("A", "C", 4), ("B", "C", 5),("B", "K", 7),("B", "F", 20),("C" , "F" ,8),("K" , "E" ,1),("E", "F",2)]
--best path F E K B A
--best tour A B K E F C A -> TOUR COST: 24

gTesy6 :: RoadMap
gTesy6 = [("A", "B", 10), ("B", "C", 15), ("C", "A", 20)]

gTest7 :: RoadMap
gTest7 = [("A", "B", 5), ("C", "B", 5), ("C", "D", 3), ("D","A",2)]

mTest1 :: MemoTable
mTest1 = Data.Array.array ((0, 0), (5, Data.Bits.shiftL 1 5 - 1)) [((i, j), Just maxBound) | i <- [0..5], j <- [0..Data.Bits.shiftL 1 5 - 1]]




