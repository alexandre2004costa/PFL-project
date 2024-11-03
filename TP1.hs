
import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

import Debug.Trace (trace)


-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int
type RoadMap = [(City,City,Distance)]
--------------------------------------------------------------
type AdjMatrix = Data.Array.Array (Int, Int) (Maybe Distance)
type AdjList = [(City, [(City, Distance)])]
--------------------------------------------------------------
type TableCoord = (Int, Int)
type TableEntry = (Maybe Distance, Path)
type Table = Data.Array.Array TableCoord TableEntry



------------------------------------------- 1 ------------------------------------------
--Description: Extracts a unique list of all cities present in the RoadMap.
--Arguments:
--rm: The road map of city connections and distances.
cities :: RoadMap -> [City]
cities [] = []
cities rm = Data.List.nub $ allCities rm
    where 
        allCities [] = []
        allCities ((c1,c2,_):xs) = c1 : c2 : allCities xs
----------------------------------------------------------------------------------------

------------------------------------------- 2 ------------------------------------------
--Description: Checks if two cities are directly connected in the RoadMap.
--Arguments:
--rm: The road map of city connections and distances.
--c1: The first city.
--c2: The second city.
--Details: Tests both possible directions (c1 -> c2) and (c2 -> c1) for adjacency, because graph is undirected.
areAdjacent :: RoadMap -> City -> City -> Bool 
areAdjacent rm c1 c2 = any (\(city1,city2,dist) -> (city1 == c1 && city2 == c2) || (city2 == c1 && city1 == c2)) rm
----------------------------------------------------------------------------------------

------------------------------------------- 3 ------------------------------------------
--Description: Calculates the distance between two directly connected cities, if available.
--Arguments:
--rm: The road map of city connections and distances.
--c1: The starting city.
--c2: The destination city.
distance :: RoadMap -> City -> City -> Maybe Distance 
distance [] c1 c2 = Nothing
distance rm c1 c2
    |dist > 0 = Just dist
    |otherwise = Nothing
        where dist = sum [d | (city1,city2,d) <- rm , (city1 == c1 && city2 == c2) || (city2 == c1 && city1 == c2)]
----------------------------------------------------------------------------------------

------------------------------------------- 4 ------------------------------------------
--Description: Lists all cities directly connected to a given city, along with their respective distances.
--Arguments:
--rm: The road map of city connections and distances.
--city: The initial city.
adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent [] city = []
adjacent ((c1,c2,d):xs) city
    | c1 == city = (c2,d) : adjacent xs city
    | c2 == city = (c1,d) : adjacent xs city
    | otherwise = adjacent xs city
----------------------------------------------------------------------------------------

------------------------------------------- 5 ------------------------------------------
--Description: Calculates the cumulative distance of a path.
--Arguments:
--rm: The road map of city connections and distances.
--path: A list of cities in the order they are visited.
pathDistance :: RoadMap -> Path -> Maybe Distance 
pathDistance rm [x] = Just 0
pathDistance rm (city1:city2:ps) = do
    d  <- distance rm city1 city2
    dp <- pathDistance rm (city2:ps)
    return (d + dp)
----------------------------------------------------------------------------------------

------------------------------------------- 6 ------------------------------------------
--Description: Finds cities with the highest degree (most direct connections) in the RoadMap.
--Arguments:
--rm: The road map of city connections and distances.
rome :: RoadMap -> [City]
rome rm = [city | (city, degree) <- degrees, degree == maxDegree]
    where
        degrees = [(city, length (adjacent rm city)) | city <- cities rm]
        maxDegree = maximum [degree | (_, degree) <- degrees]
        --maxDegree = foldr (\(_,degree) acc -> max degree acc) 0 degrees
----------------------------------------------------------------------------------------

------------------------------------------- 7 ------------------------------------------
--Description: Checks if the RoadMap is strongly connected, meaning all cities are reachable from any other city.
--Arguments:
--rm: The road map of city connections and distances.
--Details: Uses the helper cityIsStronglyConnected to verify reachability from a starting city.
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected rm 
    | null allCities = False
    | otherwise      = cityIsStronglyConnected rm [head allCities] 0
    where allCities = cities rm

--Description: Recursive helper that attempts to visit all cities in the RoadMap starting from an initial city.
--Arguments:
--rm: The road map of city connections and distances.
--cs: A list of cities currently reachable.
-- n: Counter for the number of visited cities before, to detect new additions and prevent infinite loops
cityIsStronglyConnected :: RoadMap -> [City] -> Int -> Bool
cityIsStronglyConnected rm cs n
    | length cs == length (cities rm) = True
    | length cs == n = False
    | otherwise = cityIsStronglyConnected rm (Data.List.nub adj) (length cs)
    where adj = cs ++ [c | city <- cs, (c,_) <- adjacent rm city]
----------------------------------------------------------------------------------------

------------------------------------------- 8 ------------------------------------------
--Description: Builds an adjacency list representation of the road map, listing all neighboring cities and distances for each city.
--Arguments:
--rm: The road map of city connections and distances.
createAdjList :: RoadMap -> AdjList 
createAdjList rm = foldr addRoad [] rm
  where
    addRoad (c1, c2, d) adjList = addNeighbor c1 c2 d (addNeighbor c2 c1 d adjList)
    addNeighbor city neighbor distance [] = [(city, [(neighbor, distance)])]
    addNeighbor city neighbor distance ((c, neighbors):rest)
      | city == c = (c, (neighbor, distance) : neighbors) : rest
      | otherwise = (c, neighbors) : addNeighbor city neighbor distance rest

--Description: Initializes an array to hold distances from the start city to all others, with the starting city distance set to 0 and others to maxBound.
--Arguments:
--rm: The road map of city connections and distances.
--start: The starting city.
createAllDistancesArray :: RoadMap -> City ->  Data.Array.Array Int ([City], Distance)
createAllDistancesArray rm start = Data.Array.array (0, n - 1) 
    [(i, ([], if i == read start then 0 else maxBound :: Distance)) | (i, city) <- zip [0..] citys]
    where
        citys = Data.List.nub $ cities rm
        n = length citys

--Description: Retrieves the list of directly connected cities (with distances) from a specified city.
--Arguments:
--adjList: The adjacency list of the road map.
--city: The city whose neighbors are to be found.
getAdjacentCities :: AdjList -> City -> [(City, Distance)]
getAdjacentCities [] _ = []
getAdjacentCities ((c, cd):xs) city
    | c == city = cd
    | otherwise = getAdjacentCities xs city


data MinHeap = MinHeap {
    heapArray :: Data.Array.Array Int (City, [City], Distance), -- Array to store heap elements
    size :: Int,                                         -- Current number of elements in the heap
    capacity :: Int                                     -- Maximum capacity of the heap
}

--Description: Initializes a min-heap with a specified capacity, setting all distances to maxBound.
--Arguments:
--capacity: The maximum number of elements the heap can hold.
createHeap :: Int -> MinHeap
createHeap capacity = MinHeap {
    heapArray = Data.Array.array (0, capacity - 1) [ (i, ("", [], maxBound :: Distance)) | i <- [0..capacity - 1]],
    size = 0,
    capacity = capacity
}

--Description: Adds a city with a given distance to the heap, maintaining the min-heap property.
--Arguments:
--heap: The current min-heap.
--newElement: A tuple containing the current city, the upcoming cities, and the distance to that cities.
insert :: MinHeap -> (City, [City], Distance) -> MinHeap
insert heap@(MinHeap arr size capacity) newElement
    | size < capacity = 
        let 
            newSize = size + 1
            newArr = arr Data.Array.// [(size, newElement)]  -- Add the new element at the end
            updatedHeap = heap { heapArray = newArr, size = newSize }
        in bubbleUp updatedHeap (newSize - 1)  -- Maintain heap property
    | otherwise = error "Heap is full"  -- Handle overflow case

--Description: Helper function to maintain the min-heap property by bubbling up
--Arguments:
--heap: The current min-heap.
--index: The index of the node to be "bubbled up."
bubbleUp :: MinHeap -> Int -> MinHeap
bubbleUp heap@(MinHeap arr size capacity) index
    | index > 0 = let parentIndex = (index - 1) `div` 2
                      (childCityA, childCityB, childDist) = arr Data.Array.! index
                      (parentCityA, parentCityB, parentDist) = arr Data.Array.! parentIndex
                  in if childDist < parentDist
                     then let newArr = arr Data.Array.// [(parentIndex, (childCityA, childCityB, childDist)),(index, (parentCityA, parentCityB, parentDist))]
                          in bubbleUp (heap { heapArray = newArr }) parentIndex
                     else heap
    | otherwise = heap  -- No need to bubble up if we're at the root

--Description: This function extracts the minimum element from the min-heap and returns it along with the new state of the heap.
--Arguments:
--heap: The current state of the min-heap.
extractMin :: MinHeap -> ((City, [City], Distance), MinHeap)
extractMin heap@(MinHeap arr size capacity)
    | size == 0 = error "Heap is empty"  -- Handle underflow
    | otherwise = (minElement, newHeap)
  where
    minElement = arr Data.Array.! 0  -- Min element is in root
    lastElement = arr Data.Array.! (size - 1)  -- Last element
    newHeap = bubbleDown (heap { heapArray = newArr, size = size - 1 }) 0 -- Mantain heap property by "Bubbling-down"
    newArr = arr Data.Array.// [(0, lastElement)]  -- Change lastElement for the min


--Description: This helper function ensures the min-heap property is maintained by moving a node down in the heap until both children nodes are larger than the node being moved.
--Arguments:
--heap: The current state of the min-heap.
--index: The index of the node to be "bubbled down."
bubbleDown :: MinHeap -> Int -> MinHeap
bubbleDown heap@(MinHeap arr size capacity) index
    | 2 * index + 1 < size =  -- If left side child exists
        let 
            leftChildIndex = 2 * index + 1
            rightChildIndex = 2 * index + 2
            (parentCityA, parentCityB, parentDist) = arr Data.Array.! index
            (leftCityA, leftCityB, leftDist) = arr Data.Array.! leftChildIndex
            (rightCityA, rightCityB, rightDist) =
                if rightChildIndex < size 
                    then arr  Data.Array.! rightChildIndex
                    else (parentCityA, parentCityB, parentDist) -- No right child
            minChildIndex = if rightChildIndex < size && rightDist < leftDist
                                then rightChildIndex
                                else leftChildIndex
            (minChildCityA, minChildCityB, minChildDist) = arr  Data.Array.! minChildIndex
         in   
            if minChildDist < parentDist
                then
                    let newArr = arr  Data.Array.// [(index, (minChildCityA, minChildCityB, minChildDist)), (minChildIndex, (parentCityA, parentCityB, parentDist))]
                    in bubbleDown (heap { heapArray = newArr }) minChildIndex
                else heap 
            
    | otherwise = heap  

--Description: Updates the element at position index in the heap’s array with newValue.
--Arguments:
--MinHeap: The current min-heap.
--index: The index where the value should be updated.
--newValue: The new tuple (City, [City], Distance) to be placed at the specified index.
updateAtIndex :: MinHeap -> Int -> (City, [City], Distance) -> MinHeap
updateAtIndex (MinHeap arr size capacity) index newValue =
    MinHeap (arr  Data.Array.// [(index, newValue)]) size capacity

--Description: Returns the index of targetCity if it exists in the heap; otherwise, returns Nothing.
--Arguments:
--MinHeap: The current min-heap.
--targetCity: The city we want to locate.
findCityIndex :: MinHeap -> City -> Maybe Int
findCityIndex (MinHeap arr size _) targetCity =
    let elements = [(i, arr  Data.Array.! i) | i <- [0..size-1]]
    in fmap fst $ Data.List.find (\(_, (city, _, _)) -> city == targetCity) elements  

--Description: For each unvisited adjacent city, calculates a new distance. If this new distance is smaller or equal to the existing distance, the heap is updated.
--Arguments:
--currentCity: The current city from which we're attempting to relax paths.
--currentDist: The accumulated distance to currentCity.
--MinHeap: The min-heap that will store the updated distances.
--AdjList: The adjacency list representing the graph.
--visited: The list of already visited cities, which will be ignored.
relaxAdjacentCities :: City -> Distance -> MinHeap -> AdjList -> [City] -> MinHeap
relaxAdjacentCities currentCity currentDist heap adjList visited =
        foldr (\(adjCity, dist) h' -> 
            if adjCity `notElem` visited
            then
                let newDist = currentDist + dist
                in case findCityIndex h' adjCity of
                    Nothing -> insert h' (adjCity, [currentCity], newDist)  -- Primeiro predecessor
                    Just index ->
                        let (_, preds, existingDist) = heapArray h' Data.Array.! index
                        in if newDist < existingDist
                           then updateAtIndex h' index (adjCity, [currentCity], newDist)  -- Substitui com nova distância e predecessor
                           else if newDist == existingDist
                                then updateAtIndex h' index (adjCity, currentCity : preds, existingDist)  -- Adiciona predecessor ao empate
                                else h'
            else h'
        ) heap (getAdjacentCities adjList currentCity)
    
--Description: Calls explorePaths to explore all paths while avoiding cycles, returning a list of paths.
--Arguments:
--distances: An array where each entry contains a list of predecessors and the accumulated distance to the city at the corresponding index.
--start: The starting city of the path.
--end: The ending city of the path.
getAllPaths :: Data.Array.Array Int ([City], Distance) -> City -> City -> [[City]]
getAllPaths distances start end = explorePaths distances start end []

--Description: Recursively explores paths from current to end, avoiding cycles by keeping track of visited cities. Returns all possible paths that reach the end city.
--Arguments:
--Data.Array.Array Int ([City], Distance): Array where each entry holds a list of predecessor cities and the minimum distance to the city at the corresponding index.
--current: The current city being explored.
--end: The target city for paths.
--visited: List of cities that have been visited along the current path, used to prevent cycles.
explorePaths :: Data.Array.Array Int ([City], Distance) -> City -> City -> [City] -> [[City]]
explorePaths distances current end visited
    | current == end = [(end : visited)] 
    | otherwise =
        let (predecessors, _) = distances Data.Array.! read current 
        in concatMap (\pred -> if pred `elem` visited
                                 then [] 
                                 else explorePaths distances pred end (current : visited)) predecessors

--Description: Updates the array with newDistance and closestCities if newDistance is less than the current distance; if it is equal, it adds closestCities to the existing predecessors.
--Arguments:
--distances: Array with distances and predecessors for each city.
--startCity: The city from which the predecessors originate.
--closestCities: List of the closest predecessor cities.
--newDistance: The newly calculated distance.    
updateDistances :: Data.Array.Array Int ([City], Distance) -> City -> [City] -> Distance -> Data.Array.Array Int ([City], Distance)
updateDistances distances startCity closestCities newDistance =
    let
        startIndex = read startCity 
        (predecessors, existingDistance) = distances Data.Array.! startIndex
        updatedDistances =
            if newDistance < existingDistance
            then distances Data.Array.// [(startIndex, (closestCities, newDistance))]
            else if newDistance == existingDistance
                 then distances Data.Array.// [(startIndex, (closestCities ++ predecessors, existingDistance))] 
                 else distances 
    in
        updatedDistances

--Description: This function computes the shortest paths between two cities in a road map using Dijkstra's algorithm.
--Arguments:
--rm: The road map containing city connections and distances.
--start: The starting city.
--end: The destination city.
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath rm start end = getAllPaths (dijkstra adjList heap distances []) end start
  where
    adjList = createAdjList rm
    distances = createAllDistancesArray rm start
    heap = insert (createHeap 100) (start, [start], 0) -- 100 edges of capacity

    --Description: This is the core function of Dijkstra's algorithm that iteratively extracts the minimum distance node, relaxes its adjacent nodes, and updates the distances in the min-heap until all nodes are processed or the destination is reached.
    --Arguments:
    --adjList: The adjacency list representing the graph.
    --heap: The current state of the min-heap.
    --distances: An array holding the minimum distances from the start city to each city and the predecessor cities.
    --visited: A list of cities that have already been visited during the algorithm's execution.
    dijkstra :: AdjList -> MinHeap -> Data.Array.Array Int ([City], Distance) -> [City] -> Data.Array.Array Int ([City], Distance)
    dijkstra adjList heap distances visited
        | size heap == 0 = distances  -- Empty heap, end of search
        | otherwise =
            let
                ((startCity, closestCitys, d), newHeap) = extractMin heap 
                updatedDistance =  updateDistances distances startCity closestCitys d
                updatedHeap = relaxAdjacentCities startCity d newHeap adjList visited
            in
                if startCity == end 
                    then updatedDistance  -- Found the end
                    else dijkstra adjList updatedHeap updatedDistance (startCity : visited)
----------------------------------------------------------------------------------------

------------------------------------------- 9 ------------------------------------------
--Description: This function creates an empty adjacency matrix of the specified size, initializing all entries to Nothing, indicating no edges between any nodes.
--Arguments:
--size: The number of cities (nodes) in the matrix.
createEmptyMatrix :: Int -> AdjMatrix
createEmptyMatrix size = Data.Array.array ((0, 0), (size - 1, size - 1)) [((c1, c2), Nothing) | c1 <- [0..size-1], c2 <- [0..size-1]]

--Description: This function adds an edge with a specific distance between two cities to the adjacency matrix, updating both directions since the graph is undirected.
--Arguments:
--matrix: The current adjacency matrix.
--(c1, c2, dist): A tuple representing two cities and the distance between them.
addEdgeMatrix :: AdjMatrix -> (City, City, Distance) -> AdjMatrix
addEdgeMatrix matrix (c1, c2, dist) = matrix Data.Array.// [((read c1, read c2), Just dist)] Data.Array.// [((read c2, read c1), Just dist)]

--Description: This function creates an adjacency matrix for the given road map by adding all edges defined in the road map to an initially empty matrix.
--Arguments:
--rm: The road map containing city connections and distances.
createAdjMatrix :: RoadMap -> AdjMatrix
createAdjMatrix rm = foldl addEdgeMatrix (createEmptyMatrix nCities) rm
    where nCities = length (cities rm)


--Description: Converts a bitmask integer to a subset list.
--Arguments:
--n: An integer that represents a bitmask, where each bit indicates the presence of a city on the list.
intToSubset :: Int -> [Int]
intToSubset n = [i | i <- [0 .. (Data.Bits.finiteBitSize n - 1)], Data.Bits.testBit n i]

--Description: Converts a subset list into a bitmask integer.
--Arguments:
--subset: A list of integers representing the cities in the subset.
subsetToInt :: [Int] -> Int
subsetToInt subset = foldl Data.Bits.setBit 0 subset

--Description: Creates the subset without a specific city.
--Arguments:
--c: An integer representing the city to be removed.
--set: A list of integers representing the current set of cities.
createSubset :: Int -> [Int] -> Int
createSubset c set = subsetToInt (filter (/= c) set)


--Description: Calculates the cumulative distance by adding the distance between two cities to a given distance
--Arguments:
--dist: The current distance (as a Maybe value).
--i: The intermediate city.
--c: The city used to add to the distance.
--matrix: The adjacency matrix.
sumDist :: Maybe Distance -> City -> City -> AdjMatrix -> Maybe Distance
sumDist dist i c matrix = do
    dist <- dist 
    d <- matrix Data.Array.! (read i, read c) 
    return (dist + d)  

--Description: This function finds the minimum distance from a list of distance-path pairs, returning the pair with the smallest distance.
--Arguments:
--A list of tuples, each containing a Maybe Distance and a corresponding path.
minim :: [(Maybe Distance,Path)] -> (Maybe Distance,Path)
minim [] = (Nothing, [])
minim [x] = x
minim (x1:x2:xs) = minim ((mini x1 x2):xs)
    where
        mini :: (Maybe Distance,Path) -> (Maybe Distance,Path) -> (Maybe Distance,Path)
        mini (Nothing, _) (Nothing, _) = (Nothing, []) 
        mini x1 (Nothing, _) = x1
        mini (Nothing, _) x2 = x2                      
        mini x1@(Just d1, p1) x2@(Just d2, p2)
            | d1 < d2 = x1
            | otherwise = x2


--Description: Creates an empty table to store the results of the dynamic programming solution
--Arguments:
--n: The number of cities.
createTableMatrix :: Int -> Table
createTableMatrix n = Data.Array.array ((0, 0), (n - 1, 2^n - 1))
                      [((city, int_subset), (Nothing, [])) | city <- [0..n-1], int_subset <- [0 .. (2^n - 1)]]

--Description: Computes and returns the entry (distance and path) of the table for a given city and subset of cities.
--Arguments:
--matrix: The adjacency matrix  with the distances between cities.
--start: An integer representing the starting city.
--table: The table used for dynamic programming.
--(i, subint): A coordinate tuple representing the current city and the subset of cities.
setEntryTable :: AdjMatrix -> Int -> Table -> TableCoord -> TableEntry
setEntryTable matrix start table (i, subint)
    | subset == [] = (matrix Data.Array.! (i, start), [show i, show start])
    | otherwise = (dist, (show i):path)
    where
        (dist, path) = minim (map (\(dist, path) -> (sumDist dist (show i) (head path) matrix, path)) pathList)

        subset = intToSubset subint
        eachPath c = table Data.Array.! (c, createSubset c subset)  
        pathList = map eachPath subset


--Description: Fills the table with the entries for valid city subsets associated with each city.
--Arguments:
--matrix: The adjacency matrix with the distances between cities.
--table: The table used for dynamic programming.
--start: An integer representing the starting city.
--n: The number of cities.
fillTable :: AdjMatrix -> Table -> Int -> Int -> Table
fillTable matrix table startCity n = foldl fillEntry table validSubsets
  where

    allSubsets = [(i, subset) | subset <- [0 .. (2^n - 1)], i <- [0..n-1]]
    validSubsets = filter isValid allSubsets

    isValid (i, subint) 
        | i == startCity  = subset == intToSubset (createSubset startCity (intToSubset (2^n - 1)))
        | otherwise       = not (elem i subset) && not (elem startCity subset)
        where subset = intToSubset subint
    
    fillEntry t (i, subint) =
      let entry = setEntryTable matrix startCity t (i, subint)
      in t Data.Array.// [((i, subint), entry)]
         

--Description: Calculates and returns the optimal path for the Traveling Salesman Problem using dynamic programming.
--Arguments:
--rm: The road map containing city connections and distances.
travelSales :: RoadMap -> Path
travelSales rm
    | optDist == Nothing = []
    | otherwise = optPath

    where 
        allCities = cities rm
        startCity = head allCities
        matrix = createAdjMatrix rm
        emptyTable = createTableMatrix (length allCities)

        startInt = read startCity
        table = fillTable matrix emptyTable startInt (length allCities)
        (optDist, optPath) = table Data.Array.! (startInt, createSubset startInt [read c | c <- allCities])  
----------------------------------------------------------------------------------------



-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]
