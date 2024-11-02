
import qualified Data.List
import qualified Data.Array
--import qualified Data.Bits


-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int
type RoadMap = [(City,City,Distance)]
type AdjMatrix = Data.Array.Array (Int, Int) (Maybe Distance)
type AdjList = [(City, [(City, Distance)])]


------------------------------------------- 1 ------------------------------------------
--Extracts a unique list of all cities present in the RoadMap.
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
createAllDistancesArray :: RoadMap -> City ->  Data.Array.Array Int (City, Distance)
createAllDistancesArray rm start = Data.Array.array (0, n - 1) 
    [(i, (city, if i == read start then 0 else maxBound :: Distance)) | (i, city) <- zip [0..] citys]
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

--Description: Reconstructs the shortest path from the start city to the end city using the distances array.
--Arguments:
--distances: An array where each element holds a city, the predecessor city on the path, and the distance.
--end: The destination city.
--start: The starting city.
getPath :: Data.Array.Array Int (City, Distance) -> City -> City -> [City]
getPath distances end start =
    reverse $ go end []
  where
    go current path
      | current == start = start : path
      | otherwise =
          let (predecessor, _) = distances Data.Array.! (read current)
          in go predecessor (current : path)


data MinHeap = MinHeap {
    heapArray :: Data.Array.Array Int (City, City, Distance), -- Array to store heap elements
    size :: Int,                                         -- Current number of elements in the heap
    capacity :: Int                                     -- Maximum capacity of the heap
}

--Description: Initializes a min-heap with a specified capacity, setting all distances to maxBound.
--Arguments:
--capacity: The maximum number of elements the heap can hold.
createHeap :: Int -> MinHeap
createHeap capacity = MinHeap {
    heapArray = Data.Array.array (0, capacity - 1) [ (i, ("", "", maxBound :: Distance)) | i <- [0..capacity - 1]],
    size = 0,
    capacity = capacity
}

--Description: Adds a city with a given distance to the heap, maintaining the min-heap property.
--Arguments:
--heap: The current min-heap.
--newElement: A tuple containing the current city, the adjacent city, and the distance to that city.
insert :: MinHeap -> (City, City, Distance) -> MinHeap
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
extractMin :: MinHeap -> ((City, City, Distance), MinHeap)
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
    
--Description: This function relaxes the distances of adjacent cities from the current city, updating the min-heap if a shorter path is found.
--Arguments:
--currentCity: The city currently being processed.
--currentDist: The distance from the start city to the current city.
--heap: The current state of the min-heap.
--adjList: The adjacency list representing the graph of cities and distances.
--visited: A list of cities that have already been visited.
relaxAdjacentCities :: City -> Distance -> MinHeap -> AdjList -> [City] -> MinHeap
relaxAdjacentCities currentCity currentDist heap adjList visited =
        foldr (\(adjCity, dist) h ->
                  if adjCity `notElem` visited  -- Checking if city have already been visited
                  then
                      let newDist = currentDist + dist
                          shouldInsert = case findMinForCity h adjCity of 
                              Nothing -> True  -- adjCity not in heap
                              Just (_, _, existingDist) -> newDist < existingDist
                      in if shouldInsert
                         then insert h (currentCity, adjCity, newDist)
                         else h
                  else h 
              ) heap (getAdjacentCities adjList currentCity)

--Description: This function searches for the minimum distance entry for a given city in the min-heap and returns it if found.
--Arguments:
--heap: The current state of the min-heap.
--targetCity: The city for which the minimum distance is being searched.
findMinForCity :: MinHeap -> City -> Maybe (City, City, Distance)
findMinForCity (MinHeap arr size _) targetCity = 
    let elements = [arr Data.Array.! i | i <- [0..size-1]]
    in foldr 
        (\(startCity, city, dist) acc -> 
            if city == targetCity 
            then case acc of 
                    Nothing -> Just (startCity, city, dist) 
                    Just (_, _, existingDist) -> if dist < existingDist 
                                                 then Just (startCity, city, dist) 
                                                 else acc
            else acc
        ) 
        Nothing 
        elements

--Description: This function computes the shortest path between two cities in a road map using Dijkstra's algorithm.
--Arguments:
--rm: The road map containing city connections and distances.
--start: The starting city.
--end: The destination city.
shortestPath :: RoadMap -> City -> City -> Path
shortestPath rm start end = reverse $ getPath (dijkstra adjList heap distances []) end start
  where
    adjList = createAdjList rm
    distances = createAllDistancesArray rm start
    heap = insert (createHeap 100) (start, start, 0) -- 100 edges of capacity

    --Description: This is the core function of Dijkstra's algorithm that iteratively extracts the minimum distance node, relaxes its adjacent nodes, and updates the distances in the min-heap until all nodes are processed or the destination is reached.
    --Arguments:
    --adjList: The adjacency list representing the graph.
    --heap: The current state of the min-heap.
    --distances: An array holding the minimum distances from the start city to each city.
    --visited: A list of cities that have already been visited during the algorithm's execution.
    dijkstra :: AdjList -> MinHeap -> Data.Array.Array Int (City, Distance) -> [City] -> Data.Array.Array Int (City, Distance)
    dijkstra adjList heap distances visited
        | size heap == 0 = distances  -- Empty heap, end of search
        | otherwise =
            let
                ((startCity, closestCity, d), newHeap) = extractMin heap -- Closest edge
                (_, oldDist) = distances Data.Array.! (read closestCity)
                (_, startDist) = distances Data.Array.! (read startCity)
                newDist = startDist + d
                updatedDistance = if oldDist > newDist
                                    then distances Data.Array.// [(read closestCity, (startCity, newDist))]
                                    else distances

                updatedHeap = relaxAdjacentCities closestCity d newHeap adjList visited
            in
                if closestCity == end
                    then distances  -- Found the end
                    else dijkstra adjList updatedHeap updatedDistance (closestCity:visited)
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


--Description: This function computes the initial distance from a start city to an intermediate city i through another city c using the adjacency matrix.
--Arguments:
--startPoint: The starting city.
--i: The intermediate city.
--c: The city used to calculate the distance.
--matrix: The adjacency matrix.
initialDist :: City -> City -> City -> AdjMatrix -> Maybe Distance
initialDist startPoint i c matrix = do
    d1 <- matrix Data.Array.! (read startPoint, read c) 
    d2 <- matrix Data.Array.! (read c, read i) 
    return (d1 + d2) 

--Description: This function calculates the total distance from the start city to an intermediate city i through another city c, given a potential distance.
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

--Description: This function implements a variant of the Traveling Salesman Problem algorithm, computing the minimum distance for visiting all cities starting from the first city, and returning the distance and the path taken. ?????
--Arguments:
--rm: The road map containing city connections and distances.
travelSales :: RoadMap -> Path
travelSales rm
    | optDist == Nothing = []
    | otherwise = optPath ++ [city]
    
    where
        citiess = cities rm
        city = head citiess
        matrix = createAdjMatrix rm

        (optDist, optPath) = helper city city (tail citiess)
        
        helper :: City -> City -> [City] -> (Maybe Distance,Path)
        helper startPoint i [c] = (initialDist startPoint i c matrix, [i,c])
        helper startPoint i cs = (distance, i:pathh)
            where
                (distance,pathh) = minim (map (\(dist, path) -> (sumDist dist i (head path) matrix, path)) pathList)
                pathList = map (\c -> helper startPoint c (filter (/= c) cs)) cs
----------------------------------------------------------------------------------------


-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]


gTest4 :: RoadMap 
gTest4 = [("0","1",4),("2","3",2), ("1","2",3),("3","0",3)]
