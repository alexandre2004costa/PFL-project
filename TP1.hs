
import qualified Data.List
import qualified Data.Array

--import qualified Data.Bits


-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int
type Edge = (City,City,Distance) -- Podemos definir este Edge ??????????????????
type RoadMap = [Edge]
type AdjMatrix = Data.Array.Array (Int, Int) (Maybe Distance)
type AdjList = [(City, [(City, Distance)])]

cities :: RoadMap -> [City]
cities [] = []
cities rm = Data.List.nub $ allCities rm
    where allCities ((c1,c2,d):xs) = c1 : c2 : cities xs


areAdjacent :: RoadMap -> City -> City -> Bool --Dont know if order matters here, so i just check both ways
areAdjacent rm c1 c2 = any (\(city1,city2,dist) -> (city1 == c1 && city2 == c2) || (city2 == c1 && city1 == c2)) rm

distance :: RoadMap -> City -> City -> Maybe Distance --Can change to recurssion also
distance [] c1 c2 = Nothing
distance rm c1 c2
    |dist > 0 = Just dist
    |otherwise = Nothing
        where dist = sum [d | (city1,city2,d) <- rm , (city1 == c1 && city2 == c2) || (city2 == c1 && city1 == c2)]

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent [] city = []
adjacent ((c1,c2,d):xs) city
    | c1 == city = (c2,d) : adjacent xs city
    | c2 == city = (c1,d) : adjacent xs city
    | otherwise = adjacent xs city

pathDistance :: RoadMap -> Path -> Maybe Distance --Maybe can be optimized, Log (n^2) for now
pathDistance rm [x] = Just 0
pathDistance rm (city1:city2:ps) = do
    d  <- distance rm city1 city2
    dp <- pathDistance rm (city2:ps)
    return (d + dp)


rome :: RoadMap -> [City]
rome rm = [city | (city, degree) <- degrees, degree == maxDegree]
    where
        degrees = [(city, length (adjacent rm city)) | city <- cities rm]
        maxDegree = maximum [degree | (_, degree) <- degrees]
        --maxDegree = foldr (\(_,degree) acc -> max degree acc) 0 degrees


isStronglyConnected :: RoadMap -> Bool
isStronglyConnected rm = cityIsStronglyConnected rm [head (cities rm)] 0

cityIsStronglyConnected :: RoadMap -> [City] -> Int -> Bool
cityIsStronglyConnected rm cs n
    | length cs == length (cities rm) = True
    | length cs == n = False
    | otherwise = cityIsStronglyConnected rm (Data.List.nub adj) (length cs)
    where adj = cs ++ [c | city <- cs, (c,_) <- adjacent rm city]

createAdjList :: RoadMap -> AdjList -- O(E*V)
createAdjList rm = foldr addRoad [] rm
  where
    addRoad (c1, c2, d) adjList = addNeighbor c1 c2 d (addNeighbor c2 c1 d adjList)
    addNeighbor city neighbor distance [] = [(city, [(neighbor, distance)])]
    addNeighbor city neighbor distance ((c, neighbors):rest)
      | city == c = (c, (neighbor, distance) : neighbors) : rest
      | otherwise = (c, neighbors) : addNeighbor city neighbor distance rest

createAllDistancesArray :: RoadMap -> City ->  Data.Array.Array Int (City, Distance)
createAllDistancesArray rm start = Data.Array.array (0, n - 1) 
    [(i, (city, if i == read start then 0 else maxBound :: Distance)) | (i, city) <- zip [0..] citys]
    where
        citys = Data.List.nub $ cities rm  -- A função `cities` deve retornar a lista de cidades
        n = length citys

getAdjacentCities :: AdjList -> City -> [(City, Distance)]
getAdjacentCities [] _ = []
getAdjacentCities ((c, cd):xs) city
    | c == city = cd
    | otherwise = getAdjacentCities xs city

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
createHeap :: Int -> MinHeap
createHeap capacity = MinHeap {
    heapArray = Data.Array.array (0, capacity - 1) [ (i, ("", "", maxBound :: Distance)) | i <- [0..capacity - 1]], -- Initialize with a large distance
    size = 0,
    capacity = capacity
}
insert :: MinHeap -> (City, City, Distance) -> MinHeap
insert heap@(MinHeap arr size capacity) newElement
    | size < capacity = 
        let 
            newSize = size + 1
            newArr = arr Data.Array.// [(size, newElement)]  -- Add the new element at the end
            updatedHeap = heap { heapArray = newArr, size = newSize }
        in bubbleUp updatedHeap (newSize - 1)  -- Maintain heap property
    | otherwise = error "Heap is full"  -- Handle overflow case

-- Helper function to maintain the min-heap property by bubbling up
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

-- Function to extract the minimum element from the heap
extractMin :: MinHeap -> ((City, City, Distance), MinHeap)
extractMin heap@(MinHeap arr size capacity)
    | size == 0 = error "Heap is empty"  -- Handle underflow
    | otherwise = (minElement, newHeap)
  where
    minElement = arr Data.Array.! 0  -- O menor elemento está na raiz
    lastElement = arr Data.Array.! (size - 1)  -- O último elemento na heap
    newHeap = bubbleDown (heap { heapArray = newArr, size = size - 1 }) 0
    newArr = arr Data.Array.// [(0, lastElement)]  -- Substitui o elemento raiz pelo último elemento


-- Helper function to maintain the min-heap property by bubbling down
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
                  else h  -- Se adjCity foi visitada, ignora
              ) heap (getAdjacentCities adjList currentCity)

findMinForCity :: MinHeap -> City -> Maybe (City, City, Distance)
findMinForCity (MinHeap arr size _) targetCity = 
    let elements = [arr Data.Array.! i | i <- [0..size-1]]
    in foldr 
        (\(startCity, city, dist) acc -> 
            if city == targetCity 
            then case acc of 
                    Nothing -> Just (startCity, city, dist)  -- Primeira ocorrência da cidade
                    Just (_, _, existingDist) -> if dist < existingDist 
                                                 then Just (startCity, city, dist) 
                                                 else acc
            else acc
        ) 
        Nothing 
        elements
shortestPath :: RoadMap -> City -> City -> Path
shortestPath rm start end = reverse $ getPath (dijkstra adjList heap distances []) end start
  where
    adjList = createAdjList rm
    distances = createAllDistancesArray rm start
    heap = insert (createHeap 100) (start, start, 0) -- 100 edges of capacity

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



createEmptyMatrix :: Int -> AdjMatrix
createEmptyMatrix size = Data.Array.array ((0, 0), (size - 1, size - 1)) [((c1, c2), Nothing) | c1 <- [0..size-1], c2 <- [0..size-1]]

addEdgeMatrix :: AdjMatrix -> (City, City, Distance) -> AdjMatrix
addEdgeMatrix matrix (c1, c2, dist) = matrix Data.Array.// [((read c1, read c2), Just dist)] Data.Array.// [((read c2, read c1), Just dist)]

createAdjMatrix :: RoadMap -> AdjMatrix
createAdjMatrix rm = foldl addEdgeMatrix (createEmptyMatrix nCities) rm
    where nCities = length (cities rm)


--------------------------------------------------------------------
initialDist :: City -> City -> City -> AdjMatrix -> Maybe Distance
initialDist startPoint i c matrix = do
    d1 <- matrix Data.Array.! (read startPoint, read c) 
    d2 <- matrix Data.Array.! (read c, read i) 
    return (d1 + d2) 

sumDist :: Maybe Distance -> City -> City -> AdjMatrix -> Maybe Distance
sumDist dist i c matrix = do
    dist <- dist 
    d <- matrix Data.Array.! (read i, read c) 
    return (dist + d)  

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


travelSales :: RoadMap -> (Maybe Distance,[City])
travelSales rm = helper city city (tail citiess)
    where
        citiess = cities rm
        city = head citiess
        matrix = createAdjMatrix rm
        
        helper :: City -> City -> [City] -> (Maybe Distance,Path)
        helper startPoint i [c] = (initialDist startPoint i c matrix, [i,c])
        helper startPoint i cs = (distance, i:pathh)
            where
                (distance,pathh) = minim (map (\(dist, path) -> (sumDist dist i (head path) matrix, path)) pathList)
                pathList = map (\c -> helper startPoint c (filter (/= c) cs)) cs


--------------------------------------------------------------------

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2), ("1","2",3),("3","0",3)]

--main :: IO ()
--main = printAdjMatrix $ createAdjMatrix 6 gTest2
