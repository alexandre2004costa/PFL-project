import Debug.Trace (trace)
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

compareDistances :: (City, Distance) -> (City, Distance) -> Ordering
compareDistances (_, d1) (_, d2)
    | d1 < d2 = LT  
    | d1 > d2 = GT  
    | otherwise = EQ 

addToUnvisited :: City -> (City, Distance) -> [(City, City, Distance)] -> [(City, City, Distance)]
addToUnvisited c (c1, d) [] = [(c, c1, d)]
addToUnvisited c (c1, d) ((c2, c3, dist) : xs)
    | dist > d = (c, c1, d) : (c2, c3, dist) : xs  -- Adiciona a nova cidade e mantém as restantes
    | otherwise = (c2, c3, dist) : addToUnvisited c (c1, d) xs  -- Continua a acumular

getPath ::  Data.Array.Array Int (City, Distance) -> City -> City -> [City]
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
    | otherwise = trace ("min ele: " ++ show minElement) (minElement, newHeap)
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
                else heap -- No need to swap; heap is in correct order
            
    | otherwise = heap  -- No children, so we're done

instance Show MinHeap where
    show (MinHeap arr size capacity) =
        "MinHeap: [Size: " ++ show size ++ ", Capacity: " ++ show capacity ++ ", Elements: " ++ show (elems arr) ++ "]"
        where
            elems a = [a Data.Array.! i | i <- [0 .. size - 1]]  -- Extraímos apenas os elementos válidos


shortestPath :: RoadMap -> City -> City -> Path
shortestPath rm start end = reverse $ getPath (dijkstra adjList distances heap [start]) end start

  where
    adjList = createAdjList rm
    distances = createAllDistancesArray rm start
    heap = insert (createHeap 100) (start, start, 0)  -- Initial capacity for the heap

    dijkstra :: AdjList -> Data.Array.Array Int (City, Distance) -> MinHeap -> [City] -> Data.Array.Array Int (City, Distance)
    dijkstra adjList distances heap visited
        | size heap == 0 = trace ("Heap is empty") distances
        | otherwise =
        let

            -- Extract the closest city from the heap
            ((startCity, closestCity, d), newHeap) = trace ("Current Heap: " ++ show heap) extractMin heap
            adjacentCities = filter (\(c, _) -> c `notElem` visited) (getAdjacentCities adjList closestCity)
            
            -- Insert adjacent cities into the heap
            updatedHeap = trace (" Visited: " ++ show (closestCity : visited)) foldr (\(city, dist) h -> insert h (closestCity, city, dist)) newHeap adjacentCities
            
            newDistances =  distances
            (oldPoint, oldDist) = distances Data.Array.! (read closestCity)
            (startPoint, startDist) = distances Data.Array.! (read startCity)
            newDist = startDist + d
            
            updatedDistance = if oldDist > newDist
                                then newDistances Data.Array.// [(read closestCity, (startCity, newDist))]
                                else newDistances
        in
            trace ("!!!!C,C,D: " ++ show startCity ++ show closestCity ++ show d ++ "---" ++ show updatedDistance)dijkstra adjList updatedDistance updatedHeap (closestCity : visited)




createEmptyMatrix :: Int -> AdjMatrix
createEmptyMatrix size = Data.Array.array ((0, 0), (size - 1, size - 1)) [((c1, c2), Nothing) | c1 <- [0..size-1], c2 <- [0..size-1]]

addEdgeMatrix :: AdjMatrix -> (City, City, Distance) -> AdjMatrix
addEdgeMatrix matrix (c1, c2, dist) = matrix Data.Array.// [((read c1, read c2), Just dist)] Data.Array.// [((read c2, read c1), Just dist)]

createAdjMatrix :: RoadMap -> AdjMatrix
createAdjMatrix rm = foldl addEdgeMatrix (createEmptyMatrix nCities) rm
    where nCities = length (cities rm)


minim :: City ->  [(Maybe Distance,Path)] -> (Maybe Distance,Path) -> (Maybe Distance,Path)
minim i [] (d1, p1) = (d1, p1)
minim i ((d,p):dp) (d1, p1)  
    | d < d1 = minim i dp (d,p)
    | otherwise = minim i dp (d1,p1)

outra :: City -> City -> City -> AdjMatrix -> Maybe Distance
outra startPoint i c matrix = do
    d <- matrix Data.Array.! (read startPoint, read c) -- Pegar o valor de Maybe Distance
    dp <- matrix Data.Array.! (read c, read i) -- Pegar o valor de Maybe Distance
    return (d + dp) -- Somar os valores e retornar


travelSales :: RoadMap -> (Maybe Distance,[City])
travelSales rm = helper city city (tail citiiies)
    where
        citiiies = cities rm
        city = head citiiies
        matrix = createAdjMatrix rm
        helper :: City -> City -> [City] -> (Maybe Distance,Path)
        helper startPoint i [c] = (outra startPoint i c matrix, [c,i])
        helper startPoint i xs = (distance, i:pathh)
            where
                (distance,pathh) = minim startPoint (map (\c -> helper startPoint c (filter (/= c) xs)) xs) (Just 9999,[])


    

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