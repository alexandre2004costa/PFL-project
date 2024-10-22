import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int
type Edge = (City,City,Distance)
type RoadMap = [Edge]

type AdjList = [(City,[(City,Distance)])]
type AdjMatrix = Data.Array.Array (Int,Int) (Maybe Distance)

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

--pathDistance :: RoadMap -> Path -> Maybe Distance --uncompleted
--pathDistance [x] (city1:city2:ps) = Nothing
--pathDistance ((c1,c2,d):xs) [city1:city2] = if (c1,c2) == (city1,city2) then d else Nothing
--pathDistance ((c1,c2,d):xs) (city1:city2:ps)
--    |(c1,c2) == (city1,city2) = d + pathDistance xs (city2:ps)
--    |otherwise = Nothing


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


shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined


createAdjList :: RoadMap -> AdjList -- O(E*V)
createAdjList = foldr addRoad []
  where
    addRoad (c1, c2, d) adjList = addNeighbor c1 c2 d (addNeighbor c2 c1 d adjList)
    addNeighbor city neighbor distance [] = [(city, [(neighbor, distance)])]
    addNeighbor city neighbor distance ((c, neighbors):rest)
      | city == c = (c, (neighbor, distance) : neighbors) : rest
      | otherwise = (c, neighbors) : addNeighbor city neighbor distance rest

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



{-
createEmptyMatrix :: Int -> AdjMatrix
createEmptyMatrix size = Data.Array.array ((0, 0), (size - 1, size - 1)) [((i, j), Nothing) | i <- [0..size-1], j <- [0..size-1]]

addEdgeMatrix :: AdjMatrix -> (Int, Int) -> Distance -> AdjMatrix
addEdgeMatrix matrix (i, j) dist = matrix Data.Array.// [((i, j), Just dist)]
-}




sortRoadmap :: RoadMap -> RoadMap
sortRoadmap = Data.List.sortBy compareDist
  where
    compareDist (_, _, d1) (_, _, d2) = compare d1 d2


addEdgeList :: AdjList -> City -> (City,Distance) -> AdjList
addEdgeList [] city edge = [(city, [edge])]
addEdgeList ((c, edges):rest) city edge
    | c == city = (c, edge:edges):rest
    | otherwise = (c, edges): addEdgeList rest city edge


getConnection :: AdjList -> City -> Int
getConnection _ city = 0
getConnection ((c, edges):rest) city
    | c == city = length edges
    | otherwise = getConnection rest city



hasCycle :: AdjList -> City -> [City] -> Bool
hasCycle adjList city visited
    | (visited /= []) && (city `elem` visited) && (city /= last visited) = True
    | otherwise = or [hasCycle adjList c (city:visited) | c <- adjCities]
        where
            adjCities = case lookup city adjList of
                Just tuples -> [c | (c, _) <- tuples]
                Nothing -> []



travelSales :: RoadMap -> Path
travelSales rm = searchPath sortedRM [] [] 0 
    where
        sortedRM = sortRoadmap rm



-- COUNT? ROADMAP TEM DE TER TODAS AS ARESTAS PARA A FRENTE E PARA TRÁS
searchPath :: RoadMap -> AdjList -> [City] -> Int -> Path
--searchPath [] _ visited _ = visited ++ [head visited]
searchPath rm@(edge@(c1,c2,d):restRM) list visited count
    | count == numCities = visited ++ [head visited] -- Se todas as cidades foram visitadas, retorna o caminho
    | null rm = []   -- Se passou por todas as arestas e não tem todos os vértices, não há caminho
    | getConnection new_list c1 >= 2 || getConnection new_list c2 >= 2 = searchPath restRM list visited count
    | getConnection list c1 == 0 || getConnection list c2 == 0 || not (hasCycle list c1 []) = searchPath restRM new_list (c1:(c2:visited)) (count+1)
    | otherwise = [] --ACHO QUE NÃO DEVIA CHEGAR AQUI 
        where
            numCities = length (cities rm)
            new_list = addEdgeList list c1 (c2,d)



{-
addConnection :: [(City,Int)] -> City -> [(City,Int)]
-- addConnection [] _ = [] não é suposto entrar aqui
addConnection ((c,n):rest) city
    | c == city = (c,n+1):rest
    | otherwise = (c,n): addConnection rest city

getConnection :: [(City,Int)] -> City -> Int
getConnection ((c,n):rest) city
    | c == city = n
    | otherwise = getConnection rest city
-}

{-
travelSales :: RoadMap -> Path
travelSales rm = searchPath sortedRM [] [] 
    where
        sortedRM = sortRoadmap rm
        --numCities = length (cities rm)
        --list = [] --matrix = createEmptyMatrix numCities
        --connections = [(city,0) | city <- cities rm]

searchPath :: RoadMap -> AdjList -> [City] -> [(City,Int)] -> Path
--searchPath [] _ visited _ = visited ++ [head visited]
searchPath rm@(edge@(c1,c2,d):restRM) list visited connects
    | length visited == numCities = visited ++ [head visited] -- Se todas as cidades foram visitadas, retorna o caminho
    | null rm = []   -- Se passou por todas as arestas e não tem todos os vértices, não há caminho
    | getConnection (addConnection (addConnection connects c1) c2) c1 >= 2 || getConnection (addConnection (addConnection connects c1) c2) c2 >= 2 = searchPath restRM list visited connects
    | getConnection connects c1 == 0 || getConnection connects c2 == 0 || not (hasCycle list c1 []) = searchPath restRM (addEdgeList list c1 (c2,d)) (c1:(c2:visited)) (addConnection (addConnection connects c1) c2)
    | otherwise = [] --ACHO QUE NÃO DEVIA CHEGAR AQUI     searchPath restRM list visited connections
        --if nextCities == [] then [] else []
        where
            numCities = length (cities rm)

            -- searchPath restRM list (c1:(c2:visited)) connects
            --nextCities = getNextCities current visited rm -- Função que obtém as cidades não visitadas
-}





tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]