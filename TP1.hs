import qualified Data.List
--import qualified Data.Array
--import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int
type Edge = (City,City,Distance)
type RoadMap = [Edge]

cities :: RoadMap -> [City]
cities [] = []
cities ((c1,c2,d):xs) = Data.List.nub $ c1 : c2 : cities xs
    

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
    |c1 == city = (c2,d) : adjacent xs city
    |otherwise = adjacent xs city

pathDistance :: RoadMap -> Path -> Maybe Distance --uncompleted
pathDistance [x] (city1:city2:ps) = Nothing
pathDistance ((c1,c2,d):xs) [city1:city2] = if (c1,c2) == (city1,city2) then d else Nothing
pathDistance ((c1,c2,d):xs) (city1:city2:ps)
    |(c1,c2) == (city1,city2) = d + pathDistance xs (city2:ps)
    |otherwise = Nothing

rome :: RoadMap -> [City]
rome = undefined

isStronglyConnected :: RoadMap -> Bool 
isStronglyConnected = undefined

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

travelSales :: RoadMap -> Path
travelSales = undefined

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]