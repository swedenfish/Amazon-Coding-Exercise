import Data.List
import Debug.Trace
type Pos = (Int, Int)

equal :: Pos -> Pos -> Bool
equal (x1, y1) (x2, y2) = x1 == x2 && y1 == y2

distance :: Pos -> Pos -> Int
distance (x1,y1) (x2,y2) = (x1-x2)^2 + (y1-y2)^2

-- Assume we only consider 10*10 grid
findAllMove :: Pos ->  [Pos]
findAllMove (x,y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1), (x-1,y-1), (x+1,y-1), (x-1,y+1), (x+1,y+1)]

findPossibleMove :: Pos -> [Pos] -> [Pos]
findPossibleMove pos obs = (filter inBound (findAllMove pos)) \\ obs

inBound :: Pos -> Bool
inBound (x,y)
    | x < 0 || x > 9 = False
    | y < 0 || y > 9 = False
    | otherwise = True

findPath :: Pos -> Pos -> [Pos] -> [Pos] -> [Pos]
findPath start@(x1,y1) dest@(x2,y2) obs steps
    | equal start dest = steps
    | otherwise = findPath nextPos dest obs (start:steps)
        where
            nextPos = pick (findPossibleMove start obs) dest


-- choose which step to go
-- this can be replaced with other different strategies
-- currently it's using the eager way, which not consider future benefits 
pick :: [Pos] -> Pos -> Pos
pick [] dest = error "No possible way"
pick options dest = pick' options dest (0,0) 162
    where
        pick' [] _ bestOption _ = bestOption
        pick' (p:ps) dest bestOption minDistance
            | distance p dest < minDistance = pick' ps dest p (distance p dest)
            | otherwise = pick' ps dest bestOption minDistance


-- this is used as an API to simplify the operation
-- we set defaultly: 
-- start = (0,0) 
-- dest = (9,9)
-- so the only argument is the obs
go :: [Pos] -> ([Pos],Int)
go obs = (result, length result) 
    where
        result = findPath (0,0) (9,9) obs []




{- 

data SearchProb a = 
    SearchProb { start :: a
               , expand :: a -> [a]
               , isDone :: a -> Bool }

findPath :: SearchProb Pos
findPath = SearchProb start expand isDone where
    start = (0,0)
    expand = findPossibleMove'
        -- \(x,y) -> trace (show (x,y)) $ [(x+1,y) | x < 10] ++ [(x-1,y) | x >= 0] ++ [(x,y-1) | y >= 0] ++ [(x,y+1) | y < 10] ++ [(x-1,y-1) | x >= 0 && y >= 0] ++ [(x-1,y+1) | x >= 0 && y < 10] ++ [(x+1,y-1) | x < 10 && y >= 0] ++ [(x+1,y+1) | x < 10 && y < 10]
    isDone = (== (9,9)) 

type SearchAlgo a = SearchProb a -> Maybe a

bfs :: SearchAlgo a
bfs (SearchProb start expand isDone) = loop [start] where
    loop xs | any isDone xs = find isDone xs
            | otherwise     = loop (concatMap expand xs)

findPossibleMove' :: Pos -> [Pos]
findPossibleMove' pos = trace (show pos) $ ((filter inBound (findAllMove' pos)) \\ obs)
    where
        obs = []

-- Assume the robot doesn't go back
findAllMove' :: Pos ->  [Pos]
findAllMove' (x,y) = [(x+1, y), (x, y+1), (x+1,y+1)]


-}  