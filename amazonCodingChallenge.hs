
type Pos = (Int, Int)

equal :: Pos -> Pos -> Bool
equal (x1, y1) (x2, y2) = x1 == x2 && y1 == y2

-- Assume we only consider 10*10 grid
findAllMove :: Pos ->  [Pos]
findAllMove (x,y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1), (x-1,y-1), (x+1,y-1), (x-1,y+1), (x+1,y+1)]

findPossibleMove :: Pos -> [Pos]
findPossibleMove pos = filter inBound (findAllMove pos)

inBound :: Pos -> Bool
inBound (x,y)
    | x < 0 || x > 9 = False
    | y < 0 || y > 9 = False
    | otherwise = True

findPath :: Pos -> Pos -> [Pos] -> [Pos] -> [Pos]
findPath start@(x1,y1) dest@(x2,y2) obs steps
    | equal start dest = steps
    | otherwise = []

