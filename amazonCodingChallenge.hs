
type Pos = (Int, Int)

equal :: Pos -> Pos -> Bool
equal (x1, y1) (x2, y2) = x1 == x2 && y1 == y2

findPath :: Pos -> Pos -> [Pos] -> [Pos] -> [Pos]
findPath start@(x1,y1) dest@(x2,y2) obs steps
    | equal start dest _ = steps
    | otherwise = []
