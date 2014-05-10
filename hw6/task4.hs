import Data.Maybe
import Data.List

data Graph v e = Graph [(Int,v)] [(Int,Int,e)]

findVertex (a1,b1) (a2,b2) = if b1 < b2 then (a1,b1) else (a2,b2)
findEdge x y list = find (\(a,b,_) -> x == a && y == b || x == b && y == a) list
number (a,b) = (a, Just b)
value (_,_,c) = c

dijkstra :: (Eq a, Ord a, Fractional a) => Graph v a -> Int -> [(Int, Maybe a)]                                    
dijkstra (Graph vs es) root = map number $ helper (map (\(a,_) -> (a, if a == root then 0 else 1/0)) vs) []
    where helper [] res = res
          helper notVisited visited = helper (map count $ filter (/= minNotVisited) notVisited) (minNotVisited:visited)
                where minNotVisited = foldl findVertex (head notVisited) (tail notVisited)
                      count (number, distance) = count' $ findEdge number (fst minNotVisited) es
                                where count' Nothing = (number, distance)
                                      count' (Just edge) | snd minNotVisited + value edge < distance = (number, snd minNotVisited + value edge)
                                                            | otherwise = (number, distance)



main = do
    let g = Graph [(1,5),(2,4),(3,5),(4,6),(5,1),(6,2)] [(1,3,9),(1,2,7),(1,6,14),(2,3,10),(2,4,15),(6,3,2),(6,5,9),(3,4,11),(5,4,6)]
    putStrLn(show $ dijkstra g 1)