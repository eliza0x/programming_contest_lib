import Data.Array.Unboxed ((!))
import qualified Data.Array.Unboxed as A
import qualified Data.Array.ST      as S
import qualified Control.Monad      as C

-- サンプル内でのみ利用
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.List

type Table = A.Array Int
type Graph = Table [Edge]
data Edge  = Edge 
    { toEdge   :: Int
    , costEdge :: Int
    } deriving (Show, Eq, Ord)

inf :: Int
inf = 10000000000

bellmanFord :: Int              -- 絶対地点
            -> Graph 
            -> A.UArray Int Int -- 絶対地点からの最短距離
bellmanFord s graph = S.runSTUArray $ do
    let (bs,be) = A.bounds graph
    shortestPath <- S.newArray (bs,be) inf
    S.writeArray shortestPath s 0

    -- 全点を経由する最短経路が存在するとしても、その辺を構成する辺の数はbe-bs-1
    C.forM_ [bs..be-1] $ \_ ->
        C.forM_ [(f, toEdge e, costEdge e)|f<-[bs..be], e<-graph!f] $ \(from, to, edgeCost) -> do
            fromCost <- S.readArray shortestPath from
            toCost   <- S.readArray shortestPath to

            -- 直前まで意味のある値が求まっている && より良い辺を見つけた
            C.when (fromCost /= inf && toCost > fromCost+edgeCost)
                $ S.writeArray shortestPath to $ fromCost + edgeCost

    return shortestPath            

groupFstElems :: Eq a => [(a, b)] -> [(a, [b])]
groupFstElems = flip groupFstElems' []
    where
    groupFstElems' :: Eq a => [(a, b)] -> [(a, [b])] -> [(a, [b])]
    groupFstElems' [] ys         = ys
    groupFstElems' (x:xs) []     = groupFstElems' xs [(fst x, [snd x])]
    groupFstElems' (x:xs) (y:ys) = if fst x == fst y 
        then groupFstElems' xs ((fst y, snd x:snd y):ys)
        else groupFstElems' xs ((fst x, [snd x]):y:ys)

getLineNums :: IO [Int]
getLineNums = map (fst . fromJust . B.readInt) . B.words <$> B.getLine

main :: IO ()
main = do
    [v,e,r] <- getLineNums :: IO [Int]

    edges <- groupFstElems . sort <$> C.replicateM e (
        (\[s,t,d]->(s,Edge t d)) <$> getLineNums :: IO (Int, Edge)) :: IO [(Int, [Edge])]

    let nullEdges = [(i, [])|i<-[0..v-1]]
        graph = A.array (0,v-1) (nullEdges ++ edges) :: Graph

    let shortestPath = bellmanFord r graph :: A.UArray Int Int

    C.forM_ [0..v-1] $ \i ->
        putStrLn $ if shortestPath ! i == inf then "INF" else show $ shortestPath ! i

