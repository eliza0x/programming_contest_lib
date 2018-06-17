import Data.Array.Unboxed ((!))
import qualified Data.Array.Unboxed as A
import qualified Data.Array.ST      as S
import qualified Data.STRef.Strict  as S
import qualified Control.Monad.ST   as S
import qualified Control.Monad      as C
import qualified Data.Heap          as H

-- サンプル内でのみ利用
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.List
import Control.Applicative

type Table = A.Array Int
type Graph = Table [Edge]
data Edge  = Edge 
    { toEdge   :: Int
    , costEdge :: Int
    } deriving (Show, Eq, Ord)

inf :: Int
inf = 10000000000

dijkstra :: Int              -- 絶対地点
         -> Graph 
         -> A.UArray Int Int -- 絶対地点からの最短距離
dijkstra s graph = S.runSTUArray $ do
    let (bs,be) = A.bounds graph
    shortestPath <- S.newArray (bs,be) inf
    S.writeArray shortestPath s 0
    let que = H.singleton (0,s) :: H.MinHeap (Int, Int)
    update shortestPath =<< S.newSTRef que
    return shortestPath            
    where
    update :: S.STUArray s Int Int -> S.STRef s (H.MinHeap (Int, Int)) -> S.ST s ()
    update shortestPath que = do
        -- queが空になれば作業終了
        isNull <- H.null <$> S.readSTRef que
        C.unless isNull $ do
            ((knownCost, from), que') <- fromJust . H.view <$> S.readSTRef que
            S.writeSTRef que que'
            fromCost <- S.readArray shortestPath from

            -- 変化が起こっていれば更新
            C.unless (fromCost < knownCost) $
                C.forM_ [(toEdge e, costEdge e)|e <- graph!from] $ \(to, edgeCost) -> do
                    toCost <- S.readArray shortestPath to

                    -- より良い辺を見つければ更新
                    C.when (toCost > fromCost+edgeCost) $ do
                        S.writeArray shortestPath to $ fromCost+edgeCost
                        S.modifySTRef que (H.insert (fromCost+edgeCost, to)) 
            update shortestPath que

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

    let shortestPath = dijkstra r graph :: A.UArray Int Int

    C.forM_ [0..v-1] $ \i ->
        putStrLn $ if shortestPath ! i == inf then "INF" else show $ shortestPath ! i

