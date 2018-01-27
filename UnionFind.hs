module UnionFind where

import Array

import qualified Data.Array.IO as A
import qualified Control.Monad as C

newtype UnionFind = UnionFind (A.IOArray Int Int)

newUnionFindTree :: Int -> IO UnionFind
newUnionFindTree n = do
    tree <- A.newArray (0,n-1) 0 :: IO (A.IOArray Int Int)
    C.forM_ [0..n-1] $ \i -> (tree, i) <<- i
    return $ UnionFind tree

connectRoot :: UnionFind -> Int -> Int -> IO ()
connectRoot (UnionFind tree) a b = do
    a' <- traverseRoot (UnionFind tree) a
    b' <- traverseRoot (UnionFind tree) b
    (tree, b') <<- a'
 
traverseRoot :: UnionFind -> Int -> IO Int
traverseRoot (UnionFind tree) i = do
    i' <- tree ! i
    if i == i' then return i
                else do
        r <- traverseRoot (UnionFind tree) i'
        -- rootが分かるのでついでに経路圧縮を行う
        (tree, i) <<- r
        return r

