import Data.Array.IArray ((!))
import qualified Control.Monad      as C
import qualified Data.Array.Unboxed as A
import qualified Data.Array.ST      as A
import qualified Data.Maybe         as M

{- 10^5 -> 0.26sec -}
primes :: Int -> [Int]
primes upper = M.mapMaybe 
    (\i -> if eratosthenes!i then Just i else Nothing) 
    [2..upper] where
    eratosthenes :: A.UArray Int Bool
    eratosthenes = A.runSTUArray $ do
        ps <- A.newArray (2, upper) True
        C.forM_ [2..upper] $ \i -> do
            f <- A.readArray ps i
            C.forM_ [l*i|f, l <- [2..upper], l*i <= upper] $ \l ->
                A.writeArray ps l False
        return ps

main :: IO ()
main = print $ primes 10000
    
