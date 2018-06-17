import qualified Data.Array as A

type Table = A.Array Int
type Graph = Table [Edge]
data Edge  = Edge 
    { toEdge   :: Int
    , costEdge :: Int
    } deriving (Show, Eq, Ord)

inf :: Int
inf = 10000000000

