module HQP.HZX.Syntax where
import Data.Complex
import Algebra.Graph


type RealT = Double  -- Can be replaced by e.g. exact fractions or constructive reals
type ComplexT = Complex RealT

data ZXElement = H | Green RealT | Red RealT
    deriving (Eq,Show)
data ZXNode = Node Int ZXElement
    deriving (Eq,Show)

instance Ord ZXNode where
    (<=) (Node a _) (Node b _) = a <= b

type ZXDiagram = Graph ZXNode

mergeReds :: ZXDiagram -> ZXDiagram
mergeReds empty = empty
mergeReds g =
    let neighbors = adjacencyList g
    in g



-- Ideas:
-- Nums as vertices with lookup table on the side.
-- Inherit Ord etc. and make a graph consistiong of pairs