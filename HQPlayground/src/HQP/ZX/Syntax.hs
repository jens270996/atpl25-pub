module HQP.ZX.Syntax where
import Data.Complex
import Data.Ratio
import Algebra.Graph.Undirected


type RealT = Double  -- Can be replaced by e.g. exact fractions or constructive reals
type ComplexT = Complex RealT

data Phase = Real RealT | PiFrac Rational


instance Num Phase where
    (+) (Real a) (Real b) = Real $ a+b `mod` 2*pi
    (+) (PiFrac a) (Real b) = Real $ (fromRational a)*pi+b 
    (+) (Real a) (PiFrac b) = Real $ a+(fromRational b)*pi
    (+) (PiFrac a) (PiFrac b) = PiFrac $ a+b `mod` 2
    (*) (Real a) (Real b) = Real $ a*b `mod` 2*pi
    (*) (PiFrac a) (Real b) = Real $ (fromRational a)*pi*b
    (*) (Real a) (PiFrac b) = Real $ a*(fromRational b)*pi
    (*) (PiFrac a) (PiFrac b) = PiFrac $ a*b `mod` 2
    abs (Real a) = Real $ abs a
    abs (PiFrac a) = PiFrac $ abs a
    signum (Real a) = Real $ signum a
    signum (PiFrac a) = PiFrac $ signum a
    fromInteger a = (Real $ fromInteger a)
    negate (Real a) = Real $ negate a
    negate (PiFrac a) = PiFrac $ negate a

data ZXElement
    = H
    | Green RealT
    | Red RealT
    | Input
    | Output
    | I
    deriving (Eq,Show)
data ZXNode = Node Int ZXElement
    deriving (Eq,Show)

instance Ord ZXNode where
    (<=) (Node a _) (Node b _) = a <= b

type ZXDiagram = Graph ZXNode

