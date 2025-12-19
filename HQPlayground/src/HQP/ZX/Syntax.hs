module HQP.ZX.Syntax where
import Data.Complex
import Algebra.Graph.Undirected
import Data.Fixed (mod')


type RealT = Double  -- Can be replaced by e.g. exact fractions or constructive reals
type ComplexT = Complex RealT

data Phase = Real RealT | PiHalves Int
    deriving (Show)

instance Eq Phase where
    (PiHalves a) == (PiHalves b) = (a `mod` 4) == (b `mod` 4)
    (Real a) == (Real b) = a == b
    (PiHalves a) == (Real b) = (fromIntegral a) * pi / 2 == b
    (Real a) == (PiHalves b) = a == (fromIntegral b) * pi / 2

instance Num Phase where
    (+) (Real a) (Real b) = Real $ a+b -- Optionally add mod 2pi to all real cases?? May loose even more precision?
    (+) (PiHalves a) (Real b) = Real $ (fromIntegral a)*pi/2+b `mod'` (2*pi)
    (+) (Real a) (PiHalves b) = Real $ (a+(fromIntegral b)*pi/2) `mod'` (2*pi)
    (+) (PiHalves a) (PiHalves b) = PiHalves $ a+b `mod` 4
    (*) (Real a) (Real b) = Real $ a*b 
    (*) (PiHalves a) (Real b) = Real $ (fromIntegral a)*pi/2*b `mod'` (2*pi)
    (*) (Real a) (PiHalves b) = Real $ a*(fromIntegral b)*pi/2 `mod'` (2*pi)
    (*) (PiHalves a) (PiHalves b) = PiHalves $ a*b `mod` 4
    abs (Real a) = Real $ abs a
    abs (PiHalves a) = PiHalves $ abs a
    signum (Real a) = Real $ signum a
    signum (PiHalves a) = PiHalves $ signum a
    fromInteger a = Real $ fromInteger a `mod'` (2*pi)
    negate (Real a) = Real $ negate a
    negate (PiHalves a) = PiHalves $ negate a

data ZXElement
    = H
    | Green Phase
    | Red Phase
    | Input
    | Output
    deriving (Eq,Show)
data ZXNode = Node Int ZXElement
    deriving (Eq,Show)

instance Ord ZXNode where
    (<=) (Node a _) (Node b _) = a <= b

type ZXDiagram = Graph ZXNode

