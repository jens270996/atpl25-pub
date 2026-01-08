module HQP.ZX.Syntax where

import Algebra.Graph.Undirected
import Data.Fixed (mod')

data Phase = Frac Rational | PiHalves Int

tolerance :: Fractional a => a
tolerance = 1e-3

instance Eq Phase where
    (Frac a) == (Frac b) = abs (a - b) `mod'` (2*rpi) < tolerance
    (PiHalves a) == (Frac b) = abs (fromIntegral a * rpi / 2 - b) `mod'` (2*rpi) < tolerance
    (Frac a) == (PiHalves b) = abs (a - fromIntegral b * rpi / 2) `mod'` (2*rpi) < tolerance
    (PiHalves a) == (PiHalves b) = (a `mod` 4) == (b `mod` 4)

instance Num Phase where
    (+) (Frac a) (Frac b) = Frac $ (a+b) `mod'` (2*rpi)
    (+) (PiHalves a) (Frac b) = Frac $ (fromIntegral a * rpi / 2 + b) `mod'` (2*rpi)
    (+) (Frac a) (PiHalves b) = Frac $ (a + fromIntegral b * rpi / 2) `mod'` (2*rpi)
    (+) (PiHalves a) (PiHalves b) = PiHalves $ (a+b) `mod` 4
    (*) (Frac a) (Frac b) = Frac $ (a*b) `mod'` (2*rpi)
    (*) (PiHalves a) (Frac b) = Frac $ (fromIntegral a * rpi / 2 * b) `mod'` (2*rpi)
    (*) (Frac a) (PiHalves b) = Frac $ (a * fromIntegral b * rpi / 2) `mod'` (2*rpi)
    (*) (PiHalves a) (PiHalves b) = PiHalves $ (a*b) `mod` 4
    abs (Frac a) = Frac $ abs a
    abs (PiHalves a) = PiHalves $ abs a
    signum (Frac a) = Frac $ signum a
    signum (PiHalves a) = PiHalves $ signum a
    fromInteger 0 = PiHalves 0
    fromInteger a = Frac $ fromIntegral a `mod'` (2*toRational rpi)
    negate (Frac a) = Frac $ negate a
    negate (PiHalves a) = PiHalves $ negate a

instance Show Phase where
    show (PiHalves 2) = "π"
    show (PiHalves a) = if a `mod` 2 == 0 then show (a `div` 2)++" π" else show a ++ "/2 π"
    show (Frac a) = show a
data ZXElement
    = H
    | Green Phase
    | Red Phase
    | Wire
    | Input
    | Output
    deriving (Eq,Show)
    
type Lane = Int
type Depth = Int
type Id = (Lane,Depth)
data ZXNode = Node Id ZXElement
    deriving (Eq,Show)

instance Ord ZXNode where
    (<=) (Node a _) (Node b _) = a <= b

type ZXDiagram = Graph ZXNode

rpi :: Rational
rpi = toRational pi