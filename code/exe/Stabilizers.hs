module Main where
import HQP
import HQP.QOp.MatrixSemantics 
import Numeric.LinearAlgebra(rows,norm_2,find)
import Data.BitVector

{-| Stabilizers and Clifford gates:

    1. Write a function to check if a given QOp is in the Pauli group (start with 1-qubit operators, then write recursive n-qubit test).
    2. Write a function to check if a given QOp is a Clifford operator (start with 1Q operators, then write recursive n-qubit test).
    3. Write a function that takes a 1Q Clifford operator G and computes the effect of conjugation on the Paulis: GXG^\dagger, GZG^\dagger. Do you need GYG^\dagger as well? (Why/why not?)
    4. Generalize to n-qubit Clifford operators. Use BitVector (from Data.BitVector) to represent the result. 
    5. Use this to implement a stabilizer simulator that tracks the evolution of stabilizers under Clifford operations.

    
-}

{-| Divide out global phase from a matrix such that the first non-zero entry is real and positive.
    This allows us to compare matrices up to global phase. -}
factor_phase :: CMat -> CMat
factor_phase m = case first_nonzero m of
    Nothing -> m
    Just x  -> let
        phase = x / (magnitude x :+ 0)
      in
        scale (1 / phase) m

{-| Find the first non-zero entry in a matrix. We use tol to allow for floating point inaccuracy. -}
first_nonzero :: CMat -> Maybe ComplexT
first_nonzero m = 
    case find (\x -> magnitude x > tol) m of
        []        -> Nothing
        ((_,x):_) -> Just x
