module HQP.QOp.HelperFunctions where
import HQP.QOp.Syntax
import Data.Bits(FiniteBits,countTrailingZeros,shiftL,shiftR)
import Data.List (sort)
import Data.Set  (union, fromList, toList)


-- | Signature of an operator a: C^{2^m} -> C^{2^n} is (m,n) = (op_domain a, op_range a)
op_qubits :: QOp -> Nat
op_qubits op = case op of
    Id n    -> n
    Phase _ -> 0
    R a _   -> op_qubits a
    C a     -> 1 + op_qubits a
    Tensor    a b -> op_qubits a + op_qubits b
    DirectSum a b -> let  
      (w_a, w_b) = (op_qubits a, op_qubits b)
      in 
        if w_a == w_b then 1+w_a 
        else 
          error $ "Direct sum of incompatible operator dimensions: " 
          ++ show w_a ++ " qubits /= " ++ show w_b ++ " qubits."

    Compose   a b -> max (op_qubits a) (op_qubits b)
    Adjoint   a   -> op_qubits a
    Permute   ks  -> length ks 
    _             -> 1 -- 1-qubit gates

op_dimension :: QOp -> Nat
op_dimension op = 1 `shiftL` (op_qubits op)

step_qubits :: Step -> Nat
step_qubits step = case step of 
  Unitary op -> op_qubits op
  Measure ks      -> 1 + foldr max 0 ks
  Initialize ks _ -> 1 + foldr max 0 ks

           
prog_qubits :: Program -> Nat
prog_qubits program = maximum $ map step_qubits program

-- | Support of an operator: the list of qubits it acts non-trivially on.
op_support :: QOp -> [Nat]
op_support op = let
    shift ns k = map (+k) ns
    union xs ys = toList $ Data.Set.union (fromList xs) (fromList ys)
  in case op of
  Id n       -> []
  Phase _    -> []
  R _ 0      -> []
  R a _         -> op_support a
  C a           -> [0] ++ (op_support a) `shift` 1
  Tensor a b    -> (op_support a) ++ (op_support b) `shift` (op_qubits a)
  DirectSum a b -> [0] ++ (union (op_support a) (op_support b) `shift` 1)
  Compose (Permute ks) b -> permApply ks (op_support b)
  Compose a (Permute ks) -> invertPerm ks `permApply` (op_support a)
  Compose a b   -> union (op_support a) (op_support b)
  Adjoint a     -> op_support a
  Permute ks    -> permSupport ks
  _             -> [0] -- 1-qubit gates


-- Small helper functions
toBits :: (Integral a) => a -> [a]
toBits 0 = []
toBits k = (toBits (k `div` 2)) ++ [(k `mod` 2)]

toBits' :: Nat -> Nat -> [Nat]
toBits' n k = let 
    bits = toBits k
    m    = length bits
  in
    (replicate (n-m) 0) ++ bits

-- | Integer logarithm base 2 - change to floor{log_2 n} or ceil{log_2 n}.
ilog2 :: (FiniteBits a, Integral a) => a -> Nat
ilog2 = countTrailingZeros

evenOdd :: [a] -> ([a],[a])
evenOdd [] = ([],[])
evenOdd [x] = ([x],[])
evenOdd (x:y:xs) = let (es,os) = evenOdd xs in (x:es,y:os)

permApply :: [Int] -> [a] -> [a]
permApply ks xs = [ xs !! k | k <- ks ]

permSupport :: [Int] -> [Int]
permSupport ks = [ i | (i,j) <- zip [0..] ks, i /= j ]

invertPerm :: [Int] -> [Int] -- TODO: invertPerm -> permInvert for consistency
invertPerm ks = map snd $  -- For each index in the output, find its position in the input
    sort [ (k, i) | (i, k) <- zip [0..] ks ]
