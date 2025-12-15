module HQP.ZX.Syntax where
import Data.Complex

type RealT = Double  -- Can be replaced by e.g. exact fractions or constructive reals
type ComplexT = Complex RealT

{-|
    The ZXOp type is a symbolic operator in the ZX Calculus.
 -}  
data ZXOp
  = Wire -- Single wire (identity)
  | Swap -- Swaps two wires
  | RedSpider Int Int RealT -- Arity in, arity out, phase
  | GreenSpider Int Int RealT -- Arity in, arity out, phase
  | Hadamard
  | Tensor ZXOp ZXOp
  | Compose ZXOp ZXOp
  | Adjoint ZXOp
  deriving (Show, Eq)

{- TODO: Figure out ZX measurements -}
data Step
  = Unitary ZXOp    -- A unitary quantum program
  | Measure [Int]   -- Measurement of qubits ks (stochastic non-reversible process)
  deriving (Show, Eq)

type Program = [Step]

{-|
The operators form a Semigroup under composition, so we inherit Haskell's standard composition operator <>. Note that a<>b means "first apply b, then a".
-} 
class Semigroup o => Operator o where
  compose    :: o -> o -> o   -- Sequential composition 
  tensorProd :: o -> o -> o   -- Tensor product ⊗
  adj        :: o -> o        -- Adjoint: Inverse for unitary operators, bra <-> ket for states

  compose = (<>) -- Default semigroup compose operator. Operators form a SG under composition.

  -- Syntactic sugar in ASCII
  (>:), (<.>) :: o -> o -> o
  (>:) a b = compose b a -- left-to-right composition
  (<.>) = tensorProd

instance Semigroup ZXOp where
  (<>) = Compose

instance Operator ZXOp where
  tensorProd = Tensor
  adj        = Adjoint

infixr 8 <.>
infixr 6 >:
