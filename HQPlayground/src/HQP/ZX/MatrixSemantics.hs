module HQP.ZX.MatrixSemantics where
import HQP.ZX.Syntax

import Numeric.LinearAlgebra ( (><), Matrix ) -- the hmatrix library

type CMat = Matrix ComplexT
type RMat = Matrix RealT
type StateT = CMat
type OpT    = CMat

evalOp :: ZXOp -> CMat
evalOp op = case op of
    Wire -> (2><2) [1,0,
                    0,1]
    Swap -> (4><4) [1,0,0,0,
                    0,0,1,0,
                    0,1,0,0,
                    0,0,0,1]
    RedSpider _ _ _ -> error "Matrix semantics for RedSpider not implemented yet."
    GreenSpider _ _ _ -> error "Matrix semantics for GreenSpider not implemented yet."
    Hadamard ->
        let s = 1/sqrt 2
        in  s * (2><2) [1, 1,
                        1,-1]
    Tensor a b -> error "Matrix semantics for Tensor not implemented yet."
    Compose a b -> error "Matrix semantics for Compose not implemented yet."
    Adjoint _ -> error "Matrix semantics for Adjoint not implemented yet."