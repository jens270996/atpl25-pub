module HQP.ZX.Convert where
import qualified HQP.QOp.Syntax as QOp
import HQP.ZX.Syntax

-- Convert from QOp to ZXOp
fromQOp :: QOp.QOp -> ZXOp
fromQOp op = case op of
    QOp.Z           -> GreenSpider 1 1 pi
    QOp.X           -> RedSpider 1 1 pi
    QOp.R QOp.Z a   -> GreenSpider 1 1 a
    QOp.R QOp.X a   -> RedSpider 1 1 a
    QOp.H           -> Hadamard
    QOp.C QOp.X     -> GreenSpider 1 2 pi <.> Wire >:
                        Wire <.> RedSpider 2 1 pi
    QOp.Tensor a b  -> Tensor (fromQOp a) (fromQOp b)
    QOp.Compose a b -> Compose (fromQOp a) (fromQOp b)
    QOp.Adjoint a   -> Adjoint (fromQOp a)
    _               -> error "Conversion from QOp to ZXOp not implemented for this constructor."
