module HQP.ZX.Tests.Basic where
import HQP.QOp.Syntax
import HQP.ZX.Convert
import HQP.ZX.Simplify
import qualified HQP.ZX.Syntax as ZX

-- CNOT(1->2) CNOT(2->1) CNOT(1->2) will simplify to a SWAP gate between the first and last qubit.
--   We represent without permute by identity CNOT(2->1) = H ⊗ H ; CNOT(1->2) ; H ⊗ H
-- This will fail now as we dont have the simplfication implemented.
tripleCNOT :: ZX.ZXOp
tripleCNOT = let actual = simplify $ fromQOp tripleCNOTQOp
                 expected = ZX.Swap
             in if actual == expected then actual else error "Triple CNOT test failed"
    where
        tripleCNOTQOp :: QOp
        tripleCNOTQOp =
            C X >:
            H <.> H >:
            C X >:
            H <.> H >:
            C X
