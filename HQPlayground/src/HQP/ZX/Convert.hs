module HQP.ZX.Convert where
import qualified HQP.QOp.Syntax as QOp
import HQP.ZX.Syntax
import HQP.ZX.IdGenerator
import Algebra.Graph.Undirected
import HQP.ZX.Utils
-- Convert from QOp to ZXOp
-- TODO add numbers
convert :: QOp.QOp -> ZXDiagram
convert op =
    let (diagram,_) = IdGenerator (fromQOp op) 0
    in diagram



fromQOp :: QOp.QOp -> IdGenerator ZXDiagram
fromQOp op = case op of
    QOp.Z           -> wrap (Green (PiHalves 2))
    QOp.X           -> wrap (Red (PiHalves 2))
    QOp.R QOp.Z a   -> wrap (Green (Real a))
    QOp.R QOp.X a   -> wrap (Red (Real a))
    QOp.S 
    QOp.H           -> wrap H
    QOp.C QOp.X     -> do c1 <- generateVertexId
                          switchLane
                          c2 <- generateVertexId
                          return $ Connect (Node c3 (Green (PiHalves 0))) (Node c4 (Red (PiHalves 0)))
    QOp.Tensor a b -> do d <- getDepth
                         g <- fromQOp a
                         setDepth d
                         switchLane
                         g' <- fromQOp b
                         setDepth d -- ??
                         return $  overlay g g'
    QOp.Compose a b -> do l <- getLane
                          g <- fromQOp a
                          setLane l
                          proceed
                          g' <- fromQOp b
                          setLane l -- ??
                          return $ compose g g'
    QOp.Adjoint _ -> error "Remove adjoints before converting to ZX."
    _               -> error "Conversion from QOp to ZXOp not implemented for this constructor."



wrap :: ZXElement -> IdGenerator ZXDiagram
wrap e = generateVertexId >>= (\vId -> Vertex (Node vId e))


-- connect outputs from a to inputs of b
-- such that x -> Output 1, Input 1 -> y
-- gives x -> y
compose :: ZXDiagram -> ZXDiagram -> ZXDiagram
compose a b =
    let g = overlay a b
        -- Get first vertex in each lane of b
        -- Get last vertex in each lane of a
        -- Add edge between vertices
--         inputs = filter isInput $ vertexList b
--         outputs = filter isOutput $ vertexList a
--     in foldr mergeAndRemoveIOVertices g (zip inputs outputs)

-- mergeAndRemoveIOVertices :: (ZXNode,ZXNode) -> ZXDiagram -> ZXDiagram
-- mergeAndRemoveIOVertices (i,o) g =
--     case (getNeighbors i g, getNeighbors o g) of
--        ([iN],[oN]) -> overlay (edge iN oN) . removeVertex o . removeVertex i $ g
--        _       -> error "Inputs and Outputs can only have 1 edge."
