module HQP.ZX.Convert where
import qualified HQP.QOp.Syntax as QOp
import HQP.ZX.Syntax
import HQP.ZX.IdGenerator
import Algebra.Graph.Undirected
import HQP.ZX.Utils
import Data.List
import HQP.QOp.Simplify
-- Convert from QOp to ZXOp
-- TODO add numbers
convert :: QOp.QOp -> ZXDiagram
convert op =
    let circ = liftComposes $ op
        (diagram,lanes,depth,_) = runGenerator (fromQOp circ) (laneCount circ)
        inputs = map (\lane -> (Node (lane,0) Input)) (indices . laneCount $ circ)
        withInputs = if isEmpty diagram
                     then vertices inputs
                     else overlay (edges (zip (map minimum . groupBy sameLane $ vertexList diagram) inputs)) diagram
        outputs = map (\lane -> (Node (lane,depth+1) Output)) $ lanes
        lastLayer = map maximum . groupBy sameLane $ vertexList withInputs
    in overlay (edges (zip lastLayer outputs)) withInputs

laneCount :: QOp.QOp -> Lane
laneCount (QOp.Tensor a b) = laneCount a + laneCount b
laneCount (QOp.Compose a _) = laneCount a
laneCount (QOp.C QOp.X) = 2
laneCount (QOp.Id n) = n
laneCount (QOp.Permute xs) = length xs
laneCount _ = 1



fromQOp :: QOp.QOp -> IdGenerator ZXDiagram
fromQOp op = case op of
    QOp.Z           -> wrap (Green (PiHalves 2))
    QOp.X           -> wrap (Red (PiHalves 2))
    QOp.R QOp.Z a   -> wrap (Green (Frac a))
    QOp.R QOp.X a   -> wrap (Red (Frac a))
    QOp.H           -> wrap H -- TODO: split into three spiders
    QOp.Id 1        -> wrap Wire
    QOp.C QOp.X     -> do c1 <- generateVertexId
                          switchLane
                          c2 <- generateVertexId
                          return $ edge (Node c1 (Green (PiHalves 0))) (Node c2 (Red (PiHalves 0)))
    QOp.Tensor a b -> do g <- fromQOp a
                         switchLane
                         g' <- fromQOp b
                         return $  overlay g g'
    QOp.Compose a b -> do g <- fromQOp a
                          proceed
                          g' <- fromQOp b
                          return $ compose g g'
    QOp.Permute permutation -> permute permutation >> return empty
    QOp.Adjoint _ -> error "Remove adjoints before converting to ZX."
    _               -> error "Conversion from QOp to ZXOp not implemented for this constructor."



wrap :: ZXElement -> IdGenerator ZXDiagram
wrap e = do vId <- generateVertexId
            return $ vertex (Node vId e)

compose :: ZXDiagram -> ZXDiagram -> ZXDiagram
compose a b =
    let g = overlay a b
        -- Get first vertex in each lane of b
        bLayer = map minimum . groupBy sameLane $ vertexList b
        -- Get last vertex in each lane of a
        aLayer = map maximum . groupBy sameLane $ vertexList a
        -- Add edge between vertices
    in overlay g . edges $ zip aLayer bLayer


sameLane :: ZXNode -> ZXNode -> Bool
sameLane a b = vertexLane a == vertexLane b