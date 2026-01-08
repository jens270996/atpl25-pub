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
        -- Start at lane 0, depth 1, add inputs at depth 0
        (diagram,_) = runIdGenerator (fromQOp circ) (0,1)
        firstLayer = map minimum . groupBy sameLane $ vertexList diagram
        lastLayer = map maximum . groupBy sameLane $ vertexList diagram
        inputs = map (\(Node (lane,_) _) -> (Node (lane,0) Input)) firstLayer
        outputs = map (\(Node (lane,depth) _) -> (Node (lane,depth+1) Output)) lastLayer
    in overlay (edges (zip firstLayer inputs)) . overlay (edges (zip lastLayer outputs)) $ diagram



fromQOp :: QOp.QOp -> IdGenerator ZXDiagram
fromQOp op = case op of
    QOp.Z           -> wrap (Green (PiHalves 2))
    QOp.X           -> wrap (Red (PiHalves 2))
    QOp.R QOp.Z a   -> wrap (Green (Frac a))
    QOp.R QOp.X a   -> wrap (Red (Frac a))
    QOp.H           -> wrap H -- split into three spiders
    QOp.Id 1        -> wrap Wire
    QOp.C QOp.X     -> do c1 <- generateVertexId
                          switchLane
                          c2 <- generateVertexId
                          return $ edge (Node c1 (Green (PiHalves 0))) (Node c2 (Red (PiHalves 0)))
    QOp.Tensor a b -> do d <- getDepth -- Get current depth, so we can return
                         g <- fromQOp a
                         d' <- getDepth
                         setDepth d -- Reset depth
                         switchLane
                         g' <- fromQOp b
                         d'' <- getDepth
                         setDepth $ max d' d'' -- We continue from the greatest depth
                         return $  overlay g g'
    QOp.Compose a b -> do l <- getLane -- Get current lane, so we can return
                          g <- fromQOp a
                          setLane l -- Reset lane (probably always to first lane)
                          proceed
                          g' <- fromQOp b
                          setLane l -- Reset lane (probably always to first lane) 
                          return $ compose g g'
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