module HQP.ZX.Extract where
import HQP.ZX.Syntax
import HQP.ZX.Utils
import qualified HQP.QOp.Syntax as QOp

extract :: ZXDiagram -> QOp.QOp
extract g =
    let inputs = filter (\(Node _ el) -> el == Input) $ vertexList g
        circuit = QOp.Id $ length inputs
    in case sweepBoundary (g,circuit,inputs) of
        (empty,circuit,[]) -> circuit
    -- peel off inputs
    -- progress each lane one step to get a tensor QOp.
    -- recursively compose until reaching outputs.
    -- at each layer fix all the stuff that needs to be done.
    -- TODO: What if a CNOT gate has been flipped?
sweepBoundary :: (ZXDiagram,QOp.QOp,[ZXNode]) -> (ZXDiagram,QOp.QOp,[ZXNode])
sweepBoundary (g,circuit,boundary) | isEmpty g  = (g,circuit,boundary) -- Check that all nodes in boundary are outputs
sweepBoundary (g,circuit,boundary) =
    -- Split multilegged spiders
    let (g,boundary) = foldr splitSpider (g,[]) boundary
        -- Turn boundary into quantumCirquit
        -- Find new boundary
        -- Remove old boundary from graph
    in sweepBoundary (g,circuit,boundary)

progressBoundary :: (ZXDiagram,QOp.QOp,[ZXNode]) -> (ZXDiagram,QOp.QOp,[ZXNode])
progressBoundary (g,circuit,boundary) =
    -- Check if there are connections in boundary between non-adjacent lanes
    -- if so, compute permutation and swap order
    -- turn boundary into circuit and compose with existing circuit
    -- create new boundary from nexts in lane

isQuantumGate :: ZXDiagram -> ZXNode -> Bool
isQuantumGate g n = case toList $ neighbours n g of
                    [n'] | vertexLane n' == vertexLane n -> True
                    [n',n''] | vertexLane n' == vertexLane n
                               || vertexLane n'' == vertexLane n
                               && asPhase n == (Just $ PiHalves 0) -> True
                    _ -> False

--split a spider and return the root vertex of the set of split vertices
splitSpider :: ZXDiagram -> ZXNode -> (ZXDiagram,ZXNode)
splitSpider g n = case isQuantumGate g n of
                    True -> (g,n)
                    False | asPhase n /= (Just $ PiHalves 0) ->
                        -- shot out zero spider with remaining legs.
                        -- keep root spider with phase
                        let zeroSpider = (Node (getVertexId n) (if isGreen then Green 0 else Red 0))
                            g = replaceVertex n zeroSpider g
                            -- update id for n
                            n =  decrementDepth n
                        in (overlay g (edge n zeroSpider),n)
                    _ -> -- 0 spider with more than 3 legs
                        let neighs = toList $ neighbours n g
                            neigh = head $ filter (\n' -> vertexLane n' /= vertexLane n) neighs
                            -- give root new id
                            root =  updateId n id'
                            -- remove edge from zeroSpider to neigh
                            g = removeEdge n neigh
                            -- connect root to neigh
                            g = overlay g $ edge root neigh
                            -- connect root to zeroSpider
                            g = overlay g $ edge root n
                        in (overlay g (edge n zeroSpider),n)
                      