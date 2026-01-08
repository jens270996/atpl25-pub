module HQP.ZX.Extract where
import HQP.ZX.Syntax
import qualified HQP.QOp.Syntax as QOp

extract :: ZXDiagram -> QOp.QOp
extract g =
    let inputs = filter (\(Node _ el) -> el == Input) $ vertexList g
        circuit = QOp.Id $ length inputs
    in case progressBoundary (g,circuit,inputs) of
        (empty,circuit,[]) -> circuit
    -- peel off inputs
    -- progress each lane one step to get a tensor QOp.
    -- recursively compose until reaching outputs.
    -- at each layer fix all the stuff that needs to be done.
    -- TODO: What if a CNOT gate has been flipped?
progressBoundary :: (ZXDiagram,QOp.QOp,[ZXNode]) -> (ZXDiagram,QOp.QOp,[ZXNode])
progressBoundary (g,circuit,boundary) | isEmpty g  = (g,circuit,boundary) -- Check that all nodes in boundary are outputs
progressBoundary (g,circuit,boundary) =
    -- Split multilegged spiders
    let (g,boundary) = foldr splitSpider (g,[]) boundary
        -- Turn boundary into quantumCirquit
        -- Find new boundary
        -- Remove old boundary from graph
    in progressBoundary (g,circuit,boundary)

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
                        let (id',id'') = closestFreeIdPair n (toList $ neighbours n g)
                            zeroSpider = (Node id'' (if isGreen then Green 0 else Red 0))
                            g = replaceVertex n zeroSpider g
                            -- update id for n
                            n =  updateId n id'
                        in (overlay g (edge n zeroSpider),n)
                    _ -> -- 0 spider with more than 3 legs
                        let (id',id'') = closestFreeIdPair n (toList $ neighbours n g)
                            neighs = toList $ neighbours n g
                            neigh = head $ filter (\n' -> vertexLane n' /= vertexLane n) neighs
                            -- connect rest of legs to zeroSpider
                            g = replaceVertex n zeroSpider g
                            -- update id for n
                            n =  updateId n id'
                            -- remove edge from zeroSpider to neigh
                            g = removeEdge zeroSpider neigh
                            -- connect root to neigh
                            g = overlay g $ edge n neigh
                            -- connect root to zeroSpider
                            g = overlay g $ edge n zeroSpider
                        in (overlay g (edge n zeroSpider),n)

nextInLane :: ZXDiagram -> ZXNode -> Either String ZXNode
nextInLane g n = case toList $ neighbours n g of
                    [n'] | vertexLane n' == vertexLane n -> Right n'
                    _ -> Left "No single adjacent vertex in lane."

updateId :: ZXNode -> Id -> ZXNode
updateId (Node _ e) id' = (Node id' e)

closestFreeIdPair :: ZXDiagram -> ZXNode -> (Id,Id)
closestFreeIdPair g n = filter (\n' -> vertexLane n == vertexLane n') $ vertexList g

closestFreeIdPair :: ZXNode -> [ZXNode] -> Int -> (Id,Id)
closestFreeIdPair n ns dist = case filter (\n' -> abs $ vertexDepth n - vertexDepth n' <= dist) ns of
                                [] -> closestFreeIdPair n ns (dist+1)
                                (n':_) -> (min (getVertexId n) (getVertexId n'), max (getVertexId n) (getVertexId n'))


                      