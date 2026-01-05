module HQP.ZX.Visualize where
import Diagrams.Prelude
import HQP.ZX.Syntax
import Algebra.Graph.Undirected
import Diagrams.Backend.SVG.CmdLine
import HQP.ZX.Utils
import Data.List


visualizeDiagram :: ZXDiagram -> Diagram B
visualizeDiagram g =
    let
        es = edgeList g
        is = filter isInput $ vertexList g
        os = filter isOutput $ vertexList g
    in visualizeVertices es (zip is os) # visualizeEdges es

visualizeVertices ::  [(ZXNode,ZXNode)] -> [(ZXNode,ZXNode)] -> Diagram B
visualizeVertices es ios =
    -- TODO: filter out nodes already used on a path, so we do not display a node twice.
    vsep 2 $ map (\(i,o) -> visualizeLine $ iOPath es i o) ios


visualizeLine :: [ZXNode] -> Diagram B
visualizeLine ns = hsep 5 . map visualizeNode $ ns

visualizeNode :: ZXNode -> Diagram B
visualizeNode (Node id e) = visualizeElement e # named (show id)

visualizeElement :: ZXElement -> Diagram B
visualizeElement H = square 0.4 # fc yellow 
visualizeElement (Red p) = (text $ show p) <> circle 1 # fc red
visualizeElement (Green p) =  (text $ show p ) <> circle 1 # fc green
visualizeElement Input = text "in" <> circle 1 
visualizeElement Output = text "out" <> circle 1 



visualizeInputs :: ZXDiagram -> Diagram B
visualizeInputs g = vsep 1 (map visualizeElement $ getInputs g)
visualizeOutputs :: ZXDiagram -> Diagram B
visualizeOutputs g = vsep 1 (map visualizeElement $ getOutputs g)

visualizeElements :: ZXDiagram -> Diagram B
visualizeElements g = vsep 0.2 (map visualizeElement $ getElementsWhere g (\x -> x/=Output && x/=Input))

getInputs :: ZXDiagram -> [ZXElement]
getInputs g = getElementsWhere g (Input==)

getOutputs :: ZXDiagram -> [ZXElement]
getOutputs g = getElementsWhere g (Output==)

getElementsWhere :: ZXDiagram -> (ZXElement -> Bool) -> [ZXElement]
getElementsWhere g pred = filter pred . map getElement $ vertexList g 


-- Instead :: Find shortest path from input to output.
-- Idea :: Always pick neighbor with largest id??
-- TODO :: More robust solution?? Can we impose some ordering on the graph?
iOPath :: [(ZXNode,ZXNode)] -> ZXNode -> ZXNode -> [ZXNode]
iOPath es i o = iOPathRec es (getVertexId i) (getVertexId o) [i]

iOPathRec :: [(ZXNode,ZXNode)] -> Int -> Int -> [ZXNode] -> [ZXNode]
iOPathRec _ cid did path | cid == did = reverse path
iOPathRec es cid did path =
    let next = maximum . map snd . filter (\(Node id' _, Node id'' _) -> id' == cid) $ es
    in  iOPathRec es (getVertexId next) did (next:path)
-- Use arrow to connect
visualizeEdges :: [(ZXNode,ZXNode)] -> Diagram B -> Diagram B
visualizeEdges es d = foldr visualizeEdge d es


visualizeEdge :: (ZXNode,ZXNode) -> (Diagram B -> Diagram B)
visualizeEdge ((Node id1 _),(Node id2 _)) = (\d -> d # connectOutside' (with & arrowTail .~ noTail & lengths .~ large
                 & arrowHead .~ noHead) (show id1) (show id2))