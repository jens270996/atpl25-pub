module HQP.ZX.Visualize where
import Diagrams.Prelude
import HQP.ZX.Syntax
import Algebra.Graph.Undirected
import Diagrams.Backend.SVG.CmdLine
import HQP.ZX.Utils


visualizeDiagram :: ZXDiagram -> Diagram B
visualizeDiagram g =
    let
        vs = vertexList g
        es = edgeList g
        is = filter isInput $ vs
        os = filter isOutput $ vs
        ios = zip is os
    in visualizeVertices vs ios <> visualizeEdges es
    -- zip IO pairs
    -- for each pair create horizontal line of all nodes in between
    -- finally addEdges


visualizeVetrices :: [ZXNode] -> [(ZXNode,ZXNode)] -> Diagram B
visualizeVertices vs ios =
    vsep 2 $ map (\(i,o) -> visualizeLine $ getNodesBetweenIO vs i o) ios


visualizeLine :: [ZXNode] -> Diagram B
visualizeLine ns = hsep 5 . map visualizeNode $ ns

visualizeNode :: ZXNode -> Diagram B
visualizeNode (id,e) = visualizeElement e # named (show id)

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

getNodesBetweenIO :: [ZXNode] -> ZXNode -> ZXNode -> [ZXNode]
getNodesBetweenIO vs (iid,Input) (oid,Output) = filter (\(id,n) -> id >= iid && id <= oid) vs


-- Use arrow to connect
visualizeEdges :: [(ZXNode,ZXNode)] -> Diagram B
visualizeEdges = mconcat visualizeEdge


visualizeEdge :: (ZXNode,ZXNode) -> Diagram B
visualizeEdge ((id,_),(id',_)) = (with & arrowTail .~ noTail & lengths .~ large
                 & arrowHead .~ noHead
                 & shaftStyle %~ lw veryThick ) (show id) (show id')