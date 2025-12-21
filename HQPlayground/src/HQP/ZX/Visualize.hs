module HQP.ZX.Visualize where
import Diagrams.Prelude
import HQP.ZX.Syntax
import Algebra.Graph.Undirected
import Diagrams.Backend.SVG.CmdLine
import HQP.ZX.Utils


visualizeDiagram :: ZXDiagram -> Diagram B
visualizeDiagram g= layers [visualizeInputs g, visualizeElements g,visualizeOutputs g]


layers :: [Diagram B] -> Diagram B
layers ls = hsep 5 ls
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