import HQP.ZX.Syntax
import HQP.ZX.Visualize
import HQP.ZX.Convert
import Diagrams.Backend.SVG.CmdLine
import qualified HQP.QOp.Syntax as QOp
main :: IO ()
main = mainWith (visualizeDiagram (convert circuit))


circuit :: QOp.QOp
circuit = QOp.H  QOp.⊗   QOp.Z <>  -- First beam splitter
    (QOp.C QOp.X)