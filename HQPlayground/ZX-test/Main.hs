import Test.Tasty
import ConverterTests
import SimplifyTests
import ExtractTests

main :: IO ()
main = defaultMain $ testGroup "ZX tests" [SimplifyTests.tests,ConverterTests.tests, ExtractTests.tests]