import Test.Tasty
import Test.Tasty.QuickCheck
import Vec

testDouble :: Vec Int -> Bool
testDouble xs = xs + xs == scale 2 xs

testTriple
  :: Vec Int -> Vec Int -> Vec Int -> Bool
testTriple a b c  = a * (b * c) == scale (a `dot` c) b - scale (a `dot` b) c

crossTest :: Vec Int -> Bool
crossTest x = x * x == 0

tests = testGroup "Tests" [testProperty "Cross" $ crossTest,
                          testProperty "Triple" $ testTriple,
                          testProperty "Scale" $ testDouble]

main :: IO ()
main = defaultMain tests
