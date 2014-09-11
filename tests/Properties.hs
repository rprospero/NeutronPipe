import Test.Tasty
import Test.Tasty.QuickCheck
import Vec

testDouble :: Int -> Int -> Int -> Bool
testDouble a b c = xs + xs == scale 2 xs
    where
      xs = Vec a b c

testTriple
  :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool
testTriple ax ay az bx by bz cx cy cz  = a * (b * c) == scale (a `dot` c) b - scale (a `dot` b) c
    where
      a = Vec ax ay az
      b = Vec bx by bz
      c = Vec cx cy cz

crossTest :: Int -> Int -> Int -> Bool
crossTest a b c = x * x == 0
    where
      x = Vec a b c

tests = testGroup "Tests" [testProperty "Cross" $ crossTest,
                          testProperty "Triple" $ testTriple,
                          testProperty "Scale" $ testDouble]

main :: IO ()
main = defaultMain tests
