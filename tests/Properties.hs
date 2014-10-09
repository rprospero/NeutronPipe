import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Vec

import Metropolis (arb)
import Control.Monad (replicateM)
import Data.Random (RVar,runRVar)
import Data.Random.Source.DevRandom

testDouble :: Vec Int -> Bool
testDouble xs = xs + xs == scale 2 xs

testTriple
  :: Vec Int -> Vec Int -> Vec Int -> Bool
testTriple a b c  = a * (b * c) == scale (a `dot` c) b - scale (a `dot` b) c

crossTest :: Vec Int -> Bool
crossTest x = x * x == 0

sphere1 0 = 1/9.0
sphere1 u = ((sin u - u * cos u)/u**3)**2

testSphere :: Double -> Bool
testSphere x = sphere1 x > 0

-- Now we test the random number generator

gaussian :: Double -> Double -> Double -> Double
gaussian m s x = x ** (-2)

arbNorm :: Double -> Double -> RVar Double
arbNorm m s = arb (gaussian m s) (-10,10) 1

mean :: [Double] -> Double
mean xs = sum xs / (fromIntegral $ length xs)

testBasicMean :: Bool
testBasicMean = 10.0 == mean [18, 2]

testMean' :: IO Double
testMean' = flip runRVar DevURandom $ do
  values <- replicateM 1000 (arbNorm 2 2)
  return $ mean values

testMean = do
  m <- testMean'
  print m
  return $ abs m == 1

-- Group Tests

tests = testGroup "Tests" [testProperty "Cross" $ crossTest,
                          testProperty "Triple" $ testTriple,
                          testProperty "Scale" $ testDouble,
                          testCase "Basic Mean" $ assert testBasicMean,
                          testCase "Normal Mean" $ assert testMean,
                          testProperty "Sphere Sanity" $ testSphere]

main :: IO ()
main = defaultMain tests
