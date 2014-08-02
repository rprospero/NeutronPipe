import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.HUnit
import Test.QuickCheck

import Vec

arbitraryVec :: Gen (Vec Double)
arbitraryVec = choose (Vec (-1) (-1) (-1),
                       Vec 1 1 1)

testZero :: (Show a, Num a, Eq a) => a -> Property
testZero xs = (xs + 0) === xs

testDouble xs = xs + xs === scale 2 xs

tests = [testProperty "Vector Addition Identity" $ forAll arbitraryVec testZero,
        testProperty "Vector Scaling" $ forAll arbitraryVec testDouble]

main = defaultMain tests
