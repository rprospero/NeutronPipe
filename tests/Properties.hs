import Test.Tasty
import Test.Tasty.QuickCheck
import Vec

-- --We're going to use ints because they escape the issues with floating point
-- arbitraryVec :: Gen (Vec Int)
-- arbitraryVec = choose (Vec (-1) (-1) (-1),
--                        Vec 1 1 1)

-- tripToTup :: [t] -> (t, t, t)
-- tripToTup (a:b:c:xs) = (a,b,c)

-- arbitraryTriple = fmap tripToTup $ suchThat (listOf arbitraryVec) (\x -> length x > 3)

-- testZero :: (Show a, Num a, Eq a) => a -> Property
-- testZero xs = (xs + 0) === xs

-- testDouble :: (Show a, Num a, Eq a) => Vec a -> Property
-- testDouble xs = xs + xs === scale 2 xs

-- testTriple
--   :: (Show a, Num a, Eq a) => (Vec a, Vec a, Vec a) -> Property
-- testTriple (a, b, c) = a * (b * c) === scale (a `dot` c) b - scale (a `dot` b) c

-- testCross :: (Show a, Num a, Eq a) => a -> Property
-- testCross xs = xs * xs === 0

-- tests = [testProperty "Vector Addition Identity" $ forAll arbitraryVec testZero,
--         testProperty "Vector Scaling" $ forAll arbitraryVec testDouble,
--         testProperty "Cross Self is Zero" $ forAll arbitraryVec testCross,
--         testProperty "Vector Triple Product" $ forAll arbitraryTriple testTriple]

crossTest :: Int -> Int -> Int -> Bool
crossTest a b c = x * x == 0
    where
      x = Vec a b c

tests = testGroup "Tests" [testProperty "Cross" $ crossTest]

main :: IO ()
main = defaultMain tests
