module Main where
import Data.List
import Debug.Trace

main :: IO ()
main = do
    eqs <- getEquations
    let ms = map m eqs
        (bigM, bigMs) = calcMs ms
    if coprimes ms then return () else error "No solution"
    let summation = sum $ map (`solutionTerm` bigM) eqs
    print $ summation `mod` bigM

{-
    Check pariwise coprimality in main later, maybe in big function with when clause
    Insert test cases via command line | 
 -}

type Modulo = Integer
data Equation = Equation { a :: Integer
                         , x :: Integer
                         , b :: Integer
                         , m :: Modulo
                         } deriving (Eq, Show)


getEquation :: IO Equation
getEquation = do 
            nums <- getLine
            let (a:b:m:[]) = traceShowId (map read $ words nums)
            return (check (Equation a (-1) b m))

check :: Equation -> Equation
check eq@(Equation a _ _ m) 
    | coprime a m = eq
    | otherwise = error $ "Coprime Error: " ++ (show a) ++ " And " ++ (show m) ++ " are not coprime"

getEquations :: IO [Equation]
getEquations = do 
            n <- getInt :: IO Int
            print n
            sequence . take n $ repeat getEquation
        where
            getInt = fmap read getLine

coprime a b = gcd' a b == 1
coprimes :: [Modulo] -> Bool
coprimes [] = True
coprimes [x] = True
coprimes (x:xs) = and (map (coprime x) xs)
                  && coprimes xs

calcMs :: [Modulo] -> (Modulo, [Modulo])
calcMs ms  = let bigM = product ms
                 bigMs = map (div bigM) ms
             in (bigM, bigMs)

gcd' :: Integer -> Integer -> Integer
gcd' a 0 = a
gcd' a b = let (q, r) = a `divMod` b
           in gcd' b r

inverse :: Integer -> Modulo -> Integer
inverse a m = let condition x = a * x `mod` m == 1
              in head $ filter condition [1..]

solutionTerm :: Equation -> Modulo -> Integer
solutionTerm (Equation a x b m) bigMi = 
        let a' = inverse a m
            y = inverse bigMi m
        in a' * b * bigMi * y

-- Assumes a b are relatevly prime
-- scanGcd :: Integer -> Integer -> [(Integer, Integer, Integer)]
-- scanGcd a b = 
--           let stop (_, b, _)= b /= 0
--               cont (a, b, _) = (b, rem a b, div a b)
--           in takeWhile stop $ iterate cont (a, b, 1)