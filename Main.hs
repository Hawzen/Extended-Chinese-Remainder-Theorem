module Main where
import System.IO
import Data.Either
import Text.Printf
import System.Exit

type Modulo = Integer
data Equation = Equation { a :: Integer
                         , b :: Integer
                         , m :: Modulo
                         } deriving (Eq, Show)


intro = "281 Project | Fall 2020 | Mohand Alrasheed"
main :: IO ()
main = do
    putStrLn intro
    eqs <- getEquations
    
    -- Check a m coprimality 
    sequence $ map handleCheck eqs

    -- Calculate m
    let ms = map m eqs
        as = map a eqs
        (bigM, bigMs) = calcMs ms

    -- Check if ms are pairwise coprime
    if coprimes ms then return ()
        else do
            putStrLn "No solution (modulos are not pairwise coprime)"
            again

    let ans = (sum $ map (solutionTerm bigM) eqs) `mod` bigM
        -- Check solutions
        bs = map (\(a, m) -> a * ans `mod` m) $ zip as ms
        checkSolution (a, b, m) = printf "\t%d * %d = %d (mod %d)\n" ans a b m :: String
        checks = map checkSolution $ zip3 as bs ms
    -- Print results and checks
    printf "X: %d\n" ans
    printf "Checks:\n%s\n" $ mconcat checks
    again

    where
      checkEquation :: Equation -> Either Equation String
      checkEquation eq@(Equation a _ m) 
          | m <= 1 = Right "No solution (m is <= 1)"
          | coprime a m = Left $ eq
          | otherwise = Right $ "No solution (" ++ (show a) ++ " And " ++ (show m) ++ " are not coprime)"

      handleCheck eq = do
            let res = checkEquation eq
                printAgain s = do 
                        putStrLn s
                        again
            either (\_ -> return ()) printAgain res
            
      coprime :: Integer -> Integer -> Bool
      coprime a b = gcd' a b == 1

      coprimes :: [Modulo] -> Bool
      coprimes [] = True
      coprimes [x] = True
      coprimes (x:xs) = and (map (coprime x) xs)
                        && coprimes xs

      -- Helper function
      again :: IO ()
      again = do
          putStrLn "Again? (y\\n): "
          again <- getLine
          if ('y' `elem` again) then main else exitSuccess



getEquation :: IO Equation
getEquation = do 
            putStr "Enter an Equation (a b m): "
            hFlush stdout
            nums <- getLine
            let (a:b:m:[]) = handleRead $ map read $ words nums
                a':b':[] = map (formatter m) [a, b]
            return (Equation a' b' m)
        where
            handleRead (a:b:m:[]) = (a:b:m:[])
            handleRead  _ = error "Wrong format"
            formatter m x = x `mod` m

getEquations :: IO [Equation]
getEquations = do 
            putStr "Enter the number of equations: "
            hFlush stdout
            n <- getInt :: IO Int
            sequence . take n $ repeat getEquation
        where
            getInt = fmap read getLine

gcd' :: Integer -> Integer -> Integer
gcd' a 0 = a
gcd' a b = let (q, r) = a `divMod` b
           in gcd' b r

calcMs :: [Modulo] -> (Modulo, [Modulo])
calcMs ms  = let bigM = product ms
                 bigMs = map (div bigM) ms
             in (bigM, bigMs)

inverse :: Integer -> Modulo -> Integer
inverse a m = let condition x = a * x `mod` m == 1
                  list = filter condition [1..m]
              in head list 

solutionTerm :: Modulo -> Equation -> Integer
solutionTerm bigM (Equation a b m) = 
        let a' = inverse a m
            bigMi = div bigM m
            y = inverse bigMi m
        in a' * b * bigMi * y