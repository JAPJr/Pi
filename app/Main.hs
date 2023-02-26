module Main (main) where

import Data.Number.CReal
import Data.List


-- Calculate pi to an arbitrary number of decimal places using Ramanujian's formula 
-- 1000 digits can be calculated with 125 terms in a couple of minutes



main :: IO ()
main = do

  putStrLn "Enter number of terms in pi approximation:  "
  n <- fmap read getLine :: IO Int
  putStrLn "Enter the number of digits to be displayed:  "
  ndig <- fmap read getLine :: IO Int
--  putStrLn ("pi = " ++ showCReal 1000 (piNTerms n))
  putStrLn ("pi = " ++ showCReal ndig (piNTerms n))
{--
  putStrLn "Enter term number for desired approximations of pi in the form '[n1, n2, ...]'"
  ns <- fmap read getLine :: IO [Int]
  putStrLn $ showTable $ piTableForNs' $ piUsingNsTerms ns
 --}




piTable :: Int -> [(Int, CReal, CReal)]
piTable nMax = map addDiffs [0 .. nMax]
  where pis = map (\n -> (n, piNTerms n)) [0 .. nMax] :: [(Int, CReal)] 
        addDiffs :: Int -> (Int, CReal, CReal)
        addDiffs nNew  = if nNew == 0 then (nNew, snd (pis !! 0), (0 :: CReal))
                                    else  (nNew, snd (pis !! nNew), abs ( snd (pis !! nNew) - snd (pis !! (nNew - 1)) ) )   



piNTerms :: Int -> CReal
piNTerms n = 9801.0 / (sqrt 8 * series n) 

series :: Int -> CReal
series n
  |n < 0 = 0
  |otherwise = term n + series (n-1)
  where term :: Int -> CReal
        term n = factCR (4 * n) * (1103.0 + 26390.0 * fromIntegral n) / ( (factCR n)^^4 * 396.0^^(4 * n)) 


factCR :: Int -> CReal
factCR 0 = 1.0
factCR n = fromIntegral n * factCR (n-1)


showCRealSci' :: CReal -> String
showCRealSci' aDiff = "\n"


showCRealSci :: CReal -> String
showCRealSci cr = showCReal 3 (cr * 10^^(-e)) ++ "e" ++ show e
  where e = (floor $ (read (showCReal 10 $ logBase 10 cr) :: Float)) :: Int

showTable  :: [(Int, CReal, CReal)] -> String
showTable [] = ""
showTable ( (n, piApprox, deltaPi) : moreRows ) = show n ++ "\n" ++ showCReal 1000 piApprox ++ "\n" ++ (showCRealSci deltaPi) ++ "\n\n" ++ showTable moreRows


piUsingNsTerms :: [Int] -> [(Int, CReal)]
piUsingNsTerms ns = zip ns $ map calcPi $ reverse $ snd $ foldl (stepThroughTerms ns) (0, []) [0 .. nMax]
  where nMax = maximum ns
        calcPi seriesSum = 9801.0/(sqrt 8 * seriesSum)

        
stepThroughTerms :: [Int] -> (CReal, [CReal]) -> Int -> (CReal, [CReal])
stepThroughTerms ns (seriesSum, keepSums) n                                  
  | n `elem` ns = (newSeriesSum, newKeepSums)        
  | otherwise   = (newSeriesSum, keepSums)
  where newSeriesSum = addTerm seriesSum n
        newKeepSums = newSeriesSum : keepSums

piTableForNs :: [(Int, CReal)] -> [(Int, CReal, CReal)]
piTableForNs nsAndPis = snd $ foldl' ( \(piLast, table) (nNext, piNext) -> (piNext, (nNext, piNext, abs (piNext - piLast) ) : table) ) (0, []) nsAndPis

piTableForNs' :: [(Int, CReal)] -> [(Int, CReal, CReal)]
piTableForNs' nsAndPis =  zipWith addDiff pisShifted nsAndPis 
  where pisShifted = init $ 0 : map snd nsAndPis
        addDiff piPrev (n, pi) = (n, pi, abs (pi - piPrev))
 

addTerm :: CReal -> Int -> CReal
addTerm currentApprox n = currentApprox + term n
  where term :: Int -> CReal
        term n = factCR (4 * n) * (1103.0 + 26390.0 * fromIntegral n) / ( (factCR n)^^4 * 396.0^^(4 * n))
