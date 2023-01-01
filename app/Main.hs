module Main (main) where

import Data.Number.CReal

main :: IO ()
main = do
  putStrLn $ showTable  $  tail $ piTable 20
--  putStrLn ("n = 124\n  pi = " ++ showCReal 1000 approx1)
--  putStrLn ("n = 150\n  pi = " ++ showCReal 1000 approx2)

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

term :: Int -> CReal --BigFloat (PrecPlus20 Prec50) 
term n = factDb (4 * n) * (1103.0 + 26390.0 * nDb) / ( (factDb n)^^4 * 396.0^^(4 * n)) 
  where nDb= fromIntegral n

factDb :: Int -> CReal --BigFloat (PrecPlus20 Prec50)
factDb 0 = 1.0
factDb n = fromIntegral n * factDb (n-1)


showCRealSci' :: CReal -> String
showCRealSci' aDiff = "\n"


showCRealSci :: CReal -> String
showCRealSci cr = showCReal 3 (cr * 10^^(-e)) ++ "e" ++ show e
  where e = (floor $ (read (showCReal 10 $ logBase 10 cr) :: Float)) :: Int

showTable  :: [(Int, CReal, CReal)] -> String
showTable [] = ""
showTable ( (n, piApprox, deltaPi) : moreRows ) = show n ++ "   " ++ showCReal 200 piApprox ++ "\n" ++ (showCRealSci deltaPi) ++ "\n\n" ++ showTable moreRows


