module F1 where

import Data.Char

-- Vad ska de andra funktionernas typsignaturer vara?
fib :: Int -> Int

fib n = last (fibSerie [0,1] n)

fibSerie :: [Int] -> Int -> [Int]
fibSerie xs n
    | len == n = xs ++ [(xs !! (len - 1) + xs !! (len - 2))]
    | len < n = fibSerie (xs ++ [(xs !! (len - 1) + xs !! (len - 2))]) n
    | otherwise = [n] -- Deals with fib 0 and fib 1 if they come.
    --Could also have done it as other basecases above fib n function although I think this should be quicker.
    where len = length xs


rovarsprak :: [Char] -> [Char]
rovarsprak (x:xs)
    | elem x vokaler = x : rovarsprak xs
    | otherwise = x : ('o' : (x : (rovarsprak xs)))
    where vokaler = "aeiouyåäö"
rovarsprak [] = []

karpsravor :: [Char] -> [Char]
karpsravor (x:xs)
    | elem x vokaler = x : karpsravor xs
    | otherwise = x : karpsravor (tail (tail xs))
    where vokaler = "aeiouyåäö"
karpsravor [] = []


medellangd :: [Char] -> Double
medellangd s = fst result / (snd result)
    where result = medellangdcalc s 1

medellangdcalc :: [Char] -> Double -> (Double, Double)
medellangdcalc (s:ss) addnewword
    | isAlpha s  = (fst nextiteration + 1, snd nextiteration + addnewword)
    | otherwise = (fst nextiteration, snd nextiteration)
    where nextiteration = medellangdcalc ss (if isAlpha s then 0 else 1)
medellangdcalc [] _ = (0, 0)


skyffla :: [a] -> [a]
skyffla s = varannan s [] True

varannan :: [a] -> [a] -> Bool -> [a]
varannan [] [] _ = []
varannan [] end _ = varannan end [] True
varannan start end totake
    | totake = take 1 start ++ varannan (drop 1 start) end (not totake)
    | otherwise = varannan (drop 1 start) (end ++ take 1 start) (not totake)


    
