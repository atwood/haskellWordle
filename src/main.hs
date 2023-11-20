module Main where

--import System.Random
--import System.Console.ANSI
tempHack = 78

main = do
  putStrLn "Hi, here's a Wordle clone"
  putStrLn "the system has chosen a word."
  putStrLn "You have 6 tries to guess the word."
  w <- chooseWord
  play 6 w []
  return ()

play 0 s g = do
  putStr$ "Sorry, you're out of guesses.\n" ++
           "Word was: " ++ s ++ "\n"
play n secret guesses = do
  g <- getGuess
  if (g==secret) then putStrLn "You Win!"
                 else do
                   putStr$ guess secret g
                   putStrLn$ show (n-1)
                   play (n-1) secret (guesses++[g])
  --return ()

guess s g = 
   let t = map (decorate s) (zip s g)
   in g ++ "\n" ++ t ++ "\n"

decorate s (sl, gl) | sl == gl  = 'G'
decorate s (sl, gl) | elem gl s = 'Y'
decorate s _        | otherwise = ' '
{-
hotCold g s = do
  putStrLn g
  return g

printState guesses = do
   putStrLn (concatMap (++"\n") guesses)
   return ()
-}   
-- a = readFile "words5.txt"
-- b = fmap (read::String->[String]) a
-- c = fmap (!!0) b

chooseWord = do
  a <- readFile "words5.txt"
  b   <- return$ (read::String->[String]) a
  len <- return$ length b
--  r <- getStdRandom (randomR (0,len))
  let r = tempHack
  c   <- return$ (!!r) b
  return c

getGuess = do
  putStrLn "Please enter a guess:"
  g <- getLine
  return g

