module Main where

-- Pascal's triangle


--function to sum consecutive digits
{-

  1 1
  121    121
         210
  1331
  14641

-}

onStep init@ (x:xs) = 
  [1] ++ ks
  where
    x = (tail init) ++ [0]
    ks = zipWith (+) init x

pascal n = (iterate onStep [1,1]) !! (n-1)



main = do
  putStrLn "otiger"
