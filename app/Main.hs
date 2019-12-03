module Main where

import Lib
import Control.Monad
import System.Random

main :: IO ()
main = do
    g <- getStdGen
    let (results,_) = runRandom g $ foldM (\acc _ -> fmap (:acc) (rollDiceN 100)) [] [1..10000]
    mapM_ print results


rollDiceN :: RandomGen g => Int -> RandomM g Int
rollDiceN n = foldM (\acc _ -> fmap (+ acc) dice) 0 [1..n]

dice :: RandomGen g => RandomM g Int
dice = RandomM $ randomR (1,6)

roll_dice10_10 :: RandomGen g => RandomM g [Int]
roll_dice10_10 = do
    x0 <- rollDiceN 10
    x1 <- rollDiceN 10
    x2 <- rollDiceN 10
    x3 <- rollDiceN 10
    x4 <- rollDiceN 10
    x5 <- rollDiceN 10
    x6 <- rollDiceN 10
    x7 <- rollDiceN 10
    x8 <- rollDiceN 10
    x9 <- rollDiceN 10
    return $ [x0,x1,x2,x3,x4,x5,x6,x7,x8,x9]
