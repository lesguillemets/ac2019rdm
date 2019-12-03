{-# LANGUAGE InstanceSigs #-}
module Lib
    ( RandomM (..)
    , runRandom
    ) where

newtype RandomM g a = RandomM { runR :: g -> (a,g) }
runRandom :: g -> RandomM g a -> (a,g)
runRandom g r = runR r g

instance Functor (RandomM g) where
    fmap :: (a -> b) -> RandomM g a -> RandomM g b
    fmap f r = RandomM $ \ g -> let (a, g') = runR r g
                                    in (f a, g')

instance Applicative (RandomM g) where
    pure :: a -> RandomM g a
    pure a = RandomM (\g -> (a,g))
    rf <*> ra = RandomM (\g -> let (f, g') = runR rf g
                                   (a, g'') = runR ra g
                                   in (f a, g''))

instance Monad (RandomM g) where
    ra >>= fr = RandomM (\g -> let (a, g') = runR ra g in
                                    runR (fr a) g')

--
-- g <- getStdGn
-- let x = runRandom g $ do
--     d <- randInt 
--     y <- randIntBetween 1 3
--     b :: Bool <- random
--     return (d,y,b)
--  -- g : (g', d,y,b)
