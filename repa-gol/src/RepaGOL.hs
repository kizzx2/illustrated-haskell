{-# LANGUAGE QuasiQuotes
           , TemplateHaskell
           , TypeOperators
           #-}

import qualified Data.Array.Repa as R
import Data.Array.Repa (Z(..), (:.)(..))

import Data.Array.Repa.Stencil

type World = R.Array R.DIM2 Int

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs = go (length xs) xs
    where
        go l xs | l > n = take n xs : go (l-n) (drop n xs)
                | otherwise = [xs]

show2D :: (R.Elt a) => Int -> R.Array R.DIM2 a -> IO ()
show2D n = mapM_ (putStrLn . unwords . map show) . chunksOf n . R.toList

toad :: World
toad = R.fromList (Z :. 6 :. 6)
    [ 0, 0, 0, 0, 0, 0
    , 0, 0, 0, 0, 0, 0
    , 0, 0, 1, 1, 1, 0
    , 0, 1, 1, 1, 0, 0
    , 0, 0, 0, 0, 0, 0
    , 0, 0, 0, 0, 0, 0
    ]

sten :: Stencil R.DIM2 Int
sten = [stencil2| 1 1 1
                  1 0 1
                  1 1 1 |]

transit :: Int -> Int -> Int
transit 1 2 = 1
transit 1 3 = 1
transit 1 _ = 0
transit 0 3 = 1
transit 0 _ = 0


tick :: World -> World
tick world = R.force $ R.zipWith transit world neighbors
    where neighbors = mapStencil2 (BoundConst 0) sten world

program :: World -> IO ()
program world = do
    show2D 6 world
    input <- getLine
    case input of
        "q" -> return ()
        _ -> program $ tick world

main :: IO ()
main = program toad
