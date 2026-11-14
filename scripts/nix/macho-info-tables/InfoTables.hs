{-# OPTIONS_GHC -fno-worker-wrapper #-}
module InfoTables where

{-# NOINLINE f01 #-}
f01 :: Int -> Int
f01 x = x + 1

{-# NOINLINE f02 #-}
f02 :: Int -> Int -> Int
f02 x y = x + y

{-# NOINLINE f03 #-}
f03 :: Int -> Int -> Int -> Int
f03 x y z = x + y * z

{-# NOINLINE f04 #-}
f04 :: Int -> Int
f04 x = if x > 4 then f01 x else f02 x 4

{-# NOINLINE f05 #-}
f05 :: Int -> Int -> Int
f05 x y = f03 x y 5

{-# NOINLINE f06 #-}
f06 :: Int -> Int -> Int -> Int
f06 x y z = f04 x + f05 y z

{-# NOINLINE f07 #-}
f07 :: Int -> Int
f07 x = f06 x 7 x

{-# NOINLINE f08 #-}
f08 :: Int -> Int -> Int
f08 x y = f07 x + f04 y

{-# NOINLINE f09 #-}
f09 :: Int -> Int -> Int -> Int
f09 x y z = f08 x y + z

{-# NOINLINE f10 #-}
f10 :: Int -> Int
f10 x = f09 x 10 x

{-# NOINLINE f11 #-}
f11 :: Int -> Int -> Int
f11 x y = f10 x + f10 y

{-# NOINLINE f12 #-}
f12 :: Int -> Int -> Int -> Int
f12 x y z = f11 x y + f11 y z
