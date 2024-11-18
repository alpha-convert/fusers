{-#LANGUAGE BangPatterns #-}
{-#LANGUAGE TemplateHaskell #-}
module Benchmarks (bm) where

import           Data.Conduit as C
import qualified Data.Conduit.Combinators as C

import           Pipes as P
import qualified Pipes.Prelude as P
  
import qualified Streaming.Prelude as Str

import qualified System.IO.Streams as IOS

import qualified Data.Machine as M
import           Data.Machine.Runner  (foldlT)
import qualified Data.List as List

import           Control.Exception
import           Criterion.Main

import qualified FusersBm as F
--
import           Data.Function ((&))


import           Test.Hspec
import           Test.Hspec.Expectations
big :: Int
big = 250000497000

bm :: IO ()
bm = do
    defaultMain 
      [ bgroup "sum"     [ bench "streaming"      $ nfIO streaming_sum
                         , bench "fusers"         $ nfIO fusers_sum
                         , bench "fusers-fast-range"         $ nfIO fusers_sum'
                         , bench "conduit"        $ nfIO conduit_sum
                         {-
                         
                         -}
                         , bench "pipes"          $ nfIO pipes_sum
                         , bench "iostreams"      $ nfIO iostreams_sum
                         , bench "machine"        $ nfIO machines_sum
                         ]
    --   , bgroup "toList"  [ bench "streaming"      $ nfIO streaming_basic
                        --  , bench "conduit"        $ nfIO conduit_basic
                        --  , bench "pipes"          $ nfIO pipes_basic
                        --  , bench "iostreams"      $ nfIO iostreams_basic
                        --  , bench "machine"        $ nfIO machines_basic
                        --  ]
      ]

fusers_sum :: IO Int
fusers_sum = $$F.fusers_sum

fusers_sum' :: IO Int
fusers_sum' = $$F.fusers_sum'



pipes_basic :: IO Int
pipes_basic = do
    xs <- P.toListM $ P.each [1..1000000]
      >-> P.filter even
      >-> P.map (+1)
      >-> P.drop 1000
      >-> P.map (+1)
      >-> P.filter (\x -> x `mod` 2 == 0)
    assert (Prelude.length xs == 499000) $
        return (Prelude.length xs)

pipes_sum :: IO Int
pipes_sum = do
    n <- P.fold (+) 0 id $ P.each [1..1000000]
      >-> P.filter even
      >-> P.map (+1)
      >-> P.drop 1000
      >-> P.map (+1)
      >-> P.filter (\x -> x `mod` 2 == 0)
    assert (n == big) $ return n

conduit_basic :: IO Int
conduit_basic = do
    xs <-  C.yieldMany [1..1000000]
      $= C.filter even
      $= C.map ((+1) :: Int -> Int)
      $= (C.drop 1000)
      $= C.map ((+1) :: Int -> Int)
      $= C.filter (\x -> x `mod` 2 == 0)
      $$ C.sinkList
    assert (Prelude.length xs == 499000) $
        return (Prelude.length (xs :: [Int]))
--
conduit_sum :: IO Int
conduit_sum = do
    n <-  C.yieldMany [1..1000000]
      $= C.filter even
      $= C.map ((+1) :: Int -> Int)
      $= (C.drop 1000 >> C.awaitForever C.yield)
      $= C.map ((+1) :: Int -> Int)
      $= C.filter (\x -> x `mod` 2 == 0)
      $$ C.foldl (+) 0
    assert (n == big) $
        return n
        
streaming_basic :: IO Int
streaming_basic = do
    xs <- Str.toList_ $ 
      Str.each [1..1000000]
      & Str.filter even
      & Str.map (+1)
      & Str.drop 1000
      & Str.map (+1)
      & Str.filter (\x -> x `mod` 2 == 0)
    assert (Prelude.length xs == 499000) $
        return (Prelude.length (xs :: [Int]))


streaming_sum :: IO Int
streaming_sum = do
    n <- Str.sum_ $ 
      Str.each [1..1000000]
      & Str.filter even
      & Str.map (+1)
      & Str.drop 1000
      & Str.map (+1)
      & Str.filter (\x -> x `mod` 2 == 0)
    assert (n == big) $
        return n
--


iostreams_sum :: IO Int
iostreams_sum = do
  s0 <- IOS.fromList [1..1000000]
  s1 <- IOS.filter even s0
  s2 <- IOS.map (+1) s1
  s3 <- IOS.drop 1000 s2
  s4 <- IOS.map (+1) s3
  s5 <- IOS.filter (\x -> x `mod` 2 == 0) s4
  n <- IOS.fold (+) 0 s5
  assert (n == big) $
      return n

iostreams_basic :: IO Int
iostreams_basic = do
  s0 <- IOS.fromList [1..1000000]
  s1 <- IOS.filter even s0
  s2 <- IOS.map (+1) s1
  s3 <- IOS.drop 1000 s2
  s4 <- IOS.map (+1) s3
  s5 <- IOS.filter (\x -> x `mod` 2 == 0) s4
  xs <- IOS.toList s5
  assert (Prelude.length xs == 499000) $
      return (Prelude.length (xs :: [Int]))
      



machines_basic :: IO Int
machines_basic =  do 
  xs <- M.runT $ 
          M.source [1..1000000 :: Int]
          M.~> M.filtered even 
          M.~> M.mapping (+1) 
          M.~> M.dropping 1000 
          M.~> M.mapping (+1)
          M.~> M.filtered (\x -> x `mod` 2 == 0)
  assert (Prelude.length xs == 499000) $
      return (Prelude.length xs)

machines_sum :: IO Int
machines_sum =  do 
  n <- foldlT (+) 0 $ 
          M.source [1..1000000 :: Int]
          M.~> M.filtered even 
          M.~> M.mapping (+1) 
          M.~> M.dropping 1000 
          M.~> M.mapping (+1)
          M.~> M.filtered (\x -> x `mod` 2 == 0)
  assert (n == big) $
      return n
  