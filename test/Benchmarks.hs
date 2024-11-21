{-

Taken from https://gist.github.com/michaelt/ee3710c5bab9b7d0892bd552e0eedfd9

-}
{-#LANGUAGE BangPatterns #-}
{-#LANGUAGE TemplateHaskell #-}
module Benchmarks (bm) where

import Criterion.Main

import qualified Data.Conduit      as C
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as C hiding (map, drop, sinkNull, filter)

import qualified Data.Machine      as M
import qualified Data.Machine.Runner as M

import qualified Pipes             as P
import qualified Pipes.Prelude     as P
import qualified Streaming.Prelude as S

import Streaming (Of (..))
import Streaming.Internal (Stream(..))
import qualified Streaming.Internal as I
import qualified Streaming as S

import qualified System.IO.Streams as Streams

import Data.List (foldl')
import Control.Monad (void)
import Control.Monad.Identity
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
      [ bgroup "fold"
          [ bench "list"      $ whnf (foldl' (+) 0. ($ ()))  sourceL
          , bench "streaming" $ whnf (runIdentity . S.fold (+) 0 id . ($ ())) ( sourceS)
          , bench "conduit"   $ whnf (runIdentity . (C.$$ CC.foldl (+) 0) . ($ ())) sourceC
          , bench "pipes"     $ whnf (runIdentity . P.fold (+) 0 id . ($ ())) sourceP
          , bench "machines"  $ whnf (runIdentity . M.foldlT  (+) 0 . ($ ())) sourceM
          ]
      , bgroup "map"
          [ bench "streaming" $ whnf (runIdentity . drainS) (S.map (+1))
          , bench "conduit"   $ whnf ((runIdentity . drainC)) (C.map (+1))
          , bench "pipes"     $ whnf (runIdentity . drainP) (P.map (+1))
          , bench "machines"  $ whnf (runIdentity . drainM) (M.auto (+1))
          ]
      , bgroup "drop"
          [ bench "streaming" $ whnf (runIdentity . drainS) (S.drop value)
          , bench "conduit" $ whnf (runIdentity . drainC) (C.drop value)
          , bench "pipes" $ whnf (runIdentity . drainP) (P.drop value)
          , bench "machines" $ whnf (runIdentity . drainM) (M.dropping value)
          ]
      , bgroup "dropWhile"
          [ bench "streaming" $ whnf (runIdentity . drainS) (S.dropWhile (<= value))
          , bench "conduit" $ whnf (runIdentity . drainC) (CC.dropWhile (<= value))
          , bench "pipes" $ whnf (runIdentity . drainP) (P.dropWhile (<= value))
          , bench "machines" $ whnf (runIdentity . drainM) (M.droppingWhile (<= value))
          ]
      , bgroup "scan"
          [ bench "streaming" $ whnf (runIdentity . drainS) (S.scan (+) 0 id)
          , bench "conduit" $ whnf (runIdentity . drainC) (CC.scanl (+) 0)
          , bench "pipes" $ whnf (runIdentity . drainP) (P.scan (+) 0 id)
          , bench "machines" $ whnf (runIdentity . drainM) (M.scan (+) 0)
          ]
      , bgroup "take"
          [ bench "streaming" $ whnf (runIdentity . drainS) (S.take value)
          , bench "conduit" $ whnf (runIdentity . drainC) (C.isolate value)
          , bench "pipes" $ whnf (runIdentity . drainP) (P.take value)
          , bench "machines" $ whnf (runIdentity . drainM) (M.taking value)
          ]
      , bgroup "takeWhile"
          [ bench "streaming" $ whnf (runIdentity . drainS) (S.takeWhile (<= value))
          , bench "conduit" $ whnf (runIdentity . drainC) (CC.takeWhile (<= value) C.=$= C.sinkNull)
          , bench "pipes" $ whnf (runIdentity . drainP) (P.takeWhile (<= value))
          , bench "machines" $ whnf (runIdentity . drainM) (M.takingWhile (<= value))
          ]
       , bgroup "composite"     [ bench "streaming"      $ nfIO streaming_sum
                         , bench "fusers"         $ nfIO fusers_sum
                         , bench "fusers-fast-range"         $ nfIO fusers_sum'
                         , bench "conduit"        $ nfIO conduit_sum
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
{-

yield :: forall (m :: * -> *) o i. Monad m => o -> ConduitT i o m ()
yield o = ConduitT $ \rest -> HaveOutput (rest ()) o

yieldManyC :: MonoFoldable mono => mono -> ConduitT i (Element mono) m ()
yieldManyC [1..n] = ofoldMap yield [1..n]
                  {- definition of ofoldMap -}
                  = foldMap yield [1..n]
                  {- definition of foldmap on lists -}
                  = ((mconcat .) . map) yield [1..n]
                  {- definition -}
                  = mconcat (map yield [1..n])
                  {- mconcat -}
                  = foldr mappend mempty (map yield [1..n])
                  {- definition -}
                  = foldr (\k o -> yield k <> o) mempty [1..n]
                  {- definitions... -}
                  = foldr (\k o -> yield k >> o) mempty [1..n]
                  = foldr (\k o -> yield k >>= \_ -> o) mempty [1..n]
                  = foldr (\k o -> (ConduitT $ \rest -> HaveOutput (rest ()) k) >>= \_ -> o) mempty [1..n]
                  = foldr (\k o -> ConduitT $ \h -> (\rest -> HaveOutput (rest ()) k) $ \a -> unConduitT ((\_ -> o) a) h)
                  = foldr (\k o -> ConduitT $ \h -> (\rest -> HaveOutput (rest ()) k) $ \a -> unConduitT o h)
                  = foldr (\k o -> ConduitT $ \h -> HaveOutput ((\a -> unConduitT o h) ()) k)
                  = foldr (\k o -> ConduitT $ \h -> HaveOutput (unConduitT o h) k) mempty [1..n]
-}

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
  
value :: Int
value = 100000

drainS p = S.effects (p (sourceS ()) )

-- drainM :: M.ProcessT Identity Int o -> ()
drainM m =  M.runT_ (sourceM () M.~> m)

-- drainP :: P.Proxy () Int () a Identity () -> ()
drainP p =  P.runEffect $ P.for (sourceP () P.>-> p) P.discard

-- drainC :: C.Conduit Int Identity a -> ()
drainC c = (sourceC () C.$= c) C.$$ C.sinkNull

-- drainSC :: C.Sink Int Identity b -> b
drainSC c = sourceC () C.$$ c

sourceM () = M.enumerateFromTo 1 value
sourceC () = C.sourceList [1..value]
sourceP () = P.each [1..value]
sourceS () = S.each [1..value] -- take value $ S.enumFrom 1
sourceL () = [1..value]