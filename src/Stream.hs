{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module Stream where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Control.Monad
import Gen
import Split
import Data.Void

{-
We need to win on all of these benchmarks.
https://github.com/composewell/streaming-benchmarks/tree/master?tab=readme-ov-file
-}

{-
Even better: we should probably fuse the monads themselves, too!
https://github.com/AndrasKovacs/staged/tree/main/icfp24paper/supplement
-}
data Step a m r s j where
    Jump :: j -> Step a m r s j
    Effect :: CodeQ (m s) -> Step a m r s j
    Tau :: CodeQ s -> Step a m r s j
    Yield :: CodeQ a -> CodeQ s -> Step a m r s j
    Done :: CodeQ r -> Step a m r s j

yield :: MonadGen m' => CodeQ a -> CodeQ s -> m' (Step a m r s j)
yield ca cs = return (Yield ca cs)

jump :: MonadGen m' => j -> m' (Step a m r s j)
jump j = return (Jump j)

done :: MonadGen m' => CodeQ r -> m' (Step a m r s j)
done = return . Done

stateMapC :: Monad m => (CodeQ s -> CodeQ s') -> Step a m r s j -> Step a m r s' j
stateMapC f (Effect cmx) = Effect [|| $$cmx >>= (\s -> return $$(f [||s||])) ||]
stateMapC f (Tau cx) = Tau (f cx)
stateMapC f (Yield ca cx) = Yield ca (f cx)
stateMapC _ (Done cr) = Done cr

data Stream a m r where
    S :: forall j a m r s. CodeQ s -> (CodeQ s -> Gen (Step a m r s j)) -> (j -> CodeQ s -> Stream a m r) -> Stream a m r

{-
CONSTRUCTION
-}

{- cute: you can do completely static lists embedded. probaly a nice feature. -}
fromList :: [CodeQ a] -> Stream a m ()
fromList [] = S [|| () ||] (const (done [||()||])) absurd
fromList (ca:xs) = S [|| () ||] (const _) _

append :: Stream a m () -> Stream a m () -> Stream a m ()
append (S cx0 next andThen) s' = S cx0 next' andThen'
    where
        next' cx = next cx >>= \case
            Done _ -> return (Jump (Right ()))
            Yield ca cx' -> return (Yield ca cx')
            Tau cx' -> return (Tau cx')
            Jump j -> return (Jump (Left j))
            Effect cmx' -> return (Effect cmx')
        andThen' j cx' = case j of
            Left j0 -> append (andThen j0 cx') s'
            Right () -> s'

cons :: CodeQ a -> Stream a m r -> Stream a m r
cons ca s = S [|| False ||] next after
    where
        next cb = do
            b <- split cb
            if not b then yield ca [||True||] else jump ()
        after () _ = s

{-
COMBINATORS
-}

mapC :: (CodeQ a -> CodeQ b) -> Stream a m r -> Stream b m r
mapC f (S cx0 next after) = S cx0 next' (\j cs -> mapC f (after j cs))
    where
        next' x = next x >>= \case
                    Jump j -> return (Jump j)
                    Effect cmx' -> return (Effect cmx')
                    Tau cx' -> return (Tau cx')
                    Yield ca cx' -> return (Yield (f ca) cx')
                    Done cr -> return (Done cr)

{-
ELIMINATORS
-}
foldC :: (Monad m)  => (CodeQ x -> CodeQ a -> CodeQ x) -> CodeQ x -> (CodeQ x -> CodeQ b) -> Stream a m r -> CodeQ (m (b,r))
foldC step begin done (S kcx0 next _) = unGen kcx0 $ \cx0 -> [|| do
    let loop !acc !x = $$(unGen (next [|| x ||]) $ \case
            Effect cmx' -> [|| $$(cmx') >>= loop acc ||]
            Tau cx' -> [|| loop acc $$cx' ||]
            Yield ca cx' -> [|| loop $$(step [|| acc ||] ca) $$cx' ||]
            Done cr -> [|| return ($$(done [|| acc ||]),$$cr) ||]
         )
    loop $$begin $$cx0
 ||]


end :: (Monad m) => Stream a m r -> CodeQ (m r)
end s = [|| (snd <$> $$(foldC (\_ _ -> [|| () ||]) [|| () ||] (const [|| () ||]) s)) ||]

drain :: (Monad m) => Stream a m r -> CodeQ (m ())
drain s = [|| (Control.Monad.void $$(foldC (\_ _ -> [|| () ||]) [|| () ||] (const [|| () ||]) s))||]