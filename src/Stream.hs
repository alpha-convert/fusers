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
    Jump :: j -> CodeQ s -> Step a m r s j
    Effect :: CodeQ (m s) -> Step a m r s j
    Tau :: CodeQ s -> Step a m r s j
    Yield :: CodeQ a -> CodeQ s -> Step a m r s j
    Done :: CodeQ r -> Step a m r s j

yield :: MonadGen m' => CodeQ a -> CodeQ s -> m' (Step a m r s j)
yield ca cs = return (Yield ca cs)

jump :: MonadGen m' => j -> CodeQ s -> m' (Step a m r s j)
jump j cx = return (Jump j cx)

tau :: MonadGen m' => CodeQ s -> m' (Step a m r s j)
tau = return . Tau

done :: MonadGen m' => CodeQ r -> m' (Step a m r s j)
done = return . Done

stateMapC :: Monad m => (CodeQ s -> CodeQ s') -> Step a m r s j -> Step a m r s' j
stateMapC f (Effect cmx) = Effect [|| $$cmx >>= (\s -> return $$(f [||s||])) ||]
stateMapC f (Tau cx) = Tau (f cx)
stateMapC f (Yield ca cx) = Yield ca (f cx)
stateMapC _ (Done cr) = Done cr
stateMapC f (Jump j cx) = Jump j (f cx)

data Stream a m r where
    S :: forall j a m r s. CodeQ s -> (CodeQ s -> Gen (Step a m r s j)) -> (j -> CodeQ s -> Stream a m r) -> Stream a m r

{-
CONSTRUCTION
-}

range :: Int -> Int -> Stream Int m ()
range lo hi = S [|| lo ||] next absurd
    where
        next ca = do
            b <- split [|| $$ca < hi ||]
            if b then do
                ca' <- genLet [|| succ $$ca ||]
                yield ca' ca' else done [|| () ||]

append :: Stream a m () -> Stream a m () -> Stream a m ()
append (S cx0 next andThen) s' = S cx0 next' andThen'
    where
        next' cx = next cx >>= \case
            Done _ -> return (Jump (Right ()) cx)
            Yield ca cx' -> return (Yield ca cx')
            Tau cx' -> return (Tau cx')
            Jump j cx' -> return (Jump (Left j) cx')
            Effect cmx' -> return (Effect cmx')
        andThen' j cx' = case j of
            Left j0 -> append (andThen j0 cx') s'
            Right () -> s'

cons :: CodeQ a -> Stream a m r -> Stream a m r
cons ca s = S [|| False ||] next after
    where
        next cb = do
            b <- split cb
            if not b then yield ca [||True||] else jump () cb {- this is a bit strange, jumping requires a state (sometimes). this was nice for various reasons, but it's also weird when you jump off of a Done. -}
        after () _ = s

{-
COMBINATORS
-}

drop :: CodeQ Int -> Stream a m r -> Stream a m r
drop cn s = S cn dropN (\() _ -> s)
    where
        dropN cn = do
            b <- split [|| $$cn > 0 ||]
            if b then tau [|| $$cn - 1 ||] else jump () [||0||]

interleave :: Stream a m () -> Stream a m () -> Stream a m ()
interleave = _

mapC :: (CodeQ a -> CodeQ b) -> Stream a m r -> Stream b m r
mapC f (S cx0 next after) = S cx0 next' (\j cs -> mapC f (after j cs))
    where
        next' x = next x >>= \case
                    Jump j cx' -> return (Jump j cx')
                    Effect cmx' -> return (Effect cmx')
                    Tau cx' -> return (Tau cx')
                    Yield ca cx' -> return (Yield (f ca) cx')
                    Done cr -> return (Done cr)

{-
ELIMINATORS
-}
foldC :: (Monad m)  => (CodeQ x -> CodeQ a -> CodeQ x) -> CodeQ x -> (CodeQ x -> CodeQ b) -> Stream a m r -> Gen (CodeQ (m (b,r)))
foldC step begin finalize (S cx0 next after) = do
    loop <- genLetRec $ \loopRec -> do
        genLam $ \cacc -> genLam $ \cx -> do
            st <- next cx
            case st of
                Effect cmx' -> return [|| $$cmx' >>= $$loopRec $$cacc ||]
                Tau cx' -> return [|| $$loopRec $$cacc $$cx' ||]
                Yield ca cx' -> do
                    cacc' <- genLet (step cacc ca)
                    return [|| $$loopRec $$cacc' $$cx' ||]
                Done cr -> return [|| return ($$(finalize cacc),$$cr) ||]
                Jump j cx' -> foldC step cacc finalize (after j cx')
    return [|| $$loop $$begin $$cx0 ||]

end :: (Monad m) => Stream a m r -> CodeQ (m r)
end s = [|| (snd <$> $$(runGen $ foldC (\_ _ -> [|| () ||]) [|| () ||] (const [|| () ||]) s)) ||]

drain :: (Monad m) => Stream a m r -> CodeQ (m ())
drain s = [|| (Control.Monad.void $$(runGen $ foldC (\_ _ -> [|| () ||]) [|| () ||] (const [|| () ||]) s))||]

printCode c = ppr <$> runQ (unTypeCode c)