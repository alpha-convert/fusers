{-#LANGUAGE TemplateHaskell #-}
module FusersBm (fusers_sum,fusers_sum',ifusers_sum') where

import Language.Haskell.TH
import Stream as S
import ImpStream as I

evenC :: Code Q Int -> Code Q Bool
evenC cx = [|| $$cx `mod` 2 == 0 ||]

plusOneC :: Code Q Int -> Code Q Int
plusOneC cx = [|| $$cx + 1 ||]

fusers_sum :: Code Q (IO Int)
fusers_sum = S.sumC (S.filterC evenC (S.mapC plusOneC (S.drop 1000 (S.mapC plusOneC (S.filterC evenC (S.fromListC [||[1..1000000]||]))))))

fusers_sum' :: Code Q (IO Int)
fusers_sum' = S.sumC (S.filterC evenC (S.mapC plusOneC (S.drop 1000 (S.mapC plusOneC (S.filterC evenC (S.range 1 1000000))))))

-- ifusers_sum :: Code Q (IO Int)
-- ifusers_sum = I.sumC (I.filterC evenC (I.mapC plusOneC (I.drop 1000 (I.mapC plusOneC (I.filterC evenC (I.fromListC [||[1..1000000]||]))))))

ifusers_sum' :: Code Q (IO Int)
ifusers_sum' = I.sumC (I.filterC evenC (I.mapC plusOneC (I.drop 1000 (I.mapC plusOneC (I.filterC evenC (I.range 1 1000000))))))


