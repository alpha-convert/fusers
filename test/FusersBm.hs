{-#LANGUAGE TemplateHaskell #-}
module FusersBm (fusers_sum,fusers_sum') where

import Language.Haskell.TH
import Stream as S

evenC :: Code Q Int -> Code Q Bool
evenC cx = [|| $$cx `mod` 2 == 0 ||]

plusOneC :: Code Q Int -> Code Q Int
plusOneC cx = [|| $$cx + 1 ||]

fusers_sum :: Code Q (IO Int)
fusers_sum = S.sumC (S.filterC evenC (S.mapC plusOneC (S.drop 1000 (S.mapC plusOneC (S.filterC evenC (S.fromListC [||[1..1000000]||]))))))

fusers_sum' :: Code Q (IO Int)
fusers_sum' = S.sumC (S.filterC evenC (S.mapC plusOneC (S.drop 1000 (S.mapC plusOneC (S.filterC evenC (S.range 1 1000000))))))

