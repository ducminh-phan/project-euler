module PE
    ( getFunc
    ) where

import qualified Data.IntMap.Strict as M
import           PE1                (funcList1)

funcList :: [(Int, Int -> Int)]
funcList = funcList1

funcMap :: M.IntMap (Int -> Int)
funcMap = M.fromList funcList

getFunc :: Int -> Maybe (Int -> Int)
getFunc funcId = M.lookup funcId funcMap
