import Control.Monad.Zip
import Data.Maybe


program :: Ord a => [a] -> Maybe a
program mas = listToMaybe
    (mzip3 mas (drop 1 mas) (drop 2 mas) >>= \(left, mid, right) -> if (mid > left && mid > right) then [mid] else [])


mzip3 xs ys zs = mzipWith (\(x, y) z -> (x, y, z)) (mzip xs ys) zs