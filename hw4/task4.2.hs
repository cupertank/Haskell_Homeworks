program :: IO ()
program = loop []

loop :: [Int] -> IO ()
loop x = do
    line <- getLine
    let command = map read (words line) :: [Int]
    case command of
        (0:_)          -> return ()
        (1:toInput:_)  -> loop (add x toInput)
        (2:toRemove:_) -> loop (remove x toRemove)
        (3:_)          -> do
                          print x
                          loop x
        _              -> loop x

add :: [Int] -> Int -> [Int]
add [] num = [num]
add (fir:list) num = if num >= fir then num:fir:list else fir:add list num

remove :: [Int] -> Int -> [Int]
remove [] _ = []
remove (fir:list) num = if num == fir then list else fir:remove list num