bracketChecker str = bracketHelper str []

bracketHelper :: String -> [Int] -> Bool
bracketHelper (char:other) stack
    | char == '(' = bracketHelper other (1:stack)
    | char == '[' = bracketHelper other (2:stack)
    | char == '{' = bracketHelper other (3:stack)
    | otherwise   = 
        if length stack /= 0 && (
            char == ')' && head stack == 1 ||
            char == ']' && head stack == 2 ||
            char == '}' && head stack == 3
            ) then bracketHelper other (tail stack)
        else
            False

bracketHelper "" stack = (length stack) == 0