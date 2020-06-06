bracketChecker str = bracketHelper str []

bracketHelper :: String -> [Char] -> Bool
bracketHelper (char:other) stack
    | char == '(' ||
      char == '[' ||
      char == '{' = bracketHelper other (char:stack)
    | otherwise = 
        if length stack /= 0 && (char == ')' || char == ']' || char == '}') && head stack == char then
            bracketHelper other (tail stack)
        else
            False

bracketHelper "" stack = (length stack) == 0
