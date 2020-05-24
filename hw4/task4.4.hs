import System.IO

data Row = Row { name :: String, number :: String }
type Phonebook = [Row]

program = loop []

loop :: Phonebook -> IO ()
loop x = do
    line <- getLine
    let command = words line
    case command of 
        ("0":_) -> return ()
        ("1":inputName:inputNumber:_) -> loop (Row inputName inputNumber:x)
        ("2":inputName:_)   -> do
                               let phone = findPhoneByName x inputName
                               print phone
                               loop x
        ("3":inputNumber:_) -> do
                               let name = findNameByPhone x inputNumber
                               print name
                               loop x
        ("4":path:_)        -> do
                               phoneBook <- loadFromFile path
                               loop phoneBook
        ("5":path:_)        -> do 
                               saveToFile path x
                               loop x

        _                   -> loop x

findPhoneByName :: Phonebook -> String -> String
findPhoneByName [] _ = "Not found"
findPhoneByName (row:other) inputName = if name row == inputName then 
                                            number row 
                                        else 
                                            findPhoneByName other inputName


findNameByPhone :: Phonebook -> String -> String
findNameByPhone [] _ = "Not Found"
findNameByPhone (row:other) inputPhone = if number row == inputPhone then
                                            name row
                                         else 
                                            findNameByPhone other inputPhone


loadFromFile :: String -> IO Phonebook
loadFromFile path = do
                    content <- readFile path
                    let inputLines = lines content
                    parser inputLines []

parser :: [String] -> Phonebook -> IO Phonebook
parser [] output = return output
parser (line:other) output = do
                             let (name:phone:_) = words line
                             parser other (Row name phone:output)



saveToFile :: String -> Phonebook -> IO ()
saveToFile path phoneBook = do
                            file <- openFile path WriteMode
                            saveHelper file phoneBook

saveHelper :: Handle -> Phonebook -> IO ()
saveHelper _ [] = return ()
saveHelper file (Row name number:other) = do
                                          hPutStrLn file (name ++ " " ++ number)
                                          saveHelper file other