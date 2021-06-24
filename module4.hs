import Data.String

-- Contact organizer

-- DATA DEFINITION
data Person = Person { firstName :: String,
                lastName :: String,
                aStreet :: String,
                aNumber :: String,
                aZip :: String,
                city :: String,
                phone :: String
                }

instance Show Person where
   show a = "First Name: " ++ firstName a ++ "\n" ++
            "Last Name: " ++ lastName a ++ "\n" ++
            "Street: "  ++ aStreet a ++ "\n" ++
            "Street Number: "  ++ aNumber a ++ "\n" ++
            "ZIP: "  ++ aZip a ++ "\n" ++
            "City: "  ++ lastName a ++ "\n" ++
            "Phone: " ++  phone a ++ "\n"

savePerson a = firstName a ++ "," ++
            lastName a ++ "," ++
            aStreet a ++ "," ++
            aNumber a ++ "," ++
            aZip a ++ "," ++
            city a ++ "," ++
            phone a

printPerson a = firstName a ++ ", " ++
            lastName a ++ ", " ++
            aStreet a ++ ", " ++
            aNumber a ++ ", " ++
            aZip a ++ ", " ++
            city a ++ ", " ++
            phone a

showContacts :: [Person] -> IO()
showContacts contacts =
        putStrLn "ID: First Name, Last Name, Street, Street Number, ZIP, City, Phone"
        >>
        mapM_ (\x -> print ((show (fst x)) ++ ": " ++ (printPerson (snd x)))) ((zip [0 ..]) contacts)
        >> start contacts

-- loadContacts
loadContactsAsList ::IO()
loadContactsAsList = do
            contents <- readFile "./contacts.txt"
            let linesOfFiles = lines contents
            let contacts = [makeItPerson (wordsWhen (==',') x) | x <- linesOfFiles]
            start contacts

makeItPerson :: [String] -> Person
makeItPerson x = Person (head x) (x !! 1) (x !! 2) (x !! 3) (x !! 4) (x !! 5) (x !! 6)

-- saveContacts
savePersons :: [Person] -> IO()
savePersons contacts = do
        let toSave = map savePerson contacts
        let content = unlines toSave
        writeFile "./contacts.txt" content

-- Create a new contact in memory
addContact :: [Person] -> IO ()
addContact contacts = do
        putStrLn "Please Enter new Contact details:"
        putStrLn "First Name, Last Name, Street, Street Number, ZIP, City, Phone"
        input <- getLine 
        let x = wordsWhen(==',') input
        let newPerson = makeItPerson x
        if length x == 7 then start (contacts ++ [newPerson]) else putStrLn "Wrong input format!" >> start contacts
        
-- Nicely print a contact by ID
printContact :: [Person] -> IO ()
printContact contacts = do
        putStrLn "Please Enter ID of contact"
        id <- readLn
        print (contacts !! (id :: Int))
        start contacts

-- Find a contact by first name and print
findByFirstName :: [Person] -> IO ()
findByFirstName contacts = do
        putStrLn "Please Enter First Name"
        fName <- getLine
        let matches = filter (\x -> firstName x == fName) contacts
        if null matches then putStrLn "nothing found." else putStrLn "I found:" >> mapM_ print matches
        start contacts

-- Find a contact by last name and print
findByLastName :: [Person] -> IO ()
findByLastName contacts = do
        putStrLn "Please Enter Last Name"
        fName <- getLine
        let matches = filter (\x -> lastName x == fName) contacts
        if null matches then putStrLn "nothing found." else putStrLn "I found:" >> mapM_ print matches
        start contacts

-- Main Loop function --
start :: [Person] -> IO()
start contacts = do
    putStrLn "Please choose an Option below:"
    putStrLn "S = Show all contacts"
    putStrLn "P = Print contact by ID"
    putStrLn "A = Add new contact"
    putStrLn "X = Search by first name"
    putStrLn "Y = Search by last name"
    putStrLn "Q = Save and Quit"
    choice <- getLine
    processMenu choice contacts

-- Valid main menu choices --
mainChoices :: [Char]
mainChoices = ['S', 's', 'A', 'a', 'X', 'x', 'Y', 'y', 'Q', 'q']

processMenu :: String -> [Person] -> IO()
processMenu choice contacts | choice == "S" || choice == "s" = putStrLn "== Showing all contacts ==" >> showContacts contacts
                            | choice == "A" || choice == "a" = addContact contacts
                            | choice == "Q" || choice == "q" = putStrLn "Good bye!" >> savePersons contacts
                            | choice == "X" || choice == "x" = findByFirstName contacts
                            | choice == "Y" || choice == "y" = findByLastName contacts
                            | choice == "P" || choice == "p" = printContact contacts
                            | otherwise = putStrLn ("Unknown command " ++ choice) >> main






-- helper functions
-- Split Strings by delimiter, inspired by build in 'words' function:
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
main = loadContactsAsList