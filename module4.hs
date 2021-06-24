-- Contact organizer

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

savePerson a = firstName a ++ " " ++
            lastName a ++ " " ++
            aStreet a ++ " " ++
            aNumber a ++ " " ++
            aZip a ++ " " ++
            city a ++ " " ++
            phone a

-- CRUD

-- Create a new person

-- Read all persons

showContacts :: [Person] -> IO()
showContacts contacts =
        putStrLn "ID, First Name, Last Name, Street, Street Number, ZIP, City, Phone"
        >>
        mapM_ (\x -> print ((show (fst x)) ++ " " ++ (savePerson (snd x)))) ((zip [0 ..]) contacts)
        >> start contacts




makeItPerson :: [String] -> Person
makeItPerson x = Person (x !! 0) (x !! 1) (x !! 2) (x !! 3) (x !! 4) (x !! 5) (x !! 6)              

loadContactsAsList ::IO()
loadContactsAsList = do
            contents <- readFile "./contacts.txt"
            let linesOfFiles = lines contents
            let contacts = [makeItPerson (words x) | x <- linesOfFiles]
            start contacts

savePersons = do
        let person1 = Person "Theodor" "Tester" "TestStrasse" "47" "12345" "Hamburg" "1233424324"
        let person2 = Person "Tatja" "Testerin" "Strasse" "34" "12432" "Berlin" "213432432"
        let persons = [savePerson person1, savePerson person2]
        let content = unlines persons
        writeFile "./contacts.txt" content

-- addContactInput :: IO()
-- addContactInput = do
--     putStrLn "Please Enter First Name:"
--     fName <- getLine
--     putStrLn "Please Enter Last Name:"
--     lName <- getLine
--     putStrLn "Please Enter Street:"
--     street <- getLine
--     putStrLn "Please Enter Street Number:"
--     streetNumber <- getLine
--     putStrLn "Please Enter ZIP:"
--     lzip <- getLine
--     putStrLn "Please Enter City:"
--     city <- getLine
--     putStrLn "Please Enter Number:"
--     number <- getLine

--     addContact oldContacts ++ (Person fName lName street streetNumber lzip city number)


start :: [Person] -> IO()
start contacts = do
    putStrLn "Please choose an Option below:"
    putStrLn "S = Show all contacts"
    putStrLn "A = Add new contact"
    putStrLn "X = Search by first name"
    putStrLn "Y = Search by last name"
    putStrLn "Q = Save and Quit"
    choice <- getLine
    processMenu choice contacts


mainChoices :: [Char]
mainChoices = ['S', 's', 'A', 'a', 'X', 'x', 'Y', 'y', 'Q', 'q']

processMenu :: String -> [Person] -> IO()
processMenu choice contacts | choice == "S" || choice == "s" = putStrLn "== Showing all contacts ==" >> showContacts contacts
                            | choice == "Q" || choice == "q" = putStrLn "Good bye!"
                            | otherwise = putStrLn ("Unknown command " ++ choice) >> main


-- loadContacts

-- saveContacts

main = loadContactsAsList