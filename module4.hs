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
            "City: "  ++ city a ++ "\n" ++
            "Phone: " ++  phone a ++ "\n"

savePerson :: Person -> String
savePerson a = firstName a ++ "," ++
            lastName a ++ "," ++
            aStreet a ++ "," ++
            aNumber a ++ "," ++
            aZip a ++ "," ++
            city a ++ "," ++
            phone a

printPerson :: Person -> String
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
        mapM_ (\x -> print (show (fst x) ++ ": " ++ printPerson (snd x))) (zip [0 ..] contacts)
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

-- Delete a contact by ID
deleteById :: [Person] -> IO ()
deleteById contacts = do
        putStrLn "Please Enter ID of contact to delete"
        id <- readLn
        if (id :: Int) > length contacts || (id :: Int) < 0 then putStrLn "Invalid ID!" >> start contacts else
                putStrLn "You have chosen:" >>
                print (contacts !! (id :: Int)) >>
                do
                  putStrLn "Delete this contact? (y/n)"
                  choice <- getLine
                  if choice == "y" || choice == "yes"
                          then putStrLn "Deleting contact"
                               >> start (deleteAt (id :: Int) contacts)
                          else start contacts



-- Edit a contact by ID
editContact :: [Person] -> IO ()
editContact contacts = do
        putStr "ID of contact to edit: "
        id <- readLn
        if (id :: Int) > length contacts || (id :: Int) < 0 then putStrLn "Invalid ID!" >> start contacts else
             putStrLn "You have chosen:" >>
             print (contacts !! (id :: Int)) >>
             do
                putStrLn "Edit this contact? (y/n)"
                choice <- getLine
                if choice == "y" || choice == "yes"
                        then putStrLn "Starting Editor" >> do
                                let oldPerson = contacts !! (id :: Int)
                                mapM_ putStr ["First Name [", firstName oldPerson, "]: "]
                                nName <- getLine
                                mapM_ putStr ["Last Name [", lastName oldPerson, "]: "]
                                nLastName <- getLine
                                mapM_ putStr ["Street [", aStreet oldPerson, "]: "]
                                nStreet <- getLine
                                mapM_ putStr ["Street Number [", aNumber oldPerson, "]: "]
                                nStreetNumber <- getLine
                                mapM_ putStr ["ZIP [", aZip oldPerson, "]: "]
                                nZip <- getLine
                                mapM_ putStr ["City [", city oldPerson, "]: "]
                                nCity <- getLine
                                mapM_ putStr ["Phone [", phone oldPerson, "]: "]
                                nPhone <- getLine
                                let newPerson = Person{firstName = if null nName then firstName oldPerson else nName, 
                                                       lastName = if null nLastName then lastName oldPerson else nStreet,
                                                       aStreet = if null nStreetNumber then aStreet oldPerson else nStreetNumber,
                                                       aNumber = if null nStreetNumber then aStreet oldPerson else nStreetNumber,
                                                       aZip = if null nZip then aZip oldPerson else nCity,
                                                       city = if null nCity then city oldPerson else nCity,
                                                       phone = if null nPhone then phone oldPerson else nPhone
                                                }
                                
                                let newContacts = replaceNthPerson (id :: Int) newPerson contacts
                                start newContacts
                        else start contacts

deleteAt :: Int -> [a] -> [a]
deleteAt idx xs = lft ++ rgt
  where (lft, _:rgt) = splitAt idx xs

-- Main Loop function --
start :: [Person] -> IO()
start contacts = do
    putStrLn "Please choose an Option below:"
    putStrLn "S = Show all contacts"
    putStrLn "P = Print contact by ID"
    putStrLn "A = Add new contact"
    putStrLn "D = Delete Contact by ID"
    putStrLn "E = Edit Contact by ID"
    putStrLn "X = Search by first name"
    putStrLn "Y = Search by last name"
    putStrLn "Q = Save and Quit"
    choice <- getLine
    processMenu choice contacts

-- Valid main menu choices --
mainChoices :: String
mainChoices = ['S', 's', 'A', 'a', 'X', 'x', 'Y', 'y', 'Q', 'q']

processMenu :: String -> [Person] -> IO()
processMenu choice contacts | choice == "S" || choice == "s" = putStrLn "== Showing all contacts ==" >> showContacts contacts
                            | choice == "A" || choice == "a" = addContact contacts
                            | choice == "E" || choice == "e" = editContact contacts
                            | choice == "Q" || choice == "q" = putStrLn "Good bye!" >> savePersons contacts
                            | choice == "D" || choice == "d" = deleteById contacts
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

-- Change a person in place
replaceNthPerson :: Int -> Person -> [Person] -> [Person]
replaceNthPerson _ _ [] = []
replaceNthPerson n newPerson (x:xs)
   | n == 0 = newPerson:xs
   | otherwise = x:replaceNthPerson (n-1) newPerson xs