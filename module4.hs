{--
== Contact Organizer == June 2021
This program let's you manage contacts. 
Contacts get save to disk on every quit.
Providing CRUD operations and nicely prints for contacts.

Every Contact can have:
- First Name
- Last Name
- Street
- Street Number
- ZIP
- City
- Phone

--}


-- DATA DEFINITION
data Contact = Contact { firstName :: String,
                lastName :: String,
                aStreet :: String,
                aNumber :: String,
                aZip :: String,
                city :: String,
                phone :: String
                }

instance Show Contact where
   show a = "First Name: " ++ firstName a ++ "\n" ++
            "Last Name: " ++ lastName a ++ "\n" ++
            "Street: "  ++ aStreet a ++ "\n" ++
            "Street Number: "  ++ aNumber a ++ "\n" ++
            "ZIP: "  ++ aZip a ++ "\n" ++
            "City: "  ++ city a ++ "\n" ++
            "Phone: " ++  phone a ++ "\n"

-- One line String representation of a Contact
stringifyContact :: Contact -> String
stringifyContact a = firstName a ++ "," ++
            lastName a ++ "," ++
            aStreet a ++ "," ++
            aNumber a ++ "," ++
            aZip a ++ "," ++
            city a ++ "," ++
            phone a

showContacts :: [Contact] -> IO()
showContacts contacts =
        putStrLn "ID: First Name, Last Name, Street, Street Number, ZIP, City, Phone"
        >>
        if length contacts == 0 
                then putStrLn "No contacts!" >> start contacts 
        else mapM_ (\x -> print (show (fst x) ++ ": " ++ stringifyContact (snd x))) (zip [0 ..] contacts) >> start contacts
        

-- loadContacts
loadContactsAsList ::IO()
loadContactsAsList = do
            contents <- readFile "./contacts.txt"
            let linesOfFiles = lines contents
            let contacts = [createContactByList (wordsWhen (==',') x) | x <- linesOfFiles]
            if null contacts then start [] else start contacts

-- Create Contact data type by list of attributes
createContactByList :: [String] -> Contact
createContactByList x = Contact (head x) (x !! 1) (x !! 2) (x !! 3) (x !! 4) (x !! 5) (x !! 6)

-- stringifyContacts (executed at quit)
stringifyContacts :: [Contact] -> IO()
stringifyContacts contacts = do
        let tostringify = map stringifyContact contacts
        let content = unlines tostringify
        writeFile "./contacts.txt" content

-- Create a new contact in memory
addContact :: [Contact] -> IO ()
addContact contacts = do
        putStrLn "Please Enter new Contact details:"
        putStrLn "First Name, Last Name, Street, Street Number, ZIP, City, Phone"
        input <- getLine
        let x = wordsWhen(==',') input
        let newContact = createContactByList x
        if length x == 7 then start (contacts ++ [newContact]) else putStrLn "Wrong input format!" >> start contacts

-- Nicely print a contact by ID
printContact :: [Contact] -> IO ()
printContact contacts = do
        putStrLn "Please Enter ID of contact"
        id <- readLn
        print (contacts !! (id :: Int))
        start contacts

-- Find a contact by first name and print
findByFirstName :: [Contact] -> IO ()
findByFirstName contacts = do
        putStrLn "Please Enter First Name"
        fName <- getLine
        let matches = filter (\x -> uppercase (firstName x) == uppercase fName) contacts
        if null matches then putStrLn "nothing found." else putStrLn "I found:" >> mapM_ print matches
        start contacts

-- Find a contact by last name and print
findByLastName :: [Contact] -> IO ()
findByLastName contacts = do
        putStrLn "Please Enter Last Name"
        fName <- getLine
        let matches = filter (\x -> uppercase (lastName x) == uppercase fName) contacts
        if null matches then putStrLn "nothing found." else putStrLn "I found:" >> mapM_ print matches
        start contacts

-- Delete a contact by ID
deleteById :: [Contact] -> IO ()
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
editContact :: [Contact] -> IO ()
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
                                let oldContact = contacts !! (id :: Int)
                                mapM_ putStr ["First Name [", firstName oldContact, "]: "]
                                nName <- getLine
                                mapM_ putStr ["Last Name [", lastName oldContact, "]: "]
                                nLastName <- getLine
                                mapM_ putStr ["Street [", aStreet oldContact, "]: "]
                                nStreet <- getLine
                                mapM_ putStr ["Street Number [", aNumber oldContact, "]: "]
                                nStreetNumber <- getLine
                                mapM_ putStr ["ZIP [", aZip oldContact, "]: "]
                                nZip <- getLine
                                mapM_ putStr ["City [", city oldContact, "]: "]
                                nCity <- getLine
                                mapM_ putStr ["Phone [", phone oldContact, "]: "]
                                nPhone <- getLine
                                let newContact = Contact{firstName = if null nName then firstName oldContact else nName,
                                                       lastName = if null nLastName then lastName oldContact else nLastName,
                                                       aStreet = if null nStreet then aStreet oldContact else nStreet,
                                                       aNumber = if null nStreetNumber then aNumber oldContact else nStreetNumber,
                                                       aZip = if null nZip then aZip oldContact else nZip,
                                                       city = if null nCity then city oldContact else nCity,
                                                       phone = if null nPhone then phone oldContact else nPhone
                                                }
                                let newContacts = replaceNthContact (id :: Int) newContact contacts
                                start newContacts
                        else start contacts

-- Main Loop function --
start :: [Contact] -> IO()
start contacts = do
    putStrLn "Please choose an Option below:"
    putStrLn "S = Show all contacts"
    putStrLn "P = Print contact by ID"
    putStrLn "A = Add new contact"
    putStrLn "D = Delete Contact by ID"
    putStrLn "E = Edit Contact by ID"
    putStrLn "X = Search by first name"
    putStrLn "Y = Search by last name"
    putStrLn "Q = stringify and Quit"
    choice <- getLine
    processMenu choice contacts

-- Valid main menu choices
mainChoices :: String
mainChoices = ['S', 's', 'A', 'a', 'X', 'x', 'Y', 'y', 'Q', 'q']

-- Process user main menu choice
processMenu :: String -> [Contact] -> IO()
processMenu choice contacts | choice == "S" || choice == "s" = putStrLn "== Showing all contacts ==" >> showContacts contacts
                            | choice == "A" || choice == "a" = addContact contacts
                            | choice == "E" || choice == "e" = editContact contacts
                            | choice == "Q" || choice == "q" = putStrLn "Good bye!" >> stringifyContacts contacts
                            | choice == "D" || choice == "d" = deleteById contacts
                            | choice == "X" || choice == "x" = findByFirstName contacts
                            | choice == "Y" || choice == "y" = findByLastName contacts
                            | choice == "P" || choice == "p" = printContact contacts
                            | otherwise = putStrLn ("Unknown command " ++ choice) >> main


-- helper functions --

-- Split Strings by delimiter, inspired by built in 'words' function:
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'


-- Change a Contact/contact in place
replaceNthContact :: Int -> Contact -> [Contact] -> [Contact]
replaceNthContact _ _ [] = []
replaceNthContact n newContact (x:xs)
   | n == 0 = newContact:xs
   | otherwise = x:replaceNthContact (n-1) newContact xs

-- Remove entry from list a given position
deleteAt :: Int -> [a] -> [a]
deleteAt idx xs = lft ++ rgt
  where (lft, _:rgt) = splitAt idx xs

-- Make a string uppercase
uppercase :: String -> String
uppercase = map (\c -> if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c)

main = loadContactsAsList