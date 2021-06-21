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

showContacts :: IO ()
showContacts =  
        putStrLn "First Name, Last Name, Street, Street Number, ZIP, City, Phone" 
        >> do 
            contents <- readFile "./contacts.txt"
            putStrLn contents
            >> do
                user <- getChar 
                main


loadContactsAsList :: IO [String]
loadContactsAsList = do 
            contents <- readFile "./contacts.txt"
            let linesOfFiles = lines contents
            return linesOfFiles
            


savePersons = do 
        let person = Person "Theodor" "Tester" "TestStrasse" "47" "12345" "Hamburg" "1233424324"
        let persons = [savePerson person]
        let content = unlines persons
        writeFile "./contacts.txt" content
        


start :: IO ()
start = do
    putStrLn "Please choose an Option below:"
    putStrLn "S = Show all contacts"
    putStrLn "A = Add new contact"
    putStrLn "X = Search by first name"
    putStrLn "Y = Search by last name"
    putStrLn "Q = Save and Quit"
    choice <- getChar
    processMenu choice

mainChoices :: [Char]
mainChoices = ['S', 's', 'A', 'a', 'X', 'x', 'Y', 'y', 'Q', 'q']

processMenu choice | choice `notElem` mainChoices = putStrLn "Illegal choice!" >> main
                   | choice == 'Q' || choice == 'q' = putStrLn "Good bye!"
                   | choice == 'S' || choice == 's' = putStrLn "== Showing all contacts ==" >> showContacts -- vorher speichern :)
                   | otherwise = putStrLn ("command was" ++ [choice]) >> main


-- loadContacts

-- saveContacts

main = start