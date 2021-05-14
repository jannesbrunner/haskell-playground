{--
Content:

> Casar Chiffre encoding / decoding
> Vignere Chiffre encoding / decoding

--}

-- Casar-Chiffre
encryptCaesar :: Int -> [Char] -> [Char]
encryptCaesar key word = map (\x -> toEnum $ fromEnum x + key) word

decryptCaesar :: Int -> [Char] -> [Char]
decryptCaesar key word = map (\x -> toEnum $ fromEnum x - key) word

-- Vignere Chiffre

asciiCapitalLetter x = 65 <= x && x <= 90  
asciiSmallLetter x = 97 <= x && x <= 122
toSmallLetter x = if asciiSmallLetter x then x else x + 32
toAlphaIndex a = if asciiSmallLetter (fromEnum a) then (fromEnum a) - 97 else (fromEnum a) - 65


encryptVigenere :: [Char] -> [Char] -> [Char]
encryptVigenere word key = [encrypt x y | (x, y) <- zip word (cycle  key)]

encrypt x y | asciiSmallLetter   (fromEnum x) = toEnum (mod (toAlphaIndex x + toAlphaIndex y) 26 + 97)
            | asciiCapitalLetter (fromEnum x) = toEnum (mod (toAlphaIndex x + toAlphaIndex y) 26 + 65)
            | otherwise = x
 
decryptVigenere :: [Char] -> [Char] -> [Char]
decryptVigenere word key = [decrypt x y | (x, y) <- zip word (cycle  key)]

decrypt x y | asciiSmallLetter   (fromEnum x) = toEnum (mod (toAlphaIndex x - toAlphaIndex y) 26 + 97)
            | asciiCapitalLetter (fromEnum x) = toEnum (mod (toAlphaIndex x - toAlphaIndex y) 26 + 65)
            | otherwise = x