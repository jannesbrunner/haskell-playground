import Data.Char
{--
Content:

> Casar Chiffre encoding / decoding
> Vignere Chiffre encoding / decoding

--}

-- Casar-Chiffre (shifting in ASCII Table)
encryptCaesar :: Int -> [Char] -> [Char]
encryptCaesar key = map (\x -> toEnum $ fromEnum x + key)

decryptCaesar :: Int -> [Char] -> [Char]
decryptCaesar key = map (\x -> toEnum $ fromEnum x - key)

-- Vignere Chiffre (bound to a=0 .. z=25)

asciiCapitalLetter x = 65 <= x && x <= 90
asciiSmallLetter x = 97 <= x && x <= 122
toSmallLetter x = if asciiSmallLetter x then x else x + 32
toAlphaIndex a = if asciiSmallLetter (fromEnum a) then (fromEnum a) - 97 else (fromEnum a) - 65


encryptVigenere :: [Char] -> [Char] -> [Char]
encryptVigenere word key = [encrypt x y | (x, y) <- zip word (cycle key)]

encrypt x y | asciiSmallLetter   (fromEnum x) = toEnum (mod (toAlphaIndex x + toAlphaIndex y) 26 + 97)
            | asciiCapitalLetter (fromEnum x) = toEnum (mod (toAlphaIndex x + toAlphaIndex y) 26 + 65)
            | otherwise = x
decryptVigenere :: [Char] -> [Char] -> [Char]
decryptVigenere word key = [decrypt x y | (x, y) <- zip word (cycle  key)]

decrypt x y | asciiSmallLetter   (fromEnum x) = toEnum (mod (toAlphaIndex x - toAlphaIndex y) 26 + 97)
            | asciiCapitalLetter (fromEnum x) = toEnum (mod (toAlphaIndex x - toAlphaIndex y) 26 + 65)
            | otherwise = x
