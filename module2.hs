{--
Content:

> Casar Chiffre encoding / decoding
> Vignere Chiffre encoding / decoding

--}

groupFive :: [Char] -> [Char]
groupFive x = if length x > 5 then take 5 x ++ [' '] ++ groupFive (drop 5 x) else x


-- Casar-Chiffre

encryptCaesar :: Int -> [Char] -> [Char]
encryptCaesar key word = groupFive (map (caesarizer True key) (filterWhitespace word))

caesarizer up key x | up && asciiCapitalLetter (fromEnum x) = toEnum (mod (toAlphaIndex x + key) 26 + 65)
                    | up && asciiSmallLetter (fromEnum x) = toEnum (mod (toAlphaIndex x + key) 26 + 97)
                    | not up && asciiCapitalLetter (fromEnum x) = toEnum ( mod (toAlphaIndex x - key) 26 + 65)
                    | not up && asciiSmallLetter (fromEnum x) = toEnum ( mod (toAlphaIndex x - key) 26 + 97)
                    | otherwise = x

decryptCaesar :: Int -> [Char] -> [Char]
decryptCaesar key word = map (caesarizer False key) (filterWhitespace word)


-- Vignere Chiffre (bound to a=0 .. z=25)

asciiCapitalLetter x = 65 <= x && x <= 90
asciiSmallLetter x = 97 <= x && x <= 122
toSmallLetter x = if asciiSmallLetter x then x else x + 32
toAlphaIndex a = if asciiSmallLetter (fromEnum a) then (fromEnum a) - 97 else (fromEnum a) - 65

filterWhitespace :: [Char] -> [Char]
filterWhitespace = filter (\a -> fromEnum a /= 32)

encryptVigenere :: [Char] -> [Char] -> [Char]
encryptVigenere word key = groupFive ([encrypt x y | (x, y) <- zip (filterWhitespace word) (cycle key)])

encrypt x y | asciiSmallLetter   (fromEnum x) = toEnum (mod (toAlphaIndex x + toAlphaIndex y) 26 + 97)
            | asciiCapitalLetter (fromEnum x) = toEnum (mod (toAlphaIndex x + toAlphaIndex y) 26 + 65)
            | otherwise = x
decryptVigenere :: [Char] -> [Char] -> [Char]
decryptVigenere word key = [decrypt x y | (x, y) <- zip (filterWhitespace word) (cycle  key)]

decrypt x y | asciiSmallLetter   (fromEnum x) = toEnum (mod (toAlphaIndex x - toAlphaIndex y) 26 + 97)
            | asciiCapitalLetter (fromEnum x) = toEnum (mod (toAlphaIndex x - toAlphaIndex y) 26 + 65)
            | otherwise = x
