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
let rotation = [a, b, c, d, e, f, g, h, i , j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z]

encryptVigenere :: [Char] -> [Char] -> [Char]
encryptVigenere secret word = map ...........
