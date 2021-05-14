{--
Content:

> List / Data functions
> FizzBuzz
> Fibonacci
> Primes
> Prime factors

--}


-- Own implementations of useful list and data structure functions
myHead :: [a] -> a
myHead (x:_) = x
myHead [] = error "empyt list"

myTail :: [a] -> [a]
myTail (a:ax) = ax
myTail [] = error "empty list"

myLast :: [a] -> a
myLast (a:ax) = if null ax then a else myLast ax
myLast [] = error "empty list"

myInit :: [a] -> [a]
myInit [a] = []
myInit (a:ax) = a : myInit ax
myInit [] = error "empty list"

myReverse :: [a] -> [a]
myReverse [a] = [a]
myReverse (a:ax) = myReverse(ax) ++ [a]


-- FizzBuzz
fizzBuzz :: (Integral a, Show a) => a -> [String]
fizzBuzz x = [makeItFizzBuzz a | a <- [1..x]]
    where makeItFizzBuzz a | a `mod` 15 == 0 = "FizzBuzz"
                 | a `mod` 5 == 0 = "Buzz"
                 | a `mod` 3 == 0 = "Fizz"
                 | a >= 1 = show(a)

-- Fibonacci 
fibonacci :: Num a => [a]  
fibonacci = [fib a | a <- [0..]]
    where fib x | x == 0 = 0
                | x == 1 = 1
                | x > 1 = fib (x - 1) + fib (x - 2)

-- Primes
primes :: (Integral a) => [a]
primes = [a | a <- [0..], isPrime a]

isPrime :: (Integral a) => a -> Bool
isPrime a | a == 0 = False
          | a == 1 = False 
          | a > 1 = if length ( [y | y <- [2 .. a], mod a y == 0] ) > 1 then False else True
                                                        -- floor(sqrt(b)) would be better?

-- Prime factors
primeFactors :: Integral a => a -> [a]
primeFactors n = factor n primes
    where
        factor n (p:ps)
                | p*p > n        = [n]
                | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
                | otherwise      = factor n ps
