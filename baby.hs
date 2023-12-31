doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100 then x else x*2

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

conanO'Brien = "It's a-me, Conan O'Brien!"

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

-- length' xs = sum [1 | _ <- xs]  -- using list comprehension

removeNonUppercase :: String -> String
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10]]

rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]

rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

--factorial :: Integer -> Integer
--factorial n = product [1 .. n]


-- Pattern Matching
circumference :: Double -> Double
circumference r = 2 * pi * r

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck pal"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- add vectors without and with pattern Matching
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
-- addVectors a b = (fst a + fst b, snd a + snd b)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


-- get the first, second, or third from a triple
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x                              -- (x: []) could be rewritten as [x]
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y      -- (x:y:[]) could be rewritten as [x, y]
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y -- (x:y:_) has to be written this way because it could be any length


length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs


-- using patterns
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of" ++ all ++ " is " ++ [x]

-- Guards
bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise   = "You're a whale, congratulations!"

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise   = "You're a whale, congratulations!"

bmiTell'' :: (RealFloat a) => a -> a -> String
bmiTell'' weight height
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise   = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b = GT
    | a == b = EQ
    | otherwise = LT

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

capital' :: String -> String
capital' st = head [cap]
    where cap = [c | c <- st, c `elem` ['A'..'Z']]

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r^2
    in sideArea + 2 * topArea

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w/h^2]

-- case expressions
-- the following functions do the exact same thing

head'' :: [a] -> a
head'' [] = error "No head for empty lists!"
head'' (x:_) = x

head''' :: [a] -> a
head''' xs = case xs of [] -> error "No head for empty lists!"
                        (x:_) -> x

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."

-- Recursion
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs


maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "maximum of empty list"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum'' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0    = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = elem' a xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

-- compareWithHundred :: (Num a, Ord a) => a -> Ordering
-- compareWithHundred x = compare 100 x

-- we do not need x because these functions are curried
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

-- infix functions can also be partially applied
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

-- this function takes a function and then applies it twice to something
-- the parenthesis is mandatory
-- ex: ghci> applyTwice (++ " HAHA") "HEY"
applyTwice :: (a -> a) -> a -> a 
applyTwice f x = f (f x)

-- ex: ghci> zipWith' (+) [4,2,5,6] [2,6,2,3]
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith f xs ys


-- flip' :: (a -> b -> c) -> (b -> a -> c)
-- flip' f = g
--     where g x y = f y x

-- this is the same as the function above
flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

-- map function definition
-- ex: ghci> map' (map' (^2)) [[1,2],[3,4,5,6],[7,8]] 
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs

-- filter function definition
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs


-- quicksort done using the filter function rather than list comprehension
quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = 
    let smallerSorted = quicksort' (filter' (<=x) xs)
        biggerSorted = quicksort' (filter' (>x) xs)
    in smallerSorted ++ [x] ++ biggerSorted


-- find the largest number under 100,000 that's divisible by 3829
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000, 99999..])
    where p x = x `mod` 3829 == 0


-- get the sum of all quiares that are smaller than 10000
-- sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
-- the same thing written with list comprehension
-- sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n  = n:chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map' chain [1..100]))
    where isLong xs = length xs > 15


-- using lambdasa
numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

