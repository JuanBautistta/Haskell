--PRIMEROS PASOS--

doubleMe x = x + x

doubleUs x y = x*2 + y*2

doubleUs2 x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100 then x else x*2

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

conanO'Brien = "It's a-me, Conan O'Brien!"

numbers = [3,2,5,2]

boomBangs xs = [if x<10 then "boom" else "bang" | x<-xs ,odd x]

length' xs = sum [ 1 | _ <- xs]

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase xs = [ x | x<-xs ,elem x ['A'..'Z'] ]

addThree :: Int -> Int -> Int -> Int 
addThree a b c = a+b+c 

factorial :: Integer -> Integer 
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

--SINTAX IN FUNCTION--

lucky :: (Integral a) => a -> String
lucky 7 = "lucky Number seven!!"
lucky x = "sorry, you're out of luck,pal"

sayMe :: (Integral a) => a -> String
sayMe 1 = " One "
sayMe 2 = " Two "
sayMe 3 = " Three "
sayMe 4 = " Four "
sayMe 5 = " Five "
sayMe _ = "Not between 1 and 5"

factorial2 :: (Integral a) => a -> a 
factorial2 0 = 1
factorial2 x = x * factorial2 (x - 1)

charName :: Char -> String
charName 'a' = "Alberto"
charName 'b' = "Berenice"
charName 'c' = "Cristal"

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors' (x1,y1) (x2,y2) = (x1+x2,y1+y2)

first :: (a,b,c) -> a 
first (x,_,_) = x 

second :: (a,b,c) -> b 
second (_,y,_) = y 

third :: (a,b,c) -> c 
third (_,_,z) = z

head' :: [a] -> a
head' [] = error "can't call head on an empty list"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element " ++ show x
tell (x:y:[]) = "The list has 2 elements " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

length2 ::(Integral b) => [a] -> b
length2 [] = 0
length2 (_:xs) = 1 + length2 xs

sum' :: (Num a) =>[a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs 

capital :: String -> String
capital "" = "Empty String"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

--Guards--

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi 
    | bmi <= 18.5 = "You're underweight"
    | bmi <= 25.0 = "You're supposedly normal"
    | bmi <= 30.0 = "You're fat"
    | otherwise = "You're a whale"

bmiTell2 :: (RealFloat a) => a -> a -> String
bmiTell2 weight height 
    | weight / height ^ 2 <= 18.5 = "You're underweight"
    | weight / height ^ 2 <= 25 = "You're supposedly normal"
    | weight / height ^ 2 <= 30 = "You're fat"
    | otherwise = "You're a whale"

max' :: (Ord a) => a -> a -> a 
max' a b 
    |a > b = a
    |otherwise = b 

max2 :: (Ord a) => a -> a -> a
max2 a b | a>b = a | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b 
    | a > b = GT 
    | a == b = EQ 
    | otherwise = LT   

bmiTell3 :: (RealFloat a) => a -> a -> String
bmiTell3 weight height
    | bmi <= 18.5 = "You're underweight"
    | bmi <= 25.0 = "You're supposedly normal"
    | bmi <= 30.0 = "You're fat"
    | otherwise = "You're a whale"
    where bmi = weight/height ^ 2 

bmiTell4 :: (RealFloat a) => a -> a -> String 
bmiTell4 weight height
    | bmi <= skynny = "You're underweight"
    | bmi <= normal = "You're supposedly normal"
    | bmi <= fat = "You're fat"
    | otherwise = "You're a whale"
    where bmi = weight/height ^ 2
          skynny = 18.5 
          normal = 25 
          fat = 30

bmiTell5 :: (RealFloat a) => a -> a -> String
bmiTell5 weight height
    | bmi <= skynny = "You're underweight"
    | bmi <= normal = "You're supposedly normal"
    | bmi <= fat = "You're fat"
    | otherwise = "You're a whale"
    where bmi = weight/height ^ 2
          (skynny,normal,fat) = (18.5,25,30)

initials :: String -> String -> String 
initials firstName lastName = [f] ++ ". " ++ [l] ++ "."
    where(f:_) = firstName
         (l:_) = lastName

calcBmis :: (RealFloat a) => [(a,a)] -> [a] 
calcBmis xs = [bmi w h | (w,h) <- xs]
    where bmi weight height = weight/height^2

cylinder :: (RealFloat a) => a -> a -> a 
cylinder r h = 
    let sideArea = 2* pi *r*h
        topArea = pi*r^2
    in sideArea + 2 * topArea 

calcBmis2 :: (RealFloat a) => [(a,a)] -> [a]
calcBmis2 xs = [bmi | (w,h) <- xs,let bmi = w / h^2,bmi >= 25]

head2 :: [a] -> a 
head2 [] = error "No head for empty lists"
head2 (x:_) = x

head3 :: [a] -> a 
head3 xs = case xs of [] -> error "No head for empty lists"
                      (x:_) -> x
                
describeList :: [a] -> String 
describeList xs = "The list is " ++ what xs 
    where what []  = "empty" 
          what [x] = "a singleton list"
          what xs  = "a longer list"

-------RECURSION--------

maximum' ::  (Ord a) => [a] -> a 
maximum' [] = error "maximum of empty list"
maximum' [x] = x 
maximum' (x:xs)
    | x > maxTail = x 
    | otherwise = maxTail
    where maxTail = maximum' xs

maximum2 :: (Ord a) => [a] -> a 
maximum2 [] = error "maxmimun of empty list"
maximum2 [x] = x 
maximum2 (x:xs) = max x (maximum2 xs)

replicate' :: (Num i,Ord i) => i -> a -> [a]
replicate' n x 
    | n<= 0 = []
    |otherwise = x:replicate' (n-1) x

take' :: (Num i,Ord i) => i -> [a] -> [a]
take' _ [] = [] 
take' x _ 
    | x <= 0 = [] 
take' n (x:xs) = x: take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x  = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False 
elem' a (x:xs)
    | x==a = True 
    | otherwise  elem' a xs