-- example
addTwoInts :: Int -> Int -> Int
addTwoInts x y = x + y

sum_ :: Int -> Int
sum_ n = if n == 0 then 0 else n + sum_ (n-1)

fac :: Int -> Int
fac n = if n == 0 then 1 else n * fac (n-1)

fib :: Int -> Int
fib n = if n == 0 || n == 1 then 1 else fib (n-1) + fib (n-2)

gcd_ :: Int -> Int -> Int
gcd_ m n
  | m >= n = if n == 0 then m else gcd_ n (mod m n)
  | m < n = if m == 0 then n else gcd_ m (mod n m)

max_ :: [Int] -> Int
max_ l = 
  case l of
    [] -> 0
    [x] -> x
    (x:xs) -> if (max_ xs) > x then max_ xs else x

data Tree t = Leaf t | Node (Tree t, t, Tree t)

sum_tree :: Tree Int -> Int
sum_tree t = 
  case t of
    Leaf n -> n
    Node (a, b, c) -> sum_tree a + b + sum_tree c

depth :: Tree a -> Int
depth t = 
  case t of
    Leaf n -> 0
    Node (a, b, c) -> if depth a >= depth c then 1 + depth a else 1 + depth c

bin_search :: Tree Int -> Int -> Bool
bin_search t x =
  case t of
    Leaf n -> if n == x then True else False
    Node (a, b, c)
      | x == b -> True
      | x < b -> bin_search a x
      | x > b -> bin_search c x

preorder :: Tree a -> [a]
preorder t = 
  case t of
    Leaf n -> [n]
    Node (a, b, c) -> [b] ++ preorder a ++ preorder c

list_add :: [Int] -> [Int] -> [Int]
list_add l1 l2
  | l1 == [] && l2 == [] = []
  | l1 == [] && l2 /= [] = [] ++ l2
  | l1 /= [] && l2 == [] = [] ++ l1
  | l1 /= [] && l2 /= [] = [head l1 + head l2] ++ list_add (tail l1) (tail l2)

insert :: Int -> [Int] -> [Int]
insert m l =
  case l of
    [] -> [m]
    otherwise -> if head l >= m then [m] ++ l else [head l] ++ insert m (tail l)

insort :: [Int] -> [Int]
insort l =
  case l of
    [] -> []
    (x:xs) -> insert x (insort xs)

compose :: (a -> b) -> (b -> c) -> (a -> c)
compose f g = g . f

curry_ :: ((a, b) -> c) -> (a -> b -> c)
curry_ f = curry f

uncurry_ :: (a -> b -> c) -> ((a, b) -> c)
uncurry_ f = uncurry f

multifun :: (a -> a) -> Int -> (a -> a)
multifun f n
  | n == 1 = f
  | mod n 2 == 0 = multifun (compose f f) (div n 2)
  | mod n 2 == 1 = f . (multifun (compose f f) (div (n-1) 2))

ltake :: [a] -> Int -> [a]
ltake l n = take n l

lall :: (a -> Bool) -> [a] -> Bool
lall f l = all f l

lmap :: (a -> b) -> [a] -> [b]
lmap f l = map f l

lrev :: [a] -> [a]
lrev l = reverse l

lzip :: [a] -> [b] -> [(a, b)]
lzip x y = zip x y

split :: [a] -> ([a], [a])
split l = 
  case l of
    [] -> ([], [])
    [x] -> ([x], [])
    (x:y:xs) -> (x:xp, y:yp) where (xp, yp) = split xs

cartprod :: [a] -> [b] -> [(a, b)]
cartprod s t = [(x,y) | x <- s, y <- t]

main = putStrLn "Load Complete!"