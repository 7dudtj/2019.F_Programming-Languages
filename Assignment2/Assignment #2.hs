-- Part 1: Recursive Functions

lconcat :: [[a]] -> [a]
lconcat l =
  case l of
    [] -> []
    (h:t) -> h ++ (lconcat t)

lfoldl :: ((a, b) -> b) -> b -> [a] -> b
lfoldl f e l =
  case l of
    [] -> e
    (h:t) -> lfoldl f (f (h, e)) t

-- Part 2: Tail-Recursive Functions

fact :: Int -> Int
fact n =
  let
    factTR n v =
      if n == 0 then
        v
      else
        factTR (n - 1) (n * v)
  in
    factTR n 1

power :: Int -> Int -> Int
power x n =
  let
    powerTR x n v =
      if n == 0 then
        v
      else
        powerTR x (n - 1) (x * v)
  in
    powerTR x n 1

fib :: Int -> Int
fib n =
  let
    fibTR n v1 v2 =
      if n == 0 then
        v2
      else
        fibTR (n - 1) v2 (v1 + v2)
  in
    fibTR n 0 1

lfilter :: (a -> Bool) -> [a] -> [a]
lfilter p l =
  let
    lfilterTR p l l' =
      case l of
        [] -> l'
        (h:t) ->
          if p h then
            lfilterTR p t (l' ++ [h])
          else
            lfilterTR p t l'
  in
    lfilterTR p l []

ltabulate :: Int -> (Int -> a) -> [a]
ltabulate n f =
  let
    ltabulateTR n f l =
      if n == 0 then
        l
      else
        ltabulateTR (n - 1) f ([f (n - 1)] ++ l)
  in
    ltabulateTR n f []

union :: (Eq a) => [a] -> [a] -> [a]
union s t =
  case s of
    [] -> t
    sh:st ->
      if elem sh t then
        union st t
      else
        union st (t ++ [sh])

data Tree t = Leaf t | Node (Tree t, t, Tree t)

inorder :: Tree a -> [a]
inorder t =
  let
    inorderTR t tl vl =
      case (t, tl) of
        (Leaf v, []) -> vl ++ [v]
        (Leaf v, tl_h:tl_t) -> inorderTR tl_h tl_t (vl ++ [v])
        (Node (t1, v, t2), _) -> inorderTR t1 ([Leaf v, t2] ++ tl) vl
  in
    inorderTR t [] []

postorder :: Tree a -> [a]
postorder t =
  let
    postorderTR t tl vl =
      case (t, tl) of
        (Leaf v, []) -> vl ++ [v]
        (Leaf v, tl_h:tl_t) -> postorderTR tl_h tl_t (vl ++ [v])
        (Node (t1, v, t2), _) -> postorderTR t1 ([t2, Leaf v] ++ tl) vl
  in
    postorderTR t [] []

preorder :: Tree a -> [a]
preorder t =
  let
    preorderTR t tl vl =
      case (t, tl) of
        (Leaf v, []) -> vl ++ [v]
        (Leaf v, tl_h:tl_t) -> preorderTR tl_h tl_t (vl ++ [v])
        (Node (t1, v, t2), _) -> preorderTR (Leaf v) ([t1, t2] ++ tl) vl
  in
    preorderTR t [] []

-- Part 3: Sorting

quicksort :: (Ord a) => [a] -> [a]
quicksort l =
  case l of
    [] -> []
    [h] -> [h]
    (pivot : l') ->
      let
        smaller = lfilter (\x -> x < pivot) l'
        greater = lfilter (\x -> x >= pivot) l'
      in
        (quicksort smaller) ++ [pivot] ++ (quicksort greater)

mergesort :: (Ord a) => [a] -> [a]
mergesort l =
  case l of
    [] -> []
    [h] -> [h]
    _ ->
      let
        num_elems_per_list = (length l + 1) `div` 2
        list1 = mergesort (take num_elems_per_list l)
        list2 = mergesort (drop num_elems_per_list l)
        mergeLists l1 l2 =
          case (l1, l2) of
            ([], _) -> l2
            (_, []) -> l1
            (h1:t1, h2:t2) ->
              if h1 < h2 then
                [h1] ++ mergeLists t1 l2
              else
                [h2] ++ mergeLists l1 t2
      in
        mergeLists list1 list2

-- Part 4: Heap

type Loc = Int
type Heap t = [(Loc, t)]

heapEmpty :: () -> Heap a
heapEmpty () = []

heapAllocate :: Heap a -> a -> (Heap a, Loc)
heapAllocate h v =
  let
    l = length h
  in
    (h ++ [(l, v)], l)

heapDereference :: Heap a -> Loc -> Maybe a
heapDereference h l =
  if l < 0 || l >= length h then
    Nothing
  else
    Just (snd (h !! l))

heapUpdate :: Heap a -> Loc -> a -> Maybe (Heap a)
heapUpdate h l v =
  if l < 0 || l >= length h then
    Nothing
  else
    Just ((take l h) ++ [(l, v)] ++ (drop (l + 1) h))

-- DONE!

main = putStrLn "Load Complete!"