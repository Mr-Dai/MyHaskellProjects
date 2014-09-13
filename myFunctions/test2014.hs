--Question 1 (30 marks)
--Using the functions below (and no other list processing functions)
fold o b [] = b
fold o b (x:xs) = o x (fold o b xs)

unfold n e s =
        if e s then []
        else
            let (x,s') = n s
            in x : unfold n e s'

--(a) rewrite the following function in terms of list catamorphism (7 marks)
longest :: [[a]] -> [a]
longest [] = []
longest (xs:xss) =
        lngst xs xss
        where
        lngst xs [] = xs
        lngst xs (ys:yss) =
                lngst (if length xs > length ys then xs else ys) yss
                
--Solution:
longest' :: [[a]] -> [a]
longest' xss = fold
               (\a b -> if length a > length b then a else b)
               []
               xss
             
--(b) rewrite the following function in terms of list anamorphism (8 marks)
fibs :: Int -> Int -> Int -> [Int]
fibs m n x = take x (m : fibs n (m+n) (x-1))
--Solution:
fibs' :: Int -> Int -> Int -> [Int]
fibs' m n x = unfold
              (\(a,b,c) -> (a,(b,a+b,c+1)))
              (\(a,b,c) -> c > x)
              (m,n,1)
--(c) rewrite the following function in terms of list hylomorphism (10 marks)
e2x x limit = ex 1 1 1
         where
         ex num den sofar =
                num/den
                +
                if sofar > limit then 0
                else ex (num*x) (den*sofar) (sofar+1)
--Solution:
e2x' x limit = fold (+) 0 (unfold
                           (\(a,b) -> ((pow a b)/(fact b), (a,b+1)))
                           (\(a,b) -> b > limit)
                           (x,0))
               where
                pow a 0 = 1
                pow a b = a * pow a (b-1)
                fact 0 = 1
                fact a = a * fact (a-1)

--(d) using "fix" as defined below rewrite "fold" (as defined above) without explicit recursion
fix f = f (fix f)
fold' o b [] = b
fold' o b xs = (fix o) xs

--Question 2 (20 marks)
--Given the following data type and its preorder traversal function
data N a = C a [N a] deriving Eq

preorder :: N a -> [a]
preorder (C a nas) = [a] ++ concat (map preorder nas)

--(a) give the type and definition of the catamorphism "cataN" on N (5 marks)
cataN :: (a -> [t] -> t) -> (N a) -> t
cataN op (C a ns) = op a (map (cataN op) ns)
--(b) reimplement "preorder" as a catamorphism (3 marks)
preorder' :: N a -> [a]
preorder' n = cataN
              (\a ns -> a : concat ns)
              n
--(c) give the type and recursive definition of the paramorphism on N (7 marks)
paraN :: (a -> [t] -> [N a] -> t) -> (N a) -> t
paraN op (C a ns) = op a (map (paraN op) ns) ns
--(d) complete the definition as a catamorphism of the following function (5 marks)
depth :: N a -> Int
-- depth na = the longest path from the root of the n-ary tree
-- given by na, where the depth of "C a []" is defined to be 0
depth n = cataN
          (\a ns -> if ns == [] then 0 else 1 + maximum ns)
          n
          
{-
Question 3 (20 marks)
Given the functions sum and ++ defined by the equations

s1: sum [] = 0
s2: sum (x:xs) = x + sum xs
a1: [] ++ ys = ys
a2: (x:xs) ++ ys = x:(xs ++ ys)

and also given the theorems

d: x+(y+z) = (x+y)+z
z: 0+x = x

prove
    sum xs + sum ys = sum (xs ++ ys)
-}
                      