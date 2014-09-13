myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr f x [] = x
-- myfoldr f x [y] = f y x : This pattern is unnecessary, it is included in pattern (y:ys)
myfoldr f x (y:ys) = f y (myfoldr f x ys)

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] xs = xs
merge (x:xs) (y:ys) = 
                if x < y
                then x : (merge xs (y:ys))
                else y : (merge (x:xs) ys)
                
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort as) (msort bs)
            where (as, bs) = split xs

split :: [a] -> ([a],[a])
split [] = ([],[])
split a = (take half a, drop half a)
           where half = div (length a) 2
           
{-
    case xs = []
        map f [] ++ map f ys
        = (m1)
            [] ++ map f ys
        = (a1)
            map f ys
        = (a1)
            map f ([] ++ ys)
    
    map f (x:xs) ++ map f ys
    = (m2)
        f x : map f xs ++ map f ys
    = (a2)
        f x : (map f xs ++ map f ys)
    = (IH) -- Inductive Hypotheses
        f x : map f (xs ++ ys)
    = (m2)
        map f (x:xs ++ ys)
        
-}

           
paraN :: (Eq a, Num a) => (t -> a -> t) -> t -> a -> t
paraN s z 0 = z
paraN s z n = s (paraN s z (n - 1)) n

fn :: (Eq n, Num n) => n -> n
fn n = paraN (*) 1 n

data B a = N a (B a) (B a) | E
paraB ::  (t -> t1 -> B t -> t1 -> B t -> t1) -> t1 -> B t -> t1
paraB n e (N a b1 b2) = n a (paraB n e b1) b1 (paraB n e b2) b2
paraB n e E = e

myfoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldl f x [] = x
myfoldl f x (y:ys) = myfoldl f (f x y) ys

para :: (a -> [a] -> b -> b) -> b -> [a] -> b
para c n [] = n
para c n (x:xs) = c x xs (para c n xs)

data Tree content = Leaf content
                  | Branch (Tree content) (Tree content)
type TreeAlgebra content result = (content -> result, result -> result -> result) 

foldTree :: TreeAlgebra content result -> Tree content -> result
foldTree algebra@(f_leaf,   _) (Leaf   value     ) = f_leaf   value
foldTree algebra@(_, f_branch) (Branch left right) = f_branch (foldTree algebra left) (foldTree algebra right)
 
treeDepth :: TreeAlgebra content Integer
treeDepth = (const 1, \left right -> 1 + max left right)

data Dbt x = Lf x
           | Nda (Dbt x) (Dbt x)
           | Ndb (Dbt x) (Dbt x)
type DbtAlgebra content result = (content -> result, result -> result -> result, result -> result -> result)

foldDbt :: (content -> result) -> (result -> result -> result) -> (result -> result -> result) -> Dbt content -> result
foldDbt a b c (Lf value) = a value
foldDbt a b c (Nda left right) = b (foldDbt a b c left) (foldDbt a b c right)
foldDbt a b c (Ndb left right) = c (foldDbt a b c left) (foldDbt a b c right)

{-
foldDbt :: DbtAlgebra content result -> Dbt content -> result
foldDbt algebra@(f_lf, _, _) (Lf value) = f_lf value
foldDbt algebra@(_, f_na, _) (Nda left right) = f_na (foldDbt algebra left) (foldDbt algebra right)
foldDbt algebra@(_, _, f_nb) (Ndb left right) = f_nb (foldDbt algebra left) (foldDbt algebra right)
-}
-- dbtDepth :: dbtAlgebra content Integer
-- dbtDepth = (const 1, \left right -> 1 + max left right, \left right -> 1 + max left right)

suff :: [x] -> [[x]]
suff = para (\x xs suffxs -> xs : suffxs) []
 
   suff "suffix" =["uffix","ffix","fix","ix","x",""]
   para (\x xs suffxs -> xs : suffxs) [] "" = []
   para (\x xs suffxs -> xs : suffxs) [] "x" = (\x xs suffxs -> xs : suffxs) "x" "" [] = [""]
   para (\x xs suffxs -> xs : suffxs) [] "ix" = (\x xs suffxs -> xs : suffxs) "i" "x" [""] = ["x",""]
   para (\x xs suffxs -> xs : suffxs) [] "fix" = (\x xs suffxs -> xs : suffxs) "f" "ix" ["x",""] = ["ix","x",""]
   para (\x xs suffxs -> xs : suffxs) [] "ffix" = (\x xs suffxs -> xs : suffxs) "f" "fix" ["ix","x",""] = ["fix","ix","x",""]
   para (\x xs suffxs -> xs : suffxs) [] "uffix" = (\x xs suffxs -> xs : suffxs) "u" "ffix" ["fix","ix","x",""] = ["ffix","fix","ix","x",""]
   para (\x xs suffxs -> xs : suffxs) [] "sufffix" = (\x xs suffxs -> xs : suffxs) "s" "uffix" ["ffix","fix","ix","x",""] = ["uffix","ffix","fix","ix","x",""]
-}
safeTail :: [x] -> Maybe [x]
safeTail = para (\_ xs _ -> Just xs) Nothing


reverse xs = foldr (\x rxs -> rxs ++ [x]) [] xs
reverse' xs = foldr (\x rxs -> (\b -> rxs (x:b))) (\b -> b) xs []
{- 
   Here, we know that Haskell function is left-associative, so the parameters of foldr actually are
   (\x rxs -> (\b -> rxs (x:b))), (\b -> b) and xs. For instance, say xs = [1,2,3]. First, the
   foldr will take the last element of the list, 3, as the operand, and function (\b -> b) as the other operand.
   These operands actually are the parameters of function (\x rxs -> (\b -> rxs (x:b))). In this function,
   parameter rxs actually is another function, which in this case is (\b -> b), which returns its parameter.
   So when it comes to the operands 3 and (\b -> b), it becomes (\3 (\b -> b) -> (\b' -> (\b -> b) (3:b'))).
   The reason why I put apostrophes on the original b is because the b in (\b -> rxs (x:b)) is not the same as
   in (\b -> b). So the result of function (\x rxs -> (\b -> rxs (x:b))) actually is function (\b -> (3:b)), because
   (\b -> b) (3:b') is equvalent to (3:b'). In the same way, the result function will becomes (\b' -> (\b -> (3:b)) (2:b')),
   (\b'' -> (\b' -> (\b -> (3:b)) (2:b')) (1:b'')).
   The result function receives the last parameter [], making the result as [3,2,1].
-}

data KVtree k v = KVmt | KVn k v (KVtree k v) (KVtree k v) deriving (Eq, Show)

kvAdd :: Ord k => k -> v -> KVtree k v -> KVtree k v
kvAdd k v KVmt = KVn k v KVmt KVmt
kvAdd k v (KVn kn vn kvt1 kvt2) =
        if k < kn then
                KVn kn vn (kvAdd k v kvt1) kvt2
        else if k == kn then
                KVn k v kvt1 kvt2
        else
                KVn kn vn kvt1 (kvAdd k v kvt2)

kvQry :: Ord k => KVtree k v -> k -> Maybe v
-- kvQry kvt q = the value v stored in the same node as q in kvt by kvAdd above
kvQry KVmt q = Nothing
kvQry (KVn k v kv1 kv2) q
                | q == k = Just v
                | q < k = kvQry kv1 q
                | otherwise = kvQry kv2 q
                            
cataKV ms ns KVmt = ms
cataKV ms ns (KVn k v kvt1 kvt2) = ns k v (cataKV ms ns kvt1) (cataKV ms ns kvt2)

ctQry kvt q =
        cataKV
        Nothing
        (\k v kvq1 kvq2 -> if q == k then Just v else if q < k then kvq1 else kvq2)
        kvt

paraKV ms ns KVmt = ms
paraKV ms ns (KVn k v kvt1 kvt2) = ns k v (paraKV ms ns kvt1) kvt1 (paraKV ms ns kvt2) kvt2

prAdd ka va kvt =
        paraKV
        (KVn ka va KVmt KVmt)
        (\k v r1 t1 r2 t2 ->
                if ka < k then KVn k v r1 t2
                else if ka == k then KVn k v t1 t2
                else KVn k v t1 r2)
        kvt

{-
case []:
    rev ([] ++ ys)
    = (r1)
        rev ys
    = (a4)
        rev ys ++ []
    = (r1)
        rev ys ++ rev []

case (x:xs) :
    rev (x:xs ++ ys)
    = (a2)
        rev (x:(xs ++ ys))
    = (a2)
        rev (xs ++ ys) ++ [x]
    = (IH)
        rev ys ++ rev xs ++ [x]
    = (a3)
        rev ys ++ (rev xs ++ [x])
    = (r2)
        rev ys ++ rev (x:xs)
-}

unfold :: (t -> (a,t)) -> (t -> Bool) -> t -> [a]
unfold next ended seed =
        if ended seed then []
        else
            let (nx, nseed) = next seed
            in nx : unfold next ended nseed
            
from a b = unfold (\s -> (s,s+1)) (>b) a
