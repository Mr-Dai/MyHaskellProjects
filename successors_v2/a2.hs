data Sex = Male | Female deriving (Eq, Show)
instance Ord Sex where
    compare Male Male = EQ
    compare Male Female = LT
    compare Female Male = GT
    compare Female Female = EQ

data Status = Alive | Dead | Abdicated deriving (Eq, Show)

data Person = Person Sex Status String deriving (Eq, Show)
instance Ord Person where
    compare (Person sx1 _ _) (Person sx2 _ _) = compare sx1 sx2

data Dynasty =
    Descend Person [Dynasty] | Dull Person deriving (Eq, Show)
instance Ord Dynasty where
    compare (Descend p1 _) (Descend p2 _) = compare p1 p2
    compare (Descend p1 _) (Dull p2 ) = compare p1 p2
    compare (Dull p1 ) (Descend p2 _) = compare p1 p2
    compare (Dull p1) (Dull p2 ) = compare p1 p2

successors :: String -> Dynasty -> [String]
successors name dynasty = aliveafter name (linefrom dynasty)
-- define the catamorphism cataD on Dynasty
-- then reimplement linefrom to use cataD instead of explicit
-- recursion and the new version of reorder below
cataD :: (Person -> [t] -> t) -> (Person -> t) -> Dynasty -> t
cataD des dus dy = case dy of
                Dull p -> dus p
                Descend p ds -> des p (map (cataD des dus) ds)

linefrom :: Dynasty -> [Person]
linefrom dy = cataD
              (\p@(Person _ st _) ds -> if st == Abdicated then [] else [p] ++ concat ds)
              (\p@(Person _ st _) -> if st == Abdicated then [] else [p])
              (reorder dy)
{-
    case reorder dy of
        Descend (Person _ Abdicated _) ds -> []
        Descend person ds -> [person] ++ (concat . map linefrom) ds
        Dull (Person _ Abdicated _) -> []
        Dull person -> [person]
-}
-- redefine reorder so that all sub-dynasties are sorted
-- with Males before Females, using cataD
reorder :: Dynasty -> Dynasty
reorder dy = cataD
             (\p ds -> Descend p (sortds ds))
             (\p -> Dull p)
             dy

{-
reorder (Descend person ds) = Descend person (sortds ds)
reorder d@(Dull _) = d
-}
sortds :: [Dynasty] -> [Dynasty]
-- reimplement sortds to use new insertd and flatten below
sortds [] = []
sortds (d:ds) = sortds (filter (< d) ds) ++ [d] ++ sortds (filter (>= d) ds)
-- sortds dys = foldr insertd [] dys
-- define a type of binary trees for Dynasty
data BTD = Dnode BTD Dynasty BTD | Dnull
-- define the catamorphism cataBTD on the above type
cataBTD :: (t -> Dynasty -> t -> t) -> t -> BTD -> t
cataBTD dnos dnus btd = case btd of
                            Dnode a d b -> dnos (cataBTD dnos dnus a) d (cataBTD dnos dnus b)
                            Dnull -> dnus
-- use cataBTD to define a function to “flatten” btd :: BTD
-- in an in-order traversal
flatten :: BTD -> [Dynasty]
flatten a = cataBTD
            (\x y z -> x ++ [y] ++ z)
            []
            a
-- redefine insertd so that “flatten d” yields every top-level
-- Dynasty in d headed by a Male before every top-level Dynasty
-- in d headed by a Female, in particular so that
-- “flatten(insertd d btd)” yields d before every top-level Dynasty
-- headed by a Person of the same Sex as d
insertd :: Dynasty -> BTD -> BTD
insertd dy Dnull = Dnode Dnull dy Dnull
insertd dy (Dnode Dnull d Dnull) =
                            if st == GT then (Dnode Dnull d (Dnode Dnull dy Dnull))
                            else (Dnode (Dnode Dnull dy Dnull) d Dnull)
                            where st = compare dy d
insertd dy (Dnode a d Dnull) = 
                            if st == GT then (Dnode a d (Dnode Dnull dy Dnull))
                            else (Dnode (insertd dy a) d Dnull)
                            where st = compare dy d
insertd dy (Dnode Dnull d a) = 
                            if st == EQ || st == LT then (Dnode (Dnode Dnull dy Dnull) d a)
                            else (Dnode Dnull d (insertd dy a))
                            where st = compare dy d
insertd dy (Dnode a d b) =
                            if st == EQ then (Dnode (insertd dy a) d b)
                            else (Dnode a d (insertd dy b))
                            where st = compare dy d
{-
insertd :: Dynasty -> [Dynasty] -> [Dynasty]
insertd dy [] = [dy]
insertd dy (d:ds) = if dy > d then d:(insertd dy ds) else dy:d:ds
-}

aliveafter :: String -> [Person] -> [String]
aliveafter name ps =
    let fromnam = dropWhile (\(Person _ _ pname)-> name /= pname) ps
    in if null fromnam then [] else alivein (tail fromnam)
alivein :: [Person] -> [String]
alivein ps =
    map
    (\(Person _ _ name) -> name)
    (filter (\(Person _ st _) -> st == Alive) ps)
    
exdyn =
    Descend (Person Male Dead "George5")
    [
        Descend (Person Male Abdicated "Edward8") [],
        Descend (Person Male Dead "George6")
        [
            Descend (Person Female Alive "Elizabeth2")
            [
                Descend (Person Male Alive "Charles")
                [
                    Descend (Person Male Alive "William")
                    [
                        Descend (Person Male Alive "George") []
                    ],

                    Descend (Person Male Alive "Harry") []

                ],

                Descend (Person Female Alive "Anne")

                [

                    Descend (Person Male Alive "Peter")

                    [

                        Dull (Person Female Alive "Savannah"),

                        Dull (Person Female Alive "Isla")

                    ],

                    Dull (Person Female Alive "Zarah")

                ],

                Descend (Person Male Alive "Andrew")

                [

                    Dull (Person Female Alive "Beatrice"),

                    Dull (Person Female Alive "Eugenie")

                ],

                Descend (Person Male Alive "Edward")

                [

                    Dull (Person Female Alive "Louise"),

                    Dull (Person Male Alive "James")

                ]

            ],

            Descend (Person Female Dead "Margaret")

            [

                Dull (Person Male Alive "David"),

                Dull (Person Female Alive "Sarah")

            ]

        ],

        Dull (Person Female Dead "Mary"),

        Dull (Person Male Dead "Henry"),

        Dull (Person Male Dead "George"),

        Dull (Person Male Dead "John")

    ]