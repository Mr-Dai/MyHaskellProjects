-- a person is Male or Female, Abdicated, Dead or Alive, and has a name

--["Eugenie","Edward","James","Louise","Anne","Peter","Savannah","Isla","Zarah","David","Sarah"]

data Sex = Male
         | Female
         deriving (Eq, Show)

data Status = Alive
            | Dead
            | Abdicated
            deriving (Eq, Show)
            
data Person = Person Sex Status String deriving (Eq, Show)
-- a Dynasty is headed by a Person and indicates the descendants
-- oldest first; a Dull Person doesn’t have any recorded descendants
data Dynasty = Descend Person [Dynasty]
             | Dull Person
             deriving (Eq, Show)
             
fold :: (a -> [b]) -> [a] -> [b]
-- Self-defined function, folds all the results together
fold f [] = []
fold f [x] = f x
fold f (x:xs) = (f x) ++ (fold f xs)

isMale :: Dynasty -> Bool
-- Self-defined function, marks whether the head person of a dynasty is male 
isMale (Descend (Person s st n) dys) = (s == Male)
isMale (Dull (Person s st n)) = (s == Male)

isAdb :: Dynasty -> Bool
-- Self-defined function, marks whether the head person of a dynasty is abdicated
isAdb (Descend (Person s st n) dys) = (st == Abdicated)
isAdb (Dull (Person s st n)) = (st == Abdicated)

isIn :: String -> Dynasty -> Bool
-- Self-defined function, marks whether a person is inside the dynasty
isIn n (Dull (Person s st n_)) =
                if n == n_
                then True
                else False
isIn n (Descend (Person s st n_) []) =
                if n == n_
                then True
                else False
isIn n (Descend (Person s st n_) dys) =
                if n == n_
                then True
                else or (map (isIn n) dys)
 
successors :: String -> Dynasty -> [String]
successors n (Descend p []) = []
successors n (Dull p) = []
successors n (Descend (Person s st n_) [a]) =
                if n_ == n
                then alivein (fold linefrom (sortds [a]))
                else if isIn n a
                then successors n a
                else []
successors n (Descend (Person s st n_) [a,b]) =
                if n_ == n
                then alivein (fold linefrom (sortds [a, b]))
                else if isIn n b
                then (successors n b) ++ (alivein (fold linefrom (sortds [a])))
                else if isIn n a
                then (successors n a) ++ (alivein (fold linefrom (sortds [b])))
                else successors n b
successors n (Descend (Person s st n_) (a:b:ds)) =
                if n_ == n
                then alivein (fold linefrom (sortds (a:b:ds)))
                else if isIn n b
                then (successors n b) ++ (alivein (fold linefrom (sortds (a : head ds : []))))
                else if isIn n (head ds)
                then successors n (Descend (Person s st n_) (b:ds))
                else successors n (Descend (Person s st n_) ds)
successors n (Descend (Person s st n_) (a:ds)) =
                if n_ == n
                then alivein (fold linefrom (sortds (a:ds)))
                else if isIn n a
                then (successors n a) ++ (alivein (fold linefrom (sortds ((head ds):[]))))
                else successors n (Descend (Person s st n_) ds)

linefrom :: Dynasty -> [Person]
--linefrom dy = the list of all Persons in order of succession in Dynasty dy
linefrom (Dull (Person s st n)) = 
                                    if st /= Abdicated
                                    then [Person s st n]
                                    else []
linefrom (Descend (Person s st n) []) =
                                        if st /= Abdicated
                                        then [Person s st n]
                                        else []
linefrom (Descend (Person s st n) dys) = 
                                        if st /= Abdicated
                                        then (Person s st n) : (fold linefrom (sortds dys))
                                        else []

reorder :: Dynasty -> Dynasty
--reorder dy = arrange for any immediate descendants of dy with males before females
--reorder (Descend person ds) = Descend person (sortds ds)
--reorder d@(Dull _) = d
reorder (Dull p) = Dull p
reorder (Descend p ds) = Descend p ((filter isMale ds) ++ (filter (not.isMale) ds))

sortd :: Dynasty -> Dynasty
sortd (Dull p) = Dull p
sortd (Descend p ds) = Descend p (sortds ds)

sortds :: [Dynasty] -> [Dynasty]
--sort dys so that males precede females, not changing the order within each sex
--sortds dys = foldr insertd [] dys
sortds [] = []
sortds dys = map sortd ((filter isMale dys) ++ (filter (not.isMale) dys))

insertd :: Dynasty -> [Dynasty] -> [Dynasty]
--insertd dy dys = insert dy into sorted position in dys (see sortds above)
--insertd dy [] = [dy]
--insertd dy (d:ds) = if dy > d then d:(insertd dy ds) else dy:d:ds
insertd dy dys = sortds (dy:dys)

aliveafter :: String -> [Person] -> [String]
--aliveafter name ps = the names of the Alive persons in ps occurring after name
--aliveafter name ps =
--      let from_name = dropWhile (\(Person _ _ pname) -> name /= pname) ps
--      in if null from_name then [] else alivein (tail from_name)
aliveafter n [] = []
aliveafter n [p] = []
aliveafter n ((Person s st n_):ps) =
                if n_ == n
                then (alivein ps)
                else (aliveafter n ps)

alivein :: [Person] -> [String]
--alivein ps = the names of the Alive persons in ps
--alivein ps = map (\(Person _ _ name) -> name) (filter (\(Person _ st _) -> st == Alive ps))
alivein [Person s st n] =
                        if st == Alive
                        then [n]
                        else if st == Abdicated
                        then [n]
                        else []
alivein ((Person s st n):ps) =
                        if st == Alive
                        then n : (alivein ps)
                        else if st == Abdicated
                        then n : (alivein ps)
                        else alivein ps

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
    
    
george5 = Person Male Dead "George5"
edward8 =  Person Male Abdicated "Edward8"
george6 = Person Male Dead "George6"
mary = Person Female Dead "Mary"
henry = Person Male Dead "Henry"
george1 = Person Male Dead "George"
john = Person Male Dead "John"
elizabeth2 = Person Female Alive "Elizabeth2"
margaret = Person Female Dead "Margaret"
charles = Person Male Alive "Charles"
anne = Person Female Alive "Anne"
andrew = Person Male Alive "Andrew"
edward = Person Male Alive "Edward"
william = Person Male Alive "William"
george2 = Person Male Alive "George"
harry = Person Male Alive "Harry"
peter = Person Male Alive "Peter"
savannah = Person Female Alive "Savannah"
isla = Person Female Alive "Isla"
zarah = Person Female Alive "Zarah"
beatrice = Person Female Alive "Beatrice"
eugenie = Person Female Alive "Eugenie"
louise = Person Female Alive "Louise"
david = Person Male Alive "David"
sarah = Person Female Alive "Sarah"
james = Person Male Alive "James"
dynasty26 = Dull isla
dynasty25 = Dull savannah
dynasty24 = Dull george2
dynasty23 = Dull james
dynasty22 = Dull louise
dynasty21 = Dull eugenie
dynasty20 = Dull beatrice
dynasty19 = Dull zarah
dynasty18 = Descend peter [dynasty25, dynasty26]
dynasty17 = Dull harry
dynasty16 = Descend william [dynasty24]
dynasty15 = Dull sarah
dynasty14 = Dull david
dynasty13 = Descend edward [dynasty22, dynasty23]
dynasty12 = Descend andrew [dynasty20, dynasty21]
dynasty11 = Descend anne [dynasty18, dynasty19]
dynasty10 = Descend charles [dynasty16, dynasty17]
dynasty9  = Descend margaret [dynasty14, dynasty15]
dynasty8  = Descend elizabeth2 [dynasty10, dynasty11, dynasty12, dynasty13]
dynasty7  = Dull mary
dynasty6  = Dull john
dynasty5  = Dull george1
dynasty4  = Dull henry
dynasty3  = Descend george6 [dynasty8, dynasty9]
dynasty2  = Dull edward8
dynasty1  = Descend george5 [dynasty2,dynasty3,dynasty4,dynasty5,dynasty6,dynasty7]

ai_test1 = 
    if (["William","Peter","Harry"] == alivein [william, mary, john, peter, harry])
        then "OK" 
    else "fail"

ai_test2 = 
    if ([] == alivein [mary, john])
        then "OK" 
    else "fail"

af_test1 = 
    if (["Peter","Harry"] == aliveafter "Mary" [william, mary, john, peter, harry])
        then "OK" 
    else "fail"

af_test2 = 
    if ([] == aliveafter "Edward8" [william, mary, john, peter, harry])
        then "OK" 
    else "fail"

af_test3 = 
    if ([] == aliveafter "Harry" [william, mary, john, peter, harry])
        then "OK" 
    else "fail"

af_test4 = 
    if ([] == aliveafter "Harry" [])
        then "OK" 
    else "fail"


exdyn1 =
    Descend (Person Male Dead "George5")
    [
        Descend (Person Male Alive "Edward8") [],
        Descend (Person Male Abdicated "George6")
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

                    Descend (Person Female Alive "Peter")

                    [

                        Dull (Person Male Alive "Savannah"),

                        Dull (Person Female Alive "Lily"),
                        Dull (Person Female Alive "Tera"),
                        Dull (Person Male Alive "Bob")

                    ],

                    Dull (Person Male Alive "Zarah")

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

ab_test = if (["Edward8"] == successors "George5" exdyn1) then "OK" else "fail"
ab_test1 = if ([] == successors "Anne" exdyn1) then "OK" else "fail"

main = putStrLn ("alivein: " ++ ai_test1 ++ " " ++ ai_test2 ++ "\n" ++ 
    "aliveafter: " ++ af_test1 ++ " " ++ af_test2 ++ " " ++ af_test3 ++ " " ++ af_test4 ++ "\n" ++
    "Abdicated_test: " ++ ab_test ++ " " ++ ab_test1 ++ "\n")