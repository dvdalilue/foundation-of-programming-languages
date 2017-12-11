data E = Empty | EvenOnesE E | OddOnesE O
    deriving (Show, Read)

data O = OddOnesO O | EvenOnesO E
    deriving (Show, Read)

test :: E
test = EvenOnesE (OddOnesE (OddOnesO (OddOnesO (EvenOnesO Empty))))

treeToInt :: Num a => E -> [a]
treeToInt Empty         = []
treeToInt (EvenOnesE y) = 0:(treeToInt y)
treeToInt (OddOnesE y)  = 1:(treeToIntO y)
    where
        treeToIntO (OddOnesO y)  = 0:(treeToIntO y)
        treeToIntO (EvenOnesO y) = 1:(treeToInt y)

listToInt :: [Int] -> Int
listToInt = foldl (\a b -> (a*10) + b) 0