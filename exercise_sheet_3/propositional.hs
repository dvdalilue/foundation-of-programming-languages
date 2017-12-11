infixl 3 :&:
infixl 2 :|:
infixr 1 :>:
infix  0 :=:

data Prop = Prop :=: Prop
          | Prop :&: Prop
          | Prop :|: Prop
          | Prop :>: Prop
          | Negation Prop
          | Statement Char
    -- deriving Show

instance Show Prop where
    show p = go p $ -1
        where
            go (lhs :=: rhs) pre = shouldParent pre 0 $ (go lhs 0) ++ " ↔ " ++ (go rhs 0)
            go (lhs :>: rhs) pre = shouldParent pre 1 $ (go lhs 1) ++ " → " ++ (go rhs 1)
            go (lhs :|: rhs) pre = shouldParent pre 2 $ (go lhs 2) ++ " ∨ " ++ (go rhs 2)
            go (lhs :&: rhs) pre = shouldParent pre 3 $ (go lhs 3) ++ " ∧ " ++ (go rhs 3)
            go (Negation  p) pre = "¬" ++ (go p 4)
            go (Statement p) pre = [p]

            shouldParent a b s = if a >= b then "(" ++ s ++ ")" else s

data RPNB = IFF
          | OR
          | AND
          | THEN
          | NEG
          | TRUE
          | FALSE
    deriving Show


p = (Statement 'P')
q = (Statement 'Q')
r = (Statement 'R')

prop_a = p :|: p :>: p
prop_b = (p :|: q) :|: r :=: p :|: (q :|: r)
prop_c = p :>: q :=: Negation (Negation p :&: q) -- not a taut (only this?)
prop_d = p :&: (p :|: q) :=: p
prop_e = Negation (p :|: q) :=: Negation p :&: Negation q
prop_f = p :>: (q :>: r) :>: ((p :>: q) :>: (p :>: r))
prop_g = (p :>: q) :>: ((p :>: Negation q) :>: Negation p)
prop_h = p :>: Negation (Negation  p)
prop_i = Negation (Negation  p) :>: p
prop_j = (p :>: q) :|: (q :>: p)
prop_k = ((p :>: q) :>: p) :>: p
prop_l = (p :>: q) :=: (Negation p :|: q)

all_props = [prop_a,prop_b,prop_c,prop_d,prop_e,prop_f,prop_g,prop_h,prop_i,prop_j,prop_k,prop_l]

exercise_8 = map taut all_props

modus_ponens = (p :>: q) :&: p :>: q

taut p = and . map (rpnb p) $ posibleCombinationOf "PQR"

posibleCombinationOf :: [Char] -> [[(Char,Bool)]]
posibleCombinationOf cs
    | (length cs) <= 0 = []
    | otherwise = go (length cs) cs [[]]
    where
        go 0 _      acc = acc
        go n (c:cs) acc = go (n-1) cs $ combine c acc
        -- combine :: [[Bool]] -> [[Bool]]
        combine c = concatMap (\x -> map (flip (:) x) [(c,True),(c,False)])

rpnb :: Prop -> [(Char,Bool)] -> Bool
rpnb p hs = rpnToBool $ doit hs [p] []
    where
        -- doit :: [(Char,Bool)] -> [Prop] -> [RPNB] -> [RPNB]
        doit _  []                    rpn = rpn
        doit hs ((Statement p):stack) rpn = doit hs stack           (r:rpn)
            where r = lookup' p hs
        doit hs ((Negation  p):stack) rpn = doit hs (p:stack)       (NEG :rpn)
        doit hs ((lhs :=: rhs):stack) rpn = doit hs (lhs:rhs:stack) (IFF :rpn)
        doit hs ((lhs :>: rhs):stack) rpn = doit hs (lhs:rhs:stack) (THEN:rpn)
        doit hs ((lhs :|: rhs):stack) rpn = doit hs (lhs:rhs:stack) (OR  :rpn)
        doit hs ((lhs :&: rhs):stack) rpn = doit hs (lhs:rhs:stack) (AND :rpn)

        lookup' k h = case (lookup k h) of
                        (Just v) -> if v then TRUE else FALSE
                        otherwise -> error "This should never happend"

rpnToBool :: [RPNB] -> Bool
rpnToBool = go []
    where
        go [result]    []         = result
        go stack       (TRUE:ss)  = go (True:stack) ss
        go stack       (FALSE:ss) = go (False:stack) ss
        go (l:r:stack) (IFF:ss)   = go ((l == r):stack) ss
        go (l:r:stack) (THEN:ss)  = go ((if l then r else True):stack) ss
        go (l:r:stack) (OR:ss)    = go ((l || r):stack) ss
        go (l:r:stack) (AND:ss)   = go ((l && r):stack) ss
        go (u:stack)   (NEG:ss)   = go ((not u):stack) ss