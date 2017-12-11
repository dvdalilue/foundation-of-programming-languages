module Lambda where

import System.Random

{-
    Booleans
-}

-- Church booleans
churchTrue = \a -> \b -> a -- λa.λb.a

churchFalse = \a -> \b -> b -- λa.λb.b

-- Church logic operators
churchConj = \a -> \b -> a b churchFalse -- λa.λb. a b a

churchDisy = \a -> \b -> a churchTrue b -- λa.λb. a a b

churchNeg = \a -> a churchFalse churchTrue -- λa. a false true

churchIfThenElse = \p -> \a -> \b -> p a b

-- Boolean Church & Unchurch
-- boolChurch :: Bool -> (t -> t -> t)
boolChurch True = churchTrue
boolChurch False = churchFalse

-- boolUnchurch :: (Bool -> Bool -> t) -> t
boolUnchurch chb = chb True False

{-
    Natural numbers
-}

-- Apply a function n times on x
apply = \f -> \n -> \x -> foldr ($) x $ replicate n f

-- Church numbers
churchZero = \f -> id

churchOne = \f -> \x -> apply f 1 x

churchTwo = \f -> \x -> apply f 2 x

churchNatural = \n -> \f -> \x -> apply f n x

-- Number Church & Unchurch
natChurch = churchNatural

natUnchurch chn = chn (+1) 0

churchSum = \n -> \m -> \f -> \x -> n f (m f x)

churchMul = \n -> \m -> \f -> \x -> n (m f) x

churchExp = \n -> \m -> m n

{-
    Lambda calculus abstract sintax
-}

type Name = Char

data Lambda = Abstraction Name Lambda
            | Application Lambda Lambda
            | Variable Name
            deriving Eq

instance Show Lambda where
    show (Abstraction x t) = "λ" ++ [x] ++ ". " ++ show t
    show (Application t s) = show t ++ " " ++ show s
    show (Variable c) = [c]

term1 = Abstraction 'x' $ Application (Variable 'x') (Variable 'y')
term2 = Application (Abstraction 'x' (Variable 'y')) (Variable 'x')
term3 = Abstraction 'x' (Variable 'x')
term4 = Abstraction 'x' (Variable 'y')
term5 = Abstraction 'x' $ 
            Application (Abstraction 'y'
                (Abstraction 'x' 
                    (Application 
                        (Variable 'x') 
                        (Variable 'y')
                    )
                )
            )
            (Variable 'x')
term6 = Application
            (Abstraction 'x' (Application (Variable 'x') (Variable 'x')))
            (Abstraction 'x' (Application (Variable 'x') (Variable 'x')))
term7 = Application
            (Application 
                (Application
                    (Abstraction 'b' 
                        (Abstraction 'z' 
                            (Abstraction 'w' 
                                (Application 
                                    (Application 
                                        (Variable 'b') 
                                        (Variable 'z')
                                    ) 
                                    (Variable 'w')
                                )
                            )
                        )
                    )
                    (Abstraction 'a' (Abstraction 'c' (Variable 'a')))
                )
                (Variable 'x')
            )
            (Variable 'y')
term8 = Application
            (Application 
                (Application
                    (Abstraction 'b' 
                        (Abstraction 'x' 
                            (Abstraction 'y' 
                                (Application 
                                    (Application 
                                        (Variable 'b') 
                                        (Variable 'x')
                                    ) 
                                    (Variable 'y')
                                )
                            )
                        )
                    )
                    (Abstraction 'x' (Abstraction 'y' (Variable 'x')))
                )
                (Variable 'x')
            )
            (Variable 'y')


freeVars :: Lambda -> [Name]
freeVars l = go [l] ("","")
    where
        go [] vars = snd vars
        go ((Abstraction x t):ls) (bnd,free) = go (t:ls) (x:bnd,free)
        go ((Application t s):ls) vars = go (t:s:ls) vars
        go ((Variable c) :ls) vs@(bnd,free) = go ls $ if c `elem` bnd
                                                        then vs
                                                        else (bnd,(c:free))

getRanNameOut :: [Name] -> Name
getRanNameOut []         = fst $ randomR ('a','z') $ mkStdGen 23
getRanNameOut chs@(ch:_) = foo ch chs $ mkStdGen 23
    where
        foo ch chs stdGen
            | ch `elem` chs = let (nc,gen) = randomR ('a','z') stdGen 
                              in foo nc chs gen
            | otherwise = ch

capAvdSub :: Lambda -> (Name,Lambda) -> Lambda
capAvdSub l@(Abstraction y p) (x,t) =
    if x == y then l else
        if x `elem` (freeVars p) then
            if y `elem` (freeVars t) then
                let nc = getRanNameOut $ x:(freeVars (Application t p))
                in capAvdSub (Abstraction nc (capAvdSub p (y,Variable nc))) (x,t)
            else Abstraction y $ capAvdSub p (x,t)
        else l
capAvdSub (Application t s) sub = Application (capAvdSub t sub) (capAvdSub s sub)
capAvdSub (Variable c) (x,t) =
    if x == c then t
              else (Variable c)

betaRed :: Lambda -> Lambda
betaRed (Application (Abstraction x t) s) = capAvdSub t (x,s)
betaRed (Application t s) =
    let bt = (betaRed t) in
        if t == bt then 
            let bs = (betaRed s) in
                if s == bs
                    then (Application t s) 
                    else (Application t bs)
        else (Application bt s)
betaRed v = v

betaNorm :: Lambda -> Lambda
betaNorm (Application (Abstraction x t) s) = betaNorm $ capAvdSub t (x,s)
betaNorm l@(Application t s) =
    let b = Application (betaNorm t) (betaNorm s)
    in if l == b then l else betaNorm b
betaNorm (Abstraction x t) = Abstraction x (betaNorm t)
betaNorm v = v


---------------------------------------------------------------
-- Other things
---------------------------------------------------------------

-- Pair

pair = \f -> \s -> \p -> p f s

first = \p -> p churchTrue

second = \p -> p churchFalse

scc = \n -> \f -> \x -> f (n f x)

incr = \p -> pair (second p) (scc (second p))

prd = \n -> first (n incr (pair churchZero churchZero))

-- List

nil = churchFalse

cons = pair

isEmpty = \l -> l (\h t p -> churchFalse) churchTrue

hd = first

tl = second

-- rec = \g -> (\x -> g x x) (\x -> g x x)

-- app = \f -> \l -> \m -> churchIfThenElse (isEmpty l) m (cons (hd l) (f (tl l) m))