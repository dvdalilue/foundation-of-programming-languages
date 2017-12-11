data Expression = Number Int | 
                  Plus     Expression Expression |
                  Minus    Expression Expression |
                  Asterisk Expression Expression
    -- deriving Show

instance Show Expression where
    show (Number v)         = show v
    show (Plus lhs rhs)     = "(" ++ (show lhs) ++ "+" ++ (show rhs) ++ ")"
    show (Minus lhs rhs)    = "(" ++ (show lhs) ++ "-" ++ (show rhs) ++ ")"
    show (Asterisk lhs rhs) = "(" ++ (show lhs) ++ "*" ++ (show rhs) ++ ")"

test1 = Plus (Number 5) (Asterisk (Number 6) (Minus (Number 1) (Number 2)))

eval :: Expression -> Int
eval (Number v)         = v
eval (Plus lhs rhs)     = (eval lhs) + (eval rhs)
eval (Minus lhs rhs)    = (eval lhs) - (eval rhs)
eval (Asterisk lhs rhs) = (eval lhs) * (eval rhs)