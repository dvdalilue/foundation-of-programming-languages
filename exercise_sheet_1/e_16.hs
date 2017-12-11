data Program = Empty | 
               Sequence    Program Program | 
               Assignment  Variable Expression |
               Conditional Expression Program Program |
               While       Expression Program
    deriving Show

data Variable = Identifier String
    deriving Show

data Expression = Var       Variable              |
                  Number    Int                   |
                  True                            |
                  False                           |
                  Plus      Expression Expression |
                  Minus     Expression Expression |
                  Asterisk  Expression Expression |
                  Equal     Expression Expression |
                  Less      Expression Expression |
                  LessEq    Expression Expression |
                  Greater   Expression Expression |
                  GreaterEq Expression Expression |
                  Unequal   Expression Expression
    deriving Show

{- Example program

    x := 4 + (1 * 1);;
    while (x < 10)
        x := x + 1

-}
simpleP = Sequence 
            (Sequence 
                (Assignment (Identifier "x") (Plus (Number 4) (Asterisk (Number 1) (Number 1)))) 
                Empty
            ) 
            (While (Less (Var (Identifier "x")) (Number 10)) 
                (Assignment (Identifier "x") (Plus (Var (Identifier "x")) (Number 1)))
            )