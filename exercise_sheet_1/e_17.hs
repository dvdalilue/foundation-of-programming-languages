data SimpleRubikMovement = U SimpleRubikMovement |
                           L SimpleRubikMovement |
                           F SimpleRubikMovement |
                           R SimpleRubikMovement |
                           B SimpleRubikMovement |
                           D SimpleRubikMovement |
                           Stop
    deriving Show

data ExtendedRubikMovement = UE ExtendedRubikMovement |
                             LE ExtendedRubikMovement |
                             FE ExtendedRubikMovement |
                             RE ExtendedRubikMovement |
                             BE ExtendedRubikMovement |
                             DE ExtendedRubikMovement |
                             XE ExtendedRubikMovement |
                             YE ExtendedRubikMovement |
                             ZE ExtendedRubikMovement |
                             StopE
    deriving Show

mapExtendedToSimple :: ExtendedRubikMovement -> SimpleRubikMovement
mapExtendedToSimple moves = auxMap idM moves
    where
        auxMap f (UE next) = f $ U (auxMap f next)
        auxMap f (LE next) = f $ L (auxMap f next)
        auxMap f (FE next) = f $ F (auxMap f next)
        auxMap f (RE next) = f $ R (auxMap f next)
        auxMap f (BE next) = f $ B (auxMap f next)
        auxMap f (DE next) = f $ D (auxMap f next)
        auxMap f (XE next) = auxMap (rotateX . f) next
        auxMap f (YE next) = auxMap (rotateY . f) next
        auxMap f (ZE next) = auxMap (rotateZ . f) next
        auxMap f StopE     = Stop

        idM :: SimpleRubikMovement -> SimpleRubikMovement
        idM (U n) = (U n)
        idM (L n) = (L n)
        idM (F n) = (F n)
        idM (R n) = (R n)
        idM (B n) = (B n)
        idM (D n) = (D n)

        rotateX :: SimpleRubikMovement -> SimpleRubikMovement
        rotateX (U n) = (F n)
        rotateX (F n) = (D n)
        rotateX (D n) = (B n)
        rotateX (B n) = (U n)
        rotateX (L n) = (L n)
        rotateX (R n) = (R n)

        rotateY :: SimpleRubikMovement -> SimpleRubikMovement
        rotateY (L n) = (F n)
        rotateY (F n) = (R n)
        rotateY (R n) = (B n)
        rotateY (B n) = (L n)
        rotateY (U n) = (U n)
        rotateY (D n) = (D n)

        rotateZ :: SimpleRubikMovement -> SimpleRubikMovement
        rotateZ (L n) = (D n)
        rotateZ (D n) = (R n)
        rotateZ (R n) = (U n)
        rotateZ (U n) = (L n)
        rotateZ (F n) = (F n)
        rotateZ (B n) = (B n)

ex1 = R (L (D (F (F (B Stop)))))

ex2 = RE (DE (FE (BE (XE (BE (RE (DE (StopE))))))))