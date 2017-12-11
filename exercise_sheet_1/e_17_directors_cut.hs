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

extToSmp :: ExtendedRubikMovement -> SimpleRubikMovement
extToSmp moves = useMap idM moves
    where
        useMap f (UE next) = f $ U (useMap f next)
        useMap f (LE next) = f $ L (useMap f next)
        useMap f (FE next) = f $ F (useMap f next)
        useMap f (RE next) = f $ R (useMap f next)
        useMap f (BE next) = f $ B (useMap f next)
        useMap f (DE next) = f $ D (useMap f next)
        useMap f (XE next) = (useMap (rotateX' . f) next)
        useMap f (YE next) = (useMap (rotateY' . f) next)
        useMap f (ZE next) = (useMap (rotateZ' . f) next)
        useMap f StopE     = Stop

        idM :: SimpleRubikMovement -> SimpleRubikMovement
        idM (U n) = (U n)
        idM (L n) = (L n)
        idM (F n) = (F n)
        idM (R n) = (R n)
        idM (B n) = (B n)
        idM (D n) = (D n)

        rotateX' :: SimpleRubikMovement -> SimpleRubikMovement
        rotateX' (U n) = (F n)
        rotateX' (F n) = (D n)
        rotateX' (D n) = (B n)
        rotateX' (B n) = (U n)
        rotateX' (L n) = (L n)
        rotateX' (R n) = (R n)

        rotateY' :: SimpleRubikMovement -> SimpleRubikMovement
        rotateY' (L n) = (F n)
        rotateY' (F n) = (R n)
        rotateY' (R n) = (B n)
        rotateY' (B n) = (L n)
        rotateY' (U n) = (U n)
        rotateY' (D n) = (D n)

        rotateZ' :: SimpleRubikMovement -> SimpleRubikMovement
        rotateZ' (L n) = (D n)
        rotateZ' (D n) = (R n)
        rotateZ' (R n) = (U n)
        rotateZ' (U n) = (L n)
        rotateZ' (F n) = (F n)
        rotateZ' (B n) = (B n)

mapExtendedToSimple :: ExtendedRubikMovement -> SimpleRubikMovement
mapExtendedToSimple moves = auxMap $ check moves
    where
        auxMap (UE next) = U (auxMap next)
        auxMap (LE next) = L (auxMap next)
        auxMap (FE next) = F (auxMap next)
        auxMap (RE next) = R (auxMap next)
        auxMap (BE next) = B (auxMap next)
        auxMap (DE next) = D (auxMap next)
        auxMap StopE     = Stop

        check :: ExtendedRubikMovement -> ExtendedRubikMovement
        check (UE next) = UE (check next)
        check (LE next) = LE (check next)
        check (FE next) = FE (check next)
        check (RE next) = RE (check next)
        check (BE next) = BE (check next)
        check (DE next) = DE (check next)
        check (XE next) = rotateX next
        check (YE next) = rotateY next
        check (ZE next) = rotateZ next
        check StopE = StopE

        rotateX :: ExtendedRubikMovement -> ExtendedRubikMovement
        rotateX (UE next) = FE (rotateX next)
        rotateX (FE next) = DE (rotateX next)
        rotateX (DE next) = BE (rotateX next)
        rotateX (BE next) = UE (rotateX next)
        rotateX (LE next) = LE (rotateX next)
        rotateX (RE next) = RE (rotateX next)
        rotateX (XE next) = rotateX $ rotateX next 
        rotateX (YE next) = rotateY $ rotateX next
        rotateX (ZE next) = rotateZ $ rotateX next
        rotateX m = check m

        rotateY :: ExtendedRubikMovement -> ExtendedRubikMovement
        rotateY (LE next) = FE (rotateY next)
        rotateY (FE next) = RE (rotateY next)
        rotateY (RE next) = BE (rotateY next)
        rotateY (BE next) = LE (rotateY next)
        rotateY (UE next) = UE (rotateY next)
        rotateY (DE next) = DE (rotateY next)
        rotateY (XE next) = rotateX $ rotateY next
        rotateY (YE next) = rotateY $ rotateY next
        rotateY (ZE next) = rotateZ $ rotateY next
        rotateY m = check m

        rotateZ :: ExtendedRubikMovement -> ExtendedRubikMovement
        rotateZ (LE next) = DE (rotateZ next)
        rotateZ (DE next) = RE (rotateZ next)
        rotateZ (RE next) = UE (rotateZ next)
        rotateZ (UE next) = LE (rotateZ next)
        rotateZ (FE next) = FE (rotateZ next)
        rotateZ (BE next) = BE (rotateZ next)
        rotateZ (XE next) = rotateX $ rotateZ next
        rotateZ (YE next) = rotateY $ rotateZ next
        rotateZ (ZE next) = rotateZ $ rotateZ next
        rotateZ m = check m