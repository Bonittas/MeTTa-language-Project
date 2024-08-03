; Commutative law using =
(= (commutative_law_and ($A $B)) ($B $A))
(= (commutative_law_or ($A $B)) ($B $A))

; Identity law
(= (identity_law_or ($A 0)) $A)
(= (identity_law_and ($A 1)) $A)

; Null law
(= (null_law_or ($A 1)) 1)
(= (null_law_and ($A 0)) 0)

; Complement law
(= (complement_law_or ($A (not $A))) 1)
(= (complement_law_and ($A (not $A))) 0)

; Idempotent law
(= (idempotent_law_or ($A $A)) $A)
(= (idempotent_law_and ($A $A)) $A)

; Associative law
(= (associative_law_or ($A ($B $C))) (($A $B) $C))
(= (associative_law_and ($A ($B $C))) (($A $B) $C))

; Distributive law
(= (distributive_law_and ($A ($B $C))) (($A $B) ($A $C)))
(= (distributive_law_or ($A ($B $C))) (($A $B) ($A $C)))

; De Morgan's law
(= (demorgans_law_not_or (not ($A $B))) ((not $A) (not $B)))
(= (demorgans_law_not_and (not ($A $B))) ((not $A) (not $B)))

; Test cases for commutative law
! (commutative_law_and (P Q))
! (commutative_law_or (R S))

; Test cases for identity law
! (identity_law_or (T 0))
! (identity_law_and (U 1))

; Test cases for null law
! (null_law_or (V 1))
! (null_law_and (W 0))

; Test cases for complement law
! (complement_law_or (X (not X)))
! (complement_law_and (Y (not Y)))

; Test cases for idempotent law
! (idempotent_law_or (Z Z))
! (idempotent_law_and (A1 A1))

; Test cases for associative law
! (associative_law_or (B1 (C1 D1)))
! (associative_law_and (E1 (F1 G1)))

; Test cases for distributive law
! (distributive_law_and (H1 (I1 J1)))
! (distributive_law_or (K1 (L1 M1)))

; Test cases for De Morgan's law
! (demorgans_law_not_or (not (N1 O1)))
! (demorgans_law_not_and (not (P1 Q1)))