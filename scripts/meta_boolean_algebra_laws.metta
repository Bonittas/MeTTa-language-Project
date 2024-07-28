; Commutative Law
(= (commutative_law_and ($A $B)) ($B $A))
(= (commutative_law_or ($A $B)) ($B $A))

; Associative Law
(= (associative_law_and ($A ($B $C))) (($A $B) $C))
(= (associative_law_or ($A ($B $C))) (($A $B) $C))

; Distributive Law
(= (distributive_law_and ($A ($B $C))) (($A $B) ($A $C)))
(= (distributive_law_or ($A ($B $C))) (($A $B) ($A $C)))

; Identity Law
(= (identity_law_and ($A 1)) $A)
(= (identity_law_or ($A 0)) $A)

; Null Law
(= (null_law_and ($A 0)) 0)
(= (null_law_or ($A 1)) 1)

; Complement Law
(= (complement_law_and ($A (not $A))) 0)
(= (complement_law_or ($A (not $A))) 1)

; Idempotent Law
(= (idempotent_law_and ($A $A)) $A)
(= (idempotent_law_or ($A $A)) $A)

; Dominance Law
(= (dominance_law_and ($A 0)) 0)
(= (dominance_law_or ($A 1)) 1)

; Absorption Law
(= (absorption_law_and ($A ($A $B))) $A)
(= (absorption_law_or ($A ($A $B))) $A)

; De Morgan's Law
(= (demorgans_law_not_and (not ($A $B))) ((not $A) (not $B)))
(= (demorgans_law_not_or (not ($A $B))) ((not $A) (not $B)))

; Test cases for commutative law
! (commutative_law_and (P Q))
! (commutative_law_or (R S))

; Test cases for associative law
! (associative_law_and (T (U V)))
! (associative_law_or (W (X Y)))

; Test cases for distributive law
! (distributive_law_and (A1 (B1 C1)))
! (distributive_law_or (D1 (E1 F1)))

; Test cases for identity law
! (identity_law_and (G1 1))
! (identity_law_or (H1 0))

; Test cases for null law
! (null_law_and (I1 0))
! (null_law_or (J1 1))

; Test cases for complement law
! (complement_law_and (K1 (not K1)))
! (complement_law_or (L1 (not L1)))

; Test cases for idempotent law
! (idempotent_law_and (M1 M1))
! (idempotent_law_or (N1 N1))

; Test cases for dominance law
! (dominance_law_and (O1 0))
! (dominance_law_or (P1 1))

; Test cases for absorption law
! (absorption_law_and (Q1 (Q1 R1)))
! (absorption_law_or (S1 (S1 T1)))

; Test cases for De Morgan's law
! (demorgans_law_not_and (not (U1 V1)))
! (demorgans_law_not_or (not (W1 X1)))
