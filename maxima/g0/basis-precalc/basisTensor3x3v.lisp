;;; -*- Mode: LISP; package:maxima; syntax:common-lisp; -*- 
(in-package :maxima)
(DSKSETQ |$varsC| '((MLIST SIMP) $X $Y $Z)) 
(ADD2LNC '|$varsC| $VALUES) 
(DSKSETQ |$varsP| '((MLIST SIMP) $X $Y $Z $VX $VY $VZ)) 
(ADD2LNC '|$varsP| $VALUES) 
(DSKSETQ |$basisC|
         '((MLIST SIMP
            (10.
             #A((67.) BASE-CHAR
                . "/Users/junoravin/gkylcas/maxima/g0/basis-precalc/basis-pre-calc.mac")
             SRC |$writeBasisToFile| 7.))
           ((MLIST SIMP
             (33.
              #A((50.) BASE-CHAR
                 . "/Users/junoravin/gkylcas/maxima/g0/modal-basis.mac")
              SRC |$gsOrthoNorm| 31.))
            ((MEXPT SIMP) 2. ((RAT SIMP) -3. 2.))
            ((MTIMES SIMP) ((MEXPT SIMP) 2. ((RAT SIMP) -3. 2.))
             ((MEXPT SIMP) 3. ((RAT SIMP) 1. 2.)) $X)
            ((MTIMES SIMP) ((MEXPT SIMP) 2. ((RAT SIMP) -3. 2.))
             ((MEXPT SIMP) 3. ((RAT SIMP) 1. 2.)) $Y)
            ((MTIMES SIMP) ((MEXPT SIMP) 2. ((RAT SIMP) -3. 2.))
             ((MEXPT SIMP) 3. ((RAT SIMP) 1. 2.)) $Z)
            ((MTIMES SIMP) 3. ((MEXPT SIMP) 2. ((RAT SIMP) -3. 2.)) $X $Y)
            ((MTIMES SIMP) 3. ((MEXPT SIMP) 2. ((RAT SIMP) -3. 2.)) $X $Z)
            ((MTIMES SIMP) 3. ((MEXPT SIMP) 2. ((RAT SIMP) -3. 2.)) $Y $Z)
            ((MTIMES SIMP) ((MEXPT SIMP) 2. ((RAT SIMP) -3. 2.))
             ((MEXPT SIMP) 3. ((RAT SIMP) 3. 2.)) $X $Y $Z)))) 
(ADD2LNC '|$basisC| $VALUES) 
(DSKSETQ |$basisP|
         '((MLIST SIMP
            (10.
             #A((67.) BASE-CHAR
                . "/Users/junoravin/gkylcas/maxima/g0/basis-precalc/basis-pre-calc.mac")
             SRC |$writeBasisToFile| 7.))
           ((MLIST SIMP
             (33.
              #A((50.) BASE-CHAR
                 . "/Users/junoravin/gkylcas/maxima/g0/modal-basis.mac")
              SRC |$gsOrthoNorm| 31.))
            ((RAT SIMP) 1. 8.)
            ((MTIMES SIMP) ((RAT SIMP) 1. 8.)
             ((MEXPT SIMP) 3. ((RAT SIMP) 1. 2.)) $X)
            ((MTIMES SIMP) ((RAT SIMP) 1. 8.)
             ((MEXPT SIMP) 3. ((RAT SIMP) 1. 2.)) $Y)
            ((MTIMES SIMP) ((RAT SIMP) 1. 8.)
             ((MEXPT SIMP) 3. ((RAT SIMP) 1. 2.)) $Z)
            ((MTIMES SIMP) ((RAT SIMP) 1. 8.)
             ((MEXPT SIMP) 3. ((RAT SIMP) 1. 2.)) $VX)
            ((MTIMES SIMP) ((RAT SIMP) 1. 8.)
             ((MEXPT SIMP) 3. ((RAT SIMP) 1. 2.)) $VY)
            ((MTIMES SIMP) ((RAT SIMP) 1. 8.)
             ((MEXPT SIMP) 3. ((RAT SIMP) 1. 2.)) $VZ)
            ((MTIMES SIMP) ((RAT SIMP) 3. 8.) $X $Y)
            ((MTIMES SIMP) ((RAT SIMP) 3. 8.) $X $Z)
            ((MTIMES SIMP) ((RAT SIMP) 3. 8.) $Y $Z)
            ((MTIMES SIMP) ((RAT SIMP) 3. 8.) $VX $X)
            ((MTIMES SIMP) ((RAT SIMP) 3. 8.) $VX $Y)
            ((MTIMES SIMP) ((RAT SIMP) 3. 8.) $VX $Z)
            ((MTIMES SIMP) ((RAT SIMP) 3. 8.) $VY $X)
            ((MTIMES SIMP) ((RAT SIMP) 3. 8.) $VY $Y)
            ((MTIMES SIMP) ((RAT SIMP) 3. 8.) $VY $Z)
            ((MTIMES SIMP) ((RAT SIMP) 3. 8.) $VX $VY)
            ((MTIMES SIMP) ((RAT SIMP) 3. 8.) $VZ $X)
            ((MTIMES SIMP) ((RAT SIMP) 3. 8.) $VZ $Y)
            ((MTIMES SIMP) ((RAT SIMP) 3. 8.) $VZ $Z)
            ((MTIMES SIMP) ((RAT SIMP) 3. 8.) $VX $VZ)
            ((MTIMES SIMP) ((RAT SIMP) 3. 8.) $VY $VZ)
            ((MTIMES SIMP) ((RAT SIMP) 1. 8.)
             ((MEXPT SIMP) 3. ((RAT SIMP) 3. 2.)) $X $Y $Z)
            ((MTIMES SIMP) ((RAT SIMP) 1. 8.)
             ((MEXPT SIMP) 3. ((RAT SIMP) 3. 2.)) $VX $X $Y)
            ((MTIMES SIMP) ((RAT SIMP) 1. 8.)
             ((MEXPT SIMP) 3. ((RAT SIMP) 3. 2.)) $VX $X $Z)
            ((MTIMES SIMP) ((RAT SIMP) 1. 8.)
             ((MEXPT SIMP) 3. ((RAT SIMP) 3. 2.)) $VX $Y $Z)
            ((MTIMES SIMP) ((RAT SIMP) 1. 8.)
             ((MEXPT SIMP) 3. ((RAT SIMP) 3. 2.)) $VY $X $Y)
            ((MTIMES SIMP) ((RAT SIMP) 1. 8.)
             ((MEXPT SIMP) 3. ((RAT SIMP) 3. 2.)) $VY $X $Z)
            ((MTIMES SIMP) ((RAT SIMP) 1. 8.)
             ((MEXPT SIMP) 3. ((RAT SIMP) 3. 2.)) $VY $Y $Z)
            ((MTIMES SIMP) ((RAT SIMP) 1. 8.)
             ((MEXPT SIMP) 3. ((RAT SIMP) 3. 2.)) $VX $VY $X)
            ((MTIMES SIMP) ((RAT SIMP) 1. 8.)
             ((MEXPT SIMP) 3. ((RAT SIMP) 3. 2.)) $VX $VY $Y)
            ((MTIMES SIMP) ((RAT SIMP) 1. 8.)
             ((MEXPT SIMP) 3. ((RAT SIMP) 3. 2.)) $VX $VY $Z)
            ((MTIMES SIMP) ((RAT SIMP) 1. 8.)
             ((MEXPT SIMP) 3. ((RAT SIMP) 3. 2.)) $VZ $X $Y)
            ((MTIMES SIMP) ((RAT SIMP) 1. 8.)
             ((MEXPT SIMP) 3. ((RAT SIMP) 3. 2.)) $VZ $X $Z)
            ((MTIMES SIMP) ((RAT SIMP) 1. 8.)
             ((MEXPT SIMP) 3. ((RAT SIMP) 3. 2.)) $VZ $Y $Z)
            ((MTIMES SIMP) ((RAT SIMP) 1. 8.)
             ((MEXPT SIMP) 3. ((RAT SIMP) 3. 2.)) $VX $VZ $X)
            ((MTIMES SIMP) ((RAT SIMP) 1. 8.)
             ((MEXPT SIMP) 3. ((RAT SIMP) 3. 2.)) $VX $VZ $Y)
            ((MTIMES SIMP) ((RAT SIMP) 1. 8.)
             ((MEXPT SIMP) 3. ((RAT SIMP) 3. 2.)) $VX $VZ $Z)
            ((MTIMES SIMP) ((RAT SIMP) 1. 8.)
             ((MEXPT SIMP) 3. ((RAT SIMP) 3. 2.)) $VY $VZ $X)
            ((MTIMES SIMP) ((RAT SIMP) 1. 8.)
             ((MEXPT SIMP) 3. ((RAT SIMP) 3. 2.)) $VY $VZ $Y)
            ((MTIMES SIMP) ((RAT SIMP) 1. 8.)
             ((MEXPT SIMP) 3. ((RAT SIMP) 3. 2.)) $VY $VZ $Z)
            ((MTIMES SIMP) ((RAT SIMP) 1. 8.)
             ((MEXPT SIMP) 3. ((RAT SIMP) 3. 2.)) $VX $VY $VZ)
            ((MTIMES SIMP) ((RAT SIMP) 9. 8.) $VX $X $Y $Z)
            ((MTIMES SIMP) ((RAT SIMP) 9. 8.) $VY $X $Y $Z)
            ((MTIMES SIMP) ((RAT SIMP) 9. 8.) $VX $VY $X $Y)
            ((MTIMES SIMP) ((RAT SIMP) 9. 8.) $VX $VY $X $Z)
            ((MTIMES SIMP) ((RAT SIMP) 9. 8.) $VX $VY $Y $Z)
            ((MTIMES SIMP) ((RAT SIMP) 9. 8.) $VZ $X $Y $Z)
            ((MTIMES SIMP) ((RAT SIMP) 9. 8.) $VX $VZ $X $Y)
            ((MTIMES SIMP) ((RAT SIMP) 9. 8.) $VX $VZ $X $Z)
            ((MTIMES SIMP) ((RAT SIMP) 9. 8.) $VX $VZ $Y $Z)
            ((MTIMES SIMP) ((RAT SIMP) 9. 8.) $VY $VZ $X $Y)
            ((MTIMES SIMP) ((RAT SIMP) 9. 8.) $VY $VZ $X $Z)
            ((MTIMES SIMP) ((RAT SIMP) 9. 8.) $VY $VZ $Y $Z)
            ((MTIMES SIMP) ((RAT SIMP) 9. 8.) $VX $VY $VZ $X)
            ((MTIMES SIMP) ((RAT SIMP) 9. 8.) $VX $VY $VZ $Y)
            ((MTIMES SIMP) ((RAT SIMP) 9. 8.) $VX $VY $VZ $Z)
            ((MTIMES SIMP) ((RAT SIMP) 1. 8.)
             ((MEXPT SIMP) 3. ((RAT SIMP) 5. 2.)) $VX $VY $X $Y $Z)
            ((MTIMES SIMP) ((RAT SIMP) 1. 8.)
             ((MEXPT SIMP) 3. ((RAT SIMP) 5. 2.)) $VX $VZ $X $Y $Z)
            ((MTIMES SIMP) ((RAT SIMP) 1. 8.)
             ((MEXPT SIMP) 3. ((RAT SIMP) 5. 2.)) $VY $VZ $X $Y $Z)
            ((MTIMES SIMP) ((RAT SIMP) 1. 8.)
             ((MEXPT SIMP) 3. ((RAT SIMP) 5. 2.)) $VX $VY $VZ $X $Y)
            ((MTIMES SIMP) ((RAT SIMP) 1. 8.)
             ((MEXPT SIMP) 3. ((RAT SIMP) 5. 2.)) $VX $VY $VZ $X $Z)
            ((MTIMES SIMP) ((RAT SIMP) 1. 8.)
             ((MEXPT SIMP) 3. ((RAT SIMP) 5. 2.)) $VX $VY $VZ $Y $Z)
            ((MTIMES SIMP) ((RAT SIMP) 27. 8.) $VX $VY $VZ $X $Y $Z)))) 
(ADD2LNC '|$basisP| $VALUES) 