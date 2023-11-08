;;; -*- Mode: LISP; package:maxima; syntax:common-lisp; -*- 
(in-package :maxima)
(|MAXIMA|::|DSKSETQ| |MAXIMA|::|$varsC| '((|MAXIMA|::|MLIST| |MAXIMA|::|SIMP|) |MAXIMA|::|$X|)) 
(|MAXIMA|::|ADD2LNC| '|MAXIMA|::|$varsC| |MAXIMA|::|$VALUES|) 
(|MAXIMA|::|DSKSETQ| |MAXIMA|::|$varsP|
 '((|MAXIMA|::|MLIST| |MAXIMA|::|SIMP|) |MAXIMA|::|$X| |MAXIMA|::|$VX|)) 
(|MAXIMA|::|ADD2LNC| '|MAXIMA|::|$varsP| |MAXIMA|::|$VALUES|) 
(|MAXIMA|::|DSKSETQ| |MAXIMA|::|$basisC|
 '((|MAXIMA|::|MLIST| |MAXIMA|::|SIMP|
    (37. "/work/03485/jonroelt/maxima/gkylcas/maxima/g0/basis-precalc/basis-pre-calc-hybrid.mac"
     |MAXIMA|::|SRC| |MAXIMA|::|$writeGkHybBasisToFile| 34.))
   ((|MAXIMA|::|MLIST| |MAXIMA|::|SIMP|
     (33. "/work/03485/jonroelt/maxima/gkylcas/maxima/g0/modal-basis.mac" |MAXIMA|::|SRC|
      |MAXIMA|::|$gsOrthoNorm| 31.))
    ((|MAXIMA|::|MEXPT| |MAXIMA|::|SIMP|) 2. ((|MAXIMA|::|RAT| |MAXIMA|::|SIMP|) -1. 2.))
    ((|MAXIMA|::|MTIMES| |MAXIMA|::|SIMP|)
     ((|MAXIMA|::|MEXPT| |MAXIMA|::|SIMP|) 2. ((|MAXIMA|::|RAT| |MAXIMA|::|SIMP|) -1. 2.))
     ((|MAXIMA|::|MEXPT| |MAXIMA|::|SIMP|) 3. ((|MAXIMA|::|RAT| |MAXIMA|::|SIMP|) 1. 2.))
     |MAXIMA|::|$X|)))) 
(|MAXIMA|::|ADD2LNC| '|MAXIMA|::|$basisC| |MAXIMA|::|$VALUES|) 
(|MAXIMA|::|DSKSETQ| |MAXIMA|::|$basisP|
 '((|MAXIMA|::|MLIST| |MAXIMA|::|SIMP|
    (38. #1="/work/03485/jonroelt/maxima/gkylcas/maxima/g0/basis-precalc/basis-pre-calc-hybrid.mac"
     |MAXIMA|::|SRC| |MAXIMA|::|$writeGkHybBasisToFile| 34.))
   ((|MAXIMA|::|MLIST| |MAXIMA|::|SIMP|
     (33. "/work/03485/jonroelt/maxima/gkylcas/maxima/g0/modal-basis.mac" |MAXIMA|::|SRC|
      |MAXIMA|::|$gsOrthoNorm| 31.))
    ((|MAXIMA|::|RAT| |MAXIMA|::|SIMP|) 1. 2.)
    (#2=(|MAXIMA|::|MTIMES| |MAXIMA|::|SIMP|) ((|MAXIMA|::|RAT| |MAXIMA|::|SIMP|) 1. 2.)
     ((|MAXIMA|::|MEXPT| |MAXIMA|::|SIMP|) 3. ((|MAXIMA|::|RAT| |MAXIMA|::|SIMP|) 1. 2.))
     |MAXIMA|::|$X|)
    (#2# ((|MAXIMA|::|RAT| |MAXIMA|::|SIMP|) 1. 2.)
     ((|MAXIMA|::|MEXPT| |MAXIMA|::|SIMP|) 3. ((|MAXIMA|::|RAT| |MAXIMA|::|SIMP|) 1. 2.))
     |MAXIMA|::|$VX|)
    (#2# ((|MAXIMA|::|RAT| |MAXIMA|::|SIMP|) 3. 2.) |MAXIMA|::|$VX| |MAXIMA|::|$X|)
    (#2# ((|MAXIMA|::|RAT| |MAXIMA|::|SIMP|) 3. 4.)
     ((|MAXIMA|::|MEXPT| |MAXIMA|::|SIMP|) 5. ((|MAXIMA|::|RAT| |MAXIMA|::|SIMP|) 1. 2.))
     ((|MAXIMA|::|MPLUS| |MAXIMA|::|SIMP|) ((|MAXIMA|::|RAT| |MAXIMA|::|SIMP|) -1. 3.)
      ((|MAXIMA|::|MEXPT| |MAXIMA|::|SIMP|
        (39. #1# |MAXIMA|::|SRC| |MAXIMA|::|$writeGkHybBasisToFile| 34.))
       |MAXIMA|::|$VX| 2.)))
    (#2# ((|MAXIMA|::|RAT| |MAXIMA|::|SIMP|) 3. 4.)
     ((|MAXIMA|::|MEXPT| |MAXIMA|::|SIMP|) 15. ((|MAXIMA|::|RAT| |MAXIMA|::|SIMP|) 1. 2.))
     ((|MAXIMA|::|MPLUS| |MAXIMA|::|SIMP|)
      ((|MAXIMA|::|MTIMES| |MAXIMA|::|SIMP|) ((|MAXIMA|::|RAT| |MAXIMA|::|SIMP|) -1. 3.)
       |MAXIMA|::|$X|)
      ((|MAXIMA|::|MTIMES| |MAXIMA|::|SIMP|)
       ((|MAXIMA|::|MEXPT| |MAXIMA|::|SIMP|
         (39. #1# |MAXIMA|::|SRC| |MAXIMA|::|$writeGkHybBasisToFile| 34.))
        |MAXIMA|::|$VX| 2.)
       |MAXIMA|::|$X|)))))) 
(|MAXIMA|::|ADD2LNC| '|MAXIMA|::|$basisP| |MAXIMA|::|$VALUES|) 