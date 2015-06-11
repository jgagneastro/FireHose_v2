;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Document name: gaussj.pro
; Created by:    Liyun Wang, GSFC/ARC, November 10, 1994
;
; Last Modified: Sun Nov 13 21:15:43 1994 (lwang@orpheus.nascom.nasa.gov)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
PRO GAUSSJ, aa, bb, inverted=inverted, solution=solution
;+
; PROJECT:
;       SOHO - CDS
;
; NAME:
;       GAUSSJ
;
; PURPOSE:
;       Linear equation solution by Gauss-Jordan elimination
;
; EXPLANATION:
;       This is a routine translated from a Fortran subroutine of the
;       Numerical Recipes.
;
; CALLING SEQUENCE:
;       GAUSSJ, aa, bb
;
; INPUTS:
;       AA -- Coefficient matrix with N x N elements
;       BB -- Constant matrix of N x M elements, containing the M right-hand
;             side vectors
;
;       Note: N is number of unknown variabled to be solved, and therefore is
;             number of linear equations; M is number of vectors (each with N
;             elements) on the right-hand side of equations.
;
; OPTIONAL INPUTS:
;       None.
;
; OUTPUTS:
;       SOLUTION -- Corresponding set of solution vectors
;
; OPTIONAL OUTPUTS:
;       INVERTED -- Inversed matrix of input matrix AA
;
; KEYWORD PARAMETERS:
;       None.
;
; CALLS:
;       None.
;
; COMMON BLOCKS:
;       None.
;
; RESTRICTIONS:
;       None.
;
; SIDE EFFECTS:
;       None.
;
; CATEGORY:
;       Ulitities, numerical
;
; EXAMPLE:
;       To solve for the following linear equation:
;
;         | 1 -1  1 |     | -4  |
;         | 5 -4  3 | X = | -12 |
;         | 2  1  1 |     | 11  |
;
;       The matrix AA and BB should be:
;          aa = [[1,5,2],[-1,-4,1],[1,3,1]]
;          bb = [[-4,-12,11],[15,56,13]]
;       and the result will be:
;          bb = [[3,6,-1],[4,-3,8]]
;
; PREVIOUS HISTORY:
;       Written November 10, 1994, by Liyun Wang, GSFC/ARC
;
; MODIFICATION HISTORY:
;
; VERSION:
;       Version 1, November 10, 1994
;-
;
   ON_ERROR, 2
   aa_size = SIZE(aa)
   bb_size = SIZE(bb)
   IF aa_size(1) NE aa_size(2) THEN MESSAGE, 'Matrix AA must be square matrix.'
   IF aa_size(1) NE bb_size(1) THEN MESSAGE, $
      'Matrix BB is not compatible with AA.'
   n = aa_size(1)
   IF bb_size(0) EQ 1 THEN m = 1 ELSE m = bb_size(2)
   ipiv = intarr(n) & indxc = intarr(n) & indxr = intarr(n)
   aa_new = DOUBLE(aa)
   bb_new = DOUBLE(bb)
   FOR i = 0, n-1 DO BEGIN
      big = 0.0
      FOR j = 0, n-1 DO BEGIN
         IF ipiv(j) NE 1 THEN BEGIN
            FOR k = 0, n-1 DO BEGIN
               IF ipiv(k) EQ 0 THEN BEGIN
                  IF ABS(aa_new(j,k)) GE big THEN BEGIN
                     big = ABS(aa_new(j,k))
                     irow = j
                     icol = k
                  ENDIF
               ENDIF ELSE BEGIN
                  IF ipiv(k) GT 1 THEN MESSAGE, 'Singular matrix.'
               ENDELSE
            ENDFOR
         ENDIF
      ENDFOR
      ipiv(icol) = ipiv(icol)+1
      IF irow NE icol THEN BEGIN
;          FOR l = 0, n-1 DO BEGIN
;             dum = aa_new(irow,l)
;             aa_new(irow,l) = aa_new(icol,l)
;             aa_new(icol,l) = dum
;          ENDFOR
;          FOR l = 0, m-1 DO BEGIN
;             dum = bb_new(irow,l)
;             bb_new(irow,l) = bb_new(icol,l)
;             bb_new(icol,l) = dum
;          ENDFOR
         dum = aa_new(irow,*)
         aa_new(irow,*) = aa_new(icol,*)
         aa_new(icol,*) = dum
         dum = bb_new(irow,*)
         bb_new(irow,*) = bb_new(icol,*)
         bb_new(icol,*) = dum
      ENDIF
      indxr(i) = irow
      indxc(i) = icol
      IF aa_new(icol,icol) EQ 0.0 THEN MESSAGE, 'Singular matrix.'
      pivinv = 1./aa_new(icol,icol)
      aa_new(icol,icol) = 1.
      aa_new(icol,*) = aa_new(icol,*)*pivinv
      bb_new(icol,*) = bb_new(icol,*)*pivinv
      FOR ll = 0, n-1 DO BEGIN
         IF ll NE icol THEN BEGIN
            dum = aa_new(ll,icol)
            aa_new(ll,icol) = 0.0
            aa_new(ll,*) = aa_new(ll,*)-aa_new(icol,*)*dum
            bb_new(ll,*) = bb_new(ll,*)-bb_new(icol,*)*dum
         ENDIF
      ENDFOR
   ENDFOR
   FOR l = n-1, 0, -1 DO BEGIN
      IF indxr(l) NE indxc(l) THEN BEGIN
         FOR k = 0, n-1 DO BEGIN
            dum = aa_new(k,indxr(l))
            aa_new(k,indxr(l)) = aa_new(k,indxc(l))
            aa_new(k,indxc(l)) = dum
         ENDFOR
      ENDIF
   ENDFOR
   inverted = aa_new
   solution = bb_new
END

;----------------------------------------------------------------------
;  Testing part:
;----------------------------------------------------------------------
; aa = [[1,5,2],[-1,-4,1],[1,3,1]]
; bb = [[-4,-12,11],[15,56,13]]
; gaussj,aa,bb
; PRINT, bb
; END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; End of 'gaussj.pro'.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


