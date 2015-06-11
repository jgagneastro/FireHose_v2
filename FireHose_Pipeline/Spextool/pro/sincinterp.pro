;+
; NAME:
;     sincinterp
;    
; PURPOSE:
;     To perform a sinc interpolation on a data set.
;    
; CATEGORY:
;     Mathematical
;
; CALLING SEQUENCE:
;     results = sincinterp(xa,ya,x,CANCEL=cancel)
;
; INPUTS:
;     xa - The independent values of the data
;     ya - The dependent values of the data
;     x  - The new independent values of the data
;    
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;     
; OUTPUTS:
;     YA sampled at x.
;
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     None
;
; SIDE EFFECTS:
;     None
;
; RESTRICTIONS:
;     None
;
; PROCEDURE:
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;    Originally sint.pro from the Buie libray.
;    Written by Doug Loucks, Lowell Observatory, September, 1993.
;    Adapted from the IDL function sshift.pro written by Marc Buie.
;    01/14/94, DWL, Documentation update.
;
;    2003-06-03 - Modified to allow an indenpendent array input,
;                 M. Cushing, Institute for Astronomy
;    2004-09-01 - Modified loop to be long integer, M. Cushing, NASA Ames
;
;-
function sincinterp, xa,ya,x,CANCEL=cancel

cancel = 0

if n_params() ne 3 then begin

    print, 'Syntax - results = sincinterp(xa,ya,x,CANCEL=cancel)'
    cancel = 1
    return, -1

endif
cancel = cpar('sincinterp',xa,'Xa',1,[2,3,4,5],1)
if cancel then return,-1
cancel = cpar('sincinterp',ya,'Ya',2,[2,3,4,5],1)
if cancel then return,-1
cancel = cpar('sincinterp',x,'X',3,[2,3,4,5],1)
if cancel then return,-1


tabinv,xa,x,idx

f  = ya
xx  = idx

dampfac = 3.25
ksize   = 21

nx = N_ELEMENTS( xx )
nf = N_ELEMENTS( f )

ix = FIX( xx )
fx = xx - ix
z = WHERE( fx EQ 0, countz )
i = WHERE( fx NE 0, counti )

r = xx * 0

IF countz NE 0 THEN BEGIN
   ;There are integral values of the independent variable. Select the function
   ;values directly for these points.
   r[ z ] = f[ ix[z] ]
ENDIF

IF counti NE 0 THEN BEGIN
   ;Use sinc interpolation for the points having fractional values.
   FOR point=0L, long(counti)-1L DO BEGIN
      xkernel = ( FINDGEN( ksize ) - 10 ) - fx[ i[ point ] ]
      u1 = xkernel / dampfac
      u2 = !pi * xkernel
      sinc = EXP( -( u1*u1 ) ) * SIN( u2 ) / u2
      lobe = ( INDGEN( ksize ) - 10 ) + ix[ i[point] ]
      vals = FLTARR( ksize )
      w = WHERE( lobe LT 0, count )
      IF count NE 0 THEN vals[w] = f[0]
      w = WHERE( lobe GE 0 AND lobe LT nf, count )
      IF count NE 0 THEN vals[w] = f[ lobe[w] ]
      w = WHERE( lobe GE nf, count )
      IF count NE 0 THEN vals[w] = f[ nf-1 ]
      r[ i[ point ] ] = TOTAL( sinc * vals )
   ENDFOR
ENDIF

RETURN, r

END
