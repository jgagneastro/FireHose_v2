;+ 
; NAME:
;       FINDIDX (formerly TABINV)     
; PURPOSE:  
;       Finds the effective index of a function value in an ordered vector.
;
; CALLING SEQUENCE:
;       FINDIDX, XARR, X, IEFF
; INPUTS:
;       XARR - the vector array to be searched, must be monotonic
;               increasing or decreasing
;       X    - the function value(s) whose effective
;               index is sought (scalar or vector)
;
; OUTPUT:
;       IEFF - the effective index or indices of X in XARR
;               real or double precision, same # of elements as X
;
; RESTRICTIONS:
;       FINDIDX will abort if XARR is not monotonic.  (Equality of 
;       neighboring values in XARR is allowed but results may not be
;       unique.)  This requirement may mean that input vectors with padded
;       zeroes could cause routine to abort.
;
; PROCEDURE:
;       A binary search is used to find the values XARR(I)
;       and XARR(I+1) where XARR(I) < X < XARR(I+1).
;       IEFF is then computed using linear interpolation 
;       between I and I+1.
;               IEFF = I + (X-XARR(I)) / (XARR(I+1)-XARR(I))
;       Let N = number of elements in XARR
;               if x < XARR(0) then IEFF is set to !values.f_nan
;               if x > XARR(N-1) then IEFF is set to !values.f_nan
;
; EXAMPLE:
;       Set all flux values of a spectrum (WAVE vs FLUX) to zero
;       for wavelengths less than 1150 Angstroms.
;         
;       IDL> tabinv, wave, 1150.0, I
;       IDL> flux( 0:fix(I) ) = 0.                         
;
; FUNCTIONS CALLED:
;       ISARRAY()
; REVISION HISTORY:
;       Adapted from the IUE RDAF                     January, 1988         
;       More elegant code  W. Landsman                August, 1989
;       Mod to work on 2 element decreasing vector    August, 1992
;       Converted to IDL V5.0   W. Landsman   September 1997
;       2001-09-15 - Sets values out of range to !values.f_nan
;                    M. Cushing Institute for Astronomy, UH
;-      
PRO FINDIDX, XARR, X, IEFF
         
 On_error,2

 if N_params() LT 3 then begin
     print,'Syntax- FINDIDX, XARR, X, I'
     return
 endif

 Npoints = N_elements(xarr) & npt= npoints - 1

 if ( Npoints LE 1 ) then message, /TRACE, $
   'Search vector (first parameter) must contain at least 2 elements'

; Initialize binary search area and compute number of divisions needed

 ileft = intarr( N_elements(x) ) & iright = ileft

 ndivisions = fix( alog10(npoints) / alog10(2.0)+1.0 )

; Test for monotonicity 

 i = xarr - shift( xarr,1)
 i = i[1:*]               ;Added 15-Aug to properly interpret 2 element
 a = where( i GE 0, N)    ;decreasing vector

 if ( N EQ npt) then $ ; Increasing array ?

    iright = iright + npt $

 else begin

     a = where(i LE 0, N)  ; Test for decreasing array
     if ( N EQ npt ) then ileft = ileft + npt $    
     ELSE message, /TRACE, $
       'ERROR - First parameter must be a monotonic vector' 

endelse          

; Perform binary search by dividing search interval in half NDIVISIONS times

 for i = 1, ndivisions do begin    

    idiv = (ileft + iright) / 2      ;Split interval in half
    xval = xarr[idiv]                ;Find function values at center
    greater = ( x GT xval )          ;Determine which side X is on
    less   = ( x LE xval )  
    ileft =  ileft*less + idiv*greater    ;Compute new search area
    iright = iright*greater + idiv*less     

 endfor

; Interpolate between interval of width = 1

 xleft =  xarr[ileft]              ;Value on left side
 xright = xarr[iright]             ;Value on right side

 ieff = (xright-x)*ileft + (x-xleft)*iright + ileft*( xright EQ xleft ) 

 ieff = ieff / double( xright - xleft + ( xright EQ xleft ))    ;Interpolate

 ieff = ieff > 0.0 < npt        ;Do not allow extrapolation beyond ends



 z1 = where(x lt xarr[0],count1)
 z2 = where(x gt xarr[npt],count2)



 if count1 ne 0 then ieff[z1] = !values.f_nan
 if count2 ne 0 then ieff[z2] = !values.f_nan



 if not ISARRAY(x) then ieff = ieff[0]  ;Make scalar if X was scalar

 return
 end
