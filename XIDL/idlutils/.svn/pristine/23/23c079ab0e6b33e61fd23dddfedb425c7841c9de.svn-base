;+
; NAME:
;   trace_fweight
;
; PURPOSE:
;   Recenter a trace using flux-weighting centers.
;
; CALLING SEQUENCE:
;   xnew = trace_fweight( fimage, xcen, ycen, [radius=radius, xerr=xerr, 
;    invvar=invvar, nfiber=nfiber] )
;
; INPUTS:
;   fimage     - Image
;   xcen       - Initial guesses for X centers
;   ycen       - Y positions corresponding to "xcen" (expected as integers)
;
; OPTIONAL KEYWORDS:
;   radius     - Radius for centroiding; default to 3.0
;   invvar     - Inverse variance of image used only in computing errors XERR.
;                If not set, then INVVAR=1 is used.
;   nfiber     - an array of size [NBLOCK] containing the number of fibers in 
;                each v-groove block on the CCD.  NBLOCK is the number of v-groove blocks.
;                This is a MANGA-specific keyword!!
;
; OUTPUTS:
;   xnew       - New X centers
;
; OPTIONAL OUTPUTS:
;   xerr       - Formal errors for XNEW; set equal to 999.0 if there are any
;                masked pixels in a centroiding region (e.g., if INVVAR=0)
;
; COMMENTS:
;
; EXAMPLES:
;
; PROCEDURES CALLED:
;   Dynamic link to trace_fweight.c
;   ml_expand
;
; REVISION HISTORY:
;   24-Mar-1999  Written by David Schlegel, Princeton.
;   27-Dec-2012  Modified by Brian Cherinka, Toronto, for MaNGA survey.
;                Added nfiber keyword to handle a varying # of fibers per block; also for variable width boxcar extraction across the CCD
;-
;------------------------------------------------------------------------------
function trace_fweight, fimage, xcen, ycen, radius=radius, xerr=xerr, $
 invvar=invvar, idl=idl, nfiber=nfiber

   ; Need 3 parameters
   if (N_params() LT 3) then begin
      print, 'Syntax - xnew = trace_fweight( fimage, xcen, ycen, [radius=radius, $'
      print, ' xerr=xerr, invvar=invvar, nfiber=nfiber] )'
      return, -1
   endif
   if (~keyword_set(radius)) then radius = 3.0

   nx = (size(fimage))[1]
   ny = (size(fimage))[2]
   ncen = N_elements(xcen)
   xnew = float(xcen)
   xerr = 0.0 * xnew ; Allocate memory

   if (~keyword_set(invvar)) then invvar = 0.0 * fimage + 1.0

   ;MANGA specific code to handle variable fiber number per block and variable width extraction
   if keyword_set(nfiber) then begin
      if n_elements(radius) eq 1 then radius = fltarr(n_elements(nfiber))+2.0 ;check if input radius is an array
      ml_expand, radius, nfiber, generic=rad   ;expand the radius array to the size of total number of fibers, maintaining block chunking
      radarray = rad##(fltarr(ny)+1)           ;expand the radius array further to the entire CCD
   endif    
   
   if keyword_set(idl) then begin

     xinit = xcen
     x1 = xinit - radius + 0.5 
     x2 = xinit + radius + 0.5 
     ix1 = floor(x1)
     ix2 = floor(x2)

     fullpix = (min(ix2-ix1)-1) > 0
     sumw = xinit * 0.0
     sumxw = sumw
     sumwt = sumw
     sumsx1 = sumw
     sumsx2 = sumw
     qbad = long(sumxw)

     for ii=0,fullpix+3 do begin
       spot = ix1 - 1 + ii
       ih = (spot > 0) < (nx-1)
       xdiff = spot - xinit
       wt = (((radius - abs(xdiff) + 0.5) > 0) < 1) * $
             (spot GE 0 AND spot LT nx)
       sumw = sumw + fimage[ih,ycen] * wt
       sumwt = sumwt + wt
       sumxw = sumxw + fimage[ih,ycen] * xdiff * wt
       var_term = wt^2 / (invvar[ih,ycen] + (invvar[ih,ycen] EQ 0))
       sumsx2 = sumsx2 + var_term
       sumsx1 = sumsx1 + xdiff^2 * var_term
       qbad = qbad OR (invvar[ih,ycen] LE 0)
     endfor

     xnew = xinit
     xerr = xinit*0. + 999.0
     good = where(sumw GT 0 AND qbad EQ 0)
     if good[0] NE -1 then begin
       delta_x = sumxw[good]/sumw[good]
       xnew[good] = delta_x + xinit[good]
       xerr[good] = sqrt(sumsx1[good] + sumsx2[good]*delta_x^2 )/sumw[good] 
     endif 

     bad = where(abs(xnew-xinit) GT radius + 0.5 OR $
                 xinit LT radius - 0.5 OR xinit GT nx - 0.5 - radius)
     if bad[0] NE -1 then begin
       xnew[bad] = xinit[bad]
       xerr[bad] = 999.0
     endif

   endif else begin
     soname = filepath('libtrace.'+idlutils_so_ext(), root_dir=getenv('IDLUTILS_DIR'), subdirectory='lib')
     
     ;check if MANGA or BOSS
     if ~keyword_set(nfiber) then begin
         ;BOSS
         result = call_external(soname, 'trace_fweight', nx, ny, float(fimage), float(invvar), float(radius), ncen, xnew, long(ycen), xerr)
     endif else begin
         ;MANGA
         result = call_external(soname, 'ml_trace_fweight', nx, ny, float(fimage), float(invvar), float(radarray), ncen, xnew, long(ycen), xerr)
     endelse    
          
   endelse

   return, xnew
end
;------------------------------------------------------------------------------
