;+
; NAME:
;     mc_rebinspec
;
; PURPOSE:
;     To rebin a spectrum with errors
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     mc_rebinspec,iwave,iflux,newsize,owave,oflux,IERROR=ierror,$
;     OERROR=oerror,CANCEL=cancel
;
; INPUTS:
;     iwave   - A 1D wavelength array
;     iflux   - A 1D flux array
;     newsize - The requested final spectrum size
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     IERROR - The 1D error spectrum
;     OERROR - The 1D rebinned error spectrum
;     CANCEL - Set on return if there is a problem.
;
; OUTPUTS:
;     owave - The rebinned wavelength array
;     oflux - The rebinned flux array
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
;     Uses the frebin routine, except it includes error propagation.
;
; EXAMPLE:
;     Later.
;
; MODIFICATION HISTORY:
;     2006-09-25:  Written by M. Cushing, Steward Observatory,
;     University of Arizona
;-
pro mc_rebinspec,iwave,iflux,newsize,owave,oflux,IERROR=ierror, $
                 OERROR=oerror,CANCEL=cancel

  cancel = 0

  if n_params() lt 3 then begin

     print, 'Syntax - mc_rebinspec,iwave,iflux,size,owave,oflux,' + $
     print,                        'IERROR=ierror,OERROR=oerror,CANCEL=cancel'
     cancel = 1
     return

  endif

  cancel = cpar('mc_rebinspec',iwave,1,'IWave',[2,3,4,5],1)
  if cancel then return
  cancel = cpar('mc_rebinspec',iflux,2,'IFlux',[2,3,4,5],1)
  if cancel then return
  cancel = cpar('mc_rebinspec',newsize,3,'NewSize',[2,3,4,5],0)
  if cancel then return

  if n_elements(IERROR) ne 0 then begin

     cancel = cpar('mc_rebinspec',IERROR,4,'IERROR',[2,3,4,5],1)
     if cancel then return

  endif

  ndat = n_elements(iflux)

;
; determine if we can use the standard rebin function
;
  dtype = size(iflux,/TNAME)
  sbox  = (dtype eq 'DOUBLE') ? ndat/double(newsize):ndat/float(newsize)

  if dtype eq 'DOUBLE' then begin

     owave = dblarr(newsize,/NOZERO)
     oflux = dblarr(newsize,/NOZERO)
     if n_elements(IERROR) ne 0 then ovar = dblarr(newsize,/NOZERO)

  endif else begin

     owave = fltarr(newsize,/NOZERO)
     oflux = fltarr(newsize,/NOZERO)
     if n_elements(IERROR) ne 0 then ovar = fltarr(newsize,/NOZERO)

  endelse

  ns1 = ndat-1
  for i=0L,newsize-1 do begin
     rstart = i*sbox            ;starting position for each box
     istart = long(rstart)
     rstop = rstart + sbox      ;ending position for each box
     istop = long(rstop)<ns1
     frac1 = rstart-istart
     frac2 = 1.0 - (rstop-istop)
;
; add pixel values from istart to istop and  subtract fraction pixel 
; from istart to rstart and fraction pixel from rstop to istop
;
     owave[i] = total(iwave[istart:istop]) $
                 - frac1 * iwave[istart]  $
                 - frac2 * iwave[istop] 

     oflux[i] = total(iflux[istart:istop]) $
                 - frac1 * iflux[istart]  $
                 - frac2 * iflux[istop] 

     if n_elements(IERROR) ne 0 then begin

        ovar[i] = total(ierror[istart:istop]^2) $
                  - frac1^2 * ierror[istart]^2  $
                  - frac2^2 * ierror[istop]^2 
        
     endif

  endfor
  
  owave  = owave/sbox
  oflux  = oflux/sbox
  if n_elements(IERROR) ne 0 then oerror = sqrt(ovar/sbox^2)
  
end
