;+
; NAME:
;     redden
;
; PURPOSE:
;     Reddens a spectrum by an input E(B-V) value.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     REDDEN, wave, flux, ebmv, fredd, [ R_V = ]
;       or
;     REDDEN, wave, flux, ebmv, [ R_V = ]
;
; INPUTS:
;     WAVE - wavelength vector (microns)
;     FLUX - calibrated flux vector, same number of elements as WAVE
;             If only 3 parameters are supplied, then this vector will
;             updated on output to contain the dereddened flux.
;     EBMV  - color excess E(B-V), scalar.  
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     R_V - scalar specifying the ratio of total selective extinction
;             R(V) = A(V) / E(B - V). If not specified, then R_V = 3.1
;             for CCM curve and R_V = 3.07 for Rieke curve.
; OUTPUTS:
;     FREDD - reddened flux vector, same units and number of elements
;             as FLUX
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
;     Reddens the spectrum using ccdm_redden < 3.3 um and 
;     rieke_redden  > 3.3 um
;    
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2002-08 - Written by W. D. Vacca, Institute for Astronomy, UH
;-
pro redden, wave,flux,ebmv,fredd,R_V=rv,CANCEL=cancel

cancel = 0

if n_params() lt 3 then begin

    print, 'Syntax - result = redden(wave,flux,ebmv,fredd,R_V=rv,CANCEL=cancel'
    cancel = 1
    return

endif
cancel = cpar('redden',wave,1,'Wave',[2,3,4,5],1)
if cancel then return
cancel = cpar('redden',flux,2,'Flux',[2,3,4,5],1)
if cancel then return
cancel = cpar('redden',ebmv,3,'Ebmv',[2,3,4,5],0)
if cancel then return

if not keyword_set(R_V) then begin
    rvccm  = 3.1
    rvriek = 3.07
endif else begin
    rvccm  = R_V
    rvriek = R_V
endelse

x  = wave*1.0d4                 ; convert to Angstroms
zc = where(wave gt 0.3 and wave lt 3.3, ccount)
zr = where(wave ge 3.3 and wave lt 13.0, rcount)

fredd = flux
if (ccount gt 0) then begin
    if (N_params() eq 3) then begin
        ccm_unred, x[zc], flux[zc], -1.0*ebmv, R_V=rvccm 
    endif else begin
        ccm_unred, x[zc], flux[zc], -1.0*ebmv, fr, R_V=rvccm
        fredd[zc]=fr
    endelse
endif 
if (rcount gt 0) then begin
    if (N_params() eq 3) then begin
        rieke_unred, x[zr], flux[zr], -1.0*ebmv, R_V=rvriek 
    endif else begin
        rieke_unred, x[zr], flux[zr], -1.0*ebmv, fr, R_V=rvriek
        fredd[zr]=fr
    endelse 
endif
return

end


