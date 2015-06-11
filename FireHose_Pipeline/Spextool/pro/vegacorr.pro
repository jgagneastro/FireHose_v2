;+
; NAME:
;     vegacorr
;
; PURPOSE:
;     Cross correlates a spectrum against the Vega model.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     vegacorr,wave,flux,wvin,fvin,fcvin,vshift,z,WMIN=wmin,$
;              WMAX=wmax,PLOTCORR=plotcorr,CANCEL=cancel
;
; INPUTS:
;     wave      - An array of wavelength values (in microns)
;     flux      - An array of normalized flux values
;     wvin      - The wavelength array for the Vega model
;     fvin      - The flux array for the Vega model
;     cfvin     - The continuum fluxes for the Vega model
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     WMIN      - Wavelength minimum for the correlation
;     WMAX      - Wavelength maximum for the correlation
;     PLOTCORR  - Set if the cross-correlation function is to be
;                 plotted
;     CANCEL    - Set on return if there is a problem
;
; OUTPUTS:
;     vshift - Velocity shift in km/s for which the correlation 
;              is a maximum
;     z      - The redshift for the vshift
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
;     Cross-correlates the two spectra in velocity space
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2000-12-04 - Written by WDV
;     2002       - Modified a bit by M. Cushing, Institute for
;                  Astronomy, UH
;-
pro vegacorr, wave,flux,wvin,fvin,fcvin,vshift,z,WMIN=wmin,WMAX=wmax,$
              PLOTCORR=plotcorr

cancel = 0

if n_params() lt 5 then begin
    
    print, 'Syntax - vegacorr,wave,flux,wvin,fvin,fcvin,vshift,z,$'
    print, '                  WMIN=wmin,WMAX=wmax,PLOTCORR=plotcorr,$'
    print, '                  CANCEL=cancel'
    cancel = 1
    return

endif
cancel = cpar('vegacorr',wave,1,'Wave',[2,3,4,5],1)
if cancel then return
cancel = cpar('vegacorr',flux,2,'Flux',[2,3,4,5],1)
if cancel then return
cancel = cpar('vegacorr',wvin,3,'Wvin',[2,3,4,5],1)
if cancel then return
cancel = cpar('vegacorr',fvin,4,'Fvin',[2,3,4,5],1)
if cancel then return
cancel = cpar('vegacorr',fcvin,5,'Fcvin',[2,3,4,5],1)
if cancel then return

dum      = ' '
cspeed   = 2.99792458E5 		    ; km/s

wvega  = wvin
fvega  = fvin                     ; original A0V model fluxes 
fnvega = (fvin/fcvin) - 1.0    ; normalize and subtract normalization
fcvega = fcvin
fdata  = flux - 1.0                  ; subtract normalization

; --- Find min and max wavelengths

if keyword_set(WMIN) then begin
      wmin = max([wmin,min(wvega),min(wave)])
endif else begin
    wmin = max([min(wvega),min(wave)])
endelse

if keyword_set(WMAX) then begin
      wmax = min([wmax,max(wvega),max(wave)])
endif else begin
    wmax = min([max(wvega),max(wave)])
endelse

print, 'Cross correlating between: ', wmin, wmax

; --- Check number of points

ndata = n_elements(where((wave  ge wmin) and (wave  le wmax)))
nvega = n_elements(where((wvega ge wmin) and (wvega le wmax)))
if (nvega lt ndata) then print, 'Warning: Input data has higher resolution than Vega model!'
num   = max([ndata,nvega])

; --- Resample to constant spacing in log lambda space: v/c = d(ln lambda)

acoef  = float(num - 1)/(alog(wmax) - alog(wmin))
bcoef  = float(num) - (acoef * (alog(wmax)))

xpon   = findgen(num)+1.0
wx     = exp((xpon-bcoef)/acoef)

fxdata = interpol(fdata,wave,wx)
fxvega = interpol(fnvega,wvega,wx)

; --- Perform cross correlation

lag    = indgen(num)-(num/2)
result = c_correlate(fxvega,fxdata,lag)
pfit   = mpfitpeak(lag,result,pcoefs,/GAUSSIAN,NTERMS=3)
lshift = pcoefs(1)

vshift = (cspeed * lshift)/acoef
z      = (vshift/cspeed)

print, 'Velocity shift (km/s) = ',vshift
print, 'Redshift term         = ',1.0 + z

; --- Plot cross correlation function versus lag

if keyword_set(PLOTCORR) then begin
      window,/free
      plot, lag, result, xtitle='Lag', ytitle='Cross Correlation',$
        psym=-4,/xsty,/ysty
      plots,[lshift,lshift],!y.crange,color=2

endif

end









