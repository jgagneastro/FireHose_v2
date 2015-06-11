;+
; NAME:
;     rdwavecal
;
; PURPOSE:
;     Reads a wavelength calibration file for Spextool.
;     
; CATEGORY:
;     Spectroscopy
;  
; CALLING SEQUENCE:
;     rdwavecal,filename,spectra,orders,linetypes,l2pcoeffs,homeorder,dispdeg,$
;               ordrdeg,resppix,p2lcoeffs,rms,CANCEL=cancel
;
; INPUTS:
;     filename - The name of a Spextool wavelength calibration file.
;
; OUTUTS:
;     spectra     - The spectra that can be used as a x correlator
;     orders      - An array giving the possible x correlator orders
;     linetypes   = The type of lines in each order, a=argon, s=sky.
;     l2pcoeffs   - The lambda to pixel polynomial coefficients [2,norders].
;     homeorder   - The home order
;     disdeg      - The degree of the polynomial in the dispersion direction
;     ordrdeg     - The degree of the polynomial in the order direction
;     res_factor  - The resolution per pixel
;     p2lcoeffs   - The stored 2D solution for use as a standby
;     rms         - The RMS of the 2D solution
;
; KEYWORD PARAMETERS:    
;     CANCEL - Set on return if there is a problem
;
; PROCEDURE'S USED:
;     Requires the Astronomy User's Library
;
; PROCEDURE:
;     Straight forward, just reads a FITS file and header.
;
; REVISION HISTORY:
;     2002-12-02 - Written by M. Cushing, Institute for Astronomy, UH
;     2004-01-29 - Modified to return the type of line in each order
;-
pro rdwavecal,filename,spectra,orders,linetypes,ltopcoeffs,homeorder,$
              dispdeg,ordrdeg,resppix,p2lcoeffs,rms,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 1 then begin
    
    cancel = 1
    print, 'Syntax - rdwavecal,filename,spectra,orders,linetypes,l2pcoeffs,$'
    print, '                   homeorder,dispdeg,prordrdeg,resppix,$'
    print, '                   p2lcoeffs,rms,CANCEL=cancel'
    return

endif

zparcheck, 'rdwavecal', filename, 1,7,0, 'Filename'

spectra = readfits(filename,hdr)

start  = fxpar(hdr,'START')
stop   = fxpar(hdr,'STOP')

orders = fxpar(hdr,'ORDERS')
orders = fix(fsextract(orders,/INDEX))

norders = n_elements(orders)

ltopcoeffs = dblarr(2,norders)
for i = 0,norders-1 do begin

    key = 'ODR'+string(orders[i],format='(i2.2)')+'*'
    ltopcoeffs[*,i] = mcfxpar(hdr,key)

endfor

homeorder   = fxpar(hdr,'HOMEORDR')
dispdeg     = fxpar(hdr,'DISPDEG')
ordrdeg     = fxpar(hdr,'ORDRDEG')
resppix     = fxpar(hdr,'RESPPIX')
xcor_orders = fxpar(hdr,'XCORORDR')
xcor_orders = fix(fsextract(xcor_orders,/INDEX))

ncoeffs = (dispdeg+1)*(ordrdeg+1)
p2lcoeffs = dblarr(ncoeffs)

for i = 0, ncoeffs-1 do begin

    key = 'p2l_c'+string(i,format='(i2.2)')
    p2lcoeffs[i] = fxpar(hdr,key)

endfor

rms = fxpar(hdr,'RMS')
linetypes = strsplit(fxpar(hdr,'ORDRTYPE'),',',/EXTRACT)

match, orders,xcor_orders,idx
spectra = spectra[*,0:1,idx]


orders = xcor_orders



end
