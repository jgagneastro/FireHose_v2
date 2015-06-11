;+
; NAME:
;     readspec
;
; PURPOSE:
;     Reads a SpeX spectral FITS image.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     readspec,specname,spc,hdr,obsmode,start,stop,norders,naps,orders,$
;              xunits,yunits,slith_pix,slith_arc,slitw_pix,slitw_arc,$
;              airmass,xtitle,ytitle,CANCEL=cancel
;
; INPUTS:
;     flatname - A string name of the spectrum
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     spec      - The spectrum [stop-start+1,3,norders*naps]
;     hdr       - A string array of the FITS header
;     obsmode   - The obsmode
;     start     - Start column
;     stop      - Stop column
;     norders   - Number of orders
;     naps      - Number of apertures
;     orders    - An array [norders] of the order numbers
;     xunits    - A string giving the units of the x-axis
;     yunits    - A string giving the units of the y-axis
;     slith_pix - The approximate slit height in pixels
;     slith_arc - The slit height in arcseconds
;     slitw_pix - The slit width in pixels
;     slitw_arc - The slit width in arcseconds
;     airmass   - The average airmass of the observation  
;     xtitle    - A string of the Xtitle to be used for IDL plotting
;     ytitle    - A string of the Ytitle to be used for IDL plotting
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
;     Easy
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;        Written by M. Cushing, Institute for Astronomy, University of
;        Hawaii
;        2005-08-04 - Added the xtitle and ytitle input parameters
;-

pro readspec,specname,spc,hdr,obsmode,start,stop,norders,naps,orders,$
             xunits,yunits,slith_pix,slith_arc,slitw_pix,slitw_arc,$
             airmass,xtitle,ytitle,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 1 then begin

    print, 'Syntax - readspec,specname,spc,hdr,obsmode,start,stop,norders,$'
    print, '                  naps,orders,xunits,yunits,slith_pix,slith_arc,$'
    print, '                  slitw_pix,slitw_arc,airmass,xtitle,ytitle,$'
    print, '                  CANCEL=cancel'
    cancel = 1
    return

endif
cancel = cpar('readspec',specname,1,'Specname',7,0)
if cancel then return

spc = readfits(specname,hdr)

;  Get hdr info.

xunits = strtrim(fxpar(hdr,'XUNITS'),2)
yunits = strtrim(fxpar(hdr,'YUNITS'),2)

xtitle = strtrim(fxpar(hdr,'XTITLE'),2)
if xtitle eq '0' then xtitle = '!7k!5 ('+xunits+')'
ytitle = strtrim(fxpar(hdr,'YTITLE'),2)
if ytitle eq '0' then ytitle = '!5f ('+yunits+')'

norders  = fxpar(hdr,'NORDERS')
naps     = fxpar(hdr,'NAPS')
start    = fxpar(hdr,'START')
stop     = fxpar(hdr,'STOP')

obsmode  = strcompress( fxpar(hdr, 'MODENAME'), /RE )
orders   = long( strsplit( fxpar(hdr,'ORDERS'), ',', /EXTRACT) )

slith_pix = fxpar(hdr,'SLTH_PIX')
slith_arc = fxpar(hdr,'SLTH_ARC')
slitw_pix = fxpar(hdr,'SLTW_PIX')
slitw_arc = fxpar(hdr,'SLTW_ARC')

airmass   = fxpar(hdr,'AIRMASS')


end
