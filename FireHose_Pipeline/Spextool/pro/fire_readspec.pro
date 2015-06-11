;+
; NAME:
;     fire_readspec
;
; PURPOSE:
;     Reads a SpeX spectral FITS image.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     fire_readspec,specname,spc,hdr,obsmode,start,stop,shifts,norders,naps,orders,$
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
;     shifts  -  A float array of pixel shifts from between object and telluric star
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

pro fire_readspec,specname,spc,hdr,obsmode,start,stop,shifts,norders,naps,orders,$
             xunits,yunits,slith_pix,slith_arc,slitw_pix,slitw_arc,$
             airmass,xtitle,ytitle,CANCEL=cancel, fits_extension=fits_extension

cancel = 0

;  Check parameters

if n_params() lt 1 then begin

    print, 'Syntax - fire_readspec,specname,spc,hdr,obsmode,start,stop,shifts,norders,$'
    print, '                  naps,orders,xunits,yunits,slith_pix,slith_arc,$'
    print, '                  slitw_pix,slitw_arc,airmass,xtitle,ytitle,$'
    print, '                  CANCEL=cancel'
    cancel = 1
    return

endif

string_replace, specname, '//', '/'
cancel = cpar('fire_readspec',specname,1,'Specname',7,0)
if cancel then return

spc = readfits(specname,hdr,ext=fits_extension)

;  Get hdr info.

xunits = strtrim(fxpar(hdr,'XUNITS'),2)
yunits = strtrim(fxpar(hdr,'YUNITS'),2)

xtitle = strtrim(fxpar(hdr,'XTITLE'),2)
if xtitle eq '0' then xtitle = '!7k!5 ('+xunits+')'
ytitle = strtrim(fxpar(hdr,'YTITLE'),2)
if ytitle eq '0' then ytitle = '!5f ('+yunits+')'

if spc[0] eq -1L and n_elements(spc) eq 1 then begin
  spc = xmrdfits2(specname,hdr)
  start = 0L
  stop = (size(spc))[1]-1L
  if (size(spc))[0] lt 3L then norders = 1L else $
    norders = (size(spc))[3]
  naps = 1L
  xunits = 'um'
  yunits = 'rel'
  obsmode = sxpar(hdr,'OBSTYPE')
  orders = lindgen(norders)+11L
  scale = sxpar(hdr,'SCALE')
  slit = sxpar(hdr,'SLIT')
  slitw_arc = float(strmid(slit,0L,strpos(slit,'_')))
  slitw_pix = round(slitw_arc / scale)
  slith_arc = 7.
  slith_pix = round(slith_arc / scale)
  shifts = fltarr(norders)
endif else begin
  
  ;!@!@!@!@!@!
  norders  = fxpar(hdr,'NORDERS')
  if norders eq 0 then norders = fxpar(hdr,'NAXIS2')
  naps     = fxpar(hdr,'NAPS')
  if naps eq 0 then naps = 1L
  start    = fxpar(hdr,'START')
  stop     = fxpar(hdr,'STOP')
  if stop eq 0L then stop = (fxpar(hdr,'NAXIS1')-1L)
  
  ; AJB hack to add shifts
  shifts = fltarr(norders)
  for i=0L,norders-1L do shifts[i] = fxpar(hdr,'SHIFT'+strtrim(string(fix(start+i)),2))
  
  obsmode  = strcompress( fxpar(hdr, 'MODENAME'), /RE )
  ords = fxpar(hdr,'ORDERS')
  if ords[0] eq 0 and n_elements(ords) eq 1L then $
    orders = lindgen(norders)+1L else $
    orders   = long( strsplit( ords, ',', /EXTRACT) )
  
  slith_pix = fxpar(hdr,'SLTH_PIX')
  slith_arc = fxpar(hdr,'SLTH_ARC')
  slitw_pix = fxpar(hdr,'SLTW_PIX')
  slitw_arc = fxpar(hdr,'SLTW_ARC')
  if slitw_arc eq 0 then begin
    slitw_arc = float((strsplit(sxpar(hdr,'SLIT'),'_',/extract))[0])
    slitw_pix = slitw_arc / 0.18
    slith_arc = 7.
    slith_pix = slith_arc / 0.18
  endif
  
endelse
airmass   = fxpar(hdr,'AIRMASS')

end
