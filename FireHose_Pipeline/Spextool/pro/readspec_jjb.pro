;+
; NAME:
;     readspec_jjb
;
; PURPOSE:
;     Reads a SpeX spectral FITS image.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     readspec_jjb,specname,spc,hdr,obsmode,start,stop,norders,naps,orders,$
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
;;        
;;       Modified by JJB to work with XIDL-style object structure FITS files
;;       for FIRE
;;       2010-05-14 
;-

pro readspec_jjb,specname,spc,hdr,norders,naps,orders,$
             slitw_pix,slitw_arc,$
             airmass,disp, CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 1 then begin

    print, 'Syntax - readspec_jjb,specname,spc,hdr,norders,$'
    print, '                  naps,orders,xunits,yunits,slith_pix,slith_arc,$'
    print, '                  slitw_pix,slitw_arc,airmass,$'
    print, '                  CANCEL=cancel'
    cancel = 1
    return

endif
cancel = cpar('readspec_jjb',specname,1,'Specname',7,0)
if cancel then return

obj_strct = xmrdfits(specname,1,hdr)

;  Get hdr info.

;xunits = strtrim(fxpar(hdr,'XUNITS'),2)
;yunits = strtrim(fxpar(hdr,'YUNITS'),2)

;xtitle = strtrim(fxpar(hdr,'XTITLE'),2)
;if xtitle eq '0' then xtitle = '!7k!5 ('+xunits+')'
;ytitle = strtrim(fxpar(hdr,'YTITLE'),2)
;if ytitle eq '0' then ytitle = '!5f ('+yunits+')'


;norders  = fxpar(hdr,'NORDERS')
norders = N_ELEMENTS(obj_strct.order)
;naps     = fxpar(hdr,'NAPS')
naps = 1  ;hack by JJB
;start    = fxpar(hdr,'START')
;stop     = fxpar(hdr,'STOP')

exptime = obj_strct[0].exp
if (exptime EQ 0) then begin
   ; Try again; if it's not in the object structure (should be from extraction)
   ; Then get it straight from the header.
   exptime = sxpar(hdr,"EXPTIME")
endif
if (exptime EQ 0) then begin
   ; Give up.  Flux cal will be off but catches errors
   fire_siren, "EXPTIME not found in object structure or headers.  Flux calibration will be off by ratio of object:tellurix exposure times!"
   exptime = 1.0
endif

;obsmode  = strcompress( fxpar(hdr, 'MODENAME'), /RE )
;orders   = long( strsplit( fxpar(hdr,'ORDERS'), ',', /EXTRACT) )
orders = obj_strct.slitid

spc = fltarr(N_ELEMENTS(obj_strct[0].wave), 3, norders)
disp = fltarr(norders)

FOR i=0, norders-1 DO BEGIN

;inordr = where(obj_strct[i].fx NE 0 AND abs(obj_strct[i].fx) LT 60000, nordr)
;spc(inordr,0,i) = obj_strct[i].wave[inordr]/10000.  ;microns
;spc(inordr,1,i) = obj_strct[i].fx[inordr]
;spc(inordr,2,i) = obj_strct[i].var[inordr]
;disp[i] = obj_strct[i].wave[1501]/10000. - obj_strct[i].wave[1500]/10000. ;Not sure of what this is, but need to figure it out...

spc(*,0,i) = obj_strct[i].wave/10000.  ;microns
spc(*,1,i) = obj_strct[i].fx/exptime
spc(*,2,i) = sqrt(obj_strct[i].var)/exptime
disp[i] = obj_strct[i].wave[1501]/10000. - obj_strct[i].wave[1500]/10000. ;Not sure of what this is, but need to figure it out...

ENDFOR

;slith_pix = fxpar(hdr,'SLTH_PIX')
;slith_arc = fxpar(hdr,'SLTH_ARC')

;slitwidth from header for the arc and observation (usually will be
;the same I think.  Figure out how to get this in...)

slit = sxpar(hdr,"SLIT")
slitw_arc = (strsplit(slit, '_', /extract))[0]

slitw_arc = 0.6 ;fxpar(hdr,'SLTW_ARC')
slitw_pix = slitw_arc / sxpar(hdr, "SCALE") ;fxpar(hdr,'SLTW_PIX')

airmass   = sxpar(hdr,"AIRMASS")

end
