;+
; NAME:
;     readflat
;
; PURPOSE:
;     Reads a Spextool flat field FITS image.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     readflat,flatname,image,ncols,nrows,modename,slith_pix,slith_arc,$
;              slitw_pix,slitw_arc,norders,orders,edgecoeffs,xranges,rms,$
;              rotation,edgedeg,CANCEL=cancel
;
; INPUTS:
;     flatname - A string of the flat-flat name
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     image      - An array containing the image.
;     ncols      - Number of columns in the image.
;     nrows      - Number of rows in the image.
;     modename   - The observing mode of the flat.
;     slith_pix  - The approximate slit height in pixels.
;     slith_arc  - The slit height in arcsecs.
;     slitw_pix  - The slit width in pixels.
;     slitw_arc  - The slit width in arcsecs.
;     norders    - Number of orders on the array.
;     orders     - An array of the order numbers.
;     edgecoeffs - Array [degree+1,2,norders] of polynomial coefficients 
;                  which define the edges of the orders.  array[*,0,0]
;                  are the coefficients of the bottom edge of the
;                  first order and array[*,1,0] are the coefficients 
;                  of the top edge of the first order.
;     xranges    - An array [2,norders] of pixel positions where the
;                  orders are completely on the array
;     rms        - An array [norders] of rms deviations for each order
;     rotation   - The IDL rotation command (ROTATE)
;     edgedeg    - The degree of the edge coefficients
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
;     2000-04-11 - Written by M. Cushing, Institute for Astronomy, UH
;     2001-10-04 - Added xranges input and removed start and stop
;     2001-11-05 - Rotation output added.
;     2004-01-29 - Added the edgedeg output.
;-
pro readflat,flatname,image,ncols,nrows,modename,slith_pix,slith_arc,$
             slitw_pix,slitw_arc,norders,orders,edgecoeffs,xranges,rms,$
             rotation,edgedeg,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 1 then begin
    
    print, 'Syntax - readflat,flatname,image,ncols,nrows,modename,$'
    print, '                  slith_pix,slith_arc,slitw_pix,slitw_arc,$'
    print, '                  norders,orders,edgecoeffs,xranges,rms,rotation,$'
    print, '                  edgedeg,CANCEL=cancel'
    cancel = 1
    return

endif
cancel = cpar('readflat',flatname,1,'Filename',7,0)
if cancel then return

image    = readfits(flatname,hdr)

;  Get hdr info.

ncols     = fxpar(hdr,'NAXIS1')
nrows     = fxpar(hdr,'NAXIS2')
norders   = fxpar(hdr,'NORDERS')
fitorder  = fxpar(hdr,'EDGEDEG')
start     = fxpar(hdr,'START')
stop      = fxpar(hdr,'STOP')
slith_arc = fxpar(hdr,'SLTH_ARC')
slith_pix = fxpar(hdr,'SLTH_PIX')
slitw_arc = fxpar(hdr,'SLTW_ARC')
slitw_pix = fxpar(hdr,'SLTW_PIX')
rotation  = fxpar(hdr,'ROTATION') 
edgedeg   = fxpar(hdr,'EDGEDEG')

modename = strcompress( fxpar(hdr, 'MODENAME'), /RE)
orders   = long( strsplit( fxpar(hdr,'ORDERS'), ',', /EXTRACT) )

edgecoeffs = dblarr(fitorder+1,2,norders)
rms        = fltarr(norders)
xranges    = intarr(2,norders)

for i = 0,norders-1 do begin

    name_T = 'ODR'+string(orders[i],FORMAT='(i2.2)')+'_T*'
    name_B = 'ODR'+string(orders[i],FORMAT='(i2.2)')+'_B*'

    coeff_T = fxpar(hdr,name_T)
    coeff_B = fxpar(hdr,name_B)

    edgecoeffs[*,1,i] = coeff_T
    edgecoeffs[*,0,i] = coeff_B

    name         = 'ODR'+string(orders[i],FORMAT='(i2.2)')+'_XR'
    xranges[*,i] = long( strsplit( fxpar(hdr,name), ',', /EXTRACT) )

    name = 'ODR'+string(orders[i],FORMAT='(i2.2)')+'RMS'
    rmss = fxpar(hdr,name)
    rms[i] = rmss

endfor

image = rotate(temporary(image),rotation)

end



