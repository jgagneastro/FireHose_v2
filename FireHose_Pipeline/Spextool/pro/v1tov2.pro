; NAME:
;     v1tov2
;
; PURPOSE:
;     To modify a Spextool v1.x FITS file to work in Spextool v2.x.
;
; CATEGORY:
;     Data Input
;
; CALLING SEQUENCE:
;     v1tov2,ifilename,ofilename
;
; INPUTS:
;     ifilename - Input FITS file name.
;     ofilename - Output FITS file name.
; 
; OUTUTS:
;     Writes a FITS file to ofilename with the correct format and hdr.
;
; KEYWORD PARAMETERS:   
;     CANCEL - Will be set on return if there is a problem.
;
; PROCEDURES CALLED:
;     Requires the Astronomy User's Library
;
;  PROCEDURE: 
;     Adds an error spectrum consisting of 1s.  Adds the slitw_* and 
;     slith_* keywords and computes the dispersion in each order and 
;     aperture.
;
;REVISION HISTORY:
;     2001-06-26 - Written by M. Cushing, Institute for Astronomy, UH
;
pro v1tov2,ifilename,ofilename

if n_params() lt 2 then begin

    ifilename = '' 
    ofilename = ''
    read, ifilename, prompt='Please enter the v1.x FITS filename: '
    read, ofilename, prompt='Please enter the v2.x FITS filename: '

endif
ifilename = strtrim(ifilename,2)
ofilename = strtrim(ofilename,2)

zparcheck, 'v1tov2',ifilename, 1, 7, [0,1], 'Ifilename' 
zparcheck, 'v1tov2',ofilename, 2, 7, [0,1], 'Ofilename' 

for i = 0, n_elements(ifilename)-1 do begin

    spec = readfits(ifilename[i],hdr)
    sxdelpar,hdr,'HISTORY'
    history = 'The orders and apertures can be accessed as follows:  If the'+$ 
      ' user desires aperture Y in order X then lambda = [*,0,( X - '+$
      'min(orders))*naps + Y], flux = array[*,1,( X - '+$
      'min(orders))*naps + Y], error = array[*,2,( X - '+$
      'min(orders))*naps + Y]'
    
    length = strlen(history)
    loop = ceil(float(length)/65.)
    for j = 0, loop-1 do begin
        
        hist = strmid(history,70*j,70)
        fxaddpar,hdr,'HISTORY',hist
        
    endfor

    slitw_arc = float( strmid(fxpar(hdr,'SLIT'),1,3) )
    slith_arc = float( strmid(fxpar(hdr,'SLIT'),4,2) )

    slitw_pix = slitw_arc / float(fxpar(hdr,'PLATE_SC'))
    slith_pix = slith_arc / float(fxpar(hdr,'PLATE_SC'))

    fxaddpar,hdr,'SLTH_PIX',slith_pix
    fxaddpar,hdr,'SLTH_ARC',slith_arc
    fxaddpar,hdr,'SLTW_PIX',slitw_pix
    fxaddpar,hdr,'SLTW_ARC',slitw_arc

;  Compute the dispersion if necessary.

    xunits = fxpar(hdr,'XUNITS')
    if xunits ne 'Pixels' then begin

        norders = fxpar(hdr,'NORDERS')
        naps    = fxpar(hdr,'NAPS')
        orders  = long( strsplit( fxpar(hdr,'ORDERS'), ',', /extract) )

        for j = 0,naps-1 do begin
            
            for k = 0, norders-1 do begin
                
                y = spec[*,0,j+k*naps]
                z = where(finite(spec[*,1,j+k*naps]) eq 1,count)
                if count ne 0 then y = y[z]
                coeff = poly_fit(findgen(n_elements(y)),y,1)
                name = 'DPO'+string(orders[k],format='(i2.2)') + $
                  'AP'+string(j+1,format='(i1.1)')
                sxaddpar,hdr,name,coeff[1]
                
            endfor
            
        endfor
        
    endif

    naxis1 = fxpar(hdr,'NAXIS1')
    naxis3 = fxpar(hdr,'NAXIS3')

    newspec = fltarr(naxis1,3,naxis3)+1
    newspec[*,0:1,*] = spec

    writefits,ofilename[i],newspec,hdr

endfor








end
