;+
; NAME:
;     writeflat
;
; PURPOSE:
;     Writes a FITS image of a normalized spectral flat-field.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     writeflat,normflat,hdrinfo,rotation,orders,edgecoeffs,xranges,slith_pix,$
;               slith_arc,slitw_pix,slitw_arc,modename,rms,version,outname,$
;               HISTORY=history,CANCEL=cancel 
;    
; INPUTS:
;     normflat   - A 2-D normalized flat field
;     hdrinfo    - A structure with FITS keywords and values to be
;                  written to the header
;     rotation   - The IDL rotation value
;     orders     - An array of order numbers 
;     edgecoeffs - Array [degree+1,2,norders] of polynomial coefficients 
;                  which define the edges of the orders.  array[*,0,0]
;                  are the coefficients of the bottom edge of the
;                  first order and array[*,1,0] are the coefficients 
;                  of the top edge of the first order.
;     xranges    - An array [2,norders] of pixel positions where the
;                  orders are completely on the array
;     slith_pix  - The slit length in pixels
;     slith_arc  - The slit length in arcseconds
;     slitw_pix  - The slit width in pixels
;     slitw_arc  - The slit width in arcseconds
;     modename   - The name of the observing mode
;     rms        - An array of the rms values of the flat-field
;     version    - The Spextool version used to create the flat-field
;     outname    - String of the name of the fits image
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     HISTORY - A string describing the processes of the flats
;     CANCEL   - Set on return if there is a problem
;     
; OUTPUTS:
;     Writs a FITS file to disk.
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
;     Obvious
;
; EXAMPLE:
;
;     
; MODIFICATION HISTORY:
;     2000-08-25 - Written by M. Cushing, Institute for Astronomy, UH
;     2001-10-04 - Added the xranges inputs and removed the start and stop
;     2001-11-05 - Added the rotation input
;     2002-08-22 - Added the version input
;     2005-07-04 - Modified to deal with the new hdrinfo structures.
;     2006-02-24 - Modified how history was written out so that lines
;                  are broken correctly.
;
;-
pro writeflat,normflat,hdrinfo,rotation,orders,edgecoeffs,xranges,slith_pix,$
              slith_arc,slitw_pix,slitw_arc,modename,rms,version,outname,$
              HISTORY=history,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 14 then begin
    
    print, 'Syntax - writeflat,normflat,hdrinfo,rotation,orders,edgecoeffs,$'
    print, '                   xranges,slith_pix,slith_arc,slitw_pix,$'
    print, '                   slitw_arc,modename,rms,version,outname,$'
    print, '                   HISTORY=history,CANCEL=cancel'

    cancel = 1
    return

endif
cancel = cpar('writeflat',normflat,1,'Normflat',[2,3,4,5],2)
if cancel then return
cancel = cpar('writeflat',hdrinfo,2,'Hdrinfo',8,[0,1])
if cancel then return
cancel = cpar('writeflat',rotation,3,'Rotation',[2,3,4,5],0)
if cancel then return
cancel = cpar('writeflat',orders, 4,'Orders',[2,3,4,5],1)
if cancel then return
cancel = cpar('writeflat',edgecoeffs, 5,'Edgecoeffs',[2,3,4,5],[2,3])
if cancel then return
cancel = cpar('writeflat',xranges, 6,'Xranges',[2,3,4,5],[1,2])
if cancel then return
cancel = cpar('writeflat',slith_pix,7,'Slith_pix',[2,3,4,5],0)
if cancel then return
cancel = cpar('writeflat',slith_arc,8,'Slith_arc',[2,3,4,5],0)
if cancel then return
cancel = cpar('writeflat',slitw_pix,9,'Slitw_pix',[2,3,4,5],0)
if cancel then return
cancel = cpar('writeflat',slitw_arc,10,'Slitw_arc',[2,3,4,5],0)
if cancel then return
cancel = cpar('writeflat',modename,11,'Modename',7,0)
if cancel then return
cancel = cpar('writeflat',rms,12,'Rms',[2,3,4,5],[0,1])
if cancel then return
cancel = cpar('writeflat',version,13,'Version',7,0)
if cancel then return
cancel = cpar('writeflat',outname,14,'Outname',7,0)
if cancel then return

;  Get norders, the fitorder and the number of tags in the hdrinfo struc.

norders  = n_elements(orders)
fitorder = (size(edgecoeffs))[1]
ntags    = n_tags(hdrinfo.vals)

;  Make hdr with hdrinfo in it.

fxhmake,hdr,normflat

names = tag_names(hdrinfo.vals)
for i = 0, ntags - 1 do begin

    fxaddpar,hdr,names[i],hdrinfo.vals.(i),hdrinfo.coms.(i)

endfor

fxaddpar,hdr,'MODENAME',modename, ' Spectroscopy mode'
fxaddpar,hdr,'EDGEDEG',fitorder-1, $
         ' Degree of the polynomial fit to order edges'
fxaddpar,hdr,'NORDERS',norders, ' Number of orders identified'
fxaddpar,hdr,'ORDERS',strjoin(strcompress(orders,/re),','), $
         ' Orders identified'
fxaddpar,hdr,'SLTH_PIX',slith_pix, ' Slit height in pixels'
fxaddpar,hdr,'SLTH_ARC',slith_arc, ' Slit height in arcseconds'
fxaddpar,hdr,'SLTW_PIX',slitw_pix, ' Slit width in pixels'
fxaddpar,hdr,'SLTW_ARC',slitw_arc, ' Slit width in arcseconds'
fxaddpar,hdr,'ROTATION',rotation,  ' IDL ROTATE value'
fxaddpar,hdr,'VERSION',version, ' Spextool version'

;  Now add rms's.

for i = 0,norders-1 do begin

    name    = 'ODR'+string(orders[i],FORMAT='(i2.2)')+'RMS'
    comment = ' RMS deviation of the surface fit to order '+ $
              string(orders[i],FORMAT='(i2.2)')
    fxaddpar,hdr,name,rms[i],comment

endfor

;  Now add the xranges

for i = 0, norders-1 do begin

    name    = 'ODR'+string(orders[i],FORMAT='(i2.2)')+'_XR'
    comment = ' Extraction range for order '+string(orders[i],FORMAT='(i2.2)')
    sxaddpar,hdr,name,strjoin(strtrim(xranges[*,i],2),',',/SINGLE),comment

endfor

;  Now add edge coeff

index = intarr(norders)
for i = 0,norders-1 do begin

    for j = 0, 1 do begin

        for k = 0, fitorder-1 do begin

            if j eq 0 then begin

                name    = 'ODR'+string(orders[i],FORMAT='(i2.2)') + $
                          '_B'+string(k+1,FORMAT='(i1.1)')
                comment = ' a'+string(k,FORMAT='(i1.1)')+ $
                          ' edge coefficient for bottom of order '+ $
                          string(orders[i],FORMAT='(i2.2)')

            endif
                                                    
            if j eq 1 then begin

                name    = 'ODR'+string(orders[i],FORMAT='(i2.2)') + $
                          '_T'+string(k+1,FORMAT='(i1.1)')
                comment = ' a'+string(k,FORMAT='(i1.1)')+ $
                          ' edge coefficient for top of order '+ $
                          string(orders[i],FORMAT='(i2.2)')

            endif

            sxaddpar,hdr,name,edgecoeffs[k,j,i],comment

        endfor

    endfor


endfor

if n_elements(HISTORY) ne 0 then begin

   history = mc_splittext(history,70)
   sxaddhist,history,hdr

endif

writefits,outname,normflat,hdr

end

