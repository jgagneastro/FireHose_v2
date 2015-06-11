;+
; NAME:
;     writespec_ps
;    
; PURPOSE:
;     Writes a SpeX point source spectra to disk
;   
; CATEGORY:
;     Spectroscopy
; 
; CALLING SEQUENCE:
;     writespec_ps,spectra,fullpath,aimage,sky,flat,naps,orders,start,stop,$
;                  hdrinfo,appos,apradius,mode,slith_pix,slith_arc,slitw_pix,$
;                  slitw_arc,xunits,yunits,xtitle,ytitle,version,$
;                  PSFRADIUS=psfradius,BGSTART=bgstart,BGWIDTH=bgwidth,$
;                  BGORDER=bgorder,TRACEFILE=tracefile,WAVECAL=wavecal,$
;                  DISP=disp,FITS=fits,TEXT=text,RMS=rms,RES=res,$
;                  LINEARITY=linearity,EXT2D=ext2d,CANCEL=cancel
;    
; INPUTS:
;     spectra   - An array [*,3,naps*norders] array of spectra where
;                 array [*,0,0] = wavelengths
;                 array [*,1,0] = flux
;                 array [*,2,0] = error
;     fullpath  - The fullpath of the file to be written to
;     aimage    - A string of the name if the Aimage
;     sky       - A string of the sky image
;     flat      - A string of the flat field image
;     naps      - The number of apertures
;     orders    - An array of order numbers
;     start     - Start column
;     stop      - Stop column
;     hdrinfo   - A structure with FITS keywords and values
;     appos     - An array [naps,norders] of aperture positions
;     apradius  - The aperture radius of the extraction
;     mode      - A string giving the mode name of the spectra
;     slith_pix - The slit length in pixels
;     slith_arc - The slit length in arcseconds
;     slitw_pix - The slit width in pixels
;     slitw_arc - The slit width in arcseconds
;     xunits    - A string giving the units of array[*,0,0]
;     yunits    - A string giving the units of array[*,1,0]
;     xtitle    - A string of the Xtitle to be used for IDL plotting
;     ytitle    - A string of the Ytitle to be used for IDL plotting
;     version   - Spextool version number
;    
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     PSFRADIUS - The radius of the psf in the optimal extraction
;     BGSTART   - The background starting radius (see mkmask_ps.pro)
;     BGWIDTH   - The background width (see mkmask_ps.pro)
;     BGORDER   - The polynomial degree of the background fit
;     TRACEFILE - A string giving the name of the trace file used in 
;                 the extraction (if a trace file was used)
;     WAVECAL   - A string giving the name of the wavecal file
;     DISP      - An array [nordesr*naps] of wavelength dispersion
;     FITS      - Write a FITS file
;     TEXT      - Write a text file
;     RMS       - An array [naps] of RMS values of the wavelength
;                 solution
;     RES       - The average resolution of the spectra 
;     LINEARITY - If set, a note will be put in the header that the 
;                 spectra were corrected for non-linearity   
;     EXT2D     - Set to write the 2D extraction history instead of
;                 the 1D version
;     CANCEL   - Set on return if there is a problem
;     
; OUTPUTS:
;     Writes a FITS or text file to disk
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
;     2000-09-01 - Written by M. Cushing, Institute for Astronomy, UH
;     2002-10-09 - Added LINEARITY keyword
;     2003-02    - Added optimal extraction output
;     2005-07-04 - Modified to accept the new hdrinfo structure
;     2005-08-04 - Moved the xunits and ynunits parameters and added the
;                  xtitle and ytitle input parameters
;     2005-09-05 - Changed ARCIMAGE keyword to WAVECAL 
;     2006-06-09 - Added appos input
;-
pro writespec_ps,spectra,fullpath,aimage,sky,flat,naps,orders,$
                 start,stop,hdrinfo,appos,apradius,mode,slith_pix, $
                 slith_arc,slitw_pix,slitw_arc,xunits,yunits,xtitle,ytitle, $
                 version,PSFRADIUS=psfradius,BGSTART=bgstart,BGWIDTH=bgwidth, $
                 BGORDER=bgorder,TRACEFILE=tracefile,WAVECAL=wavecal, $
                 DISP=disp,FITS=fits,TEXT=text,RMS=rms,RES=res, $
                 LINEARITY=linearity,EXT2D=ext2d,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 21 then begin
    
    print, 'Syntax - writespec_ps,spectra,fullpath,aimage,sky,flat,naps,$'
    print, '                      orders,start,stop,hdrinfo,appos,apradius,$'
    print, '                      mode,slith_pix,slith_arc,slitw_pix,$'
    print, '                      slitw_arc,xunits,yunits,xtitle,ytitle,$'
    print, '                      version,PSFRADIUS=psfradius,$'
    print, '                      BGSTART=bgstart,$'
    print, '                      BGWIDTH=bgwidth,BGORDER=bgorder,$'
    print, '                      TRACEFILE=tracefile,WAVECAL=wavecal,$'
    print, '                      DISP=disp,FITS=fits,TEXT=text,RMS=rms,$'
    print, '                      RES=res,LINEARITY=linearity,EXT2D=ext2d,$'
    print, '                      CANCEL=cancel'

    cancel = 1
    return

endif
cancel = cpar('writespec_ps',spectra,1,'Spectra',[2,3,4,5],[2,3])
if cancel then return
cancel = cpar('writespec_ps',fullpath,2,'Fullpath',7,0) 
if cancel then return
cancel = cpar('writespec_ps',aimage,3,'Aimage',7,0) 
if cancel then return
cancel = cpar('writespec_ps',sky,4,'Sky',7,0)
if cancel then return
cancel = cpar('writespec_ps',flat,5,'Flat',7,0)
if cancel then return
cancel = cpar('writespec_ps',naps,6,'Naps',[2,3,4,5],0) 
if cancel then return
cancel = cpar('writespec_ps',orders,7,'Orders',[2,3,4,5],[0,1]) 
if cancel then return
cancel = cpar('writespec_ps',start,8,'Start',[2,3,4,5],0)
if cancel then return
cancel = cpar('writespec_ps',stop,9,'Stop',[2,3,4,5],0) 
if cancel then return
cancel = cpar('writespec_ps',hdrinfo,10,'Hdrinfo',8,[0,1])
if cancel then return
cancel = cpar('writespec_ps',appos,11,'Apradius',[2,3,4,5],[1,2]) 
if cancel then return
cancel = cpar('writespec_ps',apradius,12,'Apradius',[2,3,4,5],0) 
if cancel then return
cancel = cpar('writespec_ps',mode,13,'Mode',7,0)
if cancel then return
cancel = cpar('writespec_ps',slith_pix,14,'Slith_pix',[2,3,4,5],0)
if cancel then return
cancel = cpar('writespec_ps',slith_arc,15,'Slith_arc',[2,3,4,5],0)
if cancel then return
cancel = cpar('writespec_ps',slitw_pix,16,'Slitw_pix',[2,3,4,5],0)
if cancel then return
cancel = cpar('writespec_ps',slitw_arc,17,'Slitw_arc',[2,3,4,5],0)
if cancel then return
cancel = cpar('writespec_ps',xunits,18,'Xunits',7,0)
if cancel then return
cancel = cpar('writespec_ps',yunits,19,'Yunits',7,0)
if cancel then return
cancel = cpar('writespec_ps',xtitle,20,'Xtitle',7,0)
if cancel then return
cancel = cpar('writespec_ps',ytitle,21,'Ytitle',7,0)
if cancel then return
cancel = cpar('writespec_ps',version,22,'Version',7,0)
if cancel then return

;  Make hdr with hdrinfo in it.

fxhmake,hdr,spectra

norders  = n_elements(orders)
ntags    = n_tags(hdrinfo.vals)

names = tag_names(hdrinfo.vals)
for i = 0, ntags - 1 do begin

    fxaddpar,hdr,names[i],hdrinfo.vals.(i), hdrinfo.coms.(i)

endfor

fxaddpar,hdr,'MODENAME',mode, ' Spectroscopy mode'
fxaddpar,hdr,'AIMAGE',aimage, ' A image'
fxaddpar,hdr,'SKY',sky, ' Sky image'
fxaddpar,hdr,'FLAT',flat, ' Flat field image'

fxaddpar,hdr,'NAPS',naps, ' Number of apertures'
fxaddpar,hdr,'NORDERS',norders, ' Number of orders'
fxaddpar,hdr,'ORDERS',strjoin(strcompress(orders,/re),','), ' Order numbers'
fxaddpar,hdr,'START',start, ' Array column of first FITS array column'
fxaddpar,hdr,'STOP',stop, ' Array column of last FITS array column'
fxaddpar,hdr,'SLTH_PIX',slith_pix, ' Slit height in pixels'
fxaddpar,hdr,'SLTH_ARC',slith_arc, ' Slit height in arcseconds'
fxaddpar,hdr,'SLTW_PIX',slitw_pix, ' Slit width in pixels'
fxaddpar,hdr,'SLTW_ARC',slitw_arc, ' Slit width in arcseconds'

for i = 0, norders-1 do begin

   name = 'APPOSO'+string(orders[i],format='(i2.2)')
   comment = ' Aperture positions (arcsec) for order '+ $
             string(orders[i],FORMAT='(i2.2)')
   fxaddpar,hdr,name,strjoin(strtrim(appos[*,i],2),','), comment

endfor

if n_elements(PSFRADIUS) ne 0 then fxaddpar,hdr,'PSFRAD',psfradius, $
  ' Optimal extraction PSF radius in arcseconds'

fxaddpar,hdr,'APRADIUS',apradius, ' Aperture radius in arcseconds'

if n_elements(BGSTART) ne 0 then fxaddpar,hdr,'BGSTART',bgstart, $
  ' Background start radius in arcseconds'
if n_elements(BGWIDTH) ne 0 then fxaddpar,hdr,'BGWIDTH',bgwidth, $
  ' Background width in arcseconds'
if n_elements(BGORDER) ne 0 then fxaddpar,hdr,'BGORDER',bgorder, $
  ' Background polynomial fit degree'
if n_elements(TRACEFILE) ne 0 then fxaddpar,hdr,'TRACEFIL',tracefile, $
  ' Trace file name'
if n_elements(WAVECAL) ne 0 then fxaddpar,hdr,'WAVECAL',wavecal, $
  ' Wavecal file used for wavelength calibration'

fxaddpar,hdr,'XUNITS',xunits, ' Units of the X axis'
fxaddpar,hdr,'YUNITS',yunits, ' Units of the Y axis'
fxaddpar,hdr,'XTITLE',xtitle, ' IDL X title'
fxaddpar,hdr,'YTITLE',ytitle, ' IDL Y title'

fxaddpar,hdr,'VERSION',version, ' Spextool version'

if n_elements(RES) ne 0 then fxaddpar,hdr,'RES',res, $
  ' Average spectral resolving power'

if n_elements(DISP) ne 0 then begin

   for j = 0, norders-1 do begin
      
      name = 'DISPO'+string(orders[j],format='(i2.2)')
      comment = ' Dispersion ('+xunits+' pixel-1) for order '+ $
                string(orders[j],FORMAT='(i2.2)')
      sxaddpar,hdr,name,disp[j*naps],comment
      
    endfor

endif

if keyword_set(EXT2D) then begin

    history = 'The orders and apertures can be accessed as follows:  If the'+$ 
      ' user desires aperture Y in order X then lambda = [*,0,( X - '+$
      'min(orders))*naps + Y-1], flux = array[*,1:nspat,( X - '+$
      'min(orders))*naps + Y-1], error = array[*,(nspat+1):*,( X - '+$
      'min(orders))*naps + Y-1] where nspat=(NAXIS2-1)/2.'

endif else begin


    history = 'The output FITS files contain a 3-D array of data ' + $
              'consisting of sets of triplet arrays of data for each ' + $
              'aperture and each order, where each triplet is composed ' + $
              'of an array for the wavelength, an array for the flux, and ' + $
              'an array for the error. The triplets for each aperture are ' + $
              'stacked behind one another, followed by the triplets for ' + $
              'the next order, etc. If no orders have been skipped or ' + $
              'deslected in the extraction process, the contents of ' + $
              'aperture Y in order X can be found as follows:' + $
              '' + $
              'lambda = array[*,0,( X - (smallest order number))*naps ' + $
              '+ (Y-1)], flux   = array[*,1,( X - (smallest order ' + $
              'number))*naps + (Y-1)], error  = array[*,2,( X - (smallest ' + $
              'order number))*naps + (Y-1)]' + $
              '' + $
              '  For example, for an SXD file with two apertures, the ' + $
              'wavelength array for aperture 1 in order 3 is located in ' + $
              'array [*,0,0], the flux is in array [*,1,0] and the error ' + $
              'is in array [*,2,0]. For aperture 2, the wavelength is in ' + $
              'array [*,0,1], the flux is in array [*,1,1] and the error ' + $
              'is in array [*,2,1]. For order 4, the flux for aperture 1 ' + $
              'is in array [*,1,2], while the flux for aperture 2 is in ' + $
              'array [*,1,3]. For order 5, the fluxes for the two ' + $
              'apertures are in arrays [*,1,4] and [*,1,5], etc.'

endelse

if n_elements(RMS) ne 0 then begin

    history =history+'  The RMS in angstroms of the wavelength calibration '+$
      'is '+strjoin(strcompress(rms*10^4,/RE),', ')+' for aperture 1, 2, etc.'

endif

if keyword_set(LINEARITY) then history = history+'  The spectra have been '+$
  'corrected for non-linearity.'

fxaddpar,hdr,'HISTORY',' '

history = mc_splittext(history,65)
sxaddhist,history,hdr

if keyword_set(FITS) then writefits,fullpath+'.fits',spectra,hdr

if keyword_set(TEXT) then begin

    npix = fxpar(hdr,'NAXIS1')
    openw,lun,fullpath+'.txt', /get_lun

    for i = 0, n_elements(hdr)-1 do printf, lun, hdr[i]

    for i = 0, npix-1 do begin

        printf, lun,  strjoin( reform(spectra[i,*,*],3*naps*norders),'  ' )

    endfor
    close, lun
    free_lun, lun


endif


end

