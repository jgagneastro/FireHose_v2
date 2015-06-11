;+
; NAME:
; fire_multispec
;    Version 1.1
;
; PURPOSE:
;   Outputs individual order flux and error arrays into separate files (_F.fits, _E.fits)
;
; CALLING SEQUENCE:
;  
;   fire_multispec, fspec, outfile, sigfile, HDR=hdr, RESVAL=resval, /SILENT
;
; INPUTS:
;   fspec      - FIRE structure
;   outfile   -  output file for full spectrum fluxes (input to fire_1dspec)
;   sigfile   -  output file for full spectrum uncertainties (input to fire_1dspec)
;
; RETURNS:
;
; OUTPUTS:
;   1d flux      -   (fits file; FSpec/name_order#_F.fits)
;   1d error     -   (fits file; FSpec/name_order#_E.fits)
;
; OPTIONAL KEYWORDS:
;    /SILENT   - No text output
;    HDR - Starter header for output file
;    RESVAL - velocity resolution of spectrum (based on slit size)
;
; OPTIONAL OUTPUTS:
;
; COMMENTS: ;
; EXAMPLES:
;   fire_multispec, fspec, outfile, sigfile, HDR=header, /SILENT
;
; PROCEDURES/FUNCTIONS CALLED:
;
; REVISION HISTORY:
;   14-Jun-2004 Written by GEP
;   27-Jan-2011 Modified from fire_1dspec by AJB
;-
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

pro fire_multispec,fspec,outfile,sigfile, hdr=hdr, resvel=resvel, silent=silent

  func_name = 'fire_multispec'


; SET UP WAVELENGTH GRID FOR FIRE PARAMETERS - CONSTANT VELOCITY RESOLUTION
  velpix=12.5d
  cdelt = velpix/299792.458d/alog(10.0d)

  use = where(fspec.phys_ordr NE 0,nuse)

  gd = where(fspec.wave NE 0., ngd)
  if( ngd EQ 0 ) then begin
  		fire_siren, func_name + ": ERROR! no good points found!  Exiting " + $
  			"completing the task!"
  		RETURN
  endif

; LOOP THROUGH THE USABLE ORDERS 
  for i=0L,nuse-1L do begin

     outfile_ind = strmid(outfile,0,strpos(outfile,'_F.fits'))+'_order'+strtrim(string(fspec.phys_ordr[use[i]]),2)+'_F.fits'
     sigfile_ind = strmid(sigfile,0,strpos(sigfile,'_E.fits'))+'_order'+strtrim(string(fspec.phys_ordr[use[i]]),2)+'_E.fits'

;;;;;;;;; NOTE - USING THE FX PARAMETER HERE - SHOULD BE FLUX, 
;;;;;;;;; NOT SURE WHY THAT IS EMPTY  - 1/27/2011
    gd = where(fspec.fx[*,use[i]] NE 0., ngd)

    if( ngd NE 0 ) then begin

; RANGE OF WAVELENGTHS    
    minwv = min(fspec.wave[gd,use[i]])
    maxwv = max(fspec.wave[gd,use[i]])

; UPDATE HEADER
    if (keyword_set(hdr)) then begin
       head = hdr
       sxdelpar, head, 'CDELT1'
       sxdelpar, head, 'CRPIX1'
       sxdelpar, head, 'CTYPE1'
       sxdelpar, head, 'NAXIS1'
       sxdelpar, head, 'NAXIS2'
       sxdelpar, head, 'CDELT1'
       sxdelpar, head, 'CRVAL1'
     ; Extraneous keywords from when we stored the data in a structure.
       keys = "TFORM"+strtrim((indgen(53)+1),2)
       sxdelpar, head, keys
       keys = "TTYPE"+strtrim((indgen(53)+1),2)
       sxdelpar, head, keys
       sxdelpar, head, "COMMENT"
    endif else begin
       fxhmake, head
    endelse
    sxaddpar, head, 'CDELT1', cdelt
    sxaddpar, head, 'CRPIX1', 1
    sxaddpar, head, 'CRVAL1', alog10(minwv)
    sxaddpar, head, 'CTYPE1', 'LINEAR'
    sxaddpar, head, 'DC-FLAG', 1
    sxaddpar, head, 'BITPIX', -32
    sxaddpar, head, 'NAXIS', 1
    sxaddpar, head, 'NAXIS1', n_elements(tot_flux)
    arr = strsplit(sigfile,"/", /extract)
    sxaddpar, head, 'SIGFILE', arr[n_elements(arr)-1]
    sxaddpar, head, 'ORDER', fspec.phys_ordr[use[i]]
    sxdelpar, head, 'NAXIS2'
    sxdelpar, head, 'XTENSION'
    sxdelpar, head, 'PCOUNT'
    sxdelpar, head, 'GCOUNT'
    sxdelpar, head, 'TFIELDS'
    sxdelpar, head, 'EXTEND'
    sxdelpar, head, 'BITPIX'
    sxdelpar, head, 'SIMPLE'
    sxaddpar, head, 'SIMPLE', 'T', BEFORE="NAXIS"
    if (keyword_set(RESVEL)) then sxaddpar, head, 'RESVEL', resvel
  
    if not keyword_set( SILENT ) then $
     print, func_name+': Writing results to files: ', outfile_ind, ',', sigfile_ind

; WRITE OUT SPECTRA
;;;;;;;;; NOTE - USING THE FX AND VAR PARAMETERS HERE - SHOULD BE FLUX AND SIG
;;;;;;;;; NOT SURE WHY THESE ARE EMPTY  - 1/27/2011
    mwrfits, fspec.fx[gd,use[i]], outfile_ind, head, /create, /silent
    mwrfits, sqrt(fspec.var[gd,use[i]]), sigfile_ind, head, /create, /silent

    endif
    
  endfor  

  return
end
