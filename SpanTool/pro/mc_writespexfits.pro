;+
; NAME:
;     mc_writespexfits
;
; PURPOSE:
;     To write a SpeX FITS file given a wavelength and flux array
;
; CATEGORY:
;     Spectroscopy     
;
; CALLING SEQUENCE:
;     mc_writespexfits,wave,flux,oname,ERROR=error,XUNITS=xunits,$
;                     YUNITS=yunits,PXTITLE=pxtitle,PYTITLE=pytitle,$
;                     WAVETYPE=wavetype,EXTRA=extra,COMMENTS=comments,$
;                     CANCEL=cancel
;
; INPUTS:
;     wave  - The wavelength array
;     flux  - The flux array
;     oname - A string of the name of the Spextool FITS file
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     PXTITLE  - A string giving the XTITLE that can be used in IDL
;     PYTITLE  - A string giving the YTITLE that can be used in IDL
;     XUNITS   - The xunits
;     YUNITS   - The yunits
;     WAVETYPE - If the xunits are wavelengths, the type of
;                wavelength, i.e. air/vacuum.
;     EXTRA    - A structure with two tags, vals (values) and coms
;                (comments), that has additional keywords and comments.
;                EXTRA.vals is a structure where the tag name is the
;                FITS keyword and the value is the value of the FITS
;                keyword.  EXTRA.coms is a structure where the tag
;                name is the FITS keyword and the value is the comment
;                for the FITS keyword.
;                
;                e.g., 
;
;                vals = {name:'img001.fits'}
;                coms = {name:' FITS file name'}
;                extra = {vals:vals,coms:coms}
;
;     COMMENTS - A string giving comments.
;     CANCEL   - Set on return if there is a problem
;
; OUTPUTS:
;      Writes a Spextool FITS file to disk.
;
; OPTIONAL OUTPUTS:
;      None
;
; COMMON BLOCKS:
;      None
;
; SIDE EFFECTS:
;      None
;
; RESTRICTIONS:
;      None
;
; PROCEDURE:
;      Creates a header and writes a FITS file to disk
;
; EXAMPLE:
;      writespexfits,wave,flux,'test.fits',error,'um','Wm-2um-1','Vacuum'
;
; MODIFICATION HISTORY:
;      2004-02-25 - Written by M. Cushing, NASA Ames Research Center
;      2004-02-26 - Added the xunits and yunits inputs
;      2004-11-19 - Added the error and wavetype inputs
;      2005-02-22 - Added EXTRA and COMMENTS keyword
;
;-
pro mc_writespexfits,wave,flux,oname,ERROR=error,XUNITS=xunits,$
                     YUNITS=yunits,PXTITLE=pxtitle,PYTITLE=pytitle,$
                     WAVETYPE=wavetype,EXTRA=extra,COMMENTS=comments,$
                     CANCEL=cancel

  cancel = 0

  if n_params() lt 3 then begin
     
     print, 'Syntax - mcc_writespexfits,wave,flux,oname,ERROR=error,$'
     print, '                           XUNITS=xunits,YUNITS=yunits$'
     print, '                           PXTITLE=pxtitle,PYTITLE=pytitle,$'
     print, '                           WAVETYPE=wavetype,EXTRA=extra,$'
     print, '                           COMMENTS=comments,CANCEL=cancel'
     cancel = 1
     return
     
  endif
  
  cancel = cpar('mc_writespexfits',wave,1,'Wave',[2,3,4,5],1)
  if cancel then return
  cancel = cpar('mc_writespexfits',flux,2,'Flux',[2,3,4,5],1)
  if cancel then return
  cancel = cpar('mc_writespexfits',oname,3,'Oname',7,0)
  if cancel then return
  
  if n_elements(ERROR) ne 0 then begin
     
     cancel = cpar('mc_writespexfits',error,4,'Error',[2,3,4,5],[0,1])
     if cancel then return
     
  endif
  
  if n_elements(XUNITS) ne 0 then begin
     
     cancel = cpar('mc_writespexfits',xunits,5,'Xunits',7,0)
     if cancel then return
     
  endif
  
  if n_elements(YUNITS) ne 0 then begin
     
     cancel = cpar('mc_writespexfits',yunits,6,'Yunits',7,0)
     if cancel then return
     
  endif
  
  if n_elements(PXTITLE) ne 0 then begin
     
     cancel = cpar('mc_writespexfits',pxtitle,7,'Xtitle',7,0)
     if cancel then return
     
  endif
  
  if n_elements(PYTITLE) ne 0 then begin
     
     cancel = cpar('mc_writespexfits',pytitle,8,'Ytitle',7,0)
     if cancel then return
     
  endif
  
  if n_elements(WAVETYPE) ne 0  then begin
     
     cancel = cpar('mc_writespexfits',wavetype,9,'Wavetype',7,0)
     if cancel then return
     
  endif
  
  
  
  if n_elements(error) eq 0 then error = replicate(1,n_elements(wave))
  
  array = [[wave],[flux],[error]]
  hdr = mc_mkspexhdr(array,1,1,XUNITS=xunits,YUNITS=yunits,PYTITLE=pytitle,$
                     PXTITLE=pxtitle,WAVETYPE=wavetype,EXTRA=extra,$
                     COMMENTS=comments,CANCEL=cancel)
  if cancel then return
  
  writefits,oname,array,hdr
  
end
