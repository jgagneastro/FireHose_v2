;+
; NAME:
;     mc_mkspexhdr
;
; PURPOSE:
;     To create a Spextool FITS header
;
; CATEGORY:
;     Data Reduction
;
; CALLING SEQUENCE:
;     hdr = mc_mkspexhdr(array,orders,naps,XUNITS=xunits,YUNITS=yunits,$
;                        PXTITLE=pxtitle,PYTITLE=pytitle,WAVETYPE=wavetype,$
;                        EXTRA=extra,COMMENTS=comments,CANCEL=cancel)
;
; INPUTS:
;     array  - A Spextool spectra array [npix,3,naps*norders]
;              wave  = arr[*,0, ]
;              flux  = arr[*,1, ]
;              error = arr[*,2, ]
;     orders - An array (or scalar) of order numbers 
;     naps   - The number of apertures
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     XUNITS   - A string giving the xunits
;     YUNITS   - A string giving the yunits
;     PXTITLE  - A string giving the xtitle that can be used in IDL
;     PYTITLE  - A string giving the ytitle that can be used in IDL
;     WAVETYPE - If xunits is a wavelength, a string giving the type
;                of wavelengths (Air/Vacuum)
;     EXTRA    - A structure with two tags, vals (values) and coms
;                (comments), that has additional keywords and comments.
;                EXTRA.vals is a structure where the tag name is the
;                FITS keyword and the value is the value of the FITS
;                keyword.  EXTRA.coms is a structure where the tag
;                name is the FITS keyword and the value is the comment
;                for the FITS keyword.
;     COMMENTS - A string giving comments
;     CANCEL   - Set on return if there is a problem
;
; OUTPUTS:
;     hdr - A FITS header that can be read by xvspec.
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
;     hdr = mc_mkspexhdr,array,[3,4,5],2,'um','Wm-2um-1,'Vacuum',CANCEL=cancel
;
; MODIFICATION HISTORY:
;     2004-11-19 - Written by M. Cushing, NASA Ames Research Center
;-
function mc_mkspexhdr,array,orders,naps,XUNITS=xunits,YUNITS=yunits,$
                      PXTITLE=pxtitle,PYTITLE=pytitle,WAVETYPE=wavetype,$
                      EXTRA=extra,COMMENTS=comments,CANCEL=cancel

  cancel = 0

  if n_params() lt 3 then begin
     
     print, 'Syntax - hdr = mc_mkspexhdr(array,orders,naps,XUNITS=xunits,$'
     print, '                            YUNITS=yunits,PXTITLE=pxtitle,$'
     print, '                            PYTITLE=pytitle,WAVETYPE=wavetype,$'
     print, '                            EXTRA=extra,COMMENTS=comments,$'
     print, '                            CANCEL=cancel)'
     cancel = 1
     return, -1
     
  endif
  
  cancel = cpar('mc_mkspexhdr',array,1,'Array',[2,3,4,5],[2,3])
  if cancel then return,-1
  cancel = cpar('mc_mkspexhdr',orders,2,'Orders',[2,3],[0,1])
  if cancel then return,-1
  cancel = cpar('mc_mkspexhdr',naps,3,'Naps',[2,3],0)
  if cancel then return,-1
  
  if n_elements(xunits) ne 0 then begin
     
     cancel = cpar('mc_mkspexhdr',xunits,4,'Xunits',7,0)
     if cancel then return,-1
     
  endif
  
  if n_elements(yunits) ne 0 then begin
     
     cancel = cpar('mc_mkspexhdr',yunits,5,'Yunits',7,0)
     if cancel then return,-1
     
  endif
  
  if n_elements(PXTITLE) ne 0 then begin
     
     cancel = cpar('mc_mkspexhdr',pxtitle,6,'Pxtitle',7,0)
     if cancel then return,-1
     
  endif
  
  if n_elements(PYTITLE) ne 0 then begin
     
     cancel = cpar('mc_mkspexhdr',pytitle,7,'Pytitle',7,0)
     if cancel then return,-1
     
  endif
  
  if n_elements(wavetype) ne 0 then begin
     
     cancel = cpar('mc_mkspexhdr',wavetype,8,'Wavetype',7,0)
     if cancel then return,-1
     
  endif
  
  
  norders = n_elements(orders)
  orders = strjoin(strtrim(orders,2),',')
  
  mkhdr,hdr,array
  
;  Do EXTRA keywords if given
  
  if n_elements(EXTRA) ne 0 then begin
     
     ntags = n_tags(EXTRA.vals)
     names = tag_names(EXTRA.vals)
     
     for i =0, ntags-1 do fxaddpar,hdr,names[i],extra.vals.(i),extra.coms.(i)
     
  endif
  
  fxaddpar,hdr,'NORDERS',norders,' Number of Orders'
  fxaddpar,hdr,'ORDERS',orders,' Order Numbers'
  fxaddpar,hdr,'NAPS',naps,' Number of Apertures'
  
  if n_elements(xunits) ne 0 then fxaddpar,hdr,'XUNITS',xunits,$
                                           ' Wavelength Units'
  
  if n_elements(yunits) ne 0 then fxaddpar,hdr,'YUNITS',yunits,' Flux Units'
  
  if n_elements(wavetype) ne 0  then fxaddpar,hdr,'WAVETYPE',wavetype,$
     ' Wavelength Type'
  
  if n_elements(pxtitle) ne 0 then fxaddpar,hdr,'XTITLE',pxtitle,$
                                            ' IDL X Title'
  
  if n_elements(pytitle) ne 0 then fxaddpar,hdr,'YTITLE',pytitle,$
                                            ' IDL Y Title'
  
  if n_elements(COMMENTS) ne 0 then begin
     
     fxaddpar,hdr,'COMMENT',' '
     text = mc_splittext(comments,69)
     for i = 0,n_elements(text)-1 do fxaddpar,hdr,'COMMENT',text[i]
     
  endif
  
  return, hdr

end
