;+
; NAME:
;     slittrans
;
; PURPOSE:
;     Compute flux passing through a slit assuming a gaussian PSF.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = slittrans(width,height,fwhm,xoffset,yoffset,CANCEL=cancel)
;
; INPUTS:
;     width   - Width of slit.
;     height  - Height of slit.
;     fwhm    - Full-width at half-maximum of the gaussian image.
;     xoffset - Offset in x of the image from the center of the slit.
;     yoffset - Offset in y of the image from the center of the slit.
;
;     Note: the units are arbitrary but they must be the same within
;           all of the input quantities.  
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     Returned is the fraction of the total gaussian included in the slit.
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
;          
;
; PROCEDURE:
;     Easy
;
; EXAMPLE:
;     result = slittrans(0.3,15,0.6,0,0,CANCEL=cancel)
;     
;     Computes the fraction of the flux transmitted through a slit 
;     0.3x15 arcseconds with a PSF of 0.6 arcseconds FWHM.  The PSF is
;     centered on the slit.
;
; MODIFICATION HISTORY:
;     Based on M Buie program,  1991 Mar., Marc W. Buie, Lowell Observatory
;     Modified 2000 Apr., M. Cushing to include y offsets.
;-
function slittrans,width,height,fwhm,xoffset,yoffset,CANCEL=cancel

cancel = 0

if n_params() ne 5 then begin
    
    print, 'Syntax - result = slittrans(width,height,fwhm,xoffset,yoffset,$'
    print, '                            CANCEL=cancel)'
    cancel = 1
    return, -1

endif

cancel = cpar('slittrans',width,1,'Width',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('slittrans',height,2,'Height',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('slittrans',fwhm,3,'FWHM',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('slittrans',xoffset,4,'Xoffset',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('slittrans',yoffset,5,'Yoffset',[2,3,4,5],0)
if cancel then return,-1


;  Go ahead

a = width / 2.0
b = height / 2.0
s = fwhm/(sqrt(8.0*alog(2.0)))  

slit = ( 1.0 - gaussint( -(a+xoffset)/s ) - gaussint( -(a-xoffset)/s ) ) * $
       ( 1.0 - gaussint( -(b+yoffset)/s ) - gaussint( -(b-yoffset)/s ) )

return,slit

end

