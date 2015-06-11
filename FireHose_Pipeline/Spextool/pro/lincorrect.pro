;+
; NAME:
;     lincorrect
; 
; PURPOSE:
;     To correct for linearity of the SpeX array
;    
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     lincorrect,image,itime,slowcnts,ndr,coefficients,[saturation,saturated],$
;                CANCEL=cancel   
;
; INPUTS:
;     image        - A two dimensional floating point array ([NCols, NRows]) 
;		     that contains the image that is to be corrected.
;     slowcnts     - The number of slow counts.
;     ndr          - The number of non-destructive reads
;     coefficients - A three dimensional floating point array ([NCols, NRows, 
;		     FitOrder+1]) that contains the coefficients needed to 
;		     make the correction (an output of the LINLINEFIT 
;		     routine).
;    
; OPTIONAL INPUTS:
;     saturation   - A two dimensional floating point array ([NCols, NRows]) 
;		     that contains the saturation points for each pixel.  If 
;		     this input is given, the pixels in the image variable 
;		     that have a value greater then the value in this variable 
;		     are not corrected.  These pixels are flagged in the 
;		     output variable saturated (if it is given).
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;     
; OUTPUTS:
;     Returns a two dimensional floating point array ([NCols, NRows]) that 
;     contains the corrected image.
;     
; OPTIONAL OUTPUTS:
;     saturated - If given, this variable with hold a mask ([NCols, NRows]) of 
;		  the pixels in the image input variable that had values 
;		  greater than the saturation points given by the saturation 
;		  input variable.
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
;     If the saturation points are given (via the saturation input variable), 
;     the values in the image variable that are above the saturation points 
;     are saved (if the saturated output variable is given, then the saturated 
;     pixels are marked in this variable by a 1).  The entire image is then 
;     corrected by multiplying it by the factor 1/(1 + (a1/a0)*counts + ... + 
;     (an/a0)*counts^n) where the an's are the coefficients given in the 
;     coefficients input variable and the counts are the values given in the 
;     image input variable.  If the saturation points are given, the saved 
;     saturated values are then put back in the image and the corrected image 
;     is returned.
;
; EXAMPLE:
;     
;
; MODIFICATION HISTORY
;     2002-04-29 - Written by J. R. Leong, Institute for Astronomy, UH 
;     2002-10-08 - Added check for NaNs.  M. Cushing
;     2003-03-27 - Correct for pedestal level. W. Vacca
;     2003-07-24 - Added catch to force correction to be >=1.0
;-
function lincorrect,image,itime,slowcnts,ndr,coefficients,saturation,$
                    saturated,CANCEL=cancel

cancel = 0

if n_params() lt 5 then begin

    print, 'Syntax - result = lincorrect(image,itime,slowcnts,ndr,$'
    print, '                             coefficients,saturation,$'
    print, '                             saturated,CANCEL=cancel'
    cancel = 1
    return,-1

endif

cancel = cpar('lincorrect',image,1,'Image',[2,3,4,5],2)
if cancel then return,-1
cancel = cpar('lincorrect',itime,2,'Itime',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('lincorrect',slowcnts,3,'Slowcnts',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('lincorrect',ndr,4,'Ndr',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('lincorrect',coefficients,5,'Coefficients',[2,3,4,5],3)
if cancel then return,-1

; Find saturated pixels and save their original values if requested.

if n_params() eq 6 then begin

    w = where(image gt saturation, count)
    if count ne 0 then satpixels = image[w]

endif

; Create saturated image if requested.

if n_params() eq 7 then begin

    imgsize = size(image)
    saturated = bytarr(imgsize[1], imgsize[2])
    saturated[w] = 1

endif

slowdata = [  2,  3,   4,   5,   6,   7,   8,   9,   10,  11,  12,  13,$
             14, 15,  16,  17,  18,  19,  20]
tread    = [0.10,0.18,0.20,0.22,0.24,0.26,0.28,0.30,0.32,0.34,0.36,0.38,$
            0.40,0.41,0.43,0.45,0.47,0.49,0.51]

; Correct the image.

size = size(coefficients)
;oldcoeff = coefficients

;for i = 0, size[3]-1 do coefficients[*, *, i] = oldcoeff[*, *, i]/ $
;                                                oldcoeff[*, *, 0]

c0 = coefficients[*,*,0]

z = where(slowcnts eq slowdata)
pedimage = image*total(tread(z))*(float(ndr)+1.0)/(2.0*itime)  ; DN/s * t_readout * (ndr+1)/2
finimage = image + pedimage

c_pedimage = pedimage*(c0/imagepoly(pedimage, coefficients) > 1.0)
c_finimage = finimage*(c0/imagepoly(finimage, coefficients) > 1.0)

;c_pedimage = pedimage*c0/imagepoly(pedimage, coefficients)
;c_finimage = finimage*c0/imagepoly(finimage, coefficients)

newimage = c_finimage - c_pedimage 

; second iteration

pedimage = newimage*total(tread(z))*(float(ndr)+1.0)/(2.0*itime)
finimage = image + pedimage
  
c_pedimage = pedimage*(c0/imagepoly(pedimage, coefficients) > 1.0)
c_finimage = finimage*(c0/imagepoly(finimage, coefficients) > 1.0)

;c_pedimage = pedimage*c0/imagepoly(pedimage, coefficients)
;c_finimage = finimage*c0/imagepoly(finimage, coefficients)

newimage = c_finimage - c_pedimage 

; Put the original values of the saturated pixel back into the image.

if n_params() eq 6 then if count ne 0 then newimage[w] = satpixels

; Return the new image.

z = where(finite(newimage) eq 0,count)
if count ne 0 then newimage[z] = 0.0

return, newimage

end
