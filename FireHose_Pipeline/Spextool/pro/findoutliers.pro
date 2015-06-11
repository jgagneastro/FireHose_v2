;+
; NAME:
;     findoutliers
;
; PURPOSE:
;     Determines the outliers in a distribution of data.
;
; CATEGORY:
;     Statistics
;
; CALLING SEQUENCE:
;     result = findoutliers(data,thresh,MEDIAN=median,MAD=mad,CANCEL=cancel)
;
; INPUTS:
;     data   - Input data
;     thresh - The sigma threshold for the robust algorithm
;     
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     MEDIAN - The median value of the data
;     MAD    - The median absolute deviation of the data
;     CANCEL - Set on return if there is an problem
;    
; OUTPUTS:
;     Returns a mask of same size as data where 1=good,0=bad.
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
;     Computes the median and the median absolute deviation (MAD)
;     1.482*median(|x_i-x_med|) of the data and identifies data values 
;     as outliers if |x_i - x_med|/MAD > thresh where x_med is the 
;     median.
;
; EXAMPLE:
;    
; MODIFICATION HISTORY:
;     2001-04-22 - written by M. Cushing, Institute for Astronomy, UH
;     2005-04-12 - Fixed bug if MAD=0, M. Cushing
;     2005-04-14 - Added MEDIAN and MAD keywords
;-
function findoutliers,data,thresh,MEDIAN=median,MAD=mad,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 2 then begin
    
    print, 'Syntax - result = findoutliers(data,thresh,MEDIAN=median,$'
    print, '                               MAD=mad,CANCEL=cancel)'
    cancel = 1
    return,1

endif
cancel = cpar('findoutliers',data,1,'Data',[2,3,4,5],[1,2,3])
if cancel then return,-1
cancel = cpar('findoutliers',thresh,2,'Thresh',[2,3,4,5],0)
if cancel then return,-1

mask = intarr(n_elements(data))+1

med = median(data,/EVEN)
mad = 1.482*median( abs(data-med), /EVEN)
if mad eq 0.0 then goto, cont

z   = where(abs( (data-med)/mad ) gt thresh,count)
if count ne 0 then mask[z] = 0

cont:

median = med

return, mask


end

