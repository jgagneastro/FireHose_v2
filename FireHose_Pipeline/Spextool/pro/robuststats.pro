;+
; NAME:
;     robuststats
;
; PURPOSE:
;     Computes statistics on a data set avoiding deviant points.
;
; CATEGORY:
;     Statistics
;
; CALLING SEQUENCE:
;     robuststats,data,thresh,mean,var,stddev,skew,kurt,EXCLUDE=exclude,$
;                 IGOODBAD=igoodbad,OGOODBAD=ogoodbad,SILENT=silent,$
;                 CANCEL=cancel
;
; INPUTS:
;     data   - Vector of data values
;     thresh - Sigma threshold over which values are identified as outliers
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     EXCLUDE  - Numbers of data points to exclude before starting loop.
;                Ignored if given IGOODBAD
;     IGOODBAD - An input goodbad array.  0=bad, 1=good, 2=nan
;     OGOODBAD - An output goodbad array
;     SILENT   - Set to supress printing results to the screen
;     CANCEL   - Set on return if there is a problem
;
; OUTPUTS:
;     mean   - Mean
;     var    - Variance
;     stddev - Standard deviation 
;     skew   - Skewness
;     kurt   - Kurtosis	
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
;     A data point is identified as an outlier if |x_i - x_med|/MAD > 
;     thresh where x_med is the median and MAD is the median absolute 
;     deviation defined as 1.482*median(|x_i-x_med|).
;     
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     1999-10-02 - Written by M. Cushing, Institute for Astronomy, UH
;     2000-12-08 - Fixed bug in first loop.  Too hard to explain, and
;                  not really important, but a bug nevertheless.
;     2001-07-11 - Now uses the median and median absolute deviation 
;                  to determine outliers.  As a result, the parameter
;                  eps has been removed.
;-
pro robuststats,data,thresh,mean,var,stddev,skew,kurt,EXCLUDE=exclude,$
                IGOODBAD=igoodbad,OGOODBAD=ogoodbad,SILENT=silent,CANCEL=cancel

cancel = 0

; Check parameters

if n_params() lt 3 then begin
    
    print, 'Syntax - robuststats,data,thresh,mean,var,stddev,skew,kurt,$'
    print, '                     EXCLUDE=exclude,IGOODBAD=igoodbad,$'
    print, '                     OGOODBAD=ogoodbad,SILENT=silent,CANCEL=cancel'
    cancel = 1
    return

endif
cancel = cpar('robuststats',data,1,'Data',[1,2,3,4,5,12],[1,2,3,4,5,6])
if cancel then return
cancel = cpar('robuststats',thresh,2,'Thresh',[1,2,3,4,5,12],0)
if cancel then return

;  Check for an input goodbad array.

if n_elements(igoodbad) ne 0 then begin

    nan      = where(finite(data) eq 0, count_nan)    
    if count_nan ne 0 then igoodbad[nan] = 2
    z        = where(igoodbad eq 1,count_initgood)
    datas    = data[z]
    ogoodbad = igoodbad

endif else begin             

    igoodbad = replicate(1,n_elements(data))
    nan      = where(finite(data) eq 0, count_nan)    
    if count_nan ne 0 then igoodbad[nan] = 2
    z        = where(igoodbad eq 1,count_initgood)

    if count_initgood eq 0 then begin

        mean   = !values.f_nan
        var    = !values.f_nan
        stddev = !values.f_nan
        skew   = !values.f_nan
        kurt   = !values.f_nan
        cancel = 1
        return

    endif

    datas    = data[z]
    ogoodbad = igoodbad

endelse 

;  Determine the median and median absolute deviations.

if n_elements(EXCLUDE) ne 0 and n_elements(IGOODBAD) eq 0 then begin
    
    if 2*exclude ge count_initgood-2 then begin

        print, '2*EXCLUDE ge # of data points-2.'
        print, 'Require at least two data points.'
        cancel = 1
        return

    endif
    order = sort(datas)
    med = median(datas[order[exclude:n_elements(datas)-exclude-1]],/EVEN)
    mad = 1.482*median(abs(datas[order[exclude:n_elements(datas)-exclude-1]]-$
                                 med), /EVEN)

endif else begin

    med = median(datas,/EVEN)
    mad = 1.482*median(abs(datas-med), /EVEN)

endelse

good = where(abs( (datas-med)/mad ) le thresh,count_new)
bad  = where(abs( (datas-med)/mad ) gt thresh)

good_bad = replicate(0,count_initgood)
if count_new ne 0 then begin

    moments, datas[good],mean,var,stddev,skew,kurt,/DOUBLE
    good_bad[good] = 1
    
endif else begin

    moments,datas,mean,var,stddev,skew,kurt,/DOUBLE
    good_bad[*] = 1

endelse

; Reconstruct goodbad array

ogoodbad[z]    = good_bad

;  Print out results it SILENT not set.

if not keyword_set(SILENT) then begin

    z = where(ogoodbad eq 0,nbad) 
    print, ' '
    print,'Results:'
    print, ' '
    print, 'Total number of data points: ',strcompress(n_elements(data),/re)
    print, '             Number of NaNs: ',strcompress(count_nan,/re)
    print, '       Number of bad points: ',strcompress(nbad,/re)
    print, ' '
    print, '              Mean: ',strcompress(mean,/re)
    print, '          Variance: ',strcompress(var,/re)
    print, 'Standard Deviation: ',strcompress(stddev,/re)
    print, '          Skewness: ',strcompress(skew,/re)
    print, '          Kurtosis: ',strcompress(kurt,/re)
    
endif

end









