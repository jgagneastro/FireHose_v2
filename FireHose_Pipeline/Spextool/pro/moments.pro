;+
; NAME:
;     moments
;
; PURPOSE:
;     Computes statistics on an array.
;
; CATEGORY:
;     Statistics
;
; CALLING SEQUENCE:
;     moments,data,mean,var,stddev,skew,kurt,mean_stddev,CANCEL=cancel
;
; INPUTS:
;     data - An array of data
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     DOUBLE - Set to compute in double precision
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     mean        - Mean
;     var         - Variance 
;     stddev      - Standard 
;     skew        - Skewness
;     kurt        - Kurtosis   
;     mean_sttdev - The error on the mean
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
;     Calls moment and computes the stddev.  Automatically searches
;     for NaNs.  Based on M. Buie's moment4.
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2001 - Written by M. Cushing, Institute for Astronomy, UH
;     2005-08-02 - Added mean_stddev output, M. Cushing, Steward
;                  Observatory, University of Arizona
;
pro moments,data,mean,var,stddev,skew,kurt,mean_stddev,DOUBLE=double, $
            CANCEL=cancel

cancel = 0

if n_params() lt 1 then begin
    
    print, 'Syntax - moments,data,mean,var,stddev,skew,kurt,mean_sttdev,$'
    print, '                 DOUBLE=double,CANCEL=cancel'
    cancel = 1
    return

endif

n = n_elements(data)

if n le 1 then begin
    
    print, 'Error:  data must have at least 2 elements.'
    return
    
endif

result  = moment(data,/NAN,DOUBLE=double)
mean    = result[0]
var     = result[1]
stddev  = sqrt(var)
skew    = result[2]
kurt    = result[3]

mean_stddev = stddev/sqrt(n)

end





