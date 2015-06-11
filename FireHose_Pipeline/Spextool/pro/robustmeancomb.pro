; NAME:
;     robustmeancomb
;
; PURPOSE:
;     To robustly combine a data set using a (weighted) mean.
;       
; CATEGORY:
;     data reduction
;
; CALLING SEQUENCE:
;     robustmeancomb,data,thresh,mean,mvar,DATAVAR=datavar,RMS=rms,$
;                    MASK=mask,UPDATE=update,WIDGET_ID=WIDGET_ID,CANCEL=cancel
;
; INPUTS:
;     data   - Data array (2D or 3D).
;     thresh - Sigma threshold to identify outliers.
;
; OUTUTS:
;     mean - The weighted mean array.
;     mvar - The "variance of the mean", sigma_mu^2, array.
;
; KEYWORD PARAMETERS:    
;     DATAVAR   - The variances of the data points.  If given, a
;                 weighted mean is performed.
;     RMS       - Set to return the RMS error instead of the error on
;                 the mean.  This parameter is ignored if DATAVAR are given.
;     MASK      - An optional mask of the same size as data
;                 identifying pixels to use in the combination.
;                 0=bad,1=good
;     UPDATE    - If set, the program will launch the Fanning
;                 showprogress widget.
;     WIDGET_ID - If given, a cancel button is added to the Fanning
;                 showprogress routine.  The widget blocks the
;                 WIDGET_ID, and checks for for a user cancel
;                 command.
;     CANCEL    - Set on return if there is a problem
;
; REVISION HISTORY:
;     2001-04-22 - Written by M. Cushing, Institute for Astronomy, UH
;     2005-04-08 - Added the RMS keyword.
;     2005-06-01 - Added MASK keyword
;
; PROCEDURES CALLED:
;     Requires the Astronomy User's Library.
;
; PROCEDURE:
;     This routine will combine either 1-D or 2-D data robustly using either
;     a straight mean or weighted mean.  A data point is identified as
;     an outlier if |x_i - x_med|/MAD > thresh where x_med is the
;     median and MAD is the median absolute deviation defined as 
;     1.482*median(|x_i-x_med|).

;     If DATAVAR are not given, then a straight mean, <x>, and
;     variance on the mean, sigma_mu^2 = sigma^2/N are computed for
;     the good data points.  If DATAVAR are not given and RMS is set,
;     then the sample variance (sigma^2) is given.  If DATAVAR are
;     given, then a weighted mean and corresponding variance on the
;     mean are computed.  
;
pro robustmeancomb,data,thresh,mean,mvar,DATAVAR=datavar,RMS=rms,MASK=mask,$
                   UPDATE=update,WIDGET_ID=WIDGET_ID,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 2 then begin
    
    print, 'Syntax - robustmeancomb,data,thresh,mean,mvar,DATAVAR=datavar,$'
    print, '                        RMS=rms,MASK=mask,UPDATE=update,$'
    print, '                        WIDGET_ID=widget_id,CANCEL=cancel'
    cancel = 1
    return

endif

zparcheck, 'robustmeancomb',data, 1, [2,3,4,5], [2,3], 'Data array' 
zparcheck, 'robustmeancomb',thresh, 2, [2,3,4,5], 0, 'Sigma Threshold' 

data = double(data)
if n_elements(DATAVAR) ne 0 then datavar = double(datavar)

;  Get size of data array.

s = size(data)
if s[0] eq 2 then begin

    ncols = s[1]
    narr  = s[2]
    mean  = fltarr(ncols,/NOZERO)
    mvar  = fltarr(ncols,/NOZERO)
    if n_elements(MASK) eq 0 then mask = intarr(ncols,narr)+1


endif else begin 

    ncols = s[1]
    nrows = s[2]
    narr  = s[3]
    mean  = fltarr(ncols,nrows,/NOZERO)
    mvar  = fltarr(ncols,nrows,/NOZERO)
    if n_elements(MASK) eq 0 then mask = intarr(ncols,nrows,narr)+1
    
endelse

;  Check for weights.

weighted = (n_elements(DATAVAR) ne 0) ? 1:0

;  Set up the Fanning showprogress object if requested.

if keyword_set(UPDATE) then begin

    cancelbutton = (n_elements(WIDGET_ID) ne 0) ? 1:0
    progressbar = obj_new('SHOWPROGRESS',widget_id,color=2,$
                          CANCELBUTTON=cancelbutton,title='Robustmeancomb',$
                          MESSAGE='Robustly averaging the data...')
    progressbar -> start
    
endif

;  Perform combination.

for i = 0, ncols-1 do begin

    if s[0] eq 2 then begin

        start = where(mask[i,*] eq 1,cnt)
        if cnt eq 0L then begin
          mvar[i] = 0.
          mean[i] = !values.f_nan
        endif else begin
          good  = where(findoutliers(data[i,start],thresh) eq 1,count)
          if weighted then begin
              
              mvar[i] = 1./total(1./datavar[i,start[good]],/nan)
              mean[i] = total(data[i,start[good]]/datavar[i,start[good]],/nan)*mvar[i]
  
          endif else begin
  
              mean[i] = total(data[i,start[good]])/count
              del     = data[i,start[good]]-mean[i]
              mvar[i] = (total(data[i,start[good]]^2) - $
                         count*mean[i]^2)/(count-1)
              if not keyword_set(RMS) then mvar[i] = mvar[i]/float(count)
              
          endelse
        endelse
    endif else begin
    
        for j = 0, nrows-1 do begin
            
            start = where(mask[i,j,*] eq 1,cnt)
            good  = where(findoutliers(data[i,j,start],thresh) eq 1,count)
            
            if weighted then begin

                mvar[i,j] = 1./total(1./datavar[i,j,start[good]])
                mean[i,j] = total(data[i,j,start[good]]/ $
                                  datavar[i,j,start[good]])*mvar[i,j]
               
            endif else begin

                mean[i,j] = total(data[i,j,start[good]])/count
                mvar[i,j] = (total(data[i,j,start[good]]^2)- $
                             count*mean[i,j]^2)/$
                  (count-1)
                if not keyword_set(RMS) then mvar[i,j] = mvar[i,j]/float(count)
                
            endelse

        endfor

    endelse

    if keyword_set(UPDATE) then begin
        
        if cancelbutton then begin
            
            cancel = progressBar->CheckCancel()
            if cancel then begin
                
                progressBar->Destroy
                obj_destroy, progressbar
                cancel = 1
                return
                
            endif
            
        endif
        percent = (i+1)*(100./float(ncols-1))
        progressbar->update, percent
        
    endif

endfor

if keyword_set(UPDATE) then begin

    progressbar-> destroy
    obj_destroy, progressbar

endif

end




