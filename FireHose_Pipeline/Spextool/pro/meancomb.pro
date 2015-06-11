;+
; NAME:
;     meancomb
;
; PURPOSE:
;     Combines a data set using a (weighted) mean.
;
; CATEGORY:
;     Image Manipulation
;
; CALLING SEQUENCE:
;     meancomb,data,mean,mvar,MASK=mask,DATAVAR=datavar,NAN=nan,RMS=rms,$
;              CANCEL=cancel
;
; INPUTS:
;     data - Data array (1, 2 or 3D).
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     MASK      - An optional mask of the same size as data
;                 identifying pixels to use in the combination.
;                 0=bad,1=good
;     DATAVAR   - The variances of the data points.  If given, a
;                 weighted mean is performed.
;     NAN       - Set to ignore NaN values.  Same as passing a MASK
;                 with NaN pixels identified as 0.
;     RMS       - Set to return the RMS error instead of the error on
;                 the mean.  This parameter is ignored if DATAVAR are given.
;     CANCEL    - Set on return if there is a problem
;    
; OUTPUTS:
;     mean - The mean array.
;     mvar - The "variance of the mean", sigma_mu^2, array.
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
;     If the data is 2D, then the combining happens in the row
;     direction.  Used to combine spectra.
;
; PROCEDURE:
;     This routine will combine either 1, 2 or 3-D data using either
;     a straight mean or weighted mean.  If DATAVAR are not given, 
;     then a straight mean, <x>, and variance on the mean, 
;     sigma_mu^2 = sigma^2/N are computed.  If DATAVAR are given, 
;     then a weighted mean and corresponding variance on the mean are
;     computed. 
;
; EXAMPLE:
; 
; MODIFICATION HISTORY:
;     2001-04-22 - Written by M. Cushing, Institute for Astronomy, UH
;     2003-04-10 - Changed the 3D weighted average.
;                  Removed Fanning showprogress popup.
;     2003-05-27 - Added the MASK keyword, M. Cushing
;     2004-10-10 - Added the NAN keyword
;     2005-04-08 - Added the RMS keyword
;-
pro meancomb,data,mean,mvar,MASK=mask,DATAVAR=datavar,NAN=nan,RMS=rms, $
             CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 1 then begin
    
    print, 'Syntax - meancomb,data,mean,mvar,MASK=mask,DATAVAR=datavar,$'
    print, '                  NAN=nan,RMS=rms,CANCEL=cancel'
    cancel = 1
    return

endif
cancel = cpar('meancomb',data,1,'Data',[2,3,4,5],[1,2,3])
if cancel then return

;  Check for weights.

if n_elements(DATAVAR) ne 0 then weighted = 1 else weighted=0

if keyword_set(NAN) then begin

    mask = fix(data*0.0)+1
    z = where(finite(data) eq 0,cnt)
    if cnt ne 0 then mask[z] = 0

endif

;  Get size of data array.

s = size(data)

case s[0] of 

    1: begin

        narr = n_elements(data)
        if n_elements(MASK) eq 0 then mask = intarr(narr)+1.0
 
    end
    2: begin

        ncols = s[1]
        narr  = s[2]
        if n_elements(MASK) eq 0 then mask = intarr(ncols,narr)+1.0

    end
    
    3: begin

        ncols = s[1]
        nrows = s[2]
        narr  = s[3]
        mean  = dblarr(ncols,nrows,/NOZERO)
        mvar  = dblarr(ncols,nrows,/NOZERO)
        if n_elements(MASK) eq 0 then mask = intarr(ncols,nrows,narr)+1.0

    end

endcase

;  Perform combination.

if s[0] eq 1 then begin

    if weighted then begin

        z    = where(mask eq 1)
        mvar = 1./total(1./datavar[z],/DOUBLE)
        mean = total(data[z]/datavar[z],/DOUBLE)*mvar

    endif else begin

        z    = where(mask eq 1,cnt)
        mean = total(data[z],/DOUBLE)/cnt
        mvar = ( total(double(data[z])^2)-cnt*mean^2 )/(cnt-1)
        if not keyword_set(RMS) then mvar=mvar/cnt

    endelse

endif

if s[0] eq 2 then begin

    if weighted then begin

        z = where(mask eq 0,cnt)

        if cnt ne 0 then begin

            mvar = fltarr(ncols)
            mean = fltarr(ncols)

            for i = 0,ncols-1 do begin

                z = where(mask[i,*] eq 1)
                mvar[i] = 1./total(1./datavar[i,z],2,/DOUBLE)
                mean[i] = total(data[i,z]/datavar[i,z],2,/DOUBLE)*mvar[i]

            endfor

        endif else begin

            mvar = 1./total(1./datavar,2,/DOUBLE)
            mean = total(data/datavar,2,/DOUBLE)*mvar

        endelse

    endif else begin

        z = where(mask eq 0,cnt)
        if cnt ne 0 then data[z] = 0.0
        ndat = total(mask,2)

        mean = total(data,2,/DOUBLE)/ndat
        mvar = ( total(double(data)^2,2)-ndat*mean^2 )/(ndat-1)
        if not keyword_set(RMS) then mvar=temporary(mvar)/ndat

    endelse

endif

if s[0] eq 3 then begin

    if weighted then begin

        z = where(mask eq 0,cnt)

        if cnt ne 0 then begin

            mvar = fltarr(ncols,nrows)
            mean = fltarr(ncols,nrows)

            for i = 0,ncols-1 do begin

                for j=0,nrows-1 do begin

                    z = where(mask[i,j,*] eq 1)
                    mvar[i] = 1./total(1./datavar[i,j,z],2,/DOUBLE)
                    mean[i] = total(data[i,j,z]/datavar[i,j,z],2,/DOUBLE)*mvar[i]
                    
                endfor

            endfor
            
        endif else begin

            mvar = 1./total(1./datavar,3,/DOUBLE)
            mean = total(temporary(data)/datavar,3,/DOUBLE)*mvar
            
        endelse
        
    endif else begin
        
        z = where(mask eq 0,cnt)
        if cnt ne 0 then data[z] = 0.0
        
        ndat = total(mask,3)

        mean = total(data,3,/DOUBLE)/ndat
        mvar = ( total(double(data)^2,3)-ndat*mean^2 )/(ndat-1)
        if not keyword_set(RMS) then mvar=temporary(mvar)/ndat

    endelse

endif

mean = float(mean)
mvar = float(mvar)


end




