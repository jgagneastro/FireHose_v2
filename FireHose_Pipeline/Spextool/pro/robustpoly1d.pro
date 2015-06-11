;+
; NAME:
;     robustpoly1d
;
; PURPOSE:
;     Robustly fits a polynomial of a given order to a data set.
;
; CATEGORY:
;     Fitting Data
;
; CALLING SEQUENCE:
;     result = robustpoly1d,x,y,order,thresh,eps,SVD=svd,GAUSSJ=gaussj,$
;                           YERR=yerr,THRESHSVD=threshsvd,IGOODBAD=igoodbad,$
;                           SILENT=silent,OGOODBAD=ogoodbad,COVAR=covar,$
;                           YFIT=yfit,RMS=rms,CHISQ=chisq,RCHISQ=rchisq,Q=q,$
;                           VAR=var,JUSTFIT=justfit,CANCEL=cancel
;    
; INPUTS:
;     x      - An array of independent values.
;     y      - An array of dependent values.
;     order  - The order of the polynomial to be fit to the data.
;     thresh - Sigma threshold to identify outliers.
;     eps    - Limit of the fractional change in the standard
;              deviation of the fit.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     SVD       - Set to solve the linear equations using Single Value
;                 Decomposition.  
;     GAUSS     - Set to solve the normal equations using Gauss-Jordan
;                 elimination.  This is the default.
;     YERR      - A vector of errors for the dependent values. 
;     THRESHSVD - If SVD is set, the threshold to identify singular
;                 values.  The default is 1e-12.
;     IGOODBAD  - Input goodbad array. 0=bad, 1=good, 2=nan
;     SILENT    - Set to silence the report of the results at the
;                  end.
;     OGOODBAD  - Output goodbad array. 0=bad, 1=good, 2=nan 
;     COVAR     - The covariance matrix.
;     YFIT      - A vector of calculated Y values.
;     RMS       - The rms deviation of the fit.
;     CHISQ     - The Chi-Squared of the fit.
;     RCHISQ    - The reduced Chi-Squared of the fit.
;     Q         - The goodness-of-fit parameter.
;     VAR       - A vector which contains the variance estimates
;                 of the coefficients.
;     JUSTFIT   - Set to have the routine only determine the fit and 
;                 not chi^2 etc.
;     CANCEL   - Set on return if there is a problem
;     
; OUTPUTS:
;      Returns the coefficients of the polynomial
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
;     Same as poly_fit1d except it iteratively throws data points out.
;     Very basic.
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2000-03-07 - Written by M. Cushing, Institute for Astronomy, UH
;     2000-12-02 - Fixed bug that did not calculate NDOF correctly.
;     2000-12-08 - Changed WEIGHTS keyword to YERR.
;     2002-11-11 - Added the JUSTFIT keyword.
;-
function robustpoly1d,x,y,order,thresh,eps,SVD=svd,GAUSSJ=gaussj,$
                      YERR=yerr,THRESHSVD=threshsvd,IGOODBAD=igoodbad,$
                      SILENT=silent,OGOODBAD=ogoodbad,COVAR=covar,YFIT=yfit,$
                      RMS=rms,CHISQ=chisq,RCHISQ=rchisq,Q=q,VAR=var,$
                      JUSTFIT=justfit,CANCEL=cancel

cancel = 0
ittr   = 1

;  Check parameters

if n_params() lt 5 then begin
    
    print, 'Syntax - coeff =  robustpoly1d(x,y,order,thresh,eps,SVD=svd,$'
    print, '                               GAUSSJ=gaussj,YERR=yerr,$'
    print, '                               THRESHSVD=threshsvd,$'
    print, '                               IGOODBAD=igoodbad,SILENT=silent,$'
    print, '                               OGOODBAD=ogoodbad,COVAR=covar,$'
    print, '                               YFIT=yfit,RMS=rms,CHISQ=chisq,$'
    print, '                               RCHISQ=rchisq,Q=q,VAR=var,$'
    print, '                               JUSTFIT=justfit,CANCEL=cancel)'
    cancel = 1
    return, -1

endif

cancel = cpar('robustpoly1d',x,1,'X',[2,3,4,5],1)
if cancel then return,-1
cancel = cpar('robustpoly1d',y,2,'Y',[2,3,4,5],1)
if cancel then return,-1
cancel = cpar('robustpoly1d',order,3,'Order',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('robustpoly1d',thresh,4,'Thresh',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('robustpoly1d',eps,5,'Eps',[2,3,4,5],0)
if cancel then return,-1


if n_elements(YERR) eq 0 then yerr = replicate(1D,n_elements(x))

if n_elements(igoodbad) ne 0 then begin

    nan      = where(finite(y) eq 0 or finite(x) eq 0 or finite(yerr) eq 0, $
                     count_nan)    
    if count_nan ne 0 then igoodbad[nan] = 2
    z        = where(igoodbad eq 1,count_initgood)
    xx       = x[z]
    yy       = y[z]
    yyerr    = yerr[z] 
    ogoodbad = igoodbad

endif else begin

    ogoodbad = replicate(1,n_elements(y))
    nan      = where(finite(y) eq 0 or finite(x) eq 0 or finite(yerr) eq 0,$
                     count_nan)
    if count_nan ne 0 then ogoodbad[nan] = 2.
    z        = where(ogoodbad eq 1, count_initgood)
    if count_initgood eq 0 then begin

        cancel = 1
        return,[0.0,0.0]

    endif

    if count_initgood lt order+1 then begin

        cancel = 1
        return,[0.0,0.0]

    endif

    xx       = x[z]
    yy       = y[z]
    yyerr    = yerr[z]
    
endelse

;  Do a first pass fit to the data.

coeff = poly_fit1d(xx,yy,order,SVD=svd,GAUSSJ=gaussj,YERR=yyerr,$
                   THRESHSVD=threshsvd,/SILENT,COVAR=covar,YFIT=yfit,$
                   RMS=rms,CHISQ=chisq,RCHISQ=rchisq,Q=q,VAR=var,$
                   JUSTFIT=justfit)

test = yy - yfit
moments, test,mean,dummy,sig
good = where(abs( (test-mean)/sig ) le thresh,count_good)
if count_good eq 0 then begin

    cancel = 1
    return, -1

endif

;  If it doesn't find any outliers then go back, if not continue.

if count_good eq count_initgood then goto, cont1

repeat begin
    
    ittr      = ittr + 1
    sig_old  = sig
    coeff = poly_fit1d(xx[good],yy[good],order,SVD=svd,GAUSSJ=gaussj,$
                       YERR=yyerr[good],THRESHSVD=threshsvd,$
                       /SILENT,COVAR=covar,YFIT=yfit,RMS=rms,$
                       CHISQ=chisq,RCHISQ=rchisq,Q=q,VAR=var,JUSTFIT=justfit)
    test = yy[good] - yfit
    moments, test,mean,dummy,sig
    test = yy - poly(xx,coeff)
    if ((sig_old-sig)/sig_old) lt eps then goto, cont1
    good = where(abs( (test-mean)/sig ) le thresh,count_good)

    if count_good eq 0 then begin
        
        cancel = 1
        return, -1
        
    endif

endrep until ittr eq 10

cont1:

;  Reconstruct the good/bad array if necessary.

good_bad       = replicate(0.,count_initgood)
good_bad[good] = 1.
ogoodbad[z]    = good_bad

;  Report results if the SILENT keyword isn't set.

ndof = (count_good-(order+1))
if not keyword_set(SILENT) then begin

    print, ' '
    z = where(ogoodbad eq 0, bad)
    if keyword_set(gaussj) then print, 'Robust GAUSSJ result:'
    if keyword_set(SVD) then print, 'Robust SVD result:'
    print, ' '
    print, '   Number of original points  = ', strcompress(count_initgood, /re)
    print, '             Sigma threshold  = ', strcompress(thresh, /re)
    print, '             Number of loops  = ', strcompress(ittr, /re)
    print, '    Number of outlier points  = ', strcompress(bad, /re)
    print, '     Number of points in fit  = ', strcompress(count_good, /re)
    print, '              Number of NaNs  = ', strcompress(count_nan, /re)

    if not keyword_set(JUSTFIT) then begin

        print, 'Number of degrees of freedom  = ', strcompress(ndof, /re)
        print, '                 Chi-Squared  = ', strcompress(chisq, /re)
        print, '         Reduced Chi-Squared  = ', strcompress(rchisq, /re)
        print, '         Goodness-of-fit (Q)  = ', strcompress(q, /re)
        print, '        RMS deviation of fit  = ', strcompress(rms, /re)

    endif

    print, ' '
    print, 'Coefficients of the Polynomial:'
    print, ' '
    for i = 0, order do begin

        string = 'Coeff #'+string(i,format='(i2.2)')+'   =  '+ $
          strcompress(coeff[i],/re)+'  +/-  '+strcompress(sqrt(var[i]), /re)
        print, string

    endfor
    print, ' '
    print, 'Covariance Matrix:'
    print, covar
    print, ' '


endif

return, coeff

end

