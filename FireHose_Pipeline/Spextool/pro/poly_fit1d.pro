;+
; NAME:
;     poly_fit1d
;
; PURPOSE:
;     Fits a polynomial of a given order to a set of 1-D data.
;      
; CATEGORY:
;     Fitting Data
;
; CALLING SEQUENCE:
;     result = poly_fit1d(x,y,order,SVD=svd,GAUSSJ=gaussj,YERR=yerr,$
;                         GOODBAD=goodbad,THRESHSVD=threshsvd,SILENT=silent,$
;                         COVAR=covar,YFIT=yfit,RMS=rms,CHISQ=chisq,$
;                         RCHISQ=rchisq,Q=q,VAR=var,JUSTFIT=justfit,$
;                         CANCEL=cancel)
;
; INPUTS:
;     x     - An array of independent values.
;     y     - An array of dependent values.
;     order - The order of the polynomial to be fit to the data.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     SVD       - Set to solve the linear equations using Single Value
;                 Decomposition.
;     GAUSSJ    - To solve the linear equations using a Gauss-Jordan 
;                 elimination scheme. This is the default.
;     YERR      - A vector of errors for the dependent values
;     GOODBAD   - Input goodbad array. 0=bad, 1=good.
;     THRESHSVD - If SVD is set, the threshold to identify singular
;                 values.  The default is 1e-20.
;     SILENT    - Set to silence the report of the results at the end.
;     COVAR     - The covariance matrix.
;     YFIT      - A vector of calculated Y values.
;     RMS       - The rms deviation of the fit. 
;     CHISQ     - The Chi-Squared of the fit.
;     RCHISQ    - The reduced Chi-Squared of the fit.
;     Q         - The goodness-of-fit parameter.
;     VAR       - A vector which contains the variance estimates
;                 of the coefficients.
;     JUSTFIT   - Set to have poly_fit1d only perform the fit and NOT 
;                 chi^2 etc.
;     CANCEL    - Will be set on return if the something bad happens.
;
; OUTPUTS:
;     Returns the coefficients of the polynomial.
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
;     This routine is meant to replace,poly_fit,polyfitw,and
;     svdfit (I have found polyfitw does not return the correct
;     coefficients if a weighted fit is attempted.)  This program is
;     based on solving the equation A ## coeff = b.  If GAUSSJ is
;     set, the alpha (A^T ## A) and beta (A^T ## b) arrays of
;     the normal equations are constructed and passes to GAUSSJ
;     which returns the coeff array.  If SVD is set, then the design
;     matrix  A and b array are constructed.  The array coeff is
;     determined through single value decomposition. 
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;    2000-07-20 - Written by M. Cushing, Institute for Astronomy, UH
;    2000-12-08 - Changed WEIGHTS keyword to YERR.
;    2002-11-11 - Now checks ndof to make sure it is not zero.
;               - Added JUSTFIT keyword.   
;-
function poly_fit1d,x,y,order,SVD=svd,GAUSSJ=gaussj,YERR=yerr,GOODBAD=goodbad,$
                    THRESHSVD=threshsvd,SILENT=silent,COVAR=covar,YFIT=yfit,$
                    RMS=rms,CHISQ=chisq,RCHISQ=rchisq,Q=q,VAR=var,$
                    JUSTFIT=justfit,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 3 then begin
    
    print, 'Syntax - coeff =  poly_fit1d(x,y,order,SVD=svd,GAUSSJ=gaussj,$'
    print, '                             YERR=yerr,THRESHSVD=threshsvd,$'
    print, '                             SILENT=silent,COVAR=covar,YFIT=yfit,$'
    print, '                             RMS=rms,CHISQ=chisq,RCHISQ=rchisq,$'
    print, '                             Q=q,VAR=var,JUSTFIT=justfit,$'
    print, '                             CANCEL=cancel)
    cancel = 1
    return, -1

endif
cancel = cpar('poly_fit1d',x,1,'X',[2,3,4,5],1)
if cancel then return,-1
cancel = cpar('poly_fit2d',y,2,'Y',[2,3,4,5],1)
if cancel then return,-1
cancel = cpar('poly_fit2d',order,3,'Order',[2,3,4,5],0)
if cancel then return,-1

;  Check for needed (but ignored) user keywords.

if n_elements(YERR) eq 0 then yerr = replicate(1D,n_elements(x))
if not keyword_set(GAUSSJ) and not keyword_set(SVD) then gaussj = 1

;  Get rid of NaNs

z     = where(finite(x) eq 1 and finite(y) eq 1 and finite(yerr) eq 1,nonans)
xx    = double(x[z])
yy    = double(y[z])
yyerr = double(yerr[z])

if n_elements(GOODBAD) ne 0 then begin

    z = where(goodbad eq 1,count)
    if count ne 0 then begin

        xx = xx[z]
        yy = yy[z]

    endif

endif

ndat    = n_elements(xx)      ;  Number of independent variables
ncoeffs = (order+1)           ;  Number of coefficients
ndof    = (ndat-ncoeffs)    ;  Number of degrees of freedom. 

if order eq 0 then begin

    meancomb,yy,coeffs,var,DATAVAR=yvar
    coeffs = [coeffs,0.]
    if n_elements(var) ne 0 then var = [var,0]
    goto, getout


endif

if keyword_set(GAUSSJ) then begin

;  Construct the alpha and beta matrix of the normal equations.  
;  Build only the upper triangle of the alpha array since it is symmetric.  

    exp   = findgen(order+1)
    alpha = dblarr(ncoeffs,ncoeffs)
    beta  = dblarr(ncoeffs)
    b     = yy/yyerr
    for i = 0, ncoeffs-1 do begin
        
        for j = i,ncoeffs-1 do begin
            
            at = (xx^exp[i]) / yyerr 
            a  = (xx^exp[j]) / yyerr 
            alpha[j,i] = total(at * a)
            beta[i]    = total(at * b)
            
        endfor
        
    endfor
    alpha    = alpha + transpose(alpha)
    z        = findgen(ncoeffs)* (ncoeffs^2-1)/(ncoeffs-1)
    alpha[z] = alpha[z]/2.
    gaussj,alpha,beta,inverted=covar,solution=coeffs

endif

if keyword_set(SVD) then begin

;  Construct the design matrix A and the RHS vector b.
    
    b   = double(yy/yyerr)
    exp = findgen(order+1)
    A   = dblarr(ncoeffs,ndat)
    for i = 0, ncoeffs-1 do A[i,*] = xx^exp[i] / yyerr
    
;  Check for needed (but ignored) user inputs.

    if n_elements(THRESHSVD) eq 0 then threshsvd = 1e-20

;  Now follow N.R. for a SVD decomposition.
    
    svdc,A,w,u,v,/double

;Find all the non-singular values, in svdfit of N.R.

    good    = where(w gt (max(w) * threshsvd), count)
    sinvals = ncoeffs - count               
    
    if sinvals ne 0 then begin

        small    = where(w le max(w)*threshsvd)
        w[small] = 0.
        print, 'POLY_FIT1D: Warning: '+strcompress(sinvals,/remove) + $
          ' singular values found: '+strcompress(string(small))
        if count eq 0 then begin

            cancel=1
            return,-1

        endif

    endif

;  Solve the equations

    coeffs = svsol(u,w,v,b,/double)

;  Reconstruct the covariance matrix using the w,v.  See svdvar in
;  N.R.

    covar    = dblarr(order+1,order+1)
    for i = 0,order do begin

        for j = 0,i do begin

            s = 0.
            for k=0, order do s = s + (v[k,i] * v[k,j] / w[k]^2)
            covar[i,j] = s
            covar[j,i] = s
        
        endfor

    endfor

endif

;  Now given the coeffs and covar matrix, we can find determine all
;  the other things needed for the a complete description of the fit.

var = diagovec(covar)

getout:


if not keyword_set(JUSTFIT) then begin

    yfit   = poly(xx,coeffs)
    rms    = stddev(yfit-yy)
    chisq  = total( ( (yy - yfit)/yyerr )^2 )
   
    if ndof ne 0 and chisq ne !values.f_infinity and chisq ne 0 then begin

        rchisq = chisq/(ndat-ncoeffs)
        q      = chisqr_pdf( (ndat-ncoeffs),chisq)

    endif else begin
        
        rchisq = !values.f_nan
        q      = !values.f_nan
        
    endelse

endif
;  Now print out the results if the silent keyword isn't set.

if not keyword_set(SILENT) then begin

    if not keyword_set(JUSTFIT) then begin

        nans  = (n_elements(x)-nonans)
        print, ' '
        if keyword_set(GAUSSJ) then print, 'GAUSSJ result:'
        if keyword_set(SVD) then print, 'SVD result:'
        print, ' '
        print, '            Number of points  =  ', strcompress(ndat, /re)
        print, 'Number of degrees of freedom  =  ', strcompress(ndof, /re)
        print, '                 Chi-Squared  =  ', strcompress(chisq, /re)
        print, '         Reduced Chi-Squared  =  ', strcompress(rchisq, /re)
        print, '         Goodness-of-fit (Q)  =  ', strcompress(q, /re)
        print, '              Number of NaNs  =  ', strcompress(nans, /re)
        print, '        RMS deviation of fit  =  ', strcompress(rms, /re)
        print, ' '

    endif
    print, 'Coefficients of the Polynomial:'
    print, ' '
    
    for i = 0, ncoeffs-1 do begin
        
        string = 'Coeff #'+string(i,format='(i2.2)')+'   =  '+ $
          strcompress(coeffs[i],/re)+'  +/-  '+strcompress(sqrt(var[i]), /re)
        print, string
        
    endfor
    print, ' '
    print, 'Covariance Matrix:'
    print, covar
    print, ' '

endif

return, coeffs

end

