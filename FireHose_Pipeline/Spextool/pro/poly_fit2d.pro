;+
; NAME:
;     poly_fit2d
;
; PURPOSE:
;     Fits polynomials of given orders to a set of 2-D data.
;
; CATEGORY:
;     Fitting Data
;
; CALLING SEQUENCE:
;     result = poly_fit2d(x,y,z,xorder,yorder,ZERR=zerr,THRESHSVD=threshsvd,$
;                         JUSTFIT=justfit,SILENT=silent,DOALPHA=doalpha,$
;                         COVAR=covar,ZFIT=zfit,RMS=rms,CHISQ=chisq,$
;                         RCHISQ=rchisq,Q=q,SIGMA=sigma,CANCEL=cancel)
;
; INPUTS:
;     x      - A vector of the first independent values.
;     y      - A vector of the second independent values.
;     z      - A vector of dependent variables.
;     xorder - The order of the polynomial to be fit in the x direction.
;     yorder - The order of the polynomial to be fit in the y direction.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     ZERR    - A vector of errors for the dependent values. 
;     JUSTFIT - Set to supress the calculation of zfit, rms,
;               chisq,  rchisq, q, and sigma.  If z is large, then
;               this step can be time consuming.
;     SILENT  - Set to supress the report of the results at the end.
;     DOALPHA - If the surface to be fit is large, then set this to
;               NOT construct the A matrix and just construct the
;               alpha matrix.  This takes longer though.
;     COVAR   - The covariance matrix.
;     ZFIT    - A vector of calculated Z values.
;     RMS     - The rms deviation of the fit. 
;     CHISQ   - The Chi-Squared of the fit.
;     RCHISQ  - The reduced Chi-Squared of the fit.
;     Q       - The goodness-of-fit parameter.
;     SIGMA   - A vector which contains the 1-sigma error estimates
;               of the coefficients.
;     CANCEL  - Set on return if there is a problem
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
;     This program is based on solving the equation A ## coeff = b.
;     Except now, z = f(x,y) = (a_0 + a_1*x) * (b_0 + b_1*x + b_2*x^2)
;     = b_0*a_0 + b_0*a_1*x + b_1*a_0*x + b_1*a_1*x^2 ...
;     The design matrix A is constructed.  The alpha (A^T ## A) and
;     beta (A^T ## b) arrays of the normal equations are constructed
;     and passes to GAUSSJ which returns the coeff array.    
;
;     If the surface to be fit is large and/or the number of
;     coefficients is large, set the DOALPHA keyword.  Since the design 
;     matrix A can be large (ncoeff,ndat) it may take too much memory
;     and or too much time.  If DOALPHA is set, the alpha and beta
;     arrays are constructed directly and passed to GAUSSJ.  There is
;     a trade off since constructing the alpha and beta arrays
;     directly takes longer for smaller surfaces.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2000-07-20 - Written by M. Cushing, Institute for Astronomy, UH
;     2000-12-08 - Changed WEIGHTS keyword to ZERR. 
;     2001-01-16 - Added DOALPHA keyword.
;-
function poly_fit2d,x,y,z,xorder,yorder,ZERR=zerr,THRESHSVD=threshsvd,$
                    JUSTFIT=justfit,SILENT=silent,DOALPHA=doalpha,$
                    COVAR=covar,ZFIT=zfit,RMS=rms,CHISQ=chisq,$
                    RCHISQ=rchisq,Q=q,SIGMA=sigma,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 5 then begin
    
    print, 'Syntax -  coeff = poly_fit2d(x,y,z,xorder,yorder,ZERR=zerr,$'
    print, '                             THRESHSVD=threshsvd,JUSTFIT=justfit,$'
    print, '                             SILENT=silent,DOALPHA=doalpha,$'
    print, '                             COVAR=covar,ZFIT=zfit,RMS=rms,$'
    print, '                             CHISQ=chisq,RCHISQ=rchisq,Q=q,$'
    print, '                             SIGMA=sigma,CANCEL=cancel)'
    cancel = 1
    return, -1

endif

cancel = cpar('poly_fit2d',x,1,'X',[2,3,4,5],1)
if cancel then return,-1
cancel = cpar('poly_fit2d',y,2,'Y',[2,3,4,5],1)
if cancel then return,-1
cancel = cpar('poly_fit2d',z,3,'Z',[2,3,4,5],1)
if cancel then return,-1
cancel = cpar('poly_fit2d',xorder,4,'Xorder',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('poly_fit2d',yorder,5,'Yorder',[2,3,4,5],0)
if cancel then return,-1

;  Check for needed (but ignored) user inputs.

if n_elements(ZERR) eq 0 then zerr = replicate(1D,n_elements(x))

;  Check for nans.

good  = where(finite(x) eq 1 and finite(y) eq 1 and finite(z) eq 1, nonans)
xx    = double( x[good] )
yy    = double( y[good] )
zz    = double( z[good] )
zzerr = double( zerr[good] )

;  Get sizes.

ndat    = n_elements(xx)
ncoeffs = (xorder+1)*(yorder+1)
ndof    = (ndat-ncoeffs)

;  Determine the exponents of the basis functions.

xexp  = findgen(xorder+1)
yexp  = findgen(yorder+1)
order = intarr(2,ncoeffs)
idx = 0
for i = 0, yorder do begin

    for j = 0, xorder do begin

        order[0,idx] = xexp[j]
        order[1,idx] = yexp[i]
        idx = idx+1

    endfor

endfor

b = zz/zzerr

if not keyword_set(DOALPHA) then begin

;  Construct the design matrix A, then alpha and beta.

    AT = dblarr(ndat,ncoeffs)
    for i = 0, ncoeffs-1 do begin
        
        AT[*,i] = xx^order[0,i]*yy^order[1,i]/zerr
        
    endfor
    
    alpha = AT##transpose(AT)
    beta  = reform(AT##b)

endif
if keyword_set(DOALPHA) then begin

;  Construct the alpha and beta arrays.

    alpha = dblarr(ncoeffs,ncoeffs)
    beta  = dblarr(ncoeffs)

;  Build only the upper triangle of the alpha array since it is symmetric.

    for i = 0, ncoeffs-1 do begin
        
        for j = i,ncoeffs-1 do begin
            
            at = ( xx^order[0,i] * yy^order[1,i] ) / zzerr    
            a  = ( xx^order[0,j] * yy^order[1,j] ) / zzerr   
            alpha[j,i] = total(at * a)
            beta[i]    = total(at * b)
            
        endfor
        
    endfor

    alpha = alpha + transpose(alpha)
    diag = findgen(ncoeffs)* (ncoeffs^2-1)/(ncoeffs-1)
    alpha[diag] = alpha[diag]/2.
    
endif

;  Solve the normal equations.

gaussj,alpha,beta,inverted=covar,solution=coeffs

;  Now given the coeffs and covar matrix, we can determine all
;  the other things needed for a complete description of the fit.

if not keyword_set(JUSTFIT) then begin

    zfit   = poly2d(xx,yy,xorder,yorder,coeffs)
    rms    = stddev(zz-zfit)
    chisq  = total( ( (zz - zfit)/ zzerr )^2 )
    rchisq = chisq/ ndof
    sigma  = sqrt( diagovec(covar) )
    q      = chisqr_pdf( ndof,chisq)
    
;  Now print out the results if the silent keyword isn't set.

    if not keyword_set(SILENT) then begin
        
        nans  = (n_elements(x)-nonans)
        print, ' '
        print, 'GAUSSJ result:'
        print, ' '
        print, '            Number of points  =  ', strcompress(ndat, /re)
        print, 'Number of degrees of freedom  =  ', strcompress(ndof, /re)
        print, '                 Chi-Squared  =  ', strcompress(chisq, /re)
        print, '         Reduced Chi-Squared  =  ', strcompress(rchisq, /re)
        print, '         Goodness-of-fit (Q)  =  ', strcompress(q, /re)
        print, '              Number of NaNs  =  ', strcompress(nans, /re)
        print, '        RMS deviation of fit  =  ', strcompress(rms, /re)
        print, ' '
        print, 'Coefficients of the Polynomial:'
        print, ' '
        for i = 0, ncoeffs-1 do begin
            
            string = 'Coeff #'+string(i,format='(i2.2)')+'   =  '+ $
              strcompress(coeffs[i],/re)+'  +/-  '+strcompress(sigma[i], /re)
            print, string
            
        endfor
        
    endif
    
endif
return, coeffs

end

