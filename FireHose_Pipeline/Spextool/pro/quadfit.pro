;+
; NAME:
;     quadfit
;
; PURPOSE:
;     Fits a quadratic surface to data.
;
; CATEGORY:
;     Fitting Data
;
; CALLING SEQUENCE:
;     result = quadfit(img,ERR=err,JUSTFIT=justfit,SILENT=silent,$
;                      DOALPHA=doalpha,COVAR=covar,FIT=fit,RMS=rms,$
;                      CHISQ=chisq,RCHISQ=rchisq,Q=q,SIGMA=sigma,$
;                      CANCEL=cancel)
;
; INPUTS:
;     img - The 2D data to be fit
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     JUSTFIT   - Set to have poly_fit1d only perform the fit and NOT 
;                 chi^2 etc
;     SILENT    - Set to silence the report of the results at the end
;     DOALPHA   - If the surface to be fit is large, then set this to
;                 NOT construct the A matrix and just construct the
;                 alpha matrix.  This takes longer though.
;     COVAR     - The covariance matrix.
;     FIT       - The fitted surface
;     RMS       - The rms deviation of the fit. 
;     CHISQ     - The Chi-Squared of the fit.
;     RCHISQ    - The reduced Chi-Squared of the fit.
;     Q         - The goodness-of-fit parameter.
;     SIGMA     - A vector which contains the 1-sigma error estimates
;     CANCEL    - Set on return if there is a problem
; 
; OUTPUTS:
;     Returns the coefficients of the fit
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
;     Fits a surface to the function:
;     z = c[0] + c[1]*x + c[2]*y + c[3]x^2 + c[4]y^2 + c[5]*x*y
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2003-01-23 - Written by M. Cushing, Institute for Astronomy
;-
function quadfit,img,ERR=err,JUSTFIT=justfit,SILENT=silent,$
                 DOALPHA=doalpha,COVAR=covar,FIT=fit,RMS=rms,$
                 CHISQ=chisq,RCHISQ=rchisq,Q=q,SIGMA=sigma,$
                 CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 1 then begin
    
    print, 'Syntax -  result = quadfit(img,ERR=err,JUSTFIT=justfit,$'
    print, '                           SILENT=silent,DOALPHA=doalpha,$'
    print, '                           COVAR=covar,FIT=fit,RMS=rms,$'
    print, '                           CHISQ=chisq,RCHISQ=rchisq,Q=q,$'
    print, '                           SIGMA=sigma,CANCEL=cancel)'
    cancel = 1
    return, -1

endif
cancel = cpar('quadfit',img,1,'Img',[2,3,4,5],2)
if cancel then return,-1

;  Get size of the input array.

imginfo,img,xsize,ysize,ndatimg

;  Construct and x and y 2-D array.

x = findgen(xsize) # replicate(1., ysize)
y = replicate(1., xsize) # findgen(ysize)

;  Reformat the x, y, and z arrays to 1-D.

x = reform(temporary(x),ndatimg)
y = reform(temporary(y),ndatimg)
z = reform(img,ndatimg)

zerr = replicate(1D,n_elements(x))

good  = where(finite(x) eq 1 and finite(y) eq 1 and finite(z) eq 1,count_good)
xx    = x[good]
yy    = y[good]
zz    = z[good]
zzerr = zerr[good]

;  Get sizes.

ndat    = n_elements(xx)
ncoeffs = 6
ndof    = (ndat-ncoeffs)

expdeg = [[0,0],[1,0],[0,1],[2,0],[0,2],[1,1]]

b = zz/zzerr

if not keyword_set(DOALPHA) then begin

;  Construct the design matrix A, then alpha and beta.

    AT = dblarr(ndat,ncoeffs)
    for i = 0, ncoeffs-1 do begin
        
        AT[*,i] = xx^expdeg[0,i]*yy^expdeg[1,i]/zerr
        
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
            
            at = ( xx^expdeg[0,i] * yy^expdeg[1,i] ) / zzerr    
            a  = ( xx^expdeg[0,j] * yy^expdeg[1,j] ) / zzerr   
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

;  Rebuild image

    fit = fltarr(ndatimg)
    for i = 0, 5 do fit = fit + (xx^expdeg[0,i]*yy^expdeg[1,i])*coeffs[i]

    ffit = fltarr(count_good)
    for i = 0, 5 do ffit = ffit + (x^expdeg[0,i]*y^expdeg[1,i])*coeffs[i]

    rms    = stddev(zz-ffit)
    chisq  = total( ( (zz - ffit)/ zzerr )^2 )
    rchisq = chisq/ ndof
    sigma  = sqrt( diagovec(covar) )
    q      = chisqr_pdf( ndof,chisq)
    
;  Now print out the results if the silent keyword isn't set.

    if not keyword_set(SILENT) then begin
        
        nans  = (n_elements(x)-count_good)
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




;  Rebuild image

;newz = fltarr(ndatimg)
;for i = 0, 5 do begin

;    newz = newz + (x^expdeg[0,i]*y^expdeg[1,i])*coeffs[i]

;endfor
;newz = reform(temporary(newz),xsize,ysize)

return,coeffs

end
