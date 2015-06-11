;+
; NAME:
;     robustpoly2d
;
; PURPOSE:
;     Robustly fits polynomials of given orders to a set of 2-D data.
;
; CATEGORY:
;     Fitting Data
;
; CALLING SEQUENCE:
;     result = robustpoly2d(x,y,z,xorder,yorder,thresh,eps,ZERR=zerr,$
;                           IGOODBAD=igoodbad,SILENT=silent,$
;                           OGOODBAD=ogoodbad,COVAR=covar,YFIT=yfit,RMS=rms,$
;                           CHISQ=chisq,RCHISQ=rchisq,Q=q,NDOF=ndof,$
;                           SIGMA=sigma,CANCEL=cancel)
;    
; INPUTS:
;     x      - A vector of the first independent values.
;     y      - A vector of the second independent values.
;     z      - A vector of dependent variables.
;     xorder - The order of the polynomial to be fit in the x direction.
;     yorder - The order of the polynomial to be fit in the y direction.
;     thresh - Sigma threshold to identify outliers.
;     eps    - Limit of the fractional change in the standard
;              deviation of the fit.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL   - Set on return if there is a problem
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
;     Same as poly_fit2d except it itteratively removes outliers.
;     Very basic.
;
; EXAMPLE:
;
;     
; MODIFICATION HISTORY:
;     2000-03-07 - Written, by M. Cushing, Institute for Astronomy, UH
;     2000-12-08 - Changed WEIGHTS keyword to ZERR.
;-
function robustpoly2d,x,y,z,xorder,yorder,thresh,eps,ZERR=zerr,$
                      IGOODBAD=igoodbad,SILENT=silent,$
                      OGOODBAD=ogoodbad,COVAR=covar,YFIT=yfit,RMS=rms,$
                      CHISQ=chisq,RCHISQ=rchisq,Q=q,NDOF=ndof,SIGMA=sigma,$
                      CANCEL=cancel
cancel = 0
ittr   = 1

if n_params() lt 7 then begin

    print, 'Syntax - result = robustpoly2d(x,y,z,xorder,yorder,thresh,eps,$'
    print, '                               ZERR=zerr,IGOODBAD=igoodbad,$'
    print, '                               SILENT=silent,OGOODBAD=ogoodbad,$'
    print, '                               COVAR=covar,YFIT=yfit,RMS=rms,$'
    print, '                               CHISQ=chisq,RCHISQ=rchisq,Q=q,$'
    print, '                               NDOF=ndof,SIGMA=sigma,CANCEL=cancel'
    cancel = 1
    return,-1

endif

cancel = cpar('robustpoly2d',x,1,'X',[2,3,4,5],1)
if cancel then return,-1
cancel = cpar('robustpoly2d',y,2,'Y',[2,3,4,5],1)
if cancel then return,-1
cancel = cpar('robustpoly2d',z,3,'Z',[2,3,4,5],1)
if cancel then return,-1
cancel = cpar('robustpoly2d',xorder,4,'Xorder',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('robustpoly2d',yorder,5,'Yorder',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('robustpoly2d',thresh,6,'Thresh',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('robustpoly2d',eps,7,'Eps',[2,3,4,5],0)
if cancel then return,-1


if n_elements(ZERR) eq 0 then zerr= replicate(1D,n_elements(x))

;  In good_bad array, 0=bad, 1=good, 2=nan

if n_elements(igoodbad) ne 0 then begin

    nan      = where(finite(y) eq 0 or finite(x) eq 0 or finite(z) eq 0, $
                     count_nan)
    if count_nan ne 0 then igoodbad[nan] = 2
    a        = where(igoodbad eq 1, count_initgood)
    xx       = x[a]
    yy       = y[a]
    zz       = z[a]
    zzerr    = zerr[a]
    ogoodbad = igoodbad

endif else begin

    ogoodbad = replicate(1,n_elements(x))
    nan      = where(finite(x) eq 0 or finite(z) eq 0 or finite(y) eq 0,$
                     count_nan)
    if count_nan ne 0 then ogoodbad[nan] = 2.
    a = where(ogoodbad eq 1,count_initgood)
    xx       = x[a]
    yy       = y[a]
    zz       = z[a]
    zzerr    = zerr[a]

endelse

;  Do a first pass fit to the data.

coeff = poly_fit2d(xx,yy,zz,xorder,yorder,ZERR=zzerr,/SILENT,COVAR=covar,$
                   ZFIT=zfit,RMS=rms,CHISQ=chisq,RCHISQ=rchisq,Q=q,$
                   SIGMA=sigma)

test = zz - zfit
moments, test,mean,dummy,sig
good = where(abs( (test-mean)/sig ) le thresh,count_good)

;  If it doesn't find any outliers then go back, if not continue.

if count_good eq count_initgood then goto, cont1

repeat begin
    
    ittr    = ittr + 1
    sig_old = sig

    coeff = poly_fit2d(xx[good],yy[good],zz[good],xorder,yorder,$
                       ZERR=zzerr[good],SILENT=1,COVAR=covar,ZFIT=zfit,$
                       RMS=rms,CHISQ=chisq,RCHISQ=rchisq,Q=q,SIGMA=sigma)

    test = zz[good] - poly2d(xx[good],yy[good],xorder,yorder,coeff)
    moments, test,mean,dummy,sig
    test = zz-poly2d(xx,yy,xorder,yorder,coeff)
    if ((sig_old-sig)/sig_old) lt eps then goto, cont1
    good = where(abs( (test-mean)/sig ) le thresh,count_good)

endrep until ittr eq 10

cont1:

;  Reconstruct the good/bad array if necessary.

good_bad       = replicate(0.,count_initgood)
good_bad[good] = 1.
ogoodbad[a]    = good_bad

;  Report results if the SILENT keyword isn't set.

ncoeffs = (xorder+1)*(yorder+1)
ndof    = (count_good-ncoeffs)
if not keyword_set(SILENT) then begin

    print, ' '
    junk = where(ogoodbad eq 0, bad)
    print, 'Robust result:'
    print, ' '
    print, '   Number of original points  =  ', strcompress(count_initgood,/re)
    print, '             Sigma threshold  =  ', strcompress(thresh, /re)
    print, '             Number of loops  =  ', strcompress(ittr, /re)
    print, '    Number of outlier points  =  ', strcompress(bad, /re)
    print, '     Number of points in fit  =  ', strcompress(count_good, /re)
    print, 'Number of degrees of freedom  =  ', strcompress(ndof, /re)
    print, '                 Chi-Squared  =  ', strcompress(chisq, /re)
    print, '         Reduced Chi-Squared  =  ', strcompress(rchisq, /re)
    print, '         Goodness-of-fit (Q)  =  ', strcompress(q, /re)
    print, '              Number of NaNs  =  ', strcompress(count_nan, /re)
    print, '        RMS deviation of fit  =  ', strcompress(rms, /re)
    print, ' '
    print, 'Coefficients of the Polynomial:'
    print, ' '
    for i = 0, ncoeffs-1 do begin

        string = 'Coeff #'+string(i,format='(i2.2)')+'   =  '+ $
          strcompress(coeff[i],/re)+'  +/-  '+strcompress(sigma[i], /re)
        print, string

    endfor

endif

return, coeff

end

