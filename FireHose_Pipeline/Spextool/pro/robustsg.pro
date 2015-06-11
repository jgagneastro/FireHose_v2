; NAME:
;     robustsg
;
; PURPOSE:
;     To robustly smooth data set using Savitzky-Golay.
;
; CATEGORY:
;     Data Modeling
;
; CALLING SEQUENCE:
;     result = robustsg(x,y,width,thresh,eps,DEGREE=degree,IGOODBAD=igoodbad,$
;                       OGOODBAD=ogoodbad,PLT=plt,RMS=rms,CANCEL=cancel)
;
; INPUTS:
;     x      - An array of independent values
;     y      - An array of dependent values to be smoothed
;     width  - The smoothing window in units of x
;     thresh - Sigma threshold to identify outliers
;     eps    - The limit of the fractional change in the rms of the fit
; 
; OUTUTS:
;     Returns a smoothed version of y.
;
; KEYWORD PARAMETERS:    
;     DEGREE    - The Savitzky-Golay degree (See savitzky-golay)
;     IGOODBAD  - Input goodbad array. 0=bad, 1=good, 2=nan
;     OGOODBAD  - Output goodbad array. 0=bad, 1=good, 2=nan 
;     PLT       - Set to plot to the screen as the process occurs.
;     RMS       - The rms deviation of the smoothed data from the raw
;                 data.
;     CANCEL    - Set on return if there is a problem
;
;  PROCEDURE CALLED:
;     Requires the Astronomy User's Library
;     savitzky_golay
;
;  PROCEDURE:
;     Performs a robust Savitzky_Golay smoothing.  Will continue to 
;     identify outliers until either 1) ten iterrations occur or 2)
;     the fractional change in the RMS of the residuals is less than EPS.
;
;  REVISION HISTORY:
;     2000-08-01 - Written by M. Cushing, Institute for Astronomy, UH
;     2001-03-05 - Fixed bugged which caused program to break if no 
;                  bad points were identified.
;     2001-04-20 - Changed width to be in x units and then scaled into 
;                  pixel units. 
;                - Added PLT keyword.
;
function robustsg,x,y,width,thresh,eps,DEGREE=degree,IGOODBAD=igoodbad,$
                  OGOODBAD=ogoodbad,PLT=plt,RMS=rms,CANCEL=cancel

;  Check parameters

if n_params() lt 5 then begin
    
    print, 'Syntax -  result = robustsg(x,y,width,thresh,eps,DEGREE=degree,$'
    print, '                            IGOODBAD=igoodbad,OGOODBAD=ogoodbad,$'
    print, '                            PLT=plt,RMS=rms,CANCEL=cancel'
    return, -1

endif
zparcheck, 'robustsg',x, 1, [1,2,3,4,5], 1, 'X - Independent values'
zparcheck, 'robustsg',y, 2, [1,2,3,4,5], 1, 'Y - Dependent values'
zparcheck, 'robustsg',width, 3, [1,2,3,4,5], 0, 'Width'
zparcheck, 'robustsg',thresh, 4, [1,2,3,4,5], 0, 'Thresh'
zparcheck, 'robustsg',eps, 5, [1,2,3,4,5], 0, 'Eps'

ittr = 1

;  Determine the width of the SG window in pixels based on the width
;  in x units.

del = x-x[1:*]
moments,del,ave
ww = width/abs(ave)

if n_elements(igoodbad) ne 0 then begin

    nan      = where(finite(y) eq 0 or finite(x) eq 0, count_nan)    
    if count_nan ne 0 then igoodbad[nan] = 2
    ogoodbad = igoodbad

    z_good   = where(ogoodbad eq 1,count_initgood)
    yy       = y[z_good]
    xx       = x[z_good]


endif else begin

    ogoodbad = replicate(1,n_elements(y))
    nan      = where(finite(y) eq 0 or finite(x) eq 0,count_nan)
    if count_nan ne 0 then ogoodbad[nan] = 2.

    z_good   = where(ogoodbad eq 1,count_initgood)
    yy       = y[z_good]
    xx       = x[z_good]
    
endelse

;  Do a first pass fit to the data.

if keyword_set(PLT) then begin

    window, /free
    wid = !d.window
    plot,xx,yy,/xsty,psym=3,yrange=[-0.1,0.1]

endif

goodyy = savitzky_golay(yy,ww,DEGREE=degree)
test = yy - goodyy
moments, test,mean,dummy,sig
good = where(abs( (test-mean)/sig ) le thresh,count_good)
bad  = where(abs( (test-mean)/sig ) gt thresh,count_bad)

if keyword_set(PLT) then begin

    oplot,xx,goodyy,color=2
    if count_bad ne 0 then oplot,xx[bad],yy[bad],psym=4,color=3
    re = ' '
    read, re

endif

;  If it doesn't find any outliers then go back, if not continue.

if count_good eq count_initgood then goto, cont1

repeat begin

    sig_old  = sig
    ittr     = ittr + 1
    result   = savitzky_golay(yy[good],ww,DEGREE=degree)
    linterp,xx[good],result,xx,goodyy
    if keyword_set(PLT) then plot,xx,yy,/xsty,psym=3,yrange=[-0.1,0.1] 

    test = yy[good]-goodyy[good] 
    moments, test,mean,dummy,sig
    test = yy-goodyy
    good = where(abs( (test-mean)/sig ) le thresh,count_good)
    bad  = where(abs( (test-mean)/sig ) gt thresh,count_bad)

    if keyword_set(PLT) then begin

        oplot,xx,goodyy,color=2
        if count_bad ne 0 then oplot,xx[bad],yy[bad],psym=4,color=3
        re = ' '
        read, re

    endif 
    if ((sig_old-sig)/sig_old) lt eps then goto, cont1

endrep until ittr eq 10

cont1:

;  Reconstruct the good/bad array if necessary.

junk       = replicate(0,count_initgood)
junk[good] = 1
ogoodbad[z_good] = junk

yyy = fltarr(n_elements(y))+!values.f_nan
yyy[z_good] = goodyy

if keyword_set(PLT) then wdelete,wid

rms = sig

return, [[x],[yyy]]


end


