;+
; NAME:
;     vegaconv
;
; PURPOSE:
;     Determines the convolution kernel to be used in telluric.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     vegaconv,wave,flux,wvega,fcvega,fnvega,wmin,wmax,wline,kernel,scale,$
;              maxdev,rmsdev,WID=wid,STEPS=steps,TWF=twf,CANCEL=cancel
;
; INPUTS:
;     wave   - The wavelength array of the A0 V standard (in microns)
;     flux   - The flux array of the A0 V standard
;     wvega  - The wavelength array of Vega model (in microns)
;     fcvega - The continuum flux array of the Vega model
;     fnvega - The normalized flux array of the Vega model
;     wmin   - The lower wavelength limit of the deconvolution line
;              (in microns)
;     wmax   - The upper wavelength limit of the deconvolution line
;              (in microns)
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     WID     - The window ID number of the window used to plot the
;               results
;     STEPS   - If set, each step is plotted
;     TWF     - The factor for the Tapered Window Function (Default=10)
;     CANCEL  - Set on return if there is a problem
;
; OUTPUTS:
;     wline  - The wavelength of the deconvolution line (in microns)
;     kernel - The convolution kernel.
;     scale  - The EW scale factor for the deconvolution line.
;              EW of Vega*scale = EW of A0 V
;     maxdev - The maximum deviation of the ratio of the scaled and 
;              convolved Vega model with the A0 V standard
;     rmsdev - The rms deviation of the ratio of the scaled and 
;              convolved Vega model with the A0 V standard
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
;     Based entirely on W. Vacca's vegaconv.  The routine works in
;     pixel space while the original vegaconv works in ln(lambda) space.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2002-02-06 - Written by M. Cushing, Institute for Astronomy, UH
;     2005-12-15 - Updated plotting section with a new charsize and
;                  plot titles
;-
pro vegaconv,wave,flux,wvega,fcvega,fnvega,wmin,wmax,wline,kernel,scale,$
             maxdev,rmsdev,WID=wid,STEPS=steps,TWF=twf,$
             CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 7 then begin
    
    print, 'Syntax - vegaconv,wave,flux,wvega,fcvega,fnvega,wmin,wmax,$'
    print, '                    wline,kernel,scale,maxdev,rmsdev,WID=wid,$'
    print, '                    STEPS=steps,TWF=twf,CANCEL=cancel'
    cancel = 1

endif
cancel = cpar('mcvegaconv',wave,1,'Wave',[1,2,3,4,5],1)
if cancel then return
cancel = cpar('mcvegaconv',flux,2,'Flux',[1,2,3,4,5],1)
if cancel then return
cancel = cpar('mcvegaconv',wvega,3,'Wvega',[1,2,3,4,5],1)
if cancel then return
cancel = cpar('mcvegaconv',fcvega,4,'Fcvega',[1,2,3,4,5],1)
if cancel then return
cancel = cpar('mcvegaconv',fnvega,5,'Fnvega',[1,2,3,4,5],1)
if cancel then return
cancel = cpar('mcvegaconv',wmin,6,'Wmin',[1,2,3,4,5],0)
if cancel then return
cancel = cpar('mcvegaconv',wmax,7,'Wmax',[1,2,3,4,5],0)
if cancel then return

fact = (n_elements(TWF) ne 0) ? twf:10.
dlambda = (n_elements(DLAMBDA) ne 0) ? dlambda:0.1

wabsmin = max([min(wave),min(wvega)])
wabsmax = min([max(wave),max(wvega)])

if keyword_set(STEPS) then begin

    if n_elements(WID) eq 0 then window, 1 else wset, wid
    plot,wave,flux,/xsty,/ysty,xrange=[wmin,wmax],yrange=[0.5,1.1],psym=10
    oplot,wvega,fnvega,color=2,psym=10
    response = ''

endif

;  Locate deconvolution region for std and Vega

zdata = where((wave ge wmin) and (wave le wmax),ndata)
zmodl = where((wvega ge wmin) and (wvega le wmax),nmodl)

;  Find absorption feature by fitting a gaussian around the line

ffit  = mpfitpeak(wvega[zmodl],fnvega[zmodl],fcoeffs,/GAUSSIAN,NTERMS=4)
wline = fcoeffs[1]

if keyword_set(STEPS) then begin

    oplot, wvega[zmodl],ffit,color=3
    plots, [wline,wline],!y.crange,color=7,linestyle=1
    print, 'Absorption Line at  :  ', strtrim(wline,2)
    read, response
    
endif

;  Determine sampling frequency

dstep   = (max(wave[zdata])-min(wave[zdata]))/float(ndata-1)
mstep   = (max(wvega[zmodl])-min(wvega[zmodl]))/float(nmodl-1)

print, ''
print, 'Model sampling = ', strtrim(mstep,2) 
print, 'Data sampling  = ', strtrim(dstep,2)

;  Determine EWs of Absorption Feature and set scale

ewstar  = dstep*total(1.0 - flux[zdata])
ewvega  = mstep*total(1.0 - fnvega[zmodl])
scale   = ewstar/ewvega

print, '' 
print, 'Model Line EW = ',strtrim(ewvega,2)
print, 'Data Line EW  = ',strtrim(ewstar,2)
print, 'Scale Factor  = ',strtrim(scale,2)

;  Center the line, zero the spectra, and scale Vega.

dw     = min([(wmax-wline),(wline-wmin)])
wmin   = wline-dw
wmax   = wline+dw

; --- Get data and model between limits

wldata = wave[zdata]
fldata = flux[zdata] - 1.0          ; subtract normalization

wlmodl = wvega[zmodl]
flmodl = fnvega[zmodl] - 1.0        ; subtract normalization

if keyword_set(STEPS) then begin

    plot, wldata,fldata,/xsty,/ysty,xrange=[wmin,wmax],yrange=[-0.4,0.1],$
      psym=10
    oplot,wlmodl,flmodl*scale,color=2
    read, response

endif

; Scale the Vega model and resampling the A0 V data to the Vega sampling

sflmodl = flmodl*scale
rfldata = interpol(fldata,wldata,wlmodl)

; --- Perform the deconvolution

fftdata = fft(rfldata)
fftmodl = fft(sflmodl)
fftkern = fftdata/fftmodl

; --- Determine the Nyquist frequency 

;J. Gagne. This code crashes for odd number of frequencies. Small fix.
if n_elements(fftdata) mod 2 eq 1 then $
  remove, 0, fftdata, fftmodl, fftkern

dwmodl  = (max(wlmodl)-min(wlmodl))/float(nmodl-1)
dwdat   = (max(wlmodl)-min(wlmodl))/float(ndata-1)
fmaxmod = 1.0/(2.0*dwmodl)
fmaxdat = 1.0/(2.0*dwdat)
freq    = findgen(nmodl/2)/(float(nmodl)*dwmodl)
freq    = [freq, -1*reverse(freq[1:(nmodl/2)-1]), 0.0] 

; --- Apply a tapered window function

fitfft1 = mpfitpeak(freq,float(fftdata*fftdata),datcoef,/GAUSSIAN,NTERMS=4)
fitfft2 = mpfitpeak(freq,float(fftmodl*fftmodl),modcoef,/GAUSSIAN,NTERMS=4)

sfft    = min([modcoef(2),datcoef(2)])
fwhm    = 2.0*sfft*sqrt(2.0*alog(2.0))

fmax    = fact*fwhm
wind    = 1.0/(1.0 + (abs(freq/fmax))^10)
fftkern = fftkern*wind

; --- Inverse Fourier Transform

kernel = float(fft(fftkern,/INVERSE))

; --- Re-order kernel

nk     = n_elements(kernel)
kernel = shift(kernel,nk/2)
kernel = kernel/total(kernel)

;  Convolve the scaled Vega model with the kernel

zvega  = where(wvega ge wabsmin and wvega le wabsmax,nvega)
cvega  = convol((fnvega[zvega]-1.0)*scale,kernel,total(kernel))
rcvega = interpolv(cvega,wvega[zvega],wldata)

if keyword_set(STEPS) then begin

    plot,wldata,fldata,/xsty,/ysty,xrange=[wmin,wmax],yrange=[-0.4,0.1],$
      psym=10    
    oplot, wldata,rcvega,color=3,psym=10
    read, response
    
endif

;  Compute the ratio and get the residuals

ratio  = (fldata+1.0)/(rcvega+1.0)
maxdev = max(abs(ratio-1.0))
moments,ratio,mean,var,rmsdev
print, ''
print, 'Maximum Deviation = ', strtrim(maxdev,2)
print, 'RMS Deviation     = ', strtrim(rmsdev,2)

;  Plot the results

if keyword_set(STEPS) or n_elements(WID) ne 0 then begin
    
    if n_elements(WID) ne 0 then wset, wid
    rvega = interpolv(fnvega-1.0,wvega,wldata)
    plot,wldata,fldata,/xsty,/ysty,xrange=[wmin,wmax],yrange=[-0.4,0.1],$
      psym=10,CHARSIZE=mc_strsize('!5A',0.01),XTITLE='!5!7k!5 (!7l!5m)',$
         YTITLE='!5Residual Flux'

    oplot, wldata,rvega,color=2,psym=10
    oplot, wldata,rcvega,color=3,psym=10
    oplot, wldata,ratio-1.,color=4,psym=10 
    plots, !x.crange,[0.01,0.01],linestyle=1,color=6
    plots, !x.crange,[-0.01,-0.01],linestyle=1,color=6    

    xyouts,0.15,0.3,'!5Max Deviation: '+$
      string(maxdev*100.,format='(f4.2)')+'%',/NORM,$
                CHARSIZE=mc_strsize('!5A',0.01)
    xyouts,0.15,0.25,'!5RMS Deviation: '+$
      string(rmsdev*100.,format='(f4.2)')+'%',/NORM,$
                CHARSIZE=mc_strsize('!5A',0.01)

    xyouts, 0.75,0.4,'!5A0 V',/NORM,$
            CHARSIZE=mc_strsize('!5A',0.01)
    xyouts, 0.75,0.35,'!5Vega',/NORM,color=2,$
            CHARSIZE=mc_strsize('!5A',0.01)
    xyouts, 0.75,0.3,'!5Convolved Vega',/NORM,color=3,$
            CHARSIZE=mc_strsize('!5A',0.01)
    xyouts, 0.75,0.25,'!5Residuals',/NORM,color=4,$
            CHARSIZE=mc_strsize('!5A',0.01)
    
endif

end
