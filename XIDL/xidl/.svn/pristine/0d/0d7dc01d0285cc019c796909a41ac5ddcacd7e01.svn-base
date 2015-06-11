;+ 
; NAME:
;  wfc3_g280_trace_wave
;
; PURPOSE:
;   This code uses the direct image centroid to generate a first guess
;   at the trace.  It then tweaks up this trace using Gaussian
;   centroiding along the columns.  Lastly, it generates a wavelength
;   array based on the AXE team coefficients.   All of this is for
;   BEAM_A only.
;
; CALLING SEQUENCE:
;  trace_strct =  wfc3_g280_trace_wave(x0,y0,specim, JXP_KLUDGE=, WFC3=)
;
; INPUTS:
;   x0 -- Best centered x position in direct image
;   y0 -- Best centered y position in direct image
;   specim -- 2D spectral image
;
; RETURNS:
;  trace_strct -- Structure containing the trace and wavelength info
;
; OUTPUTS:
;
; OPTIONAL KEYWORDS:
;   JXP_KLUDGE= -- Offset in x for generating the wavelength solution
;                  [default: 3.0; based on eyeball comparison to SDSS spectra]
;
; OPTIONAL OUTPUTS:
;   WFC3=  -- Structure used to generate the trace and wavelength info
;
; COMMENTS:
;
; EXAMPLES:
;  trace =  wfc3_g280_trace_wave(x0,y0,specim, JXP_KLUDGE=3.0, WFC3=wfc3)
;
; PROCEDURES CALLED:
;  wfc3_g280_mkcalibstrct
;  gaussfit
;  poly_fit
;
; REVISION HISTORY:
;   23-Dec-2010 Written by JXP/JMO
;------------------------------------------------------------------------------
function wfc3_g280_trace_wave,xcen,ycen,specim,AXE1=axe1, JXP_KLUDGE=jxp_kludge, $
                      WFC3=wfc3

  if (N_params() LT 3) then begin 
    print,'Syntax - ' + $
          'trace_strct =  wfc3_g280_trace_wave(x0,y0,specim, JXP_KLUDGE=, WFC3=) [v1.0]'
    return, -1
  endif 
  
  ;generate the theoretical trace for Beam A
  if not keyword_set(wfc3) then wfc3 = wfc3_g280_mkcalibstrct(AXE1=axe1)
  if size(JXP_KLUDGE,/type) EQ 0 then jxp_kludge = 3.0
;  trace=wfc3_mkdefaulttrace(wfc3,xcen,ycen)

 
  ;BEAM A
  nax = wfc3.beama[1]-wfc3.beama[0]+1
  trace_xa = fltarr(nax)
  trace_ya_orig = fltarr(nax)
  cnt = 0L
  for i=wfc3.BEAMA[0],wfc3.BEAMA[1] do begin
     dx=float(i)
     dy=wfc3.DYDX_A_0 + wfc3.DYDX_A_1*dx + $
        wfc3.DYDX_A_2*dx*dx + wfc3.DYDX_A_3*dx*dx*dx + $
        wfc3.DYDX_A_4*dx*dx*dx*dx + $
        wfc3.DYDX_A_5*dx*dx*dx*dx*dx

     ;; Save
     trace_xa[cnt] = xcen + dx + wfc3.XOFF_A + wfc3.XOFF_A2
     trace_ya_orig[cnt] = ycen + dy + wfc3.YOFF_A + wfc3.YOFF_A2
;     print, dx, dy
     cnt = cnt+1
  endfor

  ;extract beam A
;  ax=where(trace eq 1000,nax)

  centroida=fltarr(nax)
  amplitudea=fltarr(nax)
  backgrounda=fltarr(nax)
  sigmaa=fltarr(nax)
  amperra=fltarr(nax)
  sigmaerra=fltarr(nax)
  traceoffseta=fltarr(nax)
;  trace_xa=fltarr(nax)
;  trace_ya_orig=fltarr(nax)
  trace_ya = fltarr(nax)
  trace_yafit=fltarr(nax)
  wave_a=fltarr(nax)

j=0
  for i=0,nax-1 do begin
;     t=trace[i,*]
;     a=where(t eq 1000,na) ;key in on trace a pixels
     
        ;; Defined above
;        trace_xa[j]=1.0*i
;        trace_ya_orig[j]=a
     xpix=indgen(21)
     a=round(trace_ya_orig[i])
     ypix=specim[round(trace_xa[i]),a-10:a+10] ;take 10 pixel window about trace y posn.
     
     g=gaussfit(xpix,ypix,coeffs,nterms=4,sigma=coefferrs)
     
     amplitudea[i]=coeffs[0]
     centroida[i]=coeffs[1]
     sigmaa[i]=coeffs[2]
     backgrounda[i]=coeffs[3]
     amperra[i]=coefferrs[0]
     sigmaerra[i]=coefferrs[2]
;     traceoffseta[i]=10.0-centroida[i]
     traceoffseta[i]=(a-10.0+centroida[i]) - trace_ya_orig[i]
;     if i EQ 150 then stop
  endfor

;extract beam C
;  ax=where(trace eq 3000,ncx)

;  centroidc=fltarr(ncx)
;  amplitudec=fltarr(ncx)
;  backgroundc=fltarr(ncx)
;;  sigmac=fltarr(ncx)
;  amperrc=fltarr(ncx)
;  sigmaerrc=fltarr(ncx)
;  traceoffsetc=fltarr(ncx)
;  trace_xc=fltarr(ncx)
;  trace_yc_orig=fltarr(ncx)
;  trace_yc = fltarr(ncx)
;  trace_ycfit=fltarr(ncx)
;  wave_c=fltarr(ncx)

;j=0
;  for i=0,4095 do begin
;     t=trace[i,*]
;     a=where(t eq 3000,na) ;key in on trace a pixels
     
;     if (na eq 1) then begin
;        trace_xc[j]=1.0*i
;        trace_yc_orig[j]=a
;        xpix=indgen(21)
;        ypix=specim[i,a-10:a+10] ;take 10 pixel window about trace y posn.
;        
;        g=gaussfit(xpix,ypix,coeffs,nterms=4,sigma=coefferrs)
;        
;        amplitudec[j]=coeffs[0]
;        centroidc[j]=coeffs[1]
;        sigmac[j]=coeffs[2]
;        backgroundc[j]=coeffs[3]
;        amperrc[j]=coefferrs[0]
;        sigmaerrc[j]=coefferrs[2]
;        traceoffsetc[j]=10.0-centroida[j]
;        j+=1
;     endif
;  endfor


  ;fiducial wavelengths, beam A
  for i=0,nax-1 do begin
  ;for i=xcen+wfc3.BEAMA[0],xcen+wfc3.BEAMA[1] do begin
;     dx=wfc3.beama[1] + wfc3.xoff_a  - 1.0*i
     dx=wfc3.beama[0] + 1.0*i + wfc3.xoff_A - wfc3.xoff_a2 - JXP_KLUDGE ;; JXP kludge!
     ;dx=(i-xcen)
     ;wave_a[i-xcen-wfc3.BEAMA[0]]=$
     wave_a[i]=$
        wfc3.DLDP_A_0 + wfc3.DLDP_A_1*dx + wfc3.DLDP_A_2*dx*dx + $
        wfc3.DLDP_A_3*dx*dx*dx + $
        wfc3.DLDP_A_4*dx*dx*dx*dx
  endfor
  ;wave_a=reverse(wave_a)
;stop
  ;fiducial wavelengths, beam C
;  for i=0,ncx-1 do begin
;     dx=wfc3.beamc[0] + wfc3.xoff_c + wfc3.xoff_c2 + 1.0*i
;     wave_c[i]=wfc3.DLDP_C_0 + wfc3.DLDP_C_1*dx + wfc3.DLDP_C_2*dx*dx + $
;             wfc3.DLDP_C_3*dx*dx*dx
;  endfor

  ;fiddle trace A
                                ;look at all pixels guaranteed to be
                                ;limit-free, fit a polynomial to the
                                ;offsets.  
  px2=findgen(nax)
  trace_gwv=where(wave_a gt 3000.0 and wave_a LT 4500,ntgwv)  ;; Avoid LLS troubles
  ;make a polynomial fit to the trace offset
  tpx=findgen(ntgwv)
  pfya=poly_fit(trace_gwv,traceoffseta[trace_gwv],2)
  polyoffseta=pfya[0]+pfya[1]*px2+pfya[2]*px2*px2
                                ;use the average fit polynomial from
                                ;the calib file (empirical), with
                                ;an offset in the constant term given
                                ;by the polyfit above
;  fitoffseta=pfya[0]+wfc3.tracea_poly[1]*px2+wfc3.tracea_poly[2]*px2*px2
  trace_ya=trace_ya_orig+polyoffseta
  trace_yafit=trace_ya_orig + median(traceoffseta[trace_gwv])
  print, 'wfc3_mktrace: Median offset = ', median(traceoffseta[trace_gwv])
  
  ;fiddle trace C
                                ;look at all pixels guaranteed to be
                                ;limit-free, fit a polynomial to the
                                ;offsets.  
  ;pcx2=findgen(ncx)
  ;trace_gwv=where(wave_c ge 3200.0,ntgwv)
  ;make a polynomial fit to the trace offset
  ;tpx=findgen(ntgwv)
  ;pfyc=poly_fit(tpx,traceoffsetc[trace_gwv],2)
  ;polyoffsetc=pfyc[0]+pfyc[1]*pcx2+pfyc[2]*pcx2*pcx2
                                ;use the average fit polynomial from
                                ;the calib file, with
                                ;an offset in the constant term given
                                ;by the polyfit above
  ;fitoffsetc=pfyc[0]+wfc3.tracec_poly[1]*px2+wfc3.tracec_poly[2]*px2*px2
  ;trace_yc=trace_yc_orig-polyoffsetc
  ;trace_ycfit=trace_yc_orig - fitoffsetc
  
  ;now look at the sigmas (Beam A first)
  sigma_gwv=where(reverse(wave_a) ge 2700.0,nsgwv)
  spx=findgen(nsgwv)
  pfy2a=poly_fit(spx,sigmaa[sigma_gwv],3)
  polysigmaa=pfy2a[0]+pfy2a[1]*px2+pfy2a[2]*px2*px2+pfy2a[3]*px2*px2*px2
                                ;again, pull the average polynomial
                                ;from the calib file, but this time
                                ;scale multiplicitavley to match the data
  sigmafita=wfc3.sigmaa_poly[0]+wfc3.sigmaa_poly[1]*px2+$
           wfc3.sigmaa_poly[2]*px2*px2+wfc3.sigmaa_poly[3]*px2*px2*px2
  gwv=where(reverse(wave_a) ge 3200.0,ngwv)
  scalea=median(sigmaa[gwv]/sigmafita[gwv])

  sigmafita=sigmafita*scalea

  ;Beam C sigmas
  ;sigma_gwv=where(wave_c ge 2700.0,nsgwv)
  ;spx=findgen(nsgwv)
  ;pfy2c=poly_fit(spx,sigmac[sigma_gwv],3)
  ;polysigmac=pfy2c[0]+pfy2c[1]*pcx2+pfy2c[2]*pcx2*pcx2+pfy2c[3]*pcx2*pcx2*pcx2
                                ;again, pull the average polynomial
                                ;from the calib file, but this time
                                ;scale multiplicitavley to match the data
  ;sigmafitc=wfc3.sigmac_poly[0]+wfc3.sigmac_poly[1]*px2+$
  ;         wfc3.sigmac_poly[2]*px2*px2+wfc3.sigmac_poly[3]*px2*px2*px2
  ;gwv=where(wave_c ge 3200.0,ngwv)
  ;scalec=median(sigmac[gwv]/sigmafitc[gwv])

 ;sigmafitc=sigmafitc*scalec

  struct={pfya: pfya, $
          polyoffseta: polyoffseta, $
          trace_xa: trace_xa, $
          trace_yafit: trace_yafit, $
          trace_ya: trace_ya, $
          trace_ya_orig: trace_ya_orig, $
          pfy2a: pfy2a, $
          polysigmaa: polysigmaa, $
          scalea: scalea, $
          sigmafita: sigmafita, $
          wavea: wave_a $
   ;       pfyc: pfyc, $
   ;       polyoffsetc: polyoffsetc, $
   ;       trace_xc: trace_xc, $
   ;       trace_yc: trace_yc, $
          ;trace_ycfit: trace_ycfit, $
   ;       pfy2c: pfy2c, $
    ;      polysigmac: polysigmac, $
        ;  scalec: scalec, $
       ;   sigmafitc: sigmafitc, $
     ;     wavec: wave_c $
}

return,struct
end
