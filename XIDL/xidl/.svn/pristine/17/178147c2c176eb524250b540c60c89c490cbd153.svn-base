;+ 
; NAME:
;  wfc3_g280_reduce_qso
;
; PURPOSE:
;   This code performs simple sky subtraction on the spectral image.
;   Only processes BEAM A.
;
; CALLING SEQUENCE:
;   
;  final_img = wfc3_skysub(specim, trace, SKY_IMG=)
;
; INPUTS:
;   specim -- 2D spectral image
;   trace  -- Trace structure
;
; RETURNS:
;  final_img -- Sky subtracted 2D image 
;
; OUTPUTS:
;
; OPTIONAL KEYWORDS:
;
; OPTIONAL OUTPUTS:
;   SKY_IMG= -- Image of the sky used in the sky subtraction
;
; COMMENTS:
;
; EXAMPLES:
;  final_img = wfc3_skysub(specim, trace, SKY_IMG=sky)
;
; PROCEDURES CALLED:
;
; REVISION HISTORY:
;   23-Dec-2010 Written by JXP/JMO
;------------------------------------------------------------------------------
function wfc3_g280_skysub, specim, trace, SKY_IMG=sky_img

  if (N_params() LT 2) then begin 
    print,'Syntax - ' + $
          'trace_strct =  wfc3_g280_skysub(specim, trace, SKY_IMG=) [v1.0]'
    return, -1
  endif 
  
  ;; create arrays
  sz = size(specim, /dimen)
  nax=n_elements(trace.trace_xa)
  uppersky=fltarr(nax,20)
  lowersky=fltarr(nax,20)
  fullsky=fltarr(nax,40)
  upperskymed=fltarr(nax)
  lowerskymed=fltarr(nax)
  fullskymed=fltarr(nax)

  ;; Create sky array
  for i=0,nax-1 do begin
     uppersky[i,*]=specim[round(trace.trace_xa[i]),$
                          round(trace.trace_yafit[i]+6*trace.sigmafita[i])+$
                   lindgen(20)]
     lowersky[i,*]=specim[round(trace.trace_xa[i]),$
                          round(trace.trace_yafit[i]-6*trace.sigmafita[i])-$
                   lindgen(20)]
     fullsky[i,0:19]=lowersky[i,*]
     fullsky[i,20:39]=uppersky[i,*]
  endfor

  ;; Reject
  djs_iterstat, fullsky, sigrej=2.5, mask=badpix, median=med_full

  ;; Grow 3
  grow = 1L
  szb = size(badpix, /dimens)
  cridx = where(badpix LE 0., ncr)
  if ncr GT 0 then begin
     xcr = cridx mod szb[0]
     ycr = cridx / szb[0]
     
     ximg = lindgen(szb[0]) # replicate(1L,szb[1])
     yimg = replicate(1L,szb[1]) # lindgen(szb[1])
     x0 = (xcr - grow) > 0
     x1 = (xcr + grow) < (szb[0]-1)
     y0 = (ycr - grow) > 0
     y1 = (ycr + grow) < (szb[1]-1)
  endif

  ;; Loop
  for qq=0L,ncr-1 do begin
      badpix[x0[qq]:x1[qq],y0[qq]:y1[qq]] = 0
  endfor

  ;; Simple box car median
  for i=0,nax-1 do begin
     a = where(badpix[i,*], na)
     ;; Enough for stats?
     if na LT 10 then fullskymed[i]=med_full else $
        fullskymed[i]=median(fullsky[i,a])
  endfor
  
  ;; Filter
  svfilt = savgol(10, 10, 0, 2)
  sky_model = convol(fullskymed, svfilt, /edge_trunc)
  sky_img = sky_model # replicate(1., sz[1])
  
  ;; Subtract it
  sky_sub = specim
  sky_sub[round(trace.trace_xa[0]):round(trace.trace_xa[0])+nax-1,*] = $
     specim[round(trace.trace_xa[0]):round(trace.trace_xa[0])+nax-1,*] - sky_img

  ;; Return
  return, sky_sub
end
