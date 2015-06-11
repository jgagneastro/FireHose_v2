;+ 
; NAME:
;  wfc3_g280_reduce_qso
;
; PURPOSE:
;   This code performs a full reduction on a given QSO for WFC3/G280
;   grism data.  It is tuned for quasars, but might work well on other
;   objects (especially point sources).  Currently only reduces Order
;   +1, a.k.a. Beam A.
;
; CALLING SEQUENCE:
;   
;  wfc3_g280_reduce_qso, img_fil, spec_fil, fin_strct
;
; INPUTS:
;   img_fil -- Acquisition image file (usually has a flt extension)
;   spec_fil -- HST processed spectral image (usually has a crj
;               extension)
;
; RETURNS:
;
; OUTPUTS:
;   fin_strct -- A structure containing the 1D spectra for BeamA
;
; OPTIONAL KEYWORDS:
;   QADIR= -- Directory for QA output [default: 'QA/']
;   NAME= -- Name use for the qso for filename generation [default:
;            'QSO/']
;   EXBOX= -- Number of pixels to extend analysis for object
;             centroiding in wfc3_g280_center_direct.pro [default: 10] 
;   SRCH=  -- Number of pixels to extend search for QSO in direct
;             image in wfc3_g280_center_direct.pro [default: 10]
;   XGUESS=  -- Guess at x-centroid of object in direct image
;               [default: 2047L]
;   YGUESS=  -- Guess at y-centroid of object in direct image
;               [default: 1085L]
;   NIMG=   -- Number of images used in crj stack (Default=2)
;
; OPTIONAL OUTPUTS:
;   DIRECT_IMG=  -- Direct image 
;   SPEC_IMG=  -- Spectral image
;   SKY_IMG=  -- Generated sky image (over BeamA only)
;
; COMMENTS:
;
; EXAMPLES:
;   wfc3_strct = wfc3_g280_mkcalibstrct() 
;
; PROCEDURES CALLED:
;  wfc3_g280_center_direct
;  wfc3_g280_trace_wave
;  wfc3_g280_qa_trace
;  wfc3_g280_skysub
;  wfc3_g280_boxcar
;  wfc3_g280_flux
;
; REVISION HISTORY:
;   23-Dec-2010 Written by JXP/JMO
;------------------------------------------------------------------------------
pro wfc3_g280_reduce_qso, img_fil, spec_fil, fin_strct, QADIR=qadir, NAME=name, $
                          EXBOX=exbox, SRCH=srch, $
                          DIRECT_IMG=direct_img, SPEC_IMG=specim, SKY_IMG=sky_img, $
                          XGUESS=xguess, YGUESS=yguess, NIMG=nimg
  if (N_params() LT 3) then begin 
    print,'Syntax - ' + $
          'wfc3_g280_reduce_qso, img_fil, spec_fil, [fin_strct], QADIR=, NAME=, ' + $
          'XGUESS=, YGUESS=, SEARCH= [v1.0]'
    return
  endif 

  if not keyword_set(NAME) then name = 'QSO'
  if not keyword_set(SKY_DIR) then sky_dir = 'Sky/'
  if not keyword_set(QADIR) then qadir = 'QA/'
  if not keyword_set(EXBOX) then exbox = 10L
  if not keyword_set(XGUESS) then XGUESS=2047L 
  if not keyword_set(YGUESS) then YGUESS=1085L 
  if not keyword_set(NIMG) then NIMG=2  ;; Default for our program.  Important for read-noise

  ;; Centroid the direct image
  wfc3_g280_center_direct, img_fil, x0, y0, $
                   XGUESS=XGUESS, YGUESS=YGUESS, EXTENDBOX=EXBOX, SRCH=srch, $
                   DIRECT_IMG=direct_img
  print, 'wfc3_center_img: Centroid = ', x0, y0

  ;;;;;;
  ;; Trace
  specim =  xmrdfits(spec_fil,1)
  sz = size(specim, /dimen)
  head = xheadfits(spec_fil)
  exptime = sxpar(head,'EXPTIME') 
  print, 'wfc3_reduce_qso: Exptime = ', exptime
  trace =  wfc3_g280_trace_wave(x0,y0,specim, JXP_KLUDGE=3.0, WFC3=wfc3)

  ;; QA
  psfile = QADIR+NAME+'_wfc3_trace.ps'
  wfc3_g280_qa_trace, psfile, trace, specim, NAME=name

  ;;;;;;;;;;;;;;;;;;;;;;;
  ;; Sky Subtraction
  specim1 = wfc3_g280_skysub(specim, trace, SKY_IMG=sky_img)
  skyfile = SKY_DIR+NAME+'_sky.fits'
  mwrfits, sky_img, skyfile, /create

  ;;;;;;;;;
  ;; Variance image
  var = (specim>1.) + NIMG*wfc3.readno^2

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Extract spectrum with a 9 pixel boxcar
  wfc3_g280_boxcar, specim1, var, trace, box_strct

  ;;;;;;;;;;;;;;;
  ;; Flux
  wfc3_g280_flux, box_strct, exptime, FLUX_STRCT, SENS_FIL=sens_fil
 
  ;;;;;;;;;;;;;;;;;;
  ;; Final structure
  fin_strct = {$
              plate: 0L, $
              fiber: 0L, $
              ra: 0.d, $
              dec: 0.d, $
              zem: 0., $
              exptime: exptime, $
              npix: n_elements(trace.trace_xa), $
              xcen: x0, $
              ycen: y0, $
              trace_x: trace.trace_xa, $
              trace_y: trace.trace_yafit, $
              wave: box_strct.wave, $
              box_fx: box_strct.counts, $
              box_sig: sqrt(box_strct.var), $
              flam: flux_strct.flam, $
              flam_sig: flux_strct.flam_sig $
              }
              
  return

end
