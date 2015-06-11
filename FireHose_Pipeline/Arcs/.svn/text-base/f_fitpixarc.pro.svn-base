;+ 
; NAME:
; x_fitarc   
;     Version 1.1
;
; PURPOSE:
;  To identify and centroid arc lines in each order.  There is
;  actually no fitting done at this stage other than to reject bad
;  lines.   The main program does the following:
;
;    1) Input the arc image 
;    2) Input an archived arc solution 
;    3) Extract 1D (boxcar) spectra down each order :: extract_arc
;    4) Cross correlate (FFT) against the archived 1D arc spectrum,
;    this gives the order number and the pixel offset
;    5) Automatically identify a set of lines (x_templarc)
;    6) Perform a low order fit to these lines
;    7) Reidentify all lines with this fit and refit 
;    8) Write arc solutions (one per order) to a fits file 
;    9) If the orders extend beyond the archived solution, attempt to
;    extrapolate to the remaining orders.  The idea is to use the
;    known wavelengths from the good orders to extrapolate a guess at
;    the solution.  Then use this guess to peak up on the arc lines in
;    these additional orders.
;
; CALLING SEQUENCE:
; x_fitarc, arc_fil, ordr_str, out_fil
;
; INPUTS:
;  arc_fil  -- Name of arc file
;  ordr_str -- Order strucure describing the echelle footprint
;  out_fil --  Name of IDL file containing the 1D fits
;
; RETURNS:
;
; OUTPUTS:
;  IDL fit file (one per order)  (e.g. Arcs/ArcECH_##fit.idl)
;
; Optional KEYWORDS:
;   /PINTER   - Perform fit for pre-identified lines using x_identify
;   /INTER    - Identify lines interactively and then fit
;   LINLIST   -  Arc line list (default: $XIDL_DIR/Spec/Arcs/Lists/hires_thar.lst
;   /CHK      - Manually check steps along the way
;   /DEBUG    - Debugging
;   /BCKWD    - Data runs from red to blue [only necessary for
;               interactive fitting]
;   SIGREJ=   - Rejection sigma for outliers in arc line fitting
;              (default: 2.)
;   IORDR     - Initial order for analysis
;   FORDR     - Final order for analysis
;   /CLOBBER  - Overwrite previous fits
;   /FWEIGHT  - Centroid arc lines using a flux-weighting scheme
;   /FGAUSS   - Centroid arc lines using a gaussian fitting scheme
;   IPSIG     - Array which sets the sigma significance for arc lines
;               as a function of order#.  This is for the initial
;               solution and it is recommended to use a higher value
;               than IFSIG.  An example:  [ [0,40,10],
;               [40,80,20]] -- This sets the significance to 10 for
;                              orders 0 to 40 and 20 for orders 40 to
;                              80.
;   IFSIG     - Similar to IPSIG except this is for the final line
;               list.  One may tend to use weaker lines (lower
;               significance) for this step. 
;   /NOLOG    - Indicates the template file does not have Log
;               wavelengths
;   SATUR     - Saturation level for the Arc [default: 30000.]
;   MXSHFT=   - Maximum shift allowed between initial guess and FFT
;               cross-correlation
;   /CLIP     - Remove especially bad lines [obsolete?]
;   GUESSARC= - Name of archived solution to use as a guess to the
;               inputted arc frame
;   /ARCPAIR  - Use the routine arcpairs to guess at the solution (not
;               well tested)
;   /NOEXTRAP - Do not try to extrapolate the solution
;   CCDSZ=    - Dimensions of the arc file [native]
;
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;
; EXAMPLES:
;   x_fitarc, aimg
;
;
; PROCEDURES/FUNCTIONS CALLED:
;   x_crossarc
;   x_fitarc_ps
;   extract_arc
;   x_templarc
;   x_identify
;   x_fndpeaks
;   z_arcpairs
;
; REVISION HISTORY:
;   12-Aug-2002 Written by JXP
;   21-Aug-2002 Streamlined + Added ps output
;   01-Feb-2003 Polished (JXP)
;   01-Jan-2004 Added a guess at unmatched orders (SB)
;   28-Jun-2005 Added arcpairs functionality (SB+JXP)
;-
;------------------------------------------------------------------------------

pro x_fitarc_fitsout, svfile, FITSFILE=fitsfile, EXPTIME=exptime, $
  MJD=mjd, GDO=gd

  ;; Open IDL file
  restore, svfile

  ;; Filename
  ipos = strpos(svfile,'.idl')
  if not keyword_set(FITSFILE) then $
    fitsfile = strmid(svfile,0,ipos)+'.fits'

  gd = where(all_arcfit.nord GT 0., ngd)
  if ngd EQ 0 then begin
      print, 'x_fitarc_fitsout: No good orders'
      return
  endif

  spec = sv_aspec[*,gd]
  mkhdr, head, spec
  if keyword_set(EXPTIME) then sxaddpar, head, 'EXPTIME', exptime
  if keyword_set(MJD) then sxaddpar, head, 'MJD-OBS', mjd
  mwrfits, spec, fitsfile, head, /create

  ;; Fit structure
  if keyword_set(USE1D) then begin
      for qq=0L,ngd-1 do begin
          dumstr = { $
                   func: all_arcfit[qq].func, $
                   nord: all_arcfit[qq].nord, $
                   nrm: all_arcfit[qq].nrm, $
                   lsig: all_arcfit[qq].lsig, $
                   hsig: all_arcfit[qq].hsig, $
                   niter: all_arcfit[qq].niter, $
                   minpt: all_arcfit[qq].minpt, $
                   maxrej: all_arcfit[qq].maxrej, $
                   flg_rej: all_arcfit[qq].flg_rej, $
                   coeff: dblarr(10), $
                   rms: all_arcfit[qq].rms $
                   }
          dumstr.coeff[0:dumstr.nord] = *all_arcfit[qq].ffit
          mwrfits, dumstr, fitsfile
      endfor
  endif
  
  ;; Compress
;  spawn, 'gzip -f '+fitsfile
          
  return
end
pro x_fitarc_ps, svdecomp, filnm, WV=wv, FIT=fit, REJPT=rejpt,$
                      ORDR=ordr, RMS=rms, FORDR=fordr, DWV=dwv

  clr = getcolor(/load)
  mn = min(wv, max=mx)
  xlbl = mn + (mx-mn)*0.05
 !p.font = 0
;device, /isolatin1
  ;; All points
  if n_elements(wv) LE 1 then NODATA=1
  plot, [wv], [(fit-wv)/dwv], psym=7, $
        charsize=1.5, ycharsize=1.5, xcharsize=1.5,$
        background=clr.white, color=clr.black,  $
        xtitle='Wavelength ('+ STRING(197B) +')', ytitle='Residual (Pixels)', $
       ; xmargin=[11,1], ymargin=[5,1], 
        yrange=[-1.2, 1.2], xrange=[mn-1,mx+1.], $
        xstyle=1, ystyle=1;,NODATA=nodata;,  xtickinterval=(fix(wv[0]/1000.)-1)*10, $
        
  ;; MASK 
  if rejpt[0] NE -1 then $
    oplot, [wv[rejpt]], [(fit[rejpt]-wv[rejpt])/dwv], psym=2, color=clr.red
  ;; Order

  ;; 
  oplot, [-9e9, 9e9], [0., 0.], linestyle=1, color=clr.blue

  xyouts, xlbl, 1.0,  'Order = '+string(ordr, FORMAT='(i3)')+$
    '   !9Dl!X = '+string(dwv, FORMAT='(f6.3)'), charsize=1.3 ;dispersion
  xyouts, xlbl, 0.85, 'RMS (pix)='+string(rms, FORMAT='(f6.3)'), charsize=1.3
  xyouts, xlbl, 0.7, 'Fit Ord='+string(fordr, FORMAT='(i2)'), charsize=1.3
          
  return
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function f_fitpixarc, arc_fil, ordr_str, piximg, orderimg, out_fil, $
                      INTER=inter, LINLIST=linlist, HIRES=hires, $
                      CHK=chk, CLOBBER=clobber, SIGREJ=sigrej, DEBUG=debug,$
                      IORDR=iordr, PINTER=pinter, BCKWD=bckwd, OSTEP=ostep, $
                      GUESSARC=guessarc, SATUR=satur, $
                      IROW_SHIFT=irow_shift, PKWDTH=pkwdth, $
                      OROW_SHIFT=row_shift, ARCPAIR=arcpair, $
                      PSIG=psig, FSIG=fsig, QAFIL=qafil, NOLOG=nolog, $
                      NOEXTRAP=noextrap, FORDR=fordr, FIT_MSK=fit_msk, $
                      CLIP=clip, OBJNAME=objname, $
                      CONSTR_POS=constr_pos, CCDSZ=ccdsz, MXSHFT=MXSHFT, $
                      FGAUSS=fgauss, THIN=THIN, TOLER=TOLER, REFIT=REFIT, $
                      ERR_MESSAGE=err_message, NOPLOT=noplot

;

  func_name = 'f_fitpixarc'
  if  N_params() LT 3  then begin 
      print,'Syntax - ' + $
        'a = f_fitarc(arc_fil, ordr_str, out_fil, ' + $
        '/INTER, IORDR=, /DEBUG, /CHK '
      print, '     /clobber, /BCKWD) [v1.1]'
      err_message = "Incorrect syntax in " + func_name + " call"
      return, -1
  endif 
  
  ;;  Optional Keywords
  if not keyword_set( MXSHFT ) then MXSHFT = 100L ; 40L
  if not keyword_set( TRCNSIG ) then trcnsig = 3.
  if not keyword_set( RADIUS ) then radius = 2.0
  if not keyword_set( PKWDTH ) then pkwdth = 5.0  
  if not keyword_set( LINLIST ) then message,'Must specifiy linelist'
  ;;if not keyword_set( NOLOG ) then FLOG = 1 else FLOG = 0
  if not keyword_set( SATUR ) then satur = 1.0d4

  ;; Instrument stuff
  if keyword_set(HIRES) then begin
      if not keyword_set(CCDSZ) then ccdsz = [2048L,3990L]
  endif

  ;; Read in order structure
  nordr = n_elements(ordr_str)

  ;; Set orders
  if not keyword_set(OSTEP) then ostep = 1 
  ordrs = lonarr(2)
  if keyword_set(IORDR) OR keyword_set(FORDR) $
    then flg_ordr = 1 else flg_ordr = 0
  if not keyword_set( IORDR ) then IORDR = ordr_str[0].order
  if not keyword_set( FORDR ) then FORDR = ordr_str[nordr-1].order
  ordrs = [IORDR, FORDR]

  ;; OUTFIL
  if flg_ordr EQ 0 AND x_chkfil(out_fil,/silent) $
    AND not keyword_set( CLOBBER ) then begin
      print, func_name + ': WARNING! Arc fits file exists!  ' + $
        'Use /CLOBBER to overwrite'
      err_message = "File exists (" + out_fil + "), /clobber not passed"
      ;; passed back as not being an error (important that this happens...)
      return, out_fil
  endif 

  ;; Open line list
  x_arclist, linlist, lines

  ;; SORT
  srt = sort(lines.wave)
  lines = lines[srt]

  ;; Grab Arc IMG
  if x_chkfil(arc_fil+'*',/silent) EQ 0 then begin
      print, 'f_fitarc: Arc ', arc_fil, ' does not exist. ' + $
        'Run a procarc!'
      err_message = "Arc file does not exist"
      return, -1
  endif
  print, 'f_fitpixarc: Reading arc: ', arc_fil
  arc_img = xmrdfits(arc_fil, 0, head, /silent)
  sz_arc = size(arc_img, /dimensions)

  ;;
  sat_region = 25*sz_arc[1]/2048L


  ;; Open Old file as necessary
  if flg_ordr EQ 1 and x_chkfil(out_fil,/silent) then restore, out_fil

  ;; Loop on Orders

  ;; MEDWID
  if not keyword_set( MEDWID ) then begin
      if sz_arc[0] EQ 2048L then MEDWID = 3L
      if sz_arc[0] EQ 1024L then MEDWID = 2L
      if sz_arc[0] LT 1024L then MEDWID = 1L
      if not keyword_set( MEDWID ) then stop
  endif
  

  msk = bytarr(sz_arc[1])
  tmp = fltarr(2*MEDWID+1L, sz_arc[1])
  ;; Create output as necessary
  if flg_ordr EQ 0 then begin
      sv_aspec = fltarr(sz_arc[1], 50)
      fittmp = { fitstrct }
      all_arcfit = replicate(fittmp, 50)
      rejtmp = { $
                 ngdf: 0L, $
                 gdfpt: lonarr(500), $
                 gdfpx: dblarr(500), $
                 nrej: 0L, $
                 rejpt: lonarr(500), $
                 rejwv: dblarr(500) $
            }
      rejstr = replicate(rejtmp, 50L)
      lintmp = { $
                 pix: dblarr(200), $
                 wv: dblarr(200), $
                 nlin: 0 $
               }
      sv_lines = replicate(lintmp, 50)
      ;; Extract the arc (pseudo-boxcar)

      nord = n_elements(ordr_str)
      ordervec = 31L-findgen(nord)
      for iord=0, nord-1 do begin
         pixel = piximg[where(orderimg EQ ordervec[iord])]
         arc1d = arc_img[where(orderimg EQ ordervec[iord])]

         arc1d_tmp = median(arc1d, 3)
         if (ordervec[iord] GE 30) then begin
            sset = bspline_iterfit(pixel, arc1d_tmp, bkspace=0.5, $
                                   upper=3.0, lower=3.0, /silent)
         endif else begin
            sset = bspline_iterfit(pixel, arc1d_tmp, bkspace=0.25, $
                                   upper=3.0, lower=3.0, /silent)
         endelse

         arcfit = bspline_valu(findgen(2048),sset)
;         if (iord EQ nord-1) then begin
;            x_splot, pixel, arc1d, psym1=3, xtwo=findgen(2048), ytwo=arcfit, /block
;         endif
         sv_aspec[0:n_elements(arcfit)-1,iord] =  arcfit
         if (ordervec[iord] EQ 31) then begin
            sv_aspec[0:650,iord] =  0
         endif

         if (ordervec[iord] EQ 30) then begin
            sv_aspec[0:400,iord] =  0
         endif

         if (ordervec[iord] EQ 29) then begin
            sv_aspec[0:50,iord] =  0
         endif
      endfor
;      sv_aspec[*,0:nordr-1] =  x_extractarc(arc_img, ordr_str)
  endif else begin
      if not keyword_set( sv_aspec ) then stop
  endelse
 
  szasp = size(sv_aspec,/dimensions)
  sv_logwv  = dblarr(szasp[0], szasp[1])

   
; Open Guess (If not interactive)
  if not keyword_set( INTER ) and not ( flg_ordr ) then begin
      ;; Open archived solutions
      if not keyword_set( GUESSARC ) then stop

      x_crossarc, guessarc, sv_aspec, guess_spec, guess_fit, guess_ordr,$
           ordr_shift, row_shift, chk=chk, sigrej=sigrej, CCDSZ=ccdsz

      print, 'f_fitpixarc: Orders offset by ', ostep*ordr_shift, $
            ' And pixel offset by ', row_shift

      ;; Reset IORDR, FORDR
      IORDR = ordr_str[0].order
      FORDR = ordr_str[nordr-1].order
      ordrs = [IORDR, FORDR]
  endif else begin
     ;;  if keyword_set( GUESSARC ) then begin
;;           x_crossarc, guessarc, sv_aspec, guess_spec, guess_fit, guess_ordr,$
;;             ordr_shift, row_shift, chk=chk, sigrej=sigrej, CCDSZ=ccdsz
;; ;            ordr_shift, row_shift, chk=chk, sigrej=sigrej, /ONLYG, CCDSZ=ccdsz
;;       endif
  endelse
  if keyword_set(guess_ordr) then begin
      svgss = guess_ordr
  endif

  ;; Set guess_ordr
  guess_ordr = ordr_str.order
      
  svshft = transpose([-99, -999L])
  fit_msk = bytarr(nordr)

  ;; Row_shift
  if keyword_set(IROW_SHIFT) then row_shift = irow_shift

  tmp_aspec = sv_aspec
  restore, guessarc
  sv_aspec = tmp_aspec

  shift_vec = fltarr(n_elements(guess_ordr))
  for ii=0, nordr-1 do begin
     jj = ordr_str[ii].order
     igss = where(guess_ordr EQ jj, n_guess)
     ;; Cross-correlate
     step = lindgen(2*MXSHFT) - MXSHFT 
     minc = (guess_ordr[ii] EQ 31) ? 650 : 0
     maxc = 2047
     corr = c_correlate((sv_aspec[minc:maxc,ii]<1000.), $
                        (guess_spec[minc:maxc,igss]<1000.), step, /double)
     mx = max(corr, imx)
     imx = step[imx]
     gfit = gaussfit(step[0.7*MXSHFT:1.3*MXSHFT], corr[0.7*MXSHFT:1.3*MXSHFT], coeffs, estimates=[1.0,imx, 3.0], nterms=3)
     shft = -coeffs[1] 
;         shft = -imx 
     if (guess_ordr[ii] EQ 11) then begin
        if (abs(shft-median(svshft[*,1])) GT 2) then begin
           shft = median(svshft[*,1])
        endif
     endif
     print, 'Order '+strtrim(guess_ordr[ii],2)+', Shifting '+strtrim(shft,2)
     shift_vec[ii] = shft
  endfor

  shft = median(shift_vec)
  print, "Applying median shift of "+strtrim(shft,2)+' pixels to the template arc to match lines...'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LOOP on ORDERS to identify and centroid arc lines
	if keyword_set(OBJNAME) AND NOT keyword_set(NOPLOT) then window, title=strtrim(objname,2) + "'s wavelength calibration"
  for ii=0, nordr-1 do begin  
      ;; Grab index
      jj = ordr_str[ii].order
      ;; Skip if IORDR set
      if jj LT min(ordrs) OR jj GT max(ordrs) then begin
          sv_logwv[*,ii] = 0.
          print, 'f_fitarc: Skipping order ', jj
          continue
      endif
      ;; Carry on
      print, 'f_fitpixarc: Order ', jj, format='(a, i4, $)'
      if not keyword_set( INTER ) then $
        igss = where(svgss EQ jj, n_guess)

      ;; Zero out
      lines[*].flg_plt = 0

; INTERACTIVE (NOT RECOMMENDED EXCEPT FOR PATIENT EXPERTS!!!)
      if keyword_set(INTER) then begin 
         
         if (keyword_set(REFIT)) then begin
            inlin   = mrdfits("xidentify_lintmp.fits",ii+1)
            incalib = mrdfits("xidentify_tmpcal.fits",ii+1)
            x_identify, sv_aspec[*,ii], finfit, LINELIST=linlist, $ 
                        OUTLIN=outlin, XSIZE=600, YSIZE=500, $
                        inlin=inlin, incalib=incalib
         endif else begin
            arr = strsplit(linlist, '/', /extract)
            narr = n_elements(arr)
             x_identify, sv_aspec[*,ii], finfit, LINELIST=arr[narr-1], $ 
                        OUTLIN=outlin, XSIZE=600, YSIZE=500 
         endelse
         
; SAVE INTERMEDIATE STEPS IN CASE XIDENTIFY/FIT BOMBS
         if (ii EQ 0) then begin 
            mwrfits, finfit, "xidentify_tmpcal.fits", /create
            mwrfits, outlin, "xidentify_lintmp.fits", /create
         endif else begin
            mwrfits, finfit, "xidentify_tmpcal.fits"
            mwrfits, outlin, "xidentify_lintmp.fits"
         endelse
         
         use = 1
          
         if (use EQ 1) then begin
            
            ngd = n_elements(outlin)
            nposs = n_elements(sv_lines[ii].pix)
            nuse = min([ngd,nposs])
;          if (ngd EQ 0) then continue
            sv_lines[ii].nlin = ngd
            
            t1 = outlin.pix
            t2 = outlin.wave
            sv_lines[ii].pix[0:nuse-1] = t1[0:nuse-1]
            sv_lines[ii].wv[0:nuse-1] = t2[0:nuse-1]
            ;; FITSTR
            
            fin_fit = {fitstrct}
            copy_struct, finfit, fin_fit, $
                         EXCEPT_TAGS=['FFIT','NRM','RMS']
            fin_fit.flg_rej = 1 
            fin_fit.niter = 3 
            fin_fit.maxrej = 10 
            fin_fit.minpt = 5
            fin_fit.hsig = sigrej
            fin_fit.lsig = sigrej
            ;; Final auto fitting in log10 space!
            fit = x_fitrej(outlin.pix, alog10(outlin.wave), FITSTR=fin_fit)
;             stop
            ;; MSK
            fit_msk[*] = 1B
            all_arcfit[ii] = fin_fit 
         endif
      endif else begin          ; endif (keyword_set(INTER))
         
; AUTO or SEMI-AUTO (PINTER)
         ;; Identify guess
         if n_guess NE 1 then begin ; Order mismatch
            print, 'f_fitarc:  Not included in guess spectra, ' + $
                   'skip to next order'
            continue
         endif else fit_msk[ii] = 1B
         
         if (0) then begin ; now handled above
         ;; Cross-correlate
            step = lindgen(2*MXSHFT) - MXSHFT + row_shift
            minc = (guess_ordr[ii] EQ 31) ? 650 : 0
            maxc = 2047
            corr = c_correlate((sv_aspec[minc:maxc,ii]<1000.), $
                               (guess_spec[minc:maxc,igss]<1000.), step, /double)
            mx = max(corr, imx)
            imx = step[imx]
            gfit = gaussfit(step[0.7*MXSHFT:1.3*MXSHFT], corr[0.7*MXSHFT:1.3*MXSHFT], coeffs, estimates=[1.0,imx, 3.0], nterms=3)
            shft = -coeffs[1] 
;         shft = -imx 
            if (guess_ordr[ii] EQ 11) then begin
               if (abs(shft-median(svshft[*,1])) GT 2) then begin
                  shft = median(svshft[*,1])
               endif
            endif
         endif

         print, ' Shifting ', shft, format='(a,i4,$)'
         
         ;; Check against row_shift ??
         
         ;; DEBUG
         if keyword_set( DEBUG ) then stop
         
          ;;;;;;;;;  GENERATE A MASK FOR FITTING ;;;;;;;;;;;;;
         
         msk = bytarr(sz_arc[1])
         good_spots = where(sv_aspec[*,ii] GT 0, ngood)
         if ngood EQ 0 then begin
         	fire_siren, func_name + ": Error generating mask for fitting!  Exiting..."
         	err_message = "Error generating mask for fitting"
         	RETURN, -1
         endif
         msk[good_spots] = 1B
         
         ;; SAT
         satpix = (sv_aspec[*,ii] GT SATUR)
         sat = where(smooth(1.0*satpix,sat_region) GT 0, nsat)
         if nsat NE 0 then msk[sat] = 0B
         
         if ptr_valid(guess_fit[igss].ffit) EQ 0 then begin
            print, 'Template order is not valid' 
            continue
         endif
         
         guess_fit.LSIG = 5.0
         guess_fit.HSIG = 5.0
         
          ;;;;;;;;;; FIRST-ROUND LINE IDENTIFICATION ;;;;;;;;
         
         ;; This shifts the template solution and Auto-ID's
         ;; wavelength-pixel pairs.  It puts the info in the lines
         ;; structure.  Does not perform the actual wavelength fit.
         
                                ; sv_aspec is the current extracted arc

         flg_templ=1
;         x_templarc, sv_aspec[*,ii], lines, guess_fit[igss], MSK=msk $
;                     , SHFT=shft, ALL_PK=all_pk, PKWDTH=pkwdth $
;                     , FORDR=9, /LOG $
;                     , PKSIG=psig[ii], FLG=flg_templ, BEST=best $ 
;                     , /THIN, TOLER=TOLER,mxoff=mxoff


;         tmp_aspec = sv_aspec
;         restore, guessarc
;         sv_aspec = tmp_aspec
         
         nlin = sv_lines[ii].nlin
         for ilin=0, nlin-1 do begin
            xguess = sv_lines[ii].pix[ilin]+shft
            xmin=floor((xguess-5) > 0)
            xmax=floor((xguess+5) < 2047)
            if (xmin GT xmax OR xguess LT 0 OR xguess GT 2047) then continue
            x = xmin + indgen((xmax-xmin))
            cent = f_centspln(x, sv_aspec[x,ii], /SILENT)
            if (abs(xguess-cent) LT 4) then begin
               sv_lines[ii].pix[ilin] = cent
               mtch = where(sv_lines[ii].wv[ilin] EQ lines.wave, nmtch)
               if (nmtch GT 0) then begin
                  if (nmtch EQ 1) then begin
                     lines[mtch].flg_plt = 1
                     lines[mtch].pix = cent
                  endif else begin
                     lines[mtch[0]].flg_plt = 1
                     lines[mtch[0]].pix = cent
                  ENDELSE
               endif
            endif
;            ;if (guess_ordr[ii] EQ 29) then stop
         endfor


         if flg_templ EQ -1 then begin
            print, 'f_fitarc: Insufficient lines for AUTO!!'
            continue
         endif
         ;; Check the number of good lines
         gdfit = where(lines.flg_plt EQ 1, ngd)
         
         if ngd lt 2 then begin
            fire_siren, func_name + ": WARNING: Not enough lines found using x_templarc in first-round line identification!  Need at least two, or x_fitrej will fail in the normalization process.  Increasing the tolerance until at least two lines are found, or we give up..."
            toler_new = TOLER
            toler_max = 20.0D ;; Maximum tolerance to use
            while( ngd lt 2 AND toler_new LT toler_max ) do begin
               toler_new = toler_new + 0.1
               x_templarc, sv_aspec[*,ii], lines, guess_fit[igss], MSK=msk $
                           , SHFT=shft, ALL_PK=all_pk, PKWDTH=pkwdth $
                           , FORDR=9, /LOG $
                           , PKSIG=psig[ii], FLG=flg_templ, BEST=best $ 
                           , /THIN, TOLER=toler_new,mxoff=mxoff
               gdfit = where(lines.flg_plt EQ 1, ngd)
            endwhile
            if( ngd lt 2 ) then begin
               fire_siren, func_name + ": WARNING: Unable to obtain at least two lines in x_templarc, even when the tolerance was increased to " + strtrim(toler_max,2) + ".  We will skip this order..."
               if keyword_set( DEBUG ) then stop
               continue
            endif else begin
               print, func_name + ": Able to obtain " + strtrim(ngd,2) + " good lines by increasing the tolerance of x_templarc to " + strtrim(toler_new,2) + "."
            endelse
         endif
         
          ;;;;;;;;;;;; FIRST (crude) FIT ;;;;;;;;;;;
         
         ;; To avoid memory leaks, if tmp_fit is already defined, then free its pointer
         if is_undefined(tmp_fit) EQ 0 then begin
            if ptr_valid(tmp_fit.ffit) EQ 1 then ptr_free, tmp_fit.ffit
         endif
         
         tmp_fit = {fitstrct}
         copy_struct, guess_fit[igss], tmp_fit, $
                      EXCEPT_TAGS=['FFIT','NRM','RMS']
         tmp_fit.flg_rej = 1 
         tmp_fit.niter = 3 
         tmp_fit.maxrej = 10 
         tmp_fit.minpt = 5
         tmp_fit.hsig = sigrej
         tmp_fit.lsig = sigrej
;          tmp_fit.nord = (4 < guess_fit[igss].nord) ; Set first fit to 4
         
         fin_fit = tmp_fit
         fin_fit.nord = guess_fit[igss].nord
         
         if tmp_fit.nord GE ngd then begin
            fire_siren, func_name + ':  WARNING!  Trying to fit a polynomial of order ' + strtrim(tmp_fit.nord,2) + ' in the first crude fit, but only ' + strtrim(ngd,2) + ' good lines were found.  Changing the order of the polynomial to be fit to ' + strtrim(ngd-1,2) + ' so as to avoid the automatic crash.'
            tmp_fit.nord = ngd-1
         endif

         fit = x_fitrej(lines[gdfit].pix, lines[gdfit].wave $
                        ,FITSTR=tmp_fit, REJPT=rejpt)
         if fit[0] EQ -1 then begin
            print, 'f_fitarc: AUTO Failed!!'
;              stop
         endif
         
         ;;;;;;;;;; THE ARCHIVE SOLUTION HAS BEEN READ IN AND MATCHED
         ;;;;;;;;;; NOW PLOT THE FIT TO ARCHIVE LINES ONLY
         
         if NOT keyword_set(NOPLOT) then begin
	         !p.multi=[0,1,3]
	         
	         lines_per_micron = 54.49 / 1000.
	         blaze_angle = 46.0 * 3.14159 / 180.
	         lam0  = (2 / lines_per_micron * sin(blaze_angle)) $
	                 / float(jj) * 10000.
	         fsr = lam0 / float(jj)
	         
	         xx = indgen(2048)
	         colors=getcolor(/load)
	         djs_iterstat,sv_aspec[*,ii], sigma=sigma
	         junk = (sv_aspec[*,ii] < (sv_aspec[*,ii]*0+900.*sigma))
	         plot, xx, sv_aspec[*,ii], yrange=[-sigma,1000*sigma], color=colors.white, /xsty, /ysty, title="Order "+strtrim(jj,2)
	         oplot, lines[gdfit].pix, junk[lines[gdfit].pix], psym=2, color=colors.red
	         
	         yy = x_calcfit(xx, fitstr=tmp_fit)
	         plot, xx, yy, /yno, color=colors.white, /xsty, background=colors.black
	         oplot, lines[gdfit].pix, lines[gdfit].wave, psym=2
	         plots, [0,2048],[lam0-0.8*fsr,lam0-0.8*fsr], linestyle=1
	         plots, [0,2048],[lam0+0.8*fsr,lam0+0.8*fsr], linestyle=1
	         
	         resid = lines[gdfit].wave-x_calcfit(lines[gdfit].pix, fitstr=tmp_fit)
	         plot, lines[gdfit].pix, resid, /xsty, /ysty, psym=2, xrange=[0,2048], color=colors.white, yrange=[-0.5,0.5]
	         plots, [0,2048], [0,0]
	         xyouts, 1500, 0.3, "Order = "+strtrim(tmp_fit.nord,2)
	         xyouts, 1500, 0.2, "RMS   = "+strtrim(tmp_fit.rms,2)
	         
	         
	         !p.multi=0
			endif
         
          ;;;;;;;; AUTO ID OFF THE INITIAL FIT
          ;;;;;;;; IF PINTER IS SET, 


         ;; AUTOID LINES IF NOT RUNNING PINTER
         ;; TURN THIS OFF FOR NOW, WE"VE PT GOOD LINES IN THE 
         ;; FIT STRUCTURE AND THIS REIDENTIFY IS ADDING BAD LINES.
         if (NOT keyword_set(PINTER) AND 0) then begin
            lines.flg_plt = 0
            shft = 0L           ; No shift expected now!
            x_templarc, sv_aspec[*,ii], lines, tmp_fit, MSK=msk, PKWDTH=pkwdth $
                        , FORDR=9, SHFT=shft, PKSIG=fsig[ii] $
                        , /THIN, toler=toler,mxoff=mxoff ;;,/LOG
         endif
         
         svshft = [svshft, transpose( [jj, round(shft)])]
         gdfit = where(lines.flg_plt EQ 1, ngd)
         
          ;;;;;;; IF TOO FEW LINES ARE FOUND, HUNT FOR MORE
         if (ngd lt 2 AND NOT keyword_set(PINTER)) then begin
            fire_siren, func_name + ": WARNING: Not enough lines found using x_templarc in crude fit re-identification!  Need at least two, or x_fitrej will fail in the normalization process.  In increasing the tolerance until at least two lines are found, or we give up..."
            toler_new = TOLER
            toler_max = 20.0D ;; Maximum tolerance to use
            
            while( ngd lt 2 AND toler_new LT toler_max ) do begin
               toler_new = toler_new + 0.1
               x_templarc, sv_aspec[*,ii], lines, guess_fit[igss], MSK=msk $
                           , SHFT=shft, ALL_PK=all_pk, PKWDTH=pkwdth $
                           , FORDR=9, /LOG $
                            , PKSIG=psig[ii], FLG=flg_templ, BEST=best $ 
                           , /THIN, TOLER=toler_new,mxoff=mxoff
               gdfit = where(lines.flg_plt EQ 1, ngd)
            endwhile
            if( ngd lt 2 ) then begin
               fire_siren, func_name + ": WARNING: Unable to obtain at least two lines in x_templarc, even when the tolerance was increased to " + strtrim(toler_max,2) + ".  We will skip this order..."
               if keyword_set( DEBUG ) then stop
               continue
            endif else begin
               print, func_name + ": Able to obtain " + strtrim(ngd,2) + " good lines by increasing the tolerance of x_templarc to " + strtrim(toler_new,2) + "."
            endelse
         endif
         
         ;; Mask out regions far outside FIRE's free spectral range.
         if (0) then begin
            lines_per_micron = 54.49 / 1000.
            blaze_angle = 46.0 * 3.14159 / 180.
            lam0  = (2 / lines_per_micron * sin(blaze_angle)) $
                    / float(jj) * 10000.
            fsr = lam0 / float(jj)
            for ichk=0, ngd-1 do begin
               if ((lines[gdfit[ichk]].wave LT lam0-fsr OR $
                    lines[gdfit[ichk]].wave GT lam0+fsr) AND $
                   NOT keyword_set(THAR)) then begin
                  lines[gdfit[ichk]].flg_plt = 0
               endif
            endfor
         endif
         
          ; WE HAVE MASKED LINES WITH A BAD INDIVIDUAL FIT, 
          ; NOW MASK LINES WITH LARGE VARIANCE IN THE INITIAL WAVE
          ; FIT

          resid = lines.wave-x_calcfit(lines.pix, fitstr=tmp_fit)
          resid_max = 0.7
          gdfit = where(lines.flg_plt EQ 1 AND $
                        abs(resid) LT resid_max, ngd)
	  if ngd lt 2 then begin
             fire_siren, func_name + ": WARNING: Not enough good lines found using crude fit!  (Need at least two.)  Increasing the acceptable difference between lines.wave and the calculated fit until at least two lines are obtained or we give up..."
             max_to_try = 5.0
             while (ngd lt 2) AND (resid_max lt max_to_try) do begin
                resid_max = resid_max + 0.1
                gdfit = where(lines.flg_plt EQ 1 AND $
                              abs(resid) LT resid_max, ngd)
             endwhile
             if ngd lt 2 then begin
                fire_siren, func_name + ": ...ok, giving up.  Couldn't find enough good lines, and we maxed out at an acceptable difference of " + strtrim(max_to_try,2) + ".  We will skip this order..."
                
                if keyword_set( DEBUG ) then stop
                continue
             endif else begin
                print, func_name + ":  Enough lines (" + strtrim(ngd,2) + ") found after increasing acceptable error to " + strtrim(resid_max,2)
             endelse 
          endif
          
          gdfit = where(lines.flg_plt EQ 1 AND $
                        abs(resid) LT 3.0, ngd)
          
          ;; Adjust order number
          fin_fit.nord = fin_fit.nord < (ngd-1)

          ;; Save gdfit (for QA)
          if ngd GT 0 then begin
             rejstr[ii].ngdf = ngd
             rejstr[ii].gdfpt[0:ngd-1] = gdfit
             rejstr[ii].gdfpx[0:ngd-1] = lines[gdfit].pix
          endif else rejstr[ii].ngdf = 0

          ;; CHK 
          if keyword_set( CHK ) then begin
             if keyword_set( DEBUG ) then $
                printcol, lines[gdfit].pix, lines[gdfit].wave
             x_prspeaks, sv_aspec[*,ii], lines[gdfit].pix, /block
             if keyword_set( DEBUG ) then stop
          endif
 
          wait, 0.5
        

          ;;;;;;;;; RE-FIT USING FULL LIST ;;;;;;;;;;;;;;;;;;;;
          ;;;; IF /PINTER IS SET, CALL X_IDENTIFY IMMEDIATELY ;;;;;;
          ;;; OTHERWISE FIT AUTOMATICALLY ;;;;;;;;;;;;;;

          if not keyword_set( PINTER ) then begin ;; AUTO FIT
             if ngd LT 2 then begin
                fire_siren, func_name + ': WARNING: Either 0 or 1 good line this order!  This will cause the normalization in x_fit.pro to fail.  We will skip this order...'
                if keyword_set( DEBUG ) then stop
                continue
             endif
             
             fit = x_fitrej(lines[gdfit].pix,lines[gdfit].wave $
                            ,FITSTR=fin_fit, REJPT=rejpt, GDPT=gdpt)
             if fit[0] EQ -1 then begin
                print, 'x_fitarc: Lowering order by 1'
                fin_fit.nord = fin_fit.nord - 1
                ivar = sv_aspec[lines[gdfit].pix]
                fit = x_fitrej(lines[gdfit].pix, lines[gdfit].wave $
                               ,FITSTR=fin_fit, REJPT=rejpt, GDPT=gdpt, $
                               IVAR=ivar)
                if fit[0] EQ -1 then begin
	                fire_siren, func_name + ': Trouble with re-fit.  We will skip this order...'
	                if keyword_set( DEBUG ) then stop
	                continue                
                endif
             endif
             
             ;; Save lines
             ngd = n_elements(gdpt)
             sv_lines[ii].nlin = ngd
             sv_lines[ii].pix[0:ngd-1] = lines[gdfit[gdpt]].pix
             sv_lines[ii].wv[0:ngd-1] = lines[gdfit[gdpt]].wave

          endif else begin ;; PINTER FIT in x_identify
             svlines = lines
             arr = strsplit(linlist, '/', /extract)
             narr = n_elements(arr)
             xsize=600 
             ysize=500 
             linein = lines[where(lines.flg_plt EQ 1, nin)]
             print, "Input "+strtrim(nin,2)+" lines to x_identify"
             x_identify, sv_aspec[*,ii], id_fit, LINELIST=arr[narr-1], $
                         INLIN=linein, OUTLIN=outlin, XSIZE=xsize, YSIZE=ysize
             ;;  Parse the rejected lines
             rejpt = where(outlin.flg_plt EQ 0 AND svlines.flg_plt EQ 1)
             ;; Save
             ngd = n_elements(outlin)
             sv_lines[ii].nlin = ngd
             sv_lines[ii].pix[0:ngd-1] = outlin.pix
             sv_lines[ii].wv[0:ngd-1] = outlin.wave
             ;; Final auto fitting in log10 space!
             fin_fit.nord = id_fit.nord
             ;; Cant deal with additional rejected lines for now  JXP

;             fit = x_fitrej(outlin.pix,outlin.wave, FITSTR=fin_fit)

             ; If we've fit interactively, use the human-checked
             ; fit and don't go back and run x_fitrej.
             fin_fit = id_fit
             ;;fit = x_fitrej(outlin.pix, alog10(outlin.wave), FITSTR=fin_fit)


             ; Sync up the sv_lines and lines structures.
             nlin = sv_lines[ii].nlin
             for ilin=0, nlin-1 do begin
                mtch = where(sv_lines[ii].wv[ilin] EQ lines.wave, nmtch)
                if (nmtch GT 0) then begin
                   if (nmtch EQ 1) then begin
                      lines[mtch].flg_plt = 1
                      lines[mtch].pix = sv_lines[ii].pix[ilin]
                   endif else begin
                      lines[mtch[0]].flg_plt = 1
                      lines[mtch[0]].pix = sv_lines[ii].pix[ilin]
                   endelse                      
                endif
             endfor
             gdfit = where(lines.flg_plt EQ 1)

          endelse
          
          ;; Rejected points (for QA)
          if rejpt[0] NE -1 then begin
             rejstr[ii].nrej = n_elements(rejpt)
             rejstr[ii].rejpt[0:rejstr[ii].nrej-1] = rejpt
             rejstr[ii].rejwv[0:rejstr[ii].nrej-1] = lines[gdfit[rejpt]].wave
          endif else rejstr[ii].nrej=0L

			if NOT keyword_set(NOPLOT) then begin
        
	          !p.multi=[0,1,3]

	          lines_per_micron = 54.49 / 1000.
	          blaze_angle = 46.0 * 3.14159 / 180.
	          lam0  = (2 / lines_per_micron * sin(blaze_angle)) $
	                / float(jj) * 10000.
	          fsr = lam0 / float(jj)

	          xx = indgen(2048)
	          colors=getcolor(/load)
	          djs_iterstat,sv_aspec[*,ii], sigma=sigma
	          plot, xx, sv_aspec[*,ii], yrange=[-sigma,50*sigma], color=colors.white, /xsty, /ysty, title="Order "+strtrim(jj,2)
	          junk = (sv_aspec[*,ii] < (sv_aspec[*,ii]*0+48.*sigma))
	          oplot, lines[gdfit].pix, junk[lines[gdfit].pix], psym=2, color=colors.red

	          yy = x_calcfit(xx, fitstr=fin_fit)
	          plot, xx, yy, /yno, color=colors.white, /xsty
	          oplot, lines[gdfit].pix, lines[gdfit].wave, psym=2
	          plots, [0,2048],[lam0-0.8*fsr,lam0-0.8*fsr], linestyle=1
	          plots, [0,2048],[lam0+0.8*fsr,lam0+0.8*fsr], linestyle=1

	          resid = lines[gdfit].wave-x_calcfit(lines[gdfit].pix, fitstr=fin_fit)
	          plot, lines[gdfit].pix, resid, /xsty, /ysty, psym=2, xrange=[0,2048], color=colors.white, yrange=[-2,2]
	          plots, [0,2048], [0,0]
	          xyouts, 1500, 1.3, "Order = "+strtrim(fin_fit.nord,2)
	          xyouts, 1500, 1.0, "RMS   = "+strtrim(fin_fit.rms,2)
	          

	          !p.multi=0
	;          stop
	;          x_splot,  xx, sv_aspec[*,ii], xtwo=lines[gdfit].pix, ytwo=junk[lines[gdfit].pix], psym2=1, /block
	       
			endif
  
          ;; Output results of the fit 
          if sv_lines[ii].nlin GT 0 then begin
             wv = x_calcfit(dindgen(sz_arc[1]),fitstr=fin_fit)
             sv_logwv[*,ii]=alog10(wv)
             dwv = abs(median(wv-shift(wv,1)))
             RMSP= fin_fit.rms/dwv
             print, ', NLIN=', ngd, format='(a, i4, $)'
             print, ', RMS (Ang,Pix) = ' ,fin_fit.rms, format='(a,f7.4,$)'
             print, RMSP,format='(f8.4)'
             ;;sv_logwv[*,ii] = x_calcfit(dindgen(sz_arc[1]),fitstr=fin_fit)
             ;;wv = 10^sv_logwv[*,ii]
;          if not keyword_set(BCKWD) then dwv = median(wv - shift(wv,1)) else $
;            dwv = median(wv - shift(wv,-1))
          endif
          
          all_arcfit[ii] = fin_fit
          
          ;; Clipping (important for BAD fits at edge of CCD)
          if keyword_set( CLIP ) then begin
             if (ii EQ 0 OR ii EQ (nordr-1)) AND (RMSP GT CLIP) then begin
                print, 'x_fitarc: Clipping lines from order ', jj
                sv_lines[ii].nlin = 0
             endif
          endif
       endelse


      ;; OUTPUT
      if keyword_set( DEBUG ) then $
         print, 'x_fitarc: Writing fit to ', out_fil
      ;; Output to hard drive (IDL file)
      save, guess_ordr, sv_aspec, all_arcfit, sv_lines, rejstr, $
            filename=out_fil
      if keyword_set(FITSOUT) then x_fitarc_fitsout, out_fil

      wait, 0.5

   endfor
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXTRAPOLATE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Now fill in the rest of the orders where we couldn't find a template
  if not keyword_set( NOEXTRAP ) then begin
      
     log_order = alog10(ordr_str.order) # replicate(1,sz_arc[1])
     wave_data  = transpose(sv_logwv[*,0:nordr-1])  
     xy2traceset, log_order, wave_data, tset_wave, invvar=wave_data GT 2.5, $
                  ncoeff = 3, yfit=wave_fit, /silent, /double
     
     wave_fit = transpose(wave_fit)
     
     search_orders = where(wave_data[*,0] LT 2.5, nsearch)
     if nsearch NE 0 then begin
        print, 'x_fitarc: Now attempting to extrapolate to unmatched orders'
        print, '   ...wish me luck'
     endif
     
     ;;
     ;;  Just copy and paste from above
     ;;  
     for i=0,nsearch -1 do begin
        ii = search_orders[i]
        jj =  ordr_str[ii].order
        print, 'x_fitarc: Order ', jj, format='(a, i4, $)'
        
        x_wave = dindgen(sz_arc[1])
        tmp_fit.nord = 3
        fin_fit = tmp_fit
        fin_fit.nord = 4
        
        fit = x_fitrej(x_wave, wave_fit[*,ii], FITSTR=tmp_fit, REJPT=rejpt)
        
        ;; Grab new lines   
        lines.flg_plt = 0
        
        ;; Find shift from closest
        if i NE 0 then shft = svshft
        
        ;; Final sigma
        ;;  if keyword_set(IFSIG) then begin
;;               gd = where(jj GE ifsig[0,*] AND jj LT ifsig[1,*],ngd)
;;               if ngd NE 1 then stop
;;               fsig = ifsig[2,gd]
;;           endif
        
        ;; Mask
        msk = bytarr(sz_arc[1])
        msk[where(sv_aspec[*,ii] GT 0)] = 1B
        satpix = (sv_aspec[*,ii] GT SATUR)
        sat = where(smooth(1.0*satpix,sat_region) GT 0, nsat) 
        if nsat NE 0 then msk[sat] = 0B
        
        anylines = where(lines.wave GT 10^min(fit) AND $
                         lines.wave LT 10^max(fit), nany)
        if nany LT tmp_fit.nord then begin
           print, 'x_fitarc: Not enough lines to match'
           fin_fit = tmp_fit
           all_arcfit[ii] = temporary(fin_fit)
           continue
        endif else fit_msk[ii] = 1B

        x_templarc, sv_aspec[*,ii], lines, tmp_fit, MSK=msk, PKWDTH=pkwdth, $
                    FORDR=9, SHFT=shft, PKSIG=fsig[ii],/THIN, /LOG $
                    ,toler=toler,mxoff=mxoff
        svshft = shft
        gdfit = where(lines.flg_plt EQ 1, ngd)
        
        if (ngd LE tmp_fit.nord) then begin
           print, 'x_fitarc: Not enough good lines.  Punting...' 
           continue
        endif
        
        ;; Save gdfit (for QA)
        rejstr[ii].ngdf = ngd
        rejstr[ii].gdfpt[0:ngd-1] = gdfit
        rejstr[ii].gdfpx[0:ngd-1] = lines[gdfit].pix
        
        ;; Adjust order number
        fin_fit.nord = fin_fit.nord < (ngd-1)
        
        if keyword_set( CHK ) then begin
           if keyword_set( DEBUG ) then $
              printcol, lines[gdfit].pix, lines[gdfit].wave
           x_prspeaks, sv_aspec[*,ii], lines[gdfit].pix, /block
           if keyword_set( DEBUG ) then stop
        endif
        
        ;; FIT
        if not keyword_set( PINTER ) then begin ;; AUTO FIT
           fit = x_fitrej(lines[gdfit].pix, alog10(lines[gdfit].wave), $
                          FITSTR=fin_fit, REJPT=rejpt, GDPT=gdpt)
           if fit[0] EQ -1 then begin
              print, 'x_fitarc: Lowering order by 1'
              fin_fit.nord = fin_fit.nord - 1
              fit = x_fitrej(lines[gdfit].pix, alog10(lines[gdfit].wave), $
                             FITSTR=fin_fit, REJPT=rejpt, GDPT=gdpt)
              if fit[0] EQ -1 then begin
              	 fire_siren, func_name + ': WARNING: Trouble with fit!  We will skip this order...'
                if keyword_set( DEBUG ) then stop
                continue
              endif
           endif
           ;; Save lines
           ngd = n_elements(gdpt)
           sv_lines[ii].nlin = ngd
           sv_lines[ii].pix[0:ngd-1] = lines[gdfit[gdpt]].pix
           sv_lines[ii].wv[0:ngd-1] = lines[gdfit[gdpt]].wave
        endif else begin ;; INTER FIT in x_identify
           x_identify, sv_aspec[*,ii], id_fit, LINELIST=linlist, $
                       INLIN=lines, OUTLIN=outlin, XSIZE=xsize, YSIZE=ysize
           ngd = n_elements(outlin)
           sv_lines[ii].nlin = ngd
           sv_lines[ii].pix[0:ngd-1] = outlin.pix
           sv_lines[ii].wv[0:ngd-1] = outlin.wave
           ;; Final auto fitting in log10 space!
           fin_fit.nord = id_fit.nord
           fit = x_fitrej(outlin.pix, alog10(outlin.wave), FITSTR=fin_fit)
        endelse
        
        ;; Rejected points (for QA)
        if rejpt[0] NE -1 then begin
           rejstr[ii].nrej = n_elements(rejpt)
           rejstr[ii].rejpt[0:rejstr[ii].nrej-1] = rejpt
        endif else rejstr[ii].nrej=0L
        
        wv = x_calcfit(dindgen(sz_arc[1]),fitstr=fin_fit)
        sv_logwv[*,ii]=alog10(wv)
        dwv = abs(median(wv - shift(wv,-1)))
        ;; Output results of the fit 
        print, ', NLIN=', ngd, format='(a, i4, $)'
        print, ', RMS (Ang,Pix) = ', fin_fit.rms, format='(a,f7.4,$)'
        print, fin_fit.rms/dwv,format='(f8.4)'
        all_arcfit[ii] = temporary(fin_fit)
        ;; OUTPUT
        if keyword_set( DEBUG ) then $
           print, 'x_fitarc: Writing fit to ', out_fil
        ;; Output
        save, guess_ordr, sv_aspec, all_arcfit, sv_lines, rejstr, filename=out_fil
        ;;  FITS
        if keyword_set(FITSOUT) then x_fitarc_fitsout, out_fil
        
     endfor

     ; If running in PINTER mode to generate a new arc solution
     ; you should comment this stop in and write out arrays
     ; to an IDL save file.
     stop 

  endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; QA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  restore, out_fil
  if keyword_set( QAFIL ) then begin
      
     x_psopen, qafil, /maxs, /landscape
     clr = getcolor(/load)
      !p.multi=[0,3,2]
      
      ;; Plot to ps file  (this does not include edited lines)
      for ii=0L,nordr-1 do begin
          if rejstr[ii].ngdf EQ 0L then continue
          gdfit = rejstr[ii].gdfpt[0:rejstr[ii].ngdf-1]
          ;; Rejpt
          if rejstr[ii].nrej NE 0 then rejpt = rejstr[ii].rejpt[0:rejstr[ii].nrej-1] $
          else rejpt = -1L
          ;; Dwv
          wv = x_calcfit(dindgen(sz_arc[1]),fitstr=all_arcfit[ii])
          ;;wv = 10^x_calcfit(dindgen(sz_arc[1]),fitstr=all_arcfit[ii])
          dwv = abs(median(wv - shift(wv,1)))
          ;; Subroutine 
          fit=x_calcfit(double(rejstr[ii].gdfpx[0:rejstr[ii].ngdf-1]), $
                        FITSTR=all_arcfit[ii])

          x_fitarc_ps, WV=lines[gdfit].wave, $
            fit=fit, $
            REJ=rejpt, $
            DWV=dwv, $
            ORDR=ordr_str[ii].order,$
            RMS=all_arcfit[ii].rms/dwv, $ ; pix
            FORDR=all_arcfit[ii].nord
      endfor
   
      x_psclose
      !p.multi=[0,1,1]
      replace_title = '"' + '%%Title: '+qafil + ' ' +systime() + '"'
      ps_replacetitle, replace_title, qafil

      spawn, 'gzip -f '+qafil
  endif

  ;; Clean up
  if keyword_set(GUESSARC) then delvarx, guessarc
	nfit = n_elements(guess_fit)
	for i=0, nfit-1 do begin
		if ptr_valid(guess_fit[i].ffit) then ptr_free, guess_fit[i].ffit
	endfor
	if ptr_valid(tmp_fit.ffit) then ptr_free, tmp_fit.ffit
	nfit = n_elements(all_arcfit)
	for i=0, nfit-1 do begin
		if ptr_valid(all_arcfit[i].ffit) then ptr_free, all_arcfit[i].ffit
	endfor

  print, 'x_fitarc_work: All done!'

  fire_undefine, err_message
  return, out_fil
end

