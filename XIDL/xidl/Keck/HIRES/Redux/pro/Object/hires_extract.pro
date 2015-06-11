;+ 
; NAME:
; hires_extract
;     Version 1.1
;
; PURPOSE:
;   Extract 1D spectra from the 2D images.  For each order, a boxcar
;   and an optimal extraction is performed.  For the latter, an object
;   profile is derived and both the object flux and sky are fit throughout
;   the order (i.e. not row by row).  The main driver is
;   mike_box_sngl.  For the optimal extraction, the data is extracted
;   to a specific set of vacuum wavelengths, chosen to be the same for
;   every spectrum to facilitate coadding without rebinning.  Here are
;   the steps in detail:
;
;    1.  Perform a boxcar extraction using extract_box
;    2.  Estimate the SNR per order from the boxcar extraction
;      -- LOOP ON ORDERS IN DECREASING SNR --
;    3.  Fit the boxcar extraction with a bspline
;    4.  Calculate the object profile
;          a. bspline_iterfit the flux vs position on slit
;          b. Force the profile to be positive everywhere and have a
;          sensible FWHM
;    5.  Fit the order using the profile and sky (bspline_extract)
;
;  For optimal extraction, the main calls are to x_extechopt and
;  x_extobjopt.  For boxcar, the main call is to x_extobjbox
;
; CALLING SEQUENCE:
;   
;  hires_extract, hires, setup, obj_id, chip, [exp], /RESCHK, /CHK
;
; INPUTS:
;   hires    -  MIKE structure
;   setup   -  Setup ID
;   obj_id  -  Object ID  (e.g. 1L)  (or STD index if /STD set)
;   chip    -  Blue (1), Green (2) OR Red (3) chip
;   [exp]   -  Exposure frames (e.g. [0L, 1L])
;
; RETURNS:
;
; OUTPUTS:
;   Fills up the 1D spectral tags in the Object structure.
;
; OPTIONAL KEYWORDS:
;   MSKTRIM -- Number of pixels to trim off the edge of the slit when
;              defining the object profile
;   /OCHK   - Plot the extracted flux (optimal) for each order
;   /RESCHK - Check the residuals of the 2D, fully extracted image
;   /STD    - Set extraction parameters for a standard star
;   /DEBUG  - Stop within extraction routine to check stuff
;   /SKIPSKYSUB - Perform extraction on the processed but NOT sky
;                 subtracted image.
;   LOWSNR = - Value of SNR^2 of the data for a given order which when
;   HIGHSNR= - Value of SNR^2 of the data for a given order which when
;             exceeded mike_box uses an additional parameter for the
;             profile shape.  (Default:  500 corresponding to SNR=22)
;             Lowering this parameter may improve extraction.
;   ORDRS   - Input array of physical order numbers to extract
;             [start,end]
;   /SCTCHK - Show the scattered light image
;   /BBOX   - Recommended for bright obj with short slits
;   PROF_LIM= -  Mininum fraction of profile covered that does not
;               include CR hits [default:0.7]
;   SCATTTRIM=  -  Trimming on orders for scattered light [default=1]
;   /NOVAC  - Turn off vacuum wavelengths
;   INPGAU= -- Inputted Gaussian profile for extraction (for very low
;             SNR data)
;   /READSKY --  Use the saved sky model from hires_skysub
;   /NOSCATT -- Do not perform a scattered light subtraction
;   /NOMEDSCATT -- Turn off the default of using the median scattered
;                  light level for the full image
;   /NOPROF  -- Do not fit profile to the data (for boxcar only)
;   XBK_SCATT= -- Number of coefficients to use in x direction for
;                scattered light subtraction.
;   /EXFLAT  -- Extract the flat (useful for blaze normalization).
;               The output is written into flux array
;   /ADD_BOX  -- Do a boxcar extraction *in addition* to an optimal
;                extraction.  The output goes into the box array
;
; Optional OUTPUTS:
;
; COMMENTS:
;  The program extracts the orders in order of decreasing SNR.  If the
;  SNR is lower than lowsnr (default: 2.49) then the optimal
;  extraction is performed using the profile parameters from the
;  previous order(s).
;
; EXAMPLES:
;   hires_extract, hires, 1L, [0L]
;
; PROCEDURES/FUNCTIONS CALLED:
;  x_echextobj
;  extract_boxcar
;  smooth_mask
;  bspline_extract
;
; REVISION HISTORY:
;   26-Aug-2003 Written by SMB
;-
;------------------------------------------------------------------------------
;---------------------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro hires_extract, hires, setup, obj_id, chip, exp, STD=std, $
                   MSKTRIM=msktrim, $
                   ORDRS=ordrs, HIGHSNR=highsnr, INPGAU=inpgau, $
                   readsky=readsky, _EXTRA=EXTRA, MODELOUT=modelout, $
                   BOXCAR=boxcar, NOHELIO=nohelio, NOSCATT=noscatt, $
                   NOVAC=novac, NOMEDSCATT=nomedscatt, SKIPSKYSUB=skipskysub,$
                   SCTCHK=sctchk, NOPROF=noprof, BBOX=bbox, $
                   USE_EXMASK=USE_EXMASK, PROF_LIM=prof_lim, $
                   SCATTTRIM=scatttrim, XBK_SCATT=xbk_scatt, $
                   ARC_XSHFT=arc_xshft, EXFLAT=exflat, EXARC=exarc, $
                   TRY_IVAR=try_ivar, ADD_BOX=add_box

;
  if  N_params() LT 4  then begin 
      print,'Syntax - ' + $
        'hires_extract, hires, setup, obj_id, chip, [exp], ' + $
        '/DEBUG, /CHK, /OCHK, /RESCHK, /BOXCAR, /READSKY, ORDRS=,'
      print, '          /STD, HIGHSNR=, /SCTCHK, /BBOX, [v1.1]'
      print, '          PROF_LIM=, /ADD_BOX'
      return
  endif 
  
;  Optional Keywords
  if not keyword_set( PROF_LIM ) then prof_lim = 0.7
  if not keyword_set( MIN_CUT ) then min_cut = 0.5
  if not keyword_set( OMSKTRIM ) then omsktrim = -1.5
  if not keyword_set( REJSIG ) then rejsig = 7.
  if not keyword_set( uniformsky ) then uniformsky = 0
  if not keyword_set(MSKTRIM) then MSKTRIM = -1.5
  if keyword_set( STD ) then begin
      BOXCAR = 1
      NOMEDSCATT = 1
      radius = 40L
  endif
  if keyword_set( BBOX ) then begin
      BOXCAR = 1
      NOMEDSCATT = 1
      SKIPSKYSUB = 1
      EXTENBOX = 1
      NOPROF = 1
   endif

  if not keyword_set(VEL_NATIVE) then vel_native = 1.3d  ;; Width of a native pixel in km/s

;  Find all relevant obj
  indx = where(hires.flg_anly NE 0 AND hires.setup EQ setup AND $
               hires.chip EQ chip[0] AND hires.obj_id EQ obj_id AND $
               (strtrim(hires.type,2) EQ 'OBJ' OR $ 
                strtrim(hires.type,2) EQ 'STD'), nindx)
  
  if nindx EQ 0 then begin
      print, 'hires_extract: No images to find obj for!', obj_id
      return
  endif

  ;;  Exposures
  if size(exp,/type) EQ 0 then exp = lindgen(nindx)

;  Read in order structure
  ordr_str = hires_getfil('ordr_str', setup, chip=chip[0])
  nordr = n_elements(ordr_str)
  sv_ostr = ordr_str

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Loop

  for q=0L,n_elements(exp)-1 do begin

      thisone = indx[exp[q]]

      ;; Offset ordr_str
      ordr_str = sv_ostr
      ordr_str.lhedg = ordr_str.lhedg +  hires[thisone].arc_xyoff[0]
      ordr_str.rhedg = ordr_str.rhedg +  hires[thisone].arc_xyoff[0]
      ordr_str.lhc[0] = ordr_str.lhc[0] +  hires[thisone].arc_xyoff[0]
      ordr_str.rhc[0] = ordr_str.rhc[0] +  hires[thisone].arc_xyoff[0]

;      rnd_edg = [[round(ordr_str.lhedg)], [round(ordr_str.rhedg)]]
;      sz = size(rnd_edg, /dimensions)
;      rnd_edg = reform(rnd_edg,sz[0],nordr,2)

      print, 'hires_extract: Reading files...'

      ;;;;;;;;;;;;;;
      ;; Open Obj file
      objfil = hires[indx[exp[q]]].obj_fil
      if x_chkfil(objfil+'*') EQ 0 then begin
          print, 'hires_extract: No Obj file! ', objfil, ' Skipping...'
          stop
          continue
      endif
      objstr = xmrdfits(objfil, 1, STRUCTYP='hiresobjstrct', /silent)
    
      ;;;;;;;;;;;;;;
      ;; SKY SUB Fil 
      imgfil =hires_getfil('fin_fil', /name, CHIP=hires[thisone].chip,$
                           CHKFIL=chkf, $
                           FRAME=hires[thisone].frame)
      if x_chkfil(imgfil+'*') EQ 0 then begin
          print, 'hires_extract: No Image file!  Returning...'
          return
      endif
      print, 'hires_extract: Image file -- ', imgfil
      head = xheadfits(imgfil)
      img = xmrdfits(imgfil, 0, /silent) ; non-SKY subtracted
      sciivar = xmrdfits(imgfil, 1, /silent)
      sz_img = size(sciivar, /dimensions)


      ;; Subtact scattered light
      scatt_file =hires_getfil('scatt_fil', CHIP=hires[thisone].chip,$
                           CHKFIL=chkf, /name, $
                           FRAME=hires[thisone].frame)
      scatt_img = 0.
      if (chkf EQ 1) AND not keyword_set(NOSCATT) then scatt_img = xmrdfits(scatt_file,/silent) $
      else begin
          if not keyword_set( NOSCATT ) then begin
              scatt_img = hires_fitscatt(img, sciivar, ordr_str, chip, $
                                         NOMEDSCATT=nomedscatt, $
                                         SCATTTRIM=scatttrim, $
                                         NXBKPT=XBK_SCATT)
          endif
          if keyword_set(SCTCHK) then begin
              xatv, scatt_img, /bloc
              xatv, img - scatt_img, /bloc
              stop
          endif
       endelse
      img_sub = img - scatt_img

      if keyword_set(SKIPSKYSUB) then skysub = img_sub $
      else skysub = xmrdfits(imgfil, 2, /silent) ; SKY subtracted

      ;; IVAR CORRECTION
      if keyword_set(TRY_IVAR) then begin
          readno = 3.0
          new_ivar = 1.0/((img_sub-7)>0. + 7 + readno^2)
          sciivar = new_ivar
      endif

      ;; Need var for most of my routines
;      var = 1./(ivar + (ivar EQ 0)) * (ivar GT 0)
      
      if (size(skysub))[0] EQ 0 then begin
        print, 'No skysubtracted image found'
        print, 'Either run hires_skysub or hires_extract with the ' + $
          '/skipskysub option'
        continue
      endif

      ;;  Read Arc
      img_arc = 0
      hel_corr = 1.0d
      helio = 0.
      arc_img = strtrim(hires[indx[exp[q]]].arc_img,2)
      print, 'hires_extract: Arc -- ', arc_img
      if x_chkfil(arc_img+'*') EQ 0 then begin
          print, 'hires_extract: No Arc file!  Returning...', arc_img
          return
      endif
      img_arc = xmrdfits(arc_img, /silent) 
      
      ;; Shift arc?
      rnshft = round(hires[thisone].arc_xyoff[0])
      if rnshft NE 0 then begin
          print, 'hires_extract:  Shifting the Arc by ', rnshft, ' pixels'
          ;; Shift
          img_arc = shift(img_arc, rnshft, 0)
          ;; Zero out edge
          if rnshft GT 0 then img_arc[0:rnshft-1,*] = 0. $
          else img_arc[sz_arc[0]-rnshft:*,*] = 0.
      endif

          

      ;; Vacuum
      if not keyword_set( NOVAC ) then begin
          print, 'hires_extract: Converting to vacuum wavelengths'
          a = where(img_arc GT 0.)
          tmpaimg = 10^img_arc[a]
          airtovac, tmpaimg
          img_arc[a] = temporary(alog10(tmpaimg))
      endif
      
      ;;  HELIO correction
      x_radec, hires[indx[exp[q]]].ra, hires[indx[exp[q]]].dec, radeg, decdeg 
      ;; JXP Removed the -1 (3/21/05)  ;; Put back!! (5/30/05)
      if not keyword_set(NOHELIO) then begin
          helio = (-1.)*x_keckhelio(radeg, decdeg, $
                                    hires[indx[exp[q]]].equinox, $
                                    jd=hires[indx[exp[q]]].date)
          print, 'hires_extract: heliocentric correction :', $
                 helio,' km/s' , format='(a,f8.3,a)'
          hel_corr = sqrt( (1.d + helio/299792.458d) / $
                           (1.d - helio/299792.458d) )
          img_arc = img_arc + alog10(hel_corr)
      endif
      sxaddpar, head, 'HELIO', helio
  
      ;; Check for Original sky fit
      if keyword_set( readsky ) then begin
          skyfil =hires_getfil('sky_fil', /name, CHIP=hires[thisone].chip,$
                               CHKFIL=chkf, $
                               FRAME=hires[thisone].frame)
      endif         
      
      ;; 
      print, systime()
  
      ;;
      velpix = VEL_NATIVE * hires[indx[exp[q]]].rowbin

      ;; Slit length
      slit_len = hires_slitlen(hires[thisone].decker)

      ;; QAFIL
      qafil = hires_getfil('qa_extract', setup, FRAME=hires[thisone].frame, $
                          CHIP=hires[thisone].chip)

      ;; Flat (for blaze normalization)
      if keyword_set(EXFLAT) then begin
          flat_img = hires_getfil('qtz_fil', setup, CHIP=hires[thisone].chip, $
                                 FIL_NM=flat_fil)
          flat_ivar = xmrdfits(flat_fil, 1)
          flat_var = 1./flat_ivar
      endif

      ;; Extract Arc?
      if keyword_set(EXARC) then begin
          ipos = strpos(arc_img, '_I')
          sv_arc = xmrdfits(strmid(arc_img,0,ipos)+'.fits')
      endif

      ;; For debugging
;      ordr_str = ordr_str[0:2]

      ;; Get profile and Sky sub image 
      ;;  This routine also tweaks up the trace
      if not keyword_set(GAUSS) and not keyword_set(NOPROF) then begin

          ;;  This routine also extracts a box car but NOT on the
          ;;  wavelength grid that is desired
          x_extechopt, img_sub, skysub, sciivar, ordr_str, objstr, velpix, $
                           chk=chk, SLIT_LEN=slit_len, img_arc=img_arc, $
                           HIGHSNR=highsnr, SCATT_IMG=scatt_img, $
                           base_aper=objstr[0].aper, MIN_CUT=min_cut, $
                           ORDRS=ordrs, skyfil=skyfil, BOXONLY=onlybox, $
                           helio=alog10(hel_corr), EXTENBOX=extenbox, $
                           RDNOISE=hires[indx[exp[q]]].readno, $
                           OBJ_NAME=hires[indx[exp[q]]].Obj, $
                           SKIPSKYSUB=skipskysub, $
                           _EXTRA=EXTRA, ORDERMASK = ordermask, MSKTRIM=MSKTRIM, $
                           EXTRACT_MASK=extract_mask, $
                           MODEL_SKY=model_sky, MODEL_PROF=model_prof, $
                           MODEL_IVAR=model_ivar
      
          ;; Optional Image 
          if keyword_set(modelout) then begin
              modelfil = strmid(objfil,0,rstrpos(objfil,'.fits')) $
                + '-Model.fits'
              mwrfits,  img_arc, modelfil, head, /create, /silent
              mwrfits,  ordermask, modelfil, /silent
              mwrfits,  model_prof, modelfil, /silent
              mwrfits,  model_obj, modelfil, /silent
              mwrfits,  model_sky, modelfil, /silent
              mwrfits,  model_ivar, modelfil, /silent
              spawn, 'gzip -f '+modelfil
          endif

          ;; Undegrade
;          if keyword_set(DEGRADE) then ivar = ivar * DEGRADE
          
          ;; Ouptut Spectra
          print, 'hires_extract: Outputting boxcar spectrum-- ', objfil
          mwrfits, objstr, objfil, /create, /silent
          spawn, 'gzip -f '+objfil
          
          ;; Output the image
          print, 'hires_extract: Outputting skysub image-- ', objfil
;          hires_taghead, head
          mwrfits, img, imgfil, head, /create
          mwrfits, sciivar, imgfil  ;; Consider multiplying by extract_mask
          mwrfits, skysub, imgfil
          mwrfits, img_sub - model_sky, imgfil
          skysub = img_sub - model_sky  ;; Final sky-subtracted image
          spawn, 'gzip -f '+imgfil
      endif

;      restore, 'debug16B.idl'

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; EXTRACT
      ;; Variance
      if keyword_set(MODEL_IVAR) then ivar = model_ivar else ivar = sciivar
      var = 1./(ivar + (ivar EQ 0)) * (ivar GT 0)

      ;; Mask CRs from the Profile fitting (not currently recommended)
      if keyword_set(EXTRACT_MASK) AND keyword_set(USE_EXMASK) then $
        var = var * extract_mask

      ;; Mask
      if keyword_set(EXTENBOX) then OMSKTRIM = 2
      ordermask = x_ordermask(sz_img[0], sz_img[1], ordr_str, trim=OMSKTRIM)
      ;; Loop on orders
      svap = fltarr(50,100,3)
      svnp = lonarr(50)
      svsig = fltarr(50)

      IORDR = 0L
;      IORDR = 11L
      for qq=IORDR,nordr-1 do begin
          print, 'hires_extract: Extracting order ', $
                 string(ordr_str[qq].order, FORMAT='(i3)')

          ;; Setup the image
          slit_length = ordr_str[qq].rhedg - ordr_str[qq].lhedg
          half_length = median(slit_length)/2.  ;; Pixels
          expix = round((objstr[0].aper-1) $
                        * half_length  + 3) > 0 ;; Pixels beyond slit edge
          lhs= round(ordr_str[qq].lhedg-expix[0]) > 0L
          rhs= round(ordr_str[qq].rhedg+expix[1])  < (sz_img[0]-1)
          a = where(lhs LE rhs, na)
          mni = min(round(lhs))
          mxi = max(round(rhs))
          msk = lonarr(sz_img[0],sz_img[1])
          for ii=0L,na-1 do begin
              j = a[ii]
              msk[lhs[j]:rhs[j],j] = 1
          endfor
          ;; Trim the edges of each order
          if not keyword_set(EXTENBOX) then begin
              bad = where(ordermask NE ordr_str[qq].order)
              msk[bad] = 0
          endif
          
          ;; Transpose
          timg = transpose(skysub[mni:mxi,*]*msk[mni:mxi,*])
          tarc = 10.^transpose(img_arc[mni:mxi,*]*msk[mni:mxi,*])
          tvar = transpose(var[mni:mxi,*]*msk[mni:mxi,*])
          if keyword_set(model_sky) then $
            tsky = transpose(model_sky[mni:mxi,*]*msk[mni:mxi,*])

          ;; Flat
          if keyword_set(EXFLAT) then begin
              flt_img = transpose(flat_img[mni:mxi,*]*msk[mni:mxi,*])
              flt_var = transpose(flat_var[mni:mxi,*]*msk[mni:mxi,*])
              szf = size(flt_var,/dimen)
              flt_sky = replicate(1., szf[0], szf[1])
          endif
              
          ;; Arc
          if keyword_set(EXARC) then begin
              arc_img = transpose(sv_arc[mni:mxi,*]*msk[mni:mxi,*])
              sza = size(arc_img,/dimen)
              arc_var = replicate(1., sza[0], sza[1])
              arc_sky = replicate(1., sza[0], sza[1])
          endif
              
          
          ;; Profile
          if keyword_set(MODEL_PROF) then begin
              tprof = transpose(model_prof[mni:mxi,*]*msk[mni:mxi,*])
              ;; 
              if max(tprof) LT 0.001 then begin
                  continue
                  print, 'hires_extract: No profile.  Skipping..'
              endif
          endif
          
          ;; Primary only
          sci = where(strtrim(objstr.obj_id,2) EQ 'a' AND $
                      objstr.order EQ ordr_str[qq].order, $
                      COMPLEMENT=b, NCOMPLEMENT=nb)

          ;; Wave limits
          gdwv = where(ordermask EQ ordr_str[qq].order and $
                       img_arc GT 1.)
          mnwv = min(img_arc[gdwv], max=mxwv)
          loglam = alog10(1.0d + velpix / 299792.458d)
          wave0  = alog10(3000.0d)
          loglam0 = (long((mnwv - wave0)/loglam) - 5) * loglam + wave0 ; -2 

          ;; Npix
          npix = round(4000. / hires[indx[exp[q]]].rowbin) 
          logwvarr = dindgen(npix) *loglam + loglam0
          wvarr = 10.^logwvarr
              
          if not keyword_set(BOXCAR) then begin
              ;; x optimal extraction 
              x_extobjopt, timg, tarc, tvar, tsky, $
                [objstr[sci].xcen, objstr[sci].ycen-mni], fin_spec, $
                APER=aper, DEBUG=debug, COLLMNX=[3000., 1e4], $
                CRVAL1=loglam0, CDELT=loglam, NPIX=npix, $
                TOT_TRC=objstr[sci].trace[0:sz_img[1]-1]-mni, $
                REJSIG=rejsig, /REBINC, RADIUS=radius, $
                READNO=hires[indx[exp[q]]].readno, DEFGAU=defgau, $
                USEGAU=INPGAU, MAXGAU=maxgau, CHK=chk, APSTRCT=apstrct, $
                REDBLUE=0L, PROFILE=tprof, PROF_LIM=PROF_LIM, GROW=grow

              if keyword_set(INPGAU) then defgau = inpgau
                  
              ;; QA  (GAUSSIAN Profile)
              if not keyword_set(MODEL_PROF) then begin
                  np = n_elements(apstrct.ax)
                  svap[qq,0:np-1,0] = apstrct.ax
                  svap[qq,0:np-1,1] = apstrct.ay
                  svap[qq,0:np-1,2] = apstrct.gfit
                  svnp[qq] = np
                  svsig[qq] = apstrct.gcoeff[2]
                  ;; Help with Gaussian
                  if qq EQ IORDR then svgau = defgau $
                  else begin
                      maxgau = (defgau/svgau + 0.2)*defgau
                      svgau = defgau
                  endelse
              endif

              if keyword_set(MODEL_PROF) then $
                objstr[sci].flg_optimal = 3 $ ;; Profile
              else objstr[sci].flg_optimal = 2  ;; Gaussian optimal

              ;; Extract flat
              if keyword_set(EXFLAT) then begin
                  x_extobjopt, flt_img, tarc, flt_var, flt_sky, $
                               [objstr[sci].xcen, objstr[sci].ycen-mni], $
                               fin_spec2, $
                               APER=aper, DEBUG=debug, COLLMNX=[3000., 1e4], $
                               CRVAL1=loglam0, CDELT=loglam, NPIX=npix, $
                               TOT_TRC=objstr[sci].trace[0:sz_img[1]-1]-mni, $
                               REJSIG=rejsig, /REBINC, RADIUS=radius, $
                               READNO=hires[indx[exp[q]]].readno, $
                               DEFGAU=defgau, NCRITER=0L, $
                               USEGAU=INPGAU, MAXGAU=maxgau, CHK=chk, $
                               APSTRCT=apstrct, $
                               REDBLUE=0L, PROFILE=tprof, PROF_LIM=PROF_LIM
              endif

              ;; Extract Arc
              if keyword_set(EXARC) then begin
                  x_extobjopt, arc_img, tarc, arc_var, arc_sky, $
                               [objstr[sci].xcen, objstr[sci].ycen-mni], $
                               fin_spec3, $
                               APER=aper, DEBUG=debug, COLLMNX=[3000., 1e4], $
                               CRVAL1=loglam0, CDELT=loglam, NPIX=npix, $
                               TOT_TRC=objstr[sci].trace[0:sz_img[1]-1]-mni, $
                               REJSIG=rejsig, /REBINC, RADIUS=radius, $
                               READNO=hires[indx[exp[q]]].readno, $
                               DEFGAU=defgau, NCRITER=0L, $
                               USEGAU=INPGAU, MAXGAU=maxgau, CHK=chk, $
                               APSTRCT=apstrct, $
                               REDBLUE=0L, PROFILE=tprof, PROF_LIM=PROF_LIM
              endif
          endif else begin
              ;; BOXCAR
              oaper = objstr[sci].aper * half_length  ;; Pixels
              x_extobjbox, timg, tarc, $
                           [objstr[sci].xcen, objstr[sci].ycen-mni], $
                           fin_spec, $
                           VAR=tvar, WVMNX=10^[mnwv, mxwv], $
                           APER=oaper, DEBUG=debug, COLLMNX=[3000.,1e4], $
                           CRVAL1=loglam0, CDELT=loglam, NPIX=npix, /REJ_CR, $
                           TOT_TRC=objstr[sci].trace[0:sz_img[1]-1]-mni, $
                           REJSIG=rejsig, /REBINC, RADIUS=radius, $
                           CHK=chk
              ;; Flag
              objstr[sci].flg_anly = 1
           endelse



          ;; Write to structure
          if fin_spec.npix NE 0 then begin
              objstr[sci].npix = fin_spec.npix
              objstr[sci].wave[0:fin_spec.npix-1] = fin_spec.wv
              objstr[sci].fx[0:fin_spec.npix-1] = fin_spec.fx
              objstr[sci].var[0:fin_spec.npix-1] = fin_spec.var
              objstr[sci].novar[0:fin_spec.npix-1] = fin_spec.novar
;              objstr[sci].novar[0:fin_spec.npix-1] = fin_spec.novar
;              objstr[sci].sky[0:fin_spec.npix-1] = fin_spec.sky
              if keyword_set(APSTRCT) then begin
                  if n_elements(apstrct.gcoeff) GT 1 then $
                    objstr[sci].opt_sigma = apstrct.gcoeff[2]
              endif
              ;; Aper
;                          objstr[sci].aper = fin_spec.aper
              ;; Add BOXCAR?
              if keyword_set(ADD_BOX) then begin
                 oaper = objstr[sci].aper * half_length ;; Pixels
                 x_extobjbox, timg, tarc, $
                              [objstr[sci].xcen, objstr[sci].ycen-mni], $
                              box_spec, $
                              VAR=tvar, WVMNX=10^[mnwv, mxwv], $
                              APER=oaper, DEBUG=debug, COLLMNX=[3000.,1e4], $
                              CRVAL1=loglam0, CDELT=loglam, NPIX=npix, /REJ_CR, $
                              TOT_TRC=objstr[sci].trace[0:sz_img[1]-1]-mni, $
                              REJSIG=rejsig, /REBINC, RADIUS=radius, $
                              CHK=chk
                 objstr[sci].box_wv[0:box_spec.npix-1] = box_spec.wv
                 objstr[sci].box_fx[0:box_spec.npix-1] = box_spec.fx
              endif
              ;; Flag
              objstr[sci].flg_anly = 1
              ;; Flat (for blaze normalization)
              if keyword_set(EXFLAT) then $
                objstr[sci].flux[0:fin_spec2.npix-1] = fin_spec2.fx
              ;; Arc
              if keyword_set(EXARC) then $
                objstr[sci].sig[0:fin_spec3.npix-1] = fin_spec3.fx
          endif else objstr[sci].flg_anly = 0
      endfor
              
      ;; QA  (GAUSSIAN ONLY)
      if keyword_set(GAUSS) then begin
          x_psopen, qafil, /maxs
          clr = getcolor(/load)
          !p.multi = [0,5,3]
          for qq=IORDR,nordr-1 do begin
              np = svnp[qq]
              if np GT 1 then begin
                  plot, svap[qq,0:np-1,0], svap[qq,0:np-1,1], $
                    color=clr.black, background=clr.white, psym=10, $
                    charsize=1.3, xmargin=[0.05, 0.95], ymargin=[0.15,0.95]
                  oplot, svap[qq,0:np-1,0], svap[qq,0:np-1,2], color=clr.red
                  ;;
                  xyouts, svap[qq,2,0], max(svap[qq,*,2])*0.8, 'Sig = '+$
                    string(svsig[qq],format='(f5.2)'), charsize=1.1, $
                    color=clr.black
                  xyouts, svap[qq,2,0], max(svap[qq,*,2])*0.5, 'Order = '+$
                    string(ordr_str[qq].order,format='(i4)'), charsize=1.1, $
                    color=clr.black
              endif
          endfor
          x_psclose
          !p.multi=[0,1,1]
          spawn, 'gzip -f '+qafil
      endif

      print, systime()

      ;; Ouptut Spectra
      print, 'hires_extract: Output spectrum in -- ', objfil
      mwrfits, objstr, objfil, /create, /silent
      spawn, 'gzip -f '+objfil
        
  endfor
  
;  DONE
  print, 'hires_extract: All done! '
  return
end

