;+ 
; NAME:
; esi_echfndobj   
;     Version 1.1
;
; PURPOSE:
;    Finds all objects and traces them
;
; CALLING SEQUENCE:
;   
;  esi_echfndobj, esi, obj_id, [exp], REFWV=, SCICLM=, PEAKTHRESH=,
;  REFORDR=, /INTER, /STD, /CHK, MAXOFF=, /NOCLOB
;
; INPUTS:
;   esi     -  ESI structure
;   obj_id  -  Object ID  (e.g. 0L, 1L, etc)
;   [exp]   -  Exposure frames (e.g. [0L, 1L])
;
; RETURNS:
;
; OUTPUTS:
;
; OPTIONAL KEYWORDS:
;  /STD     - Find object for standard star
;  /CHK     - Show overall trace
;  FITFIL=  - Map of pinholes (default: Maps/hole_fit.idl )
;  REFWV=   - Reference wavelength region (default: [5600., 5610.] )
;  REFORDR= - Reference order  (default: 4L)
;  /INTER   - Interactively identify objects and set apertures
;  MAXOFF=  - Minimum offset between max obj and center of slit
;             (default: 20.)
;  APERV=   - Aperture value (for masking sky)  [default: 10]
;  SCICLM=  - Guess at the position of the object relative to center
;  PEAKTHRESH = Fraction of brightest object on the slit to trace. 
;  ABSTHRESH  = Absolute threshold in units of peakflux to trace. 
;  MINPEAK  -- Absolute threshold required to be a prospective object
;              (default = 0.2)

; OPTIONAL OUTPUTS:
;
; COMMENTS:
;
; EXAMPLES:
;   esi_echfndobj, esi, 0L, [0L, 1L], /CHK, /INTER, 
;      REFWV=[6500., 6520.], REFORDR=5L
;
;
; PROCEDURES/FUNCTIONS CALLED:
;
; REVISION HISTORY:
;   22-Jul-2002 Written by JXP
;   03-Feb-2003 Polished (JXP)
;-
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
; Create the return structure for NOBJ objects
function fire_obj_create, nobj, slitid=slitid, ny=ny

   objstr = create_struct( $
    'OBJID' , 0L, $
    'SLITID', 0L, $
    'XFRACPOS', 0.0, $
    'PEAKFLUX', 0.0, $
    'MASKWIDTH', 0.0, $
    'FWHM', 0.0, $
    'FLX_SHFT_WAV', 0.0, $
    'FLX_SHFT_SPA', 0.0, $                   
    'FWHMFIT'  , fltarr(ny), $
    'XPOS'  , fltarr(ny), $
    'YPOS', findgen(ny), $
    'HAND_AP', 0L, $
    'HAND_X', 0.0, $
    'HAND_Y', 0.0, $
    'HAND_FWHM', 0.0)                        
;    'XPOSIVAR'  , fltarr(ny), $
     if (keyword_set(slitid)) then objstr.slitid = slitid
   if (keyword_set(nobj)) then objstr = replicate(objstr, nobj)

   return, objstr
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION fire_findobj, IMG_MINSKY=img_minsky, SCIIVAR=sciivar,$
                       WAVEIMG=waveimg, TSET_SLITS=tset_slits, $
                       DATA=data, GENERATE=generate, STD=std, $
                       FILSTD = FILSTD, $
                       APERV = aperv, APERS = apers, $
                       CHECK_INPUTS=check_inputs, $
                       NFIND = NFIND, PEAKTHRESH = PEAKTHRESH, $
                       FWHM = FWHM, $
                       ABSTHRESH = ABSTHRESH, MIN_SN=MIN_SN, $
                       CHK=chk, VERBOSE=verbose, INTER=inter, $
                       BOX_RAD=box_rad, TITLE=title, PROF_IMG=prof_img


  func_name = "fire_findobj"
  print, ''
  print, func_name + ": commencing fire_findobj..."
  
  sciivarfile = "Not read in: fed via keyword."
  waveimgfile = "Not read in: fed via keyword."
  tset_slitsfile = "Not read in: fed via keyword."
  
  if keyword_set(DATA) then begin
     if NOT keyword_set(img_minsky) then begin
        if keyword_set(GENERATE) then begin
           fire_siren, func_name + ": WARNING! Image minus sky not input!  Running fire_skymodel and creating this because /GENERATE was passed.  Make sure this is what you wanted..."
           skyimage = fire_skymodel(DATA=data, SCIIMG=sciimg, SCIIVAR=sciivar, STD=std, VERBOSE=verbose)
           img_minsky = sciimg - skyimage	
        endif else begin
           fire_siren, func_name + ": WARNING! Image minus sky not input!  Keyword /GENERATE not passed, so exiting with error."
           RETURN, 1
        endelse
     endif
     if NOT keyword_set(sciivar) then begin
        print, func_name, ": Generating sciivar..."
        sciimg = fire_get(data, /SCIIMG, STD=std, SCIIVAR=sciivar, VERBOSE=verbose, FILEREAD=sciivarfile)
     endif
     if NOT keyword_set(waveimg) then begin
        print, func_name, ": Generating waveimg..."
        waveimg = fire_get(data, /WAVEIMG, STD=std, VERBOSE=verbose, FILEREAD=waveimgfile)
        waveimg = 10^waveimg
     endif
     if NOT keyword_set(tset_slits) then begin
        print, func_name, ": Generating tset_slits..."
        tset_slits = fire_get(data, /tset_slits, VERBOSE=verbose, FILEREAD=tset_slitsfile)
     endif
  endif
  
  if NOT keyword_set(DATA) AND $
     ( NOT keyword_set(IMG_MINSKY) OR NOT keyword_set(SCIIVAR) OR NOT keyword_set(WAVEIMG) OR NOT keyword_set(TSET_SLITS) ) then begin
     fire_siren, func_name + ": invalid input to fire_findobj!  If keyword DATA is not passed, then the keywords IMG_MINSKY, SCIIVAR, WAVEIMG, and TSET_SLITS must be passed!  Exiting with error!"
     RETURN, 1
  endif
  
  if keyword_set(VERBOSE) OR keyword_set(CHECK_INPUTS) then begin
     print, ''
     print, "++++++++++++++++++++++++++++++++++++"
     print, func_name, ": /VERBOSE flag passed.  Set values:"
     print, "Ran fire_proc to obtain variance on file: ", sciivarfile
     print, "Read in wavelength image from file: ", waveimgfile
     print, "Read in tset_slits from file: ", tset_slitsfile
     print, "++++++++++++++++++++++++++++++++++++"
     print, ''
     if keyword_set(CHECK_INPUTS) then RETURN, 0
  endif
  
;;  Optional Keywords
  if not KEYWORD_SET(NORDERS) THEN NORDERS=21
  if not keyword_set( APERV ) then aperv = 10. 
  if not keyword_set( APERS ) then apers = 20. 
  IF NOT KEYWORD_SET(NFIND) THEN NFIND=1L
  cbin=1.0d
  
  alphabet = ['a', 'b', 'c', 'd', 'e', 'f','g','h','i'] ;; I forget the rest...
  
  dim = size(img_minsky, /dim)
  nx = dim[0]
  ny = dim[1] 
  
  slit_edg = fltarr(ny, norders, 2)
  traceset2xy, tset_slits[0], rows, left_edge
  traceset2xy, tset_slits[1], rows, right_edge
  slit_edg[*, *, 0] = left_edge
  slit_edg[*, *, 1] = right_edge
  ordermask = fire_ordermask(tset_slits, /fsr)
  IF KEYWORD_SET(FILSTD) THEN BEGIN
     IF x_chkfil(filstd+'*') EQ 0 then begin
        print, 'fire_findobj: STD Obj file does not exist or' + $
               ' obj_fil tag not set! Check it..', filstd
        return,0
     ENDIF ELSE stdstr = xmrdfits(filstd, 1, /silent)
     gdtrc = stdstr.trace[0:ny-1L,*]
  ENDIF ELSE gdtrc = (slit_edg[*, *, 0] + slit_edg[*, *, 1])/2.0d

  if (keyword_set(INTER)) then begin

     ; This is for User-interctive/forced definition of the aperture.
     ; Only one trace/aperture is allowed at a time, not multiple ones.
     ; May want to go back and fix this at a later time.
     
     xatv_xy, img_minsky, wv=waveimg, min=-100, max=100, tset_slits=tset_slits, $
              retx=xx, rety=yy, box_rad=box_rad, TITLE=title, prof_fitstr=prof_fitstr, /block
     

     objstruct = long_obj_create(norders, slitid=1, ny=ny)
     objstruct.objid  = 21 - indgen(21)
     objstruct.slitid = 1 + indgen(21)

     if (is_undefined(prof_fitstr) EQ 0) then begin
        ; This only if the user has fit a function to the profile,
        ; then we must create a profile image to propagate into the 
        ; extration part of the pipeline.
        ximg    = long_slits2x(tset_slits)
        inord   = where(ximg NE 0)
        profimg = ximg * 0.0
        print, "Making the profile image..."
        profimg[inord] = x_calcfit(ximg[inord], FITSTR=prof_fitstr)
        print, "Done."
        for iord=0, norders-1 do begin
           width = median(right_edge[*,iord]-left_edge[*,iord])
           xtmp = findgen(width)
           prof1d = x_calcfit(xtmp/width, FITSTR=prof_fitstr)
           offset = xtmp[where(prof1d EQ max(prof1d))]
           objstruct[iord].xpos = (left_edge[*,iord] + offset[0]) > 0
           objstruct[iord].ypos = findgen(2048)

           ihigh=offset
           while (ihigh LT width-1 AND prof1d[ihigh] GT 0.5 * max(prof1d)) do ihigh++
           ilow=offset
           while (ilow GT 1 AND prof1d[ilow] GT 0.5 * max(prof1d)) do ilow--
           fwhm_prof = xtmp[ihigh]-xtmp[ilow]
           objstruct[iord].fwhm = fwhm_prof
           objstruct[iord].fwhmfit[*] = fwhm_prof
        endfor
     endif else begin
        print, 'box_rad = ', box_rad ;PWS  
        for iord=0, norders-1 do begin
           objstruct[iord].ypos = yy
           objstruct[iord].xpos = xx[*,iord]
        endfor
     endelse
     delvarx, prof_fitstr

     nobj=1

  endif else begin
     ; Automated trace finder.
     ; Multiple objects allowed.
     objstruct = fire_echjoefind(img_minsky, sciivar*(ordermask GT 0) $
                                 , waveimg,gdtrc, slit_edg $
                                 , GFRAC = gfrac, FWHM = FWHM $
                                 , PEAKTHRESH = PEAKTHRESH, NFIND = NFIND $
                                 , ABSTHRESH = ABSTHRESH, MIN_SN=MIN_SN, $
                                 CHK=CHK)

     nobj = n_elements(objstruct)/norders
     if size(objstruct, /type) NE 8 then begin
        fire_siren, func_name + ": ERROR!  Output from fire_echjoefind is not of the correct type!  Exiting with error!"
        return, -1
     endif
     
     IF KEYWORD_SET(NFIND) THEN BEGIN
        peakflux = fltarr(nobj)
        FOR kk = 0L, nobj-1L DO BEGIN
           i1 = kk*norders
           i2 = (norders-1L) +kk*norders
           ;; Avg flux across 15 orders
           peakflux[kk] = total(objstruct[i1:i2].PEAKFLUX)/norders
        ENDFOR
        ;; Sort by average flux and keep nfind brightest objects
        peakind = reverse(sort(peakflux))
        keepind = lonarr(nfind*norders)
        FOR j = 0L, nfind-1L DO BEGIN
           i1 = j*norders
           i2 = (norders-1L) +j*norders
           keepind[i1:i2] = peakind[j]*norders + lindgen(norders)
        ENDFOR
        objstruct = objstruct[keepind]
        nobj = nfind
     ENDIF
     
     ;; Now sort by fractional slit position
     fracpos = fltarr(nobj)
     FOR kk = 0L, nobj-1L DO BEGIN
        i1 = kk*norders
        i2 = (norders-1L)+kk*norders
        ;; Avg flux across 21 orders
        fracpos[kk] = total(objstruct[i1:i2].XFRACPOS)/norders
     ENDFOR
     
     ;; Sort by average flux and keep nfind brightest objects
     fracind = sort(fracpos)
     keepind = lonarr(nobj*norders)
     FOR j = 0L, nobj-1L DO BEGIN
        i1 = j*norders
        i2 = (norders-1L)+j*norders
        keepind[i1:i2] = fracind[j]*norders + lindgen(norders)
     ENDFOR
     objstruct = objstruct[keepind]
     
  endelse

     ;; Create object structures
     
  ; Specify that we want *FIRE* objstrcts, there is some confusion with
  ; other pipelines.
  @use_fire_objstrct  

  tmp = {objstrct}
  tmp2 = objstruct[0]
  dum = {junk:0.0}
  struct_assign, dum, tmp2
  proto = struct_addtags(tmp, tmp2)
  objstr = replicate(proto, nobj*norders)
  objstr.img_fil = ' '
  objstr.UT = ' '
  objstr.field = ' '
  ;;objstr.exp = esi[indx[exp[q]]].exp
  ;;objstr.arc_fil = strtrim(esi[indx[exp[q]]].arc_fil, 2)
  ;; Set aperture

  if (keyword_set(box_rad)) then begin 
     if (n_elements(fwhm_prof) GT 0) then begin
        box_rad = fwhm_prof[0] / 2.35 * 2.0
     endif
     tmp = (n_elements(box_rad) GT 1) ? box_rad[0] : box_rad
     if (tmp GT 0) then begin
        if (n_elements(box_rad) EQ 1) then begin
           objstr.aper = box_rad
           objstr.fwhm = float(box_rad) / 2.0 * 2.35 ; guess users place the aper at 2sigma
        endif else begin
           objstr.aper = [1,1] # box_rad
           objstr.fwhm = (float(box_rad) / 2.0 * 2.35) ; guess users place the aper at 2sigma
        endelse
     endif
  endif else begin
     if keyword_set(STD) then objstr.aper[*] = APERS $
     else objstr.aper[*] = APERV
  endelse
   
;  print, 'aperture = ', objstr.aper ;PWS
;  print, 'fwhm = ', objstr.fwhm ;PWS
 
 for kk = 0L, nobj-1 do begin
     i1 = kk*norders
     i2 = (norders-1L)+kk*norders
     objstr[i1:i2].obj_id = alphabet[kk]
     objstr[i1:i2].order = lindgen(norders) 
     ;;objstr[i1:i2].trace[4999L] = gfrac[kk]
     medj = ny/2
     for jj = 0L,norders-1L do begin
        ;; xcen, ycen
        objstr[i1+jj].xcen = medj
        objstr[i1+jj].ycen = objstruct[i1+jj].xpos[medj]
        ;; Trace
        objstr[i1+jj].trace[0:ny-1L] = objstruct[i1+jj].xpos
        ;; copy over other relevant tags for these indices
        copy_struct_inx, objstruct, objstr, except_tags=["fwhm"] $
                         , index_from = (i1+jj), index_to = (i1+jj)
     endfor
  endfor
  ;; Take only the relevant structures
  objstr = objstr[0:nobj*norders-1]
      
  if keyword_set(CHK) then begin
     nobj = n_elements(objstr)/norders
     tmp = img_minsky*(ordermask GT 0.0)
     FOR iobj = 0L, nobj-1L DO BEGIN
        FOR qq = 0, norders-1L DO BEGIN
           this = where(objstr.obj_id EQ alphabet[iobj] $
                        AND objstr.order EQ qq $
                        , COMPLEMENT = b, NCOMPLEMENT = nb)
           tmp[objstr[this].trace[0:ny-1],lindgen(ny)] = -10000
           tmp[objstr[this].trace[0:ny-1]-objstr[this].aper[0],lindgen(ny)] = -10000
           tmp[objstr[this].trace[0:ny-1]+objstr[this].aper[1],lindgen(ny)] = -10000
;           rnd_trc = round(objstr[this].trace[0:ny-1L])
;           trc_msk = rnd_trc + lindgen(ny)*nx
;           tmp[trc_msk] = -10000
        ENDFOR
     ENDFOR
     xatv, tmp, /block, min = -70, max = 70
  endif
  
;  DONE
  print, 'fire_findobj: All done! '
  print, ''

  if (is_undefined(profimg) EQ 0) then begin
     prof_img = profimg
  endif

  return,objstr

end

