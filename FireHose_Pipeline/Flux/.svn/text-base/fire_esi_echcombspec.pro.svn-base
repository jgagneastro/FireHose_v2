;+ 
; NAME:
; fire_echcombspec
;    Version 1.0
;
; PURPOSE:
;   Combines multiple exposures of the same obj
;    Must be run even on an object with a single exposure
;
; CALLING SEQUENCE:
;   
;   mage_echcombspec, allframes, fspec, orders, keyindex
;
;INPUTS:
;   allobj   -- Array of echspec structures
;   ordrs    -- Orders to coadd 
;   keyindx  -- Exposure to serve as the fiducial for stats
;
; RETURNS:
;
; OUTPUTS:
;   finspec  --  echfspec structure containing the combined spectrum
;;
; OPTIONAL KEYWORDS:
;    /SILENT   - No text output
;    LIST=     - Use an input list to combine (required for multiple
;                night obs).  Output is in 'FSpec/tmp_ech.fits'.
;    OBJ_NM=   - Name of object in slit (a = science)
;    /STD      - Run on a standard star
;    OUTNM=    - Alternative output name for FSpec file
;    ORDRS=    - Orders to combine (default: [0L,9L])
;    CHK       - Various QA for call to long_combspec for combining
;                spectra. 
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;
; EXAMPLES:
;   esi_echcombspec, esi, obj_id, exp_id
;
; PROCEDURES/FUNCTIONS CALLED:
;
; REVISION HISTORY:
;   02-Aug-2002 Written by JXP
;   04-Feb-2003 Polished (JXP)
;   05-Mar-2009 Ported to MAGE by JJBq
;-
;------------------------------------------------------------------------------

pro esi_echcombspec_out, echfspec

  ;; Output to ASCII
  printf, 56, echfspec.field
  for j=0L,echfspec.nexp-1 do begin
      printf, 56, FORMAT='(10x,f7.1,1x,2f10.3,1x,a25)',$
        echfspec.texp[j], $
        echfspec.wvmnx[j,0], $
        echfspec.wvmnx[j,1], $
        echfspec.obj_fil[j]
  endfor
  return
end

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

pro fire_esi_echcombspec, allobj, echfspec, SILENT=silent $
                     , LIST=list $
                     , OBJ_NM = OBJ_NM1, STD = std, OUTNM = outnm $
                     , ORDRS = ordrs , REFO = refo, CHK = CHK $
                     , NOSHIFT = NOSHIFT, IREF = IREF $
                     , FLAG=flag, ERR_MESSAGE=err_message $
                     , USER_WEIGHTS=user_weights

	func_name = 'fire_esi_echcombspec'
	flag = 0
;
  if  N_params() LT 2 then begin 
      print,'Syntax - ' + $
      'x_echcombspec, allobj, finspec, ordrs, /SILENT, ' + $
      'OBJ_NM=, /PATCH, /CHK, /MCHK  [v1.1]'
    flag = 1
    err_message = "incorrect call to " + func_name
    return
  endif 
  
;;I know all the objects already, and the final output structure has
;;been made already


;;Bug fix added by JFH, prevents return of obj_id =99 value which breaks 
;; later calls to esi routines. 
IF n_elements(obj_id1) GT 0 THEN obj_id = obj_id1 
;  Optional Keywords
  if not keyword_set(ORDRS) then ordrs=[11L,31L]  ;removed the orders from the call
  if not keyword_set(REFO) then refo=0L

  if keyword_set( LIST ) then $
    print, 'esi_echcombspec: Combining Obj files from this list -- ', list

; Set exp
;;   if not keyword_set( LIST ) then begin
;;       if not keyword_set( STD ) then begin
;;           allexp = where(esi.type EQ 'OBJ' AND esi.flg_anly NE 0 AND $
;;                          esi.mode EQ 2 AND esi.obj_id EQ obj_id)
;;           if keyword_set(exp_id) then exp = allexp[exp_id] else exp=allexp
;;           nexp = n_elements(exp)
;;       endif else begin  ;; STD
;;           exp = obj_id[0]
;;           nexp = 1
;;           obj_id = 99L
;;       endelse
;;   endif else begin
;;       obj_id = 99L
;;       readcol, list, files, FORMAT='A'
;;       nexp = n_elements(files)
;;    endelse

  nexp = echfspec.nexp
  nspec = echfspec.nexp*21.

  IF KEYWORD_SET(OBJ_NM1) THEN BEGIN
     IF n_elements(obj_nm1) EQ 1 THEN obj_nm =  replicate(obj_nm1, nexp) $
     ELSE IF n_elements(obj_nm1) EQ nexp THEN obj_nm = obj_nm1 $
     ELSE message, 'Invalid size for obj_nm'
  ENDIF ELSE BEGIN
  	  if( nexp EQ 0 ) then begin
  	  		fire_siren, func_name + ": ERROR! nexp==0!  Not allowed!  Exiting without " $
  	  			+ "completing task!"
  	  		flag = 1
  	  		err_message = "nexp==0 in " + func_name
  	  		RETURN
  	  endif
	  obj_nm = replicate('a', nexp)
  ENDELSE

;;;;;;;;;;
; Open Files

  ;; if not keyword_set( SILENT ) AND NOT KEYWORD_SET(LIST) then BEGIN
;;       print, 'esi_echcombspec: Loading up the files...'
;;       forprint, esi[exp].OBJ_FIL, textout = 1
;;   ENDIF

;;; CREATE FINAL 2D ;;;

 ;;  if not keyword_set(LIST) then begin
;;       if keyword_set( OUTNM ) then $
;;          outfil = 'FSpec/'+outnm+obj_nm[0]+'_ech.fits' $
;;       else outfil = 'FSpec/'+strtrim(esi[exp[0]].Obj, 2)+obj_nm[0]+'_ech.fits' 
;;    endif else BEGIN
;;       IF KEYWORD_SET(OUTNM)THEN  outfil = 'FSpec/'+outnm+obj_nm[0]+'_ech.fits' $
;;      ELSE outfil = 'FSpec/tmp_ech.fits' 
;;   ENDELSE

 ; echfspec = { echfspecstrct }

  ;; Copy
  ;; echfspec.nexp = nspec
  
  nspec=nexp
  
  snr_out = fltarr(nspec, ordrs[1]-ordrs[0]+1)

  ;; Zero ordr
  all_zero = where(allobj.order EQ 11, nzero)
  if nzero NE nspec then stop

  ;; Set texp
  for i=0L,nspec-1 do echfspec.texp[i] = allobj[all_zero[i]].exp

  ;; Other tags
  copy_struct, allobj[0], echfspec $
               , EXCEPT = ["wave", "fx", "var", "novar", "sky", "npix"]
  ;; Loop on ORDER (reverse)
  for qq=ordrs[1],ordrs[0],-1 do begin
      all_ordr = where(allobj.order EQ 31-qq)
      ;fill in order numbers
      echfspec.phys_ordr[qq] = qq
      npix = allobj[all_ordr[REFO]].npix
      ;; Coadd
      if nspec EQ 1 then begin 
         echfspec.wave[0:npix-1,qq] = allobj[all_ordr].wave[0:npix-1]
          echfspec.fx[0:npix-1,qq] = allobj[all_ordr].flux[0:npix-1]
          echfspec.var[0:npix-1,qq] = allobj[all_ordr].sig[0:npix-1]^2
          echfspec.novar[0:npix-1, qq] = allobj[all_ordr].nosig[0:npix-1]^2
          echfspec.sky[0:npix-1, qq] = allobj[all_ordr].sky[0:npix-1]
       endif else begin

         print, 'fire_echcombspec: Order ', qq

         ; Pulls fluxes for the current order from all exposures
         influx = allobj[all_ordr].flux[0:npix-1]
;         if (qq EQ 11) then stop

         pos = where(influx GT 0, npos)
         if (npos GT 0) then begin
            medflux = median(influx[where(influx GT 0)])
         endif else begin
            continue
         endelse
         
;;;;;;;;;; Check this hack.. For some reason finding the median
;;;;;;;;;; explicitly causes offsets on combine to 1D, but
;;;;;;;;;; shouldn't hard code in this median.
 ;        stop
         medflux = 1.1e-16
         influx /= medflux
         var   = double(allobj[all_ordr].sig[0:npix-1]^2) / medflux^2
         bd = where(finite(var) EQ 0, nbd)
         if (nbd GT 0) then var[bd] = 0.0
         novar =  double(allobj[all_ordr].nosig[0:npix-1]^2) / medflux^2
         mask = (var GT 0.0 AND (finite(var) NE 0))
         inivar  = mask/(var + (var EQ 0.0))
         innivar = mask/(novar + (novar EQ 0.0))
         inloglam = alog10(allobj[all_ordr[REFO]].wave[0:npix-1])
         insky  =  allobj[all_ordr].sky[0:npix-1] / medflux
;         if (median(inivar) GT 1e6) then begin
;            mivar = median(inivar[where(influx NE 0)])
;            mflux = median(influx[where(influx NE 0)])
;            inivar /= mivar
;            innivar /=  mivar
;            influx /= mflux
;         endif 
         ;;??????? BUG  ????
         ;; Currently we are not allowing for any shifts between
         ;; exposures. But once wavelengths are fixed we may want to 
         ;; allow shifts so that we can corect flexure. 
         NOSHIFT=1
;         colors = getcolor(/load)
;         plot, 10^inloglam, influx[*,0], yrange=[-1,200]
;         oplot, 10^inloglam, influx[*,1]
;         oplot, 10^inloglam, influx[*,2]
;         oplot, 10^inloglam, influx[*,3]

         if (not keyword_set(USER_WEIGHTS)) then begin
            fire_long_combspec, influx, inivar, inloglam, insky = insky $
                             , innivar = innivar $
                             , newflux = fflux, newivar = newivar $
                             , newnivar = newnivar, newsky = fsky $
                             , newmask = newmask, CHECK = chk $
                             , NOSHIFT = NOSHIFT, IREF = IREF, FLAG=flag $
                             , ERR_MESSAGE=err_message, /MEDSCALE $
                             , SNR_OUT=snr_calc
            snr_out[*,31-qq] = snr_calc
         endif else begin
            fire_long_combspec, influx, inivar, inloglam, insky = insky $
                             , innivar = innivar $
                             , newflux = fflux, newivar = newivar $
                             , newnivar = newnivar, newsky = fsky $
                             , newmask = newmask, CHECK = chk $
                             , NOSHIFT = NOSHIFT, IREF = IREF, FLAG=flag $
                             , ERR_MESSAGE=err_message, /MEDSCALE $
                             , USER_WEIGHTS=user_weights
         endelse

         if FLAG NE 0 then begin
            fire_siren, func_name + $
                        ": ERROR with fire_long_combspec!  Exiting prematurely!"
            RETURN
         endif                           
 ;        oplot, 10^inloglam, fflux, color=colors.red                        
 ;        stop

;         if (median(inivar) GT 1e6) then begin
;            newflux *= medflux
;            newivar /= medflux^2
;            newnivar /=  medflux^2
;            newsky *= medflux
;         endif 
          fvar   = newmask/(newivar + (newivar EQ 0.0))
          fnovar = newmask/(newnivar + (newnivar EQ 0.0))
          bad = where(newmask EQ 0, nbad)
          IF nbad NE 0 THEN BEGIN
              fvar[bad] = 0.0D
              fnovar[bad] = 0.0D
          ENDIF
          echfspec.wave[0:npix-1, qq] = allobj[all_ordr[REFO]].wave[0:npix-1]
          echfspec.fx[0:npix-1, qq] = medflux * fflux[0:npix-1]
          echfspec.var[0:npix-1, qq] = (medflux^2) * fvar[0:npix-1]
          echfspec.novar[0:npix-1, qq] = (medflux^2) * fnovar[0:npix-1]
          echfspec.sky[0:npix-1, qq] = medflux * fsky[0:npix-1]
      endelse
   endfor
  ;;;; OUTPUT  ;;;;

  ;x_wrechfspec, echfspec, outfil 
  ;close, 56

	;; All's well..
  fire_undefine, err_message
  flag = 0
	
  if (total(snr_out) GT 0) then begin
     mwrfits, snr_out, "snr_array.fits", /create
  endif

  print, 'fire_esi_echcombspec:  All done!'	
	
  return
end
  

