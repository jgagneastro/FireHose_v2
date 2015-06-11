pro fire_pipe_widget_status, message, VERBOSE=verbose, $
                             WIDGET=widget, _EXTRA = keys

  if keyword_set(VERBOSE) AND keyword_set(WIDGET) then begin
     WIDGET_CONTROL, widget, set_value=message, _EXTRA = keys
  endif
  
  RETURN
  
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro fire_pipe, fire, $
               targets=targetnames, $
               clobber=clobber, $
               verbose=verbose, $
               widget=widget, $
               reduxpath=reduxpath, $
               BRIGHT=bright,$
               chkwpix=chkwpix, $  ; Plot wavepix information
               chkarc=chkarc, $    ; Plot arc information
               chkobj=chkobj, $    ; Plot object finding information
               chkext=chkext, $    ; Plot extraciton information
               fitsname=fitsname, $ ; fitsfile name of firestrct
               sep_sky_exp=sep_sky_exp, $ ; Form sky model from separate exposure
               reset_sky_matches=reset_sky_matches, $ ; if /sep_sky_exp, $
               	; then reset sky matches if already set
               debug=debug, $
               _EXTRA=keys


  func_name = 'fire_pipe'
  twodarc = 0
              

  ntargs = n_elements(targetnames)

  ;; If required, first match all objects with sky frames
	if keyword_set(sep_sky_exp) then begin

	  fire_pipe_widget_status, func_name + ": Matching science exposures with sky exposures...", $
	  		VERBOSE=verbose, WIDGET=widget, _EXTRA=keys
		
		fire_match_skies, fire, flag=flag, widget=widget, fitsname=fitsname, raw=raw, /no_tell_matches, $
			targets = targetnames, VERBOSE=verbose, reset_sky_matches=reset_sky_matches

		if flag NE 0 then begin
	      fire_siren, func_name + $
	                  ": ERROR! Unable to match sky exposures! " + $
	                  "Exiting!", $
	                  WIDGET=widget, /append, /both
			RETURN
		endif		
		
	endif


  print, func_name + ": Starting Reduction loop"

;  if NOT keyword_set(PLOT_INFO) then NOPLOT = 1
  ;; Break NOPLOT into parts for arcs, obj finding, and extraction.
 

  fire_pipe_widget_status, func_name + ":  Will run pipeline on " + $
                           fire_string(ntargs) + $
                           " object" + s_or_not(ntargs) + $
                           ":", VERBOSE=verbose, WIDGET=widget, _EXTRA=keys
  fire_pipe_widget_status, "    " + targetnames, VERBOSE=verbose, $
                           WIDGET=widget, _EXTRA=keys

  ;; Cycle through the list of science objects
  for itarg=0, ntargs-1 do begin
     
     sci = where(fire.object EQ targetnames[itarg] AND $
                 strupcase(strtrim(fire.exptype,2)) EQ "SCIENCE", nmatch)
     ;sci = where(fire.object EQ targetnames[itarg] AND $
     ;            strupcase(strtrim(fire.exptype,2)) EQ "TELL" or strupcase(strtrim(fire.exptype,2)) EQ "Tell", nmatch)

;     stop
    
     if (nmatch EQ 0) then begin
        print, "No frames found for this target"
        continue
     endif
     
     fire_pipe_widget_status, func_name + $
                              ": Running pipeline on object " + $
                              targetnames[itarg], $
                              VERBOSE=verbose, WIDGET=widget, _EXTRA = keys
     
     ;; Cycle through the science frames taken for each object
     for iframe=0, nmatch-1 do begin
        
        ;;;;;;;;;;;;;;;; PRELIMINARIES FOR THIS FRAME ;;;;;;;;;;;;;;;;;
             
        ttt = [1,13,14,16]
        for ii=0L, n_elements(fire)-1L do begin
          for jj=0L, n_elements(ttt)-1L do begin
            aa = fire.(ttt[jj])
            string_replace, aa, '//', '/'
            fire.(ttt[jj]) = aa
          endfor
        endfor
        
        scistruct = fire[sci[iframe]]
        
        rawpath = fire_get(scistruct, /rawpath)
        scifil1 = strtrim(fire_get(scistruct, /fitsfile, /donotcalc),2) ;data
        scifil  = rawpath + scifil1
        pixflat = fire_get(scistruct, /Pixflatfile)
        illum   = fire_get(scistruct, /Illumflatfile)
        tell    = fire_get(scistruct, /TFiles)
        ordrfil = fire_get(scistruct, /Orderfile)
        
        ostrfil = fire_get(scistruct, /Ordr_str_file)
        objname = fire_get(scistruct, /Object) + $
                  " (" + fire_get_fitnum(scifil1) + ")"
        if keyword_set(sep_sky_exp) then begin
		skyfil = rawpath + scistruct.skymodfile
	endif
        ntells = n_elements(tell)
        
        ;; Determine if a telluric has not been matched.  
        ;; If so, let the user extract the object, but then
        ;; warn later on down the road
        bad_tells = 0
        if is_empty(tell) EQ 1 then begin
           bad_tells = 1
        endif else if ntells EQ 1 then begin
           if strmatch(strtrim(tell,2),"unknown",/FOLD_CASE) EQ 1 then begin
              bad_tells = 1
           endif
        endif
        
        ; Update the widget
        fire_pipe_widget_status, "  Analyzing exposure " + scifil + $
                                 " (file " + fire_string(iframe+1) + " of " + $
                                 fire_string(nmatch) + " for object " + $
                                 fire_string(itarg+1) + " of " + $
                                 fire_string(ntargs) + ")", $
                                 VERBOSE=verbose, WIDGET=widget, _EXTRA = keys
        
        ;; Determine whether to use OH lines from the science image itself, or
        ;; ThAr/Ar lines from an arc file based upon the exposure time.

        min_time_for_OH = 307 ;; minimum allowed exptime for using OH lines
        exptime = fire_get(scistruct, /Exptime)			
        scistruct.OH_cal = 0 ; Temporary hack!
        if scistruct.OH_cal EQ 0 then begin
           THAR = 1
           fire_undefine, OH        	
        endif else begin
           OH = 1
           fire_undefine, THAR        
        endelse
        
       ; File names for the wavecal img
       ; (science frame for OH calib, or ThAr
        
        if keyword_set(OH) then begin
           wcfile = rawpath + fire_get(scistruct, /WAVECALFILE, /OH) 
           wcfits = fire_get(scistruct, /ARCFITS, /OH) 
           wcimg = fire_get(scistruct, /ARCIMGFITS, /OH)
        endif else if keyword_set(THAR) then begin
           wcfile = fire_get(scistruct, /WAVECALFILE, /THAR) 
           wcfits = fire_get(scistruct, /ARCFITS, /THAR)
           wcimg = fire_get(scistruct, /ARCIMGFITS, /THAR) 
        endif else begin
           fire_siren, func_name + $
                       ": ERROR! Must use either OH or THAR lines for " + $
                       "wavelength calibration!  Skipping this exposure!", $
                       WIDGET=widget, /append, /both
           fire[sci[iframe]] = scistruct ; save back.
           continue
        endelse
        
        if( is_empty(wcfile) EQ 1 ) then begin
           fire_siren, func_name + $
                       ": ERROR! wavelength calibration file " + $
                       "for science file " + scifil + $
                       "does not exist!  Skipping this exposure...", $
                       WIDGET=widget, /append, /both
           fire[sci[iframe]] = scistruct ;; save 
           continue           		
        endif

        if keyword_set(VERBOSE) then begin
           print, ''
           print, "++++++++++++++++++++++++++++++++++++"
           print, "WAVELENGTH CALIBRATION details:"
           print, ""
           print, "Important input values:"
           print, "Science file: ", scifil
           print, "Rawpath to data: ", rawpath
           print, "Use ThAr? ", yes_if_defined(ThAr)
           print, "Use OH (in science file)? ", yes_if_defined(OH)
           print, "Wavelength calibration file: ", wcfile
           print, "Pixel flat file name: ", pixflat
           print, "Illumination flat file name: ", illum
           print, "Order file name: ", ordrfil
           print, "Order structure file name: ", ostrfil
           print, ""			
           print, "Output file names will be:"
           print, "wavelength calibration fits file: ", wcfits
           print, "wavelength calibration image file: ", wcimg
           print, ""
           print, "General info:"
           print, "Pass /chk? ", yes_if_defined(CHK)
           print, "Clobber? ", yes_if_defined(CLOBBER)
           print, "++++++++++++++++++++++++++++++++++++"
           print, ''
           if keyword_set(DEBUG) then stop
        endif		
        
        ;; Make sure that the order file has been created
        if (x_chkfil(ordrfil) EQ 0) then begin
           fire_siren, $
              "Order mask does not exist...skipping exposure", $
              WIDGET=WIDGET, /append, /both
           fire[sci[iframe]] = scistruct ;; save 
           continue
        endif else begin
           tset_slits = xmrdfits(ordrfil, 1)
        endelse
        
        ;;;;;;;; DONE WITH PRELIMINARIES: ALL FILES SHOULD NOW BE IN
        ;;;;;;;; PLACE 


        ;;;;;;;;;;;;;;;;; BEGIN PIPELINE ;;;;;;;;;;;;;;;;

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;; ARC SOLUTION ;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        if ( x_chkfil(wcimg, /SILENT) EQ 0 OR keyword_set(CLOBBER) ) then begin
           
           if keyword_set(THAR) then begin
              
              ; Is the lamp actually on?
              lamp_val = strtrim(sxpar(headfits(wcfile),"LAMP"),2)
              if lamp_val NE 'ThAr' AND lamp_val NE 'Ar' then begin
                 fire_siren, func_name + $
                             ": ERROR! Arc solution requested for " + $
                             wcfile + " but the lamps are not on!  Aborting.", $
                             WIDGET=WIDGET, /append, /both
                 fire[sci[iframe]] = scistruct ;; save 
                 continue
              endif
              
              ; If pinter flag is set, then user interactive fit to pre-guess.
              ; Tedious.
              ; Chkarc=2 -> interactive
              ; chkarc=1 -> watch plots
              ; chkarc=0 -> Silent
              pinter = (chkarc EQ 2) ? 1 : 0
              noplot = (chkarc EQ 0 OR chkarc EQ 2) ? 1 : 0
              
              if (twodarc) then begin
                 fire_arc, arcraw = wcfile, $
                           ordr_str=mrdfits(ostrfil,1), $
                           arcfits=wcfits, arcimgfits=wcimg, /ThAr, $
                           reduxpath=reduxpath, $
                           CLOBBER=clobber, ordermask=ordrfil
                 result = 1
              endif else begin
                 
                 result=fire_arc2(arcraw = wcfile, $
                                  ordr_str=mrdfits(ostrfil,1), $
                                  arcfits=wcfits, arcimgfits=wcimg, /ThAr, $
                                  reduxpath=reduxpath, OBJNAME=objname, $
                                  NOPLOT=noplot, CHKWPIX=chkwpix, $
                                  CLOBBER=clobber, ordermask=ordrfil, $
                                  CHKARC=chkarc, $
                                  PINTER=pinter, ERR_MESSAGE=err_message)
              endelse
              
              ; Go back and tweak the arc lamp solution to align it 
              ; with the scienc frames OH lines.  Without this stwp
              ; we were experiencing ~0.5-1 pixel differences between
              ; wavelength solutions derived for ThAr and OH.
              fire_ThAr_tweak, wcimg, scifil, ordrfil, /chk

              if result EQ -1 then begin
                 fire_siren, func_name + ": Wavelength calibration FAILED (" + $
                             err_message + ")!  Skipping exposure.", $
                             WIDGET=WIDGET, /append, /both
                 fire[sci[iframe]] = scistruct 
                 continue
              endif
              
           endif else if keyword_set(OH) then begin
              
              if (exptime LT min_time_for_OH) then begin
                 fire_siren, func_name + $
                   "WARNING: Attempting an OH wavecal for exptime < " $
                   + fire_string(min_time_for_OH) + $
                   " sec.  The lines are faint, you should consider ThAr.  Caveat emptor!"
              endif
              
              if (twodarc) then begin
                 fire_arc, arcraw = strtrim(wcfile,2), $
                           ordr_str=mrdfits(ostrfil,1), $
                           arcfits=wcfits, arcimgfits=wcimg, /OH, $
                           reduxpath=reduxpath, $
                           CLOBBER=clobber, ordermask=ordrfil
                 result = 1
              endif else begin
                 stop
                 result=fire_arc2(arcraw = strtrim(wcfile,2), $
                                  ordr_str=mrdfits(strtrim(ostrfil,2),1), $
                                  arcfits=strtrim(wcfits,2), $
                                  arcimgfits=strtrim(wcimg,2), /OH, $
                                  reduxpath=reduxpath, OBJNAME=objname, $
                                  NOPLOT=noplot, CHKWPIX=chkwpix, $
                                  CLOBBER=clobber, CHKARC=chkarc, $
                                  ordermask=ordrfil, ERR_MESSAGE=err_message)
              endelse

              if result EQ -1 then begin
                 fire_siren, func_name + $
                             ": Wavelength calibration FAILED (" + $
                             err_message + ")!  Skipping exposure.", $
                             WIDGET=WIDGET, /append, /both
                 fire[sci[iframe]] = scistruct 
                 continue
              endif
              
           endif else begin
              ;; Should never reach here unless problems with the structure.
              ;; If you're here, then neither OH lines or ThAr
              ;; are being used.
              fire_siren, func_name + $
                          ": ERROR! Must use either OH lines or ThAr lines!",$
                          WIDGET=WIDGET, /append, /both
              fire[sci[iframe]] = scistruct 
              stop
              continue
           endelse
           
           fire_pipe_widget_status, $
              "    ...done with wavelength calibration", $
              VERBOSE=verbose, WIDGET=widget, _EXTRA = keys
        endif else begin
           
           print, func_name + ": Using existing arc solution (no /clobber)"
           fire_pipe_widget_status, "    ...done with wavelength calibration", $
                                    VERBOSE=verbose, WIDGET=widget, _EXTRA = keys
           
        endelse
        
        ;; Read in the wavelength image (just created by fire_arc)
        if (x_chkfil(wcimg) EQ 0 ) then begin
           fire_siren, "Wavelength calibration image (" + wcimg + $
                       ") does not exist!" + $
                       "  fire_arc FAILED!  Skipping exposure...", $
                       WIDGET=WIDGET, /append, /both
           fire[sci[iframe]] = scistruct 
           continue
        endif else begin
           waveimg = xmrdfits(wcimg)
        endelse			
        
        ;; Determine the important file names
        objstrfil = fire_get(scistruct, /OBJSTRFILE)
        outfil = fire_get(scistruct, /ECHEXTFILE) 
        outfilzip = outfil + '.gz' ;; doesn't appear to be used...
        
        if keyword_set(VERBOSE) then begin
           print, ''
           print, "++++++++++++++++++++++++++++++++++++"
           print, "EXTRACTION details:"
           print, ""
           print, "Important input values:"
           print, "Rawpath to data: ", rawpath
           print, "Wavelength image file: ", wcimg
           print, "Pixel flat file name: ", pixflat
           print, "Illumination flat file name: ", illum
           print, "Order file name: ", ordrfil
           print, "Order structure file name: ", ostrfil
           print, ""			
           print, "Output file names will be:"
           print, "Object structure file: ", objstrfil
           print, "Extracted object file: ", outfil
           print, ""
           print, "General info:"
           print, "Pass /chk? ", yes_if_defined(CHK)
           print, "Clobber? ", yes_if_defined(CLOBBER)
           print, "++++++++++++++++++++++++++++++++++++"
           print, ''
           if keyword_set(DEBUG) then stop
        endif		

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;; EXTRACTION ;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        
        ; NOW PROCESS AND EXTRACT
        if (x_chkfil(objstrfil, /SILENT) EQ 0) OR keyword_set(CLOBBER) then begin
           ;;;;;;;;; Trim, flat field, etc ;;;;;;;;;;;;;;;;
           fire_proc, strtrim(scifil,2), sciimg, sciivar, hdr=scihdr, $
                      pixflatfile=strtrim(pixflat,2),$
                      illumflatfile=strtrim(illum,2), /MASKBADPIX

	   if keyword_set(sep_sky_exp) then begin
		fire_proc, strtrim(skyfil,2), skyimg, skyivar, hdr=scihdr, $
		      pixflatfile=strtrim(pixflat,2), $
		      illumflatfile=strtrim(illum,2), /MASKBADPIX
	   endif

           ;;;;;;;  Map the slit tilts with fire_makescipix ;;;;;;;;
           if keyword_set(THAR) then begin
              fire_proc, wcfile, arcimg, arcivar, hdr=archdr, $
                         pixflatfile=pixflat,$
                         illumflatfile=illum, /MASKBADPIX
              piximg = fire_makescipix(arcimg, tset_slits, $
                                       arc2=arcimg, CHK=chkwpix)
           endif else begin

              ; If we're not using a ThAr wavelength soln, still use the
              ; nearest ThAr frame for tilt mapping.  Get the file name
              ; from the FIRE structurte.
              thar_fits = fire_get(scistruct, /ARCS)
              if is_empty(thar_fits) EQ 1 then begin
                 fire_siren, func_name $
                             + ": ERROR! 'ARCFILE' not in firestrct"+$
                             " (needed for slit tilt map)!"+$
                             "  Edit firestrct and try again later." + $
                             "  Skipping this exposure for now...", $
                             WIDGET=WIDGET, /append, /both
                 fire[sci[iframe]] = scistruct 
                 continue					  
              endif
              narcs = n_elements(thar_fits)
              if narcs GT 1 then begin
              		thar_fits = thar_fits[narcs-1]
              endif
              fire_proc, thar_fits, arcimg, arcivar, hdr=archdr, $
                         pixflatfile=pixflat,$
                         illumflatfile=illum, /MASKBADPIX
              piximg = fire_makescipix(sciimg, tset_slits, arc2=arcimg, $
                                       CHK=chkwpix)
           endelse

           if size(piximg, /type) EQ 2 then begin
              fire_siren, func_name + $
                          ": ERROR with slit tile map (fire_makescipix)!"+$
                          "  Skipping this exposure for now...", $
                          WIDGET=WIDGET, /append, /both
              fire[sci[iframe]] = scistruct 
              continue
           endif
           

           ;;;;;;;;;;;;; Slit tilts mapped, now fit the 2D sky ;;;;;;;;;;
           if not keyword_set(SEP_SKY_EXP) then begin
	           skyimage = fire_skymodel(sciimg=sciimg, sciivar=sciivar, piximage=piximg, tset_slits=tset_slits)
	           
	           fire_pipe_widget_status, "    ...done creating sky model", $
	                                    VERBOSE=verbose, WIDGET=widget, $
	                                    _EXTRA = keys
	   endif else begin
		skyimage = skyimg ;skysubalign(skyimg)
			
	   endelse
           
           ;;;;;;;;;;;;;;;;;; Object Finding ;;;;;;;;;;;;;;;;;;;;;;;;;
           imgminsky=sciimg-skyimage
           objinter = (chkobj EQ 2) ? 1 : 0

           titlestr = "Object finding for frame: "+strtrim(scifil1,2)
           obj_strct = fire_findobj(img_minsky=imgminsky,$
                                    sciivar=sciivar,waveimg=10^waveimg,$
                                    tset_slits=tset_slits,filstd=filstd, $
                                    nfind=1, chk=chkobj, inter=objinter, $
                                    BOX_RAD=box_rad, title=titlestr, $
                                    PROF_IMG=prof_image)
           
           if size(obj_strct,/type) NE 8 then begin
              fire_siren, func_name + ": ERROR! fire_findobj failed "+$
                          "(output not of correct type) " + $
                          "for science file " + scifil + $
                          "!  Skipping this exposure...", $
                          WIDGET=widget, /append, /both
              fire[sci[iframe]] = scistruct
              continue
           endif
           fire_pipe_widget_status, "    ...done finding object", $
                                    VERBOSE=verbose, $
                                    WIDGET=widget, _EXTRA = keys
           

           ;;;;;;;;;;;;;; Extract the science object (SLOW) ;;;;;;;;;;;;
           if (NOT keyword_set(BRIGHT)) then begin

              ; box_rad set only if interactive
              ; object finding turned on, in which
              ; case you get back a scalar.

              if (keyword_set(box_rad)) then begin
                 if (n_elements(box_rad) LT 21) then box_rad = replicate(box_rad[0], 21)
                 if (box_rad[0] LT 1) then begin
                    print, " "
                    print, "ERROR: Boxcar radius selected is smaller than one pixel"
                    print, "  Something is probably wrong.  You can force the aperture size in"
                    print, "  pixels by typing:"
                    print, "    IDL> box_rad[*] = N <where N is the desired aperture in pixels>"
                    print, "    IDL> .continue"
                    print, " "
                    print, "  but you may wish to check with the pipeline authors about this."
                    stop
                 endif
                 ;!@! #$# SIENCE EXTRACTION HAPPENS HERE J. G.
                 result = fire_echextobj(sciimg=sciimg, sciivar=sciivar, $
                                         scihdr=scihdr, $
                                         skyimage=skyimage,$
                                         piximg=piximg, waveimg=10^waveimg, $
                                         tset_slits=tset_slits, $
                                         objfile=objstrfil, $
                                         objstr=obj_strct, outfil=outfil, $
                                         reduxdir=reduxdir, NOPLOT=noplot, $
;                                         CHK=chkext, /HELIO_CORR, $
                                         CHK=1, /HELIO_CORR, $
                                         OBJNAME=objname, box_rad=box_rad, $
                                         PROFILE_INPUT=prof_image)
              endif else begin
                 result = fire_echextobj(sciimg=sciimg, sciivar=sciivar, $
                                         scihdr=scihdr, $
                                         skyimage=skyimage,$
                                         piximg=piximg, waveimg=10^waveimg, $
                                         tset_slits=tset_slits, $
                                         objfile=objstrfil, $
                                         objstr=obj_strct, outfil=outfil, $
                                         reduxdir=reduxdir, NOPLOT=noplot, $
                                         CHK=chkext, /HELIO_CORR, $
                                         OBJNAME=objname)
              endelse                 

           endif else begin
              ; Boxcar for bright objects if the user selects
              ; Boxcar turned on by including the /STD flag.
;              if (not keyword_set(BOX_RAD)) then box_rad = 10.
;              if (size(box_rad, /dimen) GT 0) then box_rad = box_rad[0]
              result = fire_echextobj(sciimg=sciimg, sciivar=sciivar, $
                                      scihdr=scihdr, $
                                      skyimage=skyimage,$
                                      piximg=piximg, waveimg=10^waveimg, $
                                      tset_slits=tset_slits, $
                                      objfile=objstrfil, $
                                      objstr=obj_strct, outfil=outfil, $
                                      reduxdir=reduxdir, NOPLOT=noplot, $
                                      CHK=chkext, /HELIO_CORR, $
                                      OBJNAME=objname, $
                                      /STD $
                                     )
           endelse

           if result NE 0 then begin
              fire_siren, func_name + ": ERROR extracting object!  "+$
                          "Skipping this exposure...", WIDGET=WIDGET, $
                          /append, /both
              fire[sci[iframe]] = scistruct 
              continue
           endif else begin
              fire_pipe_widget_status, "    ...done extracting object", $
                                       VERBOSE=verbose, WIDGET=widget, $
                                       _EXTRA = keys
           endelse	                                    
           
        endif else begin 
           
           print, func_name + ": File " + strtrim(scifil,2) + $
                  " already extracted (object structure file = " + $
                  objstrfil + ") and /clobber not passed!  Skippping..."
           
        endelse
        
        fire_pipe_widget_status, $
           "    ...done extracting object", $
           VERBOSE=verbose, WIDGET=widget, _EXTRA = keys
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;; SCIENCE EXTRACTION COMPLETE ;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


        ;;;;;;;;;;; CALIBRATE AND EXTRACT TELLURICS ;;;;;;;;;;;;;
        
        if bad_tells EQ 1 then begin
           fire_siren, "No tellurics matched to this exposure!", $
                       WIDGET=WIDGET, /append, /both
           fire[sci[iframe]] = scistruct
           continue 
        endif

        fire_pipe_widget_status, "  Extracting " + fire_string(ntells) $
                  + " telluric files for this exposure (" + $
                  fire_str_array_to_str(fire_string(fire_get_fitnum(tell)) )$
                  + ")", VERBOSE=verbose, WIDGET=widget, _EXTRA = keys
        
        for itel=0, ntells-1 do begin
           
           telfile = tell[itel]
           
           ; If we've already done this telluric, skip it.
           if is_undefined(tellsdone) EQ 0 then begin
              if max( strmatch(tellsdone, telfile) ) EQ 1 then begin
                 msg = "Skipping telluric file " $
                       + fire_get_fitnum(telfile) + " (" + $
                       fire_string(itel+1) + $
                       " of " + fire_string(ntells) + $
                       "; already visited for this pipeline run.)"
                 fire_pipe_widget_status, "    ..." + msg,$
                                          VERBOSE=verbose, $
                                          WIDGET=widget, _EXTRA = keys
                 fire[sci[iframe]] = scistruct 
                 continue
              endif
           endif
           msg = "Analyzing telluric file " + telfile + $
                 " (" + fire_string(itel+1) + $
                 " of " + fire_string(ntells) + ")"
           fire_pipe_widget_status, "    " + msg + "...", $
                                    VERBOSE=verbose, WIDGET=widget, $
                                    _EXTRA = keys          
           
           tarc   = (fire_get(scistruct, /TARCS))[0]

           
           pixflat = fire_get(scistruct, /Pixflatfile)
           illum   = fire_get(scistruct, /Illumflatfile)
           rawpath = scistruct.rawpath
           ordrfil = fire_get(scistruct, /Orderfile)
           ostrfil = fire_get(scistruct, /Ordr_str_file)
           twcfits = fire_get(scistruct, /ARCFITS, /THAR, $
                              STD=itel+1, /DONOTSET) ;; tell wavecal structure
           twcimg  = fire_get(scistruct, /ARCIMGFITS, /THAR, $
                              STD=itel+1, /DONOTSET) ;; tell wavecal arcimg
           tobjname = fire_get(scistruct, /TELLNAME) + " (" + $
                      fire_get_fitnum( telfile ) + ")"
           
           if keyword_set(VERBOSE) then begin
              print, ''
              print, "++++++++++++++++++++++++++++++++++++"
              print, "-------- TELLURIC --------"
              print, "WAVELENGTH CALIBRATION details:"
              print, ""
              print, "Important input values:"
              print, "Telluric file: ", telfile
              print, "Rawpath to data: ", rawpath
              print, "Wavelength calibration file: ", tarc
              print, "Pixel flat file name: ", pixflat
              print, "Illumination flat file name: ", illum
              print, "Order file name: ", ordrfil
              print, "Order structure file name: ", ostrfil
              print, ""			
              print, "Output file names will be:"
              print, "wavelength calibration fits file: ", twcfits
              print, "wavelength calibration image file: ", twcimg
              print, ""
              print, "General info:"
              print, "Pass /chk? ", yes_if_defined(CHK)
              print, "Clobber? ", yes_if_defined(CLOBBER)
              print, "++++++++++++++++++++++++++++++++++++"
              print, ''
              if keyword_set(DEBUG) then stop
           endif		

	;; Determine the name of the output structure file
           objstrfil = fire_get(scistruct, /TOBJSTRFILE, STD=itel+1)
           
           if (x_chkfil(objstrfil, /SILENT) NE 0) AND $
              NOT keyword_set(CLOBBER) then begin
              print, func_name + ": File " + objstrfil + $
                     " already exists, /clobber not passed!  Skipping..."
              fire_pipe_widget_status, "    ...done extracting telluric file " $
                                       + fire_get_fitnum(telfile) + " (" + $
                                       fire_string(itel+1) + " of " + $
                                       fire_string(ntells) + $
                                       ") (already extracted)", $
                                       VERBOSE=verbose, WIDGET=widget, $
                                       _EXTRA = keys
              fire[sci[iframe]] = scistruct 
              continue
           endif
           
           if (x_chkfil(ordrfil) EQ 0) then begin
              fire_siren, "Order mask not found.  Skipping this exposure!", $
                          WIDGET=WIDGET, /append, /both
              fire[sci[iframe]] = scistruct 
              continue              
           endif else begin
              tset_slits = xmrdfits(ordrfil, 1)
           endelse
           
           lampon = 0

           if (x_chkfil(twcimg, /SILENT) EQ 1) AND $
              NOT keyword_set(CLOBBER) then begin
              
              fire_pipe_widget_status, $
                 "    ...done with telluric wavelength calibration"+$
                 "(already existed, /clobber not passed)", $
                 WIDGET=WIDGET, VERBOSE=verbose, /append, /both					
              
           endif else begin 
              if (is_empty(tarc) EQ 1) then begin
                 fire_siren, "No arc matched to telluric calibrator!"+$
                             "  Skipping this exposure!", WIDGET=WIDGET, $
                             /append, /both
                 fire[sci[iframe]] = scistruct
                 continue
              endif else begin
                 if (x_chkfil(tarc) EQ 0) then begin
                    fire_siren, "Matched arcfile does not exist!  "+$
                                "Skipping this exposure!", WIDGET=WIDGET, $
                                /append, /both
                    fire[sci[iframe]] = scistruct
                    continue			
                 endif else begin 
                    tmp = strtrim(sxpar(headfits(tarc),"LAMP"),2)
                    lampon = tmp EQ 'ThAr' or tmp EQ 'Ar'

                    ;;;;;;;;;  TELLURIC WAVELENGTH SOLUTION ;;;;;;;;;
                    if (lampon) then begin
                       if (twodarc) then begin
                          fire_arc, arcraw = strtrim(tarc,2), $
                                    ordr_str=mrdfits(ostrfil,1), $
                                    arcfits=twcfits, arcimgfits=twcimg, $
                                    /ThAr, $
                                    reduxpath=reduxpath, $
                                    CLOBBER=clobber, ordermask=ordrfil
                       endif else begin
                          result = fire_arc2(arcraw = tarc, $
                                             ordr_str=mrdfits(ostrfil,1), $
                                             arcfits=twcfits, NOPLOT=noplot, $
                                             arcimgfits=twcimg, $
                                             reduxpath=reduxpath, $
                                             /ThAr, CHKARC=chkarc, $
                                             CLOBBER=clobber, OBJNAME=tobjname, $
                                             ORDERMASK=ordrfil, $
                                             ERR_MESSAGE=err_message, $
                                             CHKWPIX=chkwpix)
                       endelse
                       if result EQ -1 then begin
                          fire_siren, func_name + ": Telluric wavelength "+$
                                      "calibration FAILED (" + err_message + $
                                      ")!  Skipping exposure.", WIDGET=WIDGET,$
                                      /append, /both
                          fire[sci[iframe]] = scistruct 
                          continue
                       endif
                    endif else begin ;; lamp on
                       fire_siren, "Lamp not on for telluric wavelength"+$
                                   "calibration file (" + tarc + ")!" + $
                                   "  Will attempt to use OH lines.  "+$
                                   "Check your results!", WIDGET=WIDGET, $
                                   /append, /both
                       
                       result = fire_arc2(arcraw = tarc, $
                                          ordr_str=mrdfits(ostrfil,1), $
                                          arcfits=twcfits, NOPLOT=noplot, $
                                          arcimgfits=twcimg, $
                                          reduxpath=reduxpath, $
                                          /OH, CHKARC=chkarc, CHKWPIX=chkwpix, $
                                          CLOBBER=clobber, OBJNAME=tobjname, $
                                          ORDERMASK=ordrfil, $
                                          ERR_MESSAGE=err_message)
                       
                       if result EQ -1 then begin
                          fire_siren, func_name + $
                                      ": Telluric wavelength calibration"+$
                                      "FAILED (" + err_message + $
                                      ")!  Skipping exposure.", $
                                      WIDGET=WIDGET, /append, /both
                          fire[sci[iframe]] = scistruct
                          continue
                       endif else begin
                          fire_pipe_widget_status, $
                             "    ...done with telluric wavelength calibration",$
                             WIDGET=WIDGET, VERBOSE=verbose, /append, /both
                       endelse
                    endelse ;; lamp not on
                 endelse ;; xchkfil(tarc)
              endelse ;; is_empty(tarc)
           endelse ;; file needs to be created: doesn't exist, or /clobber

           
           ;; Read in the wavelength image (just created by fire_arc)
           if (x_chkfil(twcimg) EQ 0) then begin
              fire_siren, "Telluric wavelength calibration image (" + $
                          twcimg + ") does not exist!" + $
                          "  fire_arc FAILED!  Skipping exposure...", $
                          WIDGET=WIDGET, /append, /both
              continue
           endif else begin
              waveimg = xmrdfits(twcimg)
           endelse		
           
           
           ;;;;;;;;;;;;; EXTRACT TELLURIC ;;;;;;;;;;;;;;;;;
           
           objstrfil = fire_get(scistruct, /TOBJSTRFILE, STD=itel+1)
           outfil = fire_get(scistruct, /TECHEXTFILE, STD=itel+1) 
           outfilzip = outfil + '.gz' ;; doesn't appear to be used...
           
           if keyword_set(VERBOSE) then begin
              print, ''
              print, "++++++++++++++++++++++++++++++++++++"
              print, func_name, ": /VERBOSE flag passed."
              print, "-------- TELLURIC --------"
              print, "EXTRACTION details:"
              print, ""
              print, "Important input values:"
              print, "Telluric file: ", telfile
              print, "Rawpath to data: ", rawpath
              print, "Wavelength calibration file: ", tarc
              print, "Pixel flat file name: ", pixflat
              print, "Illumination flat file name: ", illum
              print, "Order file name: ", ordrfil
              print, "Order structure file name: ", ostrfil
              print, ""			
              print, "Output file names will be:"
              print, "Object structure file: ", objstrfil
              print, "Extracted object file: ", outfil
              print, ""
              print, "General info:"
              print, "Pass /chk? ", yes_if_defined(CHK)
              print, "Clobber? ", yes_if_defined(CLOBBER)
              print, "++++++++++++++++++++++++++++++++++++"
              print, ''
              if keyword_set(DEBUG) then stop
           endif		
           
           if (x_chkfil(objstrfil, /SILENT) EQ 0) OR $
              keyword_set(CLOBBER) then begin
              
              ;;;;;;;;;;;;;;; Trim, flat field, etc ;;;;;;;;;
              fire_proc, telfile, sciimg, sciivar, hdr=scihdr, $
                         pixflatfile=pixflat,$
                         illumflatfile=illum, /MASKBADPIX
              

              ;;;;;;;;;;;;  Map Slit tilts (TELLURIC) ;;;;;;;;;;;;;;;;;;
              if ( is_empty(tarc) EQ 1 ) then begin
                 fire_proc, wcfile, wcimg, wcimgivar, hdr=wcimghdr, $
                            pixflatfile=pixflat,$
                            illumflatfile=illum, /MASKBADPIX

                 piximg = fire_makescipix(wcimg, tset_slits, arc2=wcimg, $
                                          CHK=chkwpix) 
              endif else begin
                 ; Always use the ThAr arc for telluric calibrations
                 ; The sky is too faint for ~1 second exposures.
                 fire_proc, tarc, tmparcimg, tmparcivar, hdr=tmparchdr, $
                            pixflatfile=pixflat,$
                            illumflatfile=illum, /MASKBADPIX
                 piximg = fire_makescipix(tmparcimg, tset_slits, $
                                          arc2=tmparcimg, CHK=chkwpix, /THAR)
              endelse
              
              if size(piximg, /type) EQ 2 then begin
                 fire_siren, func_name + ": ERROR with fire_makescipix!"+$
                             "  Skipping this exposure for now...", $
                             WIDGET=WIDGET, /append, /both
                 fire[sci[iframe]] = scistruct 
                 continue
              endif
              
              ;;;;;;;;;;;;;; Fit the 2D sky (TELLURIC) ;;;;;;;;;;;;;;;;;;
              skyimage = fire_skymodel(sciimg=sciimg, sciivar=sciivar, $
                                       piximage=piximg, $
                                       tset_slits=tset_slits)
              
              ;;;;;;;;;;;;;; Object Finding (TELLURIC) ;;;;;;;;;;;;;;;;
              ;;;;; Do we need to subtract the sky here?? ;;;;
              imgminsky=sciimg-skyimage
              obj_strct = fire_findobj(img_minsky=imgminsky,$
                                       sciivar=sciivar,waveimg=10^waveimg,$
                                       tset_slits=tset_slits,filstd=filstd, $
                                       CHK=chkobj, box_rad=box_rad, STD=keyword_set(bright))
              
              ;J. Gagne --- Here is the problem with extraction of the telluric [X] [*]
              ;/STD must be removed in order to use optimal extraction for the science target
              ;;;;;;;;  Extract the Telluric - Boxcar (FAST) ;;;;;;;;;;;;
              result = fire_echextobj(sciimg=sciimg, sciivar=sciivar, $
                                      scihdr=scihdr, $
                                      skyimage=skyimage,$
                                      piximg=piximg, waveimg=10^waveimg, $
                                      tset_slits=tset_slits, $
                                      objfile=objstrfil, $
                                      objstr=obj_strct, outfil=outfil, STD=keyword_set(bright), $
                                      NOPLOT=noplot, $
                                      /HELIO_CORR, REDUXDIR=reduxdir, $
                                      CHK=chkobj, OBJNAME=tobjname)
              if result NE 0 then begin
                 fire_siren, func_name + ": ERROR extracting telluric!"+$
                             "  Skipping this telluric...", WIDGET=WIDGET, $
                             /append, /both
                 fire[sci[iframe]] = scistruct
                 continue
              endif 
              
              ;; Mark this telluric file as being processed this data run.
              if is_undefined(tellsdone) EQ 0 then begin
                 tellsdone = [ tellsdone, telfile ]
              endif else begin
                 tellsdone = [telfile]
              endelse
              
              fire_pipe_widget_status, "    ...done extracting telluric file " $
                                       + fire_get_fitnum(telfile) + " (" + $
                                       fire_string(itel+1) + " of " + $
                                       fire_string(ntells) + ")", $
                                       VERBOSE=verbose, WIDGET=widget, $
                                       _EXTRA = keys
              
           endif else begin
              
              print, func_name + ": File " + strtrim(scifil,2) + $
                     " already extracted, skipping..."
              fire_pipe_widget_status, "    ...done extracting telluric file " + $
                                       fire_get_fitnum(telfile) + " (" + $
                                       fire_string(itel+1) + " of " + $
                                       fire_string(ntells), $
                                       VERBOSE=verbose, WIDGET=widget, $
                                       _EXTRA = keys
              
           endelse
           
        endfor ;; cycle through tellurics
        
        fire[sci[iframe]] = scistruct ;; save any changes made to the structure
        
     endfor ;; iframe, cycle through science frames for a target

  endfor ;; cycle through targets
  
  RETURN
  
END
