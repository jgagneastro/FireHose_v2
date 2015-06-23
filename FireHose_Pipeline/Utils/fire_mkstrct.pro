
; FIRE_MKSTRCT
;
; This routine reads in a bunch of raw fits files and outputs a
;  structure containing information about eachi file's purpose
;  and how calibration frames are grouped together


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; FIRE_APPEND_WARNING: adds a warning string to a data structure
;
PRO FIRE_APPEND_WARNING, data, index, new_msg

	func_name = "fire_append_warning()"

	if is_undefined(data) + is_undefined(index) + is_undefined(new_msg) GT 0 then begin
		fire_siren, func_name + ": One or more inputs undefined!  Exiting..."
		RETURN
	endif

	if( is_empty(data[index].warning) EQ 1 ) then begin
		if is_empty(new_msg) EQ 0 then begin
			data[index].warning = new_msg
		endif
	endif else begin
		data[index].warning = strtrim(data[index].warning,2) + ";" + strtrim(new_msg,2)
	endelse

	RETURN
END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; FIRE_EXPTYPE_MATCH: returns 1 if old value and new_value are
; consistent or matching, otherwise returns 0
;
FUNCTION FIRE_EXPTYPE_MATCH, file, old_value, new_value, LOUD=loud
  
  match = 0
  
  if strmatch(new_value, "FIRESTARTER", /FOLD_CASE) EQ 1 then begin
     RETURN, -1
  endif
  
  ;; Arc
  if strmatch(new_value, "ARC", /FOLD_CASE) then begin
     if strmatch(old_value, "ARC", /FOLD_CASE) OR strmatch(old_value, "ARCFILE", /FOLD_CASE) OR $
        strmatch(old_value, "THAR", /FOLD_CASE) OR strmatch(old_value, "NENEAR", /FOLD_CASE) OR $
        strmatch(old_value, "AR", /FOLD_CASE) OR strmatch(old_value, "FIRESTARTER", /FOLD_CASE) then begin
        match = 1
     endif else begin
        match = 0
     endelse
     
     ;; Flat
  endif else if strmatch(new_value, "FLAT", /FOLD_CASE) then begin
     if strmatch(old_value, "FLAT", /FOLD_CASE) OR strmatch(old_value, "FLATFILE", /FOLD_CASE) $
        OR strmatch(old_value, "PIXFLAT", /FOLD_CASE) OR strmatch(old_value, "QUARTZH", /FOLD_CASE) $
        OR strmatch(old_value, "FIRESTARTER", /FOLD_CASE) then begin
        match = 1
     endif else begin
        match = 0
     endelse
     
     ;; Junk/Error
  endif else if strmatch(new_value, "JUNK*", /FOLD_CASE) then begin
     match = 0
     
     ;; Telluric
  endif else if strmatch(new_value, "TELL", /FOLD_CASE) then begin
     if strmatch(old_value, "TELLURIC", /FOLD_CASE) then begin
        match = 1
     endif else begin
        match = 0
     endelse
     
     ;; Dark
  endif else if strmatch(new_value, "DARK", /FOLD_CASE) then begin
     if strmatch(old_value, "TEST", /FOLD_CASE) then begin
        match = 1
     endif else begin
        match = 0
     endelse
     
     ;; LD: we'll worry about this mode later...
  endif else if strmatch(new_value, "LD", /FOLD_CASE) then begin
     match = 1
     
     ;; Other
  endif else if strmatch(new_value, old_value, /FOLD_CASE) then begin
     match = 1
  endif else begin
     match = 0
  endelse
  
  if keyword_set(LOUD) then begin
     if match EQ 1 then begin
        print, "fire_exptype_match(): exptype stored in " + file + $
               " (" + old_value + ") matches calculated type (" + new_value + ")!"
     endif else begin
        print, "fire_exptype_match(): exptype stored in " + file + $
               " (" + old_value + ") does NOT match calculated type (" + new_value + ")!"
     endelse
  endif
  
  RETURN, match
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; FIRE_CHAGE_EXPTYPE_VAL: a very interactive way of changing
;  the exptype string
;

function FIRE_CHANGE_EXPTYPE_VAL, func_name, file, type, old_value, $
                         new_value, LOUD=loud, INTERACTIVE=interactive, $
                         ORIGINAL=original
  
  func_name = "fire_change_exptype_val()"
  if NOT keyword_set(LOUD) then loud = 0
  
  ;; Determine if the new value matches the old value
  match = FIRE_EXPTYPE_MATCH(file, old_value, new_value, LOUD=loud)
  
  if NOT keyword_set(INTERACTIVE) then begin
     out = new_value
  endif else begin
     if match EQ -1 then begin
        if keyword_set(ORIGINAL) then begin
           if keyword_set(LOUD) then print, func_name + $
                 ": Using exposure type set in header because object is 'FireStarter' and /Original was passed"
           out = old_value
        endif else begin
           dont_stop = 1
           while dont_stop EQ 1 do begin
              prompt = func_name + ": Change " + type + " from orignal value of " + old_value + " to " + new_value + "? (y/n) "
              ans = ""
              read, func_name + ": Unable to determine exposure type since user did not change object name from 'FireStarter'.  Use original type (" + old_value + ")? (y/n; if n, will be prompted for type) ", ans
              if strmatch(ans,"y",/FOLD_CASE) OR strmatch(ans,"yes",/FOLD_CASE) then begin
                 dont_stop = 0
                 out = old_value
              endif else if strmatch(ans,"n",/FOLD_CASE) OR strmatch(ans,"no",/FOLD_CASE) then begin
                 dont_stop = 0
                 read, "Input exposure type: ", ans
                 out = ans
              endif else begin
                 print, "Must input 'y' or 'n'!  Trying again..."
              endelse
           endwhile
        endelse
     endif else if match EQ 0 then begin
        dont_stop = 1
        while dont_stop EQ 1 do begin
           ans = ""
           read, func_name + ": Change " + type + " from orignal value of " + old_value + " to " + new_value + "? (y/n) ", ans
           if strmatch(ans,"y",/FOLD_CASE) OR strmatch(ans,"yes",/FOLD_CASE) then begin
              dont_stop = 0
              out = new_value
           endif else if strmatch(ans,"n",/FOLD_CASE) OR strmatch(ans,"no",/FOLD_CASE) then begin
              dont_stop = 0
              out = old_value
           endif else begin
              print, "Must input 'y' or 'n'!  Trying again..."
           endelse
        endwhile
     endif else begin
        out = new_value
     endelse
  endelse
  
  if LOUD EQ 1 then print, func_name + $
                           ": Final exposure type of file  " + $
                           file + " determined to be " + out
  
  RETURN, out

END


FUNCTION FIRE_PATTERN_MATCH, input, names
  
  if is_undefined(names) EQ 1 then RETURN, 0
  nel = n_elements(names)
  for i=0, nel-1 do begin
     if strmatch( input, names[i], /FOLD_CASE ) EQ 1 then RETURN, 1
  endfor
  RETURN, 0
  
END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;---------------------------------------------------------------------
;;
;; Inputs a fits header from a firestrct structure and guesses the 
;;   exposure type given the information in the header.  This routine 
;;   performs catalog matching and overwrites the catalog names for
;;   objects whose positions match.  This eleiminates night-to-night
;;   variations in object naming conventions, etc.
;;
;;;---------------------------------------------------------------------
     
;    Exposure types may be the following: 
;    LD			= Long slit mode (no further explanation at this point)
;    FLAT		= Flat (either internal or sky)
;    SCIENCE	= Science frame
;    TELL		= standard star observation
;    ARC			= ThAr or Ar arc lamp
;	  SKY			= Sky model frame (telescope pointing at blank sky frame)

PRO FIRE_GUESS_EXPTYPE, data,LOUD=loud, VERBOSE=verbose, $
                        INTERACTIVE=interactive, $
                        TELLCAT=tellcat, SCICAT=scicat, TELLNAMES=tellnames, $
                        SCINAMES=scinames, ORIGINAL=original, $
                        CATPATH=catpath, OBJCATS=objcats, TELLCATS=tellcats
  
  func_name = "fire_guess_exptype()"
  
  if NOT keyword_set(LOUD) then loud = 0
  if keyword_set(LOUD) then verbose = 1
  if NOT keyword_set(VERBOSE) then verbose = 0
  
  nfiles = n_elements( data )
  
  ;; Determine the current epoch from the (reduced) Julian day of the first data file
  epoch = ( (data[0].jd + 2400000) - 2451545.0 )/(365.25) + 2000.0
  if (keyword_set(VERBOSE)) then print, func_name + " Current epoch approximately " + fire_string(epoch)
  
  ;; Read in science catalogs (if supplied)
  if keyword_set(OBJCATS) then begin
     if verbose EQ 1 then print, func_name + ": Reading in science catalogs..."
     use_objcat = 1
     cat_objs = fire_read_cat( OBJCATS, PATH=catpath, CURR_EPOCH=epoch, VERBOSE=verbose )
     if size(cat_objs, /type) EQ 2 then begin ;; none of the input catalogs existed
     		use_objcat = 0
			fire_siren, func_name + ": ERROR! None of the user catalogs existed.  Catalogs won't be used..."
     endif else begin
     		if verbose EQ 1 then print, func_name + ": ...science catalogs read in."
     endelse
  endif else begin
     use_objcat = 0
  endelse
  
  ;; Read in telluric catalogs (if supplied)
  if keyword_set(TELLCATS) then begin
     if verbose EQ 1 then print, func_name + ": Reading in telluric catalogs..."
     use_tellcat = 1
     if size(tellcats, /type) NE 7 then begin
        ;; In this case, a string array of filenames has not been passed.  Assumption: /TELLCATS was passed instead
        ;; Read in the default telluric file
        fire_dir = getenv("FIRE_DIR")
        tellcats = fire_dir + "Utils/FIRE_tellurics.cat"
        cat_tells = fire_read_cat( TELLCATS, CURR_EPOCH=epoch, VERBOSE=verbose )
     endif else begin
        cat_tells = fire_read_cat( TELLCATS, PATH=catpath, CURR_EPOCH=epoch, VERBOSE=verbose )
     endelse
     if verbose EQ 1 then print, func_name + ": ...telluric catalogs read in."
  endif else begin
     use_tellcat = 0
  endelse

  ;; Cycle through files and determine exposure types
  for ifile=0, nfiles-1 do begin
     
     bad_hdr = 0
     hdr = fire_get(data[ifile], /HDR)
     if (n_elements(hdr) LE 1) then begin
        bad_hdr = 1
        continue
     endif
     
     file    = sxpar(hdr, "FILE")
     exptype = sxpar(hdr, "EXPTYPE")
     lamp    = sxpar(hdr, "LAMP")
     mirror  = sxpar(hdr, "MIRROR")
     grating = strtrim(sxpar(hdr, "GRISM"),2)
     object  = sxpar(hdr, "OBJECT")
     slit    = sxpar(hdr, "SLIT")
     ra		= double( sxpar(hdr, "RA") )
     dec	= double( sxpar(hdr, "DEC") )
     
     guess = ""
     
     if size(object,/type) NE 7 OR size(lamp,/type) NE 7 OR size(mirror,/type) NE 7 then begin
        bad_hdr = 1
     endif
     
     if (LOUD EQ 1) then begin
        if bad_hdr EQ 0 then begin
           print, func_name + ": analyzing " + file
           print, func_name + ": " + file + $
                  " exposure type defined in header to be: " + exptype
           print, func_name + ": Lamp status of  " + file + ": " + lamp
           print, func_name + ": Mirror status of  " + file + ": " + mirror
           print, func_name + ": Object of " + file + ": " + object
        endif else begin
           print, func_name + ": Problem with file # " + fire_string(ifile) + ": Bad header!"
           fire_append_warning, data, ifile, "Bad_header"
        endelse
     endif
     
     ;; FAIL
     if bad_hdr EQ 1 then begin
        guess = "unknown"
        data[ifile].exptype = FIRE_CHANGE_EXPTYPE_VAL(func_name, "(name unknown)", "EXPTYPE", "unknown", $
                                                      guess, INTERACTIVE=interactive, LOUD=loud, $
                                                      ORIGINAL=original)
        continue
     endif
     
     ;; LONGSLIT: We'll worry about long slit later...  Just mark these as LD for now
     if (grating NE 'Echelle') then begin
        guess = "LD"
        data[ifile].exptype = FIRE_CHANGE_EXPTYPE_VAL(func_name, file, "EXPTYPE", exptype, $
                                                      guess, INTERACTIVE=interactive, LOUD=loud, $
                                                      ORIGINAL=original)
        continue
     endif
     
     ;; DARK: Check for the dark slide being in
     if (slit EQ 'DarkSlide') then begin
        guess = "DARK"
        data[ifile].exptype = FIRE_CHANGE_EXPTYPE_VAL(func_name, file, "EXPTYPE", exptype, $
                                                      guess, INTERACTIVE=interactive, LOUD=loud, $
                                                      ORIGINAL=original)
        continue
     endif
     
     ;; ARC: Determine if the input is an arc
     if lamp EQ 'ThAr' or lamp EQ 'Ar' then begin
        if mirror EQ 'Clear' then begin
           print, func_name, ": problem with data file " $
                  + file + $
                  "--> ThAr or Ar lamp on, but mirror is clear!  Data is junk!"
           fire_append_warning, data, ifile, "ThAr or Ar lamp but no mirror"
           guess = "JUNK"
        endif else begin
           data[ifile].object = lamp
           guess = "ARC"
        endelse
        data[ifile].exptype = FIRE_CHANGE_EXPTYPE_VAL(func_name, file, "EXPTYPE", exptype, $
                                                      guess, INTERACTIVE=interactive, LOUD=loud, $
                                                      ORIGINAL=original)
        continue
     endif
     
     ;; FLAT: Determine if the input is an internal flat
     if lamp EQ 'QuartzH' then begin
        if mirror EQ 'Clear' then begin
           print, func_name, ": problem with data file " + file + $
                  "--> QuartzH lamp on, but mirror is clear!  Data is junk!"
				fire_append_warning, data, ifile, "QuartzH lamp, but no mirror"
           guess = "JUNK"
        endif else begin
           data[ifile].object = "QuartzH"
           guess = "FLAT"
        endelse
        data[ifile].exptype = FIRE_CHANGE_EXPTYPE_VAL(func_name, file, "EXPTYPE", exptype, $
                                                      guess, INTERACTIVE=interactive, LOUD=loud, $
                                                      ORIGINAL=original)
        continue
     endif


     ;;;;;;; ON-SKY FRAMES: SCIENCE OR TELLURIC ;;;;;;;;;

     if lamp EQ 'Off' then begin
        
        ;; Make sure that the mirror isn't in the way...
        if mirror EQ 'Mirror' then begin
           print, func_name, ": problem with data file " + file + $
                  "--> no lamps on, yet the mirror is in!  This is not a real frame."
           fire_append_warning, data, ifile, "Mirror in, no lamps on"
           guess = "JUNK"
	        data[ifile].exptype = FIRE_CHANGE_EXPTYPE_VAL(func_name, file, $
	                                                      "EXPTYPE", exptype, $
	                                                      guess, INTERACTIVE=interactive, $
	                                                      LOUD=loud, $
	                                                      ORIGINAL=original)
        endif else begin
           ;; Determine if the exposure is a test run: see if
           ;; 'test' is written somewhere in the object name
           if strmatch( object, "*test*", /FOLD_CASE ) AND is_empty(guess) then begin
              guess = "TEST"
              data[ifile].exptype = FIRE_CHANGE_EXPTYPE_VAL(func_name, file, $
                                                            "EXPTYPE", exptype, $
                                                            guess, INTERACTIVE=interactive, $
                                                            LOUD=loud, $
                                                            ORIGINAL=original)
              continue
           endif

           ;;;;;;;;;;;;; TELLURIC ;;;;;;;;;;;;;;;;
           
           ;; Determine if the exposure is a telluric.  
           ;; This is done before science objects, just in case the
           ;; user input science catalogs have tellurics in them

           if use_tellcat EQ 1 AND is_empty(guess) EQ 1 then begin
              ;; The most robust check for tellurics: input our
              ;; archived list of telluric RA/DEC and match by
              ;; telescope pointing.

              match = FIRE_MATCH_MAG_CAT( ra, dec, cat_tells, MATCHCUT=matchcut, $
                                          MIN_IND=min_ind, MIN_ANG=min_ang )
              if LOUD then print, "Closest telluric match: " + file + " " + object $
                                  + " " + cat_tells[min_ind].name + " ang (deg) = " $
                                  + fire_string(min_ang)
              
              if match NE -1 then begin
                 if LOUD EQ 1 then print, func_name + ": Telluric located in catalogs! file: " + $
                                          file + ", hdr obj name  = " + object + ", cat obj name = ", $
                                          cat_tells[match].name
                 
                 ;; data[ifile].obj_id = cat_tells[match].ref
                 data[ifile].object = cat_tells[match].name
                 data[ifile].magcat_comments = cat_tells[match].comments
						data[ifile].tvmags = fire_read_mags(cat_tells[match].comments,"V")
						data[ifile].tbmags = fire_read_mags(cat_tells[match].comments,"B")
                 guess = "TELL"
                 ; Change the EXPTYPE to guess="TELL"
                 data[ifile].exptype = FIRE_CHANGE_EXPTYPE_VAL(func_name, file, "EXPTYPE", exptype, $
                                                               guess, INTERACTIVE=interactive, LOUD=loud, $
                                                               ORIGINAL=original)
                 continue
              endif else begin
                 if LOUD EQ 1 then print, func_name + ": Telluric not found in catalogs! file: " + file + ", hdr obj name  = " + object
              endelse
           endif else if is_empty(guess) EQ 1 then begin
              ;; Check in the object has the appropriate prefix.  If
              ;; so, then it must be a telluric.  This is last resort
              if (FIRE_PATTERN_MATCH( object, tellnames ) EQ 1 $
                  OR strpos(strupcase(object), "HD") GE 0 OR strpos(strupcase(object), "HIP") GE 0) then begin
                 guess = "TELL"
                 data[ifile].exptype = FIRE_CHANGE_EXPTYPE_VAL(func_name, file, "EXPTYPE", exptype, $
                                                               guess, INTERACTIVE=interactive, LOUD=loud, $
                                                               ORIGINAL=original)
                 continue
              endif
           endif 

           ;;;;;;;;;;;;;;; SCIENCE! ;;;;;;;;;;;;;;;
           
           ;; Determine if the exposure is a science target
           ;; the user should enter in their observing catalog for matching.

           if use_objcat EQ 1 AND is_empty(guess) then begin
              ;; The most robust check for science objects:
              ;; input user's observing catalog
              match = FIRE_MATCH_MAG_CAT( ra, dec, cat_objs, MATCHCUT=matchcut )
              if match NE -1 then begin
                 if LOUD EQ 1 then print, func_name + ": Object located in catalogs! file: " $
                                          + file + ", hdr obj name  = " + object $
                                          + ", cat obj name = ", cat_objs[match].name
                 ;; data[ifile].obj_id = cat_objs[match].ref
                 data[ifile].object = cat_objs[match].name
                 data[ifile].magcat_comments = cat_objs[match].comments
                 guess = "SCIENCE"
                 data[ifile].exptype = FIRE_CHANGE_EXPTYPE_VAL(func_name, file, "EXPTYPE", exptype, $
                                                               guess, INTERACTIVE=interactive, LOUD=loud, $
                                                               ORIGINAL=original)
                 continue
              endif else begin
                 if LOUD EQ 1 then print, func_name + ": Object not found in catalogs! file: " $
                 + file + ", hdr obj name  = " + object
              endelse
           endif else if is_empty(guess) EQ 1 then begin
              ;; Check if the object has the appropriate prefix.  If so mark it as a science object
              if FIRE_PATTERN_MATCH( object, scinames ) EQ 1 then begin
                 guess = "SCIENCE"
                 data[ifile].exptype = FIRE_CHANGE_EXPTYPE_VAL(func_name, file, "EXPTYPE", exptype, $
                                                               guess, INTERACTIVE=interactive, LOUD=loud, $
                                                               ORIGINAL=original)
                 continue
              endif
           endif
           
           ;; If we haven't had any luck yet, then we're stuck
           ;; relying on the user's input for 'object'
           
           ;; Determine if the user simply did not change the object 
           ;; name from 'FireStarter'
           if is_empty(guess) EQ 1 AND (strmatch(object,"FIRESTARTER",/FOLD_CASE) EQ 1) then begin
              guess = object
           endif
           
           ;; Check for some kind of flat.  Since the lamp is off, this would be something like a sky/illum flat
           if strmatch( object, "*Flat*", /FOLD_CASE ) AND is_empty(guess) then begin
              guess = "FLAT"
           endif
           
           ;; Check for a long slit arc.
           if strmatch( object, "NeNeAr", /FOLD_CASE ) AND is_empty(guess) then begin
              guess = "ARC"
           endif

           ;; Check for a forced telluric.
           if strpos(strupcase(exptype), "TELL") EQ 0 AND is_empty(guess) then begin
              guess = "TELL"
           endif
           
           if is_empty(guess) then guess = 'SCIENCE'
           
           
           data[ifile].exptype = FIRE_CHANGE_EXPTYPE_VAL(func_name, file, "EXPTYPE", exptype, $
                                                         guess, INTERACTIVE=interactive, LOUD=loud, $
                                                         ORIGINAL=original)
           continue
           
        endelse ;; mirror clear
        
     endif ;; lamp off
     
  endfor
  
  RETURN
  
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; inputs an array of firestrct, and matches all science objects and tellurics within that array to arcfiles within that array
PRO FIRE_MATCH_ARCS, data, LOUD=loud, INTERACTIVE=interactive, VERBOSE=verbose
  
  func_name = "fire_match_arcs()"
  if NOT keyword_set(LOUD) then loud = 0
  if NOT keyword_set(VERBOSE) then verbose = 0
  
  ;; Determine which files require these arcs...
  obs2match = where( strmatch(strtrim(data.exptype,2), 'Science', /fold_case) OR strmatch(strtrim(data.exptype,2), 'Tell', /fold_case), nobjs )
  
	if nobjs EQ 0 then begin
		fire_siren, func_name + ": No science objects present amongst these data files!  Exiting function..."
		RETURN
	endif
  ;;   ...and the Julian dates of these objects
  jd_obs = data[obs2match].jd
  
  ;; Determine which files are the correct type for the matched objects...
  arcs = where( strmatch(strtrim(data.exptype,2), 'Arc', /fold_case), narcs )
  ;;   ...and the Julian dates of these arcs
  jd_arcs = data[arcs].jd
  
  max_diff = 4.0 ;; look for arcs within this time range (in hours)
  max_ang_off = 20.5 ;; maximum allowed offset in degrees
  
  no_arcs = 0
  for i=0, nobjs-1 do begin
     spot = obs2match[i]
     fname = data[ spot ].fitsfile
     jd1 = jd_obs[i]
     matches = where( abs( jd_arcs - jd1 ) LE (max_diff/24.0), npos )
     if npos EQ 0 then begin
        print, func_name + ": No arcs taken within " + fire_string(max_diff) + " hours of file " + fname + ". Labeling arc as unknown"
        no_arcs = no_arcs + 1
        data[ spot ].arcfits = "unknown"
        data[ spot ].num_arcs = 0
        fire_append_warning, data, spot, "No arcs taken"
        continue
     endif

      if LOUD then print, func_name + ": " + fire_string(npos) + " arc(s) found within restricted time limit of " + fire_string(max_diff) + " hours for file " + fname
      if LOUD then print, func_name + ": full list = ", data[ arcs[ matches ] ].fitsfile
      possible_arcs = data[arcs[ matches ]]
      angs = FIRE_CALC_ANGLE(DATA1=data[spot], DATA2=possible_arcs)
      closest = where( angs LE max_ang_off, nclos )
      if nclos EQ 0 then begin
         if LOUD then print, func_name + ": No arcs taken within " + fire_string(max_ang_off) + " degree(s) on the sky of file " + fname + ". Labeling arcfile as unknown"
         no_arcs = no_arcs + 1
         data[ spot ].arcfile = "unknown"
         data[ spot ].num_arcs = 0
         fire_append_warning, data, spot, "No arcs taken"
         continue
      endif
      if nclos GT 1 then begin
      	;; More than one arc matched.  Need to choose one of these.
      	;; Amongst the arcs close enough in angular offset, determine the arc taken closest in time to this file.
      	order = sort( (abs(jd_arcs(matches)-jd1))(closest) )
      	;; Determine the minimum time offset.
      	tmin = (abs(jd_arcs(matches)-jd1))(closest(order(0)) )
      	;; Determine all arcs taken within 10 minutes of this arc: these are the contenders for the match.
			contenders = where( abs( jd_arcs( matches(closest) ) - jd_arcs( matches(closest(order(0))) ) ) LT (10.0/(60.0*24.0)), ncontenders )
			;; Of these contenders, the chosen arc is the one taken last in time.
			order_contenders = sort( ((jd_arcs(matches))(closest))(contenders) )
			winner = order_contenders(ncontenders-1)
         closest = closest(contenders(winner))
      endif
      bestarc = (data[arcs[matches]])[closest]
      data[ spot ].arcfile = bestarc.fitsfile
		data[ spot ].arcs = names_to_list( data[ spot ].arcfile )
		data[ spot ].num_arcs = 1
      if LOUD then begin
         toff = (jd1 - bestarc.jd)*24.0*60.0
         angoff = angs[closest]
         print, func_name + ": " + fname + " matched with " + data[ spot ].arcfile + "; time offset = " + fire_string(toff) + " min, angular offset = " + fire_string(angoff) + " deg"
      endif
     
  endfor

  if VERBOSE EQ 1 OR LOUD EQ 1 then print, func_name + ": Unable to find arcs for " + fire_string(no_arcs) + " objects"
  
  RETURN
  
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION FIRE_CALC_TELL_WEIGHTS, angs, am_diffs, hr_diffs, max_ang_off, max_am_off, max_hr_off
	ang_weights = 1.0/max_ang_off*(max_ang_off - angs)
	bad_angs = where( angs GT max_ang_off, nbad )
	if nbad GT 0 then	ang_weights[bad_angs] = 0.0
	am_weights = 1.0/max_am_off*(max_am_off - am_diffs)
	bad_ams = where( am_diffs GT max_am_off, nbadam )
	if nbadam GT 0 then am_weights[bad_ams] = 0.0  ;; No penalty if taken within an hour
	hr_weights = 1.0/max_hr_off*(max_hr_off - (hr_diffs>1.0) )
	bad_hrs = where( hr_diffs GT max_hr_off, nbadhrs )
	if nbadhrs GT 0 then hr_weights[bad_hrs] = 0.0
	RETURN, ang_weights*am_weights*hr_weights
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; inputs an array of firestrct, and matches all science objects and tellurics within that array to arcfiles within that array
PRO FIRE_MATCH_TELLURICS, data, LOUD=loud, INTERACTIVE=interactive, VERBOSE=verbose
  
  func_name = "fire_match_tellurics()"
  if NOT keyword_set(LOUD) then loud = 0
  if NOT keyword_set(VERBOSE) then verbose = 0
  
  ;; Determine which files require telluric matches...
  obs2match = where( strmatch(strtrim(data.exptype,2), 'Science', /fold_case), nobjs )
	if nobjs EQ 0 then begin
		fire_siren, func_name + ": No science objects present amongst these data files!  Exiting function..."
		RETURN
	endif
  ;;   ...and the Julian dates of these objects
  jd_obs = data[obs2match].jd
  
  ;; Determine which files are tellurics...
  tells = where( strmatch(strtrim(data.exptype,2), 'Tell', /fold_case), ntells )
  
  if ntells EQ 0 then begin
  	fire_siren, func_name + ": No objects labeled as tellurics!  Exiting fire_match_tellurics..."
  	RETURN
  endif
  ;;   ...and the Julian dates of these tellurics
  jd_tells = data[tells].jd

  ;; Group these tellurics
  index = 0
  cut = 5.0/60.0 ;; cutoff between objects in degrees
  group = intarr(ntells,1)
  group[0] = index
  for i=1, ntells-1 do begin
     ang_off = FIRE_CALC_ANGLE(DATA1=data[tells[i]], DATA2=data[tells[i-1]])
     if( ang_off GT cut ) then index = index + 1
     group[i] = index
  endfor
  
  max_diff = 3.0         ;; look for arcs within this time range (in hours)
  max_ang_off = 90.0     ;; maximum allowed offset in degrees
  max_am_off = 1.0       ;; maximum allowed airmass difference
  
  ;; Determine the best tellurics for each object
  no_tells = 0
  for i=0, nobjs-1 do begin
     spot = obs2match[i]
     fname = data[ spot ].fitsfile
     jd1 = jd_obs[i]
     matches = where( abs( jd_tells - jd1 ) LE (max_diff/24.0), npos )
     if npos EQ 0 then begin
        print, func_name + ": No tellurics taken within " + fire_string(max_diff) + " hours of file " + fname + ". Labeling telluric as unknown"
        no_tells = no_tells + 1
        data[ spot ].tfiles = "unknown"
     endif else begin
        if LOUD then print, func_name + ": " + fire_string(npos) + " telluric(s) found within restricted time limit of " + fire_string(max_diff) + " hours for file " + fname
        if LOUD then print, func_name + ": full list = ", data[ tells[ matches ] ].fitsfile
        ;; Calculate the angles to these tellurics
        possible_tells = data[tells[ matches ]]
        angs = FIRE_CALC_ANGLE(DATA1=data[spot], DATA2=possible_tells)
        ;; Calculate the airmass differences with respect to these tellurics
        am_diffs = abs( data[spot].airmass - data[tells[ matches ]].airmass )
        ;; Calculate (somewhat arbitrary) weights based upon these angular and airmass differences
        ;; Calculate the hour differences
        hr_diffs = abs(jd_tells-jd1)*24.0
        hr_diffs = hr_diffs[matches]
        weights = FIRE_CALC_TELL_WEIGHTS( angs, am_diffs, hr_diffs, max_ang_off, max_am_off, max_diff )
        best = where( weights EQ max(weights), nbest )
        if nbest GT 1 then begin
           best = best[0]  ;; tie-breaker is first file for now
        endif
        if weights[best] EQ 0.0 then begin
 ;          if LOUD then print, func_name + ": All tellurics either more than " + fire_string(max_ang_off) + " degree(s) away [actual "+strtrim(am_diffs[matches[best]],2)+"] or more than " + fire_string(max_am_off) + " off in airmass [actual "+strtrim(angs[matches[best]],2)+"] compared to the object in " +  fname + ". Labeling tfiles as unknown"
           no_tells = no_tells + 1
           data[ spot ].tfiles = "unknown"
        endif else begin
           best_ind = (tells[matches])[best]
           bestset = tells[where( group EQ (group[matches[best]])[0] )]
           bestfiles = data[bestset].fitsfile
           data[ spot ].tfiles = names_to_list(bestfiles)
           besttell = data[best_ind]
           data[ spot ].tarcs = besttell.arcs
           data[spot].tvmags = fire_read_mags(besttell.magcat_comments,"V")
           data[spot].tbmags = fire_read_mags(besttell.magcat_comments,"B")
           if LOUD then begin
              toff = (besttell.jd - jd1)*24.0*60.0
              angoff = angs[best]
              am_off = am_diffs[best]
              print, func_name + ": " + fname + " matched with " + data[ spot ].tfiles + "; time offset (best match) = " + fire_string(toff) + " min, angular offset = " + fire_string(angoff) + " deg, airmass difference = " + fire_string(am_off)
           endif
        endelse
     endelse
     
  endfor
  
  if VERBOSE EQ 1 OR LOUD EQ 1 then print, func_name + ": Unable to find tellurics for " + fire_string(no_tells) + " objects"
  
  RETURN
  
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


PRO FIRE_ASSIGN_IDS, data, LOUD=loud, VERBOSE=verbose, INTERACTIVE=interactive

  func_name = "fire_assign_ids()"
  if NOT keyword_set(LOUD) then loud = 0
  
  ;; Determine which files require IDs...
  objs = where( strmatch(strtrim(data.exptype,2), 'Science', /fold_case), nobjs )
  if nobjs EQ 0 then begin
     fire_siren, func_name + ": No science objects present amongst these data files!  Exiting function..."
     RETURN
  endif
  
  ;; define the maximum angle separation (in degrees) allowed by a single object.
  ;; all objects within this distance will be given the same object id
  max_ang_off = 5.0/60.0
  
  obj_ids = intarr( nobjs, 1 )
  id = 1
  for i=0, nobjs-1 do begin
     data1 = data[objs[i]]
     if data1.obj_id NE -1 then continue ;; if already set, then continue on to the next object
     angs = FIRE_CALC_ANGLE( DATA1=data1, DATA2=data[objs] )
     same_obj = where( angs LT max_ang_off )
     data[objs[same_obj]].obj_id = id
     if LOUD EQ 1 then begin
        print, func_name + ": Assigned obj_id = " + fire_string(id) + " to objects with these object tags:"
        print, data[objs[same_obj]].object
     endif
     id = id + 1
  endfor
  
  RETURN
  
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


PRO FIRE_SET_FLAT_TAG, data, flatfiles, flats, flatnums, region, $
                       PIXFLATS=pixflats, ILLUMFLATS=illumflats, $
                       OMASKS=omasks, OSTRS=ostrs, WIDGET=widget
  
  func_name = "fire_set_flat_tag()"
  type = 'UNKNOWN'

  ;; Determine the ranges of the fitsfile numbers for the input pix flat files
  if is_undefined(flatfiles) EQ 1 then begin
		bad = 1
	endif else if is_empty(flatfiles) EQ 1 then begin
		bad = 1
	endif else begin
		bad = 0
     nflatfiles = n_elements(flatfiles)
     flatregs = intarr(nflatfiles, 1)
     for i =0, nflatfiles-1 do begin

        ;; Determine the min and max fitsfile numbers for this file
        FIRE_RANGE_FROM_NAME, flatfiles[i], min, max
 
        ;; Match up this min and max with a location within the data structure
        spot = where(flatnums EQ min, nmatch)
        if nmatch EQ 0 then begin
           print, func_name + ": WARNING: flat file " + fire_string(min) $
                  + " (" + flatfiles[i] + ") not labeled as flat file!  Might be an illumflat masquerading as a telluric."
           flats = where( strmatch(strtrim(data.exptype,2), 'Flat', /fold_case) OR strmatch(strtrim(data.exptype,2), 'Tell', /fold_case) )
           flatnums1 = FIX(FIRE_GET_FITNUM(data[flats].fitsfile))
           spot = where(flatnums1 EQ min, nmatch)
           if nmatch GT 0 then begin
              fire_siren, func_name + ": Telescope was pointing at a telluric during the illumination flat.  Continuing on..."
           endif else begin
              print, func_name + ": Nope.  Not sure what's going on here. punting on this flat file..."
              continue
           endelse
           minspot = flats(where(flatnums1 EQ min))
           data[minspot].object = 'IllumFlat'
           data[minspot].exptype = 'FLAT'
           fire_append_warning, data, minspot, "Telluric in slit during this sky flat"
           dontcheck = 1
        endif else begin
           tmp = where(flatnums EQ min, test)
           if (test GT 0) then begin
              minspot = flats(where(flatnums EQ min))
           endif else begin
              minspot = min(flats(where(flatnums GT min)))
           endelse
           
           tmp = where(flatnums EQ max, test)
           if (test GT 0) then begin
              maxspot = flats(where(flatnums EQ max))
           endif else begin
              maxspot = max(flats(where(flatnums LT max)))
           endelse

           dontcheck = 0
        endelse
        
        ;; Determine the flat region for this location
        reg = (region[minspot])[0]
        if dontcheck EQ 0 then begin
           if reg NE (region[maxspot])[0] then begin
              print, func_name + ": bad processed flat file (" + flatfiles[i] $
                     + ")! Spans more then one region.  Exiting with error...'
              RETURN
           endif
        endif
        
        ;; Set the value of the flats and the pixflat name to the appropriate values
        flatregs[i] = reg
        regfiles = where(region EQ reg, nreg)
        if nreg NE 0 then begin
           if keyword_set(PIXFLATS) then begin
              data[regfiles].pixflatfile = flatfiles[i]
           endif else if keyword_set(ILLUMFLATS) then begin
              data[regfiles].illumflatfile = flatfiles[i]
           endif else if keyword_set(OMASKS) then begin
              data[regfiles].orderfile = flatfiles[i]
           endif else if keyword_set(OSTRS) then begin
              data[regfiles].ordr_str_file = flatfiles[i]
           endif 
        endif else begin
           print, func_name + ": funny result.  file does not appear to have a region..."
        endelse
        
     endfor
     
     ;; Cycle through the data and make sure that each file was allocated flatfiles.
     nfiles = n_elements(data)
     num_bad = 0
     for j=0, nfiles-1 do begin
        
        if keyword_set(PIXFLATS) then begin
           val = data[j].pixflatfile
        endif else if keyword_set(ILLUMFLATS) then begin
           val = data[j].illumflatfile
        endif else if keyword_set(OMASKS) then begin
           val = data[j].orderfile
        endif else if keyword_set(OSTRS) then begin
           val = data[j].ordr_str_file
        endif 
        
        ;; If not, then set the value to that of the flatfile with the closest region
        ;; Forget about long slit files for now.
        if is_fits(val) EQ 0 AND data[j].exptype NE 'LD' then begin
           ;; Determine the closest region
           diffs = flatregs - region[j]
           best_diff = MIN( diffs )
           if n_elements(best_diff) GT 1 then best_diff = best_reg[0]
           best_file = where( diffs EQ best_diff )
           if n_elements(best_file) GT 1 then best_file = best_file[0]
           if keyword_set(PIXFLATS) then begin
              data[j].pixflatfile = flatfiles[best_file]
              type = "pix flat file"
           endif else if keyword_set(ILLUMFLATS) then begin
              data[j].illumflatfile = flatfiles[best_file]
              type = "illum flat file"
           endif else if keyword_set(OMASKS) then begin
              data[j].orderfile = flatfiles[best_file]
              type = "orderfile flat file"
           endif else if keyword_set(OSTRS) then begin
              data[j].ordr_str_file = flatfiles[best_file]
              type = "order structure file"
           endif else begin
              type = "unknown file type"
           endelse
           num_bad = num_bad+1
           fire_append_warning, data, j, "Questionable match with " + type
        endif
        
     endfor
     
     if num_bad NE 0 then begin
        fire_siren, func_name + ": Warning: " + fire_string(num_bad) $
                    + " files are matched with a " + type $
                    + " taken after the slit was changed."
     endif
     
  endelse
  
  if bad EQ 1 then begin
     if keyword_set(PIXFLATS) then begin
			file_type = "pixel flat"
        type = "PIXFLATS"
     endif else if keyword_set(ILLUMFLATS) then begin
			file_type = "illum flat"
        type = "ILLUMFLATS"
     endif else if keyword_set(OMASKS) then begin
			file_type = "order mask"
        type = "OMASKS"
     endif else if keyword_set(OSTRS) then begin
			file_type = "order structure"
        type = "OSTRS"
     endif else begin
			file_type = ""
        type = "UNKNOWN"
	  endelse

		warning_msg = func_name + ": " + file_type + " files not defined!  Exiting without matching flats.  "
     
     ;; Print warning to terminal
     if NOT keyword_set(WIDGET) then begin
	     fire_siren, warning_msg + $
	                 "Did you remember to set the " + type + " keyword when running fire_mkstrct.pro?"
		endif else begin
		;; Print warning to firehose GUI
	     fire_siren, warning_msg + $
	                 "Did you remember to fill in the " + file_type + " box in the GUI?", $
	                 WIDGET=widget, /both, /append
		endelse
     
	endif
  
  RETURN
  
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Match science objects with flats
PRO FIRE_MATCH_FLATS, data, PIXFLATS=pixflats, ILLUMFLATS=illumflats, $
                      OMASKS=omasks, OSTRS=ostrs, LOUD=loud, $
                      VERBOSE=verbose, INTERACTIVE=interactive, WIDGET=widget

	func_name = "fire_match_flats()"
	if NOT keyword_set(LOUD) then loud = 0
	print, "loud: ", loud
	;; Cycle through the data and find the different flat regions
	nfiles = n_elements(data)
	region = intarr(nfiles, 1)
	index = 0
	previous = 'Nonsense'
	for i=0, nfiles-1 do begin
		slit = data[i].slit
		if size(slit, /type) NE 7 OR is_empty(slit) EQ 1 OR strmatch(strtrim(slit,2),'unknown', /FOLD_CASE) EQ 1 then begin
			;; in this case, there's an issue with the header
			slit = "unknown"
		endif else if slit NE previous AND ( strmatch(slit, 'DARK', /FOLD_CASE) EQ 0 AND slit NE '0.75' ) then begin
			index = index + 1
			previous = slit
			if LOUD EQ 1 then print, func_name + ": Flat region changed.  File index = " $
				+ fire_string(i) + ".  New region index = " + fire_string(index) + ", new slit value = " + slit
		endif
		region[i] = index
	endfor

	;; Determine the numbers of the fitsfiles associated with all flat files
        flats = where(strpos(strupcase(data.exptype), 'FLAT') NE -1, nflats)

        if (nflats EQ 0) then begin
           print, "ERROR: no raw flat files found"
           if (n_elements(pixflats) EQ 1 AND n_elements(illumflats) EQ 1 AND $
               n_elements(omasks) EQ 1) then begin
              
              print, "I will try to asign known flats to your data, OK? (Y/N)"
              data.pixflatfile = pixflats[0]
              data.illumflatfile = illumflats[0]
              data.orderfile = omasks[0]
           endif else begin
              print, "Go make yourself some flats...I can't find any raw or processed ones."
              print, " "
              retall
           endelse
        endif else begin
           flatnums = FIX(FIRE_GET_FITNUM(data[flats].fitsfile))
           FIRE_SET_FLAT_TAG, data, pixflats, flats, flatnums, region, WIDGET=widget, /PIXFLATS
           FIRE_SET_FLAT_TAG, data, illumflats, flats, flatnums, region, WIDGET=widget, /ILLUMFLATS
           FIRE_SET_FLAT_TAG, data, omasks, flats, flatnums, region, WIDGET=widget, /OMASKS
        endelse	

	;; Determine the name of the order structure files from the names of the assigned
	;; order files.  (Simply replace "Orders_" with "OStr_fire_"

	for i=0, nfiles-1 do begin
		tmp = strsplit(data[i].orderfile, "Orders_", /EXTRACT, /REGEX)
		tmp1 = tmp[1]
		string_replace, tmp1, 'to', '_'
		tmp[1] = tmp1
		if n_elements(tmp) EQ 2 then begin
			tmp = strjoin(tmp, "OStr_")
			;; Check if this file was chosen by the user.
			if total( strmatch(ostrs, tmp), /integer ) GT 0 then begin
				data[i].ordr_str_file = tmp
			endif else begin
				fire_siren, func_name + ": ERROR! Order structure file " + tmp + " does not exist!" + $
					" Leaving ordr_str_file tag empty..."
				data[i].ordr_str_file = " "
			endelse			
		endif else begin
			;; In this case, the orderfile was most likely not determined.
			data[i].ordr_str_file = " "
		endelse
	endfor
		
	;; Determine the name of the pixel image files from the names of the assigned
	;; pixel flat files.  (Simply replace "Pixflat_" with "piximg_flats_"
	for i=0, nfiles-1 do begin
		tmp = strsplit(data[i].pixflatfile, "Pixflat_", /EXTRACT, /REGEX)
		tmp1 = tmp[1]
		string_replace, tmp1, 'to', '_'
		tmp[1] = tmp1

		if n_elements(tmp) EQ 2 then begin
			tmp = strjoin(tmp, "Pixflat_")
			;tmp = strjoin(tmp, "piximg_flats_")
			;; Check if this file exists.
			if FILE_TEST( tmp ) EQ 1 then begin
				data[i].piximgfile = tmp
			endif else begin
				fire_siren, func_name + ": ERROR! Pixel image file " + tmp + " does not exist!" + $
					" Leaving piximgfile tag empty..."
				data[i].piximgfile = " "
			endelse			
		endif else begin
			;; In this case, the orderfile was most likely not determined.
			data[i].piximgfile = " "
		endelse	
	endfor

	RETURN

END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; verbose: prints status information to screen
;; loud: prints much more information to screen (only recommended for debugging)

PRO FIRE_MKSTRCT, data, RAWPATH=rawpath, VERBOSE=verbose, $
				  LOUD=loud, INTERACTIVE=interactive, ORIGINAL=original, $
                  TELLNAMES=tellnames, SCINAMES=scinames, CATPATH=catpath, $
                  OBJCATS=objcats, TELLCATS=tellcats, PIXFLATS=pixflats, $
                  ILLUMFLATS=illumflats, OMASKS=omasks, OSTRS=ostrs, WIDGET=widget
  prog_name = "fire_mkstrct.pro"
  
	if NOT keyword_set(LOUD) then LOUD = 0
	if NOT keyword_set(VERBOSE) then VERBOSE = 0
	
	print, "LOUD: ", loud
  ;; If not provided, prompt the user for the data path
  if NOT keyword_set( RAWPATH ) then begin
     path = DIALOG_PICKFILE(/DIRECTORY, title="Please select the data directory")
     if is_empty(path) then begin
        fire_siren, prog_name + ": ERROR with chosen path (empty string)." + $
						"User must have exited out of window.  Exiting program gracefully."
        RETURN
     endif
  endif else begin
     path = rawpath
  endelse
  print, prog_name, ": Data directory input as ", path
  
  print, prog_name, ": Retrieving files in this directory..."
  cmd = "ls "+strtrim(path,2)+"fire*.fits"
  spawn, cmd, files
  nfiles = n_elements(files)
  print, prog_name, ":  ...", fire_string(nfiles), " retrieved"
  
  ;; Create the structures
  tmp = fire_create_struct()
  data = replicate(tmp, nfiles)


  ;; Cycle through all files and populate these structures based upon the
  ;; information stoed in the headers of these fit files.
  for ifile=0, nfiles-1 do begin

     split = (strsplit(files[ifile],"/",/EXTRACT))
     data[ifile].fitsfile = split[n_elements(split)-1]

     hdr = headfits(files[ifile], /SILENT)
     
     if size(hdr, /type) NE 7 then begin
        data[ifile].exptype = 'ERROR'
        fire_append_warning, data, ifile, 'Bad header'
        continue
     endif
     
     a = data[ifile]
     a.rawpath   = path
     a.hdr       = hdr
     a.run = sxpar(hdr, "NIGHT")
     exptime = fire_get(a, /EXPTIME, /QUIET)
     object = fire_get(a, /OBJECT, /QUIET)
     ra_deg = fire_get(a, /RA_DEG, /QUIET)
     dec_deg = fire_get(a, /DEC_DEG, /QUIET)
     airmass = fire_get(a, /AIRMASS, /QUIET)
     type     = fire_get(a, /EXPTYPE, /QUIET)
     jd = fire_get(a, /JD, /QUIET)
     slitstring = fire_get(a, /SLIT, /QUIET)
     
     if size(slitstring, /type) EQ 2 then begin
        a.slit = 'unknown'
     endif else if strmatch(slitstring, '0') EQ 1 then begin
        a.slit = 'unknown'
     endif else if (slitstring EQ 'DarkSlide') then begin
        a.slit      = 'DARK'
     endif else begin
        a.slit = (strsplit(slitstring,'_',/extract))[0]
     endelse
     
     ;if a.slit EQ '0' then stop
     
     data[ifile] = a ;; load this back into the structure.
     
  endfor
  
  ;; Set the exposure type using crude set of rules.
 
  FIRE_GUESS_EXPTYPE, data, LOUD=loud, VERBOSE=verbose, INTERACTIVE=interactive, $
                      ORIGINAL=original, TELLNAMES=tellnames, $
                      SCINAMES=scinames, CATPATH=catpath, OBJCATS=objcats, TELLCATS=tellcats
  
  ;; Match certain objects with arcs
  FIRE_MATCH_ARCS, data, LOUD=loud, INTERACTIVE=interactive
 
  ;; Match certain objects with tellurics
  FIRE_MATCH_TELLURICS, data, LOUD=loud, VERBOSE=verbose, INTERACTIVE=interactive

  ;; Match science objects with flats
  FIRE_MATCH_FLATS, data, PIXFLATS=PIXFLATS, ILLUMFLATS=illumflats, $
                    OMASKS=omasks, OSTRS=ostrs, LOUD=loud, $
                    VERBOSE=verbose, INTERACTIVE=interactive, WIDGET=widget

  ;; Attach IDs to all science objects
  FIRE_ASSIGN_IDS, data, LOUD=loud, VERBOSE=verbose, INTERACTIVE=interactive

  ;; Determine wavelength calibration method (OH lines or ThAr/Ar) for all science files
	scis = fire_get_scis(data, spots=spots, NSPOTS=nspots)
	min_time_for_OH = 300
	if nspots GT 0 then begin
		OH_spots = where( data(spots).exptime GE min_time_for_OH, noh, COMPLEMENT=INTERNAL_spots, NCOMPLEMENT=ninternal )
		if noh GT 0 then begin
			data( spots(OH_spots) ).OH_cal = 1
		endif
		if ninternal GT 0 then begin
			data( spots(INTERNAL_spots) ).OH_cal = 0
		endif
	endif
    
  ;; Fill in the names of the object structure files
  FIRESTRCT_FILL_FILENAMES, data, /OBJSTRFILE, /QUIET

	;; Update the flat information in the data structure
  update_firestrct_flat_arc_info, data	

  ;; Update telluric information (ie, B and V mags) stored in the data structure
  update_firestrct_tell_info, data

  ;; Set the skymodel to an empty space so that the structure may be saved correctly
  ;data.skymodfile = " "

  ;; Print the results to screen
  if LOUD EQ 1 then begin
  	
                                ; Print out a summary.
     for ifile=0, nfiles-1 do begin
        if (data[ifile].obj_id NE -1) then begin
           print, data[ifile].fitsfile, data[ifile].object, data[ifile].slit, float(data[ifile].airmass), data[ifile].exptype, data[ifile].tfiles, data[ifile].arcfile, data[ifile].obj_id, format='(%"%15s %-17s%10s%7.3f%10s  %-15s  %-15s %3d")'
        endif else begin
           print, data[ifile].fitsfile, data[ifile].object, data[ifile].slit, float(data[ifile].airmass), data[ifile].exptype, data[ifile].tfiles, data[ifile].arcfile, format='(%"%15s %-17s%10s%7.3f%10s  %-15s  %-15s")'
        endelse
     endfor
  endif
  ;;mwrfits, data, "firestrct.fits", /create
  
  RETURN
end
