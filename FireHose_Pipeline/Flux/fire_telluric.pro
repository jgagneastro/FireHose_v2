pro fire_telluric_initcommon

  common fire_telluric_common, tel_indx, telobjs, telluric

end

pro fire_telluric_event, ev

  common fire_telluric_common

  WIDGET_CONTROL, ev.top, get_uvalue = state, /no_copy
  WIDGET_CONTROL, ev.id, get_uvalue = uval
  
  case uval of
      'LIST': 
      'PLOTTELL': fire_telluric_display, state
      'COMBINE': fire_telluric_combine, state
      'CANCEL': begin
          widget_control, ev.top, /destroy
          user_cancel=1
          return 
      end
      'DONE' : begin
         fire_telluric_combine, state
         widget_control, ev.top, /destroy
         return
      end
      else:
  endcase

;
  WIDGET_CONTROL, state.base_id, set_uvalue = state, /no_copy
  return
end
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro fire_telluric_display, state

  common fire_telluric_common

  tel_indx = widget_info(state.list_id, /list_select)
;  print, telobjs[tel_indx]

  if (n_elements(tel_indx) EQ 1) then begin
     objstr = xmrdfits(telobjs[tel_indx[0]],1)
     x_splot, objstr.wave, objstr.fx, title="Individual telluric extractions", /block
  endif

  if (n_elements(tel_indx) EQ 2) then begin
     objstr = xmrdfits(telobjs[tel_indx[0]],1)
     objstr2 = xmrdfits(telobjs[tel_indx[1]],1)
     x_splot, objstr.wave, objstr.fx, ytwo=objstr2.fx, title="Individual telluric extractions", /block
  endif

  if (n_elements(tel_indx) gt 2) then begin
     print, "Only two spectra may be over-plotted at a time.  Displaying first two in list..."
     objstr = xmrdfits(telobjs[tel_indx[0]],1)
     objstr2 = xmrdfits(telobjs[tel_indx[1]],1)
     x_splot, objstr.wave, objstr.fx, ytwo=objstr2.fx, title="Individual telluric extractions", /block
  endif


end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro fire_telluric_combine, state

  common fire_telluric_common

	func_name = "fire_telluric_combine"

  tel_indx = widget_info(state.list_id, /list_select)
	if n_elements(tel_indx) EQ 1 then begin
		if tel_indx EQ -1 then begin
			fire_siren, func_name + ": No objects chosen!  Aborting!"
			RETURN
		endif
	endif

  if (n_elements(tel_indx) EQ 1) then begin
     telluric = telobjs[tel_indx[0]]
  endif else begin
  		;!@!@!@!@!@!
  		filelist = telobjs( tel_indx )
  		cd,current=dir0
  		dir0 += path_sep()+file_dirname(filelist[0])+path_sep()
  		spos = strpos(file_basename(filelist[0]),'_')
  		pref = strmid(file_basename(filelist[0]),0,spos)+'_'
  		nums = strjoin(strtrim(long(file_basename(file_basename(strmid(file_basename(filelist),spos+1),'.gz'),'.fits')),2),',')
  		;outf = pref+'xcomb_'+strjoin(strtrim(long(file_basename(file_basename(strmid(file_basename(filelist),spos+1),'.gz'),'.fits')),2),'_')+'.fits'
  		outf = pref+strjoin(file_basename(file_basename(strmid(file_basename(filelist),spos+1),'.gz'),'.fits'),'_')
  		telluric = 'Object/'+outf+'.fits'
  		
  		if ~file_test(telluric) then begin
  		  fire_xcombspec,path_input=dir0,/fits_ext,prefix_input=pref,files_input=nums,save_input=outf, /BLOCKING, NPIX=12
        stop
  		endif
  		
  		;Old way
  		;telluric = fire_stdcombine( filelist, sigrej=3, /chk )
  endelse

  ;if (n_elements(tel_indx) EQ 2) then begin
  ;   filelist = [telobjs[tel_indx[0]],telobjs[tel_indx[1]]]
  ;   telluric = fire_stdcombine(filelist, sigrej=3, /chk)
  ;endif

  return

end


PRO fire_telluric_filenames, telluric, outfile, outfile1


	if (strpos(telluric,'/') NE -1) then begin
	   telluric = (strsplit(telluric,'/',/Extract))[1]
	endif
	outfile = telluric
	strput, outfile, "Tel", 0
	outfile1 = ((strsplit(outfile,'.', /extract))[0])
	outfile  = ((strsplit(outfile,'.', /extract))[0])+'_tellspec.fits'

	RETURN

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro fire_telluric, fire, objstrfile=objstrfile, CLOBBER=clobber, NAME=name, QUICK=quick, rawpath=rawpath, NOAPPEND=noappend
;; name (optional): name of object (string)

  func_name = "fire_telluric"

  common fire_telluric_common

  fire_telluric_initcommon

  state = { $
          indx: -1L, $
          list_id: 0L, $
          base_id: 0L $
          }

  if keyword_set(CLOBBER) then begin
     if keyword_set(QUICK) then begin
        print, func_name + ": /Clobber eradicates /Quick!  Un-setting keyword QUICK."
        fire_undefine, quick
     endif
  endif
  
  if keyword_set(QUICK) then print, func_name + ": /QUICK option detected"
     
  if (NOT keyword_set(OBJSTRFILE)) then begin
     fire_siren, func_name + ": ERROR: No object structure found!"
     return
  endif

  if (keyword_set(NOAPPEND) EQ 0) then begin
     objstr = 'Object/'+strtrim(objstrfile,2)
  endif else begin
     objstr = objstrfile
  endelse
  

  if (FILE_TEST(objstr) EQ 0) then begin
     fire_siren, func_name + "ERROR: Object structure file does not exist!"
     print, objstr
     return
  endif

  tmpobj = objstrfile

  struct_entry = where(strtrim(fire.objstrfile,2) EQ objstr, nmatch)
  num_tells = fire[struct_entry[0]].num_tfiles

  if (num_tells EQ 0) then begin
     tmpnum = n_elements(strsplit(fire[struct_entry[0]].tfiles, ';', /extract))
     if (tmpnum NE num_tells) then begin
        fire[struct_entry[0]].num_tfiles = tmpnum        
        num_tells = tmpnum
     endif
  endif

  

;;;; Combine multiple exposures of the same telluric; let the user
;;;; choose which exposures to use.

  telobjs = strarr(num_tells)

  if (num_tells GT 1) then begin
    
     ; Grab the possible telluric obstr file names
     for itel=0, num_tells-1 do begin
     print,itel,num_tells-1

        telobjs[itel] = fire_get(fire[struct_entry[0]],/TOBJSTRFILE, std=itel+1)
        filenum = (strsplit(telobjs[itel], '_.', /extract))[1]
;        matchstr = "Object/Tel*"+strtrim(filenum,2)+"*"
        matchstr = "Object/Obj*"+strtrim(filenum,2)+"*"
        matchfiles = file_search(matchstr)
        if (n_elements(matchfiles) GT 1 OR matchfiles[0] NE '') then begin
           if (n_elements(tellfiles) EQ 0) then begin
              tellfiles = matchfiles
           endif else begin
              tellfiles = [tellfiles, matchfiles]
           endelse
        endif
     endfor

     ntellobjs = n_elements(tellfiles)
     if (ntellobjs EQ 1) then begin		
        telluric = tellfiles[0]
     endif else if (ntellobjs GT 1) AND NOT keyword_set(QUICK) then begin
        
        print, "Multiple tellurics"
        
        ;;Make a little widget for choosing which ones to combine/use
        base = WIDGET_BASE( title = 'fire_telluric', /column, $
                            xoffset=xoffset,yoffset=yoffset)
        state.base_id = base
        
        msg = WIDGET_LABEL(base, Value='Multiple exposures found for this calibrator.')
        msg = WIDGET_LABEL(base, Value='Select exposures to view or combine and')
        msg = WIDGET_LABEL(base, Value='click continue to move on to telluric')
        msg = WIDGET_LABEL(base, Value='calibration.')
        toolbar = WIDGET_BASE( state.base_id, /row, /frame, /base_align_center, /align_center)
        chg_objid = WIDGET_BUTTON(toolbar, value='View Extraction',uvalue='PLOTTELL', /align_right)
        ;; chg_type = WIDGET_BUTTON(toolbar, value='Combine Exposures',uvalue='COMBINE', /align_right)
        state.list_id = widget_list(base, value=telobjs, xsize=40L, ysize=5L, $
                                    uvalue = 'LIST', /MULTIPLE)
        cancel = WIDGET_BUTTON(toolbar, value='Cancel',uvalue='CANCEL', /align_right)
        cancel = WIDGET_BUTTON(toolbar, value='Continue',uvalue='DONE', /align_right)
        WIDGET_CONTROL, base, /realize
        WIDGET_CONTROL, base, set_uvalue=state, /no_copy
        xmanager, 'fire_telluric', base

     endif else if (ntellobjs GT 1) AND keyword_set(QUICK) then begin
        
        ;; keyword /QUICK was passed: choose the most recently created telluric object structure from the bunch.
        print, func_name + ": Multiple telluric files found (" + fire_str_array_to_str(tellfiles) + ").  Keyword /QUICK set." + $
               "  Will use the most recently created file." 
        
        stats = file_info(tellfiles)
        best = max( stats.ctime, max_ind )
        best_time = systime(0,stats(max_ind).ctime)
        telluric = tellfiles(max_ind)
        print, func_name + ": Best match is " + fire_string(telluric) + ", created " + best_time
        
        ;; Check if this telluric correction file already exists.  If not, then the /quick option is not allowed!
        fire_telluric_filenames, telluric, outfile, outfile1
        tellcorfile = 'Object/'+strtrim(outfile,2)
        if( x_chkfil(tellcorfile, /SILENT) EQ 0) then begin
           ;; File doesn't exist!  /QUICK is not allowed.  Re-run this code, but without the /QUICK flag
           print, func_name + ": Telluric correction file " + tellcorfile + " does not exist, so /QUICK is not allowed!  " + $
                  "Starting over, but without the /QUICK flag this time..."
           fire_telluric, fire, objstrfile=objstrfile, CLOBBER=clobber, NAME=name
           RETURN
        endif
        
        ;; Otherwise, we're in the clear...
        fire_siren, func_name + ": Using telluric structure " + fire_string(tellfiles(max_ind)) + ", created " + fire_string(best_time) + $
                    ".  If this is not what you want, then turn off the /QUICK option and try again."
        
     endif else begin
        fire_siren, func_name + ": ERROR! No telluric object structures found!  Exiting."
        RETURN
     endelse
     
  endif else begin
                                ; Case where only 1 file available
     telobjs[0] = fire_get(fire[struct_entry[0]],/TOBJSTRFILE, std=1)
     telluric   = telobjs[0]
  endelse
  
  B = fire[struct_entry[0]].tbmags
  V = fire[struct_entry[0]].tvmags
  if n_elements(telluric) eq 0 then return
  filenum = (strsplit(telluric, '_.', /extract))[1]
  
  ;; Get the output filenames  
  fire_telluric_filenames, telluric, outfile, outfile1
  tellcorfile = 'Object/'+strtrim(outfile,2)

  if (x_chkfil(tellcorfile, /SILENT) EQ 0) OR keyword_set(CLOBBER) then begin
  		print, func_name + ": Telluric correction file " + tellcorfile + " does not exist or /CLOBBER passed.  Running fire_xtellcor..."
     fire_xtellcor, object=tmpobj, userpath="Object/", telluric=telluric, bmag=B, vmag=V, outfile=outfile1, name=name, rawpath=rawpath
  endif else begin
  		print, func_name + ": Telluric correction file " + tellcorfile + " exists!  Running fire_xtellcor_finish..."
     fire_xtellcor_finish, tmpobj, userpath="Object/", telluric=outfile, name=name, QUICK=quick
  endelse     
     
end
