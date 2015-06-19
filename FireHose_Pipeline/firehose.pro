PRO fire_event, ev

COMMON share, firestrct, $
   wpixmode, arcmode, skymode, skyreset, findmode, extmode, $
   fire_pipe_clobber, arcverb, findverb, extverb

COMPILE_OPT hidden
  
	func_name = "fire_event"

  widget_control, ev.id, get_uvalue=uvalue ;event handler


;; Programming Note:  uvalue here should really be uname!  This would have freed up the uvalue slot
;; to be used for other parameter values of any type (whereas uname has to be a string).  We'll live
;; with the current convention...

IF N_ELEMENTS(uvalue) EQ 1 AND size(uvalue, /type) NE 8 THEN CASE uvalue OF
   'list':
   'tab':
   
   ;; MENU Buttons
   'quit':  BEGIN
   ans = dialog_message("Extinguish fire (quit firehose)?  ", /Question, /center)
   if (ans EQ "Yes") then begin
	WIDGET_CONTROL, ev.TOP, /DESTROY
	return
   endif else begin
   	WIDGET_CONTROL, ev.top, get_uvalue=s
   	WIDGET_CONTROL, s.wtext, set_value="Action aborted by user.  Thanks for stickin' around.", /append
   endelse   
   END
   
   'clear': BEGIN
   	WIDGET_CONTROL, ev.top, get_uvalue=s
   	WIDGET_CONTROL, s.wtext, set_value="All clear..."
   END
   'pwd': BEGIN
   	WIDGET_CONTROL, ev.top, get_uvalue=s
   	spawn, 'pwd', cwd
   	fix_path, cwd
      WIDGET_CONTROL, s.wtext, set_value='Current working directory is '+cwd, /append
   END
   'raw': BEGIN
   	WIDGET_CONTROL, ev.top, get_uvalue=s
      WIDGET_CONTROL, s.setupdir, get_value=rawpath
      WIDGET_CONTROL, s.wtext, set_value='Current raw data directory is '+rawpath, /append
   END
   'website': BEGIN
   	WIDGET_CONTROL, ev.top, get_uvalue=s	
		WIDGET_CONTROL, s.wtext, set_value='FIRE website: https://wikis.mit.edu/confluence/display/FIRE/FIRE+Data+Reduction', /append   
   END
	'fire_logo': BEGIN
   	WIDGET_CONTROL, ev.top, get_uvalue=s	
		WIDGET_CONTROL, s.wtext, set_value='FIRE website: https://wikis.mit.edu/confluence/display/FIRE/FIRE+Data+Reduction', /append
	END
   'about': about = DIALOG_MESSAGE(['FIRE Spectral Extractor v0.01 - Written by John Bochanski, Adam Burgasser, Mike Matejek, Rob Simcoe', '', 'For more information:  simcoe@space.mit.edu'], title='About')

   'arcsdir': BEGIN
      WIDGET_CONTROL, ev.top, get_uvalue=s
      WIDGET_CONTROL, s.reduxdir, get_value=reduxpath
      dir = strtrim(reduxpath,2)+'Arcs'+path_sep()
      cmd = 'ls '+ dir
      spawn, cmd, vals
      WIDGET_CONTROL, s.wtext, set_value="", /append
      WIDGET_CONTROL, s.wtext, set_value='Contents of Arcs/ directory (' + dir + '):', /append
      WIDGET_CONTROL, s.wtext, set_value=fire_str_array_to_str(vals), /append
      WIDGET_CONTROL, s.wtext, set_value="", /append
   END
   'finaldir': BEGIN
      WIDGET_CONTROL, ev.top, get_uvalue=s
      WIDGET_CONTROL, s.reduxdir, get_value=reduxpath
      dir = strtrim(reduxpath,2)+'Final'+path_sep()
      cmd = 'ls '+ dir
      spawn, cmd, vals
      WIDGET_CONTROL, s.wtext, set_value="", /append		
      WIDGET_CONTROL, s.wtext, set_value='Contents of Final/ directory (' + dir + '):', /append
      WIDGET_CONTROL, s.wtext, set_value=fire_str_array_to_str(vals), /append
      WIDGET_CONTROL, s.wtext, set_value="", /append
   END
   'flatdir': BEGIN
      WIDGET_CONTROL, ev.top, get_uvalue=s
      WIDGET_CONTROL, s.reduxdir, get_value=reduxpath
      dir = strtrim(reduxpath,2)+'Flat'+path_sep()
      cmd = 'ls '+ dir
      spawn, cmd, vals
      WIDGET_CONTROL, s.wtext, set_value="", /append
      WIDGET_CONTROL, s.wtext, set_value='Contents of Flat/ directory (' + dir + '):', /append
      WIDGET_CONTROL, s.wtext, set_value=fire_str_array_to_str(vals), /append
      WIDGET_CONTROL, s.wtext, set_value="", /append
   END
   'objectdir': BEGIN
      WIDGET_CONTROL, ev.top, get_uvalue=s
      WIDGET_CONTROL, s.reduxdir, get_value=reduxpath
      dir = strtrim(reduxpath,2)+'Object'+path_sep()
      cmd = 'ls '+ dir
      spawn, cmd, vals
      WIDGET_CONTROL, s.wtext, set_value="", /append
      WIDGET_CONTROL, s.wtext, set_value='Contents of Object/ directory (' + dir + '):', /append
      WIDGET_CONTROL, s.wtext, set_value=fire_str_array_to_str(vals), /append
      WIDGET_CONTROL, s.wtext, set_value="", /append
   END
   
   'timesaved': BEGIN
      WIDGET_CONTROL, ev.top, get_uvalue=s
      WIDGET_CONTROL, s.strctname, get_value=file
      out = file_info(file)   
      if out.exists EQ 0 then begin
         fire_siren, func_name + ": firestrct file named " + file + " does not exist!" + $
                     "  Please use the 'Structure' tag to create this structure.", widget = s.wtext, /append
      endif else begin
      	ctime = systime(0, out.ctime)
			WIDGET_CONTROL, s.wtext, set_value='Time ' + file + ' last saved: ' + ctime, /append
      endelse
   END
   
   'default_raw': BEGIN
      WIDGET_CONTROL, ev.top, get_uvalue=s
      spawn, 'pwd', cwd
      fix_path, cwd
      arr = strsplit(cwd, path_sep())
      rawdir = cwd+strtrim('Raw'+path_sep(),2)		
      if (x_chkfil(rawdir, /SILENT) EQ 0) then begin ;; Default Raw directory doesn't exist.
         rawdir = cwd
      endif
                                ; Determine the new path
      WIDGET_CONTROL, s.setupdir, set_value=rawdir	
      WIDGET_CONTROL, s.wtext, set_value='Raw path has been updated to '+rawdir, /append
      WIDGET_CONTROL, s.wtext, set_value='Current working directory is '+cwd, /append
   END

   'default_redux': BEGIN
      WIDGET_CONTROL, ev.top, get_uvalue=s
      spawn, 'pwd', cwd
      fix_path, cwd
      reduxdir = strtrim(cwd,2)
      WIDGET_CONTROL, s.reduxdir, set_value=reduxdir	
      WIDGET_CONTROL, s.wtext, set_value='Reductions path has been updated to '+reduxdir, /append
      WIDGET_CONTROL, s.wtext, set_value='Current working directory is '+cwd, /append
   END



   'reduxdir': BEGIN ;; user edits raw dir using edit box
                                ;get IDs from the main widget
      WIDGET_CONTROL, ev.top, get_uvalue=s
                                ; Determine the new path
      WIDGET_CONTROL, s.reduxdir, get_value=new_path
                                ; Check if the new path exists, complain if not
      if( FILE_TEST( new_path, /directory) EQ 0 ) then begin
         spawn, 'pwd', cwd
         fix_path, cwd
         arr = strsplit(cwd, path_sep())
         new_path = cwd+strtrim('Raw'+path_sep(),2)		
         if (x_chkfil(new_path, /SILENT) EQ 0) then begin ;; Default Raw directory doesn't exist.
            new_path = cwd
         endif
         fire_siren, func_name + ": ERROR! Entered redux directory path as " + new_path + $
                     ", but this directory does not exist!  Resetting to the default = " + new_path,$
                     WIDGET=s.wtext, /append
         WIDGET_CONTROL, s.reduxdir, set_value=new_path			
      endif else begin
         WIDGET_CONTROL, s.wtext, set_value='Raw path has been updated to '+new_path, /append
         spawn, 'pwd', cwd
         fix_path, cwd
         WIDGET_CONTROL, s.wtext, set_value='Current working directory is '+cwd, /append		
      endelse
   END

   'rawdir': BEGIN ;; user edits raw dir using edit box
                                ;get IDs from the main widget
      WIDGET_CONTROL, ev.top, get_uvalue=s
                                ; Determine the new path
      WIDGET_CONTROL, s.setupdir, get_value=new_path
                                ; Check if the new path exists, complain if not
      if( FILE_TEST( new_path, /directory) EQ 0 ) then begin
         spawn, 'pwd', cwd
         fix_path, cwd
         arr = strsplit(cwd, path_sep())
         new_path = cwd+strtrim('Raw'+path_sep(),2)		
         if (x_chkfil(new_path, /SILENT) EQ 0) then begin ;; Default Raw directory doesn't exist.
            new_path = cwd
         endif
         fire_siren, func_name + ": ERROR! Entered raw directory path as " + new_path + $
                     ", but this directory does not exist!  Resetting to the default = " + new_path,$
                     WIDGET=s.wtext, /append
         WIDGET_CONTROL, s.setupdir, set_value=new_path			
      endif else begin
         WIDGET_CONTROL, s.wtext, set_value='Raw path has been updated to '+new_path, /append
         spawn, 'pwd', cwd
         fix_path, cwd
         WIDGET_CONTROL, s.wtext, set_value='Current working directory is '+cwd, /append		
      endelse
   END
   
   'raw_pick': BEGIN ;; user edits raw dir using browse button
       ;get IDs from the main widget
      WIDGET_CONTROL, ev.top, get_uvalue=s
      WIDGET_CONTROL, s.setupdir, get_value=inipath
      ;pick raw data directory
      path=DIALOG_PICKFILE(/Directory, /must_exist, title='Please choose raw data directory', path=inipath)
      ;; Check if path is null string (will be true if user hits 'Cancel')
      if( is_empty( path ) EQ 1 ) then begin
      	WIDGET_CONTROL, s.wtext, set_value="Action aborted by user.  Raw path not changed, remains " + $
      		inipath, /append
      endif else begin
	      ;figure out which setup dir, send the path back to the top
	      WIDGET_CONTROL, s.setupdir, set_value=path
	      WIDGET_CONTROL, s.wtext, set_value='Raw path has been updated to '+path, /append
	      spawn, 'pwd', cwd
	      fix_path, cwd
	      WIDGET_CONTROL, s.wtext, set_value='Current working directory is '+cwd, /append
	   endelse
   END

   'redux_pick': BEGIN ;; user edits raw dir using browse button
       ;get IDs from the main widget
      WIDGET_CONTROL, ev.top, get_uvalue=s
      WIDGET_CONTROL, s.reduxdir, get_value=inipath
      ;pick reductions data directory
      path=DIALOG_PICKFILE(/Directory, /must_exist, title='Please choose reductions data directory', path=inipath)
      ;; Check if path is null string (will be true if user hits 'Cancel')
      if( is_empty( path ) EQ 1 ) then begin
      	WIDGET_CONTROL, s.wtext, set_value="Action aborted by user.  Reduction path not changed, remains " + $
      		inipath, /append
      endif else begin
	      ;figure out which setup dir, send the path back to the top
	      WIDGET_CONTROL, s.reduxdir, set_value=path
	      WIDGET_CONTROL, s.wtext, set_value='Reductions path has been updated to '+path, /append
	      spawn, 'pwd', cwd
	      fix_path, cwd
	      WIDGET_CONTROL, s.wtext, set_value='Current working directory is '+cwd, /append
	   endelse
   END

   'cat_pick': BEGIN
       ;get IDs from the main widget
      WIDGET_CONTROL, ev.top, get_uvalue=s
      WIDGET_CONTROL, s.setupdir, get_value=inipath
      ;pick working directory
      obscat=DIALOG_PICKFILE(/multiple_files, /must_exist, title='Please pick an observing catalog')
      if( is_empty( obscat ) EQ 1 ) then begin ;; user hit cancel button
      	WIDGET_CONTROL, s.obscat_list, get_uvalue=obscat
      	if( is_empty( obscat ) EQ 1 OR strmatch(obscat, 'tab', /FOLD_CASE) EQ 1 ) then begin
      		obscat = "(unknown)"
      	endif
      	WIDGET_CONTROL, s.wtext, set_value="Action aported by user.  Observing catalog remains " + $
      		obscat + ".", /append
      endif else begin
       	WIDGET_CONTROL, s.wtext, set_value="Observing catalog changed to " + $     	
      		obscat + ".", /append
	      WIDGET_CONTROL, s.obscat_list, set_value=obscat, set_uvalue=obscat
		endelse
   END

 'trace_pick': BEGIN
      ;pick working directory
      WIDGET_CONTROL, ev.top, get_uvalue=s
      WIDGET_CONTROL, s.setupdir, get_value=rawpath
      file=DIALOG_PICKFILE(filter='*.fits', /read, path=rawpath, title="Please select one or more trace flat files", /must_exist, /multiple_files)
      if( is_empty(file) EQ 0 ) then begin
	      ;figure out which setup dir, send the path back to the top
   	   WIDGET_CONTROL, s.tflt_file, set_value=file
   	   WIDGET_CONTROL, s.wtext, set_value='Trace flat' + s_or_not(n_elements(file)) + ' added: '+ fire_str_array_to_str(file), /append
   	endif else begin ;; user hit 'Cancel'
           WIDGET_CONTROL, s.tflt_file, get_value=file
           if( is_empty(file) EQ 1 ) then begin
              file = "(unknown)"
           endif
           nfiles = n_elements(file)
           WIDGET_CONTROL, s.wtext, set_value="Action aborted by user.  Trace flat" + s_or_not(nfiles) + $
                           " remain" + s_or_not(nfiles, /reverse) + " " + fire_str_array_to_str(file), /append   	
        endelse
   END
 
 'go_trace': BEGIN
    WIDGET_CONTROL, ev.top, get_uvalue=s
    WIDGET_CONTROL, s.setupdir, get_value=rawpath
    WIDGET_CONTROL, s.tflt_file, get_value=tflt_file
    
    bad_input = 0
    ;; Make sure that trace flats have been entered
    if is_empty(tflt_file) then begin
       fire_siren, func_name + ": ERROR! Forgot to input Trace Flat(s).  Please use the 'Browse' " + $
                   " button, input Trace Flats, and try again!", widget = s.wtext, /append
       break
    endif
    ;; Make sure that these trace flats exist
    if min( FILE_TEST(tflt_file), bad_files) EQ 0 then begin
       bad_input = 1
       
       fire_siren, func_name + ": ERROR! At least one of the Trace Flats (" + tflt_file[bad_files] + $
                   ") does not exist!  Please use the 'Browse' " + $
                   " button, input Trace Flats, and try again!", widget = s.wtext, /append
    endif
    
    if bad_input EQ 0 then begin
       WIDGET_CONTROL, s.wtext, set_value='Tracing orders. ATV will launch on completion;  Press q to continue after inspecting trace.' + fire_str_time(), /append
       WIDGET_CONTROL, s.reduxdir, get_value=reduxpath
       if (x_chkfil(tflt_file[0]) GT 0) then begin
          if (n_elements(tflt_file) EQ 1) then begin
             orderfile=strtrim(reduxpath,2)+'Flat/Orders_'+$
                       (strtrim(strsplit(file_basename(tflt_file[0]),'to', /extract),2))[1]
             ostr_file=strtrim(reduxpath,2)+'Flat/OStr_'+$
                       (strtrim(strsplit(file_basename(tflt_file[0]),'to', /extract),2))[1]
             
          endif else begin
             orderfile=strtrim(reduxpath,2)+'Flat/Orders_'+$
                       (strtrim(strsplit(file_basename(tflt_file[0]),'_.', /extract),2))[1]$
                       +'_'+$
                       (strtrim(strsplit(file_basename(tflt_file[n_elements(tflt_file)-1]),'_',$
                                         /extract),2))[1]
             ostr_file=strtrim(reduxpath,2)+'Flat/OStr_'+$
                       (strtrim(strsplit(file_basename(tflt_file[0]),'_.', /extract),2))[1]$
                       +'_'+$
                       (strtrim(strsplit(file_basename(tflt_file[n_elements(tflt_file)-1]),'_',$
                                         /extract),2))[1]
          endelse

			 fire_timer, time, /start
       	inter = WIDGET_INFO(s.traceinterbt,/button_set)
          tset_slits=fire_findslits(traceflat=tflt_file,  /CLOBBER, $
                                   INTER=inter, ORDERFILE=orderfile, ORDR_STR_FILE=ostr_file);/CHK
                                ;                                 orderfile=orderfile, $
                                ;                                 ordr_str_file=ostr_file,
			 fire_timer, time, /stop
       	 WIDGET_CONTROL, s.wtext, set_value='Orders have been traced.' + fire_str_time(elapsed=time), /append
			 
       endif else begin
          WIDGET_CONTROL, s.wtext, set_value='Error: trace flat file not found.', /append
       endelse

       
    endif
    
 END
 
 'pix_arc_pick': BEGIN
	; determine working directory
    WIDGET_CONTROL, ev.top, get_uvalue=s
    WIDGET_CONTROL, s.setupdir, get_value=rawpath
    file=DIALOG_PICKFILE(filter='*.fits', /read, path=rawpath, $
    	title="Please select a Slit Tilt File", /must_exist)
    if( is_empty( file ) EQ 0 ) then begin
		;figure out which setup dir, send the path back to the top
	    WIDGET_CONTROL, s.pixarcfile, set_value=file
	    WIDGET_CONTROL, s.wtext, set_value='Pixel arc for flats added as '+file, /append
	 endif else begin
	    WIDGET_CONTROL, s.pixarcfile, get_value=file
	    if( is_empty(file) EQ 1 ) then begin
	    	file = "(unknown)"
	    endif
	    WIDGET_CONTROL, s.wtext, set_value="Action aborted by user.  Pixel arc for flats remains "+file, /append	 	
	 endelse
 END

;;  'go_piximage': BEGIN
;;     WIDGET_CONTROL, ev.top, get_uvalue=s
;;     WIDGET_CONTROL, s.setupdir, get_value=rawpath
;;     WIDGET_CONTROL, s.pixarcfile, get_value=pixarcfile
;;     WIDGET_CONTROL, s.piximgfile, get_value=piximgfile
;;     tset_slits=mrdfits('Orders.fits', 1)
;;     IF pixinter THEN WIDGET_CONTROL, s.wtext, set_value='Launching ATV;  Press q to continue through each order', /append
;;     mage_makepix, pixarcfile, piximgfile, tset_slits, chk=pixinter
;;     WIDGET_CONTROL, s.wtext, set_value='Pixel Image written to '+ piximgfile, /append
;;  END

;;  'pix_arc_inter_tog':BEGIN
;;     WIDGET_CONTROL, ev.top, get_uvalue=s
;;     pixinter = ev.select
    
;;  END
 


 'flat_pick': BEGIN
                                ;pick working directory
    WIDGET_CONTROL, ev.top, get_uvalue=s
    WIDGET_CONTROL, s.setupdir, get_value=rawpath
    files=DIALOG_PICKFILE(filter='*.fits', /read, path=rawpath, $
			title="Please select one or more Flat Field Files", /must_exist, /multiple_files)
	if( is_empty(files) EQ 0 ) then begin
	    ;figure out which setup dir, send the path back to the top
	    WIDGET_CONTROL, s.flatfiles, set_value=files
	    WIDGET_CONTROL, s.wtext, set_value='Flat Field files added as '+ fire_str_array_to_str(files), /append	
	endif else begin
	    WIDGET_CONTROL, s.flatfiles, get_value=files
	    if( is_empty(files) EQ 1 ) then begin
	    	files = "(unknown)"
	    endif
	  	 WIDGET_CONTROL, s.wtext, set_value="Action aborted by user.  Flat files remain " + fire_str_array_to_str(files), /append	
	endelse

 END

 'illum_flat_pick': BEGIN
    ; determine working directory
    WIDGET_CONTROL, ev.top, get_uvalue=s
    WIDGET_CONTROL, s.setupdir, get_value=rawpath
    files=DIALOG_PICKFILE(filter='*.fits', /read, path=rawpath, /must_exist, $
    	title="Please select one or more IllumFlat Files", /multiple_files)
    if( is_empty(files) EQ 0 ) then begin
		;figure out which setup dir, send the path back to the top
   	 WIDGET_CONTROL, s.illumflatfiles, set_value=files
   	 WIDGET_CONTROL, s.wtext, set_value='Illum flat files added as '+ fire_str_array_to_str(files), /append
	endif else begin ;; user hit cancel
	    WIDGET_CONTROL, s.illumflatfiles, get_value=files
	    if( is_empty(files) EQ 1 ) then begin
	    	files = "(unknown)"
	    endif
	  	 WIDGET_CONTROL, s.wtext, set_value="Action aborted by user.  Illumflat files remain " + fire_str_array_to_str(files), /append			
	endelse
 END

 'omsk_pick': BEGIN
    WIDGET_CONTROL, ev.top, get_uvalue=s
    WIDGET_CONTROL, s.reduxdir, get_value=reduxpath
    pickpath = strtrim(reduxpath,2)+'Flat'
    files=DIALOG_PICKFILE(filter='Orders*.fits', /read, path=pickpath, $
    	title="Please select one or more Order Mask Files", /must_exist)
    WIDGET_CONTROL, ev.top, get_uvalue=s
    if( is_empty(files) EQ 0 ) then begin
		;figure out which setup dir, send the path back to the top
	    WIDGET_CONTROL, s.omsk, set_value=files
	    WIDGET_CONTROL, s.wtext, set_value='Order mask file added as '+files, /append
	 endif else begin ;; user hit cancel
	    WIDGET_CONTROL, s.omsk, get_value=files
	    if( is_empty(files) EQ 1 ) then begin
	    	files = "(unknown)"
	    endif
	  	 WIDGET_CONTROL, s.wtext, set_value="Action aborted by user.  Order mask files remain " + files, /append
	 endelse
 END


 'flat_inter_tog':BEGIN
    WIDGET_CONTROL, ev.top, get_uvalue=s
    flatinter = ev.select
    
 END

 'trace_inter_tog':BEGIN
    WIDGET_CONTROL, ev.top, get_uvalue=s
    traceinter = ev.select
 END

 'single_tog':BEGIN
    WIDGET_CONTROL, ev.top, get_uvalue=s
    singlesinter = ev.select
 END

 'fire_pipe_chkbt':BEGIN
    fire_pipe_inter = ev.select
 END
 
  'fire_pipe_clobber':BEGIN
    fire_pipe_clobber = ev.select
 END

	'fire_pipe_plot': BEGIN
		fire_pipe_plot = ev.select
	END

  'firehose_label': BEGIN
  	 WIDGET_CONTROL, ev.top, get_uvalue=s
	 WIDGET_CONTROL, s.wtext, set_value="Please enter the new title in the IDL terminal, then hit return.", /append
	 new_title = ''
	 print, "Please enter the new title, then hit return: "
	 READ, "", new_title
	 WIDGET_CONTROL, s.wtext, set_value="New title entered as: " + new_title, /append	 	 
  	 WIDGET_CONTROL, ev.top, tlb_set_title=new_title	 
 END

  'play': BEGIN
  	 WIDGET_CONTROL, ev.top, get_uvalue=s
    if( is_undefined(firestrct) EQ 1 ) then begin
		fire_siren, func_name + ": ERROR!  Cannot provide access to firestrct because it has not " + $
			"been created or loaded yet!  Please visit the 'Structure' tag, generate/load a structure, and" + $
			" then try again.", widget = s.wtext, /append
		break
	 endif
	 fire_timer, time, /start
	 WIDGET_CONTROL, s.wtext, set_value=[ "", "**** Play with firestrct **** (recommended for advanced users only).  Go to your IDL terminal.  A 'stop' has been placed in the function play_with_firestrct.pro, whose only input is firestrct.  Make any changes desired, and then hit .con to exit out of play-mode." + fire_str_time() ], /append 	
	 play_with_firestrct, firestrct
	 fire_timer, time, /stop
	 WIDGET_CONTROL, s.wtext, set_value=[ "  ...done playing with firestrct.  Changes NOT saved." + $
	 	"  Visit 'Structure' tab to save changes, if desired." + fire_str_time(ELAPSED=time), "" ], /append 
  
  END

  'fire_pipe_bright':BEGIN
    fire_pipe_bright = ev.select
 END
 
 'fire_pipe_verbose':BEGIN
    fire_pipe_verbose = ev.select
 END
 
 'fire_mkstrct_verbose': BEGIN
 	fire_mkstrct_verbose = ev.select
 END
 
 'fire_mkstrct_loud': BEGIN
 	fire_mkstrct_loud = ev.select
 END

  'xtell_clobber':BEGIN
    xtell_clobber = ev.select
 END

  'xtell_quick':BEGIN
    xtell_quick = ev.select
 END

 'multispec_out':BEGIN
    multispec_out = ev.select
 END

  'fire_no_cat': BEGIN
  	fire_no_cat = ev.select
  END

 'go_flat': BEGIN
    WIDGET_CONTROL, ev.top, get_uvalue=s
    
    WIDGET_CONTROL, s.setupdir, get_value=rawpath
    WIDGET_CONTROL, s.pixarcfile, get_value=pixarcfile
    WIDGET_CONTROL, s.flatfiles, get_value=flatfiles
    WIDGET_CONTROL, s.illumflatfiles, get_value=illumflatfiles
    WIDGET_CONTROL, s.omsk, get_value=ordermask

    bad_input = 0
    ;; Make sure that files were actually loaded in by the user...
    if is_empty(pixarcfile) EQ 1 then begin
       bad_input = 1
       bad_type = 'Slit Tilt File'
    endif else if is_empty(flatfiles) EQ 1 then begin
       bad_input = 1
       bad_type = 'Flat Field Files'	
    endif else if is_empty(illumflatfiles) EQ 1 then begin
       bad_input = 1
       bad_type = 'Illum Flat Files'		
    endif else if is_empty(ordermask) EQ 1 then begin
       bad_input = 1
       bad_type = 'Order Mask'			
    endif

	if( bad_input NE 1 ) then begin    
	    ;; Make sure that the user inputs actually exist.
	    if min( FILE_TEST(pixarcfile), bad_files) EQ 0 then begin
	       bad_input = 2
	       all_files = pixarcfile
	       bad_type = 'Slit Tilt File'
	    endif else if min( FILE_TEST(flatfiles), bad_files) EQ 0 then begin
	       bad_input = 2
	       all_files = flatfiles
	       bad_type = 'Flat Field Files'	
	    endif else if min( FILE_TEST(illumflatfiles), bad_files) EQ 0 then begin
	       bad_input = 2
	       all_files = illumflatfiles
	       bad_type = 'Illum Flat Files'		
	    endif else if min( FILE_TEST(ordermask), bad_files) EQ 0 then begin
	       bad_input = 2
	       all_files = ordermask
	       bad_type = 'Order Mask'			
	    endif
	 endif
    
    ;; If all is well, then continue on...
    if bad_input EQ 0 then begin
       
       WIDGET_CONTROL, s.wtext, set_value='Generating Flat Field...this takes 5-10 minutes' + fire_str_time(), /append 
       WIDGET_CONTROL, s.reduxdir, get_value=reduxpath
       pixfile  = strtrim(reduxpath,2)+'Flat/Piximg_'$
                  +strtrim((strsplit(file_basename(pixarcfile[0]),'_', /extract))[1],2) ; Pixel image
       
                                ; Pixel flat
       if (n_elements(flatfiles) EQ 1) then begin
          pixflat  = strtrim(reduxpath,2)+'Flat/Pixflat_'$    
                     +strtrim((strsplit(file_basename(flatfiles[0]),'_', /extract))[1],2)
       endif else begin
          pixflat  = strtrim(reduxpath,2)+'Flat/Pixflat_'$    
                     +strtrim((strsplit(file_basename(flatfiles[0]),'_.', /extract))[1],2)+'_'$
                     +strtrim((strsplit(file_basename(flatfiles[n_elements(flatfiles)-1]),'_', $
                                        /extract))[1],2)
       endelse
       
                                ; Illumination Flat
       if (n_elements(illumflatfiles) EQ 1) then begin
          outfile  = strtrim(reduxpath,2)+'Flat/Illumflat_'$    
                     +strtrim((strsplit(file_basename(illumflatfiles[0]),'_', /extract))[1],2)
       endif else begin
          outfile  = strtrim(reduxpath,2)+'Flat/Illumflat_'$    
                     +strtrim((strsplit(file_basename(illumflatfiles[0]),'_.', /extract))[1],2)+'_'$
                     +strtrim((strsplit(file_basename(illumflatfiles[n_elements(illumflatfiles)-1]),$
                                        '_', /extract))[1],2)
       endelse
       
       fire_timer, time, /start
       chk = WIDGET_INFO(s.flatinterbt,/button_set)
       fire_makeflat, flatfiles=flatfiles, illum=illumflatfiles, $
                      orders=ordermask, arcfile=pixarcfile[0], $
                      scifile=pixarcfile[0], chk=chk, /clobber, OUTFILE=outfile, PIXFILE=pixfile, PIXFLAT=pixflat
		 fire_timer, time, /stop

       WIDGET_CONTROL, s.wtext, set_value='...done generating Flat Field!' + fire_str_time(ELAPSED=time), /append        

    endif else if bad_input EQ 1 then begin
       ;; If we're in here, then the user has forgotten to input some file
       fire_siren, func_name + ": ERROR!  User has not input the " + $
                   bad_type + "!  Use the 'Browse' button and select the correct files.  Exiting " + $
                   "without running fire_makeflat!", widget = s.wtext, /append
    endif else if bad_input EQ 2 then begin
       ;; If we're in here, then at least one of the user inputs does not exist
       fire_siren, func_name + ": ERROR!  At least one of the input " + $
                   bad_type + " (" + all_files[bad_files] + ") does not exist!  Use the 'Browse' button " + $
                   "and select the correct files.  Exiting " + $
                   "without running fire_makeflat!", widget = s.wtext, /append	
    endif
    
 END
 
 'make_struct':BEGIN
     
     WIDGET_CONTROL, ev.top, get_uvalue=s
     WIDGET_CONTROL, s.setupdir, get_value=rawpath
     WIDGET_CONTROL, s.strctname, get_value=strctname
     WIDGET_CONTROL, s.reduxdir, get_value=reduxpath
     slct_pixflats = WIDGET_INFO(s.strctflats, /list_select)
     slct_ilflats  = WIDGET_INFO(s.strctiflats, /list_select)
     slct_omask    = WIDGET_INFO(s.strctomask, /list_select)

     if (FILE_TEST(strtrim(reduxpath,2)+"Flat", /dir)) then begin
        spawn, "ls "+strtrim(reduxpath,2)+"Flat/Pixflat*", flatls
        spawn, "ls "+strtrim(reduxpath,2)+"Flat/Illumflat*", iflatls
        spawn, "ls "+strtrim(reduxpath,2)+"Flat/Orders*", omaskls
        spawn, "ls "+strtrim(reduxpath,2)+"Flat/OStr*", ostrls
        
        if (slct_pixflats[0] EQ -1) then pixflats = flatls else $
           pixflats=flatls[slct_pixflats]

        if (slct_ilflats[0] EQ -1)  then illflats = iflatls else $
           illflats=iflatls[slct_ilflats]        

        if (slct_omask[0] EQ -1)    then begin
           omasks   = omaskls 
           ostrs = ostrls
        endif else begin
           omasks=omaskls[slct_omask]
           ostrs=ostrls[slct_omask]
        endelse
     endif

     ;make structure
     
     ;; Get the catalog name
     WIDGET_CONTROL, s.obscat_list, get_uvalue=obscat
     no_cat = WIDGET_INFO( s.fire_no_cat, /button_set )
     if strmatch( obscat, 'tab', /FOLD_CASE ) EQ 1 AND no_cat EQ 0 then begin
     	  fire_siren, func_name + ": WARNING!  No observation catalog entered!  (See the " + $
     	  	"'Setup' tab within the GUI.)  fire_mkstrct() can work without one, but inputting your " + $
     	  	" Magellan telescope operator source file dramatically reduces the amount of editing " + $
     	  	"required later.  Type '.con' in the IDL terminal from within which firehose was launched" + $
     	  	" to continue without it, or type 'retall', upload the catalog, and try again." + $
     	  	"  Select the 'Do not use Catalog' button in the GUI to avoid this message when running " + $
     	  	"fire_mkstrct() without a catalog.", widget = s.wtext, /append, /both
     endif else if no_cat EQ 1 then begin
     		obscat1 = 0
     endif else begin
     		obscat1 = obscat
     endelse

     WIDGET_CONTROL, s.wtext, set_value="Creating fire structure" + $
     		fire_str_time() + ".  This takes about half a minute...", /append

     fire_timer, time, /start
     verbose = WIDGET_INFO(s.fire_mkstrct_verbosebt,/button_set)
     loud = WIDGET_INFO(s.fire_mkstrct_loudbt,/button_set)
     
     string_replace, pixflats, path_sep()+path_sep(), path_sep()
     string_replace, illflats, path_sep()+path_sep(), path_sep()
     string_replace, omasks, path_sep()+path_sep(), path_sep()
     string_replace, ostrs, path_sep()+path_sep(), path_sep()
     fire_mkstrct, fire, rawpath=rawpath, pixflats=pixflats, $
                   illumflats=illflats, omask=omasks, ostrs=ostrs, $
                   objcats=obscat1, loud=loud, $
                   verbose=verbose, widget=s.wtext, /tellcats
     ttt = [1,13,14,16]
     for ii=0L, n_elements(fire)-1L do begin
       for jj=0L, n_elements(ttt)-1L do begin
         aa = fire.(ttt[jj])
         string_replace, aa, path_sep()+path_sep(), path_sep()
         fire.(ttt[jj]) = aa
       endfor
     endfor
		fire_timer, time, /stop

     ;; Must now use 'Save Structure' button for this
     ;;write it to a file
     ;;mwrfits, fire, strctname[0], /create 

     firestrct = fire

     WIDGET_CONTROL, s.wtext, set_value="FIRE structure created but NOT saved" + $
     		fire_str_time(ELAPSED=time) + ".  To save this file to " + $
     		strctname[0] +", hit the 'Save Structure' button.", /append

     ; Get a list of unique object names (by object ID)
     scis = firestrct[where(firestrct.obj_id GE 0)]
     scisort = scis[sort(scis.obj_id)]
     targets = scisort[uniq(scisort.obj_id)].object

     ; Populate the Extract tab with objects in the selection window
     WIDGET_CONTROL, s.w_spec_target, set_value=targets
     WIDGET_CONTROL, s.w_spec_target, set_uvalue=targets
     
     ;; Populate the 'Telluric' tab file tree
	  populate_telluric_tree, firestrct, ev
	  
  END


  'write_struct':BEGIN
    
     WIDGET_CONTROL, ev.top, get_uvalue=s
     WIDGET_CONTROL, s.strctname, get_value=strctname
    strctname[0] = 'Structs'+path_sep()+strctname[0];'redux'+path_sep()+;J.Gagne !@!@!@!
	 	if( is_undefined(firestrct) EQ 1 ) then begin
			fire_siren, func_name + ": ERROR!  Cannot save structure because firestrct has not " + $
				"been created or loaded yet!  Please visit the 'Structure' tag, generate/load a structure, and" + $
				" then try again.", widget = s.wtext, /append
			break
	 	endif
     fire = firestrct
     ;; Before saving, we need to eliminate empty strings ('') to avoid
     ;; well known IDL bug
     firestrct_fix_empty_strings, fire
     mwrfits, fire, strctname[0], /create

     scis = firestrct[where(firestrct.obj_id GE 0)]
     scisort = scis[sort(scis.obj_id)]
     targets = scisort[uniq(scisort.obj_id)].object

     WIDGET_CONTROL, s.w_spec_target, set_value=targets
     WIDGET_CONTROL, s.w_spec_target, set_uvalue=targets
     WIDGET_CONTROL, s.wtext, set_value='FIRE structure written to '+strctname[0] + fire_str_time(), /append

  END

  'load_struct':BEGIN
     WIDGET_CONTROL, ev.top, get_uvalue=s
     WIDGET_CONTROL, s.strctname, get_value=strctname
     strctname[0] = 'Structs'+path_sep()+strctname[0]
     ;strctname[0] = 'redux'+path_sep()+strctname[0];J.Gagne !@!@!@!
     if FILE_TEST( strctname[0] ) EQ 1 then begin
	     fire = xmrdfits(strctname[0], 1)
	     firestrct = {firestrct}
	     firestrct = replicate(firestrct, n_elements(fire))
	     copy_struct, fire, firestrct, ntags
	     if ntags NE n_tags(fire) then begin
				WIDGET_CONTROL, s.wtext, set_value='Old version of firestrct structure definition detected.  Not a problem ' + $
				'(useless tags removed), but you might want to resave right now so that the latest version is stored.', /append
	     endif
	     scis = firestrct[where(firestrct.obj_id GE 0)]
	     scisort = scis[sort(scis.obj_id)]
	     targets = scisort[uniq(scisort.obj_id)].object
		  IF size(targets, /type) NE 7 THEN targets=fire_string(lindgen(n_elements(firestrct)))
	     WIDGET_CONTROL, s.w_spec_target, set_value=targets
	     WIDGET_CONTROL, s.w_spec_target, set_uvalue=targets
	     WIDGET_CONTROL, s.wtext, set_value='FIRE structure loaded from '+strctname[0]+fire_str_time(), /append
	     ;; Populate the 'Telluric' tab file tree
	     populate_telluric_tree, firestrct, ev
	  endif else begin
	  		fire_siren, func_name + ": ERROR!  File " + strctname[0] + " does not exist!  (Either " + $
	  			"it was not previously created and saved, or you are in the wrong directory.)  " + $
	  			"Exiting without loading the structure!", widget = s.wtext, /append
	  endelse
  END

 'edit_struct':BEGIN
    WIDGET_CONTROL, ev.top, get_uvalue=s 
    WIDGET_CONTROL, s.strctname, get_value=strctname
    WIDGET_CONTROL, s.reduxdir, get_value=reduxpath
    WIDGET_CONTROL, s.setupdir, get_value=rawpath

    fire = xmrdfits('Structs'+path_sep()+strctname[0], 1);J.Gagne !@!@!@!
    if( is_undefined(firestrct) EQ 1 ) then begin
		fire_siren, func_name + ": ERROR!  Cannot edit firestrct because firestrct has not " + $
			"been created or loaded yet!  Please visit the 'Structure' tag, generate/load a structure, and" + $
			" then try again.", widget = s.wtext, /append
		break
	 endif
	 
    fitsfile = 'Structs'+path_sep()+strctname[0];J.Gagne !@!@!@!
    ;fitsfile = strctname[0]
    if size(firestrct,/type) ne 8L then message, 'NO STRUCTURE WAS LOADED !'
    fire_editstrct, firestrct, fitsname=fitsfile, raw=rawpath
    fire = xmrdfits('Structs'+path_sep()+strctname[0], 1);J.Gagne !@!@!@!
    firestrct = fire

    scis = firestrct[where(firestrct.obj_id GE 0)]
    scisort = scis[sort(scis.obj_id)]
    targets = scisort[uniq(scisort.obj_id)].object

    IF n_elements(targets) EQ 0 THEN targets=fire_string( lindgen(n_elements(firestrct)) )
    WIDGET_CONTROL, s.w_spec_target, set_value=targets
    WIDGET_CONTROL, s.w_spec_target, set_uvalue=targets
    WIDGET_CONTROL, s.wtext, set_value='FIRE structure loaded from '+strctname[0]+fire_str_time(), /append
    WIDGET_CONTROL, s.wtext, set_value='FIRE structure has been edited and written to '+strctname[0], /append

  END

 'edit_script':BEGIN
    WIDGET_CONTROL, ev.top, get_uvalue=s 
    if( is_undefined(firestrct) EQ 1 ) then begin
       fire_siren, func_name + ": ERROR!  Cannot run edit script because firestrct has not " + $
                   "been created or loaded yet!  Please visit the 'Structure' tag, generate/load a structure, and" + $
                   " then try again.", widget = s.wtext, /append
       break
    endif
    
    WIDGET_CONTROL, s.wtext, set_value='Reading in template and editing data' $
                    + fire_str_time() + '...', /append
    fire_timer, time, /start
    
    ;; Run the edit script
     verbose = WIDGET_INFO(s.fire_mkstrct_verbosebt,/button_set)
     loud = WIDGET_INFO(s.fire_mkstrct_loudbt,/button_set)
    run_firestrct_script, firestrct, WIDGET=s.wtext, VERBOSE=verbose, LOUD=loud, RAWPATH=s.cwd
    
    ;; Save the result
    fire = firestrct
    WIDGET_CONTROL, s.strctname, get_value=strctname
    strctname[0] = strtrim(reduxpath,2)+path_sep()+'Structs'+path_sep()+strctname[0]
    mwrfits, fire, strctname[0], /create
    
    scis = firestrct[where(firestrct.obj_id GE 0)]
    scisort = scis[sort(scis.obj_id)]
    targets = scisort[uniq(scisort.obj_id)].object
    
    fire_timer, time, /stop
    WIDGET_CONTROL, s.w_spec_target, set_value=targets
    WIDGET_CONTROL, s.w_spec_target, set_uvalue=targets
    WIDGET_CONTROL, s.wtext, set_value='FIRE structure edited and result written to '+strctname[0] $
                    + fire_str_time(ELAPSED=time), /append
    
 END

  'show_objids': BEGIN
  
    WIDGET_CONTROL, ev.top, get_uvalue=s 
    WIDGET_CONTROL, s.strctname, get_value=strctname
    WIDGET_CONTROL, s.reduxdir, get_value=reduxpath
    WIDGET_CONTROL, s.setupdir, get_value=rawpath

    if( is_undefined(firestrct) EQ 1 ) then begin
		fire_siren, func_name + ": ERROR!  Cannot determine obj_ids because firestrct has not " + $
			"been created or loaded yet!  Please visit the 'Structure' tag, generate/load a structure, and" + $
			" then try again.", widget = s.wtext, /append
		break
	 endif  

	inds = uniq( firestrct.obj_id, sort(firestrct.obj_id) )
	good_spots = where( firestrct(inds).obj_id GT -1, ngood )
	if ngood EQ 0 then begin
    WIDGET_CONTROL, s.wtext, set_value='No valid obj ids exist', /append
    break	
	endif else begin
	 names = firestrct(inds(good_spots)).object
	 ids = firestrct(inds(good_spots)).obj_id
	 message = "  obj id: " + string(ids, FORMAT='(I3)') + "    object: " + fire_string(names)
    WIDGET_CONTROL, s.wtext, set_value='Object IDs' + fire_str_time(), /append	 
    WIDGET_CONTROL, s.wtext, set_value=message, /append
	endelse


  END

  'rebuild_telluric_tree':  BEGIN
     WIDGET_CONTROL, ev.top, get_uvalue=s 
     if is_undefined(firestrct) EQ 0 then begin
        populate_telluric_tree, firestrct, ev
     endif else begin
        fire_siren, func_name + ": ERROR!  Cannot populate telluric tree because firestrct has not " + $
                    "been created or loaded yet!  Please visit the 'Structure' tag, generate/load a structure, and" + $
                    " then try again.", widget = s.wtext, /append
     endelse
  END

 'strctname': BEGIN
	  WIDGET_CONTROL, ev.top, get_uvalue=s
     WIDGET_CONTROL, s.strctname, get_value=strctname
     if( is_fits( strctname ) EQ 0 ) then begin
     		strctname = strctname + ".fits"
     		WIDGET_CONTROL, s.strctname, set_value=strctname     		
     endif
     ;; Check if a saved data file with this name already exists.    
     exists = FILE_TEST( s.cwd + '' + strctname[0] )
     if( exists EQ 0 ) then begin     
	     WIDGET_CONTROL, s.wtext, set_value="Structure filename changed to "+strctname[0] $
	   		+ fire_str_time() + ".  No file currently saved under this name exists in the " +$
	   		"current working directory. Hit 'Save Structure' button to save a loaded structure" + $
	   		" to this new file name.", /append
	   endif else begin
	     WIDGET_CONTROL, s.wtext, set_value="Structure filename changed to "+strctname[0] $
	   		+ fire_str_time() + ".  A file currently saved under this name exists in the " +$
	   		"current working directory. Hit 'Load Structure' button to read in this loaded structure.", $
	   		/append	   
	   endelse       
 END

 'std_pick': BEGIN
                                ;pick working directory
    WIDGET_CONTROL, ev.top, get_uvalue=s
    WIDGET_CONTROL, s.setupdir, get_value=rawpath
    file=DIALOG_PICKFILE(filter='*.fits', /read, path=rawpath, /must_exist)
                                ;figure out which setup dir, send the path back to the top
    WIDGET_CONTROL, s.stdfile, set_value=file
    WIDGET_CONTROL, s.wtext, set_value='Standard star file added as '+file, /append
 END

;;  'std_arc_pick': BEGIN
;;                                 ;pick working directory
;;     WIDGET_CONTROL, ev.top, get_uvalue=s
;;     WIDGET_CONTROL, s.setupdir, get_value=rawpath
;;     file=DIALOG_PICKFILE(filter='*.fits', /read, path=rawpath, /must_exist)
;;                                 ;figure out which setup dir, send the path back to the top
;;     WIDGET_CONTROL, s.stdarcfile, set_value=file
;;     WIDGET_CONTROL, s.wtext, set_value='Standard star arc file added as '+file, /append
;;  END
 
 'std_flux_pick': BEGIN
                                ;pick working directory
    WIDGET_CONTROL, ev.top, get_uvalue=s
    WIDGET_CONTROL, s.setupdir, get_value=rawpath
    file=DIALOG_PICKFILE(filter='*.dat', /read, path='~'+path_sep()+'idl'+path_sep()+'xidl'+path_sep()+'Spec'+path_sep()+'Flux'+path_sep(), /must_exist)
                                ;figure out which setup dir, send the path back to the top
    WIDGET_CONTROL, s.stdflxfile, set_value=file
    WIDGET_CONTROL, s.wtext, set_value='Standard star flux table added as '+file, /append
 END

 'go_sensfunc': BEGIN
    WIDGET_CONTROL, ev.top, get_uvalue=s
    if is_undefined(firestrct) EQ 1 then begin
       fire_siren, func_name + ": ERROR!  Cannot correct tellurics because firestrct has not " + $
                   "been created or loaded yet!  Please visit the 'Structure' tag, generate/load a structure, and" + $
                   " then try again.", widget = s.wtext, /append
       break
    endif
    fire = firestrct
    slct = WIDGET_INFO(s.w_telcor_targets, /tree_select)
    if( slct EQ -1 ) then begin
       fire_siren, func_name + ": ERROR!  Cannot correct tellurics because target has not " + $
                   "been chosen!  Please choose a target for which to calibrate tellurics, and" + $
                   " then try again.", widget = s.wtext, /append
       break  
    endif
    WIDGET_CONTROL, slct, get_value=sci_object
    if( is_empty(sci_object) EQ 1 OR strmatch(sci_object, "*.fits") EQ 0 ) then begin
       fire_siren, func_name + ": ERROR!  Cannot correct tellurics because target has not " + $
                   "been chosen!  Please choose a target for which to calibrate tellurics, and" + $
                   " then try again.", widget = s.wtext, /append
       break
    endif
    spot = where(strmatch(firestrct.objstrfile, "*" + sci_object, /FOLD_CASE), nmatches)
    if nmatches EQ 1 then begin
    	object_name = strtrim(firestrct(spot).object,2)
    	WIDGET_CONTROL, s.wtext, set_value='Calibrating telluric associated with object structure ' $
                    + sci_object + ', ' + object_name + fire_str_time(), /append    	
    endif else begin
    	WIDGET_CONTROL, s.wtext, set_value='Calibrating telluric associated with object structure ' $
                    + sci_object + fire_str_time(), /append    
    endelse
    fire_timer, time, /start
    quick = WIDGET_INFO(s.xtell_quickbt,/button_set)
    clobber = WIDGET_INFO(s.xtell_clobberbt,/button_set)
    WIDGET_CONTROL, s.setupdir, get_value=rawpath
    fire_telluric, fire, objstrfile=sci_object, clobber=clobber, quick=quick, name=object_name, rawpath=rawpath
    fire_timer, time, /stop
    ;;WIDGET_CONTROL, s.wtext, set_value='Done calibrating telluric for science object ' $
    ;;	+ sci_object + fire_str_time(Elapsed=time), /append	   
 END 
 
 'telluric_branch': BEGIN

;    fire=firestrct
;    WIDGET_CONTROL, ev.top, get_uvalue=s
;    slct = WIDGET_INFO(s.w_telcor_targets, /tree_select)
;    WIDGET_CONTROL, slct, get_value=sci_object
;    print, sci_object
;    stop

 END 


 'telluric_leaf': BEGIN

    if is_undefined(firestrct) EQ 1 then begin
       fire_siren, func_name + ": ERROR!  Cannot select target because firestrct has not " + $
                   "been created or loaded yet!  Please visit the 'Structure' tag, generate/load a structure, and" + $
                   " then try again.", widget = s.wtext, /append
       break
    endif	
    
    fire = firestrct
    WIDGET_CONTROL, ev.top, get_uvalue=s
    slct = WIDGET_INFO(s.w_telcor_targets, /tree_select)
    if( slct NE -1 ) then begin
       WIDGET_CONTROL, slct, get_value=sci_object
       ;print, sci_object
    endif else begin
       sci_object = ''
    endelse
    if (ev.clicks EQ 2) AND is_empty(sci_object) EQ 0 then begin
       WIDGET_CONTROL, s.wtext, set_value='Calibrating telluric associated with object structure ' $
                       + sci_object + fire_str_time(), /append
       fire_telluric, fire, objstrfile=sci_object
    endif
    
 END
 
 'go_combine': BEGIN
    WIDGET_CONTROL, ev.top, get_uvalue=s
    if( is_undefined(firestrct) EQ 1 ) then begin
       fire_siren, func_name + ": ERROR!  Cannot combine targets because firestrct has not " + $
                   "been created or loaded yet!  Please visit the 'Structure' tag, generate/load a structure, and" + $
                   " then try again.", widget = s.wtext, /append
       break			
    endif
    fire=firestrct
    slct = WIDGET_INFO(s.w_combine_targets, /list_select)
    if( n_elements(slct) EQ 1 ) then begin
    	if slct EQ -1 then begin
	       fire_siren, func_name + ": ERROR!  Cannot combine targets because target has not " + $
	                   "been chosen yet!  Please choose a target to combine, and" + $
	                   " then try again.", widget = s.wtext, /append
	       break
	   endif	
    endif
    WIDGET_CONTROL, s.w_combine_targets, get_uvalue=combfiles
    targs = combfiles[slct]
    multispec_out = WIDGET_INFO(s.multispec_outbt,/button_set)
    WIDGET_CONTROL, s.wtext, set_value='Combining telluric corrected spectra...' + fire_str_time(), /append  
    fire_timer, time, /start  
    fire_guicombine, fire, targs=targs, WIDGET=s.wtext, /append, ORDERS=multispec_out
    fire_timer, time, /stop
    WIDGET_CONTROL, s.wtext, set_value='Done combining telluric corrected spectra' + fire_str_time(ELAPSED=time), /append  
 END 

 'go_pipe': BEGIN

    WIDGET_CONTROL, ev.top, get_uvalue=s
 	if( is_undefined(firestrct) EQ 1 ) then begin
		fire_siren, func_name + ": ERROR!  Cannot run pipeline because firestrct has not " + $
			"been created or loaded yet!  Please visit the 'Structure' tag, generate/load a structure, and" + $
			" then try again.", widget = s.wtext, /append
		break
 	endif
	fire = firestrct 
 
    ;; WIDGET_CONTROL, s.stdoutfile, get_value=stdoutfile
    ;; WIDGET_CONTROL, s.stdfile, get_value=stdfile
    WIDGET_CONTROL, s.w_spec_target, get_uvalue=targetfiles
    slct = WIDGET_INFO(s.w_spec_target, /list_select)
    if (slct[0] NE -1) then begin
       runtargs = targetfiles[slct]
    endif else begin
       ans = dialog_message("Reducing ALL objects.  Proceed?", /Question, /center)
       if (ans EQ "Yes") then begin
          runtargs = targetfiles
       endif else begin
          print, "Cancelled operation."
          return
       endelse
    endelse
    print, 'Reducing: ', runtargs

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; RUN THE PIPELINE!!!!!!!

    WIDGET_CONTROL, ev.top, get_uvalue=s 
    WIDGET_CONTROL, s.reduxdir, get_value=reduxpath
    WIDGET_CONTROL, s.wtext, set_value='Running pipeline' + fire_str_time() + '.  Go get lunch, this will take a while...', /append
     WIDGET_CONTROL, s.strctname, get_value=strctname    
    strctname[0] = strtrim(reduxpath,2)+path_sep()+strctname[0]
    fire_timer, time, /start

    fire_pipe, fire, reduxpath=reduxpath, targets=runtargs, $
               Verbose=verbose, BRIGHT=extmode, CLOBBER=fire_pipe_clobber, $
               WIDGET=s.wtext, chkwpix=wpixmode, chkarc=arcmode, $
               chkobj=findmode, chkext=extverb, SEP_SKY_EXP=skymode, $
               RESET_SKY_MATCHES=skyreset, FITSNAME=strctname[0], /append

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    


    ; Now update the tellurics window to reflect new files
    populate_telluric_tree, firestrct, ev

	; save changes made to the structure
    firestrct = fire

    fire_timer, time, /stop
    WIDGET_CONTROL, s.wtext, set_value='DONE running pipeline' + fire_str_time(ELAPSED=time) + '!  How was lunch?', /append
    WIDGET_CONTROL, s.wtext, set_value='Consider saving firestrct to store information on created files (optional).', /append
    
 END

 'ql_file_pick':BEGIN
    WIDGET_CONTROL, ev.top, get_uvalue=s
    WIDGET_CONTROL, s.setupdir, get_value=rawpath
    file=DIALOG_PICKFILE(filter='*.fits', /read, path=rawpath, title="Please pick a Quicklook Target", /must_exist)
    if( is_empty(file) EQ 0 ) then begin
	    ;figure out which setup dir, send the path back to the top
   	 WIDGET_CONTROL, s.qlfile, set_value=file
   	 WIDGET_CONTROL, s.wtext, set_value='Quicklook target is '+file, /append
	endif else begin ;; user hit Cancel
		WIDGET_CONTROL, s.qlfile, get_value=file
		if( is_empty(file) EQ 1 ) then begin
			file="(unentered)"
		endif
   	WIDGET_CONTROL, s.wtext, set_value="Action aborted by user.  Quick look target remains " + file, /append
	endelse
 END

 'go_ql':BEGIN
     WIDGET_CONTROL, ev.top, get_uvalue=s
     WIDGET_CONTROL, s.qlfile, get_value=qlfile
     fire_siren, func_name + ": Quick look not yet implemented!", widget = s.wtext, /append
     ;;mage_quicklook, qlfile
     ;;WIDGET_CONTROL, s.wtext, set_value='Running Quicklook on '+qlfile, /append
     END

 'select_flats': begin
    WIDGET_CONTROL, ev.top, get_uvalue=s
    WIDGET_CONTROL, s.strctflats, get_value=select_flats
    slct = WIDGET_INFO(s.strctflats, /list_select)
    if (slct[0] NE -1) then begin
       runtargs = targetfiles[slct]
    endif else begin
       ans = dialog_message("Reducing ALL objects.  Proceed?", /Question, /center)
       if (ans EQ "Yes") then begin
          runtargs = targetfiles
       endif else begin
          print, "Cancelled operation."
          return
       endelse
    endelse
    END

	;; FIREHOSE help messages
	'setup help': BEGIN
		WIDGET_CONTROL, ev.top, get_uvalue=s
		fire_help, /setup, WIDGET = s.wtext, BOTH=both, /append
	END
	'trace help': BEGIN
		WIDGET_CONTROL, ev.top, get_uvalue=s
		fire_help, /trace, WIDGET = s.wtext, BOTH=both, /append
	END
	'flats help': BEGIN
		WIDGET_CONTROL, ev.top, get_uvalue=s
		fire_help, /flats, WIDGET = s.wtext, BOTH=both, /append
	END
	'structure help': BEGIN
		WIDGET_CONTROL, ev.top, get_uvalue=s
		fire_help, /structure, WIDGET = s.wtext, BOTH=both, /append
	END
	'extract help': BEGIN
		WIDGET_CONTROL, ev.top, get_uvalue=s
		fire_help, /extract, WIDGET = s.wtext, BOTH=both, /append
	END
	'telluric help': BEGIN
		WIDGET_CONTROL, ev.top, get_uvalue=s
		fire_help, /telluric, WIDGET = s.wtext, BOTH=both, /append
	END
	'combine help': BEGIN
		WIDGET_CONTROL, ev.top, get_uvalue=s
		fire_help, /combine, WIDGET = s.wtext, BOTH=both, /append
	END
	'quicklook help': BEGIN
		WIDGET_CONTROL, ev.top, get_uvalue=s
		fire_help, /quicklook, WIDGET = s.wtext, BOTH=both, /append
	END
	'hot keys': BEGIN
		WIDGET_CONTROL, ev.top, get_uvalue=s
		fire_help, /hotkeys, WIDGET = s.wtext, BOTH=both, /append	
	END

        'fire_pipe_preferences': BEGIN
           WIDGET_CONTROL, ev.top, get_uvalue=s
           fire_pipe_setprefs, s
        END


 ELSE: BEGIN
 	 WIDGET_CONTROL, ev.top, get_uvalue=s
; 	 msg = func_name + ": ERROR!  This uvalue is not recognized ( = bug in code)!  uvalue = " + $
;    	uvalue + ".  Placing a stop to facilitate debugging."
;    fire_siren, msg, widget = s.wtext, /append, BOTH=both
;    stop
 END


ENDCASE

WIDGET_CONTROL, ev.top, get_uvalue=s 
WIDGET_CONTROL, s.reduxdir, get_value=reduxpath
if (FILE_TEST("Flat", /dir)) then begin
	rpath = strtrim(reduxpath,2)
	pixfiles = rpath + "Flat/Pixflat*"
	if FILE_TEST( pixfiles ) then begin
   	spawn, "ls " + pixfiles, flatls
   endif else begin
   	flatls=''
   endelse
   illumfiles = rpath + "Flat/Illumflat*"
	if FILE_TEST( illumfiles ) then begin
   	spawn, "ls " + illumfiles, illumls
   endif else begin
   	illumls=''
   endelse  
   ofiles = rpath + "Flat/Orders*"
 	if FILE_TEST( ofiles ) then begin
   	spawn, "ls " + ofiles, ordrls
   endif else begin
   	ordrls=''
   endelse   
endif else begin
   flatls=''
   illumls=''
   ordrls=''
endelse

if is_undefined(uvalue) EQ 0 AND size(uvalue, /type) NE 8 then begin
	if n_elements(uvalue) GT 1 then begin
		go = 1
	endif else if (uvalue NE 'list' AND uvalue NE 'quit') then begin
		go = 1
	endif else begin
		go = 0
	endelse

	if (go EQ 1) then begin
	   WIDGET_CONTROL, ev.top, get_uvalue=s
	   widget_control, s.strctflats, set_value=flatls
	   widget_control, s.strctiflats, set_value=illumls
	   widget_control, s.strctomask, set_value=ordrls
	endif
endif

END


;;-----------------------------------------------------------------
;;-----------------------------------------------------------------
;;-----------------------------------------------------------------

;; Populates the trees for the 'Telluric' tab in the GUI.
PRO populate_telluric_tree, firestrct, ev
  
  WIDGET_CONTROL, ev.top, get_uvalue=s
  
  ;; Destroy the old tree
  if is_empty( s.tell_branches ) EQ 0 then begin
     t = fix(list_to_names(s.tell_branches, /double))
     nt = n_elements(t)
     for i=0, nt-1 do begin
        WIDGET_CONTROL, t[i], /destroy
     endfor
  endif
  
  scis = firestrct[where(firestrct.obj_id GE 0)]
  scisort = scis[sort(scis.obj_id)]
  targets = scisort[uniq(scisort.obj_id)].object
  
;     Figure out which objects (if any) have already been extracted by
;     looking for an objstr file
  ext_index = where(firestrct.obj_id GE 0 AND $
                    FILE_TEST(strtrim(firestrct.objstrfile,2)) EQ 1, nextracted)
  
;    If some have been extracted, add them to the list of potential
;    telluric correction targets, and frame combine targets.
  if (nextracted GT 0) then begin 
     extracted = firestrct[ext_index]
     extsort = extracted[sort(extracted.obj_id)]
     ext_obj = extsort[uniq(extsort.obj_id)].object
     next = n_elements(ext_obj)
     tell_branches=intarr(next)
;    Build the tree in the Telluric tab        
     for iext=0, next-1 do begin
                                ; Make the root folder for each Science object
        tell_branches[iext] = WIDGET_TREE(s.w_telcor_targets, Value=ext_obj[iext], $
                                          uvalue='telluric_branch', /FOLDER)       
                                ; Now identify each science exposure and place as leaves on tree
        match = where(extracted.object EQ ext_obj[iext], nmatch)
        for imatch=0, nmatch-1 do begin
           objstr   = extracted[match[imatch]].objstrfile
           leafname = (strsplit(objstr,path_sep(),/extract))[1]
           tell_leaf = WIDGET_TREE(tell_branches[iext],value=leafname,uvalue='telluric_leaf')
        endfor
     endfor
     
     ;; Store these values in s (also needs to be saved afterwards
     if next GT 0 then begin
        s.tell_branches = names_to_list( tell_branches, /double )
     endif
     
;        Populate the selection window in the Combine tab
     WIDGET_CONTROL, s.w_combine_targets, set_value=ext_obj
     WIDGET_CONTROL, s.w_combine_targets, set_uvalue=ext_obj
     
     WIDGET_CONTROL, ev.top, set_uvalue=s
     
  endif
  
END

;;-----------------------------------------------------------------
;;-----------------------------------------------------------------

pro fire_pipe_setprefs_event, ev

  common share

  WIDGET_CONTROL, ev.top, get_uvalue = state, /no_copy
  WIDGET_CONTROL, ev.id, get_uvalue = uval
  
  case uval of
      'quit_prefs' : begin
         widget_control, ev.top, /destroy
         return
      end

      'wpix_dropbox': begin
         wpixmode = ev.index
         return
      end

      'arc_dropbox': begin
         arcmode = ev.index
         return
      end

      'find_dropbox': begin
         findmode = ev.index
         return
      end

      'ext_dropbox': begin
         extmode = ev.index
         return
      end

      'set_clob': begin
         fire_pipe_clobber = ev.select 
         return
      end

		'sky_dropbox': begin
			skymode = ev.index
			return
		end

		'sky_dropbox2': begin
			skyreset = ev.index
			return
		end

      'wpix_help': begin
         helpstr = $
            [" SLIT TILT MAPPING", " ", $
             "The first step for extraction is to map the",$
             "tilt of the slit in detector pixel coordinates.", $
             "FIREHOSE uses OH lines to do this when possible,",$
             "and ThAr arc spectra when the OH lines are", $
             "insufficient.", " ",$
             "Normally this happens in the background but if",$
             "desired the results can be plotted up for",$
             "inspection.  This is a slow process as it plots", $
             "the 2D rectified echelle orders one by one for ",$
             "each exposure.  However it is a helpful disgnostic",$
             "if you suspect the sky subtraction is dodgy for a ",$
             "particular exposure." $
            ]

        x_helpwidg, helpstr
        return
      end

      'arc_help': begin
         helpstr = $
            ["ARC LINE FITTING"," ", $
             "No plots: verbal information sent to the",$
             "   terminal but no diagnistic plots to the", $
             "   screen.  QA plots are still written to",$
             "   disk in the QA subdirectory of redux.",$
             " ", $
             "Inspect Results: This mode is the most useful",$
             "   default.  It flashes plots of the ID ",$
             "   process up but does not halt the run.",$
             " ", $
             "Interactive: This mode launches a GUI for",$
             "   users to reidentify arc/OH lines and fit",$
             "   the wavelength solution explicitly.  Very", $
             "   time consuming.  See the Wiki/website for",$
             "   documentation on running the GUI", $
             " ", $
             "SYNOPSIS",$
             " ", $
             "In normal operation, FIREHOSE derives a",$
             "wavelength solution for each frame ",$
             "automatically by cross-correlating against", $
             "an archived template for first-guess, and ",$
             "then explicitly fitting polynomicals to arc-ID",$
             "versus wavelength pairs."," ", $
             "FIREHOSE should almost always run in automated",$
             "wavelength mode, with the user choosing only", $
             "whether to plot diagnostic information to the",$
             "screen at runtime." $
             ]
         x_helpwidg, helpstr
         return
      end

      'sky_help': begin
         helpstr = $
            ["SKY MODEL CONSTRUCTION"," ", $
             "Derive from sci frame: If this option is set, ",$
             "   then the sky model is constructed using the ", $
             "   science frame itself.  This is the default, ", $
             "   and is best in most scenarios.", $
             " ", $
             "Use separate file: If this option is set, then",$
             "   a separate exposure (type: SKY) taken on a ",$
             "   nearby, relatively empty portion of the sky ",$
             "   is used for the sky model instead.",$
             " " $
             ]
         x_helpwidg, helpstr
         return
      end
      
      'sky_help2': begin
         helpstr = $
            ["RESETTING MATCHED SKY EXPOSURES"," ", $
             "This option may be safely ignored if separate ", $
             "   files are not used for construction of the ", $
             "   sky model.  Once manually matched by the user,", $
             "   SKY frames are stored in the firestrct array ", $
             "   to spare the user from constant re-matching.  ",$
             "   With this option, the user allows him or ", $
             "   herself to rematch these frames.", $
             " " $
             ]
         x_helpwidg, helpstr
         return
      end      

      'aps_help': begin
         helpstr = $
            ["OBJECT FINDING"," ", $
             "Automated: FIREHOSE finds objects on the slit",$
             "   and determines appropriate extraction boundaries.",$
             "   Pipeline flow does not stop for approval.", $
             "Inspect results: Same as AUTOMATED but a sky subtracted",$
             "   image is shown with the location of the aperture ",$
             "   indicated.  Program flow is stopped for approval by", $
             "   the user",$
             "Interactive: A GUI is launched for the user to define", $
             "   the extraction aperture explicitly.  See the Wiki/",$
             "   webpage for detailed documentation."$
            ]

         x_helpwidg, helpstr
         return
      end
      
      else:
  endcase

  WIDGET_CONTROL, ev.top, set_uvalue = state, /no_copy
  return

end

pro fire_pipe_setprefs, state

  common share

  prefsbase = WIDGET_BASE( title = 'Extraction Preferences', /column, $
                           xoffset=xoffset,yoffset=yoffset)

  wpixbase = WIDGET_BASE(prefsbase, title = 'Wavepix Calibration Preferences', /column, /frame)
  msg = WIDGET_LABEL(wpixbase, Value='Slit Tilt Mapping')
  st1 = widget_base(wpixbase, /row)
  text = widget_label(st1, Value="User Interaction:   ")
  wpix_options = ["Silent", "Inspect Results (slow)", "Debug"]
  wpixchoose = WIDGET_COMBOBOX(st1, uname='slit_tab',Value=wpix_options, uval='wpix_dropbox')
  WIDGET_CONTROL, wpixchoose, set_combobox_select=wpixmode
  wpix_help = widget_button(st1,value="?", uval='wpix_help')

  arcsbase = WIDGET_BASE(prefsbase, title = 'Wavelength Calibration Preferences', /column, /frame)
  msg = WIDGET_LABEL(arcsbase, Value='Wavelength Calibration')
  ab1 = widget_base(arcsbase, /row)
  text = widget_label(ab1, Value="User interaction:   ")
;  arc_options = ["Auto choose", "Force Arc Lamp", "Force OH Lines"]
  arc_options = ["No Plots","Inspect Results","Interactive"]
  arcchoose = WIDGET_COMBOBOX(ab1, uname='arc_tab',Value=arc_options, uval='arc_dropbox')
  WIDGET_CONTROL, arcchoose, set_combobox_select=arcmode
  arc_help = widget_button(ab1,value="?", uval='arc_help')
  ab2 = widget_base(arcsbase, /row)
  arcverb_options = ["Silent", "Verbose", "LOUD"]
  text = widget_label(ab2, Value="Verbosity:          ")
  arcverbw = WIDGET_COMBOBOX(ab2, uname='arc_verb',Value=arcverb_options, uval='arcverb_dropbox')
  WIDGET_CONTROL, arcverbw, set_combobox_select=arcverb
;  arcverb_help = widget_button(ab2,value="?", uval='wpix_help')

  skybase = WIDGET_BASE(prefsbase, title = 'Sky Model Preferences', /column, /frame)
  msg = WIDGET_LABEL(skybase, Value='Sky Model')
  sk1 = widget_base(skybase, /row)
  text = widget_label(sk1, Value="Sky model calculation:   ")
  sky_options = ["Derive from sci frame","Use separate file"]
  skychoose = WIDGET_COMBOBOX(sk1, uname='sky_tab',Value=sky_options, uval='sky_dropbox')
  WIDGET_CONTROL, skychoose, set_combobox_select=skymode
  sky_help = widget_button(sk1,value="?", uval='sky_help')
  sk2 = widget_base(skybase, /row)
  text = widget_label(sk2, Value="Reset matched sky exposures:   ")
  sky_options2 = ["No","Yes"]
  resetskychoose = WIDGET_COMBOBOX(sk2, uname='sky_tab2',Value=sky_options2, uval='sky_dropbox2')
  WIDGET_CONTROL, resetskychoose, set_combobox_select=skyreset
  sky_help2 = widget_button(sk2,value="?", uval='sky_help2')


  findbase = WIDGET_BASE(prefsbase, title = 'Object/Aperture Finding Preferences', /col, /frame)
  msg = WIDGET_LABEL(findbase, Value='Object/Aperture Finding')
  of1 = widget_base(findbase, /row)
  text = widget_label(of1, Value="User interaction:   ")
  find_options = ["Automated", "Inspect Results", "Interactive/User Defined"]
  findchoose = WIDGET_COMBOBOX(of1, uname='find_tab',Value=find_options, uval='find_dropbox')
  WIDGET_CONTROL, findchoose, set_combobox_select=findmode
  aps_help = widget_button(of1,value="?", uval='aps_help')
  of2 = widget_base(findbase, /row)
  findverb_options = ["Silent", "Verbose", "LOUD"]
  text = widget_label(of2, Value="Verbosity:          ")
  findverbw = WIDGET_COMBOBOX(of2, uname='find_verb',Value=findverb_options, uval='findverb_dropbox')
  WIDGET_CONTROL, findverbw, set_combobox_select=findverb
;  apsverb_help = widget_button(of2,value="?", uval='wpix_help')

  extbase = WIDGET_BASE(prefsbase, title = 'Extraction Preferences', /col, /frame)
  msg = WIDGET_LABEL(extbase, Value='Spectral Extraction')
  eb1 = widget_base(extbase, /row)
  ext_options = ["Optimal (Faint objs)", "Boxcar (Bright objs)"]
  text = WIDGET_LABEL(eb1, Value="Extraction Method:  ")
  extchoose = WIDGET_COMBOBOX(eb1, uname='ext_tab',Value=ext_options, uval='ext_dropbox', /align_right)
  WIDGET_CONTROL, extchoose, set_combobox_select=extmode
;  ext_help = widget_button(eb1,value="?", uval='wpix_help')

  clobbase = WIDGET_BASE(prefsbase, title = 'Clobber', /col, /frame)
  msg = WIDGET_LABEL(clobbase, Value='Overwrite existing extractions (slow)?')
  cb1 = widget_base(clobbase, /row, /nonexclusive)
  clobberbt = WIDGET_BUTTON(cb1, Value='Clobber', uname='clob', uvalue='set_clob', TOOLTIP='Delete files.')
  Widget_Control, clobberbt, Set_Button=0 ;Keep 'not loud' as a default

  prefsquit = WIDGET_BUTTON(prefsbase, Value='Exit Prefs', uvalue='quit_prefs')

  WIDGET_CONTROL, prefsbase, set_uval=state

  WIDGET_CONTROL, prefsbase, /realize
  xmanager, 'fire_pipe_setprefs', prefsbase

end


;;-----------------------------------------------------------------
;;-----------------------------------------------------------------
;;-----------------------------------------------------------------


;Main program
PRO firehose, dirname

COMMON share;, firestrct, main;flatinter, traceinter, singlesinter, fire_pipe_inter, fire_pipe_clobber, fire_pipe_verbose, fire_pipe_bright, fire_mkstrct_verbose, fire_mkstrct_loud, xtell_clobber, xtell_quick, fire_pipe_plot

;** /Added by J. Gagne !@!@!@!
setenv,'XIDL_DIR=/Users/gagne/Dropbox/IDL/IDL_Library/23-XIDL/xidl/'
setenv,'IDLSPEC2D_DIR=/Users/gagne/Dropbox/IDL/IDL_Library/23-XIDL/idlspec2d/'
setenv, 'IDLUTILS_DIR=/Users/gagne/Dropbox/IDL/IDL_Library/23-XIDL/idlutils/'
setenv,'FIRE_DIR=/Users/gagne/Dropbox/IDL/IDL_Library/21-FireHose/'
;d0 = '/Users/gagne/Dropbox/IDL/IDL_Library/21-FireHose/'+objname+'/'
if ~keyword_set(dirname) then message, ' Please specify a data directory !'
if ~file_test(dirname) then message, ' This data directory does not exist !'
fix_path, dirname
d0 = dirname
objname = file_basename(dirname)
rawdir = d0+'Raw'+path_sep()

if ~file_test(rawdir) then begin
  rawdir = d0
  ;message, ' The directory must contain a "Raw'+path_sep()+'" subdirectory that contains all raw fits files !'
endif

;Generate a manual catalog
manlog = 'manual_log.txt'
if ~file_test(rawdir+manlog) then begin
  ff = file_search(rawdir+'*.fits*')
  
  ;Get exposure times first and check for corrupted fits files at the same time
  exptimes = sxpar_mul(ff,'EXPTIME',IS_CORRUPTED=is_corrupted)
  
  ;MOVE BAD FITS FILES TO CORRUPTED DIRECTORY
  bad = where(is_corrupted, nbad)
  if nbad ne 0L then begin
    for bi=0L, nbad-1L do begin
      message, ' Found a bad FITS file ! Moving "'+file_basename(ff[bad[bi]])+'" to corrupted subdir !', /CONTINUE
      corrdir = file_dirname(ff[bad[bi]])+path_sep()+'corrupted'+path_sep()
      if ~file_test(corrdir) then file_mkdir, corrdir
      file_move, ff[bad[bi]], corrdir+file_basename(ff[bad[bi]]), /ALLOW_SAME
    endfor
    if n_elements(bad) eq n_elements(ff) then $
      message, 'All FITS files were found to be corrupted ! (Maybe the wrong fits extension was used ?'
    remove, bad, ff
    exptimes = sxpar_mul(ff,'EXPTIME',IS_CORRUPTED=is_corrupted)
  endif
  
  if n_elements(ff) gt 1 or ff[0] ne '' then $
    printuarr,rawdir+manlog,file_basename(ff),sxpar_mul(ff,'OBJECT'),sxpar_mul(ff,'GRISM'),strtrim(round(exptimes),2),/jus,symbol=' | ',title=['File','Object','Grism','Texp(s)'],/new
endif
reduxpath = d0+'redux'+path_sep()
folder_check, reduxpath
folder_check, reduxpath+'Arcs'+path_sep()
folder_check, reduxpath+'Final'+path_sep()
folder_check, reduxpath+'Flat'+path_sep()
folder_check, reduxpath+'Object'+path_sep()
folder_check, reduxpath+'Structs'+path_sep()
cd, reduxpath
reduxdir = reduxpath
obscat = rawdir+'Catalog.txt'
;file_mkdir, rawdir
;file_mkdir, reduxdir
;** /Added by Jonathan Gagne

   flatinter=0  ;Default setup
   traceinter=0 ;Default setup
   singlesinter=0 ;Default setup
   multispec_out=0 ;Default setup

  ;Get current working directory
  spawn, 'pwd', cwd
  fix_path, cwd
  arr = strsplit(cwd, path_sep())
  ;rawdir   = strmid(cwd,0,arr[n_elements(arr)-1])+strtrim('/Raw/',2)
  ;reduxdir = strtrim(cwd,2)+'/'

  if (x_chkfil(rawdir, /SILENT) EQ 0) then begin ;; Default Raw directory doesn't exist.
     rawdir = cwd
  endif

  spawn, 'whoami', name
  tmp = { firestrct }
                                                       
  main = widget_base(/Col, Title='FIREHOSE', MBAR=bar)      ; main base
  
  
  ;; MENU BUTTONS
  
  ;; File
  menu1 = WIDGET_BUTTON(bar, VALUE='File', /MENU)
  bload = WIDGET_BUTTON(menu1, VALUE='Load saved firestrct', uvalue='load_struct', accelerator="Ctrl+L")
  bsave = WIDGET_BUTTON(menu1, VALUE='Save firestrct', uvalue='write_struct', accelerator="Ctrl+S")
  bquit = WIDGET_BUTTON(menu1, VALUE='Quit', uvalue='quit', accelerator="Ctrl+Q")
   
  ;; Options
  menu2 = WIDGET_BUTTON(bar, VALUE='Options', /MENU)
  bclear = WIDGET_BUTTON(menu2, VALUE='Clear Output from FIREHOSE', uvalue='clear', accelerator="Ctrl+Shift+C")
  blabel = WIDGET_BUTTON(menu2, VALUE='Set Title...', uvalue='firehose_label')
  bplay = WIDGET_BUTTON(menu2, VALUE='Play with firestrct', uvalue='play')
  brun = WIDGET_BUTTON(menu2, VALUE='Run...', uvalue='run_stuff', /menu)
  brun_pipe = WIDGET_BUTTON(brun, VALUE='Extraction pipeline', uvalue='go_pipe', accelerator="Ctrl+E")
  brun_tell = WIDGET_BUTTON(brun, VALUE='Telluric correction', uvalue='go_sensfunc', accelerator="Ctrl+T")
  brun_comb = WIDGET_BUTTON(brun, VALUE='Combine to 1D', uvalue='go_combine', accelerator="Ctrl+1")
  
  ;; Display
  menu3 = WIDGET_BUTTON(bar, VALUE='Display', /MENU)  
  bpwd = WIDGET_BUTTON(menu3, VALUE='Current working directory', uvalue='pwd')
  braw = WIDGET_BUTTON(menu3, VALUE='Current Raw data directory', uvalue='raw', accelerator="Ctrl+D")
  bobjids = WIDGET_BUTTON(menu3, VALUE='Object IDs', uvalue='show_objids')
  bcontents = WIDGET_BUTTON(menu3, VALUE='Contents', uvalue='pwd', /MENU)
  barcsdir = WIDGET_BUTTON(bcontents, VALUE='Arcs'+path_sep(), uvalue='arcsdir')
  bfinaldir = WIDGET_BUTTON(bcontents, VALUE='Final'+path_sep(), uvalue='finaldir')
  bflatdir = WIDGET_BUTTON(bcontents, VALUE='Flat'+path_sep(), uvalue='flatdir')
  bobjectdir = WIDGET_BUTTON(bcontents, VALUE='Object'+path_sep(), uvalue='objectdir')
  btimesaved = WIDGET_BUTTON(menu3, VALUE='Time firestrct last saved', uvalue='timesaved')

  ;; Help
  bhelp = WIDGET_BUTTON(bar, Value='Help', /help)
  babout = WIDGET_BUTTON(bhelp, VALUE='About FIREHOSE', uvalue='about')
  bwebsite = WIDGET_BUTTON(bhelp, VALUE='FIRE website', uvalue='website')
  bfirehelp = WIDGET_BUTTON(bhelp, Value='Help with Sections', uvalue='fire help', /MENU)
  bhelpsetup = WIDGET_BUTTON(bfirehelp, Value='Setup', uvalue='setup help')
  bhelptrace = WIDGET_BUTTON(bfirehelp, Value='Trace', uvalue='trace help')
  bhelpflats = WIDGET_BUTTON(bfirehelp, Value='Flats', uvalue='flats help')
  bhelpstructure = WIDGET_BUTTON(bfirehelp, Value='Structure', uvalue='structure help')
  bhelpextract = WIDGET_BUTTON(bfirehelp, Value='Extract', uvalue='extract help')
  bhelptelluric = WIDGET_BUTTON(bfirehelp, Value='Telluric', uvalue='telluric help')
  bhelpcombine = WIDGET_BUTTON(bfirehelp, Value='Combine', uvalue='combine help')
  bhelpquicklook = WIDGET_BUTTON(bfirehelp, Value='Quicklook', uvalue='quicklook help')
  bhotkeys = WIDGET_BUTTON(bhelp, Value='Hot keys list', uvalue='hot keys')
  
  wlabel = WIDGET_LABEL(main, value='Output from FIREHOSE')
  wtext = WIDGET_TEXT(main, XSIZE=60, ysize=20, /frame, value='Welcome, ' + name, /wrap, /scroll)  
  wtab = widget_tab(main, Location=0, uvalue='tab')


  wt1aa = Widget_base(wtab, Title='Setup', /row)
  wt1 = Widget_base(wt1aa, /col)
  wt1a = Widget_base(wt1, /row)

  setup_label = WIDGET_LABEL(wt1a, Value='Raw Directory    ')
  setupdir  = WIDGET_TEXT(wt1a, /editable, xsize=25, value=rawdir, uvalue='rawdir')
  setupbt =  WIDGET_BUTTON(wt1a, Value='Browse', uvalue='raw_pick')
  setdefraw = WIDGET_BUTTON(wt1a, Value='Set to default', uvalue='default_raw')

  wt1c = Widget_base(wt1, /row)
  redux_label = WIDGET_LABEL(wt1c, Value='Redux Directory  ')
  reduxdir  = WIDGET_TEXT(wt1c, /editable, xsize=25, value=reduxdir, uvalue='reduxdir')
  reduxbt =  WIDGET_BUTTON(wt1c, Value='Browse', uvalue='redux_pick')
  setdefredux = WIDGET_BUTTON(wt1c, Value='Set to default', uvalue='default_redux')

  wt1b = Widget_base(wt1, /row)
  obscat_label = WIDGET_LABEL(wt1b, Value='Observing Catalog')
  obscat_list  = WIDGET_LIST(wt1b, xsize=25, ysize=3, value=obscat, uvalue='tab')
  obscatbt =  WIDGET_BUTTON(wt1b, Value='Browse', uvalue='cat_pick')

 
 	;; Read in bmp image
 	fire_dir = getenv('FIRE_DIR')
	logo_file = fire_dir + "Doc/fire_logo.bmp"
	if( FILE_TEST( logo_file ) EQ 1 ) then begin
		image = read_bmp(logo_file)
		;; Convert byte array to bitmap byte array
		image = CVTTOBM( image )
		wt1_logo = WIDGET_BUTTON(wt1aa, VALUE=image, uvalue='fire_logo', xsize=125, ysize=125, /align_center)
	endif

  ;Here the user selects one (or more) files, then it calls fire_traceorders
  wt2 = WIDGET_BASE(wtab, Title='Trace', /col, /base_align_right)
  tflt = WIDGET_BASE(wt2, /row)
  tflat_label = WIDGET_LABEL(tflt, Value='Trace Flat')
  tflt_file  = WIDGET_TEXT(tflt, /editable, value='', xsize=40, ysize=4, /scroll)
  tflat_bt =  WIDGET_BUTTON(tflt, Value='Browse', uvalue='trace_pick')
  go_trace_bt = WIDGET_BUTTON(wt2, value='Trace Orders', uvalue='go_trace')
  wtrcinter = WIDGET_BASE(wt2, /nonexclusive)
  traceinterbt = WIDGET_BUTTON(wtrcinter, Value='Interactive', uvalue='trace_inter_tog', TOOLTIP='Toggle interactive editing of slit boundaries', /align_left)
  Widget_Control, traceinterbt, Set_Button=0 ;Keep non-interactive on as a default

  ;Now we deal with flat files
  wt4 = WIDGET_BASE(wtab, Title='Flats', /col, /base_align_left)

  wleft = WIDGET_BASE(wt4, /row)
  wright = WIDGET_BASE(wt4, /row)
  wflat = WIDGET_BASE(wleft, /row)

  flat_label = WIDGET_LABEL(wflat, Value='Flat Field Files')
  flatfiles  = WIDGET_TEXT(wflat, /editable, value='', xsize=40, ysize=4, /scroll)
  flatbt =  WIDGET_BUTTON(wflat, Value='Browse', uvalue='flat_pick')

  willum = WIDGET_BASE(wleft, /row)
  illum_label = WIDGET_LABEL(willum, Value='Illum Flat Files')
  illumflatfiles  = WIDGET_TEXT(willum, /editable, value='', xsize=40, ysize=4, /scroll)
  illumbt =  WIDGET_BUTTON(willum, Value='Browse', uvalue='illum_flat_pick')
  
  wflatpixarc_label = WIDGET_LABEL(wright, Value='Slit Tilt File')
  pixarcfile = WIDGET_TEXT(wright, /editable, value='', xsize=40, ysize=1)
  pixarcbt  = WIDGET_BUTTON(wright, Value='Browse', uvalue='pix_arc_pick') 
 
  omsklabel = WIDGET_LABEL(wright, Value='Order Mask')
  omsk = WIDGET_text(wright, Value='', uvalue='list', xsize=40, ysize=1)
  omskbt = WIDGET_BUTTON(wright, Value='Browse', uvalue='omsk_pick')
  
  w3 = WIDGET_BASE(wt4, /row)
  go_flat_bt = WIDGET_BUTTON(w3, value='Make Flat Field', uvalue='go_flat')
  wflatinter = WIDGET_BASE(w3, /nonexclusive)
  flatinterbt = WIDGET_BUTTON(wflatinter, Value='Interactive', uvalue='flat_inter_tog', TOOLTIP='Toggle interactive viewing of flats in each order')
  Widget_Control, flatinterbt, Set_Button=0 ;Keep non-interactive on as a default
  
  ;Now make the FIRE structure
  wt5 = WIDGET_BASE(wtab, Title='Structure', /col);, /base_align_right)
  wt5a = WIDGET_BASE(wt5, /row)
  strctlabel = WIDGET_LABEL(wt5a, Value='Structure Filename')
  strctname = WIDGET_TEXT(wt5a, Value='firestrct.fits', uvalue='strctname', /editable)
  strctbt = WIDGET_BUTTON(wt5a, Value='Generate Structure', uvalue='make_struct')
  sbut2 = WIDGET_BUTTON(wt5a, Value='Load Structure', uvalue='load_struct', accelerator='Ctrl+L')
  sbut3 = WIDGET_BUTTON(wt5a, Value='Edit (and Save) Structure',  uvalue='edit_struct')
  sbut5 = WIDGET_BUTTON(wt5a, Value='Run Script (and Save)',  uvalue='edit_script', TOOLTIP='Run precomposed firestrct edit script')
  sbut4 = WIDGET_BUTTON(wt5a, Value='Save Structure', uvalue='write_struct', accelerator="Ctrl+S")
  
  msglabel = WIDGET_LABEL(wt5, Value="Select Flat Files, Illumination Files, and Order Masks for Reductions (Default All)")
  
  wt5b = WIDGET_BASE(wt5, /row)

  if (FILE_TEST(strtrim(reduxdir,2)+"Flat", /dir)) then begin
     spawn, "ls"+strtrim(reduxdir,2)+"Flat/Pixflat*", flatls
     spawn, "ls"+strtrim(reduxdir,2)+"Flat/Illumflat*", illumls
     spawn, "ls"+strtrim(reduxdir,2)+"Flat/Orders*", ordrls
  endif else begin
     flatls=''
     illumls=''
     ordrls=''
  endelse

  strctflabel = WIDGET_LABEL(wt5b, Value='Pixel Flats')
  strctflats = WIDGET_LIST(wt5b, Value='', uvalue='list', /multiple, xsize=30, ysize=5)

  strctilabel = WIDGET_LABEL(wt5b, Value='Illum Flats')
  strctiflats = WIDGET_LIST(wt5b, Value='', uvalue='list', /multiple, xsize=30, ysize=5)

  strctordrlabel = WIDGET_LABEL(wt5b, Value='Order Masks')
  strctomask = WIDGET_LIST(wt5b, Value='', uvalue='list', /multiple, xsize=30, ysize=5)

  widget_control, strctflats, set_value=flatls
  widget_control, strctiflats, set_value=illumls
  widget_control, strctomask, set_value=ordrls

  wtc5 = WIDGET_BASE(wt5, /nonexclusive, /row)
  fire_mkstrct_verbosebt = WIDGET_BUTTON(wtc5, Value='Verbose?', uvalue='fire_mkstrct_verbose', TOOLTIP='Print out some status information when generating the structure.')
  fire_mkstrct_verbose = 0
  Widget_Control, fire_mkstrct_verbosebt, Set_Button=0 ;Keep 'not verbose' as a default
  fire_mkstrct_loudbt = WIDGET_BUTTON(wtc5, Value='Loud?', uvalue='fire_mkstrct_loud', TOOLTIP='Print out A LOT of status information when generating the structure.')
  fire_mkstrct_loud = 0
  Widget_Control, fire_mkstrct_loudbt, Set_Button=0 ;Keep 'not loud' as a default
  fire_no_cat = WIDGET_BUTTON(wtc5, Value='Do not use Catalog', uname='off', uvalue='fire_no_cat', TOOLTIP='Do not use Magellan Observation catalog.')
  Widget_Control, fire_no_cat, Set_Button=0 ;Keep 'not loud' as a default

  ;PIPELINE
  wt7 = WIDGET_BASE(wtab, Title='Extract',  /col)
  wt7a = WIDGET_BASE(wt7, /row)
  go_pipe_bt = WIDGET_BUTTON(wt7a, Value='Run Pipeline', uvalue='go_pipe', xsize=350, ysize=150)
  wt7b = WIDGET_BASE(wt7a, /row)
  w_spec_target_label = WIDGET_LABEL(wt7b, Value ='Reduce ONLY these objects')
  w_spec_target =  WIDGET_LIST(wt7b, Value='', uvalue='select_target', /multiple, xsize=50, ysize=8)
  wt7c = WIDGET_BASE(wt7, /row)
  ; Singles option doesn't exist yet.  Don't put a button until it does...'
  singleinterbt = -1
  ;singleinterbt = WIDGET_BUTTON(wt7c, Value='Singles', uvalue='single_tog', TOOLTIP='Extraction of each spectrum of an object (before coaddition)', /align_left)

  if (0) then begin
  ;Widget_Control, singleinterbt, Set_Button=1 ;Keep interactive on as a default
  fire_pipe_chkbt = WIDGET_BUTTON(wt7c, Value='Interactive', uvalue='fire_pipe_chkbt', TOOLTIP='Plot checks during the extraction process', /align_left)
  Widget_Control, fire_pipe_chkbt, Set_Button=0 ; Default: don't run fire_pipe with the /chk flag
  fire_pipe_inter = 0
  fire_pipe_clobberbt = WIDGET_BUTTON(wt7c, Value='Clobber', uvalue='fire_pipe_clobber', TOOLTIP='Delete previously reduced data', /align_left)
  Widget_Control, fire_pipe_clobberbt, Set_Button=0 ; Default: don't run fire_pipe with the /clobber flag
  fire_pipe_clobber = 0
  fire_pipe_brightbt = WIDGET_BUTTON(wt7c, Value='Boxcar', uvalue='fire_pipe_bright', TOOLTIP='Use for bright objects to change CR rejections', /align_left)
  Widget_Control, fire_pipe_brightbt, Set_Button=0 ; Default: don't run fire_pipe with the /clobber flag
  fire_pipe_bright = 0
  fire_pipe_verbosebt = WIDGET_BUTTON(wt7c, Value='Verbose?', uvalue='fire_pipe_verbose', TOOLTIP='Prints out some status information to screen', /align_left)
  Widget_Control, fire_pipe_verbosebt, Set_Button=1 ; Default: run fire_pipe with the /verbose flag
  fire_pipe_verbose = 1
  fire_pipe_plotbt = WIDGET_BUTTON(wt7c, Value='Make Plots', uvalue='fire_pipe_plot', TOOLTIP='Plots some status to screen', /align_left)
  Widget_Control, fire_pipe_plotbt, Set_Button=1 ; Default: run fire_pipe with the /verbose flag
  fire_pipe_plot = 1

endif else begin

   fire_pipe_prefs = WIDGET_BUTTON(wt7c, Value='Preferences', uvalue='fire_pipe_preferences', TOOLTIP='Set preferences / interaction level for extraction process')
   wpixmode = 0
   arcmode  = 0
   skymode = 0
   skyreset = 0
   findmode = 0
   extmode  = 0
   arcverb  = 0
   findverb  = 0

   fire_pipe_plot = 1
   fire_pipe_verbose = 1
   fire_pipe_bright = 0
   fire_pipe_clobber = 0
   fire_pipe_inter = 0

endelse

  ;TELLURIC
  if (0) then begin
     wt6 = WIDGET_BASE(wtab, Title='Telluric', /col, /base_align_right)
     wstd = WIDGET_BASE(wt6, /row)
     std_label = WIDGET_LABEL(wstd, Value='Standard Star')
     stdfile  = WIDGET_TEXT(wstd, /editable, value='', xsize=40)
     stdbt =  WIDGET_BUTTON(wstd, Value='Browse', uvalue='std_pick')
     
     wstdflx = WIDGET_BASE(wt6, /row)
     stdflx_label = WIDGET_LABEL(wstdflx, Value='Standard Star Flux Table')
     stdflxfile  = WIDGET_TEXT(wstdflx, /editable, value='', xsize=40)
     stdflxbt =  WIDGET_BUTTON(wstdflx, Value='Browse', uvalue='std_flux_pick')
     
     wstdout = WIDGET_BASE(wt6, /row)
     stdout_label = WIDGET_LABEL(wstdout, Value='Sensitivity Function')
     stdoutfile  = WIDGET_TEXT(wstdout, /editable, value=cwd+'std.sav', xsize=40)
     go_sensfunc_bt = WIDGET_BUTTON(wt6, value='Make Sensitivity Function', uvalue='go_sensfunc')
  endif else begin
     wt6 = WIDGET_BASE(wtab, Title='Telluric',  /col)
     wt6a = WIDGET_BASE(wt6, /row)
     go_sensfunc_bt = WIDGET_BUTTON(wt6a, Value='Correct Tellurics', uvalue='go_sensfunc', xsize=350, ysize=100)
     wt6b = WIDGET_BASE(wt6a, /row)
     w_telcor_label = WIDGET_LABEL(wt6b, Value ='Choose targets to calibrate tellurics')
;     w_telcor_targets =  WIDGET_LIST(wt6b, Value='', uvalue='select_tellurics', /multiple, xsize=50, ysize=8)


     w_telcor_targets =  WIDGET_TREE(wt6b, ysize=140)
;     w_tree_root =  WIDGET_TREE(w_telcor_targets, Value='Obsrun', uvalue='Root')

     wt6c = WIDGET_BASE(wt6, /row)
     wt6c2 = WIDGET_BASE(wt6c, /nonexclusive, /row)
     xtell_clobberbt = WIDGET_BUTTON(wt6c2, Value='Reset Telluric Correction', uvalue='xtell_clobber', $
                                     TOOLTIP='Ignores any previous calibration (turn off to go to Xtellcor Finish).', /align_left)
     Widget_Control, xtell_clobberbt, Set_Button=0 ; Default: allow Xtellcor finish
     xtell_clobber = 0
     xtell_quickbt = WIDGET_BUTTON(wt6c2, Value='Quick', uvalue='xtell_quick', $
                                     TOOLTIP="Quick, but less acurate.  Can't make velocity shifts, or choose which telluric files to use.", /align_left)
     Widget_Control, xtell_quickbt, Set_Button=0 ; Default: don't run quickly
     xtell_quick = 0
     rebuild_tell_tree_bt = WIDGET_BUTTON(wt6c, value='Rebuild Telluric File Tree', uvalue='rebuild_telluric_tree', /align_right) 
     
  endelse

  ; COMBINE 
  if (0) then begin
     wt8 = WIDGET_BASE(wtab, Title='Combine', /col, /base_align_right)
     wstd = WIDGET_BASE(wt8, /row)
     std_label = WIDGET_LABEL(wstd, Value='Standard Star')
     stdfile  = WIDGET_TEXT(wstd, /editable, value='', xsize=40)
     stdbt =  WIDGET_BUTTON(wstd, Value='Browse', uvalue='std_pick')
     
     wstdflx = WIDGET_BASE(wt8, /row)
     stdflx_label = WIDGET_LABEL(wstdflx, Value='Standard Star Flux Table')
     stdflxfile  = WIDGET_TEXT(wstdflx, /editable, value='', xsize=40)
     stdflxbt =  WIDGET_BUTTON(wstdflx, Value='Browse', uvalue='std_flux_pick')
     
     wstdout = WIDGET_BASE(wt8, /row)
     stdout_label = WIDGET_LABEL(wstdout, Value='Sensitivity Function')
     stdoutfile  = WIDGET_TEXT(wstdout, /editable, value=cwd+'std.sav', xsize=40)
     go_sensfunc_bt = WIDGET_BUTTON(wt8, value='Make Sensitivity Function', uvalue='go_sensfunc')
  endif else begin     
     wt8 = WIDGET_BASE(wtab, Title='Combine',  /col)
     wt8a = WIDGET_BASE(wt8, /row)
     go_combine_bt = WIDGET_BUTTON(wt8a, Value='Combine to 1D', uvalue='go_combine', xsize=350, ysize=150)
     wt8b = WIDGET_BASE(wt8a, /row)
     w_combine_label = WIDGET_LABEL(wt8b, Value ='Choose targets to combine :')
     w_combine_targets =  WIDGET_LIST(wt8b, Value='', uvalue='select_combine', /multiple, xsize=50, ysize=8)
     wt8c = WIDGET_BASE(wt8, /nonexclusive)
     multispec_outbt = WIDGET_BUTTON(wt8c, Value='Write out individual orders', uvalue='multispec_out', TOOLTIP='Save separate fits files for the individual combined spectral orders', /align_left)
     Widget_Control, multispec_outbt, Set_Button=0 ;By default output multiple orders
  endelse

; QUICKLOOK
  wt9 = WIDGET_BASE(wtab, Title='Quicklook', /col)
  wql = WIDGET_BASE(wt9, /row)
  ql_label = WIDGET_LABEL(wql, Value='Quicklook Target')
  qlfile  = WIDGET_TEXT(wql, /editable, value='', xsize=40)
  qlfilebt =  WIDGET_BUTTON(wql, Value='Browse', uvalue='ql_file_pick')

  qlbt = WIDGET_BUTTON(wt9, Value='Quicklook', uvalue='go_ql')
  
  widget_control, main, /realize ; create the widgets

;  state = {cwd:cwd, wlabel:wlabel, wtext:wtext, setupdir:setupdir,reduxdir:reduxdir, obscat_list:obscat_list, tflt_file:tflt_file, pixarcfile:pixarcfile, flatinterbt:flatinterbt, traceinterbt:traceinterbt, singleinterbt:singleinterbt, fire_pipe_chkbt:fire_pipe_chkbt, fire_pipe_clobberbt:fire_pipe_clobberbt, fire_pipe_brightbt:fire_pipe_brightbt, fire_pipe_verbosebt:fire_pipe_verbosebt, fire_pipe_plotbt: fire_pipe_plotbt, fire_mkstrct_verbosebt: fire_mkstrct_verbosebt, fire_mkstrct_loudbt: fire_mkstrct_loudbt, fire_no_cat: fire_no_cat, xtell_clobberbt: xtell_clobberbt, xtell_quickbt: xtell_quickbt, flatfiles:flatfiles, illumflatfiles:illumflatfiles, strctname:strctname, w_spec_target:w_spec_target, qlfile:qlfile, bquit:bquit,strctflats:strctflats, strctiflats:strctiflats, strctomask:strctomask, w_telcor_targets:w_telcor_targets, tell_branches:"", w_combine_targets:w_combine_targets, multispec_outbt:multispec_outbt, omsk:omsk}

  state = {cwd:cwd, wlabel:wlabel, wtext:wtext, setupdir:setupdir,reduxdir:reduxdir, obscat_list:obscat_list, tflt_file:tflt_file, pixarcfile:pixarcfile, flatinterbt:flatinterbt, traceinterbt:traceinterbt, singleinterbt:singleinterbt, fire_mkstrct_verbosebt: fire_mkstrct_verbosebt, fire_mkstrct_loudbt: fire_mkstrct_loudbt, fire_no_cat: fire_no_cat, xtell_clobberbt: xtell_clobberbt, xtell_quickbt: xtell_quickbt, flatfiles:flatfiles, illumflatfiles:illumflatfiles, strctname:strctname, w_spec_target:w_spec_target, qlfile:qlfile, bquit:bquit,strctflats:strctflats, strctiflats:strctiflats, strctomask:strctomask, w_telcor_targets:w_telcor_targets, tell_branches:"", w_combine_targets:w_combine_targets, multispec_outbt:multispec_outbt, omsk:omsk}

  widget_control, main, set_uvalue = state
  ;I think this makes the draw window the spot for plotting
  ;widget_control, draw, get_value=window
  ;wset, window
  
  xmanager, 'fire', main , /no_block      ; wait for events
END
