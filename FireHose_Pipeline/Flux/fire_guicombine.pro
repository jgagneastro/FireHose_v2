pro fire_guicombine, fire, $
                     targs=targs, $
                     groupleader=groupleader, $
                     WIDGET=widget, _EXTRA = keys, $
                     ORDERS=orders

  func_name = 'fire_guicombine'
  ntargs = n_elements(targs)
  used_tot = 0
  notused_tot = 0

  for itarg=0, ntargs-1 do begin
     
     outfile = 'FSpec/'+strtrim(targs[itarg],2)+'_F.fits'
     sigfile = 'FSpec/'+strtrim(targs[itarg],2)+'_E.fits'
     
     match = where(fire.object EQ targs[itarg] AND $
                   strtrim(strupcase(fire.exptype),2) EQ 'SCIENCE', nmatch)
     
     if nmatch EQ 0 then begin
        fire_siren, func_name + ": No files found for object " + targs[itarg] + "!"
        return
     endif else begin
        objstr_filenames = make_array( nmatch, 1, /string)
        used = make_array( nmatch, 1, value=0 )
     endelse
     first = 1

     ; Read in the individual files
     objstr_files = strarr(nmatch)
     for imatch=0, nmatch-1 do begin        
        objstr_file = strtrim(fire[match[imatch]].objstrfile,2)
        objstr_files[imatch] = objstr_file
        objstr_filenames[imatch] = fire_get_fitnum(objstr_file)
        if (x_chkfil(objstr_file) EQ 1) then begin
           objstr = mrdfits(objstr_file,1,hdr)
        endif else continue
        gd = where(objstr.flux NE 0, ngd) ;; Has the telluric correction been applied?
        if (ngd EQ 0) then begin
           print,func_name+": WARNING, this file is not telluric/flux corrected, skipping (" + $
                 objstr_file + ")"
           continue
        endif else begin
           used[imatch] = 1        
        endelse
        
        if (first EQ 1) then begin
           all_objstr = objstr
           output_hdr = hdr
           first = 0
           total_exptime = sxpar(hdr,"EXPTIME")
        endif else begin
           all_objstr = [all_objstr,objstr]
           total_exptime += sxpar(hdr,"EXPTIME")
        endelse
     endfor

     ;; if first still equals 1, then we've struck out: no matches.
     if( first EQ 1 ) then begin
     		if is_undefined(all_bad) EQ 1 then begin
     			all_bad = targs[itarg]
     		endif else begin
     			all_bad = [ all_bad, targs[itarg] ]    		
     		endelse
     endif else begin

        sxdelpar, output_hdr, "EXPTIME"
        sxaddpar, output_hdr, "EXPTIME", total_exptime
        
        if (file_test('FSpec', /DIR) EQ 0) then spawn, "mkdir FSpec"
        
        ;!@!@!@!@!@!
        cd,current=dir0
        dir0 += path_sep()+file_dirname(objstr_files[0])+path_sep()
        spos = strpos(file_basename(objstr_files[0]),'_')
        pref = strmid(file_basename(objstr_files[0]),0,spos)+'_'
        nums = strjoin(strtrim(long(file_basename(file_basename(strmid(file_basename(objstr_files),spos+1),'.gz'),'.fits')),2),',')
        ;outf = pref+'xcomb_'+strjoin(strtrim(long(file_basename(file_basename(strmid(file_basename(filelist),spos+1),'.gz'),'.fits')),2),'_')+'.fits'
        outf = pref+strjoin(file_basename(file_basename(strmid(file_basename(objstr_files),spos+1),'.gz'),'.fits'),'_')
        combfile = 'Object/'+outf+'.fits'
        if ~file_test(combfile) then begin
          fire_xcombspec,path_input=dir0,/fits_ext,prefix_input=pref,files_input=nums,save_input=outf, NPIX=45
          stop
        endif
        
        ;Rederive the telluric correction and apply it
        tmp = xmrdfits(objstr_files[0],1)
        tellcor = tmp.fx/tmp.flux
        bad = where(tellcor le 0., nbad)
        if nbad ne 0L then tellcor[bad] = !values.f_nan
        
        fspec_new = xmrdfits(combfile,1,hdr_new)
        norders = n_elements(tmp)
        for tci=0L, norders-1L do begin
          gtci = where(finite(tellcor[*,tci]) and tellcor[*,tci] lt median(tellcor[*,tci])/1d3,ngtci)
          if ngtci ne 0L then tellcor[gtci,tci] = !values.f_nan
          gtci = where(finite(tellcor[*,tci]),ngtci)
          if ngtci gt 5L then tellcor[gtci[0:2],tci] = !values.f_nan
          if ngtci gt 5L then tellcor[gtci[-3L:-1L],tci] = !values.f_nan
          fspec_new[tci].fx /= tellcor[*,tci]
          fspec_new[tci].var /= (tellcor[*,tci]^2d0)
        endfor
        mwrfits, fspec_new, 'Object/'+outf+'_tellcor.fits', hdr_new, /create
        fspec_new.wave *= 1d-4
        
        xmergeorders, science_input=dir0+outf+'_tellcor.fits', save_input=outf+'_merge'
        
        print, ' Done !'
        return
        stop
        
        ;Old way
        fire_combspec, all_objstr, fspec, FLAG=flag, ERR_MESSAGE=err_message
        
;        ;Put the xcombspec combination in the old format
;        gg = where(fspec.phys_ordr ne 0L, ngg)
;        npix = (size(fspec_new))[1]
;        tmpwave = fspec.wave
;        tmpflux = fspec.flux
;        tmpsig = fspec.sig
;        for qq=0L,ngg-1 do begin & $
;          tmpwave[0:npix-1L,gg[-1L-qq]] = reform(fspec_new[*,0,qq]) & $
;          tmpflux[0:npix-1L,gg[-1L-qq]] = reform(fspec_new[*,1,qq]) & $
;          tmpsig[0:npix-1L,gg[-1L-qq]] = reform(fspec_new[*,2,qq]) & $
;        endfor
;        fspec.wave = tmpwave
;        fspec.flux = tmpflux
;        fspec.sig = tmpsig
;        fspec.var = (fspec.sig)^(2d0)
        
        if FLAG NE 0 then begin
           if is_undefined(err_message) EQ 1 then err_message = "None provided."
           fire_siren, func_name + ": FAILURE combining " + targs[itarg] + $
                       "!  (Error message: " + err_message + $
                       ")  Skipping this target's correction.", WIDGET=widget, /both, /append
           used(*) = 0     	 
        endif else begin
           fire_1dspec, fspec, outfile, sigfile, hdr=output_hdr;, /chk
           if (keyword_set(ORDERS)) then fire_multispec, fspec, outfile, sigfile, hdr=output_hdr
        endelse
        
     endelse
     
		;; Print names of used and unused files to screen
     use_spots = where( used EQ 1, nused, COMPLEMENT=notused_spots, NCOMPLEMENT=nnotused )
     used_tot = used_tot + nused
     notused_tot = notused_tot + nnotused
     if is_undefined(message) then message = make_array( ntargs, 1, /string )
     tmp = "  Object " + strtrim(targs[itarg],2) + ": "
     if nused NE 0 then begin
        tmp = tmp + fire_string(nused) + " file" + s_or_not(nused) + " combined (" + fire_str_array_to_str( objstr_filenames(use_spots) ) + ").  "
     endif else begin
        tmp = tmp + "No files combined!  "		
     endelse
     if nnotused NE 0 then begin
        tmp = tmp +  fire_string(nnotused) + " file" + s_or_not(nnotused) + " NOT combined (" + fire_str_array_to_str( objstr_filenames(notused_spots) ) + ")."
     endif else begin
        tmp = tmp + "No files omitted."			
     endelse
     message(itarg) = tmp
     
  endfor ;; end, cycle through targets

	;; Warn user about objects that had no telluric corrected files.
	if is_undefined(all_bad) EQ 0 then begin
		nall_bad = n_elements(all_bad)
		fire_siren, func_name + ": ERROR!  Could not locate any files which have been " + $
	            "telluric corrected for " + fire_string(nall_bad) + " object" + s_or_not(nall_bad) + $
	            "!: " + fire_str_array_to_str(all_bad) + ".  Please correct tellurics and" + $
	            " then try again.", widget = widget, /both, _EXTRA = keys
	endif
  
	print, func_name + ": Final file checklist:"
	print, message
	print, "  Total: " + fire_string(used_tot) + " used, " + fire_string(notused_tot) + " unused"
	print, func_name + ": Program complete!"    
     
end
