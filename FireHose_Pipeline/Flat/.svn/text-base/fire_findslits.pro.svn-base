function fire_findslits, DATA=data, TRACEFLAT=traceflat, $
                         ORDERFILE=orderfile, ORDR_STR_FILE=ordr_str_file, $
                         CLOBBER=clobber, VERBOSE=VERBOSE, CHK=CHK, $
                         CHECK_INPUTS=check_inputs, INTER=INTER

  func_name = "fire_findslits.pro"
  if keyword_set(traceflat) then begin
     default_filename = FIRE_GET_FILE_NAMES(traceflat, /ORDERFILE)
     default_str_filename = FIRE_GET_FILE_NAMES(traceflat, /ORDR_STR_FILE)
  endif

  if keyword_set( data ) then begin
     if NOT keyword_set(orderfile) AND NOT keyword_set(TRACEFLAT) then begin
        orderfile = fire_get(data,/ORDERFILE)
     endif else if NOT keyword_set(orderfile) AND keyword_set(TRACEFLAT) then begin
        orderfile = default_filename
        fire_set,data,orderfile,/ORDERFILE
     endif
     if NOT keyword_set(ordr_str_file) AND NOT keyword_set(TRACEFFLAT) then begin
        ordr_str_file = fire_get(data,/ORDR_STR_FILE)
     endif else if NOT keyword_set(ordr_str_file) AND keyword_set(TRACEFLAT) then begin
        ordr_str_file = default_str_filename
        fire_set,data,ordr_str_file,/ORDR_STR_FILE
     endif
     if NOT keyword_set(traceflat) then traceflat = fire_get(data,/FLATS)
  endif
  
  if NOT keyword_set( traceflat ) AND NOT keyword_set( DATA ) then begin
     fire_siren, func_name + ": ERROR with fire_findslits!  fire_findslits must be passed either a firestrct structure, or keyword traceflat.  Exiting with error."
     RETURN, 1
  endif 
  
  if keyword_set(VERBOSE) OR keyword_set(CHECK_INPUTS) then begin
     print, ''
     print, "++++++++++++++++++++++++++++++++++++"
     print, func_name, ": /VERBOSE flag passed.  Set values:"
     print, "Flats: ", traceflat
     print, "Order file name: ", orderfile
     print, "Order structure file name: ", ordr_str_file
     print, "++++++++++++++++++++++++++++++++++++"
     print, ''
     if keyword_set(CHECK_INPUTS) then RETURN, 0
  endif
  
  ;; Determine the name of the output file
  if NOT keyword_set( orderfile ) then begin
     file = default_filename
  endif else begin
     file = orderfile
  endelse
  
  if NOT keyword_set( ordr_str_file ) then begin
     ordr_str_file = default_str_filename
  endif 
  
  ;; Check if the file already exists.  If it does, then don't bother re-creating it...
  if NOT keyword_set( CLOBBER ) AND FILE_TEST( file ) eq 1 then begin
     print, func_name, ": File ", file, " already exists and keyword /clobber not passed!  Exiting without further ado..."
     RETURN, 0
  endif
  
  nflat = n_elements(traceflat)
  if nflat EQ 1 then begin
     fire_proc, traceflat, trcimg, hdr=hdr
  endif else begin
     FIRE_COMBINE_FLATS, traceflat, IMAGE=trcimg, hdr=hdr
  endelse
  
  medimg = djs_median(trcimg, width=5)
  trcimg = medimg
  
  tset_slits = fire_traceorders(trcimg, /crude, ORDR_STR_FILE=ordr_str_file, ksize=13, CHK=CHK, INTER=INTER)
  if size(tset_slits, /type) NE 8 then begin
     fire_siren, func_name + ": Invalid out for fire_traceorders!  Exiting without completing task..."
     return, 0
  endif
  
  ordermask=fire_ordermask(tset_slits, /fsr) 
  ;;slitmask = long_slits2mask(tset_slits)
  ;;ordermask = 0 * slitmask
  ;;ordermask[WHERE(slitmask GT 0)] = -slitmask[WHERE(slitmask GT 0)] + 21L

  mwrfits, ordermask, file, hdr, /create
  mwrfits, tset_slits, file
  ;; Add flat information to header
  flat_str = names_to_list(traceflat)
  nflats = n_elements(traceflat)
  fxhmodify, file, "FLATS", flat_str
  fxhmodify, file, "NFLATS", nflats
  
  print, func_name, ": All done!"
  
  return, tset_slits
  
end
