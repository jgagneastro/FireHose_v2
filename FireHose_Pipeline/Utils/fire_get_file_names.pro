FUNCTION FIRE_GET_FILE_NAMES, flats, ILLUMFLAT=illumflat, PIXFLATFILE=pixflatfile, ORDERFILE=orderfile, ORDR_STR_FILE=ordr_str_file, PIXIMGFILE=piximgfile, ARCFITS=arcfits, TARCFITS=tarcfits,  ARCIMGFITS=arcimgfits, TARCIMGFITS=tarcimgfits, ECHEXTFILE=echextfile, TECHEXTFILE=techextfile, OBJSTRFILE=objstrfile, TOBJSTRFILE=tobjstrfile, ARC1DGZ=arc1dgz, ARC2D=arc2d, ARC_SOL=arc_sol
	;; get file range
	nums = FIRE_GET_FITNUM( flats )
	min_num = MIN(nums, MAX=max_num)
	suffix = '.fits' ;; default value for suffix
  ;stop;
	if keyword_set( PIXFLATFILE ) then begin
  		;IF NOT FILE_TEST('Flat', /DIR) THEN FILE_MKDIR, 'Flat'
		prefix = 'Flat'+path_sep()+'Pixflat_'
	endif else if keyword_set( ORDERFILE ) then begin
                ;IF NOT FILE_TEST('Flat', /DIR) THEN FILE_MKDIR, 'Flat'
		prefix = 'Flat'+path_sep()+'Orders_'
	endif else if keyword_set( ILLUMFLAT ) then begin
  		;IF NOT FILE_TEST('Flat', /DIR) THEN FILE_MKDIR, 'Flat'
		;; get slit
	    	hdr=headfits(flats[0])
	    	slit= double((strsplit(sxpar(hdr, 'SLIT'),'_', /extract))[0])
		slit= string(slit, FORMAT='(F4.2)')
		prefix = 'Flat'+path_sep()+'Illumflat_' + slit + '_'
	endif else if keyword_set( PIXIMGFILE ) then begin
		;IF NOT FILE_TEST('Flat', /DIR) THEN FILE_MKDIR, 'Flat'
		prefix = 'Flat'+path_sep()+'piximg_flats_'
	endif else if keyword_set(ARCFITS) OR keyword_set(TARCFITS) then begin
		;IF NOT FILE_TEST('Arcs', /DIR) THEN FILE_MKDIR, 'Arcs'
		prefix = 'Arcs'+path_sep()+'Arc';J. Gagne !@!@!@!
	endif else if keyword_set(ARCIMGFITS) OR keyword_set(TARCIMGFITS) then begin
		;IF NOT FILE_TEST('Arcs', /DIR) THEN FILE_MKDIR, 'Arcs'
		prefix = 'Arcs'+path_sep()+'ArcImg'
		suffix = '.fits.gz'
	endif else if keyword_set(ECHEXTFILE) OR keyword_set(TECHEXTFILE) then begin
		;IF NOT FILE_TEST('Final', /DIR) THEN FILE_MKDIR, 'Final'
		prefix = 'Final'+path_sep()+'f'
	endif else if keyword_set(OBJSTRFILE) OR keyword_set(TOBJSTRFILE) then begin
		;IF NOT FILE_TEST('Object', /DIR) THEN FILE_MKDIR, 'Object'
		prefix = 'Object'+path_sep()+'Obj_'
	endif else if keyword_set( ORDR_STR_FILE ) then begin
		;IF NOT FILE_TEST('Flats', /DIR) THEN FILE_MKDIR, 'Flat'
		prefix = 'Flat'+path_sep()+'OStr_fire_'
	endif else if keyword_set( ARC1DGZ ) then begin
		;IF NOT FILE_TEST('Arcs', /DIR) THEN FILE_MKDIR, 'Arcs'
		prefix = 'Arcs'+path_sep()+'Arc1d_qa'
		suffix = '.fits.ps.gz'
	endif else if keyword_set( ARC2D ) then begin
		;IF NOT FILE_TEST('Arcs', /DIR) THEN FILE_MKDIR, 'Arcs'
		prefix = 'Arcs'+path_sep()+'Arc2d'
	endif else if keyword_set( ARC_SOL ) then begin
		;IF NOT FILE_TEST('Arcs', /DIR) THEN FILE_MKDIR, 'Arcs'
		prefix = 'Arcs'+path_sep()+'Arc_sol_'
	endif
  ;stop
  ;if (strsplit(prefix,path_sep(),/extract))[0] ne 'redux' then $
  ;  prefix = 'redux'+path_sep()+prefix
  
	if max_num ne min_num then begin
		file = prefix + strtrim(string(min_num),2) + 'to' + strtrim(string(max_num),2) + suffix
	endif else begin
		file = prefix + strtrim(string(min_num),2) + suffix
	endelse

	RETURN, file
END



