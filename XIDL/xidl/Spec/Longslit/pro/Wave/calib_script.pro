
;; Read in the reference solution
refslit = 3
restore, '../../1112+6612/Red600/calib' $
         + strcompress(string(refslit), /rem) + '.sav'
restore, '../../1112+6612/Red600/wave-lred0022.sav'
calib_ref = calib_temp
arc_ref = arc1d
;; read in the current arc array
restore,'wave-lred0023.sav'
dims=size(arc1d,/dim)
ny = dims[0]
nyby2 = ny/2L
nslits = dims[1]
; read in the solution for the 600 line grism with the longslit
; this has the appropriate fit paramters, but not the right fit. 
IF NOT KEYWORD_SET(ARCHIVE) THEN BEGIN
;restore, getenv('LONGSLIT_DIR') + '/calib/linelists/lris_red_600_7500.sav'
;; Add central wavelenght and central dispersion tags, which will 
;; be needed later
;; now run x_identify for each slit. It is redundant to calibrate
;; multiple slits at the same mask x-position, so to save time you might
;; want to choose them at different x values. 
    slit = 6
    x_identify, arc1d[*, slit-1L], calib_temp $
                , mfitstr = calib_ref, mspec = arc_ref[*, refslit-1L]$
                , xsize = 1200, ysize = 600 $
                , linelist = getenv('XIDL_DIR')+ $
                '/Spec/Arcs/Lists/lris_red_600.lst'
    calib_temp = struct_addtags(calib_temp $
                                , create_struct('WAVE_CEN', 0.0D $
                                                , 'DISP_CEN', 0.0D))
    wave_vec = x_calcfit(dindgen(ny), fitstr = calib_temp)
    wave_cen = wave_vec[nyby2]
    disp_cen = wave_vec[nyby2]-wave_vec[nyby2-1L]
    calib_temp.WAVE_CEN = wave_cen
    calib_temp.DISP_CEN = DISP_CEN
;; Add central wavelength and central dispersion to
    save, calib_temp, file = 'calib' + strcompress(string(slit), /rem) + '.sav'
ENDIF
stop
;; After you have fit each arc, save them in a file named 
;; getenv('LONGSLIT_DIR') + '/calib/linelists/lris_blue_600.sav'
restore, 'calib1.sav'
calib_now=replicate(calib_temp,nslits)
FOR j = 0L, nslits-1L DO BEGIN
;; where calib is an array of 25 structures
   restore,file= 'calib' + strcompress(string(j+1),/rem) + '.sav'
   calib_now[j] = calib_temp
ENDFOR
restore, file = getenv('LONGSLIT_DIR') + $
         '/calib/linelists/lris_red_600-10000_d680_mswave_8051.sav'
archive_arc = [[archive_arc], [arc1d]]
calib = [calib, calib_now]
spawn, 'mv ' + getenv('LONGSLIT_DIR') + $
       '/calib/linelists/lris_red_600-10000_d680_mswave_8051.sav  ' $
       +  getenv('LONGSLIT_DIR') + $
       '/calib/linelists/lris_red_600-10000_d680_mswave_8051_old.sav'
save, archive_arc, calib $
      , file = getenv('LONGSLIT_DIR') + $
      '/calib/linelists/lris_red_600-10000_d680_mswave_8051.sav'
;; 

END
