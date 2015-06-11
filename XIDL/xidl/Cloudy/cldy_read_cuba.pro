;+ 
; NAME:
; cldy_read_cuba
;   Version 1.1
;
; PURPOSE:
;  Read a CUBA file and pass back wave, Jnu
;
; CALLING SEQUENCE:
;
; INPUTS:
;   fil  - CUBA output file
;   z    - Redshift
;
; RETURNS:
; OUTPUTS:
;;  WAVE = wavelength
;  JNU  = radiation field
; OPTIONAL INPUTS:
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;
; EXAMPLES:
; file, '/u/xavier/Cloudy/Spec/Data/CUBA/Q1G0/bkgthick.out',
; 
;
;
; PROCEDURES/FUNCTIONS CALLED:
;
; REVISION HISTORY:;  26-May-2010 Written by JFH
;-
;------------------------------------------------------------------------------

PRO CLDY_READ_CUBA, file, z, WAVE = WAVE_OUT, JNU = JNU_OUT

if  N_params() LT 2  then begin 
   print, 'Syntax -  read_cuba, file, z, WAVE=WAVE,JNU=JNU'
   return
endif 

COMMON CUBA_SPEC, zval, wave, jnu, file_current, nwave, nblock, nz

;; This is redundant based on N_params()
;IF NOT KEYWORD_SET(FILE) OR NOT KEYWORD_SET(Z) THEN $
;   message, 'Problem with inputs'

IF NOT KEYWORD_SET(FILE_CURRENT) THEN FILE_CURRENT = ''

IF strmatch(file, file_current) EQ 0 THEN BEGIN
    file_current =  file
;   compute the number of total lines in the Madau file ignoring the 
;   comment lines
    readcol, file, junk, comment = '#', FORMAT = 'F'
    
;   Data should be in blocks of 432
    nwave = 432L
    nblock = n_elements(junk)/nwave
    nz = 10L
    
;; Open Madau file
    close, /all
    openr, 1, file
    zval = dblarr(nz*nblock)
    zval_temp = fltarr(nz)
    wave = dblarr(nwave)
    jnu = dblarr(nwave, nz*nblock)
    dumf = dblarr(nz+1)
    FOR ii = 0L, nblock-1L DO BEGIN
        readf, 1, zval_temp, FORMAT = '(11x,10f11.4)' 
        zinds = ii*nz + lindgen(nz)
        zval[zinds] = zval_temp
        FOR jj = 0L, nwave-1L DO BEGIN
            readf, 1, dumf          
            jnu[jj, zinds] = dumf[1:*]
            IF ii EQ 0 THEN wave[jj] = dumf[0]
        ENDFOR
    ENDFOR
close, 1
ENDIF

jnu_out = dblarr(nwave)
wave_out = wave

; Spline interpolate onto redshift of interest for each wavelength
FOR ii = 0, nwave-1L DO $
   jnu_out[ii] = interpol(jnu[ii, *], zval, z, /spline)


RETURN
END
