; + 
; NAME:
; fire_fitstd
; Version 0.1
;
; PURPOSE:
;  Determines a fit to a standard star and saves it so it can be 
;  used to flux other observations.  
;
; CALLING SEQUENCE:
;
;  mage_fitstd,fluxtable,objstr,outfil 
;
; INPUTS:
;   fluxtable - The file path of an ESO standard star file to
;               calculate the fit from
;   objstr    - The object structure generated from the mage_script
;               extraction routines
;
; RETURNS:
;
; OUTPUTS:
;
;   outfil    - The file path of an IDL save file that contains the
;               fit results
;
; OPTIONAL KEYWORDS:
;
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;
; EXAMPLES:
;   fire_fitstd,'fgd108.dat',obj_strct,'gd108cal.sav'
;
;
; PROCEDURES/FUNCTIONS CALLED:
;
;    mage_echfitstd
;
; REVISION HISTORY:
;   16-Jun-2008 CLW


pro fire_fittell,fluxtable,objstr,outfile

;Read in the calibration table
gnirs_telluric_std, type, loglam = loglam_std, flux = flux_std, V = V
;flux_std = 1.0d17*flux_std      ; fluxes are in units of 1.0e-17
swv = loglam_std
sfx = flux_std

;old version
;readcol,fluxtable,swv,sfx,format='D,D'



;Normalize the exposure time
for i=0,N_elements(objstr)-1 do objstr[i].flux/=objstr[i].exp
for i=0,N_elements(objstr)-1 do objstr[i].var/=((objstr[i].exp)^2)
for i=0,N_elements(objstr)-1 do objstr[i].sig/=objstr[i].exp

fire_echfittell,objstr,swv,sfx,outfile


end
