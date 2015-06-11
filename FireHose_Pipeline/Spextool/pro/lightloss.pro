;+
; NAME:
;     lightloss
;
; PURPOSE:
;     To correct a (post telluric correction) SpeX spectrum for slit losses
;
; CATEGORY:
;     Spectroscopy     
;
; CALLING SEQUENCE:
;     lightloss, obj, std, wguide, seeing, out, CANCEL=cancel
;
; INPUTS:
;     obj    - FITS file of the object spectrum to be corrected
;     std    - FITS file of the std spectrum used for telluric correction
;     wguide - wavelength at which guiding was done
;     seeing - seeing FWHM at the guiding wavelength
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
; 
; OUTPUTS:
;     Writes a Spextool FITS file ('out') to disk
;
; OPTIONAL OUTPUTS:
;     None
; 
; COMMON BLOCKS:
;     None
;
; SIDE EFFECTS:
;     None
; 
; RESTRICTIONS:
;     None
;
; PROCEDURE:
;     Reads a Spextool FITS file.
; 
; EXAMPLE:
;     None
;
; MODIFICATION HISTORY:
;     2003-10-21 - Written by W D Vacca 
;-
;
pro lightloss, objfile, stdfile, wguide, seeing, outfile,CANCEL=cancel

cancel = 0

if n_params() ne 5 then begin

    print, 'Syntax - lightloss,objfile,stdfile,wguide,seeing,outfile,$'
    print, '                   CANCEL=cancel'
    cancel = 1
    return


endif

cancel = cpar('lightloss',objfile,1,'Objfile',7,0)
if cancel then return
cancel = cpar('lightloss',stdfile,2,'Sbjfile',7,0)
if cancel then return
cancel = cpar('lightloss',wguide,3,'Wguide',[2,3,4,5],0)
if cancel then return
cancel = cpar('lightloss',seeing,4,'Seeing',[2,3,4,5],0)
if cancel then return
cancel = cpar('lightloss',outfile,5,'Outfile',7,0)
if cancel then return

; --- Open input files

obj    = readfits(objfile,objhdr)
wobj   = obj[*,0]
fobj   = obj[*,1]
eobj   = obj[*,2]

out    = obj
outhdr = objhdr

std    = readfits(stdfile,stdhdr)

; --- Read header keywords

tele       = fxpar(objhdr,'TELESCOP')
slitwd     = fxpar(objhdr,'SLTW_ARC')
slitht     = fxpar(objhdr,'SLTH_ARC')
xaxis      = fxpar(objhdr,'XAXIS')
xunits     = fxpar(objhdr,'XUNITS')
yaxis      = fxpar(objhdr,'YAXIS')
yunits     = fxpar(objhdr,'YUNITS')

posang_obj = fxpar(objhdr,'POSANGLE')
HA_objstr  = fxpar(objhdr,'HA')
DEC_objstr = fxpar(objhdr,'DEC')

posang_std = fxpar(stdhdr,'POSANGLE')
HA_stdstr  = fxpar(stdhdr,'HA')
DEC_stdstr = fxpar(stdhdr,'DEC')

; --- Process keywords

coord_str  = HA_objstr + ' ' + DEC_objstr
get_coords, coords, InString=coord_str
HA_obj     = coords[0]		
DEC_obj    = coords[1]

coord_str  = HA_stdstr + ' ' + DEC_stdstr
get_coords, coords, InString=coord_str
HA_std     = coords[0]		
DEC_std    = coords[1]

if posang_obj lt 0.0   then posang_obj = posang_obj + 180.0
if posang_obj ge 180.0 then posang_obj = posang_obj - 180.0

if posang_std lt 0.0   then posang_std = posang_std + 180.0
if posang_std ge 180.0 then posang_std = posang_std - 180.0

if tele eq 'NASA IRTF' then begin
   obsdeg  = 19.0
   obsmin  = 49.0
   obssec  = 34.39
   obsalt  = 4.16807    ; observatory altitude in km
   teldiam = 3.0        ; diameter of primary in meters
   press   = 615.0 	; mm Hg typical value
   water   = 2.0        ; mm Hg typical value
   temp    = 0.0        ; deg C typical value
endif else begin 
   print, 'Unknown Telescope - stopping!'
   return
endelse

obslat     = ten(obsdeg,obsmin,obssec)

; --- Compute Parallactic Angle

parangle, HA_obj, DEC_obj, obslat, pa_obj, za_obj
parangle, HA_std, DEC_std, obslat, pa_std, za_std
dtheta_obj = posang_obj - pa_obj
dtheta_std = posang_std - pa_std

print, posang_obj, pa_obj, dtheta_obj, HA_obj
print, posang_std, pa_std, dtheta_std, HA_std

; --- Compute Differential Atmospheric Dispersion

disp_obj = atmosdisp(wobj,wguide,za_obj,press,temp,water,obsalt)
disp_std = atmosdisp(wobj,wguide,za_std,press,temp,water,obsalt)

; --- Compute FWHM at each input wavelength
 
diff     = 2.0*1.22d-6*3600.0*!radeg*(wobj/teldiam)	; arcsec
fwhm     = total(seeing*(wobj/wguide)^(-0.2))
z        = where(fwhm lt diff, count)
if count ne 0 then fwhm[z] = diff[z]

; --- Compute Relative Fraction of Light contained within the slit

dx_obj   = total(disp_obj*sind(dtheta_obj))
dy_obj   = total(disp_obj*cosd(dtheta_obj))

dx_std   = total(disp_std*sind(dtheta_std))
dy_std   = total(disp_std*cosd(dtheta_std))

fracobj  = slittrans(slitwd,slitht,fwhm,dx_obj,dy_obj)/ $
           slittrans(slitwd,slitht,seeing,0.0,0.0)
fracstd  = slittrans(slitwd,slitht,fwhm,dx_std,dy_std)/ $
           slittrans(slitwd,slitht,seeing,0.0,0.0)

;window, /Free
;plot, wobj, fracobj 

;sigma   = fwhm / (2.0 * sqrt(2.0*alog(2.0)))
;uplimx  = -(dx_obj - slitwd/2.0)/sigma
;lolimx  = -(dx_obj + slitwd/2.0)/sigma
;uplimy  = -(dy_obj - slitht/2.0)/sigma
;lolimy  = -(dy_obj + slitht/2.0)/sigma
;fracobj2 = (gaussint(uplimx) - gaussint(lolimx))*(gaussint(uplimy) - $
;                                                  gaussint(lolimy))
;
;uplimx  = -(dx_std - slitwd/2.0)/sigma
;lolimx  = -(dx_std + slitwd/2.0)/sigma
;uplimy  = -(dy_std - slitht/2.0)/sigma
;lolimy  = -(dy_std + slitht/2.0)/sigma
;fracstd2 = (gaussint(uplimx) - gaussint(lolimx))*(gaussint(uplimy) - $
;                                                  gaussint(lolimy))

; --- Compute Correction

slitcorr = fracstd / fracobj

;window,/Free
;plot, wobj, slitcorr, /xsty, /ysty

; --- Correct Input File

;window, 2 
;plot, wobj, fobj, /xsty, /ysty, psym=10, xtitle=xaxis+' ('+xunits+')', $
;      ytitle=yaxis+' ('+yunits+')'

fobj = temporary(fobj) * slitcorr
eobj = temporary(eobj) * slitcorr

;oplot, wobj, fobj, color=2

; --- Write Output File

out[*,0] = wobj
out[*,1] = fobj
out[*,2] = eobj

commnt   = 'Corrected for slit losses relative to the standard star in '+$
           stdfile

fxaddpar,outhdr,'HISTORY',commnt

writefits, outfile+'.fits', out, outhdr

end
