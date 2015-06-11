;+
; NAME:
;     parangle
;
; PURPOSE:
;     To compute the parallactic angle at a given position on the sky.
;
; CATEGORY:
;     Spectroscopy    
;
; CALLING SEQUENCE:
;     parangle, HA, DEC, lat, eta, [za], CANCEL=cancel
;
; INPUTS:
;     HA  - Hour angle of the object, in decimal hours (0,24)
;     DEC - Declination of the object, in degrees
;     lat - The latitude of the observer, in degrees
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     eta - The parallactic angle
;
; OPTIONAL OUTPUTS:
;     za  - The zenith angle
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
;     Given an objects HA and DEC and the observers latitude, the
;     zenith angle and azimuth are computed.  The law of cosines
;     then gives the parallactic angle.  
;
; EXAMPLE:
;     NA
;
;
; MODIFICATION HISTORY:
;     2000-04-05 - written by M. Cushing, Institute for Astronomy,UH
;     2002-08-15 - cleaned up a bit.
;     2003-10-21 - changed to pro; outputs zenith angle as well - WDV
;
;-
pro parangle, HA, DEC, lat, eta, za, CANCEL=cancel

cancel = 0

if n_params() lt 3 then begin

    print, 'Syntax -  parangle,HA,DEC,lat,eta,za,CANCEL=cancel'
    cancel = 1
    return
    
endif

cancel = cpar('parangle',HA,1,'HA',[2,3,4,5],0)
if cancel then return
cancel = cpar('parangle',DEC,2,'DEC',[2,3,4,5],0)
if cancel then return
cancel = cpar('parangle',lat,3,'lat',[2,3,4,5],0)
if cancel then return

;  If HA equals zero then it is easy.
;  Check to see if HA is greater than 12.

if HA gt 12. then HA = 24.-HA

HA = HA*15.

;  Determine Zenith angle and Azimuth

cos_za = sin(lat*!dtor)*sin(DEC*!dtor) + $
         cos(lat*!dtor)*cos(DEC*!dtor)*cos(HA*!dtor)
za     = acos(cos_za)*!radeg
cos_az = (sin(DEC*!dtor) - sin(lat*!dtor)*cos(za*!dtor))/ $
         (cos(lat*!dtor)*sin(za*!dtor))
az     = acos(cos_az)*!radeg

if az eq 0. then if DEC lt lat then az = 180.

tan_eta = sin(HA*!dtor)*cos(lat*!dtor)/ $
          (cos(DEC*!dtor)*sin(lat*!dtor) - $
           sin(DEC*!dtor)*cos(lat*!dtor)*cos(HA*!dtor))
eta     = atan(tan_eta)*!radeg

if eta lt 0. then eta = eta + 180.
if eta eq 0. then if az eq 0. then eta = 180.
if za gt 90. then eta = !values.f_nan

HA = HA/15.0



end

