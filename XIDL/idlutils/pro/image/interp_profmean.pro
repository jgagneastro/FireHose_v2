;+
; NAME:
;   interp_profmean
;
; PURPOSE:
;   Interpolates a radial profile of the sort output by photo
;
; CALLING SEQUENCE:
;   interp_profmean,nprof,profmean,radius,maggies, [maggieserr=, profradius= $
;      proferr=, radiusscale=, maggiesscale=]
;
; INPUTS:
;   nprof - number of measured elements in the profile 
;   profmean - values (in maggies) in the profile [15]
;   radius - a set of values to interpolate to [N]
;   maggies - calculated maggies
;
; OPTIONAL INPUTS:
;   proferr - errors in profile
;   profradius - boundaries of annuli in profile (set to photo default
;                in arcsec)
;   radiusscale - asinh scale for radii
;   maggiesscale - asinh scale for maggieses
;
; OUTPUTS:
;   maggieserr - calculated error
;
; OPTIONAL INPUT/OUTPUTS:
;
; COMMENTS:
;   Set up for using the profMean in the fpObjc files of the SDSS,
;   input and output in maggies (or any linear measure of surface 
;   brightness)
;
; EXAMPLES:
;
; BUGS:
;   Slow.
;
; PROCEDURES CALLED:
;
; REVISION HISTORY:
;   16-Jan-2002  Written by Mike Blanton, NYU
;-
;------------------------------------------------------------------------------
pro interp_profmean, nprof, profmean, radius, counts, rscale=rscale, $
                     profradius=profradius

if(n_params() lt 4) then begin
    print,'Syntax - interp_profmean, nprof, profmean, radius, counts [, rscale= ]'
    return
endif

if(NOT keyword_set(rscale)) then rscale=1.e-11
if(NOT keyword_set(pmscale)) then pmscale=0.1
if(NOT keyword_set(profradius)) then $
  profradius=[0.564190, 1.692569, 2.585442, 4.406462, $
              7.506054, 11.576202, 18.584032, 28.551561, $
              45.503910, 70.510155, 110.530769, 172.493530, $
              269.519104, 420.510529, 652.500061]
nrad=n_elements(profradius)

if(nprof eq 0) then begin
    counts=0.
    return
endif

profcum= fltarr(n_elements(profradius))
profcum[0]= profmean[0]*!DPI*profradius[0]^2
profcum[1L:n_elements(profradius)-1L]= $
  total(profmean[1L:n_elements(profradius)-1L]* $
        !DPI*(profradius[1L:n_elements(profradius)-1L]^2- $
              profradius[0L:n_elements(profradius)-2L]^2), /cum)

aprofradius= asinh(profradius)
aprofcum= asinh(profcum)
aradius= asinh(radius)

acounts= interpol(aprofcum, aprofradius, aradius, /spline)

counts= sinh(acounts)

end
