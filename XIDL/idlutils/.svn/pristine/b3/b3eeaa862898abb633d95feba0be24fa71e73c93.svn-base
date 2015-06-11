; set some params for the pipeline
function psf_par
; pre-EFS: 12, 9, 1
; working-EFS: 15,12,1
; overpredicts IPP at 21,18,2
  par = {boxrad:  18, $  ; box radius of PSF cutout (2*boxrad+1, 2*boxrad+1)
         fitrad:  15, $   ; radius of region used in PSF fit
         cenrad:  2}   ; region used to center PSF
         
; nsigma ?
; nfaint?
; use ivar in psf_polyfit
; check condition of matrix in psf_polyfit


  return, par
end


