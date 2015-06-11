;+
; NAME:
;     mkmask_xs
;
; PURPOSE:
;     Constructs an aperture mask for extended source spatial profile.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = mkmask_xs(slit,positions,apradius,BG=bg,WIDGET_ID=widget_id,$
;                        CANCEL=cancel)
;
; INPUTS:
;     slit      - Array of slit values (e.g. pixels, arcseconds)
;     positions - Array of apertures positions in units of slit
;     apradius  - Aperture radius in units of slit
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     BG        - Array of starting and ending positions for the 
;                 background regions in units of slit, ( [[1-2],[13-15]] ).
;     WIDGET_ID - If given, an pop-up error message will appear over
;                 the widget
;     CANCEL    - Set on return if there is a problem
;
; OUTPUTS:
;     Returns a mask where:
;        0+ap < value < 1+ap - aperture
;        value = -1          - background
;        0                   - nothing
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
;      First the background regions are set to -1.  Then the apertures 
;      are indexed to apnum.  Finally the edge pixels are set to reflect their
;      fractional pixel values.  To determine where aperture n is, 
;      z = where(mask gt n-1 and mask le n).  If BG is not given, 
;      then the background regions are not defined.
;
; EXAMPLE:
;      
; MODIFICATION HISTORY:
;     2000-08-06 - Written by M. Cushing, Institute for Astronomy, UH
;     2000-11-13 - Modified program to accept multiple background
;                  positions.  Removed LEFTBG and RIGHTBG keywords and
;                  replaced with BG.
;     2006-05-31 - Modified to include test for aperture overlap and
;                  also to clear out the bg region for all apertures
;                  BEFORE filling in the apertures.  Avoids clearing
;                  out already present apertures located nearby.
;     2006-06-09 - Added a WIDGET_ID keyword so the error message for
;                  overlaping apertures will appear over Spextool
;-
function mkmask_xs,slit,positions,apradius,BG=bg,WIDGET_ID=widget_id, $
                   CANCEL=cancel

cancel = 0

if n_params() lt 3 then begin
    
    print, 'Syntax - result = mkmask_xs(slit,positions,apradius,$'
    print, '                            BG=bg,WIDGET_ID=widget_id,$'
    print, '                            CANCEL=cancel)'
    cancel = 1
    return, -1

endif
cancel = cpar('mkmask_xs',slit,1,'Slit',[2,3,4,5],1)
if cancel then return, -1
cancel = cpar('mkmask_xs',positions,2,'Positions',[2,3,4,5],[0,1])
if cancel then return, -1
cancel = cpar('mkmask_xs',apradius,3,'Apradius',[2,3,4,5],[0,1])
if cancel then return, -1

npix = n_elements(slit)
naps = n_elements(positions)
mask = fltarr(npix)

;  Check to make sure the apertures don't overlap 

for i = 0, naps-1 do begin

   ap  = [(positions[i]-float(apradius[i])),(positions[i]+float(apradius[i]))]
   tabinv, slit, ap, ap_idx
   mask[ap_idx[0]:ap_idx[1]] = mask[ap_idx[0]:ap_idx[1]] + 1

endfor
z = where(mask gt 1,cnt)
if cnt gt 0 then begin

   result = dialog_message([['The extraction apertures overlap.  Please '],$
                           ['lower the aperture radii.']],/ERROR,$
                           DIALOG_PARENT=widget_id)

   cancel = 1
   return, -1

endif

;  Reset and create mask

mask[*] = 0

;  Get transformation for the end pixels of the apertures.
  
arctopix    = fltarr(2)
arctopix[1] = 1./(slit[1]-slit[0])
arctopix[0] = -1.*arctopix[1]*slit[0]

;  First determine background regions.

if n_elements(BG) ne 0 then begin

    s = size(bg)
    nbg = (s[0] eq 1) ? 1:s[2]
    for i = 0, nbg-1 do begin
        
        tabinv, slit, reform(bg[*,i]), idx
        mask[idx[0]:idx[1]]   = -1
        
    endfor

endif

;  Now construct the apertures.

for i = 0, naps-1 do begin

    ap  = [(positions[i]-float(apradius[i])),$
           (positions[i]+float(apradius[i]))]
    tabinv, slit, ap, ap_idx
    mask[ap_idx[0]:ap_idx[1]] = i+1

;  Fix endpoints to reflect fractional pixels.

    if ap_idx[0]-floor(ap_idx[0]) ge 0.5 then begin

        mask[ap_idx[0]]   = 0
        mask[ap_idx[0]+1] = (0.5 + round(ap_idx[0])-ap_idx[0])+i

    endif else begin

        mask[ap_idx[0]] = (0.5 - (ap_idx[0]-floor(ap_idx[0]) ) ) + i

    endelse
    if ap_idx[1]-floor(ap_idx[1]) ge 0.5 then begin

        mask[ap_idx[1]+1] = (0.5 - (round(ap_idx[1])-ap_idx[1])) + i

    endif else begin

        mask[ap_idx[1]] = ( 0.5 + (ap_idx[1]-round(ap_idx[1])) ) + i
    
    endelse

endfor

return, mask

end

