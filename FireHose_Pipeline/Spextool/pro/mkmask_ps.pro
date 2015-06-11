;+
; NAME:
;     mkmask_ps
;
; PURPOSE:
;     Constructs an aperture mask for point source spatial profile.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = mkmask_ps(slit,positions,apradius,BGSTART=bgstart,$
;                        BGWIDTH=bgwidth,WIDGET_ID=widget_id,CANCEL=cancel)
;
; INPUTS:
;     slit      - Array of slit values (e.g. pixels, arcseconds)
;     positions - Array of apertures positions in units of slit
;     apwidth   - Aperture radius in units of slit
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     BGSTART   - Radius at which to start the background in units of slit.
;     BGWIDTH   - Width of the background in units of slit.
;     WIDGET_ID - If given, an pop-up error message will appear over
;                 the widget
;     CANCEL    - Set on return if there is a problem.
;
; OUTPUTS:
;     Returns a 1D mask where:
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
;      First the backgrounds are set to -1 without regard to the
;      apertures.  Then from appos[i]-bgstart to appos[i]+bgstart is         
;      cleared out and set to 0.  Then the apertures are indexed to            
;      apnum.  Finally the edge pixels are set to reflect their
;      fractional pixel values.  To determine where aperture n is, 
;      z = where(mask gt n-1 and mask le n).  If BGWIDTH is not given, 
;      then the background regions are not defined.
;
; EXAMPLE:
;      result = mkmask_ps(findgen(16),[3,8],0.5)
;
; MODIFICATION HISTORY:
;      2000-08-06 - Written by M. Cushing, Institute for Astronomy, UH
;      2006-05-31 - Modified to include test for aperture overlap and
;                   also to clear out the bg region for all apertures
;                   BEFORE filling in the apertures.  Avoids clearing
;                   out already present apertures located nearby.
;      2006-06-09 - Added a WIDGET_ID keyword so the error message for
;                   overlaping apertures will appear over Spextool
;-
function mkmask_ps,slit,positions,apradius,BGSTART=bgstart,BGWIDTH=bgwidth,$
                   WIDGET_ID=widget_id,CANCEL=cancel

cancel = 0

;  Check parameters 

if n_params() lt 3 then begin
    
    print, 'Syntax - result = mkmask_ps(slit,positions,apradius,$'
    print, '                            BGSTART=bgstart,BGWIDTH=bgwidth,$'
    print, '                            WIDGET_ID=widget_id,CANCEL=cancel)'
    cancel = 1
    return, -1

endif

cancel = cpar('mkmask_ps',slit,1,'Slit',[2,3,4,5],1)
if cancel then return,-1
cancel = cpar('mkmask_ps',positions,2,'Positions',[2,3,4,5],[0,1])
if cancel then return,-1
cancel = cpar('mkmask_ps',apradius,3,'Apradius',[2,3,4,5],0)
if cancel then return,-1

npix   = n_elements(slit)
npeaks = n_elements(positions)
mask   = fltarr(npix)

;  Check to make sure the apertures don't overlap 

for i = 0, npeaks-1 do begin

   ap  = [(positions[i]-float(apradius)),(positions[i]+float(apradius))]
   tabinv, slit, ap, ap_idx
   mask[ap_idx[0]:ap_idx[1]] = mask[ap_idx[0]:ap_idx[1]] + 1

endfor
z = where(mask gt 1,cnt)
if cnt gt 0 then begin

   result = dialog_message([['The extraction apertures overlap.  Please '],$
                           ['lower the aperture radius.']],/ERROR,$
                          DIALOG_PARENT=widget_id)

   cancel = 1
   return, -1

endif

;  Reset and create mask

mask[*] = 0
;  Get transformation for the end pixels of the aperture.

arctopix    = fltarr(2)
arctopix[1] = 1./(slit[1]-slit[0])
arctopix[0] = -1.*arctopix[1]*slit[0]

;  First determine background regions around each peak.

if n_elements(BGWIDTH) ne 0 then begin

    for i = 0, npeaks-1 do begin
        
;       botbg = [(positions[i]-bgstart-bgwidth),(positions[i]-bgstart)] > $
;               slit[0]
;       topbg = [(positions[i]+bgstart),(positions[i]+bgstart+bgwidth)] < $
;               slit[npix-1]

       botbg = [(positions[i]-bgstart-bgwidth),(positions[i]-bgstart)]
       topbg = [(positions[i]+bgstart),(positions[i]+bgstart+bgwidth)]

        tabinv, slit, botbg, bot_idx
        tabinv, slit, topbg, top_idx

        mask[bot_idx[0]:bot_idx[1]] = -1
        mask[top_idx[0]:top_idx[1]] = -1
        
    endfor
    
endif    

for i =0, npeaks-1 do begin
        
;  Now clear out from -bgstart to bgstart around each peak.
    
   if n_elements(BGWIDTH) ne 0 then begin
      
      clr = [(positions[i]-bgstart),(positions[i]+bgstart)]
      tabinv, slit, clr, clr_idx
      mask[clr_idx[0]:clr_idx[1]] = 0
      
   endif
   
endfor

for i = 0,npeaks-1 do begin

;  Define aperture
   
   ap  = [(positions[i]-float(apradius)),(positions[i]+float(apradius))]
   tabinv, slit, ap, ap_idx
   mask[ap_idx[0]:ap_idx[1]] = i+1
   
;  Fix endpoints to reflect fractional pixels.
   
   if ap_idx[0]-floor(ap_idx[0]) ge 0.5 then begin
      
      mask[ap_idx[0]]   = 0
      mask[ap_idx[0]+1] = (0.5 + round(ap_idx[0])-ap_idx[0]) + i
      
   endif else begin
      
      mask[ap_idx[0]] = (0.5 - (ap_idx[0]-floor(ap_idx[0]) ) ) + i
      
   endelse
   if ap_idx[1]-floor(ap_idx[1]) ge 0.5 then begin
      
      mask[ap_idx[1]+1] = (0.5 - (round(ap_idx[1])-ap_idx[1]) ) + i
      
      
   endif else begin
      
      mask[ap_idx[1]] = ( 0.5 + (ap_idx[1]-round(ap_idx[1])) ) + i
      
   endelse
   
endfor

return, mask

end
