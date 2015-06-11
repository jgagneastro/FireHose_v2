;+
; NAME:
;     pscolor
;
; PURPOSE:
;     Checks the plotting color for a postscript file.
;
; CATEGORY:
;     Plotting and Image Display
;
; CALLING SEQUENCE:
;     result = pscolor([value],BACKGROUND=background,CANCEL=cancel)
;
; INPUTS:
;     None
;
; OPTIONAL INPUTS:
;     value - The color (from mkct.pro)
;
; KEYWORD PARAMETERS:
;     BACKGROUND - Set to return the background color
;     CANCEL     - Set on return if there is a problem
;
; OUTPUTS:
;     Returns the color value
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
;     Based on mkct.pro
;
; PROCEDURE:
;     Given the color, value, the program checks to see if the device
;     is set to 'X' or 'ps' and returns the correct color.  Basically,
;     this program is used make sure black and white look correct in
;     a postscript file.
;
; EXAMPLE:
;     from mkct.pro, 0-black, and 1-white.
;
;     if 'X' then pscolor(1)=1, pscolor(0)=0
;     if 'ps' then pscolor(1)=0, pscolor(0)=1
;
;     if 'X' then pscolor(/BACKGROUND)=0
;     if 'ps' then pscolor(/BACKGROUND)=1
;
; MODIFICATION HISTORY:
;     2002 - Written by M. Cushing, Institute for Astronomy, UH
;-
function pscolor,value,BACKGROUND=background,CANCEL=cancel

if n_params() eq 0 then begin

    if keyword_set(BACKGROUND) then val = (!d.name eq 'X') ? 0:1 else $
      val = (!d.name eq 'X') ? 1:0
    return, val

endif else begin
   
   if !d.name eq 'PS' then begin

      z1 = where(value eq 1,cnt1)
      z2 = where(value eq 0,cnt0)
      if cnt1 ne 0 then value[z1] = 0
      if cnt0 ne 0 then value[z0] = 1

   endif

   return, value

endelse

end


