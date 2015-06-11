;+
; NAME:
;     setfocus
;
; PURPOSE:
;     Sets the cursor focus in the coyote_field2 compound widget.
;
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     setfocus,field_id,CANCEL=cancel
;
; INPUTS:
;     field_id - 2-element array giving the widget IDs of the
;                coyote_field and text window
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     None
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
;     Sets the focus to a compound field widget created with
;     COYOTE_FIELD2 and places the cursor at the end of the text.  
;     
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2000-08-23 - Written by M. Cushing, Institute for Astronomy, UH
;-
pro setfocus,field_id,CANCEL=cancel

cancel = 0

if n_params() lt 1 then begin
    
    cancel = 1
    print, 'Syntax - setfocus,field_id,CANCEL=cancel'
    return

endif
cancel = cpar('setfocus',field_id,1,'Field_id',[2,3,4,5],1)
if cancel then return

result = cfld(field_id,7,CANCEL=cancel)
if cancel then return
length = strlen(result)
widget_control, field_id[1], /INPUT_FOCUS
widget_control, field_id[1],SET_TEXT_SELECT=length

end
