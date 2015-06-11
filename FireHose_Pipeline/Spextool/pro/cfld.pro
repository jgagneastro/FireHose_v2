;+
; NAME:
;     cfld (check field)
;
; PURPOSE:
;     Extracts a value from a widget field created by coyote_field2.pro.
;
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     result = xfld(field_id,type,EMPTY=empty,CANCEL=cancel)
;
; INPUTS:
;     field_id - Widget IDs [id1,id2] of the coyote_field2 and text window. 
;                id1 is the widget id of the entire compound widget
;                returned by coyote_field2 while id2 is the widget id
;                of the text field returned by the keyword TEXTID
;     type     - The extracted value is converted to this type
;
;                1 - Byte
;                2 - Integer
;                3 - Longword integer
;                4 - Floating point
;                5 - Double-precision floating
;                7 - String
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     EMPTY  - If set, cfld will return and error if the field is empty.
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     The field value if the type conversion was good.  If not, -1 is
;     return.
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
;     The field must have the uvalue keyword filled since the error 
;     message uses this as the name of the field.
;
; PROCEDURE:
;     Easy
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     Written 2000-01-23 by M. Cushing, Institute for Astronomy, UH
;-
function cfld,field_id,type,EMPTY=empty,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 2 then begin
    
    print, 'Syntax -  result = cfld(field_id,type,EMPTY=empty,CANCEL=cancel)'
    cancel = 1
    return, -1

endif
cancel = cpar('cfld',field_id,1,'Field_id',[2,3,4,5],1)
if cancel then return,-1
cancel = cpar('cfld',type,2,'Type',[2,3,4,5],0)
if cancel then return,-1

widget_control, field_id[1], GET_VALUE=val
widget_control, field_id[0], GET_UVALUE=uval

if strcompress(val[0],/REMOVE_ALL) eq '' and keyword_set(empty) then begin
       
    cancel = 1
    ok = dialog_message(uval+' field empty.',/ERROR,DIALOG_PARENT=field_id[0])
    setfocus,field_id
    return, -1
    
endif 

;  Now convert field based on type.

on_ioerror, getout
   
   case type of 
    
       7:  value = val[0]
       1:  value = byte(val[0])
       2:  value = fix(val[0])
       3:  value = long(val[0])
       4:  value = float(val[0])
       5:  value = double(val[0])
    
   end
   goto, cont1
   getout:

      cancel = 1
      mess = 'Type conversion error of '+uval+' field.'
      ok = dialog_message(mess,/ERROR,DIALOG_PARENT=field_id[0])
      setfocus,field_id
      return, -1

cont1: on_ioerror, null

return, value

end





