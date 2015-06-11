;+
; NAME:
;     crange
;
; PURPOSE:
;     Checks if a value is in range.
;
; CATEGORY:
;     Mathematical
;
; CALLING SEQUENCE:
;     result = crange(value,range,name,KGT=kgt,KGE=kge,KLT=klt,KLE=kle,$
;                     ODD=odd,EVEN=even,WIDGET_ID=widget_id,CANCEL=cancel)
;
; INPUTS:
;     value - The value(s) to be checked
;     range - The range that 'value' is to be checked against
;     name  - The name of the value
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     KGT       - Greater than
;     KGE       - Greater than or equal to
;     KLT       - Less than
;     KLE       - Less than or equal to
;     ODD       - Value should be odd
;     EVEN      - Value should be even
;     WIDGET_ID - If ID of widget is given, an pop-up error message will
;                 appear over the widget
;     CANCEL    - Set on return if the value is out of range or if
;                 there is a problem
;    
; OUTPUTS:
;     The value(s) are returned.  Check CANCEL keyword for result
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
;     Obvious
;
; EXAMPLE:
;     result = crange(5,[3,8],'Number',/KGT,/KLT,CANCEL=cancel)
;
; MODIFICATION HISTORY:
;     2000-09-06 - written by M. Cushing, Institute for Astronomy, UH
;-
function crange,value,range,name,KGT=kgt,KGE=kge,KLT=klt,KLE=kle,$
                ODD=odd,EVEN=even,WIDGET_ID=widget_id,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 3 then begin
    
    print, 'Syntax -  result = crange(value,range,name,KGT=kgt,KGE=kge,$'
    print, '                          KLT=klt,KLE=kle,ODD=odd,EVEN=even,$'
    print, '                          WIDGET_ID=widget_id,CANCEL=cancel)'
    cancel = 1
    return, -1

endif
cancel = cpar('crange',value,1,'Values',[1,2,3,4,5],[0,1])
if cancel then return,-1
cancel = cpar('crange',range,2,'Range',[1,2,3,4,5],[0,1])
if cancel then return,-1
cancel = cpar('crange',name,3,'Name',7,0)
if cancel then return,-1 

num    = n_elements(range)
widget = (n_elements(widget_id) ne 0) ? 1:0

;  Set up error message.

mess_a = ' '
mess_b = ' '
if keyword_set(KGT) then begin

    mess_a = strcompress(range[0])+' < '

endif
if keyword_set(KGE) then begin

    mess_a = strcompress(range[0])+' <= '

endif
if keyword_set(KLT) then begin
    
    if num eq 2 then test = range[1] else test = range
    mess_b = ' <'+strcompress(test)

endif
if keyword_set(KLE) then begin
    
    if num eq 2 then test = range[1] else test = range
    mess_b = ' <='+strcompress(test)

endif
mess = name+' out of range.'
mess = [[[mess]],[[mess_a+name+mess_b]]]

;  Now test the value.

nvalues = n_elements(value)

for i = 0, nvalues-1 do begin

    if keyword_set(KLT) then begin

        if num eq 2 then test = range[1] else test = range
        if value[i] ge test then begin
            
            cancel = 1
            if widget then ok = dialog_message(mess,/ERROR,$
                                               DIALOG_PARENT=widget_id)
            if not widget then print, mess
            return, -1
        
        endif
        
    endif
    if keyword_set(KLE) then begin
        
        if num eq 2 then test = range[1] else test = range
        if value[i] gt test then begin
            
            cancel = 1
            if widget then ok = dialog_message(mess,/ERROR,$
                                               DIALOG_PARENT=widget_id)
            if not widget then print, mess
            return, -1
            
        endif
        
    endif
    if keyword_set(KGT) then begin
        
        if num eq 2 then test = range[0] else test = range
        if value[i] le test then begin
            
            cancel = 1
            if widget then ok = dialog_message(mess,/ERROR,$
                                               DIALOG_PARENT=widget_id)
            if not widget then print, mess
            return, -1
            
        endif
        
    endif
    if keyword_set(KGE) then begin
        
        if num eq 2 then test = range[0] else test = range
        if value[i] lt test then begin
            
            cancel = 1
            if widget then ok = dialog_message(mess,/ERROR,$
                                               DIALOG_PARENT=widget_id)
            if not widget then print, mess
            return, -1
            
        endif
        
    endif
    if keyword_set(ODD) then begin

        if value[i] mod 2 ne 1 then begin

            mess = name+' out of must be odd.'
            cancel = 1
            if widget then ok = dialog_message(mess,/ERROR,$
                                               DIALOG_PARENT=widget_id)
            if not widget then print, mess
            return, -1            



        endif

    endif
    if keyword_set(EVEN) then begin

        if value[i] mod 2 ne 0 then begin

            mess = name+' must be even.'
            cancel = 1
            if widget then ok = dialog_message(mess,/ERROR,$
                                               DIALOG_PARENT=widget_id)
            if not widget then print, mess
            return, -1            



        endif
        
    endif

endfor
return, value
    
end

