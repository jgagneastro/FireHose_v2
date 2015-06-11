;+
; NAME:
;   bitmask_widget
; PURPOSE:
;   Set a bitmask according to a set of radial buttons from user
; CALLING SEQUENCE:
;   bits= bitmask_widget(names [, default=])
; INPUTS;
;   names - [N] names for bits (1<=N<=32)
; OPTIONAL INPUTS:
;   default - default bitmask to bring up for user
; OUTPUTS:
;   bits - 32-bit integer with bit settings from user
; REVISION HISTORY:
;   31-Mar-2009  MRB, NYU
;-
;------------------------------------------------------------------------------
pro bitmask_event, event

common com_bitmask_widget, bitarray, w_done, w_list

if (event.id eq w_done) then begin
    widget_control, event.top, /destroy  
    return
endif

if (event.id eq w_list) then begin
    bitarray[event.value]= event.select
    return
endif

end
;
function bitmask_widget, names, default=default

common com_bitmask_widget

if(NOT keyword_set(default)) then default=0L
bits=default

;; get number of bits
nnames=n_elements(names)
if(nnames eq 0) then $
  message, 'Must input some bit names'
if(nnames gt 32) then $
  message, 'bitmask_widget only works up to 32 bits'

;; set up base widget
w_base = widget_base(/column, /base_align_top, /scroll, scr_xsize=200, $
                     scr_ysize=100L+30L*nnames)
w_label = widget_label(w_base, value='Bitmask values')
w_done = widget_button(w_base, value='Done')  

;; set up bit array with initial values
bitarray=bytarr(nnames)
for i=0L, nnames-1L do begin
    bit=2L^i
    if((bits AND bit) gt 0) then $
      bitarray[i]=1
endfor

;; create list with bits
w_list = cw_bgroup(w_base, names, column=1, /nonexclusive, $
                   set_value=bitarray)


;; realize the whole thing
widget_control, w_base, /realize 

;; now manage events (using bitmask_event above)
xmanager, 'bitmask', w_base

;; bit array was changed, remake bitmask
bits=0L
for i=0L, nnames-1L do begin
    bit=2L^i
    if(bitarray[i] gt 0) then $
      bits= bits OR bit
endfor

return, bits

end  

