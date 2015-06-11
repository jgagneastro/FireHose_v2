;+
; NAME:
;     xmc_pickorder
;
; PURPOSE:
;     To choose an order and aperture for a Spextool FITS file
;    
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     xmc_pickorder,orders,naps,group_leader,porder,pap,CANCEL=canel
;    
; INPUTS:
;     orders       - A array of order numbers
;     naps         - The number of apertures in the FITS file
;    
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     GROUP_LEADER - The group leader if called from another widget.  
;     CANCEL       - Set on return if there is a problem.
;     
; OUTPUTS:
;     porder - The order chosen by the user.
;     pap    - The aperture chosen by the user.
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
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2002 - Written by M. Cushing, Institute for Astronomy, UH
;-
;
;*****************************************************************************
;
;-----------------------------Support Procedures------------------------------
;
;*****************************************************************************
;
pro xmc_pickorder_init,orders,naps

common xmc_pickorder_state,state

state = {pickorder_base:0L,$
         orders:orders,$
         naps:naps,$
         porder:orders[0],$
         pspec:0,$
         pap:1}

end
;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro xmc_pickorder_event,event

common xmc_pickorder_state
widget_control, event.id,  GET_UVALUE = uvalue

case uvalue of

    'Pick Ap': state.pap =  fix(event.value)

    'Pick Order': state.porder =  fix(event.value)

    'Accept': widget_control, event.top, /DESTROY
    
endcase

end
;
;******************************************************************************
;
pro xmc_pickorder,orders,naps,porder,pap,GROUP_LEADER=group_leader,CANCEL=cancel

cancel = 0
if n_params() lt 2 then begin

    print, 'Syntax - xmc_pickorder,ders,naps,porder,pap,$'
    print, '                    GROUP_LEADER=group_leader,CANCEL=cancel'
    cancel = 1
    return

endif
cancel = cpar('xmc_pickorder',orders,1,'Orders',[2,3,4,5],[0,1])
if cancel then return
cancel = cpar('xmc_pickorder',naps,2,'Naps',[2,3,4,5],0)
if cancel then return

common xmc_pickorder_state

mc_getfonts,buttonfont

xmc_pickorder_init,orders,naps

state.pickorder_base = widget_base(GROUP_LEADER=group_leader, $
                                   /COLUMN,$
                                   /FLOATING,$
                                   TITLE='Pick Spectrum')

   aps_bg = cw_bgroup(state.pickorder_base,$
                      FONT=buttonfont,$
                      string(indgen(naps)+1,FORMAT='(i2.2)'),$
                      /EXCLUSIVE,$
                      /RETURN_NAME,$
                      SET_VALUE=0,$
                      /NO_RELEASE,$
                      LABEL_LEFT='Aperture:',$
                      /COLUMN,$
                      UVALUE='Pick Ap')

   orders_bg = cw_bgroup(state.pickorder_base,$
                         FONT=buttonfont,$
                         string(orders,FORMAT='(i2.2)'),$
                         /EXCLUSIVE,$
                         /RETURN_NAME,$
                         /NO_RELEASE,$
                         LABEL_LEFT='Order:',$
                         /ROW,$
                         SET_VALUE=0,$
                         UVALUE='Pick Order')
         
   button = widget_button(state.pickorder_base,$
                          VALUE='Accept',$
                          UVALUE='Accept',$
                          FONT=buttonfont)
   
widget_control, state.pickorder_base, /REALIZE
   
; Start the Event Loop. This will be a non-blocking program.

if n_elements(GROUP_LEADER) ne 0 then widget_control, group_leader, SENSITIVE=0

XManager, 'xmc_pickorder', $
  state.pickorder_base

if n_elements(GROUP_LEADER) ne 0 then widget_control, group_leader, SENSITIVE=1

porder = state.porder
pap    = state.pap



end
