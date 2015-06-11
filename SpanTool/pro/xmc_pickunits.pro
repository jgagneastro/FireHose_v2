;+
; NAME:
;     xmc_pickunits
;
; PURPOSE:
;     To pick the units of a spectrum
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     xmc_pickunits,xunits,yunits,GROUP_LEADER=group_leader,CANCEL=cancel
;
; INPUTS:
;     None
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     GROUP_LEADER - The group leader if called from another widget.
;     CANCEL       - Set on return if there is a problem.
;
; OUTPUTS:
;     xunits - String giving the wavelength units
;              um:  microns
;              A :  Angstroms
;              nm:  Nanometers
;     yunits - String giving the flux density units
;              ergs
;
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     None
;
; SIDE EFFECTS:
;     Blocking widget
;
; RESTRICTIONS:
;     None
;
; PROCEDURE:
;     Queries the user for the units of the spectrum.
;
; EXAMPLE:
;     None
;
; MODIFICATION HISTORY:
;     2004-08-13 - Written M. Cushing, NASA Ames Research Center
;-
;
;*****************************************************************************
;
;-----------------------------Support Procedures------------------------------
;
;*****************************************************************************
;
pro xmc_pickunits_init

common xmc_pickunits_state,state

state = {cancel:0,$
         xunits:'um',$
         yunits:'Wm-2um-1'}


end
;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro xmc_pickunits_event,event

common xmc_pickunits_state
widget_control, event.id,  GET_UVALUE = uvalue

case uvalue of

    'Accept': widget_control, event.top, /DESTROY

    'Cancel': begin

        state.cancel = 1
        widget_control, event.top, /DESTROY

    end
    
    'Flux Density': begin

        case event.index of 

            0: state.yunits = 'Wm-2um-1'

            1: state.yunits = 'ergss-1cm-2A-1'

            2: state.yunits = 'ergss-1cm-2um-1'
            
            3: state.y.units = 'ergss-1cm-2Hz-1'


        endcase

    end

    'Wavelength': begin

        case event.index of 

            0: state.xunits = 'um'

            1: state.xunits = 'A'
            
            2: state.xunits = 'nm'

        endcase

    end




    
endcase


end
;
;******************************************************************************
;
;--------------------------------Main Program---------------------------------
;
;******************************************************************************
;
pro xmc_pickunits,group_leader,xunits,yunits,NOTE=note,CANCEL=cancel

common xmc_pickunits_state,state
xmc_pickunits_init

getfonts,buttonfont,textfont

widget_control, group_leader, SENSITIVE=0

xmc_pickunits_base = widget_base(GROUP_LEADER=group_leader,$
                             /COLUMN,$
                             /FLOATING,$
                             TITLE='xmc_pickunits')

   if keyword_set(NOTE) then begin

       label = widget_text(xmc_pickunits_base,$
                           YSIZE=2,$
                           VALUE=[['Cannot read FITS header units.'],$
                         ['Please select wavelength and flux density units.']])

   endif

   dl = widget_droplist(xmc_pickunits_base,$
                        FONT=buttonfont,$
                        TITLE='Wavelength:',$
                        VALUE=['um','A','nm'],$
                        UVALUE='Wavelength')

   dl = widget_droplist(xmc_pickunits_base,$
                        FONT=buttonfont,$
                        TITLE='Flux Density:',$
                        VALUE=['W m-2 um-1','ergs s-1 cm-2 A-1',$
                               'ergs s-1 cm-2 um-1','ergs s-1 cm-2 Hz-1'],$
                        UVALUE='Flux Density')

   row_base = widget_base(xmc_pickunits_base,$
                          /ROW,$
                          /BASE_ALIGN_CENTER)

      
      button = widget_button(row_base,$
                             VALUE='Cancel',$
                             UVALUE='Cancel',$
                             FONT=buttonfont)

      button = widget_button(row_base,$
                             VALUE='Accept',$
                             UVALUE='Accept',$
                             FONT=buttonfont)

  widget_control, xmc_pickunits_base, /REALIZE
   
; Start the Event Loop. This will be a blocking program.

XManager, 'xmc_pickunits', $
  xmc_pickunits_base

if state.cancel then begin

    xunits=''
    yunits=''

endif else begin

    xunits = state.xunits
    yunits = state.yunits

endelse

cancel = state.cancel    
widget_control, group_leader, SENSITIVE=1


end
