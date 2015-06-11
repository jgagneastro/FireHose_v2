;+
; NAME:
;     xgetoffset
;
; PURPOSE:
;     Determines the pixel shift between two spectra interactively.
;    
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     xgetoffset,x1,y1,x2,y2,offset,ORDER=order,CANCEL=cancel
;
; INPUTS:
;     x1 - The indepedent values of the first spectrum
;     y1 - The dependent values of the first spectrum
;     x2 - The indepedent values of the second spectrum
;     y2 - The dependent values of the second spectrum
;    
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL   - Set on return if there is a problem
;     
; OUTPUTS:
;     offset - The offset of spectrum 2 from spectrum 1
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
;     Click on the same line in each spectrum
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     ? -          Written by M. Cushing, Institute for Astronomy, UH
;     2002-01-08 - Added ORDER keyword
;-

;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro xgetoffset_event, event

widget_control, event.top, GET_UVALUE=state, /NO_COPY
if event.top eq event.id then uvalue = 'Resize' else $ 
  widget_control, event.id,  GET_UVALUE=uvalue

widget_control, /HOURGLASS

case uvalue of

    'Accept': begin
        
        *state.r.ptr = state.r.pos2-state.r.pos1
        widget_control, event.top, /DESTROY
        goto, getout
        
    end

    'Plot Window 1': begin

        if event.press eq 1 then begin

            !x = state.p.xscale_win1
            !y = state.p.yscale_win1
            !p = state.p.pscale_win1
            
            xy = convert_coord(event.x/state.p.plotwinsize[0],$
                               event.y/state.p.plotwinsize[1],/NORMAL,/TO_DATA)
            state.r.guesspos1 = xy[0]
            
            wset, state.p.plotwin1_wid
            erase
            xgetoffset_plotspec,state,/SPEC1
            plots, [xy[0],xy[0]],!y.crange,COLOR=2
            xgetoffset_zoom,state,/SPEC1

        endif

    end

    'Plot Window 2': begin

        if event.press eq 1 then begin

            !x = state.p.xscale_win2
            !y = state.p.yscale_win2
            !p = state.p.pscale_win2

            xy = convert_coord(event.x/state.p.plotwinsize[0],$
                               event.y/state.p.plotwinsize[1],/NORMAL,/TO_DATA)
            state.r.guesspos2 = xy[0]

            wset, state.p.plotwin2_wid
            erase
            xgetoffset_plotspec,state,/SPEC2
            plots, [xy[0],xy[0]],!y.crange,COLOR=2
            xgetoffset_zoom,state,/SPEC2

        endif

    end

    'Resize':xgetoffset_resize,state

endcase

;  Put state variable into the user value of the top level base.
 
cont: 
widget_control, state.w.xgetoffset_base, SET_UVALUE=state, /NO_COPY
getout:

end
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xgetoffset_cleanup,xgetoffset_base

widget_control, xgetoffset_base, GET_UVALUE=state, /NO_COPY
if n_elements(state) ne 0 then state = 0B

end
;
;******************************************************************************
;
pro xgetoffset_plotspec,state,SPEC1=spec1,SPEC2=SPEC2

!p.multi=0
if keyword_set(SPEC1) then begin
    
    wset, state.p.plotwin1_wid
    plot, state.d.x1,state.d.y1,/XSTY,/YSTY,XTITLE='!5 Wavelength (Pixels)',$
      TITLE='!5Database Spectrum',PSYM=10
    
    state.p.xscale_win1 = !x
    state.p.yscale_win1 = !y
    state.p.pscale_win1 = !p
    
endif 
if keyword_set(SPEC2) then begin

    wset, state.p.plotwin2_wid
    plot, state.d.x2,state.d.y2,/XSTY,/YSTY,XTITLE='!5 Wavelength (Pixels)',$
      TITLE='!5Current Spectrum',PSYM=10
    
    state.p.xscale_win2 = !x
    state.p.yscale_win2 = !y
    state.p.pscale_win2 = !p
    
endif

end
;
;******************************************************************************
;
pro xgetoffset_zoom,state,SPEC1=spec1,SPEC2=spec2

expr = 'p[0] + p[1]*x + gaussian(x,p[2:4])'

if keyword_set(SPEC1) then begin

    tabinv,state.d.x1,state.r.guesspos1,idx
    
    botidx = 0 > (idx-15)
    topidx = (idx+15) < (n_elements(state.d.x1)-1)
    
    xwin = state.d.x1[botidx:topidx]
    ywin = state.d.y1[botidx:topidx]
            
    tabinv,xwin,state.r.guesspos1,idx
    start =float( [0.,0.,ywin[round(idx)],state.r.guesspos1,2] )
    fit = mpfitexpr(expr,xwin,ywin,dummy,start,/QUIET,/NOCOVAR)

;  Plot it up.

    yup = max(ywin,/NAN) + 0.10*max(ywin, /NAN)
    ydo = min(ywin, /NAN) - 0.20*abs(min(ywin, /NAN))
    wset, state.p.zoomwin1_wid
    plot,xwin,ywin,/XSTY,/YSTY,PSYM=10,YRANGE=[ydo,yup],XTICKS=4,$
      TITLE='!5Center = '+strcompress(fit[3],/RE)    

;    f = ( findgen(n_elements(xwin)*10.) )/10.  + min(xwin)
;    oplot,f,poly(f,fit[0:1])+gaussian(f,fit[2:4]),color=2,psym=10
    oplot,xwin,poly(xwin,fit[0:1])+gaussian(xwin,fit[2:4]),COLOR=3,PSYM=10    

    plots, [fit[3],fit[3]],!y.crange,COLOR=4
    state.r.pos1 = fit[3]

endif

if keyword_set(SPEC2) then begin

    tabinv,state.d.x2,state.r.guesspos2,idx
    
    botidx = 0 > (idx-15)
    topidx = (idx+15) < (n_elements(state.d.x2)-1)
    
    xwin = state.d.x2[botidx:topidx]
    ywin = state.d.y2[botidx:topidx]

    tabinv,xwin,state.r.guesspos2,idx
    start =float( [0.,0.,ywin[round(idx)],state.r.guesspos2,2] )
    fit = mpfitexpr(expr,xwin,ywin,dummy,start,/QUIET,/NOCOVAR)

;  Plot it up.
            
    yup = max(ywin,/NAN) + 0.10*max(ywin, /NAN)
    ydo = min(ywin, /NAN) - 0.20*abs(min(ywin, /NAN))
    wset, state.p.zoomwin2_wid
    plot,xwin,ywin,/XSTY,/YSTY,PSYM=10,YRANGE=[ydo,yup],XTICKS=4,$
      TITLE='!5Center = '+strcompress(fit[3],/RE)

;    f = ( findgen(n_elements(xwin)*10.) )/10.  + min(xwin)
;    oplot,f,poly(f,fit[0:1])+gaussian(f,fit[2:4]),color=2,psym=10
    oplot,xwin,poly(xwin,fit[0:1])+gaussian(xwin,fit[2:4]),COLOR=3,PSYM=10    

    plots, [fit[3],fit[3]],!y.crange,COLOR=4
    state.r.pos2 = fit[3]

endif

end
;
;******************************************************************************
;
pro xgetoffset,x1,y1,x2,y2,offset,ORDER=order,CANCEL=cancel

cancel = 0

cleanplot,/SILENT

;  Check parameters

if n_params() lt 4 then begin
    
    print, 'Syntax - xgetoffset,x1,y1,x2,y2,offset,ORDER=order,CANCEL=cancel'
    cancel = 1
    return

endif

cancel = cpar('xgetoffset',x1,1,'X1',[2,3,4,5],1)
if cancel then return
cancel = cpar('xgetoffset',y1,2,'Y1',[2,3,4,5],1)
if cancel then return
cancel = cpar('xgetoffset',x2,3,'X2',[2,3,4,5],1)
if cancel then return
cancel = cpar('xgetoffset',y2,4,'Y2',[2,3,4,5],1)
if cancel then return

mkct

;  Set the fonts

getfonts,buttonfont,textfont

;  Build three structures which will hold important info.
;  w - contains info pertaining to widget operations.
;  r - contains info pertaining to the reduction process.
;  d - contains all of the data.

ptr = ptr_new({offset:0.})

w = {plotwin1:0,$
     plotwin2:0,$
     xgetoffset_base:0L}

r = {guesspos1:1.,$
     stuff:1.,$
     guesspos2:1.,$
     pos1:1.,$
     pos2:1.,$
     ptr:ptr}

d = {x1:x1,$
     x2:x2,$
     y1:y1,$
     y2:y2}

p = {plotwin1_wid:0L,$
     plotwin2_wid:0L,$
     plotwinsize:[600.,280.],$
     pscale_win1:!p,$
     pscale_win2:!p,$
     widgetsize:[0,0],$
     xscale_win1:!x,$
     xscale_win2:!x,$
     yscale_win1:!y,$
     yscale_win2:!y,$
     zoomwin1_wid:0L,$
     zoomwin2_wid:0L}

;  Load the three structures in the state structure.

state = {w:w,r:r,d:d,p:p}

;  Build the widget.

state.w.xgetoffset_base = widget_base(TITLE='XGetoffset', $
                                      /COLUMN)

   o = (n_elements(ORDER) ne 0) ? 'Order '+string(order,FORMAT='(i2.2)'):''

   message=o+' - '+'Please select one line, and click on it in each spectrum.'
   label = widget_label(state.w.xgetoffset_base,$
                        /ALIGN_CENTER,$
                        VALUE=message,$
                        FONT=buttonfont)

   row1 = widget_base(state.w.xgetoffset_base,$
                      /ROW)
   
      state.w.plotwin1 = widget_draw(row1,$
                                     XSIZE=state.p.plotwinsize[0],$
                                     YSIZE=state.p.plotwinsize[1],$
                                     /BUTTON_EVENTS,$
                                     UVALUE='Plot Window 1')
   
      zoom1 = widget_draw(row1,$
                          XSIZE=250,$
                          YSIZE=280)

   row2 = widget_base(state.w.xgetoffset_base,$
                      /ROW)

      state.w.plotwin2 = widget_draw(row2,$
                                     XSIZE=state.p.plotwinsize[0],$
                                     YSIZE=state.p.plotwinsize[1],$
                                     /BUTTON_EVENTS,$
                                     UVALUE='Plot Window 2')
      zoom2 = widget_draw(row2,$
                          XSIZE=250,$
                          YSIZE=280)      

   button = widget_button(state.w.xgetoffset_base,$
                          VALUE='Accept',$
                          FONT=buttonfont,$
                          UVALUE='Accept')

; Get things running.  Center the widget using the Fanning routine.

centertlb,state.w.xgetoffset_base

widget_control, state.w.xgetoffset_base, /REALIZE

;  Get plotwin ids

widget_control, state.w.plotwin1, GET_VALUE=x
state.p.plotwin1_wid = x
widget_control, state.w.plotwin2, GET_VALUE=x
state.p.plotwin2_wid = x
widget_control, zoom1, GET_VALUE=x
state.p.zoomwin1_wid = x
widget_control, zoom2, GET_VALUE=x
state.p.zoomwin2_wid = x

xgetoffset_plotspec,state,/SPEC1,/SPEC2

base = state.w.xgetoffset_base

; Put state variable into the user value of the top level base.

widget_control, state.w.xgetoffset_base, SET_UVALUE=state, /NO_COPY

; Start the Event Loop. This will be a non-blocking program.

XManager, 'xgetoffset', $
  base, $
  CLEANUP='xgetoffset_cleanup'

offset = *ptr
ptr_free, ptr


end





