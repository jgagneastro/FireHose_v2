;+
; NAME:
;     xmkmask
;    
; PURPOSE:
;     Eliminates bad spectra from a stack of spectra.
;    
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     xmkmask,wave,stack,mask,XUNITS=xunits,YUNITS=yunits,$
;             GROUP_LEADER=group_leader,IMASK=imask,CANCEL=cancel
;
; INPUTS:
;     wave  - A 1-D wavelength array
;     stack - An array [*,nspec] of spectra
;    
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     GROUP_LEADER - The widget ID of an existing widget that serves
;                    as "group leader" for the newly-created widget. 
;     IMASK        - An array the size of 'stack' of 1s=good and 0s=bad giving 
;                    the input mask
;     XUNITS       - A string giving the wavelength units
;     YUNITS       - A string giving the flux units 
;     CANCEL       - Set on return if there is a problem
;     CANCEL       - Set on return if there is a problem
;     
; OUTPUTS:
;     mask - An array of the size 'stack' of 1s=good and 0s=bad
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
;     Create a mask for a set of spectra.
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2002-05-27 - Written by M. Cushing, Institute for Astornomy, UH
;-
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xmkmask_initcommon,wave,stack,STACKMASK=stackmask,ISPECMASK=ispecmask

common xmkmask_state, state

imginfo,stack,ndat,nspec

medcomb,stack,med
yrange = [0.4*min(med,/NAN,MAX=max),1.2*max]

if n_elements(ISPECMASK) eq 0 then begin

    ispecmask = stack
    ispecmask[*] = 1


endif

if n_elements(STACKMASK) eq 0 then stackmask=intarr(nspec)+1

orders = indgen(nspec)+1
;z = where(stackmask eq 1)


;  Build three structures which will hold important info.

w = {idwin:0,$
     keyboard:0L,$
     mask_bg:0L,$
     message:0L,$
     plotwin:0,$
     xmkmask_base:0L,$
     xmin_fld:[0L,0L],$
     xmax_fld:[0L,0L],$
     ymin_fld:[0L,0L],$
     ymax_fld:[0L,0L]}

r = {colors:[1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,$
             1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,$
             1,2,3,4,5,6,7,8,9,10],$
     ispecmask:ispecmask,$
     lines:[0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,$
            3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,$
            5,5,5,5,5,5,5,5,5,5],$
     cancel:0,$
     cursormode:'None',$
     maskrange:[0.,0.],$
     nspec:nspec,$
     orders:orders[z],$
     mask1d:intarr(ndat)+1,$
     maskmask:stackmask,$
     stackmask:stackmask}

d = {oflux:stack,$
     owave:wave}

p = {buffer:[0.,0.],$
     cursor:0,$
     idwin_wid:0L,$
     pixmap_wid:0L,$
     plotwin_wid:0L,$
     plotxrange:[min(wave,MAX=max),max],$
     plotyrange:yrange,$
     plotabsyrange:yrange,$
     plotabsxrange:[min(wave,MAX=max),max],$
     plotsize:[600,450],$
     pscale:!p,$
     xscale:!x,$
     xunits:'',$
     yscale:!y,$
     yunits:'',$
     reg:[[!values.f_nan,!values.f_nan],$
          [!values.f_nan,!values.f_nan]]}

;  Load the three structures in the state structure.

state = {w:w,r:r,d:d,p:p}

end
;
;******************************************************************************
;
pro xmkmask_plotidwin

common xmkmask_state

wset, state.p.idwin_wid

plot,indgen(state.r.nspec+2),/NODATA,XRANGE=[0,1],$
     YRANGE=[0,state.r.nspec+1],XSTY=5,YSTY=5,XMARGIN=[0,0],YMARGIN=[0,0], $
     CHARSIZE=mc_strsize('!5A',0.01)

for i = 0, state.r.nspec-1 do begin

    if state.r.stackmask[i] eq 1 then begin

        plots,[0,0.5],[state.r.nspec-i,state.r.nspec-i],$
          COLOR=state.r.colors[i],LINESTYLE=state.r.lines[i]
        xyouts,0.6,state.r.nspec-i,'Spec '+string(i+1,FORMAT='(i2.2)'),$
          COLOR=state.r.colors[i],CHARSIZE=mc_strsize('!5A',0.01)
       
    endif

endfor

end
;
;******************************************************************************
;
pro xmkmask_plotspec

common xmkmask_state

;z = where(state.r.stackmask eq 1,count)

plot, state.d.owave,state.d.oflux[*,0],/NODATA,/XSTY,/YSTY,$
      YRANGE=state.p.plotyrange,XRANGE=state.p.plotxrange, $
      CHARSIZE=mc_strsize('!5A',0.01)

for i = 0, state.r.nspec-1 do begin

    if state.r.stackmask[i] eq 1 then begin

        flux = state.d.oflux[*,i]
        if state.r.maskmask[i] eq 1 then begin
            
            z = where(state.r.mask1d eq 0,count)
            if count ne 0 then flux[z] = !values.f_nan
            
        endif
        
        oplot,state.d.owave,flux,COLOR=state.r.colors[i],$
          LINESTYLE=state.r.lines[i],PSYM=10
        
    endif
    
endfor

z = where(finite(state.r.maskrange) eq 1,count)
if count ne 0 then begin

    for i = 0, count-1 do plots,[state.r.maskrange[i],state.r.maskrange[i]],$
      !y.crange,COLOR=7,LINESTYLE=2,THICK=2
    
endif

state.p.pscale = !p
state.p.xscale = !x
state.p.yscale = !y

end
;
;******************************************************************************
; 
pro xmkmask_plotupdate

common xmkmask_state

wset, state.p.pixmap_wid
erase
xmkmask_plotspec

wset, state.p.plotwin_wid
device, COPY=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,0,$
              state.p.pixmap_wid]

end
;
;******************************************************************************
;
pro xmkmask_setminmax

common xmkmask_state

widget_control, state.w.xmin_fld[1],SET_VALUE=strtrim(state.p.plotxrange[0],2)
widget_control, state.w.xmax_fld[1],SET_VALUE=strtrim(state.p.plotxrange[1],2)
widget_control, state.w.ymin_fld[1],SET_VALUE=strtrim(state.p.plotyrange[0],2)
widget_control, state.w.ymax_fld[1],SET_VALUE=strtrim(state.p.plotyrange[1],2)

end
;
;******************************************************************************
;
pro xmkmask_zoom,IN=in,OUT=out

common xmkmask_state

delabsx = state.p.plotabsxrange[1]-state.p.plotabsxrange[0]
delx    = state.p.plotxrange[1]-state.p.plotxrange[0]

delabsy = state.p.plotabsyrange[1]-state.p.plotabsyrange[0]
dely    = state.p.plotyrange[1]-state.p.plotyrange[0]

xcen = state.p.plotxrange[0]+delx/2.
ycen = state.p.plotyrange[0]+dely/2.

case state.r.cursormode of 

    'XZoom': begin

        z = alog10(delabsx/delx)/alog10(2)
        if keyword_set(IN) then z = z+1 else z=z-1
        hwin = delabsx/2.^z/2.
        state.p.plotxrange = [xcen-hwin,xcen+hwin]
        xmkmask_plotupdate

    end

    'YZoom': begin

        z = alog10(delabsy/dely)/alog10(2)
        if keyword_set(IN) then z = z+1 else z=z-1
        hwin = delabsy/2.^z/2.
        state.p.plotyrange = [ycen-hwin,ycen+hwin]
        xmkmask_plotupdate

    end

    'Zoom': begin

        z = alog10(delabsx/delx)/alog10(2)
        if keyword_set(IN) then z = z+1 else z=z-1
        hwin = delabsx/2.^z/2.
        state.p.plotxrange = [xcen-hwin,xcen+hwin]

        z = alog10(delabsy/dely)/alog10(2)
        if keyword_set(IN) then z = z+1 else z=z-1
        hwin = delabsy/2.^z/2.
        state.p.plotyrange = [ycen-hwin,ycen+hwin]

        xmkmask_plotupdate

    end

    else:

endcase

end
;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro xmkmask_event,event

common xmkmask_state

widget_control, event.id,  GET_UVALUE = uvalue

case uvalue of

    'Accept': widget_control, event.top, /DESTROY

    'Cancel': begin

        state.r.cancel = 1
        widget_control, event.top, /DESTROY

    end

    'Clear Mask': begin

        state.r.maskrange = !values.f_nan
        state.r.mask1d = 1
        xmkmask_plotupdate

    end
    'Keyboard': begin

        case strtrim(event.ch,2) of 

            'c': begin ; Clear

                state.r.cursormode = 'None'
                state.p.reg=!values.f_nan
                xmkmask_plotupdate
                
            end

            'i': xmkmask_zoom,/IN

            'o': xmkmask_zoom,/OUT

            's': begin

                state.r.cursormode = 'Select'
                state.p.reg = !values.f_nan
                xmkmask_plotupdate

            end

            'w': begin

                state.p.plotxrange = state.p.plotabsxrange
                z = where(state.r.stackmask eq 1)
                state.p.plotyrange = state.p.plotabsyrange
                xmkmask_plotupdate
                xmkmask_setminmax
                
            end
            
            'x': begin 

                state.r.cursormode = 'XZoom'
                state.p.reg=!values.f_nan

            end

            'y': begin 
                
                state.r.cursormode = 'YZoom'
                state.p.reg=!values.f_nan

            end

            'z': begin

                state.r.cursormode = 'Zoom'
                state.p.reg=!values.f_nan

            end
        
            else:
            
        endcase
           
    end

    'Help Done': widget_control, event.top, /DESTROY

    'Select Spectra': begin

        ploto = state.r.orders[event.value]
        state.r.maskmask[ploto-1] = event.select
        xmkmask_plotupdate

    end

endcase

cont: 

end
;
;******************************************************************************
;
pro xmkmask_minmax_event,event

common xmkmask_state

xmin = cfld(state.w.xmin_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
xmin2 = crange(xmin,state.p.plotxrange[1],'X Min',/KLT,$
               WIDGET_ID=state.w.xmkmask_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.xmin_fld[0],SET_VALUE=state.p.plotxrange[0]
    return

endif else state.p.plotxrange[0] = xmin2

xmax = cfld(state.w.xmax_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
xmax2 = crange(xmax,state.p.plotxrange[0],'X Max',/KGT,$
               WIDGET_ID=state.w.xmkmask_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.xmax_fld[0],SET_VALUE=state.p.plotxrange[1]
    return

endif else state.p.plotxrange[1] = xmax2

ymin = cfld(state.w.ymin_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
ymin2 = crange(ymin,state.p.plotyrange[1],'Y Min',/KLT,$
               WIDGET_ID=state.w.xmkmask_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.ymin_fld[0],SET_VALUE=state.p.plotyrange[0]
    return

endif else state.p.plotyrange[0] = ymin2

ymax = cfld(state.w.ymax_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
ymax2 = crange(ymax,state.p.plotyrange[0],'Y Max',/KGT,$
               WIDGET_ID=state.w.xmkmask_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.ymax_fld[0],SET_VALUE=state.p.plotyrange[1]
    return

endif else state.p.plotyrange[1] = ymax2

xmkmask_plotupdate

end
;
;******************************************************************************
;
pro xmkmask_winevent,event

common xmkmask_state

widget_control, event.id,  GET_UVALUE = uvalue

;  Check to see if it is a TRACKING event.

if strtrim(tag_names(event,/STRUCTURE_NAME),2) eq 'WIDGET_TRACKING' then begin

    if event.enter eq 0 then widget_control, state.w.keyboard, SENSITIVE=0
    goto, cont
    
endif

;  If not, set the keyboard focus and active window.

widget_control, state.w.keyboard, /INPUT_FOCUS, /SENSITIVE
wset, state.p.plotwin_wid

!p = state.p.pscale
!x = state.p.xscale
!y = state.p.yscale
x  = event.x/float(state.p.plotsize[0])
y  = event.y/float(state.p.plotsize[1])
xy = convert_coord(x,y,/NORMAL,/TO_DATA)

if event.type eq 1 then begin

    case state.r.cursormode of 

       'Select': begin

            z = where(finite(state.p.reg) eq 1,count)
            if count eq 0 then begin
                
                state.p.reg[*,0] = xy[0:1]
                wset, state.p.pixmap_wid
                plots, [event.x,event.x],[0,state.p.plotsize[1]],COLOR=7,$
                  /DEVICE,LINESTYLE=2,THICK=2
                wset, state.p.plotwin_wid
                device, COPY=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,$
                              0,state.p.pixmap_wid]


            endif else begin 
                
                state.p.reg[*,1] = xy[0:1]
                tmp = reform(state.p.reg[0,*])
                tmp = tmp[sort(tmp)]
                state.r.maskrange = tmp

                tabinv,state.d.owave,state.r.maskrange,idx
                state.r.mask1d[idx[0]:idx[1]] = 0.0
                
                state.r.cursormode = 'None'
                state.p.reg = !values.f_nan
                xmkmask_plotupdate
                
            endelse
    
        end


        'XZoom': begin

            z = where(finite(state.p.reg) eq 1,count)
            if count eq 0 then begin
                
                state.p.reg[*,0] = xy[0:1]
                wset, state.p.pixmap_wid
                plots, [event.x,event.x],[0,state.p.plotsize[1]],COLOR=2,$
                  /DEVICE,LINESTYLE=1,THICK=2
                wset, state.p.plotwin_wid
                device, COPY=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,$
                              0,state.p.pixmap_wid]

            endif else begin

                state.p.reg[*,1] = xy[0:1]
                state.p.plotxrange = [min(state.p.reg[0,*],MAX=m),m]
                state.r.cursormode = 'None'
                state.p.reg = !values.f_nan
                xmkmask_plotupdate
                xmkmask_setminmax
                
            endelse

        end

        'YZoom': begin

            z = where(finite(state.p.reg) eq 1,count)
            if count eq 0 then begin
                
                state.p.reg[*,0] = xy[0:1]
                wset, state.p.pixmap_wid
                plots, [0,state.p.plotsize[0]],[event.y,event.y],COLOR=2,$
                  /DEVICE,LINESTYLE=1,THICK=2

                wset, state.p.plotwin_wid
                device, COPY=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,$
                              0,state.p.pixmap_wid]
               
            endif else begin

                state.p.reg[*,1] = xy[0:1]
                state.p.plotyrange = [min(state.p.reg[1,*],MAX=m),m]
                state.r.cursormode = 'None'
                state.p.reg = !values.f_nan
                xmkmask_plotupdate
                xmkmask_setminmax
                
            endelse

        end
        
        'Zoom': begin
            
            z = where(finite(state.p.reg) eq 1,count)
            if count eq 0 then state.p.reg[*,0] = xy[0:1] else begin 
                
                state.p.reg[*,1] = xy[0:1]
                state.p.plotxrange   = [min(state.p.reg[0,*],MAX=max),max]
                state.p.plotyrange   = [min(state.p.reg[1,*],MAX=max),max]
                xmkmask_plotupdate
                xmkmask_setminmax
                state.r.cursormode   = 'None'
                state.p.reg = !values.f_nan
                
            endelse
            
        end

    endcase

endif

;  Copy the pixmaps and draw the lines.

wset, state.p.plotwin_wid
device, COPY=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,0,$
              state.p.pixmap_wid]

case state.r.cursormode of 

    'XZoom': plots, [event.x,event.x],[0,state.p.plotsize[1]],COLOR=2,/DEVICE

    'YZoom': plots, [0,state.p.plotsize[0]],[event.y,event.y],COLOR=2,/DEVICE

    'Zoom': begin

        plots, [event.x,event.x],[0,state.p.plotsize[1]],COLOR=2,/DEVICE
        plots, [0,state.p.plotsize[0]],[event.y,event.y],COLOR=2,/DEVICE
        xy = convert_coord(event.x,event.y,/DEVICE,/TO_DATA)
        plots,[state.p.reg[0,0],state.p.reg[0,0]],[state.p.reg[1,0],xy[1]],$
          LINESTYLE=2,COLOR=2
        plots, [state.p.reg[0,0],xy[0]],[state.p.reg[1,0],state.p.reg[1,0]],$
          LINESTYLE=2,COLOR=2
        
    end

    else: begin

        plots, [event.x,event.x],[0,state.p.plotsize[1]],COLOR=2,/DEVICE
        plots, [0,state.p.plotsize[0]],[event.y,event.y],COLOR=2,/DEVICE

    end

endcase

;  Update cursor tracker

label = 'Cursor X: '+strtrim(xy[0],2)+', Y:'+strtrim(xy[1],2)
widget_control,state.w.message,SET_VALUE=label
    
cont:
   
end
;
;******************************************************************************
;
; ------------------------------Main Program-------------------------------- 
;
;******************************************************************************
;
pro xmkmask,wave,stack,specmask,XUNITS=xunits,YUNITS=yunits,$
            STACKMASK=stackmask,ISPECMASK=ispecmask,$
            GROUP_LEADER=group_leader,CANCEL=cancel

if n_params() lt 2 then begin

    print, 'Syntax - xmkmask,wave,stack,specmask,stackmask,XUNITS=xunits,$'
    print, '                 YUNITS=yunits,STACKMASK=stackmask,$'
    print, '                 SPECMASK=ispecmask,$'
    print, '                 GROUP_LEADER=group_leader,CANCEL=cancel'
    canel = 1
    return

endif

cancel = cpar('xmkmask',wave,1,'Wave',[2,3,4,5],1)
if cancel then return
cancel = cpar('xmkmask',stack,2,'Stack',[2,3,4,5],[1,2])
if cancel then return

xmkmask_initcommon,wave,stack,STACKMASK=stackmask,ISPECMASK=ispecmask


if n_elements(GROUP_LEADER) ne 0 then widget_control,group_leader,SENSITIVE=0

common xmkmask_state
    
;  Get the fonts and load color table

getfonts,buttonfont,textfont
mkct

state.w.xmkmask_base = widget_base(TITLE='Xmkmask', $
                                       GROUP_LEADER=group_leader,$
                                       /COLUMN)

   BUTTON = widget_button(state.w.xmkmask_base,$
                          FONT=buttonfont,$
                          EVENT_PRO='xmkmask_event',$
                          VALUE='Cancel',$
                          UVALUE='Cancel')

   row_base = widget_base(state.w.xmkmask_base,$
                          /ROW)
       
      col1_base = widget_base(row_base,$
                              EVENT_PRO='xmkmask_event',$
                              /FRAME,$
                              /COLUMN)

         if state.r.nspec gt 15 then begin
             
             value = string(findgen(state.r.nspec)+1,format='(i2.2)')
             z = where(state.r.stackmask eq 1,cnt)
             state.w.mask_bg = cw_bgroup(col1_base,$
                                         FONT=buttonfont,$
                                         'Spec '+value[z],$
                                         /COLUMN,$
                                         /SCROLL,$
                                         XSIZE=100,$
                                         X_SCROLL_SIZE=100,$
                                         Y_SCROLL_SIZE=400,$
                                         /RETURN_INDEX,$
                                         YSIZE=1000,$
                                         /NONEXCLUSIVE,$
                                         LABEL_TOP='Mask Spectra:',$
                                         UVALUE='Select Spectra',$
                                         SET_VALUE=intarr(cnt)+1)
             
             
         endif else begin
             
             value = string(findgen(state.r.nspec)+1,format='(i2.2)')
             z = where(state.r.stackmask eq 1,cnt)
             state.w.mask_bg = cw_bgroup(col1_base,$
                                         FONT=buttonfont,$
                                         'Spec '+value[z],$
                                         /COLUMN,$
                                         /RETURN_INDEX,$
                                         /NONEXCLUSIVE,$
                                         LABEL_TOP='Mask Spectra:',$
                                         UVALUE='Select Spectra',$
                                         SET_VALUE=intarr(cnt)+1)
             
         endelse

      col2_base = widget_base(row_base,$
                              /COLUMN)
             
         state.w.message = widget_text(col2_base, $
                                       YSIZE=1)

         state.w.keyboard = widget_text(col2_base, $
                                        /ALL_EVENTS,$
                                        SCR_XSIZE=1, $
                                        SCR_YSIZE=1, $
                                        EVENT_PRO='xmkmask_event',$
                                        UVALUE='Keyboard',$
                                        VALUE='')
         
         row = widget_base(col2_base,$
                           /ROW)

            state.w.idwin = widget_draw(row,$
                                        XSIZE=150,$
                                        YSIZE=state.p.plotsize[1],$
                                        UVALUE='ID Window')
            
            
            state.w.plotwin = widget_draw(row,$
                                          XSIZE=state.p.plotsize[0],$
                                          YSIZE=state.p.plotsize[1],$
                                          /TRACKING_EVENTS,$
                                          /BUTTON_EVENTS,$
                                          /MOTION_EVENTS,$
                                          EVENT_PRO='xmkmask_winevent',$
                                          UVALUE='Plot Window 1')
            
            accept = widget_button(col2_base,$
                                   EVENT_PRO='xmkmask_event',$
                                   FONT=buttonfont,$
                                   VALUE='Clear Mask',$
                                   UVALUE='Clear Mask')       

         row_base = widget_base(col2_base,$
                                /FRAME,$
                                /ROW)
         
            xmin = coyote_field2(row_base,$
                                 LABELFONT=buttonfont,$
                                 FIELDFONT=textfont,$
                                 TITLE='X Min:',$
                                 UVALUE='X Min',$
                                 XSIZE=12,$
                                 EVENT_PRO='xmkmask_minmax_event',$
                                 /CR_ONLY,$
                                 TEXTID=textid)
            state.w.xmin_fld = [xmin,textid]
            
            xmax = coyote_field2(row_base,$
                                 LABELFONT=buttonfont,$
                                 FIELDFONT=textfont,$
                                 TITLE='X Max:',$
                                 UVALUE='X Max',$
                                 XSIZE=12,$
                                 EVENT_PRO='xmkmask_minmax_event',$
                                 /CR_ONLY,$
                                 TEXTID=textid)
            state.w.xmax_fld = [xmax,textid]
            
            ymin = coyote_field2(row_base,$
                                 LABELFONT=buttonfont,$
                                 FIELDFONT=textfont,$
                                 TITLE='Y Min:',$
                                 UVALUE='Y Min',$
                                 XSIZE=12,$
                                 EVENT_PRO='xmkmask_minmax_event',$
                                 /CR_ONLY,$
                                 TEXTID=textid)
            state.w.ymin_fld = [ymin,textid]
            
            ymax = coyote_field2(row_base,$
                                 LABELFONT=buttonfont,$
                                 FIELDFONT=textfont,$
                                 TITLE='Y Max:',$
                                 UVAL='Y Max',$
                                 XSIZE=12,$
                                 EVENT_PRO='xmkmask_minmax_event',$
                                 /CR_ONLY,$
                                 TEXTID=textid)
            state.w.ymax_fld = [ymax,textid]

            accept = widget_button(state.w.xmkmask_base,$
                                   EVENT_PRO='xmkmask_event',$
                                   FONT=buttonfont,$
                                   VALUE='Accept',$
                                   UVALUE='Accept')            
             
; Get things running.  Center the widget using the Fanning routine.
          
centertlb,state.w.xmkmask_base
widget_control, state.w.xmkmask_base, /REALIZE

;  Get plotwin ids
          
widget_control, state.w.plotwin, GET_VALUE = x
state.p.plotwin_wid = x

widget_control, state.w.idwin, GET_VALUE = x
state.p.idwin_wid = x

window, /FREE, /PIXMAP,XSIZE=state.p.plotsize[0],$
  YSIZE=state.p.plotsize[1]
state.p.pixmap_wid = !d.window


xmkmask_plotidwin
xmkmask_plotupdate
xmkmask_setminmax


; Start the Event Loop. This will be a non-blocking program.

XManager, 'xmkmask', $
  state.w.xmkmask_base

cancel = state.r.cancel


specmask = state.r.ispecmask 
if not cancel then begin

    for i = 0,state.r.nspec-1 do begin

        if state.r.maskmask[i] eq 1 then specmask[*,i] = state.r.mask1d

    endfor

endif 
    
if n_elements(GROUP_LEADER) ne 0 then widget_control, group_leader, SENSITIVE=1

end
