;+
; NAME:
;     xmc_syntphot
;   
; PURPOSE:
;     To perform synthetic photometry on SpeX spectra
;    
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     xmc_syntphot
;    
; INPUTS:
;     None
;    
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;     
; OUTPUTS:
;     Outputs the results to the widget
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
;     Integrates over the chosen filters relative to Vega.
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2003       - Written by M. Cushing, Institute for Astronomy, UH
;     2005-02-11 - Added output features
;
;-
;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro xmc_syntphot_event, event

widget_control, event.top, GET_UVALUE = state, /NO_COPY
widget_control, event.id,  GET_UVALUE = uvalue
widget_control, /HOURGLASS

case uvalue of

    'Compute Magnitudes': xmc_syntphot_computemags,state

    'Keyboard': begin

        if state.r.freeze then goto, cont
        case strtrim(event.ch,2) of 
            
           'a': begin
              
              state.p.plotabsxrange = state.p.plotxrange
              state.p.plotabsyrange = state.p.plotyrange
                

           end
           
           'c': begin
                
                state.r.cursormode = 'None'
                state.p.reg = !values.f_nan
                xmc_syntphot_plotupdate,state
            
            end

            'e': state.r.cursormode = 'Connect'

            'i': xmc_syntphot_zoom,state,/IN

            'o': xmc_syntphot_zoom,state,/OUT

            's': *state.d.espec = *state.d.pspec

            'u': begin ; Undo

                *state.d.pspec = *state.d.espec
                state.p.reg = !values.f_nan
                xmc_syntphot_plotupdate,state

            end

            'w': begin

                state.p.plotxrange = state.p.plotabsxrange
                state.p.plotyrange = state.p.plotabsyrange
                xmc_syntphot_plotupdate,state
                xmc_syntphot_setminmax,state

            end

            'x': begin 

                state.r.cursormode = 'XZoom'
                state.p.reg = !values.f_nan
                
            end

            'y': begin 
                
                state.r.cursormode = 'YZoom'
                state.p.reg = !values.f_nan

            end

            'z': begin

                state.r.cursormode = 'Zoom'
                state.p.reg = !values.f_nan
                
            end
        
            else:

        endcase

    end

    'Load Spectrum': xmc_syntphot_loadspec,state

    'Filter': begin

    end

    'Info Type': begin

        widget_control, state.w.coltable_base,MAP=0
        widget_control, state.w.magtable_base,MAP=0
        state.r.infotype = fix(event.value)
        
        if state.r.infotype eq 0 then widget_control, $
          state.w.magtable_base,MAP=1
        if state.r.infotype eq 1 then widget_control, $
          state.w.coltable_base,MAP=1


    end

    'Quit': begin

        widget_control, event.top, /DESTROY
        goto, getout
        
    end

    'Spectrum Button': begin

        spec = dialog_pickfile(DIALOG_PARENT=state.w.xmc_syntphot_base,$
                               PATH=state.r.path,/MUST_EXIST,FILTER='*.fits',$
                               GET_PATH=path)
        if spec eq '' then goto, cont
        state.r.path = path
        widget_control,state.w.spectrum_fld[1],SET_VALUE = strtrim(spec,2)
        mc_setfocus,state.w.spectrum_fld

    end

    'System': begin

        state.r.currentsystem = total(event.index)
        xmc_syntphot_changesystem,state

    end

    'Write Results': xmc_syntphot_writefile,state

    else:

endcase

;  Put state variable into the user value of the top level base.
 
cont: 
widget_control, state.w.xmc_syntphot_base, SET_UVALUE=state, /NO_COPY
getout:

end
;
;******************************************************************************
;
pro xmc_syntphot_plotwinevent,event

widget_control, event.top, GET_UVALUE = state, /NO_COPY
widget_control, event.id,  GET_UVALUE = uvalue

;  Check to see if it is a TRACKING event.

if strtrim(tag_names(event,/STRUCTURE_NAME),2) eq 'WIDGET_TRACKING' then begin

    if event.enter eq 0 then widget_control, state.w.keyboard, SENSITIVE=0
    wset, state.p.plotwin_wid
    device, COPY=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,0,$
                  state.p.pixmap_wid]
    goto, cont
    
endif

;  If not, set the keyboard focus and active window.

widget_control, state.w.keyboard, /INPUT_FOCUS, /SENSITIVE

!p = state.p.pscale
!x = state.p.xscale
!y = state.p.yscale
x  = event.x/float(state.p.plotsize[0])
y  = event.y/float(state.p.plotsize[1])
xy = convert_coord(x,y,/NORMAL,/TO_DATA)

if event.type eq 1 then begin

    case state.r.cursormode of 

        'Connect': begin
            
            z = where(finite(state.p.reg) eq 1,count)
            if count eq 0 then begin
                
                state.p.reg[*,0] = xy[0:1]
                tabinv,(*state.d.espec)[*,0],xy[0],idx
                idx = total(idx)
                
                wset, state.p.pixmap_wid
                plots, [xy[0],xy[0]],[(*state.d.espec)[idx,1],$
                                      (*state.d.espec)[idx,1]],COLOR=3,PSYM=2
                wset, state.p.plotwin_wid
                device, COPY=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,$
                              0,state.p.pixmap_wid]
                
            endif else begin
                
                state.p.reg[*,1] = xy[0:1]
                xmc_syntphot_connect,state
            
            endelse
        
        end

        'Zoom': begin
            
            z = where(finite(state.p.reg) eq 1,count)
            if count eq 0 then state.p.reg[*,0] = xy[0:1] else begin 
                
                state.p.reg[*,1] = xy[0:1]
                state.p.plotxrange   = [min(state.p.reg[0,*],MAX=max),max]
                state.p.plotyrange   = [min(state.p.reg[1,*],MAX=max),max]
                xmc_syntphot_plotupdate,state
                xmc_syntphot_setminmax,state
                state.r.cursormode   = 'None'
                state.p.reg = !values.f_nan
                
            endelse
            
        end
        
        'XZoom': begin
            
            z = where(finite(state.p.reg) eq 1,count)
            if count eq 0 then begin
                
                state.p.reg[*,0] = xy[0:1]
                wset, state.p.pixmap_wid
                plots, [event.x,event.x],[0,state.p.plotsize[1]],COLOR=2,$
                  /DEVICE,LINESTYLE=2,THICK=2
                wset, state.p.plotwin_wid
                device, COPY=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,$
                              0,state.p.pixmap_wid]
                
            endif else begin
                
                state.p.reg[*,1] = xy[0:1]
                state.p.plotxrange = [min(state.p.reg[0,*],MAX=m),m]
                state.r.cursormode = 'None'
                state.p.reg = !values.f_nan
                xmc_syntphot_plotupdate,state
                xmc_syntphot_setminmax,state
                
            endelse
            
        end

        'YZoom': begin

            z = where(finite(state.p.reg) eq 1,count)
            if count eq 0 then begin
                
                state.p.reg[*,0] = xy[0:1]
                wset, state.p.pixmap_wid
                plots, [0,state.p.plotsize[0]],[event.y,event.y],COLOR=2,$
                  /DEVICE,LINESTYLE=2,THICK=2
                
                wset, state.p.plotwin_wid
                device, COPY=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,$
                              0,state.p.pixmap_wid]
                
            endif else begin
                
                state.p.reg[*,1] = xy[0:1]
                state.p.plotyrange = [min(state.p.reg[1,*],MAX=m),m]
                state.r.cursormode = 'None'
                state.p.reg = !values.f_nan
                xmc_syntphot_plotupdate,state
                xmc_syntphot_setminmax,state
                
            endelse
            
        end
        
        else:
        
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

;  Update cursor position.
    
if not state.r.freeze then begin

    tabinv, (*state.d.pspec)[*,0],xy[0],idx
    idx = round(idx)
    label = 'Cursor X: '+strtrim(xy[0],2)+', Y:'+strtrim(xy[1],2)
    label = label+'   Spectrum X: '+$
      strtrim( (*state.d.pspec)[idx,0],2)+$
      ', Y:'+strtrim( (*state.d.pspec)[idx,1],2)
    widget_control,state.w.message,SET_VALUE=label

endif

cont:

widget_control, state.w.xmc_syntphot_base, SET_UVALUE=state, /NO_COPY



end
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xmc_syntphot_cleanup,base

widget_control, base, GET_UVALUE = state, /NO_COPY
if n_elements(state) ne 0 then begin


    ptr_free, state.r.objmags
    ptr_free, state.r.scales
    ptr_free, state.r.usescale

    ptr_free, state.d.spec
    ptr_free, state.d.hdr
    ptr_free, state.d.wvega
    ptr_free, state.d.fvega
    ptr_free, state.d.filtertrans
    ptr_free, state.d.files


endif
state = 0B

end
;
;*****************************************************************************
;
pro xmc_syntphot_minmax,event

print, 'fix this later'

end
;
;******************************************************************************
;
pro xmc_syntphot_changesystem,state

state.r.nfilters = n_elements((state.d.filters).(state.r.currentsystem))

;  Update the table 

labels = strarr(6)
labels[*] = ' '
labels[0:state.r.nfilters] = $
  ['Units',strtrim(state.d.filters.(state.r.currentsystem),2)]

widget_control, state.w.magtable,COLUMN_LABELS=labels,USE_TABLE_SELECT=-1
widget_control, state.w.magtable,SET_VALUE=strarr(6,6),$
  USE_TABLE_SELECT=[0,0,5,5]

labels = strarr(6)
labels[*] = ' '
labels[0] = 'Units'
for i =0,state.r.nfilters-2 do begin

    labels[i+1] = strtrim(state.d.filters.(state.r.currentsystem)[i],2)+$
      '-'+strtrim(state.d.filters.(state.r.currentsystem)[i+1],2)

endfor

widget_control, state.w.coltable,COLUMN_LABELS=labels,USE_TABLE_SELECT=-1
widget_control, state.w.coltable,SET_VALUE=strarr(6),USE_TABLE_SELECT=[0,0,0,0]

;  Load the filter profiles

for i = 0, state.r.nfilters-1 do begin

   readcol,filepath(((*state.d.files).(state.r.currentsystem))[i], $
                    ROOT_DIR=state.r.packagepath,SUBDIR='data'),w,t, $
           FORMAT='D,D',COMMENT='#'
   z = where(t ge 0.0)
   w = w[z]
   t = t[z]
   key = 'filter'+string(i,FORMAT='(i2.2)')
   struc = (i eq 0) ? create_struct(key,[[w],[t]]):$
           create_struct(struc,key,[[w],[t]])
    
endfor
*state.d.filtertrans = struc

;  Erase plot window

if not state.r.freeze then xmc_syntphot_plotupdate,state


end
;
;******************************************************************************
;
pro xmc_syntphot_computemags,state

spec = *state.d.espec

;  Do magnitudes first

magstring = strarr(6,6)
magstring[0,*] = ['microns','','microns','microns','photons*hc',' ']

colstring = strarr(6)
colstring[0] = ''

mags = fltarr(state.r.nfilters)

for i = 0, state.r.nfilters-1 do begin


   trans = (*state.d.filtertrans).(i)
   result = mc_filtcoverage(spec[*,0],trans[*,0])

   if result eq 0 or result eq 1 then begin
        
      mc_syntphot,spec[*,0]*10000.,spec[*,1],*state.d.wvega*10000., $
                  *state.d.fvega,state.r.vegazero[state.r.currentsystem], $
                  trans[*,0]*10000.,trans[*,1],bandwidth,l_eff,l_ave,flux,mag,$
                    PHOTONS=state.d.photons[state.r.currentsystem], $
                    CANCEL=cancel
        
        coverage = (result eq 0) ? 'Full':'Partial'

        magstring[i+1,*] = [strtrim(bandwidth,2),$
                            strtrim(coverage,2),$
                            strtrim(l_ave,2),$
                            strtrim(l_eff,2),$
                            strtrim(flux,2),$
                            string(mag,format='(f6.3)')]

        mags[i] = mag

    endif else begin

        magstring[i+1,*] = ['***','***','***','***','***','***']
        mags[i] = !values.f_nan

    endelse

    *state.d.magstring=mags

endfor

;  Do color array

for i = 0,state.r.nfilters-2 do begin

    colstring[i+1] = string(mags[i]-mags[i+1],FORMAT='(f6.3)')

endfor

widget_control, state.w.magtable,set_value=magstring,USE_TABLE_SELECT=[0,0,5,5]
widget_control, state.w.coltable,set_value=colstring

print, mags



end
;
;******************************************************************************
;
pro xmc_syntphot_connect,state

lwave = min(state.p.reg[0,*],max=rwave)

mctabinv,(*state.d.espec)[*,0],lwave,idx
lidx = total(idx)

mctabinv,(*state.d.espec)[*,0],rwave,idx
ridx = total(idx)

disp = ((*state.d.espec)[lidx,0]-(*state.d.espec)[lidx-10,0]) / 10. 
nx   = floor( ((*state.d.espec)[ridx,0]-(*state.d.espec)[lidx,0])/disp)

newwave = (findgen(nx)+1) * disp + (*state.d.espec)[lidx,0]
z = where(newwave lt (*state.d.espec)[ridx,0],count)
if count ne 0 then newwave = newwave[z]

mflux = ((*state.d.espec)[ridx,1]-(*state.d.espec)[lidx,1]) / $
  ((*state.d.espec)[ridx,0]-(*state.d.espec)[lidx,0])
bflux = (*state.d.espec)[lidx,1]-mflux*(*state.d.espec)[lidx,0]

merror = ((*state.d.espec)[ridx,2]-(*state.d.espec)[lidx,2]) / $
  ((*state.d.espec)[ridx,0]-(*state.d.espec)[lidx,0])
berror = (*state.d.espec)[lidx,2]-merror*(*state.d.espec)[lidx,0]

newflux = poly(newwave,[bflux,mflux])
newerror = poly(newwave,[berror,merror])

*state.d.pspec = [[(*state.d.espec)[0:lidx,0],$
                   newwave,(*state.d.espec)[ridx:*,0]],$
                  [(*state.d.espec)[0:lidx,1],$
                   newflux,(*state.d.espec)[ridx:*,1]],$
                  [(*state.d.espec)[0:lidx,2],$
                   newerror,(*state.d.espec)[ridx:*,2]]]


state.r.cursormode = 'None'
xmc_syntphot_plotupdate,state

end
;
;******************************************************************************
;
pro xmc_syntphot_loadspec,state

;  Clear table.

widget_control, state.w.magtable,SET_VALUE=strarr(6,6),$
  USE_TABLE_SELECT=[0,0,5,5]

widget_control, state.w.coltable,SET_VALUE=strarr(6),USE_TABLE_SELECT=[0,0,0,0]

;  Get files.

spec = cfld(state.w.spectrum_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
spec = cfile(spec,WIDGET_ID=state.w.xmc_syntphot_base,CANCEL=cancel)
if cancel then return

;  Read spectra and load data.

mc_readspec,spec,spec,hdr,obsmode,start,stop,norders,naps,orders,$
            xunits,yunits,slith_pix,slith_arc,slitw_pix,slitw_arc,rp, $
            stdairmass

z = where(finite(spec[*,0]) eq 1)

case strcompress(yunits,/RE) of 

   'ergss-1cm-2A-1':
   
   'ergss-1cm-2Hz-1': begin

      spec[*,1] = mc_chfunits(spec[*,0],spec[*,1],0,3,1,IERROR=spec[*,2], $
                              OERROR=tmp)
      spec[*,2] = tmp

   end

;  This next one is for Mark's models which I got the units wrong on

   'ergs-1cm-1Hz-1': begin

      spec[*,1] = mc_chfunits(spec[*,0],spec[*,1],0,3,1,IERROR=spec[*,2], $
                              OERROR=tmp)
      spec[*,2] = tmp

   end
   
   'Wm-2um-1': begin

      spec[*,1] = mc_chfunits(spec[*,0],spec[*,1],0,0,1,IERROR=spec[*,2], $
                              OERROR=tmp)
      spec[*,2] = tmp
      

   end
   
   'Wm-2Hz-1': begin

      spec[*,1] = mc_chfunits(spec[*,0],spec[*,1],0,2,1,IERROR=spec[*,2], $
                              OERROR=tmp)
      spec[*,2] = tmp

   end
   
   'Jy': begin

      spec[*,1] = mc_chfunits(spec[*,0],spec[*,1],0,4,1,IERROR=spec[*,2], $
                              OERROR=tmp)
      spec[*,2] = tmp

   end

endcase

*state.d.ospec = spec[z,*]
*state.d.espec = spec[z,*]
*state.d.pspec = spec[z,*]

*state.d.hdr  = hdr

state.p.xunits = xunits
state.p.yunits = yunits

;  Load Vega

restore, filepath('lvega5.sav',ROOT_DIR=state.r.spextoolpath,SUBDIR='data')
*state.d.wvega = wvin/10000.

c = 2.99792458e+8

*state.d.fvega = fvin

state.p.plotxrange    = [min(spec[*,0],MAX=max),max]
state.p.plotyrange    = [0,max(spec[*,1],/NAN)]

state.p.plotabsxrange = state.p.plotxrange
state.p.plotabsyrange = state.p.plotyrange

;  Unfreeze the widget 

state.r.freeze = 0

widget_control, state.w.plotwin, DRAW_BUTTON_EVENTS=1

xmc_syntphot_plotupdate,state
xmc_syntphot_setminmax,state


end
;******************************************************************************
;
pro xmc_syntphot_plotfilter,state

wset, state.p.plotwin_wid

trans = *state.d.filtertrans
trange = [min((trans.(state.p.plotfilter))[*,0],MAX=max),max]

result = filtcoverage((*state.d.spec)[*,0],(*state.d.spec)[*,1],$
                      reform((trans.(state.p.plotfilter))[*,0]),$
                      reform((trans.(state.p.plotfilter))[*,1]))

if result eq 0 or result eq 1 then begin

    interpspec,(trans.(state.p.plotfilter))[*,0],$
      (trans.(state.p.plotfilter))[*,1],(*state.d.spec)[*,0],rfilter

    plot,(*state.d.spec)[*,0],rfilter,$
      XSTY=5,YSTY=5,COLOR=5,XRANGE=trange,PSYM=10
    ticks = strtrim(findgen(11)*.1,2)
    axis,YAXIS=1,YTICKS=10,YTICKNAME=ticks,YMINOR=1,COLOR=5

    plot,(*state.d.spec)[*,0],(*state.d.spec)[*,1],/XSTY,YSTY=9,$
      XRANGE=trange,/NOERASE,PSYM=10,XTITLE='!5Wavelength (microns)'

endif else begin

    plot, (trans.(state.p.plotfilter))[*,0],(trans.(state.p.plotfilter))[*,1],$
      XSTY=1,YSTY=1,COLOR=1,XRANGE=trange,PSYM=10,/NODATA,$
      XTITLE='!5Wavelength (microns)',YTITLE='!5Transmission'
    oplot,(trans.(state.p.plotfilter))[*,0],(trans.(state.p.plotfilter))[*,1],$
      color=5

endelse


end
;
;******************************************************************************
;
pro xmc_syntphot_plotspec,state

xtitle ='!5Wavelength ('+strtrim(state.p.xunits,2)+')'
ytitle ='!5Flux ('+strtrim(state.p.yunits,2)+')'

if state.p.plotfilter then begin

        plot, findgen(10),YRANGE=[0,1],YSTYLE=5,XSTYLE=5,$
          XRANGE=state.p.plotxrange,PSYM=10,/NODATA
        ticks = string(findgen(11)*.1,FORMAT='(f3.1)')
        axis,YAXIS=1,YTICKS=10,YTICKNAME='!5'+ticks,YMINOR=1,COLOR=5
        ystyle = 9

endif else ystyle=1
trans = *state.d.filtertrans

for i = 0, state.r.nfilters-1 do begin

    oplot, trans.(i)[*,0],trans.(i)[*,1],COLOR=5

endfor

plot,(*state.d.pspec)[*,0],(*state.d.pspec)[*,1],PSYM=10,/XSTY,YSTY=ystyle,$
  XRANGE=state.p.plotxrange,YRANGE=state.p.plotyrange,XTITLE=xtitle,$
  YTITLE=ytitle,/NOERASE

z = where(finite((*state.d.pspec)[*,1]) eq 0,cnt)
if cnt ne 0 then begin

    yval = !y.crange[1]-(!y.crange[1]-!y.crange[0])*0.1 
    oplot,(*state.d.pspec)[z,0],replicate(yval,cnt),PSYM=1,COLOR=6

endif


state.p.pscale = !p
state.p.xscale = !x
state.p.yscale = !y

end
;
;******************************************************************************
;
pro xmc_syntphot_plotupdate,state

;  Plot window 1

wset, state.p.pixmap_wid
erase
xmc_syntphot_plotspec,state

wset, state.p.plotwin_wid
erase
device, COPY=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,0,$
              state.p.pixmap_wid]

end
;
;******************************************************************************
;
pro xmc_syntphot_setminmax,state

widget_control,state.w.xmin_fld[1],SET_VALUE=strtrim(state.p.plotxrange[0],2)
widget_control,state.w.xmax_fld[1],SET_VALUE=strtrim(state.p.plotxrange[1],2)
widget_control,state.w.ymin_fld[1],SET_VALUE=strtrim(state.p.plotyrange[0],2)
widget_control,state.w.ymax_fld[1],SET_VALUE=strtrim(state.p.plotyrange[1],2)

end
;
;******************************************************************************
;
pro xmc_syntphot_writefile,state

ofile = cfld(state.w.oname_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

openw, lun, ofile, /GET_LUN

for i = 0, n_elements(*state.d.magstring)-1 do begin

    printf, lun, (strtrim(state.d.filters.(state.r.currentsystem)))[i],$
                  (*state.d.magstring)[i]

;    printf, lun, *state.d.magstring

endfor
free_lun, lun

end
;
;******************************************************************************
;
pro xmc_syntphot_zoom,state,IN=in,OUT=out

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
        xmc_syntphot_plotupdate,state
        xmc_syntphot_setminmax,state
        
    end
        
    'YZoom': begin
        
        z = alog10(delabsy/dely)/alog10(2)
        if keyword_set(IN) then z = z+1 else z=z-1
        hwin = delabsy/2.^z/2.
        state.p.plotyrange = [ycen-hwin,ycen+hwin]
        xmc_syntphot_plotupdate,state
        xmc_syntphot_setminmax,state
        
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
        
        xmc_syntphot_plotupdate,state
        xmc_syntphot_setminmax,state
        
    end
    
    else:

endcase

end
;
;******************************************************************************
;
; ------------------------------Main Program---------------------------------- 
;
;******************************************************************************
;
pro xmc_syntphot

print, ' '
print, '==================='
print, ' '
print, 'Fix the stupid A and micron screw up in the integration.'
re = ' '
read, re

;  Determine package path

last   = strpos(!path,'Spantool3')
first  = strpos(!path,':',last,/REVERSE_SEARCH)
result = strmid(!path,first+1,last-first+8)

packagepath = cpath(result,CANCEL=cancel)
if cancel then return

spextoolpath = file_dirname(file_dirname(file_which('Instrument.dat'),/MARK))

;  Read photometry file

mc_rdphotfile,filepath('photsystems.dat',ROOT_DIR=packagepath,SUBDIR='data'),$
              nsystems,systems,filters,files,photons,vegazero
if cancel then return

;  Load color table and fonts

mkct
mc_getfonts,buttonfont,textfont

;  Build three structures which will hold important info.

w = {col2_base:0L,$
     coltable:0L,$
     coltable_base:0L,$
     keyboard:0L,$
     magtable_base:0L,$
     magtable:0L,$
     message:0L,$
     oname_fld:[0L,0L],$
     plotwin:0L,$
     spectrum_fld:[0L,0L],$
     xmin_fld:[0L,0L],$
     xmax_fld:[0L,0L],$
     xmc_syntphot_base:0L,$
     ymin_fld:[0L,0L],$
     ymax_fld:[0L,0L]}

r = {currentsystem:0,$
     cursormode:'None',$
     freeze:1,$
     infotype:0,$
     nfilters:n_elements(filters.(0)),$
     packagepath:packagepath,$
     path:'',$
     spextoolpath:spextoolpath,$
     vegazero:vegazero}

d = {files:ptr_new(2),$
     filters:filters,$
     filtertrans:ptr_new(2),$
     fvega:ptr_new(2),$
     hdr:ptr_new(2),$
     magstring:ptr_new(2),$
     ospec:ptr_new(2),$
     photons:photons,$
     pspec:ptr_new(2),$
     espec:ptr_new(2),$
     systems:systems,$
     wvega:ptr_new(2)}

*d.files = files

p = {pixmap_wid:0L,$
     plotfilter:1,$
     plotsize:[860,350],$
     plotwin_wid:0L,$
     plotabsxrange:[0.,0.],$
     plotabsyrange:[0.,0.],$
     plotxrange:[0.,0.],$
     plotyrange:[0.,0.],$
     pscale:!p,$
     reg:[[!values.f_nan,!values.f_nan],$
          [!values.f_nan,!values.f_nan]],$
     xscale:!x,$
     yscale:!y,$
     xunits:'',$
     yunits:''}

;  Load the three structures in the state structure.

state = {w:w,r:r,d:d,p:p}

state.w.xmc_syntphot_base = widget_base(TITLE='Xmc_Syntphot', $
                                    /COLUMN)

   quit_button = widget_button(state.w.xmc_syntphot_base,$
                               FONT=buttonfont,$
                               EVENT_PRO='xmc_syntphot_event',$
                               VALUE='Done',$
                               UVALUE='Quit')

   state.w.keyboard = widget_text(state.w.xmc_syntphot_base, $
                                  /ALL_EVENTS, $
                                  SCR_XSIZE=1, $
                                  SCR_YSIZE=1, $
                                  UVALUE='Keyboard', $
                                  EVENT_PRO='xmc_syntphot_event',$
                                  VALUE= '')

   row1_base = widget_base(state.w.xmc_syntphot_base,$
                           EVENT_PRO='xmc_syntphot_event',$
                           /ROW)

      col1_base = widget_base(row1_base,$
                              FRAME=1,$
                              /COLUMN)

         row = widget_base(col1_base,$
                           /ROW,$
                           /BASE_ALIGN_CENTER)
         
            spec_but = widget_button(row,$
                                     FONT=buttonfont,$
                                     VALUE='Spectrum',$
                                     UVALUE='Spectrum Button')
            
            spec_fld = coyote_field2(row,$
                                     LABELFONT=buttonfont,$
                                     FIELDFONT=textfont,$
                                     TITLE=':',$
                                     UVALUE='Spectrum Field',$
                                     XSIZE=18,$
                                     TEXTID=textid)
            state.w.spectrum_fld = [spec_fld,textid]
            
         load = widget_button(col1_base,$
                              VALUE='Load Spectrum',$
                              FONT=buttonfont,$
                              UVALUE='Load Spectrum')

         spec_fld = coyote_field2(col1_base,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='Output Name:',$
                                  UVALUE='Output Name Field',$
                                  XSIZE=18,$
                                  TEXTID=textid)
         state.w.oname_fld = [spec_fld,textid]
            
         load = widget_button(col1_base,$
                              VALUE='Write Results',$
                              FONT=buttonfont,$
                              UVALUE='Write Results')

      col2_base = widget_base(row1_base,$
                              /COLUMN,$
                              FRAME=1,$
                              EVENT_PRO='xmc_syntphot_event')

      table_base = widget_base(col2_base)

         state.w.coltable_base = widget_base(table_base,$
                                     /COLUMN)

            label = ['Color']
            state.w.coltable = widget_table(state.w.coltable_base,$
                                            ROW_LABELS=label,$
                                            XSIZE=6,$
                                            YSIZE=1,$
                                            COLUMN_WIDTH=80,$
                                            ALIGNMENT=1,$
                                            UVALUE='Color Table')
         widget_control, state.w.coltable_base,MAP=0

         state.w.magtable_base = widget_base(table_base,$
                                             /COLUMN)

            label = ['Band Width','Coverage','l_0','l_eff','Flux','Mag']
            state.w.magtable = widget_table(state.w.magtable_base,$
                                            ROW_LABELS=label,$
                                            XSIZE=6,$
                                            YSIZE=6,$
                                            COLUMN_WIDTH=80,$
                                            ALIGNMENT=1,$
                                            UVALUE='Magnitude Table')

         load = widget_button(col2_base,$
                              VALUE='Compute Magnitudes',$
                              FONT=buttonfont,$
                              UVALUE='Compute Magnitudes')

   row2_base = widget_base(state.w.xmc_syntphot_base,$
                           /ROW,$
                           FRAME=1,$
                           EVENT_PRO='xmc_syntphot_event',$
                           /BASE_ALIGN_CENTER)

      systems_dl = widget_droplist(row2_base,$
                                   FONT=buttonfont,$
                                   TITLE='Photometric System:',$
                                   VALUE=state.d.systems,$
                                   UVALUE='System')   

      bg = cw_bgroup(row2_base,$
                     FONT=buttonfont,$
                     ['Magnitudes','Colors'],$
                     /EXCLUSIVE,$
                     /RETURN_INDEX,$
                     /NO_RELEASE,$
                     /ROW,$
                     SET_VALUE=0,$
                     UVALUE='Info Type')

      state.w.message = widget_text(state.w.xmc_syntphot_base, $
                                    YSIZE=1)

   row3_base = widget_base(state.w.xmc_syntphot_base,$
                           /ROW)

      state.w.plotwin = widget_draw(row3_base,$
                                    /MOTION_EVENTS,$
                                    /TRACKING_EVENTS,$
                                    EVENT_PRO='xmc_syntphot_plotwinevent',$
                                    XSIZE=state.p.plotsize[0],$
                                    YSIZE=state.p.plotsize[1],$
                                    UVALUE='Plot Window')

   row4_base = widget_base(state.w.xmc_syntphot_base,$
                           /ROW,$
                           FRAME=1)
         
      xmin = coyote_field2(row4_base,$
                           LABELFONT=buttonfont,$
                           FIELDFONT=textfont,$
                           TITLE='X Min:',$
                           UVALUE='X Min',$
                           XSIZE=12,$
                           EVENT_PRO='xmc_syntphot_minmax',$
                           /CR_ONLY,$
                           TEXTID=textid)
      state.w.xmin_fld = [xmin,textid]
      
      xmax = coyote_field2(row4_base,$
                           LABELFONT=buttonfont,$
                           FIELDFONT=textfont,$
                           TITLE='X Max:',$
                           UVALUE='X Max',$
                           XSIZE=12,$
                           EVENT_PRO='xsynphot_minmax',$
                           /CR_ONLY,$
                           TEXTID=textid)
      state.w.xmax_fld = [xmax,textid]
      
      ymin = coyote_field2(row4_base,$
                           LABELFONT=buttonfont,$
                           FIELDFONT=textfont,$
                           TITLE='Y Min:',$
                           UVALUE='Y Min',$
                           XSIZE=12,$
                           EVENT_PRO='xmc_syntphot_minmax',$
                           /CR_ONLY,$
                           TEXTID=textid)
      state.w.ymin_fld = [ymin,textid]
      
      ymax = coyote_field2(row4_base,$
                           LABELFONT=buttonfont,$
                           FIELDFONT=textfont,$
                           TITLE='Y Max:',$
                           UVALUE='Y Max',$
                           XSIZE=12,$
                           EVENT_PRO='xmc_syntphot_minmax',$
                           /CR_ONLY,$
                           TEXTID=textid)
      state.w.ymax_fld = [ymax,textid]

cont:

; Get things running.  Center the widget using the Fanning routine.

centertlb,state.w.xmc_syntphot_base

widget_control, state.w.xmc_syntphot_base, /REALIZE
widget_control, state.w.plotwin, GET_VALUE = x
state.p.plotwin_wid = x

;  Create pixmap windows

window, /FREE, /PIXMAP,XSIZE=state.p.plotsize[0],YSIZE=state.p.plotsize[1]
state.p.pixmap_wid = !d.window

xmc_syntphot_changesystem,state



; Start the Event Loop. This will be a non-blocking program.

XManager, 'xmc_syntphot', $
  state.w.xmc_syntphot_base,$
  /NO_BLOCK,$
  CLEANUP='xmc_syntphot_cleanup'
;,$
;  EVENT_HANDLER='xmc_syntphot_resizevent'


; Put state variable into the user value of the top level base.

widget_control, state.w.xmc_syntphot_base, SET_UVALUE=state, /NO_COPY

end
