;+
; NAME:
;     xmc_mergespec
;   
; PURPOSE:
;     To merge two spectra together
;    
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     xmc_mergespec
;    
; INPUTS:
;     None
;    
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     None
;     
; OUTPUTS:
;     Writes a Spextool FITS file to disk
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
;     2002 - Written M. Cushing, Institute for Astronomy, UH
;-
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xmc_mergespec_autoscale,state

state.r.scale = 1
xmc_mergespec_cutscalespec,state

z = where(finite(state.p.reg) eq 0,count)

if count ne 0 then begin

    result=dialog_message("Please select a region using 's'.",$
                          /INFORMATION,DIALOG_PARE=state.w.xmc_mergespec_base)
    return

endif
xrange = state.p.reg[0,sort(state.p.reg[0,*])]

zb = where((*state.d.bluespec)[*,0] gt xrange[0] and $
           (*state.d.bluespec)[*,0] lt xrange[1])

zy = where((*state.d.greenspec)[*,0] gt xrange[0] and $
           (*state.d.greenspec)[*,0] lt xrange[1])

bluespec_w   = reform((*state.d.bluespec)[zb,0])
bluespec_f   = reform((*state.d.bluespec)[zb,1])
bluespec_e   = reform((*state.d.bluespec)[zb,2])

greenspec_w = reform((*state.d.greenspec)[zy,0])
greenspec_f = reform((*state.d.greenspec)[zy,1])
greenspec_e = reform((*state.d.greenspec)[zy,2])

mc_interpspec,greenspec_w,greenspec_f,bluespec_w,ngreenspec_f,ngreenspec_e,$
              IYERROR=greenspec_e

z = where(finite(bluespec_f) eq 1 and finite(ngreenspec_f) eq 1 and $
          finite(bluespec_e) eq 1 and finite(ngreenspec_e) eq 1)

denom = bluespec_e^2 + ngreenspec_e^2

scale = total(ngreenspec_f[z]*bluespec_f[z]/denom[z])/$
  total(ngreenspec_f[z]^2/denom[z])

state.r.scale = scale
widget_control, state.w.scale_fld[1],SET_VALUE=strtrim(state.r.scale,2)

state.r.cursormode = 'None'

state.r.scalereg = xrange

xmc_mergespec_cutscalespec,state
xmc_mergespec_plotupdate,state

end
;
;******************************************************************************
;
pro xmc_mergespec_cleanup,base

widget_control, base, GET_UVALUE = state, /NO_COPY
if n_elements(state) ne 0 then begin

    ptr_free, state.r.mergedorders
    ptr_free, state.d.addspec
    ptr_free, state.d.bluespec
    ptr_free, state.d.greenspec
    ptr_free, state.d.hdr
    ptr_free, state.d.mergedspec
    ptr_free, state.d.origspec
    ptr_free, state.d.tempmergedspec
    
endif

state = 0B

end
;
;******************************************************************************
;
pro xmc_mergespec_combinespec,state

;  Fix this crappy program someday


gspec_w = (*state.d.greenspec)[*,0]
gspec_f = (*state.d.greenspec)[*,1]
gspec_e = (*state.d.greenspec)[*,2]

bspec_w = (*state.d.bluespec)[*,0]
bspec_f = (*state.d.bluespec)[*,1]
bspec_e = (*state.d.bluespec)[*,2]

mc_mergespec,bspec_w,bspec_f,gspec_w,gspec_f,owave,oflux,overlap, $
             E1=bspec_e,E2=gspec_e,OERROR=oerror,CANCEL=cancel
if cancel then return

state.r.overlap = overlap

*state.d.tempmergedspec = [[owave],[oflux],[oerror]]

end
;
;******************************************************************************
;
pro xmc_mergespec_cutscalespec,state

if finite(state.r.trimblue) eq 1 then begin

    if state.r.trimdirblue eq 'Right' then begin

        z = where((*state.d.mergedspec)[*,0] lt state.r.trimblue or $
                  finite((*state.d.mergedspec)[*,0]) eq 0)
        *state.d.bluespec = (*state.d.mergedspec)[z,*]

    endif else begin

        z = where((*state.d.mergedspec)[*,0] gt state.r.trimblue or $
                  finite((*state.d.mergedspec)[*,0]) eq 0)
        *state.d.bluespec = (*state.d.mergedspec)[z,*]

    endelse
    
endif else (*state.d.bluespec) = (*state.d.mergedspec)

if finite(state.r.trimgreen) eq 1 then begin

    if state.r.trimdirgreen eq 'Right' then begin

        z = where((*state.d.addspec)[*,0] lt state.r.trimgreen)
        *state.d.greenspec = (*state.d.addspec)[z,*]
        (*state.d.greenspec)[*,1] = (*state.d.greenspec)[*,1]*state.r.scale
        if state.r.weighted then (*state.d.greenspec)[*,2] = $
          (*state.d.greenspec)[*,2]*abs(state.r.scale)

    endif else begin

        z = where((*state.d.addspec)[*,0] gt state.r.trimgreen)
        *state.d.greenspec = (*state.d.addspec)[z,*]
        (*state.d.greenspec)[*,1] = (*state.d.greenspec)[*,1]*state.r.scale
        if state.r.weighted then (*state.d.greenspec)[*,2] = $
          (*state.d.greenspec)[*,2]*abs(state.r.scale)

    endelse

endif else begin

    *state.d.greenspec = reform((*state.d.addspec)[*,*])   
    (*state.d.greenspec)[*,1] = (*state.d.greenspec)[*,1]*state.r.scale
    if state.r.weighted then (*state.d.greenspec)[*,2] = $
      (*state.d.greenspec)[*,2]*abs(state.r.scale)

endelse

end
;
;******************************************************************************
;
pro xmc_mergespec_getminmax1,state

;  Get X range

arr = [min((*state.d.mergedspec)[*,0],MAX=maxb),maxb,$
       min((*state.d.addspec)[*,0],MAX=maxg),maxg]

state.p.plot1xrange    = [min(arr,MAX=max),max]
state.p.plot1absxrange = state.p.plot1xrange

;  Get Y range


zb = where((*state.d.bluespec)[*,0] gt state.p.plot1xrange[0] and $
           (*state.d.bluespec)[*,0] lt state.p.plot1xrange[1])

zg = where((*state.d.greenspec)[*,0] gt state.p.plot1xrange[0] and $
           (*state.d.greenspec)[*,0] lt state.p.plot1xrange[1])

fbmax = max((*state.d.bluespec)[zb,1],/NAN,min=fbmin)
fymax = max((*state.d.greenspec)[zg,1],/NAN,min=fymin)

state.p.plot1yrange    = [(fbmin < fymin),(fbmax > fymax)]
state.p.plot1absyrange = state.p.plot1yrange

end
;
;******************************************************************************
;
pro xmc_mergespec_loadspec,state

;  Get files.

anchor = cfld(state.w.anchorspec_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
anchor = cfile(anchor,WIDGET_ID=state.w.xmc_mergespec_base,$
               CANCEL=cancel)
if cancel then return


add = cfld(state.w.addspec_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
add = cfile(add,WIDGET_ID=state.w.xmc_mergespec_base,$
            CANCEL=cancel)
if cancel then return

;  Read spectra.

mc_readspec,anchor,anchorspec,anchorhdr,obsmode,start,stop,norders,naps,orders,$
            xunits,yunits,slith_pix,slith_arc,slitw_pix,slitw_arc,rp,airmass

mc_readspec,add,addspec,hdr,obsmode,start,stop,norders,naps,orders,$
            xunits,yunits,slith_pix,slith_arc,slitw_pix,slitw_arc,rp,airmass

;if not state.r.weighted then begin

;    anchorspec[*,2] = 1.
;    addspec[*,2] = 1.
    
; endif

state.w.freeze = 0
widget_control, state.w.plotwin1, DRAW_BUTTON_EVENTS=1


*state.d.mergedspec = anchorspec
;nonan = where(finite(addspec[*,1]) eq 1)
;*state.d.addspec = addspec[nonan,*]
*state.d.addspec = addspec

*state.d.greenspec = *state.d.addspec
*state.d.bluespec  = *state.d.mergedspec

*state.d.hdr = anchorhdr

state.p.reg = !values.f_nan

if max(anchorspec[*,0],/NAN) gt max(addspec[*,0],/NAN) then begin

    scale = (min(anchorspec[*,0],/NAN) gt max(addspec[*,0],/NAN)) ? 0:1

    min = min(anchorspec[*,0],/NAN)
    max = max(addspec[*,0],/NAN)

endif else begin

    scale = (max(anchorspec[*,0],/NAN) lt min(addspec[*,0],/NAN)) ? 0:1

    min = min(addspec[*,0],/NAN)
    max = max(anchorspec[*,0],/NAN)

endelse

state.r.trimspec = 'None'
state.r.trimblue = !values.f_nan
state.r.trimgreen = !values.f_nan

state.p.reg = [[min,1],[max,1]]


if scale then xmc_mergespec_autoscale,state
xmc_mergespec_getminmax1,state
xmc_mergespec_plotupdate,state
xmc_mergespec_setminmax,state



end
;
;******************************************************************************
;
pro xmc_mergespec_mergespec,state

xmc_mergespec_combinespec,state

*state.d.mergedspec = reform(*state.d.tempmergedspec)
message = '!5Merging complete.'

wset, state.p.plotwin1_wid
erase
xyouts,0.5,0.5,message,/NORM,charsiz=3,ALIGN=0.5
wset, state.p.pixmap1_wid
erase
xyouts,0.5,0.5,message,/NORM,charsiz=3,ALIGN=0.5

end
;
;******************************************************************************
;
pro xmc_mergespec_plotflux,state

xtitle ='!5Wavelength ('+strtrim(state.p.xunits,2)+')'
ytitle ='!5Flux ('+strtrim(state.p.yunits,2)+')'

if state.p.plottype eq 'Overlap' then begin
    
    plot, findgen(10),/xsty,/ysty,/nodata,$
      xrange=state.p.plot1xrange,yrange=state.p.plot1yrange,$
      xtitle=xtitle,ytitle=ytitle,title='!5Overlapping Spectra'
    
    if state.p.plotbluespec then oplot, (*state.d.bluespec)[*,0],$
      (*state.d.bluespec)[*,1],color=4,psym=10
    
    if state.p.plotgreenspec then oplot, (*state.d.greenspec)[*,0],$
      (*state.d.greenspec)[*,1],color=3,psym=10
    
endif else begin
    
    plot, (*state.d.tempmergedspec)[*,0],(*state.d.tempmergedspec)[*,1],$
      /xsty,/ysty,/nodata,xrange=state.p.plot1xrange,$
      yrange=state.p.plot1yrange,xtitle=xtitle,ytitle=ytitle,$
      title='!5Merged Spectrum'
    
    oplot,(*state.d.tempmergedspec)[*,0],(*state.d.tempmergedspec)[*,1],$
      color=7,psym=10
    
endelse

if state.r.cursormode eq 'Select' then begin
    
    if finite(state.p.reg[0,0]) eq 1 then $
      plots,[state.p.reg[0,0],state.p.reg[0,0]],!y.crange,color=2,$
      linestyle=1,thick=2
    if finite(state.p.reg[0,1]) eq 1 then $
      plots,[state.p.reg[0,1],state.p.reg[0,1]],!y.crange,color=2,$
      linestyle=1,thick=2

endif

state.p.pscale1 = !p
state.p.xscale1 = !x
state.p.yscale1 = !y

end
;
;******************************************************************************
;
pro xmc_mergespec_plotupdate,state

wset, state.p.pixmap1_wid
erase
xmc_mergespec_plotflux,state

wset, state.p.plotwin1_wid
device, copy=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
              state.p.pixmap1_wid]

end
;
;******************************************************************************
;
pro xmc_mergespec_setminmax,state

widget_control, state.w.xmin_fld[1],SET_VAL=strtrim(state.p.plot1xrange[0],2)
widget_control, state.w.xmax_fld[1],SET_VAL=strtrim(state.p.plot1xrange[1],2)
widget_control, state.w.ymin1_fld[1],SET_VAL=strtrim(state.p.plot1yrange[0],2)
widget_control, state.w.ymax1_fld[1],SET_VAL=strtrim(state.p.plot1yrange[1],2)

end
;
;******************************************************************************
;
pro xmc_mergespec_writefile,state

anchor = cfld(state.w.anchorspec_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

add = cfld(state.w.addspec_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

ofile = cfld(state.w.oname_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

ahistory = cfld(state.w.history_fld,7,CANCEL=cancel)
if cancel then return

;if not state.r.weighted then (*state.d.mergedspec)[*,2] = 1.
start = 0
stop  = n_elements((*state.d.mergedspec)[*,1])-1

hdr = *state.d.hdr

fxaddpar,hdr,'IRAFNAME',ofile
fxaddpar,hdr,'ORDERS','0'
fxaddpar,hdr,'NORDERS',1
fxaddpar,hdr,'NAPS',1
fxaddpar,hdr,'START',start
fxaddpar,hdr,'STOP',stop

history = 'This merged spectrum was derived from the files'+$
  strtrim(anchor,2)+' and '+strtrim(add,2)+'.  The scale factor is '+$
  strtrim(state.r.scale,2)

if finite(state.r.scalereg[0]) eq 1 then begin

    history = history+' determined from '+strtrim(state.r.scalereg[0],2)+$
      ' to '+strtrim(state.r.scalereg[1],2)+' microns.'

endif 

history = history+'  The overlap region was from '+$
  strtrim(state.r.overlap[0],2)+' to '+strtrim(state.r.overlap[1],2)+$
  ' microns.  '

history = history+ahistory

length = strlen(history)
loop = ceil(float(length)/65.)
for i = 0, loop-1 do begin
    
    hist = strmid(history,65*i,65)
    fxaddpar,hdr,'HISTORY',hist
    
endfor

if state.r.fitsoutput then writefits,ofile+'.fits',*state.d.mergedspec,hdr

xvspec,ofile+'.fits'

if state.r.textoutput then begin
    
    npix = fxpar(hdr,'NAXIS1')
    openw,lun,ofile+'.txt', /get_lun
    
    for i = 0, n_elements(hdr)-1 do printf, lun, hdr[i]
    
    for i = 0, npix-1 do begin
        
        printf, lun,  strjoin( reform((*state.d.mergedspec)[i,*],3),'  ' )
        
    endfor
    close, lun
    free_lun, lun
    
endif

end
;
;******************************************************************************
;
pro xmc_mergespec_zoom,state,IN=in,OUT=out

delabsx = state.p.plot1absxrange[1]-state.p.plot1absxrange[0]
delx    = state.p.plot1xrange[1]-state.p.plot1xrange[0]

delabsy = state.p.plot1absyrange[1]-state.p.plot1absyrange[0]
dely    = state.p.plot1yrange[1]-state.p.plot1yrange[0]

xcen = state.p.plot1xrange[0]+delx/2.
ycen = state.p.plot1yrange[0]+dely/2.

case state.r.cursormode of 
    
    'XZoom': begin
        
        z = alog10(delabsx/delx)/alog10(2)
        if keyword_set(IN) then z = z+1 else z=z-1
        hwin = delabsx/2.^z/2.
        state.p.plot1xrange = [xcen-hwin,xcen+hwin]
        xmc_mergespec_plotupdate,state
        xmc_mergespec_setminmax,state
        
    end
    
    'YZoom': begin
        
        z = alog10(delabsy/dely)/alog10(2)
        if keyword_set(IN) then z = z+1 else z=z-1
        hwin = delabsy/2.^z/2.
        state.p.plot1yrange = [ycen-hwin,ycen+hwin]
        xmc_mergespec_plotupdate,state
        xmc_mergespec_setminmax,state
        
    end
        
    'Zoom': begin
        
        z = alog10(delabsx/delx)/alog10(2)
        if keyword_set(IN) then z = z+1 else z=z-1
        hwin = delabsx/2.^z/2.
        state.p.plot1xrange = [xcen-hwin,xcen+hwin]
        
        z = alog10(delabsy/dely)/alog10(2)
        if keyword_set(IN) then z = z+1 else z=z-1
        hwin = delabsy/2.^z/2.
        state.p.plot1yrange = [ycen-hwin,ycen+hwin]
        
        xmc_mergespec_plotupdate,state
        xmc_mergespec_setminmax,state
        
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
pro xmc_mergespec_event, event

;  Check to see if it is the help file 'Done' Button

widget_control, event.id,  GET_UVALUE = uvalue

if uvalue eq 'Quit' then begin

    widget_control, event.top, /DESTROY
    goto, getout

endif

;  Must be a main widget event

widget_control, event.top, GET_UVALUE = state, /NO_COPY

case uvalue of
    
   'Add Spectrum': begin
        
        obj = dialog_pickfile(DIALOG_PARENT=state.w.xmc_mergespec_base,$
                              /MUST_EXIST,PATH=state.r.addpath,GET_PATH=path,$
                              FILTER='*.fits')
        if obj eq '' then goto, cont
        widget_control,state.w.addspec_fld[1],SET_VALUE = strtrim(obj,2)
        mc_setfocus,state.w.addspec_fld
        state.r.addpath = path
        
    end

    'Auto Scale': begin

        if state.w.freeze then goto, cont
        xmc_mergespec_autoscale,state

    end
    
    'Anchor Spectrum': begin
        
        obj = dialog_pickfile(DIALOG_PARENT=state.w.xmc_mergespec_base,$
                              /MUST_EXIST,PATH=state.r.anchorpath,$
                              GET_PATH=path,FILTER='*.fits')
        if obj eq '' then goto, cont
        widget_control,state.w.anchorspec_fld[1],SET_VALUE = strtrim(obj,2)
        mc_setfocus,state.w.anchorspec_fld
        state.r.anchorpath = path
        
    end
    
    'Keyboard': begin
        
        if state.w.freeze then goto, cont
        case strtrim(event.ch,2) of 
            
            'c': begin
                
                state.r.cursormode = 'None'
                state.r.trimspec = 'None'
                state.p.reg = !values.f_nan
                xmc_mergespec_plotupdate,state
                
            end
            
            'b': begin

                state.r.cursormode = 'None'
                state.r.trimspec = 'Blue'
            
            end

            'i': xmc_mergespec_zoom,state,/IN

            'o': xmc_mergespec_zoom,state,/OUT

            'l': begin
                
                if state.r.trimspec eq 'Blue' then begin
                    
                    state.r.trimdirblue='Left'
                    widget_control,state.w.trimdir_bg,SET_VALUE=0
                    state.r.trimblue = !values.f_nan
                    
                endif

                if state.r.trimspec eq 'Green' then begin

                    state.r.trimdirgreen='Left'
                    widget_control,state.w.trimdir_bg,SET_VALUE=0
                    state.r.trimgreen = !values.f_nan

                endif
                xmc_mergespec_cutscalespec,state
                xmc_mergespec_plotupdate,state
                
            end

            'r': begin

                if state.r.trimspec eq 'Blue' then begin

                    state.r.trimdirblue='Right'
                    widget_control,state.w.trimdir_bg,SET_VALUE=1
                    state.r.trimblue = !values.f_nan
                    
                endif

                if state.r.trimspec eq 'Green' then begin

                    state.r.trimdirgreen='Right'
                    widget_control,state.w.trimdir_bg,SET_VALUE=1
                    state.r.trimgreen = !values.f_nan

                endif
                xmc_mergespec_cutscalespec,state
                xmc_mergespec_plotupdate,state
                
            end

            
            's': begin
                
                state.r.cursormode = 'Select'
                state.p.reg = !values.f_nan
                
            end
            
            'g': begin

                state.r.cursormode = 'None'
                state.r.trimspec = 'Green'

            end

            'w': begin
               
                if state.p.plotwin eq 1 then begin

                    state.p.plot1xrange = state.p.plot1absxrange
                    state.p.plot1yrange = state.p.plot1absyrange

                endif else state.p.plot2yrange = state.p.plot2absyrange

                xmc_mergespec_plotupdate,state
                xmc_mergespec_setminmax,state
                
            end

            'u': begin
                    
                if state.r.trimspec eq 'Blue' then $
                  state.r.trimblue = !values.f_nan
                if state.r.trimspec eq 'Green' then $
                  state.r.trimgreen = !values.f_nan
                
                xmc_mergespec_cutscalespec,state
                xmc_mergespec_plotupdate,state

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

    'Load Spectrum': xmc_mergespec_loadspec,state

    'Merge Spectra': begin

        if state.w.freeze then goto, cont
        xmc_mergespec_mergespec,state

    end

    'Output Format': begin

        if event.value eq 'Text' then state.r.textoutput=event.select
        if event.value eq 'FITS' then state.r.fitsoutput=event.select

    end

    'Plot Spectrum': begin

        if state.w.freeze then goto, cont
        if event.value eq 'Blue' then state.p.plotbluespec = event.select
        if event.value eq 'Green' then state.p.plotgreenspec = event.select
        xmc_mergespec_plotupdate,state

    end

    'Plot Type': begin

        if state.w.freeze then goto, cont
        state.p.plottype = event.value
        if state.p.plottype eq 'Combine' then xmc_mergespec_combinespec,state
        xmc_mergespec_plotupdate,state

    end

    'Scale Spectrum': begin

        if state.w.freeze then goto, cont
        val = cfld(state.w.scale_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then return
        state.r.scale = val
        state.r.scalereg = !values.f_nan
        xmc_mergespec_cutscalespec,state
        xmc_mergespec_plotupdate,state

    end

    'Trim Direction': begin

        if state.r.trimspec eq 'Blue' then begin

            state.r.trimdirblue=strtrim(event.value,2)
            state.r.trimblue = !values.f_nan

        endif
        if state.r.trimspec eq 'Green' then begin

            state.r.trimdirgreen=strtrim(event.value,2)
            state.r.trimgreen = !values.f_nan        

        endif
        if state.w.freeze then goto, cont
        xmc_mergespec_cutscalespec,state
        xmc_mergespec_plotupdate,state

    end
    'Weighted Mean': state.r.weighted = event.select
    
    'Write File': begin

        if state.w.freeze then goto, cont
        xmc_mergespec_writefile,state
        
    end

    else:
    
endcase

cont: 

widget_control, state.w.xmc_mergespec_base, SET_UVALUE=state, /NO_COPY
getout:

end
;
;******************************************************************************
;
pro xmc_mergespec_minmax,event

widget_control, event.top, GET_UVALUE = state, /NO_COPY
widget_control, event.id,  GET_UVALUE = uvalue

case uvalue of 

    'X Min': begin

        xmin = cfld(state.w.xmin_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then goto, cont
        xmin2 = crange(xmin,state.p.plot1xrange[1],'X Min',/KLT,$
                       WIDGET_ID=state.w.xmc_mergespec_base,CANCEL=cancel)
        if cancel then begin
            
            widget_control, state.w.xmin_fld[0],$
              SET_VALUE=state.p.plot1xrange[0]
            goto, cont
            
        endif else state.p.plot1xrange[0] = xmin2

    end
    'X Max': begin

        xmax = cfld(state.w.xmax_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then goto, cont
        xmax2 = crange(xmax,state.p.plot1xrange[0],'X Max',/KGT,$
                       WIDGET_ID=state.w.xmc_mergespec_base,CANCEL=cancel)
        if cancel then begin
            
            widget_control, state.w.xmax_fld[0],$
              SET_VALUE=state.p.plot1xrange[1]
            goto, cont
            
        endif else state.p.plot1xrange[1] = xmax2

    end
    'Y1 Min': begin

        ymin = cfld(state.w.ymin1_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then goto, cont
        ymin2 = crange(ymin,state.p.plot1yrange[1],'Y Min',/KLT,$
                       WIDGET_ID=state.w.xmc_mergespec_base,CANCEL=cancel)
        if cancel then begin
            
            widget_control,state.w.ymin1_fld[0],set_val=state.p.plot1yrange[0]
            goto, cont
            
        endif else state.p.plot1yrange[0] = ymin2
        
    end
    'Y1 Max': begin

        ymax = cfld(state.w.ymax1_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then return
        ymax2 = crange(ymax,state.p.plot1yrange[0],'Y Max',/KGT,$
                       WIDGET_ID=state.w.xmc_mergespec_base,CANCEL=cancel)
        if cancel then begin
            
            widget_control,state.w.ymax1_fld[0],set_val=state.p.plot1yrange[1]
            return
            
        endif else state.p.plot1yrange[1] = ymax2
        
    end
    
endcase

xmc_mergespec_plotupdate,state
cont:

widget_control, state.w.xmc_mergespec_base, SET_UVALUE=state, /NO_COPY

end
;
;******************************************************************************
;
pro xmc_mergespec_plotwinevent1, event

widget_control, event.top, GET_UVALUE = state, /NO_COPY
widget_control, event.id,  GET_UVALUE = uvalue

;  Check to see if it is a TRACKING event.

if strtrim(tag_names(event,/STRUCTURE_NAME),2) eq 'WIDGET_TRACKING' then begin

    if event.enter eq 0 then widget_control, state.w.keyboard, SENSITIVE=0
    wset, state.p.plotwin1_wid
    device, copy=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
                  state.p.pixmap1_wid]

    state.p.plotwin = 1    
    goto, cont

endif

;  If not, set the keyboard focus and active window.

widget_control, state.w.keyboard, /INPUT_FOCUS, /SENSITIVE

!p = state.p.pscale1
!x = state.p.xscale1
!y = state.p.yscale1
x  = event.x/float(state.p.plot1size[0])
y  = event.y/float(state.p.plot1size[1])
xy = convert_coord(x,y,/NORMAL,/TO_DATA)


if event.type eq 1 then begin
    
    case state.r.cursormode of 

        'Select': begin
            
            z = where(finite(state.p.reg) eq 1,count)
            if count eq 0 then state.p.reg[*,0] = xy[0:1] else begin 
                
                state.p.reg[*,1] = xy[0:1]
                
            endelse
            xmc_mergespec_plotupdate,state
            
        end

        'XZoom': begin

            z = where(finite(state.p.reg) eq 1,count)
            if count eq 0 then begin
                
                state.p.reg[*,0] = xy[0:1]
                wset, state.p.pixmap1_wid
                plots, [event.x,event.x],[0,state.p.plot1size[1]],color=2,$
                  /DEVICE,linestyle=1,thick=2
                wset, state.p.plotwin1_wid
                device, copy=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,$
                              0,state.p.pixmap1_wid]
               
            endif else begin

                state.p.reg[*,1] = xy[0:1]
                state.p.plot1xrange = [min(state.p.reg[0,*],max=m),m]
                state.r.cursormode = 'None'
                state.p.reg = !values.f_nan
                xmc_mergespec_plotupdate,state
                xmc_mergespec_setminmax,state
                
            endelse


        end

        'YZoom': begin

            z = where(finite(state.p.reg) eq 1,count)
            if count eq 0 then begin
                
                state.p.reg[*,0] = xy[0:1]
                wset, state.p.pixmap1_wid
                plots, [0,state.p.plot1size[0]],[event.y,event.y],color=2,$
                  /DEVICE,linestyle=1,thick=2

                wset, state.p.plotwin1_wid
                device, copy=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,$
                              0,state.p.pixmap1_wid]
               
            endif else begin

                state.p.reg[*,1] = xy[0:1]
                state.p.plot1yrange = [min(state.p.reg[1,*],max=m),m]
                state.r.cursormode = 'None'
                state.p.reg = !values.f_nan
                xmc_mergespec_plotupdate,state
                xmc_mergespec_setminmax,state
                
            endelse

        end
        
        'Zoom': begin
            
            z = where(finite(state.p.reg) eq 1,count)
            if count eq 0 then state.p.reg[*,0] = xy[0:1] else begin 
                
                state.p.reg[*,1] = xy[0:1]
                state.p.plot1xrange   = [min(state.p.reg[0,*],max=max),max]
                state.p.plot1yrange   = [min(state.p.reg[1,*],max=max),max]
                xmc_mergespec_plotupdate,state
                xmc_mergespec_setminmax,state
                state.r.cursormode   = 'None'
                state.p.reg  = !values.f_nan
                
            endelse
            
        end
        
        else: begin

            if state.r.trimspec ne 'None' and event.release le 1 then begin
                    
                if state.r.trimspec eq 'Blue' then $
                  state.r.trimblue = xy[0]
                if state.r.trimspec eq 'Green' then $
                  state.r.trimgreen = xy[0]
                
            endif 
            
            xmc_mergespec_cutscalespec,state
            xmc_mergespec_plotupdate,state
          
        end
        
    endcase
    
endif

;  Copy the pixmaps and draw the lines.


wset, state.p.plotwin1_wid
device, copy=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
              state.p.pixmap1_wid]

wset, state.p.plotwin1_wid
case state.r.cursormode of 

    'XZoom': plots, [event.x,event.x],[0,state.p.plot1size[1]],color=2,/DEVICE

    'YZoom': plots, [0,state.p.plot1size[0]],[event.y,event.y],color=2,/DEVICE

    'Zoom': begin

        plots, [event.x,event.x],[0,state.p.plot1size[1]],color=2,/DEVICE
        plots, [0,state.p.plot1size[0]],[event.y,event.y],color=2,/DEVICE
        xy = convert_coord(event.x,event.y,/DEVICE,/TO_DATA)
        plots,[state.p.reg[0,0],state.p.reg[0,0]],[state.p.reg[1,0],xy[1]],$
          linestyle=2,color=2
        plots, [state.p.reg[0,0],xy[0]],[state.p.reg[1,0],state.p.reg[1,0]],$
          linestyle=2,color=2
        
    end

    else: begin

        plots, [event.x,event.x],[0,state.p.plot1size[1]],color=2,/DEVICE
        plots, [0,state.p.plot1size[0]],[event.y,event.y],color=2,/DEVICE

    end

endcase

;  Update the cursor tracking

if not state.w.freeze then begin

    label = 'Cursor X: '+strtrim(xy[0],2)
    widget_control,state.w.message,set_value=label

endif

cont:

widget_control, state.w.xmc_mergespec_base, SET_UVALUE=state, /NO_COPY


end
;
;******************************************************************************
;
pro xmc_mergespec_resize, event

widget_control, event.top, GET_UVALUE = state, /NO_COPY
widget_control, event.id,  GET_UVALUE = uvalue

widget_control, state.w.xmc_mergespec_base, TLB_GET_SIZE=size

;  Window 1

state.p.plot1size[0]=size[0]-state.p.buffer[0]
state.p.plot1size[1]=(size[1]-state.p.buffer[1])*state.p.plot1scale

widget_control, state.w.plotwin1, DRAW_XSIZE=state.p.plot1size[0]
widget_control, state.w.plotwin1, DRAW_YSIZE=state.p.plot1size[1]

wdelete,state.p.pixmap1_wid
window, /FREE, /PIXMAP,XSIZE=state.p.plot1size[0],YSIZE=state.p.plot1size[1]
state.p.pixmap1_wid = !d.window

wset, state.p.plotwin1_wid
device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
              state.p.pixmap1_wid]

if not state.w.freeze then xmc_mergespec_plotupdate,state

widget_control, state.w.xmc_mergespec_base, SET_UVALUE=state, /NO_COPY

end
;
;******************************************************************************
;
;-------------------------------Main Program----------------------------------
;
;******************************************************************************
;
pro xmc_mergespec

;  Load color table and get fonts

mkct
mc_getfonts,buttonfont,textfont

;  Build three structures which will hold important info.
;  w - contains info pertaining to widget operations.
;  r - contains info pertaining to the reduction process.
;  d - contains all of the data.

w = {addspec_fld:[0L,0L],$
     anchorspec_fld:[0L,0L],$
     buttonfont:buttonfont,$
     freeze:1,$
     history_fld:[0L,0L],$
     keyboard:0L,$
     message:0L,$
     oname_fld:[0L,0L],$
     plotspec_bg:0L,$
     plottype_bg:0L,$
     plotwin1:0,$
     scale_fld:[0L,0L],$
     textfont:textfont,$
     trimdir_bg:0L,$
     xmc_mergespec_base:0L,$
     xmin_fld:[0L,0L],$
     xmax_fld:[0L,0L],$
     ymin1_fld:[0L,0L],$
     ymax1_fld:[0L,0L]}

r = {addpath:'',$
     anchorpath:'',$
     fitsoutput:1,$
     greenpos:'',$
     cursormode:'None',$
     mergedorders:ptr_new(2),$
     naps:1,$
     norders:1,$
     overlap:[0.,0.],$
     scale:1.,$
     scalereg:[!values.f_nan,!values.f_nan],$
     textoutput:0,$
     trimblue:!values.f_nan,$
     trimdirblue:'Left',$
     trimdirgreen:'Right',$
     trimgreen:!values.f_nan,$
     trimspec:'Green',$
     weighted:0}

d = {addspec:ptr_new(2),$
     bluespec:ptr_new(2),$
     greenspec:ptr_new(2),$
     hdr:ptr_new(2),$
     mergedspec:ptr_new(2),$
     origspec:ptr_new(2),$
     tempmergedspec:ptr_new(2)}

p = {activespec:1,$
     buffer:[0.,0.],$
     pixmap1_wid:0L,$
     plotwin:1,$
     plotwin1_wid:0L,$
     plot1absxrange:[0.,0.],$
     plot1absyrange:[0.,0.],$
     plot1scale:0.0,$
     plot1xrange:[0.,0.],$
     plot1yrange:[0.,0.],$
     plot1size:[670,500],$
     plotbluespec:1,$
     plottype:'Overlap',$
     plotgreenspec:1,$
     title:'',$
     pscale1:!p,$
     xscale1:!x,$
     xunits:'',$
     yscale1:!y,$
     yunits:'',$
     reg:[[!values.f_nan,!values.f_nan],$
          [!values.f_nan,!values.f_nan]]}

p.plot1scale = float(p.plot1size[1])/(p.plot1size[1])

;  Load the three structures in the state structure.

state = {w:w,r:r,d:d,p:p}

state.w.xmc_mergespec_base = widget_base(title='Xmc_Mergespec', $
                                      /COLUMN,$
                                      /TLB_SIZE_EVENTS)

   quit_button = widget_button(state.w.xmc_mergespec_base,$
                               FONT=state.w.buttonfont,$
                               EVENT_PRO='xmc_mergespec_event',$
                               VALUE='Quit',$
                               UVALUE='Quit')
   
   state.w.keyboard = widget_text(state.w.xmc_mergespec_base, $
                                  /ALL_EVENTS, $
                                  SCR_XSIZE=1, $
                                  SCR_YSIZE=1, $
                                  UVALUE='Keyboard', $
                                  EVENT_PRO='xmc_mergespec_event',$
                                  VALUE='')
   
   row_base = widget_base(state.w.xmc_mergespec_base,$
                          /ROW)

      col1_base = widget_base(row_base,$
                              EVENT_PRO='xmc_mergespec_event',$
                              /COLUMN)
      
         box1_base = widget_base(col1_base,$
                                 /COLUMN,$
                                 /FRAME)
         
            label = widget_label(box1_base,$
                                 VALUE='1.  Load Spectra',$
                                 FONT=state.w.buttonfont,$
                                 /ALIGN_LEFT)
               
            row = widget_base(box1_base,$
                              /ROW,$
                              /BASE_ALIGN_CENTER)
            
               input = widget_button(row,$
                                     FONT=state.w.buttonfont,$
                                     VALUE='Anchor Spectrum',$
                                     UVALUE='Anchor Spectrum',$
                                     EVENT_PRO='xmc_mergespec_event')
               
               
               input_fld = coyote_field2(row,$
                                         LABELFONT=state.w.buttonfont,$
                                         FIELDFONT=state.w.textfont,$
                                         TITLE=':',$
                                         UVALUE='Anchor Spectrum Field',$
                                         XSIZE=20,$
                                         EVENT_PRO='xmc_mergespec_event',$
                                         /CR_ONLY,$
                                         TEXTID=textid)
               state.w.anchorspec_fld = [input_fld,textid]

            row = widget_base(box1_base,$
                              /ROW,$
                              /BASE_ALIGN_CENTER)
            
               input = widget_button(row,$
                                     FONT=state.w.buttonfont,$
                                     VALUE='Add Spectrum',$
                                     UVALUE='Add Spectrum',$
                                     EVENT_PRO='xmc_mergespec_event')
               
               
               input_fld = coyote_field2(row,$
                                         LABELFONT=state.w.buttonfont,$
                                         FIELDFONT=state.w.textfont,$
                                         TITLE=':',$
                                         UVALUE='Add Spectrum Field',$
                                         XSIZE=20,$
                                         EVENT_PRO='xmc_mergespec_event',$
                                         /CR_ONLY,$
                                         TEXTID=textid)
               state.w.addspec_fld = [input_fld,textid]

               bg = cw_bgroup(box1_base,$
                              ['Weighted Mean'],$
                              FONT=state.w.buttonfont,$
                              UVALUE='Weighted Mean',$
                              SET_VALUE=state.r.weighted,$
                              /NONEXCLUSIVE)

            load = widget_button(box1_base,$
                                 VALUE='Load Spectrum',$
                                 UVALUE='Load Spectrum',$
                                 FONT=state.w.buttonfont)
            
         box3_base = widget_base(col1_base,$
                                 /COLUMN,$
                                 FRAME=1)

            label = widget_label(box3_base,$
                                 VALUE='3.  Cut/Scale/Merge Spectra',$
                                 FONT=state.w.buttonfont,$
                                 /ALIGN_LEFT)

            row = widget_base(box3_base,$
                              /BASE_ALIGN_CENTER,$
                              /ROW)
               
               scale = coyote_field2(row,$
                                     LABELFONT=state.w.buttonfont,$
                                     FIELDFONT=state.w.textfont,$
                                     TITLE='Scale:',$
                                     UVALUE='Scale Spectrum',$
                                     XSIZE=12,$
                                     VALUE=state.r.scale,$
                                     EVENT_PRO='xmc_mergespec_event',$
                                     /CR_ONLY,$
                                     TEXTID=textid)
               state.w.scale_fld = [scale,textid]
               
               widget_button = widget_button(row,$
                                             VALUE='Auto Scale',$
                                             UVALUE='Auto Scale',$
                                             FONT=state.w.buttonfont)
            
            state.w.plotspec_bg = cw_bgroup(box3_base,$
                                            FONT=state.w.buttonfont,$
                                            ['Blue','Green'],$
                                            /ROW,$
                                            /RETURN_NAME,$
                                            /NONEXCLUSIVE,$
                                            LABEL_LEFT='Plot Spectrum:',$
                                            UVALUE='Plot Spectrum',$
                                            SET_VALUE=[1,1])

            state.w.trimdir_bg = cw_bgroup(box3_base,$
                                           FONT=state.w.buttonfont,$
                                           ['Left','Right'],$
                                           /ROW,$
                                           /RETURN_NAME,$
                                           /EXCLUSIVE,$
                                           LABEL_LEFT='Trim Direction:',$
                                           UVALUE='Trim Direction',$
                                           SET_VALUE=1)
            
            
            state.w.plottype_bg = cw_bgroup(box3_base,$
                                            FONT=state.w.buttonfont,$
                                            ['Overlap','Combine'],$
                                            /ROW,$
                                            /RETURN_NAME,$
                                            /NO_RELEASE,$
                                            /EXCLUSIVE,$
                                            LABEL_LEFT='Spec Type:',$
                                            UVALUE='Plot Type',$
                                            SET_VALUE=0)

         box4_base = widget_base(col1_base,$
                                 /COLUMN,$
                                 /FRAME)
         
            label = widget_label(box4_base,$
                                 VALUE='4.  Write Spectra to File',$
                                 FONT=state.w.buttonfont,$
                                 /ALIGN_LEFT)
            
            outformat_bg = cw_bgroup(box4_base,$
                                     FONT=state.w.buttonfont,$
                                     ['FITS','Text'],$
                                     /ROW,$
                                     /RETURN_NAME,$
                                     /NONEXCLUSIVE,$
                                     LABEL_LEFT='Output Format:',$
                                     UVALUE='Output Format',$
                                     SET_VALUE=[1,0])
            
            oname = coyote_field2(box4_base,$
                                  LABELFONT=state.w.buttonfont,$
                                  FIELDFONT=state.w.textfont,$
                                  TITLE='File Name:',$
                                  UVALUE='Object File Oname',$
                                  XSIZE=18,$
                                  TEXTID=textid)
            state.w.oname_fld = [oname,textid]

            fld = coyote_field2(box4_base,$
                                LABELFONT=state.w.buttonfont,$
                                FIELDFONT=state.w.textfont,$
                                TITLE='History:',$
                                UVALUE='History',$
                                XSIZE=32,$
                                TEXTID=textid)
            state.w.history_fld = [fld,textid]
            
            write = widget_button(box4_base,$
                                  VALUE='Write File',$
                                  UVALUE='Write File',$
                                  FONT=state.w.buttonfont)

          col2_base = widget_base(row_base,$
                                  /COLUMN)

             state.w.message = widget_text(col2_base, $
                                           VALUE='',$
                                           YSIZE=1)

             state.w.plotwin1 = widget_draw(col2_base,$
                                            XSIZE=state.p.plot1size[0],$
                                            YSIZE=state.p.plot1size[1],$
                                            /TRACKING_EVENTS,$
                                            /MOTION_EVENTS,$
                                      EVENT_PRO='xmc_mergespec_plotwinevent1',$
                                            UVALUE='Plot Window 1')            
 
            widget_button = widget_button(col2_base,$
                                          EVENT_PRO='xmc_mergespec_event',$
                                          VALUE='Merge Spectra',$
                                          UVALUE='Merge Spectra',$
                                          FONT=state.w.buttonfont)


             row_base = widget_base(col2_base,$
                                    /FRAME,$
                                    /ROW)
   
                xmin = coyote_field2(row_base,$
                                     LABELFONT=state.w.buttonfont,$
                                     FIELDFONT=state.w.textfont,$
                                     TITLE='X Min:',$
                                     UVALUE='X Min',$
                                     XSIZE=12,$
                                     EVENT_PRO='xmc_mergespec_minmax',$
                                     /CR_ONLY,$
                                     TEXTID=textid)
                state.w.xmin_fld = [xmin,textid]
                
                xmax = coyote_field2(row_base,$
                                     LABELFONT=state.w.buttonfont,$
                                     FIELDFONT=state.w.textfont,$
                                     TITLE='X Max:',$
                                     UVALUE='X Max',$
                                     XSIZE=12,$
                                     EVENT_PRO='xmc_mergespec_minmax',$
                                     /CR_ONLY,$
                                     TEXTID=textid)
                state.w.xmax_fld = [xmax,textid]
                
                ymin = coyote_field2(row_base,$
                                     LABELFONT=state.w.buttonfont,$
                                     FIELDFONT=state.w.textfont,$
                                     TITLE='Y Min:',$
                                     UVALUE='Y1 Min',$
                                     XSIZE=12,$
                                     EVENT_PRO='xmc_mergespec_minmax',$
                                     /CR_ONLY,$
                                     TEXTID=textid)
                state.w.ymin1_fld = [ymin,textid]
                
                ymax = coyote_field2(row_base,$
                                     LABELFONT=state.w.buttonfont,$
                                     FIELDFONT=state.w.textfont,$
                                     TITLE='Y Max:',$
                                     UVALUE='Y1 Max',$
                                     XSIZE=12,$
                                     EVENT_PRO='xmc_mergespec_minmax',$
                                     /CR_ONLY,$
                                     TEXTID=textid)
                state.w.ymax1_fld = [ymax,textid]

; Get things running.  Center the widget using the Fanning routine.
      
    centertlb,state.w.xmc_mergespec_base
      
    widget_control, state.w.xmc_mergespec_base, /REALIZE
    
    mkct
    
;  Get plotwin ids
    
    widget_control, state.w.plotwin1, GET_VALUE=x
    state.p.plotwin1_wid = x
    
    window, /FREE, /PIXMAP,XSIZE=state.p.plot1size[0],$
      YSIZE=state.p.plot1size[1]
    state.p.pixmap1_wid = !d.window
    
;  Get sizes for things.
    
    widget_geom = widget_info(state.w.xmc_mergespec_base, /GEOMETRY)
    state.p.buffer[0]=widget_geom.xsize-state.p.plot1size[0]
    state.p.buffer[1]=widget_geom.ysize-state.p.plot1size[1]

; Start the Event Loop. This will be a non-blocking program.
    
    XManager, 'xmc_mergespec', $
      state.w.xmc_mergespec_base, $
      CLEANUP='xmc_mergespec_cleanup',$
      EVENT_HANDLER='xmc_mergespec_resize',$
      /NO_BLOCK
    
; Put state variable into the user value of the top level base.

widget_control, state.w.xmc_mergespec_base, SET_UVALUE=state, /NO_COPY

cont:

end
