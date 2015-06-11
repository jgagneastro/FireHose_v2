;+
; NAME:
;     xmergexd
;
; PURPOSE:
;     Merges different orders from a SpeX spectra file.
;    
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     xmergexd
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
;     Write s SpeX spectral FITS file to disk
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
;     Scale and clip spectra from different orders and then combine
;     them together.
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2001       - Written M. Cushing, Institute for Astronomy, UH
;     2004-01-07 - Changed the cutting procedure and something other 
;                  cosmetic things.  Code just getting worse... 
;-

;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xmergexd_autoscale,state

state.r.scale = 1
xmergexd_cutscalespec,state

z = where(finite(state.p.reg) eq 0,count)

if count ne 0 then begin

    result=dialog_message("Please select a region using 's'.",$
                          /INFORMATION,DIALOG_PARENT=state.w.xmergexd_base)
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

interpspec,greenspec_w,greenspec_f,bluespec_w,ngreenspec_f,ngreenspec_e,$
  YAERROR=greenspec_e

z = where(finite(bluespec_f) eq 1 and finite(ngreenspec_f) eq 1 and $
          finite(bluespec_e) eq 1 and finite(ngreenspec_e) eq 1)

denom = bluespec_e^2 + ngreenspec_e^2

scale = total(ngreenspec_f[z]*bluespec_f[z]/denom[z])/$
  total(ngreenspec_f[z]^2/denom[z])

state.r.scale = scale
widget_control, state.w.scale_fld[1],SET_VALUE=strtrim(state.r.scale,2)

state.r.cursormode = 'None'

xmergexd_cutscalespec,state
xmergexd_plotupdate,state

end
;
;******************************************************************************
;
pro xmergexd_cleanup,base

widget_control, base, GET_UVALUE = state, /NO_COPY
if n_elements(state) ne 0 then begin


    ptr_free, state.d.addspec
    ptr_free, state.d.bluespec
    ptr_free, state.d.greenspec
    ptr_free, state.d.mergedspec
    ptr_free, state.d.tempmergedspec


endif

state = 0B

end
;
;******************************************************************************
;
pro xmergexd_combinespec,state

;  Fix this crappy program someday


gspec_w = (*state.d.greenspec)[*,0]
gspec_f = (*state.d.greenspec)[*,1]
gspec_e = (*state.d.greenspec)[*,2]

bspec_w = (*state.d.bluespec)[*,0]
bspec_f = (*state.d.bluespec)[*,1]
bspec_e = (*state.d.bluespec)[*,2]

;  Get position of greenspec with respect to bluespec

if min((*state.d.greenspec)[*,0]) lt min((*state.d.mergedspec)[*,0],/NAN) $
  then greenpos = 'Left' else $
  if max((*state.d.greenspec)[*,0]) gt max((*state.d.mergedspec)[*,0],/NAN) $
  then greenpos = 'Right' else greenpos = 'Inside'

if greenpos eq 'Right' then begin

    nleft = n_elements(bspec_w)
    zbm   = where(bspec_w gt gspec_w[0],count)

    if count eq 0 then begin   

        tempmergeorder_w = [bspec_w,gspec_w]
        tempmergeorder_f = [bspec_f[0:nleft-2],!values.f_nan,!values.f_nan,$
                            gspec_f[1:*]]
        tempmergeorder_e = [bspec_e[0:nleft-2],!values.f_nan,!values.f_nan,$
                            gspec_e[1:*]]

    endif else begin

        mbspec_w = bspec_w[[zbm[0]-1,zbm]]
        mbspec_f = bspec_f[[zbm[0]-1,zbm]]
        mbspec_e = bspec_e[[zbm[0]-1,zbm]]

        interpspec,mbspec_w,mbspec_f,gspec_w,nmbspec_f,nmbspec_e,$
          YAERROR=mbspec_e

        meancomb,[[gspec_f],[nmbspec_f]],olap_flux,olap_var,$
          DATAVAR=[[gspec_e^2],[nmbspec_e^2]],CANCEL=cancel
        
        nonan = where(finite(olap_flux) eq 1)
        nan   = where(finite(olap_flux) eq 0)


        tempmergeorder_w = [bspec_w[0:zbm[0]-1],gspec_w]
        tempmergeorder_f = [bspec_f[0:zbm[0]-1],olap_flux[nonan],gspec_f[nan]]
        tempmergeorder_e = [bspec_e[0:zbm[0]-1],sqrt(olap_var[nonan]),$
                            gspec_e[nan]]

    endelse

endif
if greenpos eq 'Left' then begin

    nleft = n_elements(gspec_w)
    zym   = where(gspec_w gt bspec_w[0],count)

    if count eq 0 then begin

        tempmergeorder_w = [gspec_w,bspec_w]
        tempmergeorder_f = [gspec_f[0:nleft-2],!values.f_nan,!values.f_nan,$
                            bspec_f[1:*]]
        tempmergeorder_e = [gspec_e[0:nleft-2],!values.f_nan,!values.f_nan,$
                            bspec_e[1:*]]

    endif else begin

        mgspec_w = gspec_w[[zym[0]-1,zym]]
        mgspec_f = gspec_f[[zym[0]-1,zym]]
        mgspec_e = gspec_e[[zym[0]-1,zym]]

        interpspec,mgspec_w,mgspec_f,bspec_w,nmgspec_f,nmgspec_e,$
          YAERROR=mgspec_e

        meancomb,[[bspec_f],[nmgspec_f]],olap_flux,olap_var,$
          DATAVAR=[[bspec_e^2],[nmgspec_e^2]],CANCEL=cancel
        
        nonan = where(finite(olap_flux) eq 1)
        nan   = where(finite(olap_flux) eq 0)


        tempmergeorder_w = [gspec_w[0:zym[0]-1],bspec_w]
        tempmergeorder_f = [gspec_f[0:zym[0]-1],olap_flux[nonan],bspec_f[nan]]
        tempmergeorder_e = [gspec_e[0:zym[0]-1],sqrt(olap_var[nonan]),$
                            bspec_e[nan]]

    endelse

endif
if greenpos eq 'Inside' then begin

;  Since this is for Order 08 ShortXD, we don't have to worry about 
;  which spectrum is sampled at a higher frequency.  

;  Now this isn't totally true since you might want to cut the 
;  spectrum reaching shorter wavelenghs (Higher order number) so that
;  the spectrum reaching longer wavelength (Lower order number) now
;  actually reaches the shortest wavelength.  That now dominates which
;  spectrum gets sampled onto which wavelength scale.

    if min(gspec_w) gt min(bspec_w) then begin

;  This is the case where the higher order fits entirely in the lower order

        interpspec,gspec_w,gspec_f,bspec_w,ngspec_f,ngspec_e,$
          YAERROR=gspec_e
        
        meancomb,[[bspec_f],[ngspec_f]],olap_flux,olap_var,$
          DATAVAR=[[bspec_e^2],[ngspec_e^2]],CANCEL=cancel
        
        nonan = where(finite(olap_flux) eq 1)
        
        tempmergeorder_w = bspec_w
        tempmergeorder_f = bspec_f & tempmergeorder_f[nonan] = olap_flux[nonan]
        tempmergeorder_e = bspec_e & tempmergeorder_e[nonan] = $
          sqrt(olap_var[nonan])
        
    endif else begin

;  This is the screwy case.  This is just like greenpos eq 'Left'

        nleft = n_elements(gspec_w)
        zym   = where(gspec_w gt bspec_w[0],count)
        
        if count eq 0 then begin
            
            tempmergeorder_w = [gspec_w,bspec_w]
            tempmergeorder_f = [gspec_f[0:nleft-1],!values.f_nan,$
                                !values.f_nan,$
                                bspec_f[1:*]]
            tempmergeorder_e = [gspec_e[0:nleft-1],!values.f_nan,$
                                !values.f_nan,$
                                bspec_e[1:*]]
            
        endif else begin
            
            mgspec_w = gspec_w[[zym[0]-1,zym]]
            mgspec_f = gspec_f[[zym[0]-1,zym]]
            mgspec_e = gspec_e[[zym[0]-1,zym]]
            
            interpspec,mgspec_w,mgspec_f,bspec_w,nmgspec_f,nmgspec_e,$
              YAERROR=mgspec_e
            
            meancomb,[[bspec_f],[nmgspec_f]],olap_flux,olap_var,$
              DATAVAR=[[bspec_e^2],[nmgspec_e^2]],CANCEL=cancel
            
            nonan = where(finite(olap_flux) eq 1)
            nan   = where(finite(olap_flux) eq 0)
            
            
            tempmergeorder_w = [gspec_w[0:zym[0]-1],bspec_w]
            tempmergeorder_f = [gspec_f[0:zym[0]-1],olap_flux[nonan],$
                                bspec_f[nan]]
            tempmergeorder_e = [gspec_e[0:zym[0]-1],sqrt(olap_var[nonan]),$
                                bspec_e[nan]]
                        
        endelse

    endelse


endif

*state.d.tempmergedspec = [[tempmergeorder_w],[tempmergeorder_f],$
                           [tempmergeorder_e]]

end
;
;******************************************************************************
;
pro xmergexd_cutscalespec,state


if finite(state.r.trim) eq 1 then begin

    if state.r.trimspec eq 'Green' then begin

        if state.r.trimdir eq 'Right' then begin

            z = where((*state.d.addspec)[*,0] lt state.r.trim)
            *state.d.greenspec = (*state.d.addspec)[z,*]
            (*state.d.greenspec)[*,1] = (*state.d.greenspec)[*,1]*state.r.scale
            if state.r.weighted then (*state.d.greenspec)[*,2] = $
              (*state.d.greenspec)[*,2]*sqrt(abs(state.r.scale))
            
        endif else begin
            
            z = where((*state.d.addspec)[*,0] gt state.r.trim)
            *state.d.greenspec = (*state.d.addspec)[z,*]
            (*state.d.greenspec)[*,1] = (*state.d.greenspec)[*,1]*state.r.scale
            if state.r.weighted then (*state.d.greenspec)[*,2] = $
              (*state.d.greenspec)[*,2]*sqrt(abs(state.r.scale))

        endelse

    endif

    if state.r.trimspec eq 'Blue' then begin

        if state.r.trimdir eq 'Right' then begin
            
            z = where((*state.d.mergedspec)[*,0] lt state.r.trim or $
                      finite((*state.d.mergedspec)[*,0]) eq 0)
            *state.d.bluespec = (*state.d.mergedspec)[z,*]
            
        endif else begin
            
            z = where((*state.d.mergedspec)[*,0] gt state.r.trim or $
                      finite((*state.d.mergedspec)[*,0]) eq 0)
            *state.d.bluespec = (*state.d.mergedspec)[z,*]
            
        endelse
        
    endif

endif else begin

    if state.r.trimspec eq 'Blue' then *state.d.bluespec=*state.d.mergedspec

    if state.r.trimspec eq 'Green' then begin

        *state.d.greenspec = reform(*state.d.addspec)   
        (*state.d.greenspec)[*,1] = (*state.d.greenspec)[*,1]*state.r.scale
        if state.r.weighted then (*state.d.greenspec)[*,2] = $
          (*state.d.greenspec)[*,2]*sqrt(abs(state.r.scale)) 

    endif

endelse

end
;
;******************************************************************************
;
pro xmergexd_getminmax1,state

;  Get X range

state.p.plot1xrange[1] = max((*state.d.longxdspec)[*,0])
state.p.plot1xrange[0] = min((*state.d.shortxdspec)[*,0])

;  Get Y range

z     = where(finite((*state.d.mergedspec)[*,0]) eq 1)
mspec = (*state.d.mergedspec)[z,*]
zb    = where(mspec[*,0] gt state.p.plot1xrange[0] and $
              mspec[*,0] lt state.p.plot1xrange[1])
specb = mspec[zb,1]

zy    = where((*state.d.greenspec)[*,0] gt state.p.plot1xrange[0] and $
              (*state.d.greenspec)[*,0] lt state.p.plot1xrange[1])
specy = (*state.d.greenspec)[zy,1]

fbmax = max(specb,/NAN,MIN=fbmin)
fymax = max(specy,/NAN,MIN=fymin)

state.p.plot1yrange = [(fbmin < fymin),(fbmax > fymax)]

state.p.plot1absxrange = state.p.plot1xrange
state.p.plot1absyrange = state.p.plot1yrange

end
;
;******************************************************************************
;
pro xmergexd_getminmax2,state

;  2nd plot window.

z     = where(finite((*state.d.mergedspec)[*,0]) eq 1)
mspec = (*state.d.mergedspec)[z,*]
zb    = where(mspec[*,0] gt state.p.plot1xrange[0] and $
              mspec[*,0] lt state.p.plot1xrange[1],countb)
specb = (countb ne 0) ? ( $
        *state.d.mergedspec)[zb,1]/(*state.d.mergedspec)[zb,2]:!values.f_nan


zg    = where((*state.d.greenspec)[*,0] gt state.p.plot1xrange[0] and $
              (*state.d.greenspec)[*,0] lt state.p.plot1xrange[1],county)
specg = (county ne 0) ? ( $
        *state.d.greenspec)[zg,1]/(*state.d.greenspec)[zg,2]:!values.f_nan


fbmax = max(specb,/NAN,MIN=fbmin)
fgmax = max(specg,/NAN,MIN=fgmin)

state.p.plot2yrange = [(fbmin < fgmin),(fbmax > fgmax)]

state.p.plot2absyrange = state.p.plot2yrange

end
;
;******************************************************************************
;
pro xmergexd_help,state

openr, lun, filepath('xmergexd_helpfile.txt',$
                     ROOT_DIR=state.r.packagepath,$
                     SUBDIR='helpfiles'),/GET_LUN
nlines = numlines(filepath('xmergexd_helpfile.txt',$
                           ROOT_DIR=state.r.packagepath,$
                           SUBDIR='helpfiles'))
array = strarr(nlines)
readf, lun, array
free_lun, lun

xmc_displaytext,array,TITLE='Xmergexd Help File',$
                GROUP_LEADER=state.w.xmergexd_base

end
;
;******************************************************************************
;
pro xmergexd_loadspec,state

;  Get files.

shortxd = cfld(state.w.shortxd_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
shortxd = cfile(shortxd,WIDGET_ID=state.w.xmergexd_base,CANCEL=cancel)
if cancel then return

longxd = cfld(state.w.longxd_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
longxd = cfile(longxd,WIDGET_ID=state.w.xmergexd_base,CANCEL=cancel)
if cancel then return

;  Read spectra.

readspec,shortxd,shortxd,shdr,obsmode,start,stop,norders,naps,orders,$
  xunits,yunits,slith_pix,slith_arc,slitw_pix,slitw_arc,airmass,xtitle,ytitle

readspec,longxd,longxd,lhdr,obsmode,start,stop,norders,naps,orders,$
  dxunits,dyunits,slith_pix,slith_arc,slitw_pix,slitw_arc,airmass

if not state.r.weighted then shortxd[*,2,*] = 1.
if not state.r.weighted then longxd[*,2,*] = 1.

;  Clip the NaNs at the beginning of the LongXD spectrum and at the
;  end of the ShortXD spectrum.

z = where(finite(longxd[*,1]) eq 1)
longxd = longxd[z[0]:*,*]

z = where(finite(shortxd[*,1]) eq 1,count)
shortxd = shortxd[0:z[count-1],*]

;  Save info

*state.d.shortxdspec = shortxd
*state.d.longxdspec  = longxd
*state.d.shortxdhdr  = shdr          
*state.d.longxdhdr   = lhdr   

if state.r.anchor eq 'ShortXD' then begin

    *state.d.bluespec   = shortxd
    *state.d.mergedspec = shortxd
    *state.d.greenspec  = longxd
    *state.d.addspec    = longxd
    state.p.greenpos    = 'Right'

endif else begin

    *state.d.bluespec   = longxd
    *state.d.mergedspec = longxd
    *state.d.greenspec  = shortxd
    *state.d.addspec    = shortxd
    state.p.greenpos    = 'Left'

endelse

state.p.xunits      = xunits
state.p.yunits      = yunits
state.p.xtitle      = xtitle
state.p.ytitle      = ytitle
state.r.scale       = 1.0
widget_control, state.w.scale_fld[1],SET_VALUE=strtrim(state.r.scale,2)

;  Unfreeze the widget

state.w.freeze = 0
widget_control, state.w.plotwin1, DRAW_BUTTON_EVENTS=1
widget_control, state.w.plotwin2, DRAW_BUTTON_EVENTS=1


state.p.title='!5Overlap - ShortXD and LongXD'

xmergexd_getminmax1,state
xmergexd_getminmax2,state
xmergexd_plotupdate,state
xmergexd_setminmax,state

end
;
;******************************************************************************
;
pro xmergexd_mergespec,state

xmergexd_combinespec,state
*state.d.bluespec   = reform(*state.d.tempmergedspec)
*state.d.mergedspec = reform(*state.d.tempmergedspec)

message = '!5XD spectra merged.'

wset, state.p.plotwin1_wid
erase
xyouts,0.5,0.5,message,/NORM,CHARSIZ=3,ALIGN=0.5
wset, state.p.pixmap1_wid
erase
xyouts,0.5,0.5,message,/NORM,CHARSIZ=3,ALIGN=0.5

wset, state.p.plotwin2_wid
erase
wset, state.p.pixmap2_wid
erase

end
;
;******************************************************************************
;
pro xmergexd_ploterror,state

if state.p.plottype eq 'Overlap' then begin
    
    plot, findgen(10),/XSTY,/YSTY,/NODATA,$
          XRANGE=state.p.plot1xrange,YRANGE=state.p.plot2yrange,$
          XTITLE=state.p.xtitle,YTITLE='!5S/N', $
          TITLE='!5Signal-to-Noise Spectra',CHARSIZE=mc_strsize('!5A',0.01)
    
    if state.p.front then begin

        if state.p.plotbluespec then oplot, (*state.d.bluespec)[*,0],$
          (*state.d.bluespec)[*,1]/(*state.d.bluespec)[*,2],COLOR=4,PSYM=10
        
        if state.p.plotgreenspec then oplot, (*state.d.greenspec)[*,0],$
          (*state.d.greenspec)[*,1]/(*state.d.greenspec)[*,2],COLOR=3,PSYM=10
    
    endif else begin

        if state.p.plotgreenspec then oplot, (*state.d.greenspec)[*,0],$
          (*state.d.greenspec)[*,1]/(*state.d.greenspec)[*,2],COLOR=3,PSYM=10

        if state.p.plotbluespec then oplot, (*state.d.bluespec)[*,0],$
          (*state.d.bluespec)[*,1]/(*state.d.bluespec)[*,2],COLOR=4,PSYM=10
         
    endelse

endif else begin
    
    plot, (*state.d.tempmergedspec)[*,0], $
          (*state.d.tempmergedspec)[*,1]/(*state.d.tempmergedspec)[*,2],$
          /XSTY,/YSTY,/NODATA,XRANGE=state.p.plot1xrange,$
          YRANGE=state.p.plot2yrange,XTITLE=state.p.xtitle,YTITLE='!5S/N',$
          TITLE='!5Signal-to-Noise Spectrum',CHARSIZE=mc_strsize('!5A',0.01)
    oplot,(*state.d.tempmergedspec)[*,0], $
          (*state.d.tempmergedspec)[*,1]/(*state.d.tempmergedspec)[*,2],$
          COLOR=7,PSYM=10
    
endelse

if state.r.cursormode eq 'Select' then begin

    if finite(state.p.reg[0,0]) eq 1 then $
      plots,[state.p.reg[0,0],state.p.reg[0,0]],!y.crange,COLOR=7,$
      LINESTYLE=2
    if finite(state.p.reg[0,1]) eq 1 then $
      plots,[state.p.reg[0,1],state.p.reg[0,1]],!y.crange,COLOR=7,$
      LINESTYLE=2
    
endif

state.p.pscale2 = !p
state.p.xscale2 = !x
state.p.yscale2 = !y

end
;
;******************************************************************************
;
pro xmergexd_plotflux,state

if state.p.plottype eq 'Overlap' then begin
    
    plot, findgen(10),/XSTY,/YSTY,/NODATA,$
          XRANGE=state.p.plot1xrange,YRANGE=state.p.plot1yrange,$
          XTITLE=state.p.xtitle,YTITLE=state.p.ytitle, $
          TITLE='!5Overlapping Spectra',CHARSIZE=mc_strsize('!5A',0.01)

    if state.p.front eq 0 then begin

        if state.p.plotbluespec then oplot, (*state.d.bluespec)[*,0],$
          (*state.d.bluespec)[*,1],COLOR=4,PSYM=10
        
        if state.p.plotgreenspec then oplot, (*state.d.greenspec)[*,0],$
          (*state.d.greenspec)[*,1],COLOR=3,PSYM=10
    
    endif else begin

        if state.p.plotgreenspec then oplot, (*state.d.greenspec)[*,0],$
          (*state.d.greenspec)[*,1],COLOR=3,PSYM=10

        if state.p.plotbluespec then oplot, (*state.d.bluespec)[*,0],$
          (*state.d.bluespec)[*,1],COLOR=4,PSYM=10
       
    endelse

endif else begin
    
    plot, (*state.d.tempmergedspec)[*,0],(*state.d.tempmergedspec)[*,1],$
      /XSTY,/YSTY,/NODATA,XRANGE=state.p.plot1xrange,$
      YRANGE=state.p.plot1yrange,XTITLE=state.p.xtitle,YTITLE=state.p.ytitle,$
      TITLE='!5Merged Spectrum',CHARSIZE=mc_strsize('!5A',0.01)
    
    oplot,(*state.d.tempmergedspec)[*,0],(*state.d.tempmergedspec)[*,1],$
      COLOR=7,PSYM=10
    
endelse

if state.r.cursormode eq 'Select' then begin
    
    if finite(state.p.reg[0,0]) eq 1 then $
      plots,[state.p.reg[0,0],state.p.reg[0,0]],!y.crange,COLOR=7,$
      LINESTYLE=2
    if finite(state.p.reg[0,1]) eq 1 then $
      plots,[state.p.reg[0,1],state.p.reg[0,1]],!y.crange,COLOR=7,$
      LINESTYLE=2

endif

state.p.pscale1 = !p
state.p.xscale1 = !x
state.p.yscale1 = !y

end
;
;******************************************************************************
;
pro xmergexd_plotupdate,state

wset, state.p.pixmap1_wid
erase
xmergexd_plotflux,state

wset, state.p.plotwin1_wid
device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
              state.p.pixmap1_wid]


wset, state.p.pixmap2_wid
erase 
xmergexd_ploterror,state

wset, state.p.plotwin2_wid
device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,0,$
              state.p.pixmap2_wid]

end
;
;******************************************************************************
;
pro xmergexd_setminmax,state

widget_control, state.w.xmin_fld[1],SET_VALUE=strtrim(state.p.plot1xrange[0],2)
widget_control, state.w.xmax_fld[1],SET_VALUE=strtrim(state.p.plot1xrange[1],2)
widget_control, state.w.ymin1_fld[1],$
  SET_VALUE=strtrim(state.p.plot1yrange[0],2)
widget_control, state.w.ymax1_fld[1],$
  SET_VALUE=strtrim(state.p.plot1yrange[1],2)

widget_control, state.w.ymin2_fld[1],$
  SET_VALUE=strtrim(state.p.plot2yrange[0],2)
widget_control, state.w.ymax2_fld[1],$
  SET_VALUE=strtrim(state.p.plot2yrange[1],2)

end
;
;******************************************************************************
;
pro xmergexd_writefile,state

shortxd = cfld(state.w.shortxd_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
shortxd = cfile(shortxd,WIDGET_ID=state.w.xmergexd_base,CANCEL=cancel)
if cancel then return

longxd = cfld(state.w.longxd_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
longxd = cfile(longxd,WIDGET_ID=state.w.xmergexd_base,CANCEL=cancel)
if cancel then return

shortxd = strmid(shortxd,strpos(shortxd,'/',/REVERSE_S)+1)
longxd  = strmid(longxd,strpos(longxd,'/',/REVERSE_S)+1)

ofile = cfld(state.w.oname_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

scale = cfld(state.w.scale_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return

if not state.r.weighted then (*state.d.mergedspec)[*,2] = 1.
start = 0
stop  = n_elements((*state.d.mergedspec)[*,1])-1

obj = fxpar(*state.d.shortxdhdr,'OBJECT')

fxhmake,hdr,*state.d.mergedspec
fxaddpar,hdr,'IRAFNAME',ofile
fxaddpar,hdr,'ORDERS','0',' Order numbers'
fxaddpar,hdr,'NORDERS',1, ' Number of orders'
fxaddpar,hdr,'NAPS',1, 'Number of apertures'
fxaddpar,hdr,'START',start, ' Array column of first FITS array column'
fxaddpar,hdr,'STOP',stop, ' Array column of first FITS array column'
fxaddpar,hdr,'OBJECT',obj

fxaddpar,hdr,'XUNITS',state.p.xunits, ' Units of the X axis'
fxaddpar,hdr,'YUNITS',state.p.yunits, ' Units of the Y axis'
fxaddpar,hdr,'XTITLE',state.p.xtitle, ' IDL X title'
fxaddpar,hdr,'YTITLE',state.p.ytitle, ' IDL Y title'

history = 'This spectrum was created using the ShortXD file '+$
  strtrim(shortxd,2)+' and the LongXD file '+strtrim(longxd,2)+'.'

scalee = (state.r.anchor eq 'ShortXD') ? 'LongXD':'ShortXD'

history = history+'  The '+strtrim(scalee,2)+' spectrum was scaled by '+$
  strtrim(scale,2)+'.'

history = mc_splittext(history,70)
sxaddhist,history,hdr

;  Get the ShortXD header

sxaddhist,' ',hdr
sxaddhist,'***HERE is the SXD History***',hdr
sxaddhist,' ',hdr
sxdhist = fxpar(*state.d.shortxdhdr,'HISTORY')
sxaddhist,sxdhist,hdr

sxaddhist,' ',hdr
sxaddhist,'***HERE is the LXD History***',hdr
sxaddhist,' ',hdr
lxdhist = fxpar(*state.d.longxdhdr,'HISTORY')
sxaddhist,lxdhist,hdr

writefits,ofile+'.fits',*state.d.mergedspec,hdr
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
pro xmergexd_zoom,state,IN=in,OUT=out

if state.p.plotwin eq 1 then begin

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
            xmergexd_plotupdate,state
            xmergexd_setminmax,state
            
        end
        
        'YZoom': begin
            
            z = alog10(delabsy/dely)/alog10(2)
            if keyword_set(IN) then z = z+1 else z=z-1
            hwin = delabsy/2.^z/2.
            state.p.plot1yrange = [ycen-hwin,ycen+hwin]
            xmergexd_plotupdate,state
            xmergexd_setminmax,state
            
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
            
            xmergexd_plotupdate,state
            xmergexd_setminmax,state
            
        end
        
        else:
        
    endcase

endif else begin

    delabsy = state.p.plot2absyrange[1]-state.p.plot2absyrange[0]
    dely    = state.p.plot2yrange[1]-state.p.plot2yrange[0]
    
    ycen = state.p.plot2yrange[0]+dely/2.

    case state.r.cursormode of 
        
        'YZoom': begin
            
            z = alog10(delabsy/dely)/alog10(2)
            if keyword_set(IN) then z = z+1 else z=z-1
            hwin = delabsy/2.^z/2.
            state.p.plot2yrange = [ycen-hwin,ycen+hwin]
            xmergexd_plotupdate,state
            xmergexd_setminmax,state
            
        end

        else:

    endcase

endelse

end
;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro xmergexd_event, event

;  Check to see if it is the help file 'Done' Button

widget_control, event.id,  GET_UVALUE = uvalue

if uvalue eq 'Quit' then begin

    widget_control, event.top, /DESTROY
    goto, getout

endif

;  Must be a main widget event

widget_control, event.top, GET_UVALUE = state, /NO_COPY

case uvalue of
    
    'Anchor': begin

        state.r.anchor = event.value
        xmergexd_loadspec,state

    end

    'Auto Scale': begin

        if state.w.freeze then goto, cont
        xmergexd_autoscale,state

    end

    'Help': xmergexd_help,state
    
    'Keyboard': begin
        
        if state.w.freeze then goto, cont
        case strtrim(event.ch,2) of 
            
            'b': begin

                state.r.trimspec = 'Blue'
                widget_control, state.w.trimspec_bg, SET_VALUE=1

            end

            'c': begin
                
                state.r.cursormode = 'None'
                state.p.reg = !values.f_nan
                xmergexd_plotupdate,state
                state.r.trim = !values.f_nan

                
            end

           'l': begin

                state.r.cursormode = 'Cut'
                state.r.trimdir = 'Left'
                state.r.trim=!values.f_nan
                widget_control, state.w.trimdir_bg, SET_VALUE=1
                
            end

            'i': xmergexd_zoom,state,/IN

            'o': xmergexd_zoom,state,/OUT
            
            's': begin
                
                state.r.cursormode = 'Select'
                state.p.reg = !values.f_nan
                
            end
            
            'g': begin

                state.r.trimspec = 'Green'
                widget_control, state.w.trimspec_bg, SET_VALUE=0

            end

            'f': begin

                state.p.front = (state.p.front eq 1) ? 0:1

                print, state.p.front
                xmergexd_plotupdate,state

            end

            'r': begin

                state.r.cursormode = 'Cut'
                state.r.trimdir = 'Right'
                state.r.trim=!values.f_nan
                widget_control, state.w.trimdir_bg, SET_VALUE=0
                
            end

            'w': begin
               
                if state.p.plotwin eq 1 then begin

                    state.p.plot1xrange = state.p.plot1absxrange
                    state.p.plot1yrange = state.p.plot1absyrange

                endif else state.p.plot2yrange = state.p.plot2absyrange

                xmergexd_plotupdate,state
                xmergexd_setminmax,state
                
            end

            'u': begin
                    
                state.r.trim = !values.f_nan
                state.r.cursormode='None'
                xmergexd_cutscalespec,state
                xmergexd_plotupdate,state

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

    'Load Spectrum': xmergexd_loadspec,state

    'LongXD': begin
        
        obj = dialog_pickfile(DIALOG_PARENT=state.w.xmergexd_base,$
                              PATH=state.r.longxdpath,GET_PATH=path,$
                              /MUST_EXIST,FILTER='*.fits')
        if obj eq '' then goto, cont
        state.r.longxdpath = path
        widget_control,state.w.longxd_fld[1],SET_VALUE=strtrim(obj,2)
        setfocus,state.w.longxd_fld
        

    end

    'Merge Orders': begin

        if state.w.freeze then goto, cont
        xmergexd_mergespec,state

    end

    'Output Format': state.r.textoutput=event.select
    
    'Plot Spectrum': begin

        if state.w.freeze then goto, cont
        if event.value eq 'Blue' then state.p.plotbluespec = event.select
        if event.value eq 'Green' then state.p.plotgreenspec = event.select
        xmergexd_plotupdate,state

    end

    'Plot Type': begin

        if state.w.freeze then goto, cont
        state.p.plottype = event.value
        if state.p.plottype eq 'Combine' then xmergexd_combinespec,state
        xmergexd_plotupdate,state

    end

    'Scale Spectrum': begin

        if state.w.freeze then goto, cont
        val = cfld(state.w.scale_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then return
        state.r.scale = val
        xmergexd_cutscalespec,state
        xmergexd_plotupdate,state

    end

    'ShortXD': begin
        
        obj = dialog_pickfile(DIALOG_PARENT=state.w.xmergexd_base,$
                              PATH=state.r.shortxdpath,GET_PATH=path,$
                              /MUST_EXIST,FILTER='*.fits')
        if obj eq '' then goto, cont
        state.r.shortxdpath = path
        widget_control,state.w.shortxd_fld[1],SET_VALUE=strtrim(obj,2)
        setfocus,state.w.shortxd_fld
        
    end

    'Trim Direction': state.r.trimdir = event.value

    'Trim Spectrum': state.r.trimspec = event.value

    'Weighted Mean': state.r.weighted = event.select
    
    'Write File': begin

        if state.w.freeze then goto, cont
        xmergexd_writefile,state
        
    end

    else:
    
endcase

cont: 

widget_control, state.w.xmergexd_base, SET_UVALUE=state, /NO_COPY
getout:

end
;
;******************************************************************************
;
pro xmergexd_minmax,event

widget_control, event.top, GET_UVALUE = state, /NO_COPY
widget_control, event.id,  GET_UVALUE = uvalue

if state.w.freeze then goto, cont
case uvalue of 

    'X Min': begin

        xmin = cfld(state.w.xmin_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then goto, cont
        xmin2 = crange(xmin,state.p.plot1xrange[1],'X Min',/KLT,$
                       WIDGET_ID=state.w.xmergexd_base,CANCEL=cancel)
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
                       WIDGET_ID=state.w.xmergexd_base,CANCEL=cancel)
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
                       WIDGET_ID=state.w.xmergexd_base,CANCEL=cancel)
        if cancel then begin
            
            widget_control,state.w.ymin1_fld[0],$
              SET_VALUE=state.p.plot1yrange[0]
            goto, cont
            
        endif else state.p.plot1yrange[0] = ymin2
        
    end
    'Y1 Max': begin

        ymax = cfld(state.w.ymax1_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then return
        ymax2 = crange(ymax,state.p.plot1yrange[0],'Y Max',/KGT,$
                       WIDGET_ID=state.w.xmergexd_base,CANCEL=cancel)
        if cancel then begin
            
            widget_control,state.w.ymax1_fld[0],$
              SET_VALUE=state.p.plot1yrange[1]
            return
            
        endif else state.p.plot1yrange[1] = ymax2
        
    end
    'Y2 Min': begin

        ymin = cfld(state.w.ymin2_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then goto, cont
        ymin2 = crange(ymin,state.p.plot2yrange[1],'Y Min',/KLT,$
                       WIDGET_ID=state.w.xmergexd_base,CANCEL=cancel)
        if cancel then begin
            
            widget_control,state.w.ymin2_fld[0],$
              SET_VALUE=state.p.plot2yrange[0]
            goto, cont
            
        endif else state.p.plot2yrange[0] = ymin2
        
    end
    'Y2 Max': begin

        ymax = cfld(state.w.ymax2_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then goto, cont
        ymax2 = crange(ymax,state.p.plot2yrange[0],'Y Max',/KGT,$
                       WIDGET_ID=state.w.xmergexd_base,CANCEL=cancel)
        if cancel then begin
            
            widget_control,state.w.ymax2_fld[0],$
              SET_VALUE=state.p.plot2yrange[1]
            goto, cont
            
        endif else state.p.plot2yrange[1] = ymax2
        
    end
    
endcase

xmergexd_plotupdate,state
cont:

widget_control, state.w.xmergexd_base, SET_UVALUE=state, /NO_COPY

end
;
;******************************************************************************
;
pro xmergexd_plotwinevent1, event

widget_control, event.top, GET_UVALUE = state, /NO_COPY
widget_control, event.id,  GET_UVALUE = uvalue

;  Check to see if it is a TRACKING event.

if strtrim(tag_names(event,/STRUCTURE_NAME),2) eq 'WIDGET_TRACKING' then begin

    if event.enter eq 0 then widget_control, state.w.keyboard, SENSITIVE=0
    wset, state.p.plotwin1_wid
    device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
                  state.p.pixmap1_wid]

    wset, state.p.plotwin2_wid
    device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,0,$
                  state.p.pixmap2_wid]

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
            xmergexd_plotupdate,state
            
        end

        'XZoom': begin

            z = where(finite(state.p.reg) eq 1,count)
            if count eq 0 then begin
                
                state.p.reg[*,0] = xy[0:1]
                wset, state.p.pixmap1_wid
                plots, [event.x,event.x],[0,state.p.plot1size[1]],COLOR=2,$
                  /DEVICE,LINESTYLE=1,THICK=2
                wset, state.p.plotwin1_wid
                device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,$
                              0,state.p.pixmap1_wid]
                wset, state.p.pixmap2_wid
                plots, [event.x,event.x],[0,state.p.plot2size[1]],COLOR=2,$
                  /DEVICE,LINESTYLE=1,THICK=2
                wset, state.p.plotwin2_wid
                device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,$
                              0,state.p.pixmap2_wid]
               
            endif else begin

                state.p.reg[*,1] = xy[0:1]
                state.p.plot1xrange = [min(state.p.reg[0,*],max=m),m]
                state.r.cursormode = 'None'
                state.p.reg = !values.f_nan
                xmergexd_plotupdate,state
                xmergexd_setminmax,state
                
            endelse


        end

        'YZoom': begin

            z = where(finite(state.p.reg) eq 1,count)
            if count eq 0 then begin
                
                state.p.reg[*,0] = xy[0:1]
                wset, state.p.pixmap1_wid
                plots, [0,state.p.plot1size[0]],[event.y,event.y],COLOR=2,$
                  /DEVICE,LINESTYLE=1,THICK=2

                wset, state.p.plotwin1_wid
                device, copy=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,$
                              0,state.p.pixmap1_wid]
               
            endif else begin

                state.p.reg[*,1] = xy[0:1]
                state.p.plot1yrange = [min(state.p.reg[1,*],MAX=m),m]
                state.r.cursormode = 'None'
                state.p.reg = !values.f_nan
                xmergexd_plotupdate,state
                xmergexd_setminmax,state
                
            endelse

        end
        
        'Zoom': begin
            
            z = where(finite(state.p.reg) eq 1,count)
            if count eq 0 then state.p.reg[*,0] = xy[0:1] else begin 
                
                state.p.reg[*,1] = xy[0:1]
                state.p.plot1xrange   = [min(state.p.reg[0,*],MAX=max),max]
                state.p.plot1yrange   = [min(state.p.reg[1,*],MAX=max),max]
                xmergexd_getminmax2,state
                xmergexd_plotupdate,state
                xmergexd_setminmax,state
                state.r.cursormode   = 'None'
                state.p.reg  = !values.f_nan
                
            endelse
            
        end
        
        else: begin

            state.r.trim = xy[0]
            xmergexd_cutscalespec,state
            xmergexd_plotupdate,state
            state.r.trim = !values.f_nan

        end
        
    endcase
    
endif

;  Copy the pixmaps and draw the lines.


wset, state.p.plotwin1_wid
device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
              state.p.pixmap1_wid]

wset, state.p.plotwin2_wid
device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,0,$
              state.p.pixmap2_wid]

wset, state.p.plotwin1_wid
case state.r.cursormode of 

    'XZoom': begin

        wset, state.p.plotwin1_wid
        plots, [event.x,event.x],[0,state.p.plot1size[1]],COLOR=2,/DEVICE
        wset, state.p.plotwin2_wid
        plots, [event.x,event.x],[0,state.p.plot2size[1]],COLOR=2,/DEVICE

    end

    'YZoom': plots, [0,state.p.plot1size[0]],[event.y,event.y],COLOR=2,/DEVICE

    'Zoom': begin

        plots, [event.x,event.x],[0,state.p.plot1size[1]],COLOR=2,/DEVICE
        plots, [0,state.p.plot1size[0]],[event.y,event.y],cOLOR=2,/DEVICE
        xy = convert_coord(event.x,event.y,/DEVICE,/TO_DATA)
        plots,[state.p.reg[0,0],state.p.reg[0,0]],[state.p.reg[1,0],xy[1]],$
          LINESTYLE=2,COLOR=2
        plots, [state.p.reg[0,0],xy[0]],[state.p.reg[1,0],state.p.reg[1,0]],$
          LINESTYLE=2,COLOR=2
        
    end

    else: begin

        plots, [event.x,event.x],[0,state.p.plot1size[1]],COLOR=2,/DEVICE
        plots, [0,state.p.plot1size[0]],[event.y,event.y],COLOR=2,/DEVICE
        wset, state.p.plotwin2_wid
        plots, [event.x,event.x],[0,state.p.plot2size[1]],COLOR=2,/DEVICE

    end

endcase

;  Update the cursor tracking

if not state.w.freeze then begin

    label = 'Cursor X: '+strtrim(xy[0],2)
    widget_control,state.w.message,SET_VALUE=label

endif

cont:

widget_control, state.w.xmergexd_base, SET_UVALUE=state, /NO_COPY


end
;
;******************************************************************************
;
pro xmergexd_plotwinevent2, event

widget_control, event.top, GET_UVALUE = state, /NO_COPY
widget_control, event.id,  GET_UVALUE = uvalue

;  Check to see if it is a TRACKING event.

if strtrim(tag_names(event,/STRUCTURE_NAME),2) eq 'WIDGET_TRACKING' then begin

    if event.enter eq 0 then widget_control, state.w.keyboard, SENSITIVE=0
    wset, state.p.plotwin1_wid
    device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
                  state.p.pixmap1_wid]

    wset, state.p.plotwin2_wid
    device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,0,$
                  state.p.pixmap2_wid]

    state.p.plotwin = 2
    goto, cont
    
endif

;  If not, set the keyboard focus and active window.

widget_control, state.w.keyboard, /INPUT_FOCUS, /SENSITIVE

!p = state.p.pscale2
!x = state.p.xscale2
!y = state.p.yscale2
x  = event.x/float(state.p.plot2size[0])
y  = event.y/float(state.p.plot2size[1])
xy = convert_coord(x,y,/NORMAL,/TO_DATA)

if event.type eq 1 and state.r.cursormode eq 'YZoom' then begin

    z = where(finite(state.p.reg) eq 1,count)
    if count eq 0 then begin
        
        state.p.reg[*,0] = xy[0:1]
        wset, state.p.pixmap2_wid
        plots, [0,state.p.plot2size[0]],[event.y,event.y],COLOR=2,$
          /DEVICE,LINESTYLE=1,THICK=2
        
        wset, state.p.plotwin2_wid
        device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,0,$
                      state.p.pixmap2_wid]
        
    endif else begin
        
        state.p.reg[*,1] = xy[0:1]
        state.p.plot2yrange = [min(state.p.reg[1,*],MAX=m),m]
        state.r.cursormode = 'None'
        state.p.reg = !values.f_nan
        xmergexd_plotupdate,state
        xmergexd_setminmax,state
        
    endelse


endif

;  Copy the pixmaps and draw the lines.


wset, state.p.plotwin1_wid
device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
              state.p.pixmap1_wid]

wset, state.p.plotwin2_wid
device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,0,$
              state.p.pixmap2_wid]

if state.r.cursormode eq 'YZoom' then begin

    plots, [0,state.p.plot2size[0]],[event.y,event.y],COLOR=2,/DEVICE

endif else begin

    plots, [0,state.p.plot2size[0]],[event.y,event.y],COLOR=2,/DEVICE
    plots, [event.x,event.x],[0,state.p.plot2size[1]],COLOR=2,/DEVICE

    wset, state.p.plotwin1_wid
    plots, [event.x,event.x],[0,state.p.plot1size[1]],COLOR=2,/DEVICE

endelse


;  Update the cursor tracking

if not state.w.freeze then begin

    label = 'Cursor X: '+strtrim(xy[0],2)
    widget_control,state.w.message,SET_VALUE=label
    
endif

;  Plot zoom line

if state.r.cursormode eq 'Zoom' then begin

    wset, state.p.plotwin2_wid
    plots,!x.crange,[state.p.reg[1,0],state.p.reg[1,0]],LINESTYLE=2,COLOR=2

endif
    
cont:

widget_control, state.w.xmergexd_base, SET_UVALUE=state, /NO_COPY

end
;
;******************************************************************************
;
pro xmergexd_resize, event

widget_control, event.top, GET_UVALUE = state, /NO_COPY
widget_control, event.id,  GET_UVALUE = uvalue

widget_control, state.w.xmergexd_base, TLB_GET_SIZE=size

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

;  Window 2

state.p.plot2size[0]=size[0]-state.p.buffer[0]
state.p.plot2size[1]=(size[1]-state.p.buffer[1])*state.p.plot2scale

widget_control, state.w.plotwin2, DRAW_XSIZE=state.p.plot2size[0]
widget_control, state.w.plotwin2, DRAW_YSIZE=state.p.plot2size[1]

wdelete,state.p.pixmap2_wid
window, /FREE, /PIXMAP,XSIZE=state.p.plot2size[0],YSIZE=state.p.plot2size[1]
state.p.pixmap2_wid = !d.window


if not state.w.freeze then xmergexd_plotupdate,state

widget_control, state.w.xmergexd_base, SET_UVALUE=state, /NO_COPY

end
;
;******************************************************************************
;
;-------------------------------Main Program----------------------------------
;
;******************************************************************************
;
pro xmergexd

cleanplot,/SILENT

getosinfo,dirsep,strsep

last  = strpos(!path,'Spextool')
first = strpos(!path,strsep,last,/REVERSE_SEARCH)
packagepath = strmid(!path,first+1,last-first+8)

;  Load color table

mkct
device, RETAIN=2

;  Get fonts

getfonts,buttonfont,textfont

;  Build three structures which will hold important info.
;  w - contains info pertaining to widget operations.
;  r - contains info pertaining to the reduction process.
;  d - contains all of the data.

w = {anchor_bg:0L,$
     ap_dl:0L,$
     dirsep:dirsep,$
     freeze:1,$
     keyboard:0L,$
     longxd_fld:[0L,0L],$
     message:0L,$
     oname_fld:[0L,0L],$
     plotspec_bg:0L,$
     plottype_bg:0L,$
     plotwin1:0,$
     plotwin2:0,$
     scale_fld:[0L,0L],$
     shortxd_fld:[0L,0L],$
     trimspec_bg:0L,$
     trimdir_bg:0L,$
     xmergexd_base:0L,$
     xmin_fld:[0L,0L],$
     xmax_fld:[0L,0L],$
     ymin1_fld:[0L,0L],$
     ymax1_fld:[0L,0L],$
     ymin2_fld:[0L,0L],$
     ymax2_fld:[0L,0L]}

r = {anchor:'ShortXD',$
     greenpos:'',$
     cursormode:'None',$
     longxdpath:'',$
     mergedorders:ptr_new(fltarr(2)),$
     packagepath:packagepath,$
     scale:1.,$
     shortxdpath:'',$
     textoutput:0,$
     trim:!values.f_nan,$
     trimdir:'Right',$
     trimspec:'Green',$
     weighted:1}

d = {addspec:ptr_new(2),$
     bluespec:ptr_new(2),$
     greenspec:ptr_new(2),$
     longxdhdr:ptr_new(2),$
     longxdspec:ptr_new(2),$
     mergedspec:ptr_new(2),$
     origspec:ptr_new(2),$
     shortxdspec:ptr_new(2),$
     shortxdhdr:ptr_new(2),$
     tempmergedspec:ptr_new(2)}

p = {activespec:1,$
     buffer:[0.,0.],$
     front:0,$
     greenpos:'',$
     pixmap1_wid:0L,$
     pixmap2_wid:0L,$
     plotwin:1,$
     plotwin1_wid:0L,$
     plotwin2_wid:0L,$
     plot1absxrange:[0.,0.],$
     plot1absyrange:[0.,0.],$
     plot1scale:0.0,$
     plot1xrange:[0.,0.],$
     plot1yrange:[0.,0.],$
     plot1size:[670,320],$
     plot2absyrange:[0.,0.],$
     plot2size:[670,320/1.4],$
     plot2scale:0.0,$
     plot2yrange:[0.,0.],$
     plotbluespec:1,$
     plottype:'Overlap',$
     plotgreenspec:1,$
     title:'',$
     pscale1:!p,$
     xscale1:!x,$
     pscale2:!p,$
     xscale2:!x,$
     xtitle:'',$
     xunits:'',$
     yscale1:!y,$
     yscale2:!y,$
     ytitle:'',$
     yunits:'',$
     reg:[[!values.f_nan,!values.f_nan],$
          [!values.f_nan,!values.f_nan]]}

p.plot1scale = float(p.plot1size[1])/(p.plot1size[1]+p.plot2size[1])
p.plot2scale = float(p.plot2size[1])/(p.plot1size[1]+p.plot2size[1])

;  Load the three structures in the state structure.

state = {w:w,r:r,d:d,p:p}

state.w.xmergexd_base = widget_base(TITLE='Xmergexd', $
                                      /COLUMN,$
                                      /TLB_SIZE_EVENTS)

   quit_button = widget_button(state.w.xmergexd_base,$
                               FONT=buttonfont,$
                               EVENT_PRO='xmergexd_event',$
                               VALUE='Quit',$
                               UVALUE='Quit')
   
   state.w.keyboard = widget_text(state.w.xmergexd_base, $
                                  /ALL_EVENTS, $
                                  SCR_XSIZE=1, $
                                  SCR_YSIZE=1, $
                                  UVALUE='Keyboard', $
                                  EVENT_PRO='xmergexd_event',$
                                  VALUE='')
   
   row_base = widget_base(state.w.xmergexd_base,$
                          /ROW)

      col1_base = widget_base(row_base,$
                              EVENT_PRO='xmergexd_event',$
                              /COLUMN)
      
         box1_base = widget_base(col1_base,$
                                 /COLUMN,$
                                 /FRAME)
         
            label = widget_label(box1_base,$
                                 VALUE='1.  Load Spectra',$
                                 FONT=buttonfont,$
                                 /ALIGN_LEFT)
            
            
            row = widget_base(box1_base,$
                              /ROW,$
                              /BASE_ALIGN_CENTER)

               input = widget_button(row,$
                                     FONT=buttonfont,$
                                     VALUE='ShortXD',$
                                     UVALUE='ShortXD',$
                                     EVENT_PRO='xmergexd_event')
               
               
               input_fld = coyote_field2(row,$
                                         LABELFONT=buttonfont,$
                                         FIELDFONT=textfont,$
                                         TITLE=':',$
                                         UVALUE='ShortXD Field',$
                                         XSIZE=18,$
                                         EVENT_PRO='xmergexd_event',$
                                         /CR_ONLY,$
                                         TEXTID=textid)
               state.w.shortxd_fld = [input_fld,textid]
               
            row = widget_base(box1_base,$
                              /ROW,$
                              /BASE_ALIGN_CENTER)

               input = widget_button(row,$
                                     FONT=buttonfont,$
                                     VALUE='LongXD',$
                                     UVALUE='LongXD',$
                                     EVENT_PRO='xmergexd_event')
               
               
               input_fld = coyote_field2(row,$
                                         LABELFONT=buttonfont,$
                                         FIELDFONT=textfont,$
                                         TITLE=':',$
                                         UVALUE='LongXD Field',$
                                         XSIZE=18,$
                                         EVENT_PRO='xmergexd_event',$
                                         /CR_ONLY,$
                                         TEXTID=textid)
               state.w.longxd_fld = [input_fld,textid]
            

               bg = cw_bgroup(box1_base,$
                              ['Weighted Mean'],$
                              FONT=buttonfont,$
                              UVALUE='Weighted Mean',$
                              SET_VALUE=state.r.weighted,$
                              /NONEXCLUSIVE)

            load = widget_button(box1_base,$
                                 VALUE='Load Spectrum',$
                                 UVALUE='Load Spectrum',$
                                 FONT=buttonfont)
            
         box2_base = widget_base(col1_base,$
                                 /COLUMN,$
                                 FRAME=1)

            label = widget_label(box2_base,$
                                 VALUE='2.  Cut/Scale/Merge Spectra',$
                                 FONT=buttonfont,$
                                 /ALIGN_LEFT)

            state.w.anchor_bg = cw_bgroup(box2_base,$
                                          font=buttonfont,$
                                          ['ShortXD','LongXD'],$
                                          /row,$
                                          /return_name,$
                                          /no_release,$
                                          /exclusive,$
                                          label_left='Anchor:',$
                                          UVALUE='Anchor',$
                                          set_VALUE=0)
            
            row = widget_base(box2_base,$
                              /BASE_ALIGN_CENTER,$
                              /ROW)
               
               scale = coyote_field2(row,$
                                     LABELFONT=buttonfont,$
                                     FIELDFONT=textfont,$
                                     TITLE='Scale:',$
                                     UVALUE='Scale Spectrum',$
                                     XSIZE=12,$
                                     VALUE=state.r.scale,$
                                     EVENT_PRO='xmergexd_event',$
                                     /CR_ONLY,$
                                     TEXTID=textid)
               state.w.scale_fld = [scale,textid]
               
               widget_button = widget_button(row,$
                                             VALUE='Auto Scale',$
                                             UVALUE='Auto Scale',$
                                             FONT=buttonfont)
            
           state.w.trimspec_bg = cw_bgroup(box2_base,$
                                           FONT=buttonfont,$
                                           ['Green','Blue'],$
                                           /ROW,$
                                           /RETURN_NAME,$
                                           /EXCLUSIVE,$
                                           LABEL_LEFT='Trim Spectrum:',$
                                           UVALUE='Trim Spectrum',$
                                           SET_VALUE=0)

           state.w.trimdir_bg = cw_bgroup(box2_base,$
                                           FONT=buttonfont,$
                                           ['Right','Left'],$
                                           /ROW,$
                                           /RETURN_NAME,$
                                           /EXCLUSIVE,$
                                           LABEL_LEFT='Trim Direction:',$
                                           UVALUE='Trim Direction',$
                                           SET_VALUE=0)


            widget_button = widget_button(box2_base,$
                                          EVENT_PRO='xmergexd_event',$
                                          VALUE='Merge Spectra',$
                                          UVALUE='Merge Orders',$
                                          FONT=buttonfont)
            
            


         box4_base = widget_base(col1_base,$
                                 /COLUMN,$
                                 /FRAME)
         
            label = widget_label(box4_base,$
                                 VALUE='4.  Write Spectra to File',$
                                 FONT=buttonfont,$
                                 /ALIGN_LEFT)
            
            outformat_bg = cw_bgroup(box4_base,$
                                     FONT=buttonfont,$
                                     ['Text Output'],$
                                     /ROW,$
                                     /RETURN_NAME,$
                                     /NONEXCLUSIVE,$
                                     LABEL_LEFT='',$
                                     UVALUE='Output Format',$
                                     SET_VALUE=[0])
            
            oname = coyote_field2(box4_base,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='File Name:',$
                                  UVALUE='Object File Oname',$
                                  XSIZE=18,$
                                  TEXTID=textid)
            state.w.oname_fld = [oname,textid]
            
            write = widget_button(box4_base,$
                                  VALUE='Write File',$
                                  UVALUE='Write File',$
                                  FONT=buttonfont)

          col2_base = widget_base(row_base,$
                                  /COLUMN)

             state.w.message = widget_text(col2_base, $
                                           VALUE='',$
                                           YSIZE=1)

             row = widget_base(col2_base,$
                               /ROW,$
                               /BASE_ALIGN_CENTER,$
                               FRAME=1,$
                               EVENT_PRO='xmergexd_event')

                state.w.plottype_bg = cw_bgroup(row,$
                                                FONT=buttonfont,$
                                                ['Overlap','Combine'],$
                                                /ROW,$
                                                /RETURN_NAME,$
                                                /NO_RELEASE,$
                                                /EXCLUSIVE,$
                                                LABEL_LEFT='Spec Type:',$
                                                UVALUE='Plot Type',$
                                                SET_VALUE=0)

                state.w.plotspec_bg = cw_bgroup(row,$
                                                FONT=buttonfont,$
                                                ['Blue','Green'],$
                                                /ROW,$
                                                /RETURN_NAME,$
                                                /NONEXCLUSIVE,$
                                                LABEL_LEFT='Plot Spectrum:',$
                                                UVALUE='Plot Spectrum',$
                                                SET_VALUE=[1,1])


             state.w.plotwin1 = widget_draw(col2_base,$
                                            XSIZE=state.p.plot1size[0],$
                                            YSIZE=state.p.plot1size[1],$
                                            /TRACKING_EVENTS,$
                                            /MOTION_EVENTS,$
                                      EVENT_PRO='xmergexd_plotwinevent1',$
                                            UVALUE='Plot Window 1')            
 

             row_base = widget_base(col2_base,$
                                    /FRAME,$
                                    /ROW)
   
                xmin = coyote_field2(row_base,$
                                     LABELFONT=buttonfont,$
                                     FIELDFONT=textfont,$
                                     TITLE='X Min:',$
                                     UVALUE='X Min',$
                                     XSIZE=12,$
                                     EVENT_PRO='xmergexd_minmax',$
                                     /CR_ONLY,$
                                     TEXTID=textid)
                state.w.xmin_fld = [xmin,textid]
                
                xmax = coyote_field2(row_base,$
                                     LABELFONT=buttonfont,$
                                     FIELDFONT=textfont,$
                                     TITLE='X Max:',$
                                     UVALUE='X Max',$
                                     XSIZE=12,$
                                     EVENT_PRO='xmergexd_minmax',$
                                     /CR_ONLY,$
                                     TEXTID=textid)
                state.w.xmax_fld = [xmax,textid]
                
                ymin = coyote_field2(row_base,$
                                     LABELFONT=buttonfont,$
                                     FIELDFONT=textfont,$
                                     TITLE='Y Min:',$
                                     UVALUE='Y1 Min',$
                                     XSIZE=12,$
                                     EVENT_PRO='xmergexd_minmax',$
                                     /CR_ONLY,$
                                     TEXTID=textid)
                state.w.ymin1_fld = [ymin,textid]
                
                ymax = coyote_field2(row_base,$
                                     LABELFONT=buttonfont,$
                                     FIELDFONT=textfont,$
                                     TITLE='Y Max:',$
                                     UVALUE='Y1 Max',$
                                     XSIZE=12,$
                                     EVENT_PRO='xmergexd_minmax',$
                                     /CR_ONLY,$
                                     TEXTID=textid)
                state.w.ymax1_fld = [ymax,textid]
                
             state.w.plotwin2 = widget_draw(col2_base,$
                                            XSIZE=state.p.plot2size[0],$
                                            YSIZE=state.p.plot2size[1],$
                                            /TRACKING_EVENTS,$
                                            /MOTION_EVENTS,$
                                      EVENT_PRO='xmergexd_plotwinevent2',$
                                            UVALUE='Plot Window 2')
          
             row_base = widget_base(col2_base,$
                                    /ROW,$
                                    /FRAME,$
                                    EVENT_PRO='xmergexd_event')
          
                ymin = coyote_field2(row_base,$
                                     LABELFONT=buttonfont,$
                                     FIELDFONT=textfont,$
                                     TITLE='Y Min:',$
                                     UVALUE='Y2 Min',$
                                     XSIZE=12,$
                                     EVENT_PRO='xmergexd_minmax',$
                                     /CR_ONLY,$
                                     TEXTID=textid)
                state.w.ymin2_fld = [ymin,textid]
                
                ymax = coyote_field2(row_base,$
                                     LABELFONT=buttonfont,$
                                     FIELDFONT=textfont,$
                                     TITLE='Y Max:',$
                                     UVALUE='Y2 Max',$
                                     XSIZE=12,$
                                     EVENT_PRO='xmergexd_minmax',$
                                     /CR_ONLY,$
                                     TEXTID=textid)
                state.w.ymax2_fld = [ymax,textid]

   button = widget_button(state.w.xmergexd_base,$
                          FONT=buttonfont,$
                          EVENT_PRO='xmergexd_event',$
                          VALUE='Help',$
                          UVALUE='Help')

; Get things running.  Center the widget using the Fanning routine.
      
    centertlb,state.w.xmergexd_base
      
    widget_control, state.w.xmergexd_base, /REALIZE
    
    mkct
    
;  Get plotwin ids
    
    widget_control, state.w.plotwin1, GET_VALUE=x
    state.p.plotwin1_wid = x
    widget_control, state.w.plotwin2, GET_VALUE=x
    state.p.plotwin2_wid = x
    
    window, /FREE, /PIXMAP,XSIZE=state.p.plot1size[0],$
      YSIZE=state.p.plot1size[1]
    state.p.pixmap1_wid = !d.window

    window, /FREE, /PIXMAP,XSIZE=state.p.plot2size[0],$
      YSIZE=state.p.plot2size[1]
    state.p.pixmap2_wid = !d.window
    
;  Get sizes for things.
    
    widget_geom = widget_info(state.w.xmergexd_base, /GEOMETRY)
    state.p.buffer[0]=widget_geom.xsize-state.p.plot1size[0]
    state.p.buffer[1]=widget_geom.ysize-state.p.plot1size[1]-$
      state.p.plot2size[1]

; Start the Event Loop. This will be a non-blocking program.
    
    XManager, 'xmergexd', $
      state.w.xmergexd_base, $
      CLEANUP='xmergexd_cleanup',$
      EVENT_HANDLER='xmergexd_resize',$
      /NO_BLOCK
    
; Put state variable into the user value of the top level base.

widget_control, state.w.xmergexd_base, SET_UVALUE=state, /NO_COPY

cont:

end
