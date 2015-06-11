; NAME:
;     xcleanspec
;    
; PURPOSE:
;     Cleans SpeX spectra interactively.
;    
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     xcleanspec
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
;     Writes a SpeX FITS file out with the cleaned spectra
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
;     Only works with SpeX spectra FITS format
;
; PROCEDURE:
;     Allows the user to either remove bad pixels or replace bad pixels.
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2002       - Written by M. Cushing, Institute for Astronomy, UH
;     2004-05-19 - Removed Change <R> and replaced with Gaussian smooth
;                  by slit width
;     2005-02-10 - Changed lower plot to S/N
;     2005-12-17 - Fixed bug where the yrange in the lower plot would
;                  not readjust after a smoothing.
;-
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xcleanspec_cleanup,base

  widget_control, base, GET_UVALUE=state, /NO_COPY
  if n_elements(state) ne 0 then begin

     ptr_free, state.r.orders
     ptr_free, state.d.hdr
     ptr_free, state.d.espec
     ptr_free, state.d.ospec
     ptr_free, state.d.pspec

  endif

  state = 0B

end
;
;******************************************************************************
;
pro xcleanspec_help,state
  
  openr, lun, filepath('xcleanspec_helpfile.txt',$
                       ROOT_DIR=state.r.packagepath,SUBDIR='helpfiles'),$
         /GET_LUN
  nlines = numlines(filepath('xcleanspec_helpfile.txt',$
                             ROOT_DIR=state.r.packagepath,$
                             SUBDIR='helpfiles'))
  array = strarr(nlines)
  readf, lun, array
  free_lun, lun

  xmc_displaytext,array,TITLE='Xcleanspec Help File', $
                  GROUP_LEADER=state.w.xcleanspec_base

end
;
;******************************************************************************
;
pro xcleanspec_loadspec,state

;  Get files.

  path = cfld(state.w.path_fld,7,CANCEL=cancel)
  if cancel then return
  path = cpath(path,WIDGET_ID=state.w.xcleanspec_base,CANCEL=cancel)
  if cancel then return

  specfile = cfld(state.w.ispectrum_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return
  specfile = cfile(path+specfile,WIDGET_ID=state.w.xcleanspec_base, $
                   CANCEL=cancel)
  if cancel then return

;  Read spectra.

  readspec,specfile,spc,hdr,obsmode,start,stop,norders,naps,orders,$
           xunits,yunits,slith_pix,slith_arc,slitw_pix,slitw_arc,$
           airmass,xtitle,ytitle

;  Store spectra 

  *state.d.ospec    = spc
  *state.d.espec    = spc
  *state.d.pspec    = spc
  *state.d.hdr      = hdr          
  *state.r.orders   = orders
  state.r.norders   = norders
  state.r.naps      = naps
  state.r.slitw_pix = slitw_pix
  state.r.start     = start
  state.r.stop      = stop

;  Update the order and aperture droplists and set up first spectrum

  widget_control, state.w.order_dl, SET_VALUE=string(orders,FORMAT='(i2.2)')
  widget_control, state.w.aperture_dl, SET_VALUE=string(indgen(naps)+1,$
                                                        FORMAT='(i2.2)')

  state.p.order = orders[0]
  state.p.ap    = 1
  state.p.idx   = 0

;  Get plot info

  state.p.xtitle = xtitle
  state.p.ytitle = ytitle

  state.p.plot1xrange = [min(spc[*,0],MAX=max),max]
  state.p.plot1yrange = [[0,max(spc[*,1],/NAN)],[0,max(spc[*,2],/NAN)]]
  state.p.plot2yrange = [min(spc[*,1]/spc[*,2],MAX=max,/NAN),max]


  state.p.plot1absxrange  = state.p.plot1xrange
  state.p.plot1absyrange = state.p.plot1yrange
  state.p.plot2absyrange  = state.p.plot2yrange

  scale = (slitw_pix gt 2) ? 1.5:2.5



  widget_control, state.w.sgwin_fld[1],$
                  SET_VALUE=strtrim(string(slitw_pix*scale,FORMAT='(f4.1)'),2)

  widget_control, state.w.fwhm_fld[1],SET_VALUE=strtrim(state.r.slitw_pix,2)

;  Unfreeze the widget 

  state.r.freeze = 0
  widget_control, state.w.plotwin1, DRAW_BUTTON_EVENTS=1
  widget_control, state.w.plotwin2, DRAW_BUTTON_EVENTS=1
  widget_control, state.w.box3_base,SENSITIVE=1

;  Plot first spectrum

  xcleanspec_plotupdate,state
  xcleanspec_setminmax,state

  state.r.smthhistory = ''

out:

end
;
;******************************************************************************
;
pro xcleanspec_plotsn,state

  z = where(finite((*state.d.pspec)[*,0,state.p.idx]) eq 1)

  sn = (*state.d.pspec)[z,1,state.p.idx]/(*state.d.pspec)[z,2,state.p.idx]


  plot,(*state.d.pspec)[z,0,state.p.idx],sn,PSYM=10,/XSTY,/YSTY,$
       XRANGE=state.p.plot1xrange,YRANGE=state.p.plot2yrange,$
       XTITLE=state.p.xtitle,YTITLE='!5S/N',CHARSIZE=mc_strsize('!5A',0.01)

  state.p.pscale2 = !p
  state.p.xscale2 = !x
  state.p.yscale2 = !y

end
;
;******************************************************************************
;
pro xcleanspec_plotflux,state,PS=ps

  atmoscolor = (keyword_set(PS) eq 1) ? 4:5

  z    = where(finite((*state.d.pspec)[*,0,state.p.idx]) eq 1)
  wave = (*state.d.pspec)[z,0,state.p.idx]
  flux = (*state.d.pspec)[z,state.p.spectype+1,state.p.idx]

  lidx   = strpos(state.p.ytitle,'(')
  ridx   = strpos(state.p.ytitle,')')
  yunits = strmid(state.p.ytitle,lidx+1,ridx-lidx-1)

  ytitle = (state.p.spectype eq 0) ? state.p.ytitle:'!5Error ('+yunits+')'

  if state.p.plotatmos then begin

     plot,state.p.awave,state.p.atrans,PSYM=10,XSTY=5,YSTY=5,$
          XRANGE=state.p.plot1xrange,YRANGE=[0,1],COLOR=atmoscolor, $
          CHARSIZE=mc_strsize('!5A',0.01) 
     ticks = string(findgen(11)*.1,FORMAT='(f3.1)')
     axis,YAXIS=1,YTICKS=10,YTICKNAME='!5'+ticks,YMINOR=1,COLOR=atmoscolor,$
          CHARSIZE=mc_strsize('!5A',0.01)


     ystyle = 9

  endif else ystyle = 1


  plot,wave,flux,PSYM=10,/XSTY,YSTY=ystyle,XRANGE=state.p.plot1xrange,$
       YRANGE=state.p.plot1yrange[*,state.p.spectype],XTITLE=state.p.xtitle, $
       YTITLE=ytitle,NOERASE=state.p.plotatmos, $
       CHARSIZE=mc_strsize('!5A',0.01)

  if state.r.plotlines then begin

;  Label H lines if requested
     
     z = where(state.d.hlines lt state.p.plot1xrange[1] and $
               state.d.hlines gt state.p.plot1xrange[0],count)
     
     for i =0, count-1 do begin
        
        tabinv,wave,(state.d.hlines)[z[i]],idx
        
        xybot = convert_coord((state.d.hlines)[z[i]],flux[idx],/DATA,/TO_NORM)
        
        plots,[xybot[0],xybot[0]],[xybot[1],0.85],LINESTYLE=1,COLOR=3,THICK=2,$
              /NORM
        name = '!5'+(state.d.hnames)[z[i]]
        xyouts, xybot[0],0.85,name,ORIENTATION=90,/NORM,COLOR=3,$
                CHARSIZE=mc_strsize('!5A',0.01)
        
     endfor
     
  endif

  if not keyword_set(PS) then begin

     state.p.pscale1 = !p
     state.p.xscale1 = !x
     state.p.yscale1 = !y

  endif

end
;
;******************************************************************************
;
pro xcleanspec_plotupdate,state,PS=ps

  if keyword_set(PS) then begin

     widget_control, state.w.xcleanspec_base,SENSITIVE=0
     forminfo = CMPS_FORM(/INITIALIZE,$
                          SELECT='Full Landscape (color)')
     
     formInfo = CMPS_FORM(Cancel=cancelled, Create=create, $
                          defaults=forminfo,$
                          button_names = ['Create PS File'],$
                          Parent=state.w.xcleanspec_base)
     
     IF NOT cancelled THEN BEGIN
        
;        print, forminfo
        thisDevice = !D.Name
        Set_Plot, "PS"
        Device, _Extra=formInfo
        xcleanspec_plotflux,state,PS=ps
        Device, /Close
        Set_Plot, thisDevice
        
     ENDIF

     widget_control, state.w.xcleanspec_base,SENSITIVE=1
     return

  endif

;  Plot window 1

  wset, state.p.pixmap1_wid
  xcleanspec_plotflux,state

  wset, state.p.plotwin1_wid
  device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
                state.p.pixmap1_wid]

;  Plot window 2

  wset, state.p.pixmap2_wid
  xcleanspec_plotsn,state

  wset, state.p.plotwin2_wid
  device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,0,$
                state.p.pixmap2_wid]

end
;
;******************************************************************************
;
pro xcleanspec_setminmax,state

  widget_control,state.w.xmin_fld[1], $
                 SET_VALUE=strtrim(state.p.plot1xrange[0],2)
  widget_control,state.w.xmax_fld[1], $
                 SET_VALUE=strtrim(state.p.plot1xrange[1],2)
  widget_control,state.w.ymin1_fld[1], $
                 SET_VALUE=strtrim(state.p.plot1yrange[0,state.p.spectype],2)
  widget_control,state.w.ymax1_fld[1], $
                 SET_VALUE=strtrim(state.p.plot1yrange[1,state.p.spectype],2)

  widget_control,state.w.ymin2_fld[1], $
                 SET_VALUE=strtrim(state.p.plot2yrange[0],2)
  widget_control,state.w.ymax2_fld[1], $
                 SET_VALUE=strtrim(state.p.plot2yrange[1],2)

end
;
;******************************************************************************
;
pro xcleanspec_smoothspec,state

  spec = *state.d.espec

  if state.r.smoothtype eq 'Savitzky-Golay' then begin

     res = state.r.resperpix/state.r.slitw_pix

     sgwin = cfld(state.w.sgwin_fld,4,/EMPTY,CANCEL=cancel)
     if cancel then return

     sgdeg = cfld(state.w.sgdeg_fld,4,/EMPTY,CANCEL=cancel)
     if cancel then return
     
     sgdeg = crange(sgdeg,sgwin,'SG Degree',/KLT,$
                    WIDGET_ID=state.w.xcleanspec_base,CANCEL=cancel)
     if cancel then return

     print, ' '
     print, 'Performing a Savitzky-Golay smoothing with a window of '+$
            strtrim(sgwin,2)
     print, ' and a degree of '+strtrim(fix(sgdeg),2)+'.'
     print, ' '

     for i = 0, state.r.norders-1 do begin
        
        for j = 0, state.r.naps-1 do begin
           
           z = where(finite((*state.d.espec)[*,1,i*state.r.naps+j]) eq 1)

           cflux  = (*state.d.espec)[z,1,i*state.r.naps+j]
           cerror = (*state.d.espec)[z,2,i*state.r.naps+j]
           
           cflux = savitzky_golay(cflux,sgwin,DEGREE=sgdeg,ERROR=cerror)

           spec[z,1,i*state.r.naps+j] = cflux
           spec[z,2,i*state.r.naps+j] = cerror
           
        endfor
        
     endfor

     state.r.smthhistory=$
        'These spectra have been convolved with a Savitzky-Golay filter'+$
        ' of width '+strtrim(sgwin,2)+' and degree '+strtrim(sgdeg,2)+'.'

  endif else begin

     fwhm_kernel = cfld(state.w.fwhm_fld,4,/EMPTY,CANCEL=cancel)
     if cancel then return


;    res = cfld(state.w.res_fld,4,/EMPTY,CANCEL=cancel)
;    if cancel then return
;    res = crange(res,[1,state.r.maxres],'<R>',/KLE,$
;                 WIDGET_ID=state.w.xcleanspec_base,CANCEL=cancel)
;    if cancel then return
;    FWHM_final  = state.r.resperpix/res
;    FWHM_kernel = sqrt(FWHM_final^2 - state.r.slitw_pix^2)

     print, ' '
     print, 'Smoothing with a Gaussian with FWHM='+$
            string(FWHM_kernel,format='(f3.1)')+'.'
     print, ' '

     for i = 0, state.r.norders-1 do begin
        
        for j = 0, state.r.naps-1 do begin
           
           z = where(finite((*state.d.espec)[*,1,i*state.r.naps+j]) eq 1)
           convolvespec,findgen(state.r.stop-state.r.start+1),$
                        (*state.d.espec)[z,1,i*state.r.naps+j],FWHM_kernel, $
                        cflux,cerror, $
                        ERROR=(*state.d.espec)[z,2,i*state.r.naps+j], $
                        CANCEL=cancel
           
           spec[z,1,i*state.r.naps+j] = cflux
           spec[z,2,i*state.r.naps+j] = cerror
           
        endfor
        
     endfor

     state.r.smthhistory = $
        'These spectra have been convolved with a Gaussain of FWHM='+$
        strcompress(FWHM_kernel,/RE)+'.'

  endelse

  *state.d.espec = spec
  *state.d.pspec = spec
  state.p.plot2yrange    = [min(spec[*,1]/spec[*,2],MAX=max,/NAN),max]
  xcleanspec_plotupdate,state
  xcleanspec_setminmax,state

  widget_control, state.w.box3_base,SENSITIVE=0

end
;
;******************************************************************************
;
pro xcleanspec_writefile,state

  path = cfld(state.w.path_fld,7,CANCEL=cancel)
  if cancel then return
  path = cpath(path,WIDGET_ID=state.w.xcleanspec_base,CANCEL=cancel)
  if cancel then return

  file = cfld(state.w.oname_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return

  hdr = *state.d.hdr
  if state.r.smthhistory ne '' then begin

     smthhistory = mc_splittext(state.r.smthhistory,70,CANCEL=cancel)
     if cancel then return
     sxaddhist,smthhistory,hdr

  endif

  writefits, path+file+'.fits',*state.d.espec,hdr
  xvspec,path+file+'.fits'

  if state.r.textoutput then begin

     npix = fxpar(*state.d.hdr,'NAXIS1')
     openw,lun,path+file+'.txt', /get_lun
     
     for i = 0, n_elements(*state.d.hdr)-1 do printf, lun, hdr[i]
     
     for i = 0, npix-1 do begin
        
        printf, lun,  strjoin( reform((*state.d.espec)[i,*],3),'  ')
        
     endfor
     close, lun
     free_lun, lun
     
  endif    

end
;
;******************************************************************************
;
pro xcleanspec_zoom,state,IN=in,OUT=out

  if state.p.plotwin eq 1 then begin

     idx = state.p.spectype
     delabsx = state.p.plot1absxrange[1,idx]-state.p.plot1absxrange[0,idx]
     delx    = state.p.plot1xrange[1,idx]-state.p.plot1xrange[0,idx]
     
     delabsy = state.p.plot1absyrange[1,idx]-state.p.plot1absyrange[0,idx]
     dely    = state.p.plot1yrange[1,idx]-state.p.plot1yrange[0,idx]
     
     xcen = state.p.plot1xrange[0,idx]+delx/2.
     ycen = state.p.plot1yrange[0,idx]+dely/2.
     
     case state.r.cursormode of 
        
        'XZoom': begin
           
           z = alog10(delabsx/delx)/alog10(2)
           if keyword_set(IN) then z = z+1 else z=z-1
           hwin = delabsx/2.^z/2.
           state.p.plot1xrange = [xcen-hwin,xcen+hwin]
           xcleanspec_plotupdate,state
           xcleanspec_setminmax,state
           
        end
        
        'YZoom': begin
           
           z = alog10(delabsy/dely)/alog10(2)
           if keyword_set(IN) then z = z+1 else z=z-1
           hwin = delabsy/2.^z/2.
           state.p.plot1yrange[*,idx] = [ycen-hwin,ycen+hwin]
           xcleanspec_plotupdate,state
           xcleanspec_setminmax,state
           
        end
        
        'Zoom': begin
           
           z = alog10(delabsx/delx)/alog10(2)
           if keyword_set(IN) then z = z+1 else z=z-1
           hwin = delabsx/2.^z/2.
           state.p.plot1xrange = [xcen-hwin,xcen+hwin]
           
           z = alog10(delabsy/dely)/alog10(2)
           if keyword_set(IN) then z = z+1 else z=z-1
           hwin = delabsy/2.^z/2.
           state.p.plot1yrange[*,idx] = [ycen-hwin,ycen+hwin]
           
           xcleanspec_plotupdate,state
           xcleanspec_setminmax,state
           
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
           xcleanspec_plotupdate,state
           xcleanspec_setminmax,state
           
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
pro xcleanspec_event, event

  widget_control, event.id,  GET_UVALUE = uvalue

  if uvalue eq 'Quit' then begin

     widget_control, event.top, /DESTROY
     goto, getout

  endif


  widget_control, event.top, GET_UVALUE = state, /NO_COPY

  case uvalue of

     'Input Spectrum': begin

        path = cfld(state.w.path_fld,7,CANCEL=cancel)
        if cancel then goto, cont
        if path ne '' then path = cpath(path,$
                                        WIDGET_ID=state.w.xcleanspec_base,$
                                        CANCEL=cancel)
        if cancel then return

        obj = dialog_pickfile(DIALOG_PARENT=state.w.xcleanspec_base,$
                              PATH=path,/MUST_EXIST,FILTER='*.fits')
        if obj eq '' then goto, cont
        widget_control,state.w.ispectrum_fld[1], $
                       SET_VALUE = strmid(obj[0],strpos(obj,state.w.dirsep, $
                                                        /REVERSE_S)+1)

     end

     'Help': xcleanspec_help,state

     'Hydrogen Lines': begin

        state.r.plotlines = event.select
        if not state.r.freeze then xcleanspec_plotupdate,state

     end

     'Keyboard': begin

        if state.r.freeze then goto, cont
        case strtrim(event.ch,2) of 
           
           'a': BEGIN

              state.p.plot1absxrange = state.p.plot1xrange
              state.p.plot1absyrange=state.p.plot1yrange
              state.p.plot2absyrange=state.p.plot2yrange

           end

           'c': begin
              
              state.r.cursormode = 'None'
              state.p.reg = !values.f_nan
              xcleanspec_plotupdate,state
              
           end

           'f': begin           ; Fix

              if state.r.cursormode eq 'None' then begin

                 state.r.cursormode = 'Fix'
                 state.p.reg = !values.f_nan
                 
              endif

           end

           'i': xcleanspec_zoom,state,/IN

           'o': xcleanspec_zoom,state,/OUT

           'p': xcleanspec_plotupdate,state,/PS

           'r': begin           ; Remove

              if state.r.cursormode eq 'None' then begin
                 
                 state.r.cursormode = 'Remove'
                 state.p.reg = !values.f_nan
                 
              endif

           end
           
           's': *state.d.espec = *state.d.pspec

           'u': begin           ; Undo

              *state.d.pspec = *state.d.espec
              state.p.reg = !values.f_nan
              xcleanspec_plotupdate,state

           end

           'w': begin

              if state.p.plotwin eq 1 then begin

                 state.p.plot1xrange = state.p.plot1absxrange
                 state.p.plot1yrange = state.p.plot1absyrange

              endif else state.p.plot2yrange = state.p.plot2absyrange

              xcleanspec_plotupdate,state
              xcleanspec_setminmax,state

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

     'Load Spectrum': xcleanspec_loadspec,state

     'Output Format': state.r.textoutput=event.select

     'Path Button': begin

        path= dialog_pickfile(/DIRECTOR,DIALOG_PARENT=state.w.xcleanspec_base,$
                              TITLE='Select Path',/MUST_EXIST)
        
        if path ne '' then begin

           path = cpath(path,WIDGET_ID=state.w.xcleanspec_base,CANCEL=cancel)
           if cancel then goto,cont
           widget_control,state.w.path_fld[1],SET_VALUE = path
           setfocus,state.w.path_fld
           
        endif
     end

     'Plot Aperture': begin

        if state.r.freeze then goto, cont
        state.p.ap = event.index+1
        idx  = (state.p.order-(*state.r.orders)[0])*state.r.naps + $
               (state.p.ap-1)
        state.p.idx = idx

        state.p.plot1xrange = [min((*state.d.espec)[*,0,idx],MAX=max,/NAN),max]
        state.p.plot1yrange = [0,max((*state.d.espec)[*,1,idx],/NAN)]
        sn = (*state.d.espec)[*,1,idx]/(*state.d.espec)[*,2,idx]
        state.p.plot2yrange = [min(sn,MAX=max,/NAN),max]


        state.p.plot1absxrange = state.p.plot1xrange
        state.p.plot1absyrange = state.p.plot1yrange
        state.p.plot2absyrange = state.p.plot2yrange
        
        xcleanspec_plotupdate,state
        xcleanspec_setminmax,state
        
     end

     'Plot Atmosphere': begin
        
        state.p.plotatmos = event.select
        xcleanspec_plotupdate,state

     end

     'Plot Order': begin

        if state.r.freeze then goto, cont
        state.p.order = (*state.r.orders)[event.index]
        idx           = event.index*state.r.naps + (state.p.ap-1)
        state.p.idx   = idx

        state.p.plot1xrange = [min((*state.d.espec)[*,0,idx],MAX=max),max]
        state.p.plot1yrange = [0,max((*state.d.espec)[*,1,idx],/NAN)]
        sn = (*state.d.espec)[*,1,idx]/(*state.d.espec)[*,2,idx]
        state.p.plot2yrange = [min(sn,MAX=max,/NAN),max]

        state.p.plot1absxrange = state.p.plot1xrange
        state.p.plot1absyrange = state.p.plot1yrange
        state.p.plot2absyrange = state.p.plot2yrange
        
        xcleanspec_plotupdate,state
        xcleanspec_setminmax,state
        
     end

     'Smooth Spectra': xcleanspec_smoothspec,state

     'Smooth Type': begin

        widget_control, state.w.sg_base, MAP=0
        widget_control, state.w.gs_base, MAP=0

        if event.value eq 'Savitzky-Golay' then begin

           state.r.smoothtype = event.value
           widget_control, state.w.sg_base, MAP=1

        endif
        if event.value eq 'Gaussian' then begin

           state.r.smoothtype = event.value
           widget_control, state.w.gs_base, MAP=1
           
        endif

     end

     'Spectrum Type': begin

        state.p.spectype = event.value
        xcleanspec_plotupdate,state

     end
     'Write File': begin

        if state.r.freeze then goto, cont
        xcleanspec_writefile,state

     end
     
     else:

  endcase

cont: 

  widget_control, state.w.xcleanspec_base, SET_UVALUE=state, /NO_COPY
getout:

end
;
;******************************************************************************
;
pro xcleanspec_plotwinevent1, event

  widget_control, event.top, GET_UVALUE = state, /NO_COPY
  widget_control, event.id,  GET_UVALUE = uvalue

;  Check to see if it is a TRACKING event.

  if strtrim(tag_names(event,/STRUCTURE_NAME),2) eq 'WIDGET_TRACKING' then $
     begin

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

        'Fix': begin

           z = where(finite(state.p.reg) eq 1,count)
           if count eq 0 then begin
              
              state.p.reg[*,0] = xy[0:1] 

              wset, state.p.plotwin1_wid
              plots,[xy[0],xy[0]],(state.p.yscale1).crange,LINESTYLE=2,$
                    COLOR=7
              wset, state.p.pixmap1_wid
              plots,[xy[0],xy[0]],(state.p.yscale1).crange,LINESTYLE=2,$
                    COLOR=7
              
           endif else begin
              
              state.p.reg[*,1] = xy[0:1]
              xrange = reform(state.p.reg[0,sort(state.p.reg[0,*])])
              tabinv,(*state.d.espec)[*,0,state.p.idx],xrange,idx
              idx = round(idx)
              
              x = [(*state.d.espec)[idx[0],0,state.p.idx],$
                   (*state.d.espec)[idx[1],0,state.p.idx]]

              y = [(*state.d.espec)[idx[0],1,state.p.idx],$
                   (*state.d.espec)[idx[1],1,state.p.idx]]

              e = [(*state.d.espec)[idx[0],2,state.p.idx],$
                   (*state.d.espec)[idx[1],2,state.p.idx]]

              if state.p.spectype eq 0 then begin

                 coeff = poly_fit1d(x,y,1,/SILENT)
                 (*state.d.pspec)[idx[0]:idx[1],1,state.p.idx] = $
                    poly((*state.d.espec)[idx[0]:idx[1],0,state.p.idx],coeff)
                 
              endif
              coeff = poly_fit1d(x,e,1,/SILENT)
              (*state.d.pspec)[idx[0]:idx[1],2,state.p.idx] = $
                 poly((*state.d.espec)[idx[0]:idx[1],0,state.p.idx],coeff)
              
              xcleanspec_plotupdate,state
              state.r.cursormode = 'None'

           endelse

        end

        'Remove': begin

           z = where(finite(state.p.reg) eq 1,count)
           if count eq 0 then begin
              
              state.p.reg[*,0] = xy[0:1] 
              wset, state.p.plotwin1_wid
              plots,[xy[0],xy[0]],(state.p.yscale1).crange,LINESTYLE=2,$
                    COLOR=7
              wset, state.p.pixmap1_wid
              plots,[xy[0],xy[0]],(state.p.yscale1).crange,LINESTYLE=2,$
                    COLOR=7
              
           endif else begin
              
              state.p.reg[*,1] = xy[0:1]
              tabinv,(*state.d.espec)[*,0,state.p.idx],$
                     reform(state.p.reg[0,*]),idx
              
              idx = round(idx)                
              min = idx[0] < idx[1]
              max = idx[0] > idx[1]

              idx = findgen(n_elements((*state.d.pspec)[*,0,0]))
              amin = min(idx,MAX=amax)
              
              if state.p.spectype eq 0 then  begin
                 
                 (*state.d.pspec)[((min) > amin):((max) < amax),1,$
                                  state.p.idx] = !values.f_nan
                 
              endif

              (*state.d.pspec)[((min) > amin):((max) < amax),2,$
                               state.p.idx] = !values.f_nan

              xcleanspec_plotupdate,state
              state.r.cursormode = 'None'
              
           endelse

        end

        'Zoom': begin
           
           z = where(finite(state.p.reg) eq 1,count)
           if count eq 0 then state.p.reg[*,0] = xy[0:1] else begin 
              
              state.p.reg[*,1] = xy[0:1]
              state.p.plot1xrange   = [min(state.p.reg[0,*],MAX=max),max]
              state.p.plot1yrange   = [min(state.p.reg[1,*],MAX=max),max]
              xcleanspec_plotupdate,state
              xcleanspec_setminmax,state
              state.r.cursormode   = 'None'
              state.p.reg = !values.f_nan
              
           endelse

        end

        'XZoom': begin

           z = where(finite(state.p.reg) eq 1,count)
           if count eq 0 then begin
              
              state.p.reg[*,0] = xy[0:1]
              wset, state.p.pixmap1_wid
              plots, [event.x,event.x],[0,state.p.plot1size[1]],COLOR=2,$
                     /DEVICE,LINESTYLE=2
              wset, state.p.plotwin1_wid
              device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,$
                            0,state.p.pixmap1_wid]
              wset, state.p.pixmap2_wid
              plots, [event.x,event.x],[0,state.p.plot2size[1]],COLOR=2,$
                     /DEVICE,LINESTYLE=2
              wset, state.p.plotwin2_wid
              device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,$
                            0,state.p.pixmap2_wid]
              
           endif else begin

              state.p.reg[*,1] = xy[0:1]
              state.p.plot1xrange = [min(state.p.reg[0,*],max=m),m]
              state.r.cursormode = 'None'
              state.p.reg = !values.f_nan
              xcleanspec_plotupdate,state
              xcleanspec_setminmax,state
              
           endelse


        end

        'YZoom': begin

           z = where(finite(state.p.reg) eq 1,count)
           if count eq 0 then begin
              
              state.p.reg[*,0] = xy[0:1]
              wset, state.p.pixmap1_wid
              plots, [0,state.p.plot1size[0]],[event.y,event.y],COLOR=2,$
                     /DEVICE,LINESTYLE=2

              wset, state.p.plotwin1_wid
              device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,$
                            0,state.p.pixmap1_wid]
              
           endif else begin

              state.p.reg[*,1] = xy[0:1]
              state.p.plot1yrange = [min(state.p.reg[1,*],max=m),m]
              state.r.cursormode = 'None'
              state.p.reg[*] = !values.f_nan
              xcleanspec_plotupdate,state
              xcleanspec_setminmax,state
              
           endelse

        end

        else:

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
        plots, [0,state.p.plot1size[0]],[event.y,event.y],COLOR=2,/DEVICE
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

;  Update cursor position.
  
  if not state.r.freeze then begin

     tabinv, (*state.d.pspec)[*,0,state.p.idx],xy[0],idx
     idx = round(idx)
     label = 'Cursor X: '+strtrim(xy[0],2)+', Y:'+strtrim(xy[1],2)
     label = label+'   Spectrum X: '+$
             strtrim( (*state.d.pspec)[idx,0,state.p.idx],2)+$
             ', Y:'+strtrim( (*state.d.pspec)[idx,1,state.p.idx],2)
     widget_control,state.w.message,SET_VALUE=label

  endif

cont:

  widget_control, state.w.xcleanspec_base, SET_UVALUE=state, /NO_COPY
  
end
;
;******************************************************************************
;
pro xcleanspec_plotwinevent2, event

  widget_control, event.top, GET_UVALUE = state, /NO_COPY
  widget_control, event.id,  GET_UVALUE = uvalue

;  Check to see if it is a TRACKING event.

  if strtrim(tag_names(event,/STRUCTURE_NAME),2) eq 'WIDGET_TRACKING' then $
     begin

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
  wset, state.p.plotwin2_wid

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
               /DEVICE,LINESTYLE=2
        
        wset, state.p.plotwin2_wid
        device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,0,$
                      state.p.pixmap2_wid]
        
     endif else begin
        
        state.p.reg[*,1] = xy[0:1]
        state.p.plot2yrange = [min(state.p.reg[1,*],MAX=m),m]
        state.r.cursormode = 'None'
        state.p.reg = !values.f_nan
        xcleanspec_plotupdate,state
        xcleanspec_setminmax,state
        
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

;  Update cursor position.
  
  if not state.r.freeze then begin

     tabinv, (*state.d.pspec)[*,0,state.p.idx],xy[0],idx
     idx = round(idx)
     sn = (*state.d.pspec)[idx,1,state.p.idx]/$
          (*state.d.pspec)[idx,2,state.p.idx]


     label = 'Cursor X: '+strtrim(xy[0],2)+', Y:'+strtrim(xy[1],2)
     label = label+'   Spectrum X: '+$
             strtrim( (*state.d.pspec)[idx,0,state.p.idx],2)+$
             ', Y:'+strtrim(sn ,2)
     widget_control,state.w.message,SET_VALUE=label

  endif


cont:

  widget_control, state.w.xcleanspec_base, SET_UVALUE=state, /NO_COPY

end
;
;******************************************************************************
;
pro xcleanspec_minmax,event

  widget_control, event.top, GET_UVALUE = state, /NO_COPY
  widget_control, event.id,  GET_UVALUE = uvalue

  if state.r.freeze then goto, cont

  case uvalue of 

     'X Min': begin

        xmin = cfld(state.w.xmin_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then goto, cont
        xmin2 = crange(xmin,state.p.plot1xrange[1],'X Min',/KLT,$
                       WIDGET_ID=state.w.xcleanspec_base,CANCEL=cancel)
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
                       WIDGET_ID=state.w.xcleanspec_base,CANCEL=cancel)
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
                       WIDGET_ID=state.w.xcleanspec_base,CANCEL=cancel)
        if cancel then begin
           
           widget_control,state.w.ymin1_fld[0],$
                          SET_VALUE=state.p.plot1yrange[0]
           goto, cont
           
        endif else state.p.plot1yrange[0] = ymin2
        
     end
     'Y1 Max': begin

        ymax = cfld(state.w.ymax1_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then goto, cont
        ymax2 = crange(ymax,state.p.plot1yrange[0],'Y Max',/KGT,$
                       WIDGET_ID=state.w.xcleanspec_base,CANCEL=cancel)
        if cancel then begin
           
           widget_control,state.w.ymax1_fld[0],$
                          SET_VALUE=state.p.plot1yrange[1]
           goto, cont
           
        endif else state.p.plot1yrange[1] = ymax2
        
     end
     'Y2 Min': begin

        ymin = cfld(state.w.ymin2_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then goto, cont
        ymin2 = crange(ymin,state.p.plot2yrange[1],'Y Min',/KLT,$
                       WIDGET_ID=state.w.xcleanspec_base,CANCEL=cancel)
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
                       WIDGET_ID=state.w.xcleanspec_base,CANCEL=cancel)
        if cancel then begin
           
           widget_control,state.w.ymax2_fld[0],$
                          SET_VALUE=state.p.plot2yrange[1]
           goto, cont
           
        endif else state.p.plot2yrange[1] = ymax2
        
     end
     
  endcase

  xcleanspec_plotupdate,state

cont:
  widget_control, state.w.xcleanspec_base, SET_UVALUE=state, /NO_COPY

end
;
;******************************************************************************
;
pro xcleanspec_resize, event

  widget_control, event.top, GET_UVALUE=state,/NO_COPY
  widget_control, event.id,  GET_UVALUE=uvalue

  widget_control, state.w.xcleanspec_base,TLB_GET_SIZE=size

;  Window 1

  state.p.plot1size[0]=size[0]-state.p.buffer[0]
  state.p.plot1size[1]=(size[1]-state.p.buffer[1])*state.p.plot1scale

  widget_control, state.w.plotwin1,DRAW_XSIZE=state.p.plot1size[0]
  widget_control, state.w.plotwin1,DRAW_YSIZE=state.p.plot1size[1]

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

  if not state.r.freeze then xcleanspec_plotupdate,state

  widget_control, state.w.xcleanspec_base, SET_UVALUE=state, /NO_COPY

end
;
;******************************************************************************
;
; ------------------------------Main Program-------------------------------- 
;
;******************************************************************************
;
pro xcleanspec, path_in

  if xregistered('xcleanspec') then goto, cont

;  Load color table

  mkct
  device, RETAIN=2

  getosinfo,dirsep,strsep

  last        = strpos(!path,'Spextool')
  first       = strpos(!path,strsep,last,/REVERSE_SEARCH)
  packagepath = strmid(!path,first+1,last-first+8)

  readfmt,filepath('HI.dat',ROOT_DIR=packagepath,SUBDIR='data'),$
          'F9.7,4x,A12',hlines,hnames,SKIPLINE=3

  restore, filepath('atrans.sav',ROOT_DIR=packagepath,SUBDIR='data')

;  Build three structures which will hold important info.

  w = {aperture_dl:0L,$
       box3_base:0L,$
       dirsep:dirsep,$
       fwhm_fld:[0L,0L],$
       gs_base:0L,$
       gs_fld:[0L,0L],$
       keyboard:0L,$
       ispectrum_fld:[0L,0L],$
       message:0L,$
       oname_fld:[0L,0L],$
       order_dl:0L,$
       path_fld:[0L,0L],$
       plotwin1:0,$
       plotwin2:0,$
       sg_base:0L,$
       sgwin_fld:[0L,0L],$
       sgdeg_fld:[0L,0L],$
       xcleanspec_base:0L,$
       xmin_fld:[0L,0L],$
       xmax_fld:[0L,0L],$
       ymin1_fld:[0L,0L],$
       ymax1_fld:[0L,0L],$
       ymin2_fld:[0L,0L],$
       ymax2_fld:[0L,0L]}

  r = {cursormode:'None',$
       freeze:1,$
       maxres:0.,$
       resperpix:0.,$
       naps:0,$
       norders:0,$
       orders:ptr_new(fltarr(2)),$
       packagepath:packagepath,$
       plotlines:0,$
       slitw_pix:0.,$
       smoothtype:'Savitzky-Golay',$
       smthhistory:'',$
       start:0L,$
       stop:0L,$
       textoutput:0}

  d = {hdr:ptr_new(strarr(2)),$
       hlines:hlines,$
       hnames:hnames,$
       espec:ptr_new(fltarr(2)),$
       ospec:ptr_new(fltarr(2)),$
       pspec:ptr_new(fltarr(2))}

  p = {activespec:1,$
       ap:0,$
       atrans:atrans,$
       awave:awave,$
       buffer:[0.,0.],$
       idx:0,$
       order:0,$
       pixmap1_wid:0L,$
       pixmap2_wid:0L,$
       plotatmos:0,$
       plotwin1_wid:0L,$
       plotwin2_wid:0L,$
       plot1absxrange:[0.,0.],$
       plot1absyrange:[[0.,0.],[0.,0.]],$
       plot1scale:0.0,$
       plot1xrange:[0.,0.],$
       plot1yrange:[[0.,0.],[0.,0.]],$
       plot2scale:0.0,$
       plot1size:[670,280],$
       plot2absyrange:[0.,0.],$
       plot2yrange:[0.,0.],$
       plot2size:[670,280/1.4],$
       plotwin:1,$
       pscale1:!p,$
       xscale1:!x,$
       pscale2:!p,$
       xscale2:!x,$
       spectype:0,$
       xtitle:'',$
       yscale1:!y,$
       yscale2:!y,$
       ytitle:'',$
       reg:[[!values.f_nan,!values.f_nan],$
            [!values.f_nan,!values.f_nan]]}

  p.plot1scale = float(p.plot1size[1])/(p.plot1size[1]+p.plot2size[1])
  p.plot2scale = float(p.plot2size[1])/(p.plot1size[1]+p.plot2size[1])


;  Load the three structures in the state structure.

  state = {w:w,r:r,d:d,p:p}

;  Build the widget.

  getfonts,buttonfont,textfont
  
  state.w.xcleanspec_base = widget_base(TITLE='Xcleanspec', $
                                        /COLUMN,$
                                        /TLB_SIZE_EVENTS)

     quit_button = widget_button(state.w.xcleanspec_base,$
                                 FONT=buttonfont,$
                                 EVENT_PRO='xcleanspec_event',$
                                 VALUE='Quit',$
                                 UVALUE='Quit')
     
     state.w.keyboard = widget_text(state.w.xcleanspec_base, $
                                    /ALL_EVENTS, $
                                    SCR_XSIZE=1, $
                                    SCR_YSIZE=1, $
                                    UVALUE='Keyboard', $
                                    EVENT_PRO='xcleanspec_event',$
                                    VALUE= '')
  
     row_base = widget_base(state.w.xcleanspec_base,$
                            /ROW)

        col1_base = widget_base(row_base,$
                                EVENT_PRO='xcleanspec_event',$
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
              
                 button = widget_button(row,$
                                        FONT=buttonfont,$
                                        VALUE='Path',$
                                        UVALUE='Path Button')
                 
                 field = coyote_field2(row,$
                                       LABELFONT=buttonfont,$
                                       FIELDFONT=textfont,$
                                       TITLE=':',$
                                       UVALUE='Path Field',$
                                       VALUE=path_in, $
                                       XSIZE=25,$
                                       TEXTID=textid)
                 state.w.path_fld = [field,textid]    
                 
              row = widget_base(box1_base,$
                                /ROW,$
                                /BASE_ALIGN_CENTER)
              
                 input = widget_button(row,$
                                       FONT=buttonfont,$
                                       VALUE='Input Spectrum',$
                                       UVALUE='Input Spectrum')
                 
                 input_fld = coyote_field2(row,$
                                           LABELFONT=buttonfont,$
                                           FIELDFONT=textfont,$
                                           TITLE=':',$
                                           UVALUE='Input Spectrum Field',$
                                           XSIZE=15,$
                                           TEXTID=textid)
                 state.w.ispectrum_fld = [input_fld,textid]
                 
              load = widget_button(box1_base,$
                                   VALUE='Load Spectrum',$
                                   UVALUE='Load Spectrum',$
                                   FONT=buttonfont)
           
           box2_base = widget_base(col1_base,$
                                   /COLUMN,$
                                   /FRAME)
           
              label = widget_label(box2_base,$
                                   VALUE='2.  Edit Spectra',$
                                   FONT=buttonfont,$
                                   /ALIGN_LEFT)
              
              row = widget_base(box2_base,$
                                /ROW,$
                                /BASE_ALIGN_CENTER)
              
                 state.w.order_dl = widget_droplist(row,$
                                                    FONT=buttonfont,$
                                                    TITLE='Order:',$
                                                    VALUE='01',$
                                                    UVALUE='Plot Order')
                 
                 state.w.aperture_dl = widget_droplist(row,$
                                                       FONT=buttonfont,$
                                                       TITLE='Aperture:',$
                                                       VALUE='01',$
                                                       UVALUE='Plot Aperture')

                 bg = cw_bgroup(box2_base,$
                                FONT=buttonfont,$
                                ['Flux','Error'],$
                                /ROW,$
                                /RETURN_INDEX,$
                                /NO_RELEASE,$
                                /EXCLUSIVE,$
                                LABEL_LEFT='Spectrum:',$
                                UVALUE='Spectrum Type',$
                                SET_VALUE=0)
                 
           state.w.box3_base = widget_base(col1_base,$
                                           /COLUMN,$
                                           /FRAME)
           
              label = widget_label(state.w.box3_base,$
                                   VALUE='3.  Smooth Spectra',$
                                   FONT=buttonfont,$
                                   /ALIGN_LEFT)
              
              smooth_bg = cw_bgroup(state.w.box3_base,$
                                    FONT=buttonfont,$
                                    ['Savitzky-Golay','Gaussian'],$
                                    /ROW,$
                                    /RETURN_NAME,$
                                    /NO_RELEASE,$
                                    /EXCLUSIVE,$
                                    LABEL_LEFT='',$
                                    UVALUE='Smooth Type',$
                                    SET_VALUE=0)
              
              row = widget_base(state.w.box3_base)

                 state.w.sg_base = widget_base(row,$
                                               /ROW,$
                                               /BASE_ALIGN_CENTER)
                 
                    window = coyote_field2(state.w.sg_base,$
                                           LABELFONT=buttonfont,$
                                           FIELDFONT=textfont,$
                                           TITLE='SG Window:',$
                                           UVALUE='SG Window',$
                                           XSIZE=4,$
                                           TEXTID=textid)
                    state.w.sgwin_fld = [field,textid] 
                    
                    window = coyote_field2(state.w.sg_base,$
                                           LABELFONT=buttonfont,$
                                           FIELDFONT=textfont,$
                                           TITLE='Degree:',$
                                           UVALUE='SG Degree',$
                                           VALUE='2',$
                                           XSIZE=3,$
                                           TEXTID=textid)
                    state.w.sgdeg_fld = [field,textid] 
                    
                 state.w.gs_base = widget_base(row,$
                                               /ROW,$
                                               /BASE_ALIGN_CENTER,$
                                               MAP=0)
                 
                    field = coyote_field2(state.w.gs_base,$
                                          LABELFONT=buttonfont,$
                                          FIELDFONT=textfont,$
                                          TITLE='FWHM=',$
                                          UVALUE='FWHM',$
                                          XSIZE=10,$
                                          TEXTID=textid)
                    state.w.fwhm_fld = [field,textid] 
                    
              smooth_button = widget_button(state.w.box3_base,$
                                            FONT=buttonfont,$
                                            VALUE='Smooth Spectra',$
                                            UVALUE='Smooth Spectra')
              
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
                                EVENT_PRO='xcleanspec_event',$
                                /COLUMN)
        
           state.w.message = widget_text(col2_base, $
                                         YSIZE=1)
           
           row = widget_base(col2_base,$
                             /ROW,$
                             /BASE_ALIGN_CENTER,$
                             FRAME=1)
           
              hlines_bg = cw_bgroup(row,$
                                    ['Plot Hydrogen Lines'],$
                                    FONT=buttonfont,$
                                    UVALUE='Hydrogen Lines',$
                                    SET_VALUE=[0],$
                                    /NONEXCLUSIVE)
              
              hlines_bg = cw_bgroup(row,$
                                    ['Plot Atmosphere'],$
                                    FONT=buttonfont,$
                                    UVALUE='Plot Atmosphere',$
                                    SET_VALUE=[0],$
                                    /NONEXCLUSIVE)
              
           state.w.plotwin1 = widget_draw(col2_base,$
                                          XSIZE=state.p.plot1size[0],$
                                          YSIZE=state.p.plot1size[1],$
                                          /TRACKING_EVENTS,$
                                          /MOTION_EVENTS,$
                                         EVENT_PRO='xcleanspec_plotwinevent1',$
                                          UVALUE='Plot Window 1')
           
           row_base = widget_base(col2_base,$
                                  /ROW,$
                                  FRAME=1)
           
              xmin = coyote_field2(row_base,$
                                   LABELFONT=buttonfont,$
                                   FIELDFONT=textfont,$
                                   TITLE='X Min:',$
                                   UVALUE='X Min',$
                                   XSIZE=12,$
                                   EVENT_PRO='xcleanspec_minmax',$
                                   /CR_ONLY,$
                                   TEXTID=textid)
              state.w.xmin_fld = [xmin,textid]
              
              xmax = coyote_field2(row_base,$
                                   LABELFONT=buttonfont,$
                                   FIELDFONT=textfont,$
                                   TITLE='X Max:',$
                                   UVALUE='X Max',$
                                   XSIZE=12,$
                                   EVENT_PRO='xcleanspec_minmax',$
                                   /CR_ONLY,$
                                   TEXTID=textid)
              state.w.xmax_fld = [xmax,textid]
              
              ymin = coyote_field2(row_base,$
                                   LABELFONT=buttonfont,$
                                   FIELDFONT=textfont,$
                                   TITLE='Y Min:',$
                                   UVALUE='Y1 Min',$
                                   XSIZE=12,$
                                   EVENT_PRO='xcleanspec_minmax',$
                                   /CR_ONLY,$
                                   TEXTID=textid)
              state.w.ymin1_fld = [ymin,textid]
              
              ymax = coyote_field2(row_base,$
                                   LABELFONT=buttonfont,$
                                   FIELDFONT=textfont,$
                                   TITLE='Y Max:',$
                                   UVALUE='Y1 Max',$
                                   XSIZE=12,$
                                   EVENT_PRO='xcleanspec_minmax',$
                                   /CR_ONLY,$
                                   TEXTID=textid)
              state.w.ymax1_fld = [ymax,textid]
              
           state.w.plotwin2 = widget_draw(col2_base,$
                                          XSIZE=state.p.plot2size[0],$
                                          YSIZE=state.p.plot2size[1],$
                                          /TRACKING_EVENTS,$
                                          /MOTION_EVENTS,$
                                         EVENT_PRO='xcleanspec_plotwinevent2',$
                                          UVALUE='Plot Window 2')
           
           row_base = widget_base(col2_base,$
                                  /ROW,$
                                  FRAME=1)
           
              ymin = coyote_field2(row_base,$
                                   LABELFONT=buttonfont,$
                                   FIELDFONT=textfont,$
                                   TITLE='Y Min:',$
                                   UVALUE='Y2 Min',$
                                   XSIZE=12,$
                                   EVENT_PRO='xcleanspec_minmax',$
                                   /CR_ONLY,$
                                   TEXTID=textid)
              state.w.ymin2_fld = [ymin,textid]
              
              ymax = coyote_field2(row_base,$
                                   LABELFONT=buttonfont,$
                                   FIELDFONT=textfont,$
                                   TITLE='Y Max:',$
                                   UVALUE='Y2 Max',$
                                   XSIZE=12,$
                                   EVENT_PRO='xcleanspec_minmax',$
                                   /CR_ONLY,$
                                   TEXTID=textid)
              state.w.ymax2_fld = [ymax,textid]
              
   button = widget_button(state.w.xcleanspec_base,$
                          FONT=buttonfont,$
                          EVENT_PRO='xcleanspec_event',$
                          VALUE='Help',$
                          UVALUE='Help')
   
; Get things running.  Center the widget using the Fanning routine.
  
  centertlb,state.w.xcleanspec_base
  widget_control, state.w.xcleanspec_base, /REALIZE

;  Get plotwin ids
  
  widget_control, state.w.plotwin1, GET_VALUE=x
  state.p.plotwin1_wid = x
  widget_control, state.w.plotwin2, GET_VALUE=x
  state.p.plotwin2_wid = x
  
;  Create pixmap windows

  window, /FREE, /PIXMAP,XSIZE=state.p.plot1size[0],YSIZE=state.p.plot1size[1]
  state.p.pixmap1_wid = !d.window

  window, /FREE, /PIXMAP,XSIZE=state.p.plot2size[0],YSIZE=state.p.plot2size[1]
  state.p.pixmap2_wid = !d.window

;  Get sizes for things.

  widget_geom = widget_info(state.w.xcleanspec_base, /GEOMETRY)
  state.p.buffer[0]=widget_geom.xsize-state.p.plot1size[0]
  state.p.buffer[1]=widget_geom.ysize-state.p.plot1size[1]-$
                    state.p.plot2size[1]

; Start the Event Loop. This will be a non-blocking program.

  XManager, 'xcleanspec', $
            state.w.xcleanspec_base, $
            CLEANUP='xcleanspec_cleanup',$
            EVENT_HANDLER='xcleanspec_resize',$
            /NO_BLOCK

; Put state variable into the user value of the top level base.

  widget_control, state.w.xcleanspec_base, SET_UVALUE=state, /NO_COPY

cont:

end
