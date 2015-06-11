;+
; NAME:
;     xmc_linepos
;
; PURPOSE:
;     To compute the moments of an absorption/emission line
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     xmc_linepos
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
;     None
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
;     Requires the IDL Astronomy User's Library and the Spextool package.
;
; PROCEDURE:
;     The user loads a Spextool FITS file into the program.  The
;     spectrum around the line is used to determine the continuum
;     spectrum and then the line moments are computed (see mcc_linemoments.pro)
;
;     M0_flux = int F Dlambda
;     M1_flux = int lambda F Dlambda / M0_flux
;     M2_flux = sqrt (abs(int (lambda-M1_flux)^2 F Dlambda)/M0_flux)
;
;     M0_net = int (F-C) Dlambda
;     M1_net = int lambda (F-C) Dlambda / M0_net
;     M2_net = sqrt (abs(int (lambda-M1_flux)^2 (F-C) Dlambda)/M0_net)
;  
;     M0_cont = int C Dlambda
;
;     EW = int (1-F/C) Dlambda
;
;
;     The user can either load a single Spextool FITS for a a list of
;     FITS files.  
;
;     If the latter, then the format for the text file is
;     
;     file objectname 
;
;     e.g., ~mcushing/Gl411.fits Gl411
;   
;     The user can then select a single line and determine its moments
;     and line selection regions for each object if necessary.  
;     for all the spectra, adjusting both the normalization regions.
;     The output text file has the same ordering as the DISPLAY Window
;     except the first three columns are the name of the object, the
;     lower line limit and the upper line limit.
;  
; EXAMPLE:
;     None
;
; MODIFICATION HISTORY:
;     2002 - Written by M. Cushing, Institute for Astronomy, UH
;-
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xmc_linepos_chunits,state,CANCEL=cancel

cancel = 0

;  First convert wavelength to microns

case state.r.wunits of 

   0: begin

      state.p.xtitle = '!5!7k!5 (!7l!5m)'
      state.p.wunit = '(um)'

   end
   1: begin

      state.r.wscale = 1./10000.
      state.p.xtitle = '!7k!5 (!5!sA!r!u!9 %!5!n)'
      state.p.wunit = '(A)'

   end
   2: begin

      state.r.wscale = 1./1000.
      state.p.xtitle = '!7k!5 (!5nm)'
      state.p.wunit = '(nm)'

   end
endcase

case state.r.funits of 

   0: begin

      state.p.ytitle = '!5f!D!7k!N!5 (W m!E-2!N !7l!5m!E-1!N)'
      state.p.funit  = '(W m-2)'

   end

   1: begin

      ang = '!5!sA!r!u!9 %!5!n'
      state.p.ytitle = '!5f!D!7k!N!5 (ergs s!E-1!N cm!E-2!N '+ang+'!E-1!N)'
      state.p.funit  = '(ergs s-1 cm-2)'
      state.r.wscale = state.r.wscale*10000.

   end

endcase

print, state.p.funit

end
;
;******************************************************************************
;
pro xmc_linepos_definenormreg,state

range = reform(state.p.reg[0,sort(state.p.reg[0,*])])
if state.r.nnormreg eq 0 then begin

    *state.r.normreg = range 
    state.r.nnormreg = 1

endif else begin

    *state.r.normreg = [[*state.r.normreg],[range]]
    state.r.nnormreg = state.r.nnormreg + 1

endelse

end
;
;******************************************************************************
;
pro xmc_linepos_displayinfo,state



if not xregistered('xinfo') then begin

    state.w.xinfo_base = widget_base(GROUP_LEADER=state.w.xmc_linepos_base, $
                                     /COLUMN,$
                                     /FLOATING,$
                                     TITLE='Xmc_Linepos Results')
    
    row = widget_base(state.w.xinfo_base,$
                      /ROW)
    
;  Set up labels

       state.w.lbl_lbl = widget_text(row,$
                                     XSIZE=25,$
                                     FONT=state.w.textfont,$
                                     YSIZE=16)
                   
       state.w.vals_lbl = widget_text(row,$
                                      XSIZE=20,$
                                      FONT=state.w.textfont,$
                                      YSIZE=16)
       
       state.w.errs_lbl = widget_text(row,$
                                      XSIZE=20,$
                                      FONT=state.w.textfont,$
                                      YSIZE=16)
                    
       done = widget_button(state.w.xinfo_base,$
                           VALUE='Done',$
                           FONT=state.w.buttonfont,$
                           UVALUE='Done Display')



       widget_control, state.w.xinfo_base, /REALIZE

; Start the Event Loop. This will be a non-blocking program.
       
       XManager, 'xinfo', $
                 /NO_BLOCK,$
                 state.w.xinfo_base,$
                 EVENT_HANDLER='xmc_linepos_info_event'
      

endif 

value = ['Moment','-------',' ',$
         'Min Wave '+state.p.wunit, $
         'Max Wave '+state.p.wunit, $
         ' ',$
         'M0_flux '+state.p.funit, $
         'M1_flux '+state.p.wunit,$
         'sig_flux '+state.p.wunit, $
         ' ',$
         'M0_net '+state.p.funit, $
         'M1_net '+state.p.wunit,$
         'sig_net '+state.p.wunit, $
         ' ',$
         'EW (A)', $
         'M0_cont '+state.p.funit]


widget_control, state.w.lbl_lbl,SET_VALUE=value


strlog     = strarr(16)
strlog[0]  = 'Value'
strlog[1]  = '-----'
strlog[2]  = ''
strlog[3]  = strtrim((*state.d.tmpdata)[0,0],2)
strlog[4]  = strtrim((*state.d.tmpdata)[1,0],2)
strlog[5]  = ' '
strlog[6]  = strtrim((*state.d.tmpdata)[2,0],2)
strlog[7]  = strtrim((*state.d.tmpdata)[3,0],2)
strlog[8]  = strtrim((*state.d.tmpdata)[4,0],2)
strlog[9]  = ' '
strlog[10]  = strtrim((*state.d.tmpdata)[5,0],2)
strlog[11]  = strtrim((*state.d.tmpdata)[6,0],2)
strlog[12]  = strtrim((*state.d.tmpdata)[7,0],2)
strlog[13] = ' '
strlog[14] = strtrim((*state.d.tmpdata)[8,0],2)
strlog[15] = strtrim((*state.d.tmpdata)[9,0],2)

widget_control, state.w.vals_lbl,SET_VALUE=strlog


strlog     = strarr(16)
strlog[0]  = 'Error'
strlog[1]  = '-----'
strlog[2]  = ''
strlog[3]  = 'NA'
strlog[4]  = 'NA'
strlog[5]  = ' '
strlog[6]  = strtrim((*state.d.tmpdata)[2,1],2)
strlog[7]  = strtrim((*state.d.tmpdata)[3,1],2)
strlog[8]  = strtrim((*state.d.tmpdata)[4,1],2)
strlog[9]  = ' '
strlog[10]  = strtrim((*state.d.tmpdata)[5,1],2)
strlog[11]  = strtrim((*state.d.tmpdata)[6,1],2)
strlog[12]  = strtrim((*state.d.tmpdata)[7,1],2)
strlog[13] = ' '
strlog[14] = strtrim((*state.d.tmpdata)[8,1],2)
strlog[15] = strtrim((*state.d.tmpdata)[9,1],2)

widget_control, state.w.errs_lbl,SET_VALUE=strlog

end
;
;******************************************************************************
;
pro xmc_linepos_fit,state

  z = where(*state.d.w gt state.r.linereg[0] and $
            *state.d.w lt state.r.linereg[1])

  result = gaussfit(double((*state.d.w)[z]),double((*state.d.ny)[z]),a, $
                    NTERMS=4)

  print, a[1]

  *state.p.overlay = [[(*state.d.w)[z]],[[result]]]



;  moments = mc_lineparms(*state.d.w*state.r.wscale,*state.d.y, $
;                         *state.d.c,state.r.linereg[0]*state.r.wscale,$
;                         state.r.linereg[1]*state.r.wscale,EFLUX=*state.d.ye,$
;                         ECONT=*state.d.ce,ERRORS=merr,CANCEL=cancel)
;  if cancel then return
;  
;;  Scale results
;  
;  moments[1:2] = moments[1:2]/state.r.wscale
;  merr[1:2] = merr[1:2]/state.r.wscale
;  
;  moments[4:6] = moments[4:6]/state.r.wscale
;  merr[4:6] = merr[4:6]/state.r.wscale
;  
;  
;  *state.d.tmpdata = [[state.r.linereg,moments],$
;                      [!values.f_nan,!values.f_nan,merr]]
;  
;;  Display results
;
;  if state.r.displayinfo then xmc_linepos_displayinfo,state
;  
;  label = 'EW '+state.p.wunit+'= '+strtrim((*state.d.tmpdata)[8,0],2)+' +- '+$
;          strtrim((*state.d.tmpdata)[8,1],2)
;  widget_control, state.w.ewlabel, SET_VALUE=label
  
end
;
;******************************************************************************
;
pro xmc_linepos_loadspec,state

  file = cfld(state.w.ifile_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return
  file = cfile(file,CANCEL=cancel)
  if cancel then return


  if state.r.itype eq 0 then begin

     mc_readspec,file,spc,hdr,obsmode,start,stop,norders,naps,orders,$
                 xunits,yunits,slith_pix,slith_arc,slitw_pix,slitw_arc,$
                 rp,airmass,CANCEL=cancel

  endif else begin

     readcol,file,files,names,FORMAT='A,A'

     *state.r.files   = files
     *state.r.names   = names
     
     mc_readspec,files[0],spc,hdr,obsmode,start,stop,norders,naps,orders,$
                 xunits,yunits,slith_pix,slith_arc,slitw_pix,slitw_arc,$
                 rp,airmass,CANCEL=cancel

     state.p.specname = names[0]

;  Make line data array

     *state.d.linedata = fltarr(10,n_elements(files)*2)+!values.f_nan
     state.r.specidx = 0

  endelse

  *state.d.w  = reform(spc[*,0])
  *state.d.f  = reform(spc[*,1])
  *state.d.fe = reform(spc[*,2])
  *state.d.y  = *state.d.f
  *state.d.ye = *state.d.fe

;  Check units

  xmc_linepos_chunits,state,CANCEL=cancel

  state.p.absxrange1 = [min(*state.d.w,MAX=max,/NAN),max]
  state.p.absxrange2 = state.p.absxrange1
  state.p.absyrange1 = [min(*state.d.f,MAX=max,/NAN),max]

  state.p.xrange1 = state.p.absxrange1
  state.p.xrange2 = state.p.absxrange1
  state.p.yrange1 = state.p.absyrange1

;  Zero things out in case of a reload    

  *state.r.normreg    = !values.f_nan
  *state.d.normcoeffs = !values.f_nan
  *state.p.goodbad    = !values.f_nan
  state.r.linereg     = !values.f_nan
  state.r.nnormreg    = 0
  state.r.lineidx     = 0
  state.r.cursormode  = 'None'

;  widget_control, state.w.ewlabel,SET_VALUE='EW:'

;  Update HI wavelength units

  state.d.hlines = state.d.hlines/state.r.wscale

;  Unfreeze widget and plot the spectrum

  state.p.freeze = 0
  xmc_linepos_setminmax,state
  xmc_linepos_plotupdate,state

cont:

end
;
;******************************************************************************
;
pro xmc_linepos_normspec,state,CANCEL=cancel

cancel = 0

;  Construct goodbad array

ndat = n_elements(*state.d.w)
goodbad = intarr(ndat)

z = where(finite(*state.r.normreg) eq 1,count)
if count eq 0 then begin

    print, 'Please select a region.'
    return

endif

hthresh = cfld(state.w.hthresh_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
lthresh = cfld(state.w.lthresh_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return

for i = 0, state.r.nnormreg-1 do begin

    tabinv,*state.d.w,(*state.r.normreg)[*,i],idx
    s = sort(idx)
    idx = idx[s]
    goodbad[round(idx[0]):round(idx[1])] = 1

endfor

;  Now fit the data

z  = where(goodbad eq 1,count)
x  = (*state.d.w)[z]
y  = (*state.d.f)[z]
if state.r.properrs eq 1 then ye =  (*state.d.fe)[z]

*state.d.normcoeffs = mcc_fitcont(x,y,state.r.normdeg,hthresh,-1.0*lthresh,$
                                  0.05,YERR=ye,/GAUSSJ,COVAR=covar,SILENT=1,$
                                  OGOODBAD=ogoodbad,CANCEL=cancel)
if cancel then return

if not state.r.properrs then begin

;  Determine the RMS of the fit using the DOF instead N-1 and scale covar
    
    good  = where(ogoodbad eq 1,cnt_good)
    dof   = cnt_good-(state.r.normdeg+1)
    rms   = sqrt( total((y[good]-poly(x[good],*state.d.normcoeffs))^2)/dof)
    ye    = replicate(rms,n_elements(*state.d.w))
    covar = covar*rms^2
    *state.d.ye = ye    

endif else *state.d.ye = *state.d.fe

;  Evaluate continuum and error.
    
cont         = poly1d(*state.d.w,*state.d.normcoeffs,covar,YVAR=cont_var)
*state.d.c   = cont
*state.d.ce  = sqrt(cont_var)


*state.d.ny  = *state.d.y/cont

;  Store points used in fit to display
    
tmp = intarr(n_elements(*state.d.w))
tmp[z] = ogoodbad
    
*state.p.goodbad = tmp
    
tabinv,*state.d.w,state.p.xrange1,idx
state.p.yrange2    = [min((*state.d.ny)[idx[0]:idx[1]]*0.85,/NAN),1.1]
state.p.absyrange2 = state.p.yrange2
   

cont:
    
end
;
;******************************************************************************
;
pro xmc_linepos_plotnormspec,state

plot, *state.d.w,*state.d.ny,/XSTY,/YSTY,XRANGE=state.p.xrange1,$
      YRANGE=state.p.yrange2,PSYM=10,YTITLE='!5Normalized Flux',$
      CHARSIZE=mc_strsize('!5A',0.01)

plots,!x.crange,[1,1],COLOR=3

plots,[state.r.linereg[0],state.r.linereg[0]],!y.crange,COLOR=7,LINESTYLE=2
plots,[state.r.linereg[1],state.r.linereg[1]],!y.crange,COLOR=7,LINESTYLE=2

if finite((*state.p.overlay)[0]) then $
oplot,(*state.p.overlay)[*,0],(*state.p.overlay)[*,1],COLOR=3,PSYM=10

end
;
;******************************************************************************
;
pro xmc_linepos_plotspec,state

plot, *state.d.w,*state.d.f,/XSTY,/YSTY,XRANGE=state.p.xrange1,$
      YRANGE=state.p.yrange1,PSYM=10,TITLE=state.p.specname,$
      CHARSIZE=mc_strsize('!5A',0.01),XTITLE=state.p.xtitle,$
      YTITLE=state.p.ytitle

;  Now plot the normalization region
    
base = state.p.yrange1[0]+0.1*(state.p.yrange1[1]-state.p.yrange1[0])

for i = 0, state.r.nnormreg-1 do begin
    
    if finite((*state.r.normreg)[0,i]) eq 1 then begin
        
        tabinv,*state.d.w,(*state.r.normreg)[0,i],idx
        plots,[(*state.r.normreg)[0,i],(*state.r.normreg)[0,i]],$
          [base,(*state.d.f)[idx]],COLOR=4,LINESTYLE=2
        
    endif
    if finite((*state.r.normreg)[1,i]) eq 1 then begin
       
        tabinv,*state.d.w,(*state.r.normreg)[1,i],idx
        plots,[(*state.r.normreg)[1,i],(*state.r.normreg)[1,i]],$
          [base,(*state.d.f)[idx]],COLOR=4,LINESTYLE=2
        
    endif
    
    plots,[(*state.r.normreg)[0,i],(*state.r.normreg)[1,i]],$
      [base,base],COLOR=4
    
endfor


if n_elements(*state.p.goodbad) gt 1 then begin

    x = (*state.d.w)
    y = (*state.d.f)
    

    z = where(*state.p.goodbad eq 0)

    y[z] = !values.f_nan
    oplot,x,y,COLOR=4,PSYM=10

endif

if finite((*state.d.normcoeffs)[0]) eq 1 then $
  oplot,*state.d.w,poly(*state.d.w,*state.d.normcoeffs),COLOR=3

plots,[state.r.linereg[0],state.r.linereg[0]],!y.crange,COLOR=7,LINESTYLE=2
plots,[state.r.linereg[1],state.r.linereg[1]],!y.crange,COLOR=7,LINESTYLE=2

if state.p.plotlines then begin

;  Label H lines if requested
    
    z = where(state.d.hlines lt state.p.xrange1[1] and $
              state.d.hlines gt state.p.xrange1[0],count)
    
    for i =0, count-1 do begin
        
        tabinv,*state.d.w,(state.d.hlines)[z[i]],idx
        plots, [(state.d.hlines)[z[i]],(state.d.hlines)[z[i]]],$
          [(*state.d.f)[idx],!y.crange[1]],LINESTYLE=1,COLOR=3,THICK=2
        
        xy = convert_coord((state.d.hlines)[z[i]],1,/DATA,/TO_NORM)
        name = '!5'+(state.d.hnames)[z[i]]
        xyouts, xy[0],0.87,name,ORIENTATION=90,/NORM,COLOR=3
        
    endfor

endif


end
;
;******************************************************************************
;

pro xmc_linepos_plotupdate,state

wset, state.p.pixmap1_wid
xmc_linepos_plotspec,state

wset, state.p.plotwin1_wid
device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
              state.p.pixmap1_wid]

state.p.pscale1 = !p
state.p.xscale1 = !x
state.p.yscale1 = !y

if finite((*state.d.normcoeffs)[0]) eq 1 then begin

    wset, state.p.pixmap2_wid
    xmc_linepos_plotnormspec,state
    
    wset, state.p.plotwin2_wid
    device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,0,$
                  state.p.pixmap2_wid]
    
    state.p.pscale2 = !p
    state.p.xscale2 = !x
    state.p.yscale2 = !y

endif else begin

    wdelete,state.p.pixmap2_wid

    window, /FREE, /PIXMAP, XSIZE=state.p.plot2size[0],$
      YSIZE=state.p.plot2size[1]
    state.p.pixmap2_wid = !d.window

    wset, state.p.plotwin2_wid
    device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,0,$
                  state.p.pixmap2_wid]

endelse

end
;
;******************************************************************************
;
pro xmc_linepos_setminmax,state

widget_control, state.w.xmin1_fld[1],SET_VALUE=strtrim(state.p.xrange1[0],2)
widget_control, state.w.xmax1_fld[1],SET_VALUE=strtrim(state.p.xrange1[1],2)
widget_control, state.w.ymin1_fld[1],SET_VALUE=strtrim(state.p.yrange1[0],2)
widget_control, state.w.ymax1_fld[1],SET_VALUE=strtrim(state.p.yrange1[1],2)

if finite((*state.d.normcoeffs)[0]) eq 1 then begin

    ymin = strtrim(state.p.yrange2[0],2)
    ymax = strtrim(state.p.yrange2[1],2)


endif else begin

    ymin = '' & ymax = ''

endelse

widget_control, state.w.ymin2_fld[1],SET_VALUE=ymin
widget_control, state.w.ymax2_fld[1],SET_VALUE=ymax

end
;
;******************************************************************************
;
pro xmc_linepos_stepspec,state,SKIP=skip,DONE=done

if keyword_set(SKIP) ne 1 or keyword_set(DONE) ne 1 then $
  (*state.d.linedata)[*,(state.r.specidx*2):(state.r.specidx*2+1)] = $
  *state.d.tmpdata

state.r.specidx = state.r.specidx+1

if state.r.specidx eq n_elements(*state.r.files) or keyword_set(DONE) then $
  begin

    command = 'End of list, write file out.'

    wset, state.p.plotwin1_wid
    erase
    xyouts,0.5,0.5,command,/NORM,CHARSIZE=3,ALIGN=0.5
    wset, state.p.pixmap1_wid
    erase
    xyouts,0.5,0.5,command,/NORM,CHARSIZE=3,ALIGN=0.5
    
    wset, state.p.plotwin2_wid
    erase
    wset, state.p.pixmap2_wid
    erase   
    return

endif

mc_readspec,(*state.r.files)[state.r.specidx],spc,hdr,obsmode,start,stop,$
            norders,naps,orders,xunits,yunits,slith_pix,slith_arc,slitw_pix, $
            slitw_arc,rp,airmass,CANCEL=cancel
*state.d.w  = reform(spc[*,0])
*state.d.f  = reform(spc[*,1])
*state.d.fe = reform(spc[*,2])
*state.d.y  = *state.d.f
*state.d.ye = *state.d.fe

state.p.specname = (*state.r.names)[state.r.specidx]

state.p.absxrange1 = [min(*state.d.w,MAX=max,/NAN),max]
state.p.absxrange2 = state.p.absxrange1
state.p.absyrange1 = [min(*state.d.f,MAX=max,/NAN),max]

tabinv,*state.d.w,state.p.xrange1,idx
state.p.yrange1 = [0.9*min((*state.d.f)[idx[0]:idx[1]],/NAN,MAX=max),1.1*max]

state.r.cursormode  = 'None'

xmc_linepos_normspec,state
xmc_linepos_setminmax,state
xmc_linepos_plotupdate,state

if finite(state.r.linereg[0]) eq 1 then xmc_linepos_ew,state



end
;
;******************************************************************************
;
pro xmc_linepos_whichlinereg,state

del = state.p.xrange1[1]-state.p.xrange1[0]

if state.p.reg[0,0] gt state.r.linereg[0]-(del*0.005) and $
  state.p.reg[0,0] lt state.r.linereg[0]+(del*0.005) then begin
    
    state.r.linereg[0] = !values.f_nan
    state.p.modlinereg = 1
    xmc_linepos_plotupdate,state
    
endif

if state.p.reg[0,0] gt state.r.linereg[1]-(del*0.005) and $
  state.p.reg[0,0] lt state.r.linereg[1]+(del*0.005) then begin
    
    state.r.linereg[1] = !values.f_nan
    state.p.modlinereg = 1
    xmc_linepos_plotupdate,state
    
endif

end
;
;******************************************************************************
;
pro xmc_linepos_whichnormline,state

del = state.p.xrange1[1]-state.p.xrange1[0]


for i = 0,state.r.nnormreg-1 do begin

    if state.p.reg[0,0] gt (*state.r.normreg)[0,i]-(del*0.005) and $
      state.p.reg[0,0] lt (*state.r.normreg)[0,i]+(del*0.005) then begin
    
        (*state.r.normreg)[0,i] = !values.f_nan
        state.p.modnormreg = 1
        xmc_linepos_plotupdate,state

    endif

    if state.p.reg[0,0] gt (*state.r.normreg)[1,i]-(del*0.005) and $
      state.p.reg[0,0] lt (*state.r.normreg)[1,i]+(del*0.005) then begin
    
        (*state.r.normreg)[1,i] = !values.f_nan
        state.p.modnormreg = 1
        xmc_linepos_plotupdate,state

    endif

endfor

end
;
;******************************************************************************
;
pro xmc_linepos_writefile,state

ofile = cfld(state.w.ofile_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

openw,lun,ofile,/GET_LUN,WIDTH=250

if state.r.itype then begin

    format = '(A'+strtrim(max(strlen(*state.r.names)),2)+')'
        
    for i = 0,n_elements(*state.r.files)-1 do begin
        
        if finite((*state.d.linedata)[0,i*2]) eq 0 then goto, cont
        
        printf, lun, string((*state.r.names)[i],FORMAT=format),$
                (*state.d.linedata)[0,i*2],(*state.d.linedata)[1,i*2],$
                (*state.d.linedata)[2,i*2],(*state.d.linedata)[2,i*2+1],$
                (*state.d.linedata)[3,i*2],(*state.d.linedata)[3,i*2+1],$
                (*state.d.linedata)[4,i*2],(*state.d.linedata)[4,i*2+1],$
                (*state.d.linedata)[5,i*2],(*state.d.linedata)[5,i*2+1],$
                (*state.d.linedata)[6,i*2],(*state.d.linedata)[6,i*2+1],$
                (*state.d.linedata)[7,i*2],(*state.d.linedata)[7,i*2+1],$
                (*state.d.linedata)[8,i*2],(*state.d.linedata)[8,i*2+1],$
                (*state.d.linedata)[9,i*2],(*state.d.linedata)[9,i*2+1]
        
        
        cont:
        
    endfor
    
endif else begin

    printf, lun, (*state.d.tmpdata)[0,0],(*state.d.tmpdata)[0,1],$
            (*state.d.tmpdata)[1,0],(*state.d.tmpdata)[1,1],$
            (*state.d.tmpdata)[2,0],(*state.d.tmpdata)[2,1],$
            (*state.d.tmpdata)[3,0],(*state.d.tmpdata)[3,1],$
            (*state.d.tmpdata)[4,0],(*state.d.tmpdata)[4,1],$
            (*state.d.tmpdata)[5,0],(*state.d.tmpdata)[5,1],$
            (*state.d.tmpdata)[6,0],(*state.d.tmpdata)[6,1],$
            (*state.d.tmpdata)[7,0],(*state.d.tmpdata)[7,1],$
            (*state.d.tmpdata)[8,0],(*state.d.tmpdata)[8,1],$
            (*state.d.tmpdata)[9,0],(*state.d.tmpdata)[9,1]





endelse

free_lun,lun

end
;
;******************************************************************************
;
pro xmc_linepos_zoom,state,IN=in,OUT=out

case state.p.plotwin of 

    1: begin

        delabsx = state.p.absxrange1[1]-state.p.absxrange1[0]
        delx    = state.p.xrange1[1]-state.p.xrange1[0]
        
        delabsy = state.p.absyrange1[1]-state.p.absyrange1[0]
        dely    = state.p.yrange1[1]-state.p.yrange1[0]
        
        xcen = state.p.xrange1[0]+delx/2.
        ycen = state.p.yrange1[0]+dely/2.
        
        case state.r.cursormode of 
            
            'XZoom': begin
                
                z = alog10(delabsx/delx)/alog10(2)
                if keyword_set(IN) then z = z+1 else z=z-1
                hwin = delabsx/2.^z/2.
                state.p.xrange1 = [xcen-hwin,xcen+hwin]
                
            end
            
            'YZoom': begin
                
                z = alog10(delabsy/dely)/alog10(2)
                if keyword_set(IN) then z = z+1 else z=z-1
                hwin = delabsy/2.^z/2.
                state.p.yrange1 = [ycen-hwin,ycen+hwin]
                
            end
            
            'Zoom': begin
                
                z = alog10(delabsx/delx)/alog10(2)
                if keyword_set(IN) then z = z+1 else z=z-1
                hwin = delabsx/2.^z/2.
                state.p.xrange1 = [xcen-hwin,xcen+hwin]
                
                z = alog10(delabsy/dely)/alog10(2)
                if keyword_set(IN) then z = z+1 else z=z-1
                hwin = delabsy/2.^z/2.
                state.p.yrange1 = [ycen-hwin,ycen+hwin]
                
            end
            
            else:
            
        endcase
        
    end

    2: begin

        delabsx = state.p.absxrange2[1]-state.p.absxrange2[0]
        delx    = state.p.xrange2[1]-state.p.xrange2[0]
        
        delabsy = state.p.absyrange2[1]-state.p.absyrange2[0]
        dely    = state.p.yrange2[1]-state.p.yrange2[0]
        
        xcen = state.p.xrange1[0]+delx/2.
        ycen = state.p.yrange2[0]+dely/2.
        
        case state.r.cursormode of 

            'YZoom': begin
                
                z = alog10(delabsy/dely)/alog10(2)
                if keyword_set(IN) then z = z+1 else z=z-1
                hwin = delabsy/2.^z/2.
                state.p.yrange2 = [ycen-hwin,ycen+hwin]
                
            end

            'Zoom': begin
                
                z = alog10(delabsx/delx)/alog10(2)
                if keyword_set(IN) then z = z+1 else z=z-1
                hwin = delabsx/2.^z/2.
                state.p.xrange2 = [xcen-hwin,xcen+hwin]
                
                z = alog10(delabsy/dely)/alog10(2)
                if keyword_set(IN) then z = z+1 else z=z-1
                hwin = delabsy/2.^z/2.
                state.p.yrange2 = [ycen-hwin,ycen+hwin]
                
            end
            
            else:
            
        endcase
        
    end

endcase

xmc_linepos_plotupdate,state

end
;
;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro xmc_linepos_event, event

widget_control, event.id,  GET_UVALUE = uvalue


if uvalue eq 'Done' then begin

    widget_control, event.top, /DESTROY
    goto, getout

endif

widget_control, event.top, GET_UVALUE = state, /NO_COPY
widget_control, /HOURGLASS

case uvalue of 

    'Clear Normalization': begin
        
        *state.r.normreg    = !values.f_nan
        *state.d.normcoeffs = !values.f_nan
        *state.p.goodbad    = !values.f_nan   
        state.r.linereg     = !values.f_nan     
        state.r.nnormreg    = 0
        state.r.cursormode  = 'None'

        xmc_linepos_plotupdate,state

    end

    'Display Info': state.r.displayinfo = event.select

    'Done Spectrum': xmc_linepos_stepspec,state,/DONE

    'File Button': begin

        fullpath = dialog_pickfile(DIALOG_PARENT=state.w.xmc_linepos_base,$
                                   /MUST_EXIST)
        
        if fullpath ne '' then begin
            
            widget_control, state.w.ifile_fld[1], SET_VALUE=strtrim(fullpath)
            setfocus, state.w.ifile_fld

        endif

     end

    'Flux Density': state.r.funits = event.index

    'Hydrogen Lines': begin

        state.p.plotlines = event.select
        xmc_linepos_plotupdate,state

    end

    'Input Type': begin

        state.r.itype = event.value
        widget_control, state.w.box3_base, SENSITIVE=event.value

    end

    'Keyboard': begin        

        case strtrim(event.ch,2) of 
            
            'a': BEGIN

                case state.p.plotwin of 

                    1: begin

                        state.p.absxrange1 = state.p.xrange1
                        state.p.absyrange1 = state.p.yrange1

                    end

                    2: state.p.absyrange2 = state.p.yrange2

                endcase

            end


            'c': begin

                state.r.cursormode = 'None'
                state.p.reg = !values.f_nan
                xmc_linepos_plotupdate,state
                
            end

            'i': xmc_linepos_zoom,state,/IN

            'm': begin

                case state.p.plotwin of 

                    1: begin

                        *state.d.normcoeffs = !values.f_nan
                        state.p.reg         = !values.f_nan                
                        state.r.cursormode  = 'Modify'
                        
                    end

                    2: begin

                        state.p.reg        = !values.f_nan  
                        state.r.cursormode = 'Modify'


                    end

                    else:

                endcase

            end

            'n': begin

                if state.p.plotwin eq 2 then goto, cont

;                *state.r.normreg    = !values.f_nan
;                state.p.reg         = !values.f_nan
                *state.d.normcoeffs = !values.f_nan
;                state.r.linereg     = !values.f_nan
;                *state.p.goodbad    = !values.f_nan
;                state.r.nnormreg    = 0
                state.r.cursormode  = 'Norm'
                xmc_linepos_plotupdate,state
                xmc_linepos_setminmax,state

            end


            'o': xmc_linepos_zoom,state,/OUT

            's': begin

                if state.p.plotwin eq 1 then goto, cont
                state.r.cursormode = 'Select'
                state.r.linereg    = !values.f_nan
                *state.p.overlay = !values.f_nan
                xmc_linepos_plotupdate,state

            end

            'w': begin

                if state.p.plotwin eq 1 then begin

                    state.p.xrange1 = state.p.absxrange1
                    state.p.yrange1 = state.p.absyrange1

                endif else begin

                    state.p.xrange2 = state.p.xrange1
                    state.p.yrange2 = state.p.absyrange2

                endelse
                xmc_linepos_plotupdate,state
                xmc_linepos_setminmax,state

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

    'Load File(s)': xmc_linepos_loadspec,state

    'Next Spectrum': xmc_linepos_stepspec,state

    'Norm Degree':state.r.normdeg = total(event.index+1)

    'Normalize Spectrum': begin

        xmc_linepos_normspec,state,CANCEL=cancel
        if cancel then goto, cont
        xmc_linepos_plotupdate,state
        xmc_linepos_setminmax,state
        if finite(state.r.linereg[0]) eq 1 then xmc_linepos_fit,state

        state.r.cursormode = 'None'
        
    end

    'Operation': state.r.operation = strtrim(event.value,2)
    
    'Propagate Errors': state.r.properrs = event.value

    'Skip Spectrum': xmc_linepos_stepspec,state,/SKIP

    'Store EW': xmc_linepos_storeline,state

    'Wavelength': state.r.wunits = event.index

    'Write File': xmc_linepos_writefile,state

endcase

;  Put state variable into the user value of the top level base.
 
cont: 
widget_control, state.w.xmc_linepos_base, SET_UVALUE=state, /NO_COPY
getout:

end
;
;******************************************************************************
;
pro xmc_linepos_info_event,event

widget_control, event.top, /DESTROY

end
;
;******************************************************************************
;
pro xmc_linepos_minmax_event,event

widget_control, event.top, GET_UVALUE = state, /NO_COPY

;  Plot Window 1

xmin1 = cfld(state.w.xmin1_fld,4,/EMPTY,CANCEL=cancel)
if cancel then goto,cont
xmin  = crange(xmin1,state.p.xrange1[1],'X Min',/KLT,$
               WIDGET_ID=state.w.xmc_linepos_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.xmin1_fld[0],SET_VALUE=state.p.xrange1[0]
    goto,cont

endif else state.p.xrange1[0] = xmin

xmax1 = cfld(state.w.xmax1_fld,4,/EMPTY,CANCEL=cancel)
if cancel then goto, cont
xmax  = crange(xmax1,state.p.xrange1[0],'X Max',/KGT,$
               WIDGET_ID=state.w.xmc_linepos_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.xmax1_fld[0],SET_VALUE=state.p.xrange1[1]
    goto, cont

endif else state.p.xrange1[1] = xmax

ymin1 = cfld(state.w.ymin1_fld,4,/EMPTY,CANCEL=cancel)
if cancel then goto, cont
ymin  = crange(ymin1,state.p.yrange1[1],'Y Min',/KLT,$
               WIDGET_ID=state.w.xmc_linepos_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.ymin1_fld[0],SET_VALUE=state.p.yrange1[0]
    goto, cont

endif else state.p.yrange1[0] = ymin

ymax1 = cfld(state.w.ymax1_fld,4,/EMPTY,CANCEL=cancel)
if cancel then goto, cont
ymax  = crange(ymax1,state.p.yrange1[0],'Y Max',/KGT,$
               WIDGET_ID=state.w.xmc_linepos_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.ymax1_fld[0],SET_VALUE=state.p.yrange1[1]
    goto, cont

endif else state.p.yrange1[1] = ymax



;  Plot Window 2

if finite((*state.d.normcoeffs)[0]) eq 1 then begin
    
    ymin2 = cfld(state.w.ymin2_fld,4,/EMPTY,CANCEL=cancel)
    if cancel then goto, cont
    ymin  = crange(ymin2,state.p.yrange2[1],'Y Min',/KLT,$
                   WIDGET_ID=state.w.xmc_linepos_base,CANCEL=cancel)
    if cancel then begin
        
        widget_control, state.w.ymin2_fld[0],SET_VALUE=state.p.yrange2[0]
        goto, cont
        
    endif else state.p.yrange2[0] = ymin
    
    ymax2 = cfld(state.w.ymax2_fld,4,/EMPTY,CANCEL=cancel)
    if cancel then goto, cont
    ymax  = crange(ymax2,state.p.yrange2[0],'Y Max',/KGT,$
                   WIDGET_ID=state.w.xmc_linepos_base,CANCEL=cancel)
    if cancel then begin
        
        widget_control, state.w.ymax2_fld[0],SET_VALUE=state.p.yrange2[1]
        goto, cont
        
    endif else state.p.yrange2[1] = ymax2
    
endif

cont: 
xmc_linepos_plotupdate,state
widget_control, state.w.xmc_linepos_base, SET_UVALUE=state, /NO_COPY


end
;
;******************************************************************************
;
pro xmc_linepos_plotwin1event,event

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

case state.r.cursormode of 

    'Modify': begin

        case event.type of 

            0: begin
                
                state.p.reg[*,0] = xy[0:1]
                xmc_linepos_whichnormline,state
                
            end

            1: begin

                if not state.p.modnormreg then goto, cont
                z = where(finite(*state.r.normreg) eq 0)
                (*state.r.normreg)[z] = xy[0]
                xmc_linepos_normspec,state
                xmc_linepos_plotupdate,state
                if finite(state.r.linereg[0]) eq 1 then xmc_linepos_ew,state
                state.p.modnormreg = 0
                state.r.cursormode = 'None'
                                    
            end

            else:

        end

    end

    'Norm': begin

        if event.type ne 1 then goto, out        
        z = where(finite(state.p.reg) eq 1,count)        
        if count eq 0 then begin
            
            state.p.reg[*,0] = xy[0:1]
            wset, state.p.pixmap1_wid
            plots,[xy[0],xy[0]],!y.crange,COLOR=6,LINESTYLE=1,THICK=2
            wset, state.p.plotwin1_wid
            device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],$
                          0,0,state.p.pixmap1_wid]                
            
        endif else begin
            
            state.p.reg[*,1] = xy[0:1]
            xmc_linepos_definenormreg,state
            xmc_linepos_plotupdate,state
            state.p.reg = !values.f_nan

        endelse

    end

    'XZoom': begin
        
        if event.type ne 1 then goto, out        
        z = where(finite(state.p.reg) eq 1,count)
        if count eq 0 then begin
            
            state.p.reg[*,0] = xy[0:1]
            wset, state.p.pixmap1_wid
            plots, [event.x,event.x],[0,state.p.plot1size[1]],COLOR=2,$
              /DEVICE,LINESTYLE=1,THICK=2
            wset, state.p.plotwin1_wid
            device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
                          state.p.pixmap1_wid]
            
        endif else begin
            
            state.p.reg[*,1]   = xy[0:1]
            state.p.xrange1    = [min(state.p.reg[0,*],MAX=m),m]
            state.p.reg        = !values.f_nan
            state.r.cursormode = 'None'
            xmc_linepos_plotupdate,state
            xmc_linepos_setminmax,state
            
        endelse
        
    end

   'YZoom': begin
        
        if event.type ne 1 then goto, out        
        z = where(finite(state.p.reg) eq 1,count)
        if count eq 0 then begin
            
            state.p.reg[*,0] = xy[0:1]
            wset, state.p.pixmap1_wid
            plots, [0,state.p.plot1size[0]],[event.y,event.y],COLOR=2,$
              /DEVICE,LINESTYLE=1,THICK=2
            
            wset, state.p.plotwin1_wid
            device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
                          state.p.pixmap1_wid]
            
        endif else begin
            
            state.p.reg[*,1]   = xy[0:1]
            state.p.yrange1    = [min(state.p.reg[1,*],MAX=m),m]
            state.r.cursormode = 'None'
            state.p.reg        = !values.f_nan
            xmc_linepos_plotupdate,state
            xmc_linepos_setminmax,state
            
        endelse
        
    end

    'Zoom': begin

        if event.type ne 1 then goto, out        
        z = where(finite(state.p.reg) eq 1,count)
        if count eq 0 then state.p.reg[*,0] = xy[0:1] else begin
            
            state.p.reg[*,1] = xy[0:1]
            state.p.xrange1 = [min(state.p.reg[0,*],MAX=max),max]
            state.p.yrange1 = [min(state.p.reg[1,*],MAX=max),max]
            state.r.cursormode = 'None'
            state.p.reg = !values.f_nan
            xmc_linepos_plotupdate,state
            xmc_linepos_setminmax,state
            
        endelse
        
    end

    else:

endcase
out:


;  Draw cursor lines

wset, state.p.plotwin1_wid
device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
              state.p.pixmap1_wid]


case state.r.cursormode of 

    'XZoom': plots, [event.x,event.x],[0,state.p.plot1size[1]],COLOR=2,/DEVICE

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
        device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,0,$
                      state.p.pixmap2_wid]

        plots,[event.x,event.x],[0,state.p.plot2size[1]],COLOR=2,/DEVICE


    end

endcase

;  Update cursor tracking

if not state.p.freeze then begin

    tabinv, *state.d.w,xy[0],idx
    idx = round(idx)
    label = 'Cursor X: '+strtrim(xy[0],2)+', Y:'+strtrim(xy[1],2)
    label = label+'   Spectrum X: '+strtrim((*state.d.w)[idx],2)+$
      ', Y:'+strtrim((*state.d.f)[idx],2)
    widget_control,state.w.track,SET_VALUE=label

endif

;  Put state variable into the user value of the top level base.
 
cont: 
widget_control, state.w.xmc_linepos_base, SET_UVALUE=state, /NO_COPY

end
;
;*****************************************************************************
;
pro xmc_linepos_plotwinevent2,event

widget_control, event.top, GET_UVALUE = state, /NO_COPY
widget_control, event.id,  GET_UVALUE = uvalue

;  Check to see if it is a TRACKING event.

if strtrim(tag_names(event,/STRUCTURE_NAME),2) eq 'WIDGET_TRACKING' then begin

    if event.enter eq 0 then widget_control, state.w.keyboard, SENSITIVE=0
    wset, state.p.plotwin2_wid
    device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,0,$
              state.p.pixmap2_wid]
    wset, state.p.plotwin1_wid
    device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
              state.p.pixmap1_wid]


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

case state.r.cursormode of 

    'Modify': begin

        case event.type of 

            0: begin
                
                state.p.reg[*,0] = xy[0:1]
                xmc_linepos_whichlinereg,state
                
            end

            1: begin

                if not state.p.modlinereg then goto, cont
                z = where(finite(state.r.linereg) eq 0)
                state.r.linereg[z] = xy[0]
                xmc_linepos_ew,state
                xmc_linepos_plotupdate,state
                state.p.modlinereg = 0
                state.r.cursormode = 'None'
                                    
            end

            else:

        end

    end

    'Select': begin

        if event.type ne 1 then goto, out    
        z = where(finite(state.r.linereg) eq 1,count)
        if count eq 0 then state.r.linereg[0] = xy[0]
        if count eq 1 then begin

            state.r.linereg[1] = xy[0]            
            state.r.cursormode = 'None'
            xmc_linepos_fit,state

        endif
        xmc_linepos_plotupdate,state
        xmc_linepos_setminmax,state
           
    end

    'YZoom': begin
        
        if event.type ne 1 then goto, out        
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
            
            state.p.reg[*,1]   = xy[0:1]
            state.p.yrange2    = [min(state.p.reg[1,*],MAX=m),m]
            state.r.cursormode = 'None'
            state.p.reg        = !values.f_nan
            xmc_linepos_plotupdate,state
            xmc_linepos_setminmax,state
            
        endelse
        
    end

   'Zoom': begin

        if event.type ne 1 then goto, out        
        z = where(finite(state.p.reg) eq 1,count)
        if count eq 0 then state.p.reg[*,0] = xy[0:1] else begin
            
            state.p.reg[*,1] = xy[0:1]
            state.p.xrange2 = [min(state.p.reg[0,*],MAX=max),max]
            state.p.yrange2 = [min(state.p.reg[1,*],MAX=max),max]
            state.r.cursormode = 'None'
            state.p.reg = !values.f_nan
            xmc_linepos_plotupdate,state
            xmc_linepos_setminmax,state
            
        endelse
        
    end

    
    else:
    
endcase
out:

;  Draw cursor lines

wset, state.p.plotwin2_wid
device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,0,$
              state.p.pixmap2_wid]

case state.r.cursormode of 

    'YZoom': plots, [0,state.p.plot2size[0]],[event.y,event.y],COLOR=2,/DEVICE

    'Zoom': begin

        plots, [event.x,event.x],[0,state.p.plot2size[1]],COLOR=2,/DEVICE
        plots, [0,state.p.plot2size[0]],[event.y,event.y],COLOR=2,/DEVICE
        xy = convert_coord(event.x,event.y,/DEVICE,/TO_DATA)
        plots,[state.p.reg[0,0],state.p.reg[0,0]],[state.p.reg[1,0],xy[1]],$
          LINESTYLE=2,COLOR=2
        plots, [state.p.reg[0,0],xy[0]],[state.p.reg[1,0],state.p.reg[1,0]],$
          LINESTYLE=2,COLOR=2
        
    end

    'Select': begin

        plots, [event.x,event.x],[0,state.p.plot2size[1]],COLOR=2,/DEVICE
        plots, [0,state.p.plot2size[0]],[event.y,event.y],COLOR=2,/DEVICE
        wset, state.p.plotwin1_wid
        device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
                      state.p.pixmap1_wid]

        plots, [event.x,event.x],[0,state.p.plot2size[1]],COLOR=2,/DEVICE
        wset, state.p.plotwin2_wid
        
    end

    else: begin

        plots, [event.x,event.x],[0,state.p.plot2size[1]],COLOR=2,/DEVICE
        plots, [0,state.p.plot2size[0]],[event.y,event.y],COLOR=2,/DEVICE

        wset, state.p.plotwin1_wid
        device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
                      state.p.pixmap1_wid]

        plots,[event.x,event.x],[0,state.p.plot1size[1]],COLOR=2,/DEVICE

    end

endcase

;  Update cursor tracking

if not state.p.freeze and finite((*state.d.normcoeffs)[0]) eq 1 then begin

    tabinv, *state.d.w,xy[0],idx
    idx = round(idx)
    label = 'Cursor X: '+strtrim(xy[0],2)+', Y:'+strtrim(xy[1],2)
    label = label+'   Spectrum X: '+strtrim((*state.d.w)[idx],2)+$
      ', Y:'+strtrim((*state.d.ny)[idx],2)
    widget_control,state.w.track,SET_VALUE=label
    
endif

;  Put state variable into the user value of the top level base.
 
cont: 
widget_control, state.w.xmc_linepos_base, SET_UVALUE=state, /NO_COPY

end
;
;******************************************************************************
;
; ------------------------------Main Program-------------------------------- 
;
;******************************************************************************
;
pro xmc_linepos

nlines = 20

mkct 
mc_getfonts,buttonfont,textfont

mc_getosinfo,dirsep,strsep

last        = strpos(!path,'Spantool')
first       = strpos(!path,strsep,last,/REVERSE_SEARCH)
packagepath = strmid(!path,first+1,last-first+8)

readfmt,filepath('HI.dat',ROOT_DIR=packagepath,SUBDIR='data'),$
  'F8.6,1x,A12',hlines,hnames


w = {box3_base:0L,$
     buttonfont:buttonfont,$
     errs_lbl:0L,$
     ewlabel:0L,$
     fitlabel:0L,$
     hthresh_fld:[0L,0L],$
     ifile_fld:[0L,0L],$
     lbl_lbl:0L,$
     xinfo_base:0L,$
     keyboard:0L,$
     linewave_fld:[0L,0L],$
     linerow_base:0L,$
     lthresh_fld:[0L,0L],$
     ofile_fld:[0L,0L],$
     plotwin1:0L,$
     plotwin2:0L,$
     textfont:textfont,$
     track:0L,$
     vals_lbl:0L,$
     xmc_linepos_base:0L,$
     xmax1_fld:[0L,0L],$
     xmin1_fld:[0L,0L],$
     xmax2_fld:[0L,0L],$
     xmin2_fld:[0L,0L],$
     ymax1_fld:[0L,0L],$
     ymin1_fld:[0L,0L],$
     ymax2_fld:[0L,0L],$
     ymin2_fld:[0L,0L]}

r = {cursormode:'None',$
     displayinfo:0,$
     files:ptr_new(2),$
     funits:0,$
     itype:0,$
     lineidx:0,$
     linereg:[!values.f_nan,!values.f_nan],$
     names:ptr_new(2),$
     nlines:nlines,$
     nnormreg:0,$
     normdeg:1,$
     normreg:ptr_new(2),$
     npeaks:1,$
     operation:'Sum',$
     properrs:0,$
     specidx:0,$
     wscale:1.0,$
     wunits:0}

d = {c:ptr_new(!values.f_nan),$
     ce:ptr_new(!values.f_nan),$
     f:ptr_new(2),$
     fe:ptr_new(2),$
     hlines:hlines,$
     hnames:hnames,$
     linedata:ptr_new(2),$
     normcoeffs:ptr_new(!values.f_nan),$
     ny:ptr_new(2),$
     tmpdata:ptr_new(2),$
     w:ptr_new(2),$
     y:ptr_new(2),$
     ye:ptr_new(2)}

p = {absxrange1:[!values.f_nan,!values.f_nan],$
     absxrange2:[!values.f_nan,!values.f_nan],$
     absyrange1:[!values.f_nan,!values.f_nan],$
     absyrange2:[!values.f_nan,!values.f_nan],$
     buffer:[0.,0.],$
     freeze:1,$
     funit:'(W m-2)',$
     goodbad:ptr_new(!values.f_nan),$
     modnormreg:0,$
     modlinereg:0,$
     overlay:ptr_new(!values.f_nan),$
     pixmap1_wid:0L,$
     pixmap2_wid:0L,$
     plotlines:0,$
     plot1size:[700,250],$
     plot2size:[700,250],$
     plotwin:1,$
     plotwin1_wid:0L,$
     plotwin2_wid:0L,$
     pscale1:!p,$
     pscale2:!p,$
     reg:[[!values.f_nan,!values.f_nan],$
          [!values.f_nan,!values.f_nan]],$
     specname:'',$
     wunit:'(um)',$
     xrange1:[!values.f_nan,!values.f_nan],$
     xrange2:[!values.f_nan,!values.f_nan],$
     xscale1:!x,$
     xscale2:!x,$
     xtitle:'',$
     yrange1:[!values.f_nan,!values.f_nan],$
     yrange2:[!values.f_nan,!values.f_nan],$
     yscale1:!y,$
     yscale2:!y,$
     ytitle:''}

state = {w:w,r:r,d:d,p:p}


state.w.xmc_linepos_base = widget_base(TITLE='Xmc_Linepos',$
                                    /COL,$
                                    /TLB_SIZE_EVENTS)

   button = widget_button(state.w.xmc_linepos_base,$
                          FONT=buttonfont,$
                          EVENT_PRO='xmc_linepos_event',$
                          VALUE='Done',$
                          UVALUE='Done')

   row_base = widget_base(state.w.xmc_linepos_base,$
                          /ROW)

      col1_base = widget_base(row_base,$
                              EVENT_PRO='xmc_linepos_event',$
                              /COL)
                     
         box1_base = widget_base(col1_base,$
                                 /COL,$
                                 FRAME=1)
         
            label = widget_label(box1_base,$
                                 /ALIGN_LEFT,$
                                 FONT=buttonfont,$
                                 VALUE='1.  File Input:')

          bg = cw_bgroup(box1_base,$
                           /ROW,$
                           ['Single (FITS)','Multi (text)'],$
                           FONT=buttonfont,$
                           UVALUE='Input Type',$
                           SET_VALUE=0,$
                           LABEL_LEFT='Input Type:',$
                           /NO_RELEASE,$
                           /EXCLUSIVE)

            row = widget_base(box1_base,$
                              /ROW,$
                              /BASE_ALIGN_CENTER)

               button = widget_button(row,$
                                      FONT=buttonfont,$
                                      VALUE='File',$
                                      UVALUE='File Button')
         
               fld = coyote_field2(row,$
                                   LABELFONT=buttonfont,$
                                   FIELDFONT=textfont,$
                                   TITLE=':',$
                                   UVALUE='File Field',$
                                   XSIZE=30,$
                                   TEXTID=textid)
               state.w.ifile_fld = [fld,textid]  

            dl = widget_droplist(box1_base,$
                                 FONT=buttonfont,$
                                 TITLE='Wavelength:',$
                                 VALUE=['um','A','nm'],$
                                 UVALUE='Wavelength')
            
            dl = widget_droplist(box1_base,$
                                 FONT=buttonfont,$
                                 TITLE='Flux Density:',$
                                 VALUE=['W m-2 um-1','ergs s-1 cm-2 A-1'],$
                                 UVALUE='Flux Density')
            
            button = widget_button(box1_base,$
                                   VALUE='Load File(s)',$
                                   FONT=buttonfont,$
                                   UVALUE='Load File(s)')

         box2_base = widget_base(col1_base,$
                                 /COL,$
                                 FRAME=1)
            
            label = widget_label(box2_base,$
                                 /ALIGN_LEFT,$
                                 FONT=buttonfont,$
                                 VALUE='2.  Normalize Spectrum:')

            bg = cw_bgroup(box2_base,$
                           ['No','Yes'],$
                           FONT=buttonfont,$
                           LABEL_LEFT='Propagate Errors:',$
                           UVALUE='Propagate Errors',$
                           /ROW,$
                           SET_VALUE=0,$
                           /NO_RELEASE,$
                           /EXCLUSIVE)

            row = widget_base(box2_base,$
                              /ROW,$
                              /BASE_ALIGN_CENTER)

               hthresh = coyote_field2(row,$
                                       LABELFONT=buttonfont,$
                                       FIELDFONT=textfont,$
                                       TITLE='High / Low Thresh:',$
                                       UVALUE='Hi Thresh',$
                                       XSIZE=5,$
                                       VALUE=5.0,$
                                       TEXTID=textid)
               state.w.hthresh_fld = [hthresh,textid]

               lthresh = coyote_field2(row,$
                                       LABELFONT=buttonfont,$
                                       FIELDFONT=textfont,$
                                       TITLE=' / ',$
                                       UVALUE='Low Thresh',$
                                       XSIZE=5,$
                                       VALUE=5.0,$
                                       TEXTID=textid)
               state.w.lthresh_fld = [lthresh,textid]


            row = widget_base(box2_base,$
                              /ROW,$
                              /BASE_ALIGN_CENTER)


               value = ['1','2','3','4','5','6','7','8','9']
               fitorder_dl = widget_droplist(row,$
                                             FONT=buttonfont,$
                                             TITLE='Fit Degree:',$
                                             VALUE=value,$
                                             UVALUE='Norm Degree')
               
               button = widget_button(row,$
                                      VALUE='Fit Continuum',$
                                      FONT=buttonfont,$
                                      UVALUE='Normalize Spectrum')
               
               button = widget_button(row,$
                                      VALUE='Clear',$
                                      FONT=buttonfont,$
                                      UVALUE='Clear Normalization')


         state.w.box3_base = widget_base(col1_base,$
                                         /COL,$
                                         FRAME=1)
            
            label = widget_label(state.w.box3_base,$
                                 /ALIGN_LEFT,$
                                 FONT=buttonfont,$
                                 VALUE='3.  Multi-Control:')

            row = widget_base(state.w.box3_base,$
                              /ROW,$
                              /BASE_ALIGN_CENTER)
               
               label = widget_label(row,$
                                    FONT=buttonfont,$
                                    VALUE='Spectrum: ')

               junk = widget_button(row,$
                                    VALUE='Next',$
                                    UVALUE='Next Spectrum',$
                                    FONT=buttonfont)
               
               junk = widget_button(row,$
                                    VALUE='Skip',$
                                    UVALUE='Skip Spectrum',$
                                    FONT=buttonfont)

               junk = widget_button(row,$
                                    VALUE='Done',$
                                    UVALUE='Done Spectrum',$
                                    FONT=buttonfont)


         box4_base = widget_base(col1_base,$
                                 /COL,$
                                 FRAME=1)
         
            label = widget_label(box4_base,$
                                 /ALIGN_LEFT,$
                                 FONT=buttonfont,$
                                 VALUE='4. Write File')

               fld = coyote_field2(box4_base,$
                                   LABELFONT=buttonfont,$
                                   FIELDFONT=textfont,$
                                   TITLE='File Name',$
                                   UVALUE='File Name',$
                                   XSIZE=15,$
                                   TEXTID=textid)
               state.w.ofile_fld = [fld,textid]

               
               button = widget_button(box4_base,$
                                      VALUE='Write File',$
                                      UVALUE='Write File',$
                                      FONT=buttonfont)

         widget_control, state.w.box3_base, SENSITIVE=0

     col2_base = widget_base(row_base,$
                             EVENT_PRO='xmc_linepos_event',$
                             /COL)

         state.w.track = widget_text(col2_base, $
                                     YSIZE=1)

         hlines_bg = cw_bgroup(col2_base,$
                               ['Plot Hydrogen Lines'],$
                               FONT=buttonfont,$
                               UVALUE='Hydrogen Lines',$
                               SET_VALUE=[0],$
                               /NONEXCLUSIVE)
         
         state.w.keyboard = widget_text(col2_base, $
                                        /ALL_EVENTS, $
                                        SCR_XSIZE=1, $
                                        SCR_YSIZE=1, $
                                        UVALUE='Keyboard', $
                                        EVENT_PRO='xmc_linepos_event',$
                                        VALUE= '')

         box1_base = widget_base(col2_base,$
                                 /FRAME,$
                                 /COLUMN)


            state.w.plotwin1 = widget_draw(box1_base,$
                                           XSIZE=state.p.plot1size[0],$
                                           YSIZE=state.p.plot1size[1],$
                                           /TRACKING_EVENTS,$
                                           /BUTTON_EVENTS,$
                                           /MOTION_EVENTS,$
                                     EVENT_PRO='xmc_linepos_plotwin1event',$
                                           UVALUE='Plot Window 1')

            row = widget_base(box1_base,$
                              EVENT_PRO='xmc_linepos_event',$
                              /ROW)
         
               xmin = coyote_field2(row,$
                                    LABELFONT=buttonfont,$
                                    FIELDFONT=textfont,$
                                    TITLE='X Min:',$
                                    UVALUE='X Min',$
                                    XSIZE=12,$
                                    EVENT_PRO='xmc_linepos_minmax_event',$
                                    /CR_ONLY,$
                                    TEXTID=textid)
               state.w.xmin1_fld = [xmin,textid]
               
               xmax = coyote_field2(row,$
                                    LABELFONT=buttonfont,$
                                    FIELDFONT=textfont,$
                                    TITLE='X Max:',$
                                    UVALUE='X Max',$
                                    XSIZE=12,$
                                    EVENT_PRO='xmc_linepos_minmax_event',$
                                    /CR_ONLY,$
                                    TEXTID=textid)
               state.w.xmax1_fld = [xmax,textid]
               
               ymin = coyote_field2(row,$
                                    LABELFONT=buttonfont,$
                                    FIELDFONT=textfont,$
                                    TITLE='Y Min:',$
                                    UVALUE='Y Min1',$
                                    XSIZE=12,$
                                    EVENT_PRO='xmc_linepos_minmax_event',$
                                    /CR_ONLY,$
                                    TEXTID=textid)
               state.w.ymin1_fld = [ymin,textid]
               
               ymax = coyote_field2(row,$
                                    LABELFONT=buttonfont,$
                                    FIELDFONT=textfont,$
                                    TITLE='Y Max:',$
                                    UVAL = 'Y Max1',$
                                    XSIZE=12,$
                                    EVENT_PRO = 'xmc_linepos_minmax_event',$
                                    /CR_ONLY,$
                                    TEXTID=textid)
               state.w.ymax1_fld = [ymax,textid]

        box2_base = widget_base(col2_base,$
                                 /FRAME,$
                                 /COLUMN)

;            row = widget_base(box2_base,$
;                              EVENT_PRO='xmc_linepos_event',$
;                              /ROW,$
;                              /BASE_ALIGN_CENTER)
;
;               bg = cw_bgroup(row,$
;                              ['Display Info'],$
;                              FONT=buttonfont,$
;                              UVALUE='Display Info',$
;                              SET_VALUE=[0],$
;                              /NONEXCLUSIVE)

;               button = cw_bgroup(row,$
;                                  /ROW,$
;                                  ['Sum','Gaussian Fit'],$
;                                  FONT=buttonfont,$
;                                  UVALUE='Operation',$
;                                  SET_VALUE=0,$
;                                  LABEL_LEFT='Operation:',$
;                                  /NO_RELEASE,$
;                                  /EXCLUSIVE)

;               state.w.ewlabel = widget_label(row,$
;                                              FONT=buttonfont,$
;                                              VALUE='   EW:',$
;                                              /DYNAMIC_RESIZE)


            state.w.plotwin2 = widget_draw(box2_base,$
                                           XSIZE=state.p.plot1size[0],$
                                           YSIZE=state.p.plot1size[1],$
                                           /TRACKING_EVENTS,$
                                           /BUTTON_EVENTS,$
                                           /MOTION_EVENTS,$
                                           EVENT_PRO='xmc_linepos_plotwinevent2',$
                                           UVALUE='Plot Window 2')
         
            row = widget_base(box2_base,$
                              EVENT_PRO='xmc_linepos_event',$
                              /ROW)
         
               ymin = coyote_field2(row,$
                                    LABELFONT=buttonfont,$
                                    FIELDFONT=textfont,$
                                    TITLE='Y Min:',$
                                    UVALUE='Y Min2',$
                                    XSIZE=12,$
                                    EVENT_PRO='xmc_linepos_minmax_event',$
                                    /CR_ONLY,$
                                    TEXTID=textid)
               state.w.ymin2_fld = [ymin,textid]
               
               ymax = coyote_field2(row,$
                                    LABELFONT=buttonfont,$
                                    FIELDFONT=textfont,$
                                    TITLE='Y Max:',$
                                    UVAL = 'Y Max2',$
                                    XSIZE=12,$
                                    EVENT_PRO = 'xmc_linepos_minmax_event',$
                                    /CR_ONLY,$
                                    TEXTID=textid)
               state.w.ymax2_fld = [ymax,textid]

; Get things running.  Center the widget using the Fanning routine.
            
centertlb,state.w.xmc_linepos_base
widget_control, state.w.xmc_linepos_base, /REALIZE
     
;  Get plotwin ids
    
widget_control, state.w.plotwin1, GET_VALUE=xx
state.p.plotwin1_wid = xx

widget_control, state.w.plotwin2, GET_VALUE=xx
state.p.plotwin2_wid = xx

window, /FREE, /PIXMAP, XSIZE=state.p.plot1size[0],$
  YSIZE=state.p.plot1size[1]
state.p.pixmap1_wid = !d.window

window, /FREE, /PIXMAP, XSIZE=state.p.plot2size[0],$
  YSIZE=state.p.plot2size[1]
state.p.pixmap2_wid = !d.window

;  Get sizes for things.

widget_geom = widget_info(state.w.xmc_linepos_base, /GEOMETRY)
state.p.buffer[0]=widget_geom.xsize-state.p.plot1size[0]
state.p.buffer[1]=widget_geom.ysize-state.p.plot1size[1]-$
  state.p.plot2size[1]

XManager, 'xmc_linepos', $
  /NO_BLOCK,$
  state.w.xmc_linepos_base,$
  EVENT_HANDLER='xmc_linepos_resize_event'

; Put state variable into the user value of the top level base.

widget_control, state.w.xmc_linepos_base, SET_UVALUE=state, /NO_COPY




end
