;+
; NAME:
;     fire_xcombspec
;    
; PURPOSE:
;     Combines FIRE spectra.
;    
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     fire_xcombspec
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
;     Writes a FIRE spectra FITS file to disk
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
;     Only works on FIRE spectra
;
; PROCEDURE:
;     See fire_xcombspec_helpfile.txt in Spextool/helpfiles
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2000       - Written by M. Cushing, Institute for Astronomy, UH
;     2002-07-19 - Heavily modified by M. Cushing
;     2005-04-x  - Added new plotting controls.
;     2005-10-20 - Modified so that NaNs in the spectra are not
;                  removed before the interpolation is performed in
;                  the loadimages program
;	8/25/11 modified by A. Burgasser to work with FIRE data
;-
;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro fire_xcombspec_event, event

widget_control, event.id,  GET_UVALUE = uvalue
if uvalue eq 'Quit' then begin
    
    widget_control, /XMANAGER_ACTIVE_COMMAND
    widget_control, /event_break
    widget_control, event.top, /DESTROY
    goto, getout

endif

widget_control, event.top, GET_UVALUE = state, /NO_COPY
widget_control, /HOURGLASS

case uvalue of

    'Aperture': begin

        state.r.ap = event.index

        z = where((*state.r.orders) eq (*state.r.scaleorder)[state.r.ap])
        widget_control, state.w.scaleorder_dl, SET_DROPLIST_SELECT=total(z)+1

        fire_xcombspec_plotupdate,state

    end

    'Combine Apertures': state.r.combineaps = event.value

    'Write File': begin

        fire_xcombspec_combinespec,state
        fire_xcombspec_writefile,state

    end
    'Combination Statistic':  begin

        state.r.combinestat = event.index
        sensitive = (event.index le 2) ? 1:0
        widget_control, state.w.rthresh_fld[0], SENSITIVE=sensitive
        fire_xcombspec_combinespec,state
        fire_xcombspec_plotupdate,state

    end
    
;    'Correct Spectral Shape': fire_xcombspec_correctspec,state

    'File Name': begin

        path =dialog_pickfile(DIALOG_PARENT=state.w.fire_xcombspec_base,$
                              /MUST_EXIST)

        if path ne '' then begin

            widget_control,state.w.filename_fld[1],SET_VALUE=path

        endif

    end

    'Help': fire_xcombspec_help,state

    'Input Mode': begin

        widget_control, state.w.manual_base,MAP=0
        widget_control, state.w.file_base,MAP=0
        state.r.inputmode = event.value
        if event.value eq 'File' then widget_control, state.w.file_base,/MAP
        if event.value eq 'Manual' then widget_control, state.w.manual_base,$
          /MAP

    end

    'Input Prefix': setfocus, state.w.specfiles_fld

    'Load Spectra': fire_xcombspec_loadspec,state

    'Make Mask': begin
      
      fire_xcombspec_mkmask,state,event.index
      fire_xcombspec_plotupdate,state
      
    end
    
    'Mean Error': state.r.meanerror = event.value

    'Modify Order Type': begin

        state.r.modspec = event.index
        if state.r.modspec eq 2 then fire_xcombspec_correctspec,state

    end
    'Modify Order': begin

        if state.r.modspec eq 0 then fire_xcombspec_scalespec,state,event.index

        if state.r.modspec eq 1 then fire_xcombspec_selectspec,state,event.index

        if state.r.modspec eq 3 then fire_xcombspec_mkmask,state,event.index
        
    end

    'Output Format': state.r.textoutput=event.select

    'Path Button': begin

        path= dialog_pickfile(/DIRECTOR,DIALOG_PARENT=state.w.fire_xcombspec_base,$
                              TITLE='Select Path',/MUST_EXIST)
        
        if path ne '' then begin

            path = cpath(path,WIDGET_ID=state.w.fire_xcombspec_base,CANCEL=cancel)
            if cancel then return
            widget_control,state.w.path_fld[1],SET_VALUE = path
            setfocus,state.w.path_fld

        endif
    end

    'Plot Type': begin

        state.p.plottype = event.index
        fire_xcombspec_plotupdate,state

    end

    'Readmode': begin

        widget_control, state.w.inprefix_fld[0],  SENSITIVE=0
        if event.value eq 'Filename' then begin
            
            state.r.filereadmode = event.value
            widget_control, state.w.inprefix_fld[0], SENSITIVE=0
            setfocus,state.w.specfiles_fld
            
        endif else begin
            
            state.r.filereadmode = event.value
            widget_control, state.w.inprefix_fld[0], /SENSITIVE
            setfocus,state.w.inprefix_fld

        endelse

    end

    'Robust Threshold': begin

        fire_xcombspec_combinespec,state
        fire_xcombspec_plotupdate,state

    end

    'Spectra Files Button': begin
                
        path = cfld(state.w.path_fld,7,CANCEL=cancel)
        if cancel then return
        if path ne '' then path = cpath(path,WIDGET_ID=state.w.fire_xcombspec_base,$
                                        CANCEL=cancel)

        fullpath = dialog_pickfile(DIALOG_PARENT=state.w.fire_xcombspec_base,$
                                   PATH=path,/MUST_EXIST,FILTER='*.fits',$
                                   /MULTIPLE_FILES)
        
        case (size(fullpath))[1] of 

            1: begin

                if fullpath[0] ne '' then begin
                    
                    widget_control,state.w.specfiles_fld[1],$
                      SET_VALUE=strmid(fullpath[0],$
                                       strpos(fullpath,'/',/REVERSE_S)+1)
                    setfocus, state.w.outfile_fld
                    
                endif

            end

            else: begin

                for i =0,(size(fullpath))[1]-1 do begin
 
                    tmp = strmid(fullpath[i],strpos(fullpath[i],'/',$
                                                    /REVERSE_S)+1)
                    arr = (i eq 0) ? tmp:[arr,tmp]

                endfor
                widget_control,state.w.specfiles_fld[1],$
                  SET_VALUE=strjoin(arr,',',/SINGLE)

            end

        endcase

    end

    'Spectra Files Field': setfocus,state.w.outfile_fld

    'Spectra Type': begin

        state.p.spectype = event.index
        fire_xcombspec_plotupdate,state

    end


    else:


endcase

;  Put state variable into the user value of the top level base.
 
cont: 
widget_control, state.w.fire_xcombspec_base, SET_UVALUE=state, /NO_COPY
getout:

end
;
;******************************************************************************
;
pro fire_xcombspec_plotwin_event,event

widget_control, event.top, GET_UVALUE = state, /NO_COPY

if state.p.plottype eq 1 then begin

    idx = floor(event.y/float(state.p.plotwinsize[1])*state.r.norders)

    case state.p.spectype of 

        0: spec = reform((*state.r.combspec)[*,1,idx])

        1: spec = reform((*state.r.combspec)[*,2,idx])

        2: spec = reform((*state.r.combspec)[*,1,idx]) / $
                  reform((*state.r.combspec)[*,2,idx])

    endcase

    xzoomplot,reform((*state.r.combspec)[*,0,idx]),spec

endif

widget_control, event.top, SET_UVALUE=state, /NO_COPY

end
;
;******************************************************************************
;
pro fire_xcombspec_resize,event

widget_control, event.top, GET_UVALUE = state, /NO_COPY
widget_control, event.id,  GET_UVALUE = uvalue

widget_control, state.w.fire_xcombspec_base, TLB_GET_SIZE=size

state.p.plotwinsize[0]=size[0]-state.p.buffer[0]
state.p.scrollsize[0] =state.p.plotwinsize[0]
state.p.scrollsize[1]=size[1]-state.p.buffer[1]

state.p.plotwinsize[1] = state.p.scrollsize[1] > state.p.pixpp*state.r.norders

;state.p.plotwinsize[1] = state.p.scrollsize[1] > $
;  state.p.pixpp*state.r.norders*(1 > (2-state.r.norders*0.1))

;  Modify plot window according to the number of orders

;widget_control, state.w.col2_base, UPDATE=0
;widget_control, state.w.plotwin, /DESTROY

;state.w.plotwin = widget_draw(state.w.col2_base,$
;                              XSIZE=state.p.plotwinsize[0],$
;                              YSIZE=state.p.plotwinsize[1],$
;                              X_SCROLL_SIZE=state.p.scrollsize[0],$
;                              Y_SCROLL_SIZE=state.p.scrollsize[1],$
;                              UVALUE='Plot Window',$
;                              /BUTTON_EVENTS,$
;                              EVENT_PRO='fire_xcombspec_plotwin_event')

;widget_control, state.w.col2_base, UPDATE=1



;wdelete, state.p.pixmap_wid
;window, /FREE, /PIXMAP,XSIZE=state.p.plotwinsize[0],$
;  YSIZE=state.p.plotwinsize[1]
;state.p.pixmap_wid = !d.window

fire_xcombspec_modwinsize,state
fire_xcombspec_plotupdate,state

widget_control, state.w.fire_xcombspec_base, SET_UVALUE=state, /NO_COPY

end
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro fire_xcombspec_cleanup,fire_xcombspec_base

widget_control, fire_xcombspec_base, GET_UVALUE = state, /NO_COPY
if n_elements(state) ne 0 then begin
    
    ptr_free, state.r.files
    ptr_free, state.r.spcmask
    ptr_free, state.r.pixmask
    ptr_free, state.r.orders
    ptr_free, state.r.corspec
    ptr_free, state.r.scaleorder
    ptr_free, state.d.ospec

    
    ptr_free, state.d.wspec
    ptr_free, state.d.hdrinfo
    
endif 
state = 0B

end
;
;******************************************************************************
;
pro fire_xcombspec_combinespec,state

spc     = *state.d.wspec
spcmask = *state.r.spcmask
pixmask = *state.r.pixmask

sspec = (state.r.combineaps eq 'Yes') ? $
  fltarr(state.r.npix,3,state.r.norders):$
  fltarr(state.r.npix,3,state.r.norders*state.r.naps)

if state.r.combinestat le 2 then begin

    thresh = cfld(state.w.rthresh_fld,4,/EMPTY,CANCEL=cancel)
    if cancel then return

endif

for i = 0, state.r.fnaps-1 do begin

    for j = 0, state.r.norders-1 do begin

        z = where(spcmask[*,j,i] eq 1,count)
        spec = reform(spc[i].(j)[*,1,z])
        err  = reform(spc[i].(j)[*,2,z])
        mask = reform(pixmask[i].(j))
        
        ;J. Gagne : If error is not set
        if total(finite(err)) eq 0. then begin
          message, ' All errors were == 0 ! Re-computing them...', /continue
          wv = reform(spc[i].(j)[*,0,z])
          ns = (size(spec))[2]
          if (size(spec))[0] eq 1L then ns = 1L
          err = fltarr((size(spec))[1],ns)+!values.f_nan
          for kkk=0L, ns-1L do $
            err[*,kkk] = spectrum_error(wv[*,kkk],spec[*,kkk])
        endif
        
        case state.r.combinestat of
            
            0: begin
                
                robustmeancomb,spec,thresh,mean,mvar,DATAVAR=err^2, $
                               MASK=mask,CANCEL=cancel
                if cancel then return
                
            end
            
            1: begin
                
                robustmeancomb,spec,thresh,mean,mvar,/RMS,MASK=mask,$
                               CANCEL=cancel
                if cancel then return
                
            end
            
            2: begin
                
                robustmeancomb,spec,thresh,mean,mvar,MASK=mask,CANCEL=cancel
                if cancel then return
                
                
            end
            
            3: begin
                
                meancomb,spec,mean,mvar,DATAVAR=err^2,MASK=mask,CANCEL=cancel
                if cancel then return
                
            end
            
            4: begin
                
                meancomb,spec,mean,mvar,/RMS,MASK=mask,CANCEL=cancel
                if cancel then return
                
            end
            
            5: begin
                
                meancomb,spec,mean,mvar,MASK=mask,CANCEL=cancel
                if cancel then return
                
            end
            
            6: begin
                
                medcomb,spec,mean,mvar,/MAD,MASK=mask,CANCEL=cancel
                if cancel then return
                
            end
            
            7: begin
                
                medcomb,spec,mean,mvar,MASK=mask,CANCEL=cancel
                if cancel then return
                
            end
            
        endcase
        
        ;!@!@!@!@!@ J. Gagne mask regions that have no data
        if max(strpos(strlowcase(tag_names(state.r)),'good_ranges')) ne -1L  and state.r.norders eq 21L then begin
          
          good_ranges = state.r.good_ranges
          mean[0:good_ranges[0L,j]-1L] = !values.f_nan
          mean[good_ranges[1L,j]+1L:*] = !values.f_nan
          mvar[0:good_ranges[0L,j]-1L] = 0.
          mvar[good_ranges[1L,j]+1L:*] = 0.
          
        endif
        
        sspec[*,0,j*state.r.fnaps+i] = reform(spc[i].(j)[*,0,0])
        sspec[*,1,j*state.r.fnaps+i] = mean
        sspec[*,2,j*state.r.fnaps+i] = sqrt(mvar)
        
    endfor

endfor

*state.r.combspec = sspec

end
;
;*****************************************************************************
;
pro fire_xcombspec_correctspec,state

cancel = 0

if state.w.modspeccontinue gt 3 then begin

    ok = dialog_message([['Cannot perform this operation.'],$
                         ['Please reload spectra and start over.']],/ERROR,$
                        DIALOG_PARENT=state.w.fire_xcombspec_base)
    cancel = 1
    return

endif

spc  = (*state.d.wspec)[state.r.ap]

for i = 0, state.r.norders-1 do begin
    
    spec = reform(spc.(i)[*,1,*])
    err  = reform(spc.(i)[*,2,*])

    nstack = speccor(spec,4,IERRSTACK=err,OERRSTACK=nerrstack, $
                     MASK=(*state.r.spcmask)[*,i,state.r.ap], $
                     CORRECTIONS=corrections,CANCEL=cancel)
    
    spc.(i)[*,1,*] = nstack
    spc.(i)[*,2,*] = nerrstack

endfor

(*state.d.wspec)[state.r.ap] = spc
(*state.r.corspec)[state.r.ap] = 1
fire_xcombspec_combinespec,state
fire_xcombspec_plotupdate,state

state.w.modspeccontinue = 3

end
;
;*****************************************************************************
;
pro fire_xcombspec_writefile,state

path = cfld(state.w.path_fld,7,CANCEL=cancel)
if cancel then return

outname = cfld(state.w.outfile_fld,7,CANCEL=cancel,/EMPTY)
if cancel then return

sspec = *state.r.combspec

;  Make new hdr.

if not state.w.basic then begin
    
    avehdr = avehdrs(*state.d.hdrinfo,TIME_OBS=state.r.time,$
                     POSANGLE=state.r.posangle,HA=state.r.ha,$
                     AIRMASS=state.r.airmass,CANCEL=cancel)
    history = ''    

endif else begin

    avehdr = (*state.d.hdrinfo)[0]
    history = 'This FITS header is from the first spectrum in the stack.  '

endelse

if min(strupcase(tag_names(avehdr.vals)) eq 'NAPS') eq 1 then $
  avehdr.vals.NAPS = state.r.fnaps

;  Create history

values = ['Robust Weighted Mean','Robust Mean (RMS)', $
          'Robust Mean (Mean Error)','Weighted Mean',$
          'Mean (RMS)','Mean (Mean Error)','Median (MAD)', $
          'Median (Median Error)']

history = history+'The spectra were combined using a '+ $
          values[state.r.combinestat]+'.  '

if state.r.combinestat le 2 then begin
    
    thresh = cfld(state.w.rthresh_fld,4,/EMPTY,CANCEL=cancel)
    if cancel then return
    history = history+'The robust threshold was '+strtrim(thresh,2)+'.'
    
endif

for i = 0, state.r.fnaps-1 do begin

    history=history+'  The files '+strjoin((*state.r.files),', ')+$
            ' were combined.  '

    history = history+'  Aperture '+string(i+1,FORMAT='(i2.2)')+' Info: '

    if finite((*state.r.scaleorder)[i]) eq 1 then begin
        
        history = history+'The scale factors were determined using order '+$
                  string((*state.r.scaleorder)[i],FORMAT='(i2.2)')+$
                  '.  The scale factors are '+$
                  strjoin(strtrim((*state.r.scales)[*,i],2),', ')+ $
                  '.  The median RMS' + ' deviation in each order is '+ $
                  strjoin(strtrim((*state.r.medrms)[*,i],2),', ')+'.  '
        
    endif 
    
    for j = 0,state.r.norders-1 do begin

        z = where((*state.r.spcmask)[*,j,i] eq 0,count)
        if count ne 0 then begin

            history = history+'  The spectrum(a) from file(s) '+ $
                      strjoin((*state.r.files)[z],', ')+' was (ere) ' + $
                      'removed from order '+ $
                      string(total((*state.r.orders)[j]),FORMAT='(i2.2)')+'.'

        endif

    endfor

    if (*state.r.corspec)[i] then history = history+ $
      '  The spectral shapes have been corrected.  '

endfor

names = tag_names(avehdr.vals)

fxhmake,newhdr,reform(sspec[*,1,*]),XTENSION=(names[0] eq 'XTENSION')
if state.r.norders ne 1L then $;J.Gagne !@!@!@!
  remove,0,newhdr

;ntags    = n_tags(avehdr.vals)

if state.r.norders ne 1L then begin ;J.Gagne !@!@!@!
  bad = where(strpos(names,'SIMPLE') ne -1, nbad); or strpos(names,'NAXIS') ne -1
  if nbad ne 0L then $
    remove, bad, names
endif
ntags = n_elements(names)

for i = 0, ntags - 1 do begin
    if state.r.norders eq 1L then begin;J.Gagne !@!@!@!
      if names[i] eq 'XTENSION' then continue
      if names[i] eq 'SIMPLE' then continue
      if names[i] eq 'BITPIX' then continue
      if strpos(names[i],'NAXIS') ne -1L then continue
    endif
    if names[i] eq 'HISTORY' then begin

        for j = 0, ceil(float(strlen(avehdr.vals.(i)))/70.)-1 do begin
            
            hist = strmid(avehdr.vals.(i),70*j,70)
            fxaddpar,newhdr,'HISTORY',hist
            
        endfor
       
    endif else fxaddpar,newhdr,names[i],avehdr.vals.(i),avehdr.coms.(i)

endfor

;Read first file for formatting example
ppath = cfld(state.w.path_fld,7)+(*state.r.files)[0]
string_replace, ppath, '//', '/'
struct0 = xmrdfits(ppath,1,newhdr) ;!@!@!@!

sxaddhist,' ',newhdr
sxaddhist,'Xcombspec History',newhdr
sxaddhist,' ',newhdr
history = mc_splittext(history,70)
sxaddhist,history,newhdr

fxaddpar,newhdr,'IRAFNAME',outname+'.fits'

;Write the extraction
norders = n_elements(struct0)
for i=0L, norders-1L do begin
  struct0[i].wave = reform(sspec[*,0L,i])
  struct0[i].fx = reform(sspec[*,1L,i])
  struct0[i].var = reform(sspec[*,2L,i]^2)
endfor
mwrfits, struct0, path+outname+'.fits', hdr0, /create

;Write a false raw object
;rawout = (*state.r.files)[0]
;string_replace, rawout, 'Obj_', 'fire_'
;rawpath = cfld(state.w.path_fld,7)
;del = path_sep()
;string_replace, rawpath, del+'Object'+del, del+'Raw'+del
;im0 = readfits(rawpath+rawout, /silent, hdr0)


fire_xvspec,path+outname+'.fits'

if state.r.textoutput then begin $

    t = (state.r.combineaps eq 'Yes') ? 1:state.r.naps
    npix = fxpar(newhdr,'NAXIS1')

    openw,lun,path+outname+'.txt', /GET_LUN

    for i = 0, n_elements(newhdr)-1 do printf, lun, newhdr[i]

    for i = 0, npix[0]-1 do begin

        printf, lun,  strjoin( reform(sspec[i,*,*],3*t*state.r.norders),'  ' )

    endfor
    close, lun
    free_lun, lun

endif

end
;
;******************************************************************************
;
pro fire_xcombspec_help,state

openr, lun, filepath('fire_xcombspec_helpfile.txt',ROOT_DIR=state.r.packagepath, $
                     SUBDIR='helpfiles'),/GET_LUN
nlines = numlines(filepath('fire_xcombspec_helpfile.txt',$
                           ROOT_DIR=state.r.packagepath,$
                           SUBDIR='helpfiles'))
array = strarr(nlines)
readf, lun, array
free_lun, lun

xmc_displaytext,array,GROUP_LEADER=state.w.fire_xcombspec_base

end
;
;******************************************************************************
;
pro fire_xcombspec_loadspec,state

;  Construct full file names

if state.r.inputmode eq 'Manual' then begin
    
;  Get path and file names
    
    path = cfld(state.w.path_fld,7,CANCEL=cancel)
    if cancel then return

    files = cfld(state.w.specfiles_fld,7,/EMPTY,CANCEL=cancel)
    if cancel then return
    
;  Construct array of full file names
    
    index    = (state.r.filereadmode eq 'Index') ? 1:0
    filename = (state.r.filereadmode eq 'Filename') ? 1:0
    if index then prefix = cfld(state.w.inprefix_fld,7,/EMPTY,CANCEL=cancel)
    if cancel then return
    
    files = fsextract(files,INDEX=index,FILENAME=filename,CANCEL=cancel)
    if cancel then return
    
    fullpaths = mkfullpath(path,files,INDEX=index,FILENAME=filename,$
                           NI=state.r.nint,$
                           PREFIX=prefix,SUFFIX='.fits',$
                           WIDGET_ID=state.w.fire_xcombspec_base,/EXIST,$
                           CANCEL=cancel)
    if cancel then return
    
endif else begin

    filename = cfld(state.w.filename_fld,7,CANCEL=cancel)
    if cancel then return    
    nl = numlines(filename)
    fullpaths = strarr(nl)
    openr,lun, filename,/GET_LUN
    line = ' '
    for i = 0, nl-1 do begin

        readf, lun, line
        fullpaths[i] = strtrim(line,2)

    endfor
    free_lun, lun
    path = ''
    
endelse

;  Read the first spectrum to get initial data

fire_readspec,fullpaths[0],first,hdr,obsmode,start,stop,shifts,norders,naps,orders,$
         xunits,yunits,slith_pix,slith_arc,slitw_pix,slitw_arc,$
         airmass,xtitle,ytitle,CANCEL=cancel, fits_extension=state.w.fits_extension

first = double(readfits(fullpaths[0],hdr,/silent));,ext=state.w.fits_extension
if n_elements(first) eq 1 then $
  first = xmrdfits2(fullpaths[0],hdr,telluric=state.w.telluric)

;Sort in increasing WV order
ss = sort(first[*,0])
first[*,0] = first[ss,0]
first[*,1] = first[ss,1]
first[*,2] = first[ss,2]

state.r.npix     = n_elements(first[*,0,0])
state.r.norders  = norders
state.r.naps     = naps
state.p.xtitle   = xtitle
lidx = strpos(ytitle,'(')
ridx = strpos(ytitle,')')
yunits = strmid(ytitle,lidx+1,ridx-lidx-1)
state.p.ytitle  = [ytitle,'!5Error ('+yunits+')','!5S/N']
*state.r.orders  = orders
state.r.nfiles   = n_elements(fullpaths)
state.w.modspeccontinue = 1

;  Modify the scale base

widget_control, state.w.scaleorder_dl,$
  SET_VALUE=[string(*state.r.orders,FORMAT='(i2.2)')]

widget_control, state.w.modtype_dl,$
  SET_DROPLIST_SELECT=0
state.r.modspec = 0

;  Modify plot window according to the number of orders

state.p.plotwinsize[1] = state.p.plotwinsize[1] > state.p.pixpp*state.r.norders
fire_xcombspec_modwinsize,state

;  Set up arrays checking to see if combining apertures

state.r.ap = 0

if state.r.combineaps eq 'No' then begin

    state.r.fnaps   = state.r.naps
    *state.r.medrms = fltarr(state.r.norders,state.r.fnaps)+!values.f_nan
    *state.r.scales = fltarr(state.r.nfiles,state.r.fnaps)+!values.f_nan
    state.r.nspec   = state.r.nfiles

    value   = strcompress(indgen(state.r.naps)+1, /RE)
    array   = fltarr(state.r.npix,3,state.r.nfiles)
    spcmask = intarr(state.r.nfiles,state.r.norders,state.r.fnaps)+1
    pmask   = intarr(state.r.npix,state.r.nfiles)+1

    widget_control, state.w.aperture_dl, SET_VALUE=value
    widget_control, state.w.aperture_dl, /SENSITIVE
    
endif else begin

    state.r.fnaps   = 1
    *state.r.medrms = fltarr(state.r.norders)+!values.f_nan
    *state.r.scales = fltarr(state.r.nfiles*state.r.naps)+!values.f_nan
    state.r.nspec   = state.r.nfiles*state.r.naps

    array   = fltarr(state.r.npix,3,state.r.nfiles*state.r.naps)
    spcmask = intarr(state.r.naps*state.r.nfiles,state.r.norders)+1
    pmask = intarr(state.r.npix,state.r.nfiles*state.r.naps)+1

    widget_control, state.w.aperture_dl, SENSITIVE=0

endelse
*state.r.scaleorder = fltarr(state.r.naps)+!values.f_nan
*state.r.corspec  = intarr(state.r.fnaps)


;  Create data structure for a single aperture

key     = 'Order'+string(00,FORMAT='(i2.2)')
spc     = create_struct(key,array)
pixmask = create_struct(key,pmask)

for i = 1, state.r.norders-1 do begin

    key     = 'Order'+string(i,FORMAT='(i2.2)')
    spc     = create_struct(spc,key,array)
    pixmask = create_struct(pixmask,key,pmask)

endfor

hdrinfo = replicate(gethdrinfo(hdr),state.r.nfiles)

;  If there are more than 1 apertures replicate the data structure.

spc     = replicate(spc,state.r.fnaps)
pixmask = replicate(pixmask,state.r.fnaps)

;  Load the data

for i = 0, state.r.nfiles-1 do begin

    data = readfits(fullpaths[i],hdr,/silent);ext=state.w.fits_extension
    if n_elements(data) eq 1L then data = xmrdfits2(fullpaths[i],hdr,telluric=state.w.telluric)
    
    ;if state.w.noshave eq 0 then begin
    ;  ;Apply a shaving algorithm
    ;  if norders eq 0L then $
    ;    data[*,1] = shave_spectrum(data[*,1],medbox=state.w.npix,nsig=26) else $
    ;    for kk=0L, norders-1L do $
    ;      data[*,1,kk] = shave_spectrum(data[*,1,kk],medbox=state.w.npix,nsig=26)
    ;      data[*,1,kk] = shave_spectrum(data[*,1,kk],NPIX=13L)
    ;endif
    
    ;Sort in increasing WV order
    ss = sort(data[*,0])
    data[*,0] = data[ss,0]
    data[*,1] = data[ss,1]
    data[*,2] = data[ss,2]

    copy_struct_inx,gethdrinfo(hdr),hdrinfo,index_to=i

    for j = 0, state.r.norders-1 do begin

        if state.r.combineaps ne 'Yes' then begin

            for k = 0, state.r.naps-1 do begin

                interpspec,data[*,0,j*state.r.naps+k],$
                  data[*,1,j*state.r.naps+k],first[*,0,j*state.r.naps+k],$
                  newflux,newerror,YAERROR=data[*,2,j*state.r.naps+k],$
                           /LEAVENANS
                
                if max(strpos(strlowcase(tag_names(state.r)),'good_ranges')) ne -1L and state.r.norders eq 21L then begin;J.Gagne !@!@!@
                  good_ranges = state.r.good_ranges
                  newflux[0:good_ranges[0L,j]-1L] = !values.f_nan
                  newflux[good_ranges[1L,j]+1L:*] = !values.f_nan
                  newerror[0:good_ranges[0L,j]-1L] = 0.
                  newerror[good_ranges[1L,j]+1L:*] = 0.
                endif
                
                spc[k].(j)[*,0,i] = first[*,0,j*state.r.naps+k]
                spc[k].(j)[*,1,i] = newflux
                spc[k].(j)[*,2,i] = newerror              
                
            endfor

        endif else begin

            for k = 0,state.r.naps-1 do begin

                interpspec,data[*,0,j*state.r.naps+k],$
                  data[*,1,j*state.r.naps+k],first[*,0,j*state.r.naps+k],$
                  newflux,newerror,YAERROR=data[*,2,j*state.r.naps+k],$
                           /LEAVENANS

                spc[0].(j)[*,0,i*state.r.naps+k] = first[*,0,j*state.r.naps+k]
                spc[0].(j)[*,1,i*state.r.naps+k] = newflux
                spc[0].(j)[*,2,i*state.r.naps+k] = newerror              

            endfor

        endelse


    endfor

endfor

*state.d.ospec    = spc
*state.d.wspec    = spc
*state.r.spcmask  = spcmask
*state.r.pixmask  = pixmask
*state.d.hdrinfo  = hdrinfo

;  Create filename string for output FITS header later

for i = 0,state.r.nfiles-1 do begin

    file = strmid(fullpaths[i],strpos(fullpaths[i],'/',/REVERSE_S)+1)
    sfile = (i eq 0) ? file:[sfile,file]

endfor
*state.r.files = sfile

;  Unfreeze the widget

widget_control, state.w.plot_base, /SENSITIVE
widget_control, state.w.box2_base,/SENSITIVE
widget_control, state.w.box3_base,/SENSITIVE
widget_control, state.w.box4_base,/SENSITIVE

fire_xcombspec_combinespec,state
fire_xcombspec_plotupdate,state

end
;
;******************************************************************************
;
pro fire_xcombspec_mkmask,state,index

cancel = 0

if state.w.modspeccontinue gt 4 then begin

    ok = dialog_message([['Cannot perform this operation.'],$
                         ['Please reload spectra and start over.']],/ERROR,$
                        DIALOG_PARENT=state.w.fire_xcombspec_base)
    cancel = 1
    return

endif
;J.GAGNE.MODIFICATION TO GET SCALED SPECTRA DISPLAYED IN THE MASKING PROCESS 
;spc     = (*state.d.ospec)[state.r.ap]
spc     = (*state.d.wspec)[state.r.ap]

zorder = total(where(*state.r.orders eq (*state.r.orders)[index]))

mask = xmkpixmask(reform(spc.(zorder)[*,0,0]),reform(spc.(zorder)[*,1,*]), $
                  ISTACKMASK=reform((*state.r.spcmask)[*,index,state.r.ap]),$
                  GROUP_LEADER=state.w.fire_xcombspec_base,CANCEL=cancel, $
                  INITMASK=(*state.r.pixmask)[state.r.ap].(zorder))

if cancel then return

(*state.r.pixmask)[state.r.ap].(zorder) = mask

widget_control, /HOURGLASS

fire_xcombspec_combinespec,state
fire_xcombspec_plotupdate,state

state.w.modspeccontinue = 4

end
;
;******************************************************************************
;
pro fire_xcombspec_modwinsize,state


geom = widget_info(state.w.plotwin,  /GEOMETRY)

if geom.xsize ne state.p.scrollsize[0] or $
  geom.ysize ne state.p.scrollsize[1] or $
  geom.draw_xsize ne state.p.plotwinsize[0] or $
  geom.draw_ysize ne state.p.plotwinsize[1]  then begin

    widget_control, state.w.col2_base, UPDATE=0
    widget_control, state.w.plotwin, /DESTROY
    
    state.w.plotwin = widget_draw(state.w.col2_base,$
                                  XSIZE=state.p.plotwinsize[0],$
                                  YSIZE=state.p.plotwinsize[1],$
                                  X_SCROLL_SIZE=state.p.scrollsize[0],$
                                  Y_SCROLL_SIZE=state.p.scrollsize[1],$
                                  UVALUE='Plot Window',$
                                  /BUTTON_EVENTS,$
                                  EVENT_PRO='fire_xcombspec_plotwin_event')
    
    widget_control, state.w.col2_base, UPDATE=1
    widget_control, state.w.plotwin, GET_VALUE = x
    state.p.plotwin_wid = x
    
    wdelete, state.p.pixmap_wid
    window, /FREE, /PIXMAP,XSIZE=state.p.plotwinsize[0],$
            YSIZE=state.p.plotwinsize[1]
    state.p.pixmap_wid = !d.window


endif


end
;
;******************************************************************************
;
pro fire_xcombspec_plotspec,state

spc  = *state.d.wspec
mask = *state.r.spcmask
pixmask = *state.r.pixmask

!p.multi[0] = state.r.norders
!p.multi[2] = state.r.norders

case state.p.spectype of
    
    0: ytitle = state.p.ytitle[0]
    
    1: ytitle = state.p.ytitle[1]
    
    2: ytitle = state.p.ytitle[2]
    
endcase

charsize = mc_strsize('!5A',0.01,WSIZE=state.p.scrollsize)
if state.r.norders ge 3 then charsize = charsize*2.0

if state.p.plottype eq 0 then begin

    for i = 0, state.r.norders-1 do begin
        
        j     = state.r.norders-1-i
        title = 'Order '+string((*state.r.orders)[j],FORMAT='(i2.2)')
        
;  Get plot range.
        
        case state.p.spectype of 

            0: spec = reform(spc[state.r.ap].(j)[*,1,*])

            1: spec = reform(spc[state.r.ap].(j)[*,2,*])

            2: spec = reform(spc[state.r.ap].(j)[*,1,*] / $
                             spc[state.r.ap].(j)[*,2,*])

        end

        medcomb,spec,med,MASK=pixmask[state.r.ap].(j);J. Gagne : Added Mask !@!@!@!@!
        ;yrange = [0.4*min(med,/NAN,MAX=max),1.5*max]
        ;stop
        yrange = [weighted_median(med,medval=.01),weighted_median(med,medval=.99)];J. Gagne !@!@!@!@!
        yrange += [-1,1] * (yrange[1]-yrange[0])*.05;J. Gagne !@!@!@!@!

;  Plot spectra    
        
        plot,spc[state.r.ap].(j)[*,0,0],spec[*,0],$
             /XSTY,/YSTY,YRANGE=yrange,/NODATA,TITLE=title,$
             XTITLE=state.p.xtitle,YTITLE=ytitle, $
             CHARSIZE=charsize
        
        for k = 0, state.r.nspec-1 do begin
            
            if mask[k,j,state.r.ap] eq 1 then begin

                flux = spec[*,k]
                z = where(pixmask[state.r.ap].(j)[*,k] eq 0,cnt)
                if cnt ne 0 then flux[z] = !values.f_nan

              oplot,spc[state.r.ap].(j)[*,0,k],flux,$
                    COLOR=state.r.colors[k],LINESTYLE=state.r.lines[k],$
                    PSYM=10

          endif
            
        endfor
        
    endfor

endif

if state.p.plottype eq 1 then begin

    wave = reform((*state.r.combspec)[*,0,*])
    
    case state.p.spectype of 
        
        0: spec = reform((*state.r.combspec)[*,1,*])
        
        1: spec = reform((*state.r.combspec)[*,2,*])
        
        2: spec = reform((*state.r.combspec)[*,1,*] / $
                         (*state.r.combspec)[*,2,*])
        
    end
    
    for i = 0, state.r.norders-1 do begin
        
        j     = state.r.norders-1-i
        title = 'Order '+string((*state.r.orders)[j],FORMAT='(i2.2)')
        
;  Get plot range.
        
        yrange = [0.4*min(spec[*,j*state.r.fnaps+state.r.ap],/NAN,MAX=max), $
                  1.5*max]
        
;  Plot spectra    
        
        plot,wave[*,j],spec[*,j*state.r.fnaps+state.r.ap],$
             /XSTY,/YSTY,YRANGE=yrange, $
             CHARSIZE=charsize,$
             TITLE=title, XTITLE=state.p.xtitle,YTITLE=ytitle,PSYM=10

    endfor
               
endif

!p.multi=0
delvarx,spc

end
;
;******************************************************************************
;
pro fire_xcombspec_plotupdate,state

wset,state.p.pixmap_wid
erase
fire_xcombspec_plotspec,state

wset, state.p.plotwin_wid
device, COPY=[0,0,state.p.plotwinsize[0],state.p.plotwinsize[1],0,0,$
                  state.p.pixmap_wid]

end
;
;******************************************************************************
;
pro fire_xcombspec_scalespec,state,index

cancel = 0

if state.w.modspeccontinue gt 1 then begin

    ok = dialog_message([['Cannot perform this operation.'],$
                         ['Please reload spectra and start over.']],/ERROR,$
                        DIALOG_PARENT=state.w.fire_xcombspec_base)
    cancel = 1
    return

endif

spc = (*state.d.ospec)[state.r.ap]
good = where((*state.r.spcmask)[*,index,state.r.ap] eq 1)
zorder = total(where(*state.r.orders eq (*state.r.orders)[index]))
(*state.r.scaleorder)[state.r.ap] = (*state.r.orders)[index]

xscalespec,reform(spc.(zorder)[*,0,0]),reform(spc.(zorder)[*,1,*]), $
           reform((*state.r.spcmask)[*,index,state.r.ap]),scales,wrange, $
           GROUP_LEADER=state.w.fire_xcombspec_base,XTITLE=state.p.xtitle,$
           YTITLE=state.p.ytitle[0],CANCEL=cancel
           
if cancel then return

widget_control, /HOURGLASS
for i = 0, state.r.norders-1 do begin
    
    s = reform(spc.(i)[*,1,*])
    e = reform(spc.(i)[*,2,*])
    
    for k = 0, n_elements((*state.r.spcmask)[*,i,state.r.ap])-1 do begin
        
        s[*,k] = scales[k] * s[*,k]
        e[*,k] = sqrt(scales[k]^2 * e[*,k]^2)
        
    endfor
    
    spc.(i)[*,1,*] = s
    spc.(i)[*,2,*] = e        
    
;  Get median RMS across each order
    
    meancomb,reform(s[*,good]),mean,var,/RMS
    (*state.r.medrms)[i,state.r.ap] = median(sqrt(var),/EVEN)
    
;  Store scales
    
    (*state.r.scales)[*,state.r.ap] = scales

endfor
(*state.d.wspec)[state.r.ap] = spc

;endelse
fire_xcombspec_combinespec,state
fire_xcombspec_plotupdate,state

state.w.modspeccontinue = 1

end
;
;******************************************************************************
;
pro fire_xcombspec_selectspec,state,index

cancel = 0

print, state.w.modspeccontinue
if state.w.modspeccontinue gt 2 then begin

    ok = dialog_message([['Cannot perform this operation.'],$
                         ['Please reload spectra and start over.']],/ERROR,$
                        DIALOG_PARENT=state.w.fire_xcombspec_base)
    cancel = 1
    return

endif



spc = *state.d.wspec
mask = *state.r.spcmask

case state.p.spectype of
    
    0: begin

        ytitle = state.p.ytitle[0]
        spec = reform(spc[state.r.ap].(index)[*,1,*])
    
    end
    1: begin

        ytitle = state.p.ytitle[1]
        spec = reform(spc[state.r.ap].(index)[*,2,*])
    
    end
    2: begin

        ytitle = state.p.ytitle[2]
        spec = reform(spc[state.r.ap].(index)[*,1,*]) / $
               reform(spc[state.r.ap].(index)[*,2,*])
    
    end
endcase

xselectspec,reform(spc[state.r.ap].(index)[*,0,0]),spec,omask,$
            GROUP_LEADER=state.w.fire_xcombspec_base, $
            IMASK=mask[*,index,state.r.ap],XTITLE=state.p.xtitle,$
            YTITLE=ytitle,CANCEL=cancel
if cancel then return

widget_control, /HOURGLASS    
(*state.r.spcmask)[*,index,state.r.ap] = omask

;(*state.d.wspec)[state.r.ap]      = (*state.d.ospec)[state.r.ap]
;(*state.r.medrms)[*,state.r.ap]   = fltarr(state.r.norders)+!values.f_nan
;(*state.r.scales)[*,state.r.ap]   = fltarr(state.r.nfiles)+!values.f_nan
;(*state.r.corspec)[state.r.ap]    = 0.0
;(*state.r.scaleorder)[state.r.ap] = !values.f_nan

fire_xcombspec_combinespec,state
fire_xcombspec_plotupdate,state

;widget_control, state.w.scaleorder_dl,SET_DROPLIST_SELECT=0

delvarx,spc

;mess = [['You must rescale the spectra and/or correct'],$
;        ['the spectral shape if you have already done so.']]
;ok = dialog_message(mess,/INFORMATION,DIALOG_PARENT=state.w.fire_xcombspec_base)

state.w.modspeccontinue = 2

end
;
;***************************  Main Program  ***************************************************
;
pro fire_xcombspec,instrfile,BASIC=basic,PATH_INPUT=path_input,PREFIX_INPUT=prefix_input,FILES_INPUT=files_input,SAVE_INPUT=save_input, QUITONWRITE=quitonwrite, FULL_FILE_NAMES=full_file_namess, FITS_EXTENSION=fits_extension, BLOCKING=blocking, TELLURIC=telluric, NOSHAVE=noshave, NPIX=npix

mkct

if ~keyword_set(npix) then npix = 7
if ~keyword_set(fits_extension) then fits_extension = 0

device, RETAIN=2

if ~keyword_set(path_input) then begin
  cd, current = dir0
  path_input = dir0
endif
if ~keyword_set(prefix_input) then $
  prefix_input = 'Obj_'

;  Determine the instrument in use, default is FIRE.

if n_elements(instrfile) eq 0 then instrfile = 'FIRE.dat'

last   = strpos(!path,'Spextool')
first  = strpos(!path,':',last,/REVERSE_SEARCH)
result = strmid(!path,first+1,last-first+7)

path = cpath(result,CANCEL=cancel)
if cancel then return

readinstrfile,path+'data/'+instrfile,instr,irafname,gain,readnoise,itime,$
  coadds,ndrs,slowcnt,readtime,time,posangle,ha,airmass,nint,bdpxmk,keywords,$
  ncols,nrows,CANCEL=cancel

;  Set the fonts

getfonts,buttonfont,textfont

;  Build three structures that will hold important info.
;  w - contains info pertaining to widget operations.
;  r - contains info pertaining to the reduction process.
;  d - contains all of the data.

w = {aperture_dl:0L,$
     basic:keyword_set(BASIC),$
     bdpxthresh1_fld:[0L,0L],$
     bdpxthresh2_fld:[0L,0L],$
     box2_base:0L,$
     box3_base:0L,$
     box4_base:0L,$
     col2_base:0L,$
     file_base:0L,$
     filename_fld:[0L,0L],$
     inprefix_fld:[0L,0L],$
     keyboard:0L,$
     manual_base:0L,$
     mask_dl:0L,$
     medscale:0L,$
     modspeccontinue:1,$
     modtype_dl:0L,$
     outfile_fld:[0L,0L],$
     path_fld:[0L,0L],$
     plot_base:0L,$
     plotwin:0L,$
     rthresh_fld:[0L,0L],$
     scaleorder_dl:0L,$
     scalerange_fld:[0L,0L],$
     specfiles_fld:[0L,0L],$
     fire_xcombspec_base:0L,$
     fits_extension:fits_extension,$
     telluric:keyword_set(telluric),$
     noshave:keyword_set(noshave),$
     npix:npix}

r = {absscalerange:[!values.f_nan,!values.f_nan],$
     airmass:airmass,$
     ap:0,$
     colors:[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,$
             1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,$
             1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,$
             1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,$
             1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,$
             1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],$
     combineaps:'No',$
     combinestat:0,$
     combspec:ptr_new(2),$
     corspec:ptr_new(2),$
     files:ptr_new(fltarr(2)),$
     filereadmode:'Index',$
     fnaps:0,$
     ha:ha,$
     ifileformat:'FITS',$
     inputmode:'Manual',$
     lines:[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,$
            1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,$
            2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,$
            3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,$
            4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,$
            5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5],$
     meanerror:1,$
     median:0,$
     medrms:ptr_new(2),$
     modspec:0,$
     naps:0,$
     nfiles:0,$
     nint:nint,$
     norders:0,$
     npix:0L,$
     nspec:0,$
     orders:ptr_new(2),$     
     packagepath:path,$
     pixmask:ptr_new(2),$
     posangle:posangle,$
     scales:ptr_new(scales),$
     scaleorder:ptr_new(0),$
     scalerange:[!values.f_nan,!values.f_nan],$
     spcmask:ptr_new(2),$
     specidx:0,$
     stackmask:ptr_new(2),$
     textoutput:0,$
     time:time,$
     good_ranges:[[156L,1100L],$;0
     [195L,1589L],$;1
     [536L,1850L],$;2
     [660L,2030L],$;3
     [577L,1998L],$;4
     [551L,2020L],$;5
     [474L,2000L],$;6
     [245L,1830L],$;7
     [542L,2198L],$;8
     [261L,1992L],$;9
     [92L,1906L],$;10
     [31L,1940L],$;11
     [84L,2094L],$;12
     [124L,2194L],$;13
     [76L,2188L],$;14
     [200L,2358L],$;15
     [261L,2485L],$;16
     [520L,2806L],$;17
     [460L,2853L],$;18
     [572L,2963L],$;19
     [882L,2998L]]}


p = {buffer:[0,0],$
     pixpp:200.0,$
     pixmap_wid:0L,$
     plotwin_wid:0,$
     plotwinsize:[512,570],$
     plottype:0,$
     spectype:0,$
     scrollsize:[512,570],$
     xtitle:'',$
     ytitle:['','','']}

d = {ospec:ptr_new(fltarr(2)),$
     wspec:ptr_new(fltarr(2)),$
     hdrinfo:ptr_new(strarr(2))}

state = {w:w,r:r,d:d,p:p}

;  Build the widget.

state.w.fire_xcombspec_base = widget_base(TITLE='fire_xcombspec', $
                                     /COLUMN,$
                                     /TLB_SIZE_EVENTS)

   quit_button = widget_button(state.w.fire_xcombspec_base,$
                               FONT=buttonfont,$
                               VALUE='Quit',$
                               EVENT_PRO='fire_xcombspec_event',$
                               UVALUE='Quit')

   state.w.keyboard = widget_text(state.w.fire_xcombspec_base, $
                                  /ALL_EVENTS, $
                                  SCR_XSIZE=1, $
                                  SCR_YSIZE=1, $
                                  UVALUE='Keyboard', $
                                  EVENT_PRO='fire_xcombspec_event',$
                                  VALUE='')

   row_base = widget_base(state.w.fire_xcombspec_base,$
                          /ROW)

      col1_base = widget_base(row_base,$
                              EVENT_PRO='fire_xcombspec_event',$
                              /COLUMN)

         box1_base = widget_base(col1_base,$
                                 /COLUMN,$
                                 /FRAME)

            row = widget_base(box1_base,$
                              /ROW,$
                              /BASE_ALIGN_CENTE)
            
               label = widget_label(row,$
                                    VALUE='1.  Load Spectra:',$
                                    /ALIGN_LEFT,$
                                    FONT=buttonfont)
               
               bg = cw_bgroup(row,$
                              ['Manual','File'],$
                              /ROW,$
                              LABEL_LEFT='',$
                              /RETURN_NAME,$
                              /NO_RELEASE,$
                              UVALUE='Input Mode',$
                              FONT=buttonfont,$
                              /EXCLUSIVE,$
                              SET_VALUE=0)

            blank = widget_base(box1_base,$
                                /BASE_ALIGN_CENTER)

               state.w.manual_base = widget_base(blank,$
                                                 /COLUMN,$
                                                 /BASE_ALIGN_LEFT)

                  row = widget_base(state.w.manual_base,$
                                    /ROW,$
                                    /BASE_ALIGN_CENTER)
                  
                     button = widget_button(row,$
                                            FONT=buttonfont,$
                                            VALUE='Path',$
                                            UVALUE='Path Button',$
                                            EVENT_PRO='fire_xcombspec_event')
                     
                     field = coyote_field2(row,$
                                           LABELFONT=buttonfont,$
                                           FIELDFONT=textfont,$
                                           TITLE=':',$
                                           UVALUE='Path Field',$
                                           VALUE=path_input,$
                                           XSIZE=25,$
                                           /CR_ONLY,$
                                           TEXTID=textid)
                     state.w.path_fld = [field,textid]               
               
                  bg = cw_bgroup(state.w.manual_base,$
                                 ['Filename','Index'],$
                                 /ROW,$
                                 LABEL_LEFT='File Read Mode:',$
                                 /RETURN_NAME,$
                                 /NO_RELEASE,$
                                 UVALUE='Readmode',$
                                 FONT=buttonfont,$
                                 /EXCLUSIVE,$
                                 SET_VALUE=1)
            
                  field = coyote_field2(state.w.manual_base,$
                                        LABELFONT=buttonfont,$
                                        FIELDFONT=textfont,$
                                        TITLE='Input Prefix:',$
                                        UVALUE='Input Prefix',$
                                        VALUE=prefix_input, $
                                        XSIZE=25,$
                                        /CR_oNLY,$
                                        TEXTID=textid)
                  state.w.inprefix_fld = [field,textid]
                  
                  row = widget_base(state.w.manual_base,$
                                    /ROW,$
                                    /BASE_ALIGN_CENTER)
                  
                     button = widget_button(row,$
                                            FONT=buttonfont,$
                                            VALUE='Files',$
                                            UVALUE='Spectra Files Button')
                     
                     field = coyote_field2(row,$
                                           LABELFONT=buttonfont,$
                                           FIELDFONT=textfont,$
                                           TITLE=':',$
                                           UVALUE='Spectra Files Field',$
                                           VALUE=files_input,$
                                           XSIZE=25,$
                                           /CR_ONLY,$
                                           TEXTID=textid)
                     state.w.specfiles_fld = [field,textid]
                     
               state.w.file_base = widget_base(blank,$
                                               /COLUMN,$
                                               /BASE_ALIGN_CENTER)

                  row = widget_base(state.w.file_base,$
                                    /ROW,$
                                    /BASE_ALIGN_CENTER)
                  
                     button = widget_button(row,$
                                            FONT=buttonfont,$
                                            VALUE='File Name',$
                                            UVALUE='File Name')
                     
                     field = coyote_field2(row,$
                                           LABELFONT=buttonfont,$
                                           FIELDFONT=textfont,$
                                           TITLE=':',$
                                           UVALUE='File Name Field',$
                                           XSIZE=25,$
                                           /CR_ONLY,$
                                           TEXTID=textid)
                     state.w.filename_fld = [field,textid]
                     
            combineap_bg = cw_bgroup(box1_base,$
                                     FONT=buttonfont,$
                                     ['Yes','No'],$
                                     /ROW,$
                                     /RETURN_NAME,$
                                     /NO_RELEASE,$
                                     /EXCLUSIVE,$
                                     LABEL_LEFT='Combine Apertures:',$
                                     UVALUE='Combine Apertures',$
                                     SET_VALUE=1)

            load = widget_button(box1_base,$
                                 FONT=buttonfont,$
                                 VALUE='Load Spectra',$
                                 UVALUE='Load Spectra')

         state.w.box2_base = widget_base(col1_base,$
                                         /COLUMN,$
                                         /FRAME)

            label = widget_label(state.w.box2_base,$
                                 VALUE='2.  Modify Spectra',$
                                 /ALIGN_LEFT,$
                                 FONT=buttonfont)
                                           
            row = widget_base(state.w.box2_base,$
                              /ROW,$
                              /BASE_ALIGN_CENTER)

               state.w.modtype_dl = widget_droplist(row,$
                                                    FONT=buttonfont,$
                                                    TITLE='',$
                                                    VALUE=['a. Scale', $
                                                           'b. Remove',$
                                                           'c. Correct Shape',$
                                                           'd. Mask Pixels'],$
                                                    UVALUE='Modify Order Type')
            
               state.w.scaleorder_dl = widget_droplist(row,$
                                                       FONT=buttonfont,$
                                                 TITLE='with Order:',$
                                                       VALUE='01',$
                                                       UVALUE='Modify Order')

;            load = widget_button(state.w.box2_base,$
;                                 FONT=buttonfont,$
;                                 VALUE='Correct Spectral Shape',$
;                                 UVALUE='Correct Spectral Shape')

;            state.w.mask_dl = widget_droplist(state.w.box2_base,$
;                                              FONT=buttonfont,$
;                                              TITLE='Make Mask:',$
;                                              VALUE='None',$
;                                              UVALUE='Make Mask')

            widget_control, state.w.box2_base, SENSITIVE=0
            
         state.w.box3_base = widget_base(col1_base,$
                                         /COLUMN,$
                                         /FRAME)
         
            label = widget_label(state.w.box3_base,$
                                 VALUE='3.  Combine Spectra',$
                                 /ALIGN_LEFT,$
                                 FONT=buttonfont)
            
            values = ['Robust Weighted Mean','Robust Mean (RMS)', $
                      'Robust Mean (Mean Error)','Weighted Mean',$
                      'Mean (RMS)','Mean (Mean Error)','Median (MAD)', $
                      'Median (Median Error)']
            combine_dl = widget_droplist(state.w.box3_base,$
                                         FONT=buttonfont,$
                                         TITLE='Statistic:',$
                                         VALUE=values,$
                                         UVALUE='Combination Statistic')

            fld = coyote_field2(state.w.box3_base,$
                                LABELFONT=buttonfont,$
                                FIELDFONT=textfont,$
                                TITLE='Robust Threshold:',$
                                UVALUE='Robust Threshold',$
                                XSIZE=7,$
                                VALUE='8.0',$
                                EVENT_PRO='fire_xcombspec_event',$
                                /CR_ONLY,$
                                TEXTID=textid)
            state.w.rthresh_fld = [fld,textid]

            widget_control, state.w.box3_base, SENSITIVE=0

         state.w.box4_base = widget_base(col1_base,$
                                         /COLUMN,$
;                                         /BASE_ALIGN_LEFT,$
                                         /FRAME)

            label = widget_label(state.w.box4_base,$
                                 VALUE='4.  Write Spectra',$
                                 /ALIGN_LEFT,$
                                 FONT=buttonfont)
            
            row = widget_base(state.w.box4_base,$
                              /ROW,$
                              /BASE_ALIGN_CENTER)


               field = coyote_field2(row,$
                                     LABELFONT=buttonfont,$
                                     FIELDFONT=textfont,$
                                     TITLE='Output File:',$
                                     UVALUE='Output File',$
                                     VALUE=save_input,$
                                     XSIZE=15,$
                                     /CR_ONLY,$
                                     TEXTID=textid)
               state.w.outfile_fld = [field,textid]

               outformat_bg = cw_bgroup(row,$
                                        FONT=buttonfont,$
                                        ['Text'],$
                                        /ROW,$
                                        /RETURN_NAME,$
                                        /NONEXCLUSIVE,$
                                        UVALUE='Output Format',$
                                        SET_VALUE=[0])
               


            combine_button = widget_button(state.w.box4_base,$
                                           FONT=buttonfont,$
                                           VALUE='Write File',$
                                           UVALUE='Write File')

         widget_control, state.w.box4_base, SENSITIVE=0

         state.w.col2_base = widget_base(row_base,$
                                         EVENT_PRO='fire_xcombspec_event',$
                                         /COLUMN)

            state.w.plot_base = widget_base(state.w.col2_base,$
                                            /ROW,$
                                            /BASE_ALIGN_CENTER,$
                                            FRAME=1)
            
               state.w.aperture_dl = widget_droplist(state.w.plot_base,$
                                                     FONT=buttonfont,$
                                                     TITLE='Aperture:',$
                                                     VALUE='1',$
                                                     UVALUE='Aperture')   

               plot_dl = widget_droplist(state.w.plot_base,$
                                         FONT=buttonfont,$
                                         TITLE='Plot:',$
                                         VALUE=['Raw','Combined'],$
                                         UVALUE='Plot Type')   

               plot_dl = widget_droplist(state.w.plot_base,$
                                         FONT=buttonfont,$
                                         TITLE='Type:',$
                                         VALUE=['Flux','Error','SNR'],$
                                         UVALUE='Spectra Type')   

               widget_control, state.w.plot_base,SENSITIVE=0

               state.w.plotwin = widget_draw(state.w.col2_base,$
                                             XSIZE=state.p.plotwinsize[0],$
                                             YSIZE=state.p.plotwinsize[1],$
                                         X_SCROLL_SIZE=state.p.scrollsize[0],$
                                         Y_SCROLL_SIZE=state.p.scrollsize[1],$
                                             UVALUE='Plot Window',$
                                             /BUTTON_EVENTS,$
                                             EVENT_PRO= $
                                             'fire_xcombspec_plotwin_event')

   button = widget_button(state.w.fire_xcombspec_base,$
                          FONT=buttonfont,$
                          VALUE='Help',$
                          EVENT_PRO='fire_xcombspec_event',$
                          UVALUE='Help')

; Get things running.  Center the widget using the Fanning routine.

centertlb,state.w.fire_xcombspec_base

widget_control, state.w.fire_xcombspec_base, /REALIZE

;  Get plotwin ids

widget_control, state.w.plotwin, GET_VALUE=x
state.p.plotwin_wid = x

window, /FREE, /PIXMAP,XSIZE=state.p.plotwinsize[0],$
  YSIZE=state.p.plotwinsize[1]
state.p.pixmap_wid = !d.window

; Start the Event Loop. This will be a non-blocking program.
XManager, 'fire_xcombspec', $
  state.w.fire_xcombspec_base, $
  /NO_BLOCK,$
  ;NO_BLOCK=~keyword_set(blocking), $
  EVENT_HANDLER='fire_xcombspec_resize',$
  CLEANUP='fire_xcombspec_cleanup'
;tmp = widget_event(/XMANAGER_BLOCK);!@!@!@!@!
;if keyword_set(blocking) then $
;  tmp = widget_event(/XMANAGER_BLOCK)

;  Get sizes of things now for resizing

widget_control, state.w.fire_xcombspec_base, TLB_GET_SIZE=result

widget_geom = widget_info(state.w.fire_xcombspec_base, /GEOMETRY)

state.p.buffer[0] = widget_geom.xsize-state.p.scrollsize[0]
state.p.buffer[1] = widget_geom.ysize-state.p.scrollsize[1]


; Put state variable into the user value of the top level base.

widget_control, state.w.fire_xcombspec_base, SET_UVALUE=state, /NO_COPY

end






