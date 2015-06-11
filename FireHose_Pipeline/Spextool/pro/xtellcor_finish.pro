;+
; NAME:
;     xtellcor_finish
;
; PURPOSE:
;     Telluric corrects a spectrum using an output telluric spectrum.
;    
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     xtellcor_finish
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
;     Writes a spectral SpeX FITS image to disk
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
;     Just applies a previously derived telluric correction spectrum
;     to a object spectrum.
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2002 - Written by M. Cushing, Institute for Astronomy, UH
;     2003-04-30 - Fixed input the xfindshift and fixed writing
;                  procedure.
;     2006-06-12 - Fixed bug with the shifts found by Dawn.
;     2006-08-01 - Fixed bug with the YUNITS keyword.
;-
;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro xtellcor_finish_event,event

widget_control, event.id,  GET_UVALUE = uvalue
if uvalue eq 'Quit' then begin

    widget_control, event.top, /DESTROY
    goto, getout

endif

widget_control, event.top, GET_UVALUE = state, /NO_COPY
widget_control, /HOURGLASS

case uvalue of

    'Correct Spectrum': xtellcor_finish_tellcor,state

    'Get Shift': xtellcor_finish_getshift,state

    'Object Spectra Button': begin

        path = cfld(state.w.path_fld,7,CANCEL=cancel)
        if cancel then return
        obj = dialog_pickfile(DIALOG_PARENT=state.w.xtellcor_finish_base,$
                              PATH=path,/MUST_EXIST,FILTER='*.fits')
        if obj eq '' then goto, cont
        widget_control,state.w.objspectra_fld[1], $
          SET_VALUE = strmid(obj[0],strpos(obj,'/',/REVERSE_S)+1)
        setfocus,state.w.objspectra_fld

    end

    'Output Format': state.r.textoutput=event.select

    'Load Spectra': xtellcor_finish_loadspec,state

    'Path Button': begin

        path= dialog_pickfile(/DIRECTORY,$
                              DIALOG_PARENT=state.w.xtellcor_finish_base,$
                              TITLE='Select Path',/MUST_EXIST)
        
        if path ne '' then begin

            path = cpath(path,WIDGET_ID=state.w.xtellcor_finish_base,$
                         CANCEL=cancel)
            if cancel then return
            widget_control,state.w.path_fld[1],SET_VALUE = path
            setfocus,state.w.path_fld

        endif

    end

    'Plot Object Ap': begin

        state.r.plotobjap = event.index
        z = where(*state.d.objorders eq state.r.plotobjorder)
        widget_control, state.w.shift,SET_VALUE='Shift: '+ $
                        string((*state.r.shift)[z,state.r.plotobjap], $
                               FORMAT='(f6.2)')
        
    end
    
    'Plot Object Order': begin
        
        state.r.plotobjorder = (*state.d.objorders)[event.index]
        z = where(*state.d.objorders eq state.r.plotobjorder)
        widget_control, state.w.shift,SET_VALUE='Shift: '+ $
                        string((*state.r.shift)[z,state.r.plotobjap], $
                               FORMAT='(f6.2)')        

    end
    'Shift Spectra': begin

        state.r.restore = event.value
        if event.value eq 'Manual' then begin

            widget_control, state.w.temp_base, MAP=1
            widget_control, state.w.dl_base, MAP=1

        endif
        if event.value eq 'No'  then begin

            widget_control, state.w.temp_base, MAP=0
            widget_control, state.w.dl_base, MAP=0

        endif

    end

    'Telluric Spectra Button': begin

        path = cfld(state.w.path_fld,7,CANCEL=cancel)
        if cancel then return
        tel = dialog_pickfile(DIALOG_PARENT=state.w.xtellcor_finish_base,$
                             PATH=path,/MUST_EXIST,FILTER='*.fits')
        
        if tel eq '' then goto, cont
        widget_control,state.w.telspectra_fld[1],$
          SET_VALUE = strmid(tel[0],strpos(tel,'/',/REVERSE_S)+1)

    end


endcase

;  Put state variable into the user value of the top level base.
 
cont: 
widget_control, state.w.xtellcor_finish_base, SET_UVALUE=state, /NO_COPY
getout:

end
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
;
;******************************************************************************
;
pro xtellcor_finish_cleanup,base

widget_control, base, GET_UVALUE = state, /NO_COPY
if n_elements(state) ne 0 then begin

    ptr_free, state.d.telspec
    ptr_free, state.d.objspec
    ptr_free, state.d.objorders
    ptr_free, state.d.telorders
    ptr_free, state.d.objhdr
    ptr_free, state.d.telhdr

endif
state = 0B

end
;
;******************************************************************************
;
pro xtellcor_finish_getshift,state

zz = where((*state.d.objorders) eq state.r.plotobjorder)

obj_wave = (*state.d.objspec)[*,0,zz*state.d.objnaps+state.r.plotobjap]
obj_flux = (*state.d.objspec)[*,1,zz*state.d.objnaps+state.r.plotobjap]

z        = where((*state.d.telorders) eq state.r.plotobjorder)
tel_wave = (*state.d.telspec)[*,0,z]
tel_flux = (*state.d.telspec)[*,1,z]

interpspec,tel_wave,tel_flux,obj_wave,new_flux
shift = xfindshift(obj_wave,obj_flux,new_flux, $
                   INITSHIFT=total((*state.r.shift)[zz,state.r.plotobjap]),$
                   XTITLE=state.r.xtitle,CANCEL=cancel)

if not cancel then begin

    (*state.r.shift)[zz,state.r.plotobjap] = shift
    z = where(*state.d.objorders eq state.r.plotobjorder)
    widget_control, state.w.shift,SET_VALUE='Shift: '+ $
                    string((*state.r.shift)[zz,state.r.plotobjap], $
                           FORMAT='(f6.2)')

endif

end
;
;******************************************************************************
;
pro xtellcor_finish_loadspec,state

path = cfld(state.w.path_fld,7,CANCEL=cancel)
if cancel then return

telfile = cfld(state.w.telspectra_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
telfile = cfile(path+telfile,WIDGET_ID=state.w.xtellcor_finish_base,$
                CANCEL=cancel)
if cancel then return

objfile = cfld(state.w.objspectra_fld, 7,/EMPTY,CANCEL=cancel)
if cancel then return
objfile = cfile(path+objfile,WIDGET_ID=state.w.xtellcor_finish_base,$
                CANCEL=cancel)
if cancel then return

readspec,telfile,tel,telhdr,telobsmode,start,stop,telnorders,telnaps,$
  telorders,telxunits,telyunits,slith_pix,slith_arc,slitw_pix,slitw_arc,$
  telairmass,xtitle,ytitle

readspec,objfile,obj,objhdr,objobsmode,start,stop,objnorders,objnaps,$
  objorders,objxunits,objyunits,slith_pix,slith_arc,slitw_pix,slitw_arc,$
  objairmass

*state.d.telspec = tel
*state.d.objspec = obj
*state.d.objhdr  = objhdr
*state.d.telhdr  = telhdr
*state.d.telorders = telorders
*state.d.objorders = objorders
state.d.objnaps    = objnaps
state.r.plotobjorder = objorders[0]

*state.r.shift = fltarr(objnorders,objnaps)
state.r.dairmass = telairmass-objairmass

state.r.xtitle = xtitle
state.r.ytitle = ytitle

widget_control, state.w.objorder_dl,SET_VALUE=string(objorders,FORMAT='(i2.2)')
widget_control, state.w.objap_dl,SET_VALUE=string(indgen(objnaps)+1,$
                                                  FORMAT='(i2.2)')

;  Compute airmass difference between obj and std.

widget_control, state.w.message,SET_VALUE=$
                [['Telluric Airmass:'+string(telairmass,FORMAT='(f7.4)')+ $
                  ', Object Airmass:'+$
                  string(objairmass,FORMAT='(f7.4)')],$
['(Telluric-Obj) Airmass: '+string((telairmass-objairmass),FORMAT='(f7.4)')]]

end
;
;******************************************************************************
;
pro xtellcor_finish_tellcor,state

oname = cfld(state.w.objoname_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

path = cfld(state.w.path_fld,7,CANCEL=cancel)
if cancel then return
path = cpath(path,WIDGET_ID=state.w.xtellcor_finish_base,CANCEL=cancel)
if cancel then return
    
telfile = cfld(state.w.telspectra_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

tel = *state.d.telspec
obj = *state.d.objspec

for i = 0, n_elements(*state.d.objorders)-1 do begin

    z = where((*state.d.telorders) eq (*state.d.objorders)[i],count)
    if count eq 0 then begin
      if n_elements(*state.d.telorders) eq 1 and n_elements(*state.d.objorders) eq 1 then begin
        count = 1
        z = 0
      endif
    endif
    if count eq 0 then goto, cont
    for j = 0,state.d.objnaps-1 do begin

        k = i*state.d.objnaps+j
        interpspec,tel[*,0,z],tel[*,1,z],obj[*,0,k],nflux,nerror,$
          YAERROR=tel[*,2,z]

;  Now shift spectrum.

        x = findgen(n_elements(tel[*,0,z]))
        interpspec,x+(*state.r.shift)[i,j],nflux,x,nnflux,nnerror, $
                   YAERROR=nerror

        obj[*,1,k] = nnflux*obj[*,1,k]
        obj[*,2,k] = sqrt(nnflux^2*obj[*,2,k]^2 + obj[*,1,k]^2*nnerror^2 )

    endfor

endfor

cont:

;  Now write it to disk

hdr = *state.d.objhdr
sxdelpar,hdr,['DIVISOR','BEAM','GRSTCNT','CALMIR','DIT','OSF','QTH_LAMP',$
              'INC_LAMP','IR_SRC','ARG_SRC','SHUTTER','AIMAGE','SKY']


;  If statements for old xtellcor_general files that don't have units
;  in the FITS keywords

xunits = fxpar(*state.d.telhdr,'XUNITS',COMMENT=cxunits)
if n_elements(cxunits) eq 0 then cxunits = ''
yunits = fxpar(*state.d.telhdr,'YUNITS',COMMENT=cyunits)
if n_elements(cyunits) eq 0 then cyunits = ''
ytitle = fxpar(*state.d.telhdr,'YTITLE',COMMENT=cytitle)
if n_elements(xytitle) eq 0 then cytitle = ''

ang = '!5!sA!r!u!9 %!5!n'

case strcompress(yunits,/RE) of 

    'ergss-1cm-2A-1/DNs-1': begin

       ytitle = '!5f!D!7k!N!5 (ergs s!E-1!N cm!E-2!N '+ang+'!E-1!N)'
       yunits = 'ergss-1cm-2A-1'

    end
    'ergss-1cm-2Hz-1/DNs-1': begin

       ytitle = '!5f!D!7m!N!5 (ergs s!E-1!N cm!E-2!N Hz!E-1!N)'
       yunits = 'ergss-1cm-2Hz-1'

    end
    'Wm-2um-1/DNs-1': begin

       ytitle = '!5f!D!7k!N!5 (W m!E-2!N !7l!5m!E-1!N)'
       yunits = 'Wm-2um-1'

    end

    'Wm-2Hz-1/DNs-1': begin

       ytitle = '!5f!D!7m!N!5 (W m!E-2!N Hz!E-1!N)'
       yunits = 'Wm-2Hz-1'

    end
    'Jy/DNs-1': begin

       ytitle = '!5f!D!7m!N!5 (Jy)' 
       yunits = 'Jy'

    end

    'ergss-1cm-2A-1': begin

       ytitle = '!5f!D!7k!N!5 (ergs s!E-1!N cm!E-2!N '+ang+'!E-1!N)'
       yunits = 'ergss-1cm-2A-1'

    end
    'ergss-1cm-2Hz-1': begin

       ytitle = '!5f!D!7m!N!5 (ergs s!E-1!N cm!E-2!N Hz!E-1!N)'
       yunits = 'ergss-1cm-2Hz-1'

    end
    'Wm-2um-1': begin

       ytitle = '!5f!D!7k!N!5 (W m!E-2!N !7l!5m!E-1!N)'
       yunits = 'Wm-2um-1'

    end
    'Wm-2Hz-1': begin

       ytitle = '!5f!D!7m!N!5 (W m!E-2!N Hz!E-1!N)'
       yunits = 'Wm-2Hz-1'

    end
    'Jy': begin

       ytitle = '!5f!D!7m!N!5 (Jy)' 
       yunits = 'Jy'
       
    end
    else: begin

       ytitle=''
       yunits = ''
       
    end
endcase


fxaddpar,hdr,'IRAFNAME',strtrim(oname+'.fits',2)
fxaddpar,hdr,'XUNITS',xunits,cxunits
fxaddpar,hdr,'YUNITS',yunits,cyunits
fxaddpar,hdr,'YTITLE',ytitle,' IDL Y title'
fxaddpar,hdr,'DAIRMASS',state.r.dairmass, $
         ' Average airmass difference between std and obj'

history = 'This spectrum was telluric corrected using the telluric '+$
  'correction spectra '+strtrim(telfile,2)+'.'

for i = 0, state.d.objnaps-1 do begin

    history = history+'  The telluric correction spectra for aperture '+ $
              string(i+1,FORMAT='(i2.2)')+' were shifted by '+ $
              strjoin(strtrim((*state.r.shift)[*,i],2),', ')+' pixels.'

endfor

history = mc_splittext(history,70)
sxaddhist,history,hdr

writefits, path+oname+'.fits',obj,hdr
xvspec,path+oname+'.fits'

if state.r.textoutput then begin

    norders = fxpar(hdr,'NORDERS')    
    npix = fxpar(hdr,'NAXIS1')
    openw,lun,path+oname+'.txt', /GET_LUN
    
    for i = 0, n_elements(hdr)-1 do printf, lun, hdr[i]
    
    for i = 0, npix-1 do begin
        
        printf, lun,  strjoin( reform(obj[i,*,*],3*norders),'  ' )
        
    endfor
    close, lun
    free_lun, lun
    
endif    

print,'Wrote the corrected spectrum to '+strtrim(path+oname,2)


orders = long( strsplit( fxpar(*state.d.objhdr,'ORDERS'), ',', /EXTRACT) )




end
;
;******************************************************************************
;
; ------------------------------Main Program---------------------------------- 
;
;******************************************************************************
;
;
pro xtellcor_finish

last   = strpos(!path,'Spextool')
first  = strpos(!path,':',last,/REVERSE_SEARCH)
packagepath = strmid(!path,first+1,last-first+8)

;  Load color table

mkct

;  Set the fonts

getfonts,buttonfont,textfont

w = {bmag_fld:[0L,0L],$
     dl_base:0L,$
     inprefix_fld:[0L,0L],$
     message:0L,$
     objoname_fld:[0L,0L],$
     objorder_dl:0L,$
     objspectra_fld:[0L,0L],$
     path_fld:[0L,0L],$
     telspectra_fld:[0L,0L],$
     temp_base:0L,$
     objap_dl:0L,$
     pixshift_fld:[0L,0L],$
     shift:0L,$
     xtellcor_finish_base:0L,$
     vmag_fld:[0L,0L]}

r = {dairmass:0.0,$
     plotobjap:0,$
     plotobjorder:0,$
     shift:ptr_new(2),$
     textoutput:0,$
     xtitle:'',$
     ytitle:''}

d = {telspec:ptr_new(fltarr(2)),$
     telhdr:ptr_new(2),$
     objhdr:ptr_new(fltarr(2)),$
     objspec:ptr_new(fltarr(2)),$
     objorders:ptr_new(fltarr(2)),$
     objnaps:0,$
     telorders:ptr_new(fltarr(2))}

state = {w:w,r:r,d:d}

state.w.xtellcor_finish_base = widget_base(TITLE='Xtellcor Finish', $
                                          EVENT_PRO='xtellcor_finish_event',$
                                          /COLUMN)
    
   quit_button = widget_button(state.w.xtellcor_finish_base,$
                               FONT=buttonfont,$
                               VALUE='Done',$
                               UVALUE='Quit')

   state.w.message = widget_text(state.w.xtellcor_finish_base, $
                                 VALUE='',$
                                 YSIZE=2)
   
   box1_base = widget_base(state.w.xtellcor_finish_base,$
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
                               XSIZE=30,$
                               /CR_ONLY,$
                               TEXTID=textid)
         state.w.path_fld = [field,textid]    
         
      row = widget_base(box1_base,$
                        /ROW,$
                        /BASE_ALIGN_CENTER)
      
         obj_but = widget_button(row,$
                                 FONT=buttonfont,$
                                 VALUE='Obj Spectra',$
                                 UVALUE='Object Spectra Button')
         
         obj_fld = coyote_field2(row,$
                                 LABELFONT=buttonfont,$
                                 fieldfont=textfont,$
                                 TITLE=':',$
                                 UVALUE='Object Spectra Field',$
                                 XSIZE=18,$
                                 /CR_ONLY,$
                                 TEXTID=textid)
         state.w.objspectra_fld = [obj_fld,textid]
         
      row = widget_base(box1_base,$
                        /ROW,$
                        /BASE_ALIGN_CENTER)
      
         tel_but = widget_button(row,$
                                 FONT=buttonfont,$
                                 VALUE='Tel Spectra',$
                                 UVALUE='Telluric Spectra Button')
         
         tel_fld = coyote_field2(row,$
                                 LABELFONT=buttonfont,$
                                 FIELDFONT=textfont,$
                                 TITLE=':',$
                                 UVALUE='Telluric Spectra Field',$
                                 XSIZE=18,$
                                 /CR_ONLY,$
                                 TEXTID=textid)
         state.w.telspectra_fld = [tel_fld,textid]

      button = widget_button(box1_base,$
                             FONT=buttonfont,$
                             VALUE='Load Spectra',$
                             UVALUE='Load Spectra')
      
      box2_base = widget_base(state.w.xtellcor_finish_base,$
                           /COLUMN,$
                           /FRAME)

         label = widget_label(box2_base,$
                              VALUE='2.  Get Shift',$
                              FONT=buttonfont,$
                              /ALIGN_LEFT)

         row = widget_base(box2_base,$
                           /ROW,$
                           /BASE_ALIGN_CENTER)

         state.w.objap_dl = widget_droplist(row,$
                                            FONT=buttonfont,$
                                            TITLE='Aperture:',$
                                            VALUE='01',$
                                            UVALUE='Plot Object Ap')
         
         state.w.objorder_dl = widget_droplist(row,$
                                               FONT=buttonfont,$
                                               TITLE='Order:',$
                                               VALUE='01',$
                                               UVALUE='Plot Object Order')

         state.w.shift = widget_label(box2_base,$
                                      /ALIGN_LEFT,$
                                      /DYNAMIC_RESIZE,$
                                      FONT=buttonfont,$
                                      VALUE='Shift: 0.0')
            
            shift = widget_button(box2_base,$
                                  VALUE='Get Shift',$
                                  UVALUE='Get Shift',$
                                  FONT=buttonfont)

      box3_base = widget_base(state.w.xtellcor_finish_base,$
                              /COLUMN,$
                              /FRAME)
      
         label = widget_label(box3_base,$
                              VALUE='3.  Write File',$
                              FONT=buttonfont,$
                              /ALIGN_LEFT)
         
         outformat_bg = cw_bgroup(box3_base,$
                                  FONT=buttonfont,$
                                  ['Text Output'],$
                                  /ROW,$
                                  /RETURN_NAME,$
                                  /NONEXCLUSIVE,$
                                  LABEL_LEFT='',$
                                  UVALUE='Output Format',$
                                  SET_VALUE=[0])

         oname = coyote_field2(box3_base,$
                               LABELFONT=buttonfont,$
                               FIELDFONT=textfont,$
                               TITLE='Object File:',$
                               UVALUE='Object File Oname',$
                               xsize=18,$
                               textID=textid)
         state.w.objoname_fld = [oname,textid]
         
         write = widget_button(box3_base,$
                               VALUE='Correct Spectrum',$
                               UVALUE='Correct Spectrum',$
                               FONT=buttonfont)

; Get things running.  Center the widget using the Fanning routine.

centertlb,state.w.xtellcor_finish_base
widget_control, state.w.xtellcor_finish_base, /REALIZE


; Start the Event Loop. This will be a non-blocking program.

XManager, 'xtellcor_finish', $
  state.w.xtellcor_finish_base, $
  CLEANUP='xtellcor_finish_cleanup',$
  /NO_BLOCK

widget_control, state.w.xtellcor_finish_base, SET_UVALUE=state, /NO_COPY


end
