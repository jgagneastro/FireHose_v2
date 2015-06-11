;+
; NAME:
;     fire_xtellcor
;    
; PURPOSE:
;     Runs the SpeX telluric correction.
;    
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     fire_xtellcor
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
;     Writes a SpeX spectral FITS file to disk of the telluric
;     corrected spectra and optional the telluric correction spectrum
;     and the convolved Vega model.
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
;     Follow the directions
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2001 - Written by M. Cushing, Institute for Astonomy, UH
;-
;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro fire_xtellcor_event, event

func_name = "fire_xtellcor_event"

widget_control, event.id,  GET_UVALUE=uvalue
if uvalue eq 'Quit' then begin

    widget_control, event.top, /DESTROY
    goto, getout

endif


widget_control, event.top, GET_UVALUE=state, /NO_COPY
widget_control, /HOURGLASS
 
case uvalue of

    'Additional Output': begin

        if event.value eq 'Telluric' then state.r.telluricoutput=event.select
        if event.value eq 'A0 V' then state.r.vegaoutput=event.select

    end

    'B Magnitude Field': setfocus, state.w.vmag_fld

    'Construct Kernel': begin

		if n_elements(*state.d.stdorders) EQ 2 then begin
			fire_siren, func_name + ": ERROR! Spectra not loaded yet!"
		endif else begin
	       if (state.r.stdorder EQ 0) then $
	          state.r.stdorder = (*state.d.stdorders)[5]
	       fire_xtellcor_conkernel,state
		endelse
    
    end

    'Construct Telluric Spectra': fire_xtellcor_contellspec,state

    'Get Shift': fire_xtellcor_getshift,state

    'Help': fire_xtellcor_help,state

    'Load Spectra': begin
       fire_xtellcor_loadspec,state
    end

    'Method': begin

        state.r.method = event.value
        
        if event.value eq 'Deconvolution' then widget_control, $
          state.w.box2a_base,MAP=1 else $
          widget_control, state.w.box2a_base, MAP=0
        
    end

    'Object Spectra Button': begin

        path = cfld(state.w.path_fld,7,CANCEL=cancel)
        if cancel then return
        obj = dialog_pickfile(DIALOG_PARENT=state.w.fire_xtellcor_base,$
                              PATH=path,/MUST_EXIST,FILTER='*.fits')
        if obj eq '' then goto, cont
        widget_control,state.w.objspectra_fld[1], $
          SET_VALUE=strmid(obj[0],strpos(obj,state.w.dirsep,/REVERSE_S)+1)
        setfocus,state.w.objspectra_fld

    end

    'Output Format': state.r.textoutput=event.select

    'Path Button': begin

        path= dialog_pickfile(/DIRECTOR,DIALOG_PARENT=state.w.fire_xtellcor_base,$
                              TITLE='Select Path',/MUST_EXIST)
        
        if path ne '' then begin

            path = cpath(path,WIDGET_ID=state.w.fire_xtellcor_base,CANCEL=cancel)
            if cancel then return
            widget_control,state.w.path_fld[1],SET_VALUE=path
            setfocus,state.w.path_fld
            setfocus,state.w.stdspectra_fld

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
        widget_control, state.w.shift,SET_VALUE='Shift: '+ $
                        string((*state.r.shift)[event.index, $
                                                state.r.plotobjap], $
                               FORMAT='(f6.2)')

    end

    'Shift All': begin

        z = where(*state.d.objorders eq state.r.plotobjorder)
        (*state.r.shift)[*,state.r.plotobjap] = (*state.r.shift)[z,state.r.plotobjap] 
        

    end

    'Standard Order': state.r.stdorder = (*state.d.stdorders)[event.index]

    'Scale Lines': fire_xtellcor_getscales,state

    'Spectrum Units': begin

        case event.index of 

            0: state.r.units = 'ergs s-1 cm-2 A-1'
            1: state.r.units = 'ergs s-1 cm-2 Hz-1'
            2: state.r.units = 'W m-2 um-1'
            3: state.r.units = 'W m-2 Hz-1'
            4: state.r.units = 'Jy'
        
        endcase 

    end

    'Standard Spectra Button': begin

        path = cfld(state.w.path_fld,7,CANCEL=cancel)
        if cancel then return
        std = dialog_pickfile(DIALOG_PARENT=state.w.fire_xtellcor_base,$
                             PATH=path,/MUST_EXIST,FILTER='*.fits')
        
        if std eq '' then goto, cont
        widget_control,state.w.stdspectra_fld[1],$
          SET_VALUE = strmid(std[0],strpos(std,state.w.dirsep,/REVERSE_S)+1)
        setfocus,state.w.stdspectra_fld

    end

    'Standard Spectra Field': setfocus,state.w.bmag_fld

    'V Magnitude Field': setfocus, state.w.objspectra_fld

    'Write File': fire_xtellcor_writefile,state

    else:

endcase

;  Put state variable into the user value of the top level base.
 
cont: 
widget_control, state.w.fire_xtellcor_base, SET_UVALUE=state, /NO_COPY
getout:

end
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro fire_xtellcor_cleanup,base

widget_control, base, GET_UVALUE = state, /NO_COPY
if n_elements(state) ne 0 then begin

    ptr_free, state.r.scales
    ptr_free, state.r.shift
    ptr_free, state.d.kernels
    ptr_free, state.d.objhdr
    ptr_free, state.d.objorders
    ptr_free, state.d.objspec
    ptr_free, state.d.nstd
    ptr_free, state.d.stddisp
    ptr_free, state.d.stdfwhm
    ptr_free, state.d.stdhdr
    ptr_free, state.d.stdorders
    ptr_free, state.d.stdspec
    ptr_free, state.d.tellspec
    ptr_free, state.d.fvega
    ptr_free, state.d.wvega
    ptr_free, state.d.vegaspec
    ptr_free, state.d.cfvega
    ptr_free, state.d.cf2vega
    
    cutreg = *state.r.cutreg
    for i = 0, n_tags(cutreg)-1 do ptr_free, cutreg.(i)
    
    ptr_free, state.r.cutreg


endif
state = 0B

end
;
;******************************************************************************
;
pro fire_xtellcor_changeunits,wave,tellspec,tellspec_error,state

c = 2.99792458e+8


ang = '!5!sA!r!u!9 %!5!n'
case state.r.units of 

    'ergs s-1 cm-2 A-1': state.r.nytitle = $
      '!5f!D!7k!N!5 (ergs s!E-1!N cm!E-2!N '+ang+'!E-1!N'

    'ergs s-1 cm-2 Hz-1': begin

        tellspec        = temporary(tellspec)* wave^2 * (1.0e-2 / c)
        tellspec_error  = temporary(tellspec_error)* wave^2 * (1.0e-2 / c)
        state.r.nytitle = '!5f!D!7m!N!5 (ergs s!E-1!N cm!E-2!N Hz!E-1!N'


    end
    'W m-2 um-1': begin 

        tellspec        = temporary(tellspec)*10.
        tellspec_error  = temporary(tellspec_error)*10.
        state.r.nytitle = '!5f!D!7k!N!5 (W m!E-2!N !7l!5m!E-1!N'
            
    end
    'W m-2 Hz-1': begin

        tellspec        = temporary(tellspec)* wave^2 * (1.0e-5 / c)
        tellspec_error  = temporary(tellspec_error)* wave^2 * (1.0e-5 / c)
        state.r.nytitle = '!5f!D!7m!N!5 (W m!E-2!N Hz!E-1!N' 

    end
    
    'Jy': begin

        tellspec        = temporary(tellspec)* wave^2 * (1.0e21 / c) 
        tellspec_error  = temporary(tellspec_error)* wave^2 * (1.0e21 / c) 
        state.r.nytitle = '!5f!D!7m!N!5 (Jy' 

    end

endcase

end
;
;******************************************************************************
;
pro fire_xtellcor_conkernel,state

func_name = "fire_xtellcor_conkernel"

if state.r.continue lt 1 then begin

    ok = dialog_message('Previous steps not complete.',/ERROR,$
                        DIALOG_PARENT=state.w.fire_xtellcor_base)
    return
    
endif

;vdisp = (state.d.stdobsmode eq 'LowRes15') ? 2.96495e-4:1.49774e-5

vdisp = 1.81081307382E-05

if state.r.method eq 'Deconvolution' then begin

    idx = where(*state.d.stdorders eq state.r.stdorder)

    xconkern,(*state.d.stdspec)[*,0,idx],(*state.d.stdspec)[*,1,idx],$
      *state.d.wvega,*state.d.fvega,*state.d.cfvega,*state.d.cf2vega,$
      state.d.awave,state.d.atrans,wline,kernel,scale,vshift,maxdev,rmsdev,$
      PARENT=state.w.fire_xtellcor_base,XTITLE=state.r.xtitle,$
      YTITLE=state.r.ytitle,CANCEL=cancel
    if cancel then return

;  Now we must scale the kernel to each order.
;  First, get this kernel in SpeX pixels.

    ndat = n_elements(kernel)
    kx_vegapix = findgen(ndat)

	;; Make sure that the user has actually fit a kernal.
	if n_elements(kernel) EQ 2 then begin
		fire_siren, func_name + ": ERROR! Complete all steps before accepting kernel!"
		RETURN
	endif

    result = gaussfit(kx_vegapix,kernel,a,NTERMS=3)
    
    kx_spexpix = (kx_vegapix - a[1])*vdisp/total((*state.d.stddisp)[idx])

;  Now construct the kernels for the other orders.

    first_order = 11

    for i = 0,state.d.stdnorders-1 do begin

        disp = total((*state.d.stddisp)[i])
        kx_spexwave = kx_spexpix*total((*state.d.stddisp)[i])

        del = max(kx_spexwave,MIN=min)-min
        npix = del/vdisp
        if not npix mod 2 then npix = npix + 1
        kx_vegawave = (findgen(npix)-npix/2.)*vdisp

        linterp,kx_spexwave,kernel,kx_vegawave,nkern
        
        nkern = nkern/total(nkern)
        key = 'Order'+string((*state.d.stdorders)[i],FORMAT='(i2.2)')
        str = (i eq 0) ? create_struct(key,nkern):create_struct(str,key,nkern)
        
    endfor


    state.r.vshift   = vshift
    state.r.scale    = scale
    *state.d.kernels = str
    state.r.maxdev   = maxdev
    state.r.rmsdev   = rmsdev

endif else begin

;  Construct the kernels for each order

    case state.d.slitw_arc of 
        
        0.3: parms = [0.0,0.969822,0.768373]
        
        0.5: parms = [0.0,1.60602,0.714163]
        
        0.6: parms = [0.0,1.93459,0.726736]
        
        0.8: parms = [0.0,2.59172,0.751883]

        1.6: parms = [0.0,5.16330,0.782718]
        
        3.0: parms = [0.0,9.63019,0.834605]

    endcase

    for i = 0, state.d.stdnorders-1 do begin
        
        vkernw_pix = (*state.d.stdfwhm)[i]/vdisp

        nkernel = round(10.*vkernw_pix)
        if not nkernel mod 2 then nkernel = nkernel + 1
        
        kernel_vx = (findgen(nkernel)-fix(nkernel)/2)*vdisp
        kernel_sx = kernel_vx/(*state.d.stddisp)[i]

        kern   = instrprof(kernel_sx,parms)
        
        key = 'Order'+string((*state.d.stdorders)[i],FORMAT='(i2.2)')
        str1 = (i eq 0) ? create_struct(key,kern):create_struct(str1,key,kern)

    endfor

    *state.d.kernels = str1
    state.r.vshift   = 0.0
    state.r.scale    = 1.0

endelse

state.r.continue = 2

end
;
;******************************************************************************
;
pro fire_xtellcor_contellspec,state

if state.r.continue lt 3 then begin
    
    ok = dialog_message('Previous steps not complete.',/ERROR,$
                        DIALOG_PARENT=state.w.fire_xtellcor_base)
    return
    
endif

norders = N_ELEMENTS((*state.d.objorders)) ; fxpar(*state.d.objhdr,'NORDERS')
;jjb hack... this match statement is not working correctly.  Just
;going to use the same order
;match,*state.d.stdorders,*state.d.objorders,stdidx
stdidx = INDGEN(N_ELEMENTS(*state.d.stdorders))

*state.d.tellspec = (*state.d.stdspec)[*,*,stdidx]
*state.d.vegaspec = (*state.d.stdspec)[*,*,stdidx]
(*state.d.vegaspec)[*,2,*] = 1.0

for i = 0, norders-1 do begin

    print, 'Constructing Telluric Correction Spectrum for Order '+$
      strtrim((*state.d.objorders)[i],2)+'...'
    
    z = where(*state.d.stdorders eq (*state.d.objorders)[i],count)
    telluric, (*state.d.stdspec)[*,0,z],(*state.d.stdspec)[*,1,z],$
      (*state.d.stdspec)[*,2,z],state.r.vmag,(state.r.bmag-state.r.vmag),$
      (*state.d.kernels).(i),(*state.r.scales)[*,norders-1-z],*state.d.wvega,$
      *state.d.fvega,*state.d.cfvega,*state.d.cf2vega,state.r.vshift,$
      tellcor,tellcor_error,scvega,CANCEL=cancel
   
;  Perform interpolations if necessary

    cutreg = *state.r.cutreg
    ndat = n_elements(*cutreg.(i))

    if ndat ne 1 then begin
        
        nreg = ndat/2
        for j = 0, nreg-1 do begin
            
            xrange = reform((*cutreg.(i))[(j*2):(j*2+1)])
            tabinv,(*state.d.stdspec)[*,0,z],xrange,idx
            idx = round(idx)
                
            x = [(*state.d.stdspec)[idx[0],0,z],$
                 (*state.d.stdspec)[idx[1],0,z]]
            
            y = [tellcor[idx[0]],tellcor[idx[1]]]

            e = [tellcor_error[idx[0]],tellcor_error[idx[1]]]
            
            ;!@!@!@!@! J. Gagne : Avoir crashes when no handles inside spectral grasp 
            if total(finite(y)) eq 0 then begin
              coeff = [1.,0.]
              coeffe = [0.,0.]
            endif else begin
              coeff  = poly_fit1d(x,y,1,/SILENT)
              coeffe = poly_fit1d(x,e,1,/SILENT)
            endelse
            
            tellcor[idx[0]:idx[1]]=$
              poly((*state.d.stdspec)[idx[0]:idx[1],0,z],coeff)

            tellcor_error[idx[0]:idx[1]]=$
              poly((*state.d.stdspec)[idx[0]:idx[1],0,z],coeffe)

        endfor

    endif
    
    fire_xtellcor_changeunits,(*state.d.stdspec)[*,0,z],tellcor,tellcor_error,state
    (*state.d.tellspec)[*,1,i] = tellcor
    (*state.d.tellspec)[*,2,i] = tellcor_error
    (*state.d.vegaspec)[*,1,i] = scvega

endfor

cont:

state.r.continue = 4

end
;
;******************************************************************************
;
pro fire_xtellcor_getscales,state

if state.r.continue lt 2 then begin
    
    ok = dialog_message('Previous steps not complete.',/ERROR,$
                        DIALOG_PARENT=state.w.fire_xtellcor_base)
    return
    
endif

vshift = state.r.vshift
IP     = (state.r.method eq 'IP') ? 1:0

wvega  = *state.d.wvega
fvega  = *state.d.fvega
fcvega = *state.d.cfvega

;if state.d.stdobsmode eq 'ShortXD' then xputs = state.d.xputs

match,*state.d.stdorders,*state.d.objorders,stdidx,objidx


xscalelines,(*state.d.stdspec)[*,*,stdidx],(*state.d.stdorders)[stdidx],$
            state.r.vmag,(state.r.bmag-state.r.vmag),wvega,fvega,fcvega, $
            *state.d.cf2vega,*state.d.kernels,vshift,(*state.d.objspec)[*,*,objidx], $
            (*state.d.objorders)[objidx],state.d.objnaps,state.d.awave,state.d.atrans, $
            state.r.hlines,state.r.hnames,state.r.scale,scales,cutreg, $
            XPUTS=xputs,PARENT=state.w.fire_xtellcor_base,XTITLE=state.r.xtitle, $
            YTITLE=state.r.ytitle,CANCEL=cancel
;J. Gagne : Scales does have the right value.
;JGAGNE : VERIFY HERE THAT THE KEYWORD CANCEL IS OFF AND THAT THE SCALES VARIABLE CONTAINS SOMETHING
if not cancel then begin
    
    *state.r.scales = scales
    *state.r.cutreg = cutreg
    state.r.continue = 3

    state.r.vshift = vshift
    state.r.vrot   = 0.0

endif

end
;
;******************************************************************************
;
pro fire_xtellcor_help,state

openr, lun, filepath('fire_xtellcor_helpfile.txt',ROOT_DIR=state.r.packagepath, $
                     SUBDIR='helpfiles'),/GET_LUN
nlines = numlines(filepath('fire_xtellcor_helpfile.txt',$
                           ROOT_DIR=state.r.packagepath,$
                           SUBDIR='helpfiles'))
array = strarr(nlines)
readf, lun, array
free_lun, lun

xmc_displaytext,array,GROUP_LEADER=state.w.fire_xtellcor_base,CANCEL=cancel
if cancel then return

end
;
;******************************************************************************
;
pro fire_xtellcor_loadspec,state

;  Get files.

path = cfld(state.w.path_fld,7,CANCEL=cancel)
if cancel then return

std = cfld(state.w.stdspectra_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
std = cfile(path+std,WIDGET_ID=state.w.fire_xtellcor_base,CANCEL=cancel)
if cancel then return

bmag = cfld(state.w.bmag_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
vmag = cfld(state.w.vmag_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return

obj = cfld(state.w.objspectra_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
obj = cfile(path+obj,WIDGET_ID=state.w.fire_xtellcor_base,CANCEL=cancel)
if cancel then return

;  Read spectra and load data.

;fire_readspec,std,std,stdhdr,stdobsmode,start,stop,stdnorders,stdnaps,stdorders,$
;         stdxunits,stdyunits,slith_pix,slith_arc,slitw_pix,slitw_arc, $
;         stdairmass,xtitle,ytitle

readspec_jjb,std,std,stdhdr,stdnorders,stdnaps,stdorders,$
         slitw_pix,slitw_arc, $
         stdairmass, stddisp


readspec_jjb,obj,obj,objhdr,objnorders,objnaps,objorders,$
         slitw_pix,slitw_arc, $
         objairmass, objdisp

;norders = (size(obj))[3]
;for kk=0L, norders-1L do $
;  obj[*,1,kk] = shave_spectrum(obj[*,1,kk],npix=25,nsig=26)


*state.d.stdspec   = std
*state.d.stdorders = stdorders
*state.d.stdhdr    = stdhdr
state.d.slitw_pix  = slitw_pix
state.d.slitw_arc  = slitw_arc
state.d.stdobsmode = 'Echelle';strtrim(stdobsmode,2)
*state.d.stddisp   = stddisp ;mcfxpar(stdhdr,'D*P*')
*state.d.stdfwhm   = *state.d.stddisp*state.d.slitw_pix
state.d.stdnorders = stdnorders

*state.r.shift = fltarr(objnorders,objnaps)

*state.d.objspec   = obj
*state.d.objorders = objorders
*state.d.objhdr    = objhdr
state.d.objnaps    = objnaps


state.r.bmag = bmag
state.r.vmag = vmag
state.r.vshift = 0.0
state.r.stdairmass = stdairmass
state.d.dairmass = stdairmass-objairmass

state.r.xtitle = 'microns';xtitle
state.r.ytitle = 'counts';ytitle

;  Compute airmass difference between obj and std.

widget_control, state.w.message,SET_VALUE=$
  'Std Airmass:'+string(stdairmass,FORMAT='(f7.4)')+', Obj Airmass:'+$
  string(objairmass,FORMAT='(f7.4)')+', (Std-Obj) Airmass: '+$
  string((stdairmass-objairmass),FORMAT='(f7.4)')

widget_control, state.w.objorder_dl,$
  SET_VALUE=string(objorders,FORMAT='(i2.2)')
widget_control, state.w.objap_dl,$
  SET_VALUE=string(indgen(objnaps)+1,FORMAT='(i2.2)')
widget_control, state.w.stdorder_dl,$
  SET_VALUE=string(stdorders,FORMAT='(i2.2)'), SET_DROPLIST_SELECT=5
state.r.plotobjorder = (*state.d.objorders)[0]

;  Load Vega model

;; if state.d.stdobsmode eq 'LowRes15' then begin ;

;;     restore, filepath('lvega99.sav',ROOT_DIR=state.r.packagepath,$
;;                       SUBDIR='data') 
;;     state.d.atrans = smooth(state.d.atrans,100)

;; endif else

 restore, filepath('lvega5.sav',ROOT_DIR=state.r.packagepath, $
                            SUBDIR='data') 

;jjb hack... multiplied Vega model to get flux into better units

*state.d.wvega = wvin/10000.
*state.d.cf2vega = fc2vin
*state.d.cfvega = fcvin
*state.d.fvega = fvin

state.r.continue=1

end
;
;******************************************************************************
;
pro fire_xtellcor_getshift,state

if state.r.continue lt 4 then begin
    
    ok = dialog_message('Previous steps not complete.',/ERROR,$
                        DIALOG_PARENT=state.w.fire_xtellcor_base)
    return
    
endif

z = where((*state.d.objorders) eq state.r.plotobjorder)

obj_wave = (*state.d.objspec)[*,0,z*state.d.objnaps+state.r.plotobjap]
obj_flux = (*state.d.objspec)[*,1,z*state.d.objnaps+state.r.plotobjap]

tel_wave = (*state.d.tellspec)[*,0,z]   ;hack by jjb
tel_flux = (*state.d.tellspec)[*,1,z]



interpspec,tel_wave,tel_flux,obj_wave,new_flux

shift = fire_xfindshift(obj_wave,obj_flux,new_flux,$
                   INITSHIFT=total((*state.r.shift)[z,state.r.plotobjap]),$
                   XTITLE=state.r.xtitle,PARENT=state.w.fire_xtellcor_base, $
                   CANCEL=cancel)

if not cancel then begin

    (*state.r.shift)[z,state.r.plotobjap] = shift
    widget_control, state.w.shift,SET_VALUE='Shift: '+ $
                    string((*state.r.shift)[z,state.r.plotobjap], $
                           FORMAT='(f6.2)')
        
endif

state.r.continue=5

end
;
;******************************************************************************
;
pro fire_xtellcor_writefile,state

;Changing this to interface with the XIDL pipeline ... dump spectra back into the object structure

if state.r.continue lt 5 then begin
    
    ok = dialog_message('Previous steps not complete.',/ERROR,$
                        DIALOG_PARENT=state.w.fire_xtellcor_base)
    return
    
endif

obj = cfld(state.w.objoname_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

path = cfld(state.w.path_fld,7,CANCEL=cancel)
if cancel then return
path = cpath(path,WIDGET_ID=state.w.fire_xtellcor_base,CANCEL=cancel)
if cancel then return
    
stdfile = cfld(state.w.stdspectra_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

xidlobj = cfld(state.w.objspectra_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

obj_strct = xmrdfits(path+xidlobj,1,objhdr)   ;read in XIDL structure

;  Write the telluric correction spectrum to disk

if state.r.telluricoutput then begin
    
;;  Get info from obj and std hdr.
  
     splitarr = strsplit(stdfile, '_', /extract)
     ;JJB hack to deal with case of "raw"
     name = './raw/fire_'+splitarr[n_elements(splitarr)-1]
     IF file_test(name) EQ 1 THEN begin
        (*state.d.stdhdr) = headfits(name) 
     endif ELSE begin 
        rawpath = (state.r.rawpath)
        (*state.d.stdhdr) = headfits(strtrim(rawpath,2)+'fire_'+splitarr[n_elements(splitarr)-1]) 
     endelse

     (*state.d.objhdr) = objhdr

     norders   = n_elements(obj_strct)
     start     = min(obj_strct.slitid)
     stopord      = max(obj_strct.slitid)
     orders    = strtrim(start,2)+','+strtrim(stopord,2)
     obsmode   = sxpar(*state.d.stdhdr,'GRISM')
     std       = sxpar(*state.d.stdhdr,'OBJECT')
     xunits    = 'Angstroms'
     xtitle    = sxpar(*state.d.stdhdr,'XTITLE',COMMENT=cxtitle,COUNT=count)

;;  For backwards compatibility

     if count eq 0 then begin

         xtitle  = '!7k!5 ('+strtrim(xunits,2)+')'
         cxtitle = 'IDL X Title'

     endif

;;  Create hdr for the telluric correction spectrum
     fxhmake,hdr,*state.d.tellspec
     fxaddpar,hdr,'IRAFNAME',strtrim(obj+'_tellspec.fits',2)
     fxaddpar,hdr,'ORDERS',orders,'Range of order numbers'
     fxaddpar,hdr,'NORDERS',norders,'Number of orders'
     fxaddpar,hdr,'START',start, 'starting order'
     fxaddpar,hdr,'STOP',stopord, 'stopping order' 
     fxaddpar,hdr,'OBJECT','Telluric Correction Spectrum'
     fxaddpar,hdr,'GRAT',obsmode, 'Observing Mode'
     fxaddpar,hdr,'NAPS',1, ' Number of apertures'
     fxaddpar,hdr,'AIRMASS',state.r.stdairmass, 'Average airmass'
     fxaddpar,hdr,'DAIRMASS',state.d.dairmass, $
              ' Average airmass difference between std/obj.'

     fxaddpar,hdr,'XUNITS',xunits,'X units'
     fxaddpar,hdr,'XTITLE',xtitle,cxtitle
     fxaddpar,hdr,'YUNITS',strcompress(state.r.units,/RE)+'/DNs-1', $
              ' Units of the Y axis'
     fxaddpar,hdr,'YTITLE',state.r.nytitle+' / DN s!U-1!N)','IDL Y title'

; hack by AJB to put in order shifts
     for i=start,stopord do fxaddpar,hdr,'SHIFT'+strtrim(string(i),2),(*state.r.shift)[i-start]

     history = 'These telluric correction spectra were constructed from the '+$
               'spectra '+strtrim(stdfile,2)+'.  The B and V magnitudes of '+$
               strtrim(std,2)+' are '+strtrim(state.r.bmag,2)+', and '+$
               strtrim(state.r.vmag,2)+ $
               ' respectively.  The velocity shift of Vega is '+$
               strtrim(state.r.vshift,2)+' km s-1.'

     if state.r.method eq 'Deconvolution' then begin

         history = history+'  The Vega model was modified using the ' + $
                   'deconvolution method.  The maximum deviation and ' + $
                   'rms deviation between the modified Vega model and the ' + $
                   'data over the kernel wavelength range are '+ $
                   strtrim(state.r.maxdev*100.0,2)+' % and '+ $
                   strtrim(state.r.rmsdev*100.0,2)+' %, respectively.'

     endif else begin

         history = history+' The Vega model was modified using the IP method.'

     endelse

     history = mc_splittext(history,70)
     sxaddhist,history,hdr
  
;;  Write telluric correction spectra to disk
  
     writefits,path+obj+'_tellspec.fits',*state.d.tellspec,hdr

     if state.r.textoutput then begin
      
         npix = fxpar(hdr,'NAXIS1')
         openw,lun,path+obj+'_tellspec.txt', /GET_LUN
      
         for i = 0, n_elements(hdr)-1 do printf, lun, hdr[i]
      
         for i = 0, npix-1 do begin
          
             printf, lun,  strjoin( reform((*state.d.tellspec)[i,*,*],$
                                           3*norders),'  ' )
          
         endfor
         close, lun
         free_lun, lun
      
     endif

     print,'Wrote the telluric correction spectrum to '+strtrim(path+obj,2)+$
       '_tellcor'
  
endif

;  Write the convolved resampled Vega spectrum to disk

if state.r.vegaoutput then begin

;;  Get info from std hdr.
    
;;     orders    = fxpar(*state.d.objhdr,'ORDERS',COMMENT=corders)
;;     norders   = fxpar(*state.d.objhdr,'NORDERS',COMMENT=cnorders)
;;     start     = fxpar(*state.d.stdhdr,'START',COMMENT=cstart)
;;     stop      = fxpar(*state.d.stdhdr,'STOP',COMMENT=cstop)
;;     obsmode   = fxpar(*state.d.stdhdr,'GRAT',COMMENT=cobsmode)
;;     std       = fxpar(*state.d.stdhdr,'OBJECT')
;;     xunits    = fxpar(*state.d.stdhdr,'XUNITS',COMMENT=cxunits)
;;     xtitle    = fxpar(*state.d.stdhdr,'XTITLE',COMMENT=cxtitle)

;;  For backwards compatibility

;;     if count eq 0 then begin

;;         xtitle  = '!7k!5 ('+strtrim(xunits,2)+')'
;;         xctitle = 'IDL X Title'

;;     endif

;;  Create FITS header for the Vega spectrum

;;     delvarx,hdr
;;     fxhmake,hdr,*state.d.vegaspec

;;     fxaddpar,hdr,'IRAFNAME',strtrim(obj+'_A0V.fits',2)
;;     fxaddpar,hdr,'ORDERS',orders,corders
;;     fxaddpar,hdr,'NORDERS',norders,cnorders
;;     fxaddpar,hdr,'START',start,cstart
;;     fxaddpar,hdr,'STOP',stop,cstop
;;     fxaddpar,hdr,'OBJECT','Convolved Vega Spectrum'
;;     fxaddpar,hdr,'GRAT',obsmode,cobsmode 
;;     fxaddpar,hdr,'NAPS',1, ' Number of apertures'

;;     fxaddpar,hdr,'XUNITS',xunits,cxunits
;;     fxaddpar,hdr,'XTITLE',xtitle,cxtitle
;;     fxaddpar,hdr,'YUNITS',strcompress(state.r.units,/RE), $
;;              'Units of the Y axis'
;;     fxaddpar,hdr,'YTITLE',state.r.nytitle+')','IDL Y title'

;;     history = 'This Vega spectrum has been scaled to a V magnitude of '+$
;;       strtrim(state.r.vmag,2)+', shifted by '+strtrim(state.r.vshift,2)+$
;;       ' km s-1, convolved with the kernel and resampled '+$
;;       'onto the wavelength grid of '+strtrim(stdfile,2)+'.'

;;     if state.r.method eq 'Deconvolution' then begin

;;         history = history+'  The Vega model was modified using the ' + $
;;                   'deconvolution method.  The maximum deviation and ' + $
;;                   'rms deviation between the modified Vega model and the ' + $
;;                   'data over the kernel wavelength range are '+ $
;;                   strtrim(state.r.maxdev*100.0,2)+' % and '+ $
;;                   strtrim(state.r.rmsdev*100.0,2)+' %, respectively.'

;;     endif else begin

;;         history = history+' The Vega model was modified using the IP method.'

;;     endelse

;;     history = mc_splittext(history,70)
;;     sxaddhist,history,hdr

;;     writefits,path+obj+'_A0V.fits',*state.d.vegaspec,hdr

;;     if state.r.textoutput then begin
        
;;         npix = fxpar(hdr,'NAXIS1')
;;         openw,lun,path+obj+'_A0V.txt', /GET_LUN
        
;;         for i = 0, n_elements(hdr)-1 do printf, lun, hdr[i]
        
;;         for i = 0, npix-1 do begin
            
;;             printf, lun,  strjoin( reform((*state.d.vegaspec)[i,*,*],$
;;                                           3*norders),'  ' )
            
;;         endfor
;;         close, lun
;;         free_lun, lun
        
;;     endif
;;     print,'Wrote the Vega spectrum to '+strtrim(path+obj,2)+'_vega'
    
endif

;  Write the telluric corrected object spectrum to disk.
;  First telluric correct the object spectra

corspec  = *state.d.objspec
for i = 0, n_elements(*state.d.objorders)-1 do begin

    z = where((*state.d.stdorders) eq (*state.d.objorders)[i],count)
    if count eq 0 then goto, cont
    for j = 0, state.d.objnaps-1 do begin

        k = i*state.d.objnaps+j

;       Interpolate telluric spectrum onto object wavelength sampling.
;       jjb hack to count backwards

        interpspec,(*state.d.tellspec)[*,0,i],(*state.d.tellspec)[*,1,(i)],$
          (*state.d.objspec)[*,0,i],nflux,nerror,$
          YAERROR=(*state.d.tellspec)[*,2,i]

;       Now add the telluric spectrum velocity tweaks

        x = findgen(n_elements((*state.d.objspec)[*,0,i]))
        interpspec,x+(*state.r.shift)[i,j],nflux,x,nnflux,nnerror, $
                   YAERROR=nerror

        corspec[*,1,k] = nnflux * (*state.d.objspec)[*,1,k]

        corspec[*,2,k] = sqrt( nnflux^2 * (*state.d.objspec)[*,2,k]^2 + $
                               (*state.d.objspec)[*,1,k]^2 * nnerror^2 )

;        corspec[*,2,k] = sqrt( nnflux^2 * (*state.d.objspec)[*,2,k]^2 + $
;                               corspec[*,1,k]^2 * nnerror^2 )


        obj_strct[i].wave = corspec[*,0,k]*10000. ;back to angstroms
        obj_strct[i].flux = corspec[*,1,k]
        obj_strct[i].sig = corspec[*,2,k]   ; I think this is what I want
        obj_strct[i].sky = obj_strct[i].sky*nnflux
        obj_strct[i].nosig = sqrt(obj_strct[i].novar)*nnflux

     ;; mask bad points
    ;;  ibd=WHERE(corspec[*,2,k] LE 0.0,nbd)
;;      IF nbd GT 0 THEN BEGIN
;;         STOP
;;         obj_strct[i].flux[ibd]=0.0
;;         obj_strct[i].sky[ibd]=0.0
;;         obj_strct[i].sig[ibd]=0.0
;;         obj_strct[i].nosig[ibd]=0.0
;;      ENDIF       


 
     
    endfor

    cont:

endfor

;;  Now write it to disk

;; hdr = *state.d.objhdr
;; sxdelpar,hdr,['DIVISOR','BEAM','GRSTCNT','CALMIR','DIT','OSF','QTH_LAMP',$
;;               'INC_LAMP','IR_SRC','ARG_SRC','SHUTTER','AIMAGE','SKY']

;; fxaddpar,hdr,'IRAFNAME',strtrim(obj+'.fits',2)
;; fxaddpar,hdr,'YUNITS',strcompress(state.r.units,/RE), 'Units of the Y axis'
;; fxaddpar,hdr,'YTITLE',state.r.nytitle+')',' IDL Y title'
;; fxaddpar,hdr,'DAIRMASS',state.d.dairmass, $
;;          ' Average airmass difference between std and obj'

;; sxaddhist,' ',hdr
;; sxaddhist,'fire_xtellcor History',hdr
;; sxaddhist,' ',hdr

;; history = 'This spectrum was telluric corrected using the telluric '+$
;;           'correction spectra '+strtrim(obj,2)+'_tellcor.fits.  ' + $
;;           'The B and V magnitudes of '+strtrim(std,2)+' are '+ $
;;           strtrim(state.r.bmag,2)+', and '+strtrim(state.r.vmag,2)+ $
;;           ' respectively.  The velocity shift of Vega is '+$
;;           strtrim(state.r.vshift,2)+' km s-1.'

;; if state.r.method eq 'Deconvolution' then begin
    
;;     history = history+'  The Vega model was modified using the ' + $
;;               'deconvolution method.  The maximum deviation and ' + $
;;               'rms deviation between the modified Vega model and the ' + $
;;               'data over the kernel wavelength range are '+ $
;;               strtrim(state.r.maxdev*100.0,2)+' % and '+ $
;;               strtrim(state.r.rmsdev*100.0,2)+' %, respectively.'
    
;; endif else begin
    
;;     history = history+' The Vega model was modified using the IP method.'
    
;; endelse

;; for i = 0, state.d.objnaps-1 do begin
    
;;     history = history+'  The telluric correction spectra for aperture '+ $
;;               string(i+1,FORMAT='(i2.2)')+' were shifted by '+ $
;;               strjoin(strtrim((*state.r.shift)[*,i],2),', ')+' pixels.'
    
;; endfor

;; history = mc_splittext(history,70)
;; sxaddhist,history,hdr

writefits, path+obj+'.fits',corspec,hdr

; We lost the header here...put it back in!

hdr = *(state.d.objhdr)
sxaddhist, '------------------------', hdr
sxaddhist, history, hdr
sxaddhist, '------------------------', hdr

mwrfits,obj_strct, path+xidlobj, hdr, /create ;write it back out

;xvspec,path+obj+'.fits'

if state.r.textoutput then begin

;;     norders = fxpar(hdr,'NORDERS')    
;;     npix = fxpar(hdr,'NAXIS1')
;;     openw,lun,path+obj+'.txt', /GET_LUN
    
;;     for i = 0, n_elements(hdr)-1 do printf, lun, hdr[i]
    
;;     for i = 0, npix-1 do begin
        
;;         printf, lun,  strjoin( reform(corspec[i,*,*],3*norders),'  ' )
        
;;     endfor
;;     close, lun
;;     free_lun, lun
    
endif    

;print,'Wrote the corrected spectrum to '+strtrim(path+obj,2)

end
;
;******************************************************************************
;
; ------------------------------Main Program---------------------------------- 
;
;******************************************************************************
;
pro fire_xtellcor, object=object, userpath=userpath, telluric=telluric, $
              bmag=bmag, vmag=vmag, groupleader=groupleader, outfile=outfile, $
              name=name, rawpath=rawpath

getosinfo,dirsep,strsep

;  Startup

;last  = strpos(!path,'Spextool')
;first = strpos(!path,strsep,last,/REVERSE_SEARCH)
;packagepath = strmid(!path,first+1,last-first+8)
packagepath = '/Users/gagne/Dropbox/IDL/IDL_Library/24-SpeXTool_FIRE/'

; Look to see if the output file already exists, if so then punt.
if (not keyword_set(OUTFILE)) then begin
   filenum = (strsplit(telluric,'_.', /extract))[1]
   outfile = 'Tel_'+strtrim(filenum,2)
endif

if (not keyword_set(RAWPATH)) then begin
   rawpath = "../Raw/"
endif

;  Get atmosphere file

restore, filepath('atrans.sav',ROOT_DIR=packagepath,SUBDIR='data')


;  Get hydrogen lines

readfmt,filepath('HI.dat',ROOT_DIR=packagepath,SUBDIR='data'),$
  'F9.7,4x,A12',hlines,hnames,SKIPLINE=3

;  Get throughputs

;restore, filepath('xputs.sav',ROOT_DIR=packagepath,SUBDIR='data')
xputs = 0

;  Load color table

mkct

;  Set the fonts

getfonts,buttonfont,textfont

;  Build three structures which will hold important info.

w = {bmag_fld:[0,0],$
     box2a_base:0L,$
     dirsep:dirsep,$
     message:0L,$
     objap_dl:0L,$
     objoname_fld:[0L,0L],$
     objorder_dl:0L,$
     objspectra_fld:[0,0],$
     path_fld:[0L,0L],$
     shift:0L,$
     stdspectra_fld:[0L,0L],$
     stdorder_dl:0L,$
     vmag_fld:[0,0],$
     fire_xtellcor_base:0L}

r = {bmag:0.,$
     continue:0L,$
     cutreg:ptr_new(2),$
     hlines:hlines,$
     hnames:hnames,$
     maxdev:0.0,$
     method:'Deconvolution',$
     nytitle:'',$
     packagepath:packagepath,$
     plotobjorder:0,$
     plotobjap:0,$
     rmsdev:0.0,$
     stdorder:0,$
     scale:0.,$
     scales:ptr_new(fltarr(2)),$
     shift:ptr_new(fltarr(2)),$
     spec:'',$
     stdairmass:0.,$
     textoutput:0,$
     vegaoutput:0,$
     telluricoutput:1,$
     units:'ergs s-1 cm-2 A-1',$
     vmag:0.,$
     vrot:0.0,$
     vshift:0.,$
     wline:0.,$
     xtitle:'',$
     ytitle:'', $
     rawpath:rawpath $
    }

d = {airmass:'',$
     dairmass:0.0,$
     awave:awave,$
     atrans:atrans,$
     kernels:ptr_new(fltarr(2)),$
     objhdr:ptr_new(fltarr(2)),$
     objorders:ptr_new(fltarr(2)),$
     objnaps:0,$
     objspec:ptr_new(fltarr(2)),$
     nstd:ptr_new(fltarr(2)),$
     slitw_arc:0.,$
     slitw_pix:0.,$
     stddisp:ptr_new(fltarr(2)),$
     stdfwhm:ptr_new(fltarr(2)),$
     stdhdr:ptr_new(fltarr(2)),$
     stdnorders:0,$
     stdobsmode:'',$
     stdorders:ptr_new(fltarr(2)),$
     stdspec:ptr_new(fltarr(2)),$
     tellspec:ptr_new(fltarr(2)),$
     fvega:ptr_new(fltarr(2)),$
     wvega:ptr_new(fltarr(2)),$
     vegaspec:ptr_new(fltarr(2)),$
     cfvega:ptr_new(fltarr(2)),$
     cf2vega:ptr_new(fltarr(2)),$
     xputs:xputs}

;  Load the three structures in the state structure.

state = {w:w,r:r,d:d}

if NOT keyword_set(NAME) then begin
	title = 'fire_xtellcor'
endif else begin
	title = 'fire_xtellcor (' + name + ')'
endelse
state.w.fire_xtellcor_base = widget_base(TITLE=title, $
                                    EVENT_PRO='fire_xtellcor_event',$
                                    /COLUMN)
    
   quit_button = widget_button(state.w.fire_xtellcor_base,$
                               FONT=buttonfont,$
                               VALUE='Done',$
                               UVALUE='Quit')
       
   state.w.message = widget_text(state.w.fire_xtellcor_base, $
                                 VALUE='',$
                                 YSIZE=1)

   row_base = widget_base(state.w.fire_xtellcor_base,$
                          /ROW)

      col1_base = widget_base(row_base,$
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
                                     VALUE=userpath, $
                                     UVALUE='Path Field',$
                                     XSIZE=25,$
                                     /CR_ONLY,$
                                     TEXTID=textid)
               state.w.path_fld = [field,textid]    
               
            row = widget_base(box1_base,$
                              /ROW,$
                              /BASE_ALIGN_CENTER)
            
               std_but = widget_button(row,$
                                       FONT=buttonfont,$
                                       VALUE='Std Spectra',$
                                       UVALUE='Standard Spectra Button')
               
               std_fld = coyote_field2(row,$
                                       LABELFONT=buttonfont,$
                                       FIELDFONT=textfont,$
                                       TITLE=':',$
                                       UVALUE='Standard Spectra Field',$
                                       XSIZE=18,$
                                       VALUE=telluric,$
                                       /CR_ONLY,$
                                       TEXTID=textid)
               state.w.stdspectra_fld = [std_fld,textid]
               
            row = widget_base(box1_base,$
                              /ROW,$
                              /BASE_ALIGN_CENTER)
            
               mag = coyote_field2(row,$
                                   LABELFONT=buttonfont,$
                                   fieldfont=textfont,$
                                   TITLE='Std Mag (B,V):',$
                                   UVALUE='B Magnitude Field',$
                                   XSIZE=6,$
                                   VALUE=string(bmag),$
                                   /CR_ONLY,$
                                   TEXTID=textid)
               state.w.bmag_fld = [mag,textid]
               
               mag = coyote_field2(row,$
                                   LABELFONT=buttonfont,$
                                   fieldfont=textfont,$
                                   UVALUE='V Magnitude Field',$
                                   XSIZE=6,$
                                   VALUE=string(vmag),$
                                   TITLE='',$
                                   /CR_ONLY,$
                                   TEXTID=textid)
               state.w.vmag_fld = [mag,textid]
               
            row = widget_base(box1_base,$
                              /row,$
                              /base_align_center)
            
               obj_but = widget_button(row,$
                                       font=buttonfont,$
                                       VALUE='Obj Spectra',$
                                       UVALUE='Object Spectra Button',$
                                       EVENT_PRO='fire_xtellcor_event')
               
               obj_fld = coyote_field2(row,$
                                       LABELFONT=buttonfont,$
                                       fieldfont=textfont,$
                                       TITLE=':',$
                                       UVALUE='Object Spectra Field',$
                                       VALUE=object,$
                                       XSIZE=18,$
                                       /CR_ONLY,$
                                       TEXTID=textid)
               state.w.objspectra_fld = [obj_fld,textid]
;               widget_control, obj_fld, uvalue=object
                     
            load = widget_button(box1_base,$
                                 VALUE='Re-load Spectra',$
                                 FONT=buttonfont,$
                                 UVALUE='Load Spectra')

         box2_base = widget_base(col1_base,$
                                 /COLUMN,$
                                 /FRAME)
            
            label = widget_label(box2_base,$
                                 VALUE='2.  Construct Convolution Kernel',$
                                 FONT=buttonfont,$
                                 /ALIGN_LEFT)

            method_bg = cw_bgroup(box2_base,$
                                  font=buttonfont,$
                                  ['Deconvolution','IP'],$
                                  /ROW,$
                                  /RETURN_NAME,$
                                  /NO_RELEASE,$
                                  /EXCLUSIVE,$
                                  LABEL_LEFT='Method:',$
                                  UVALUE='Method',$
                                  SET_VALUE=0)

            row = widget_base(box2_base)
            
               state.w.box2a_base = widget_base(row,$
                                                /ROW,$
                                                /BASE_ALIGN_CENTER,$
                                                MAP=1)

                  state.w.stdorder_dl = widget_droplist(state.w.box2a_base,$
                                                        FONT=buttonfont,$
                                                        TITLE='Order:',$
                                                        VALUE='26',$
                                                       UVALUE='Standard Order')
                  widget_control, state.w.box2a_base,MAP=1

            button = widget_button(box2_base,$
                                   VALUE='Construct Kernel',$
                                   UVALUE='Construct Kernel',$
                                   FONT=buttonfont)
            
      col2_base = widget_base(row_base,$
                              EVENT_PRO='fire_xtellcor_event',$
                              /COLUMN)


         box3_base = widget_base(col2_base,$
                                 /COLUMN,$
                                 /FRAME)
            
            label = widget_label(box3_base,$
                                 VALUE='3.  Construct Telluric Spectra',$
                                 FONT=buttonfont,$
                                 /ALIGN_LEFT)

            getshifts = widget_button(box3_base,$
                                      VALUE='Scale Lines',$
                                      UVALUE='Scale Lines',$
                                      FONT=buttonfont)

            value =['ergs s-1 cm-2 A-1','ergs s-1 cm-2 Hz-1',$
                    'W m-2 um-1','W m-2 Hz-1','Jy']
            units_dl = widget_droplist(box3_base,$
                                       font=buttonfont,$
                                       title='Units:',$
                                       value=value,$
                                       uvalue='Spectrum Units')

            constructspec = widget_button(box3_base,$
                                          VALUE='Construct Telluric Spectra',$
                                          UVALUE='Construct Telluric Spectra',$
                                          FONT=buttonfont)

         box4_base = widget_base(col2_base,$
                                 /COLUMN,$
                                 /FRAME)

            label = widget_label(box4_base,$
                                 VALUE='4.  Determine Shift',$
                                 FONT=buttonfont,$
                                 /ALIGN_LEFT)

            row = widget_base(box4_base,$
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

            shift = widget_button(box4_base,$
                                  VALUE='Get Shift',$
                                  UVALUE='Get Shift',$
                                  FONT=buttonfont)

            row = widget_base(box4_base,$
                              /ROW,$
                              /BASE_ALIGN_CENTER)
            
               state.w.shift = widget_label(row,$
                                            /ALIGN_LEFT,$
                                            /DYNAMIC_RESIZE,$
                                            FONT=buttonfont,$
                                            VALUE='Shift: 0.0')
            
              shift_all = widget_button(row,$
                                  VALUE='Apply to All Orders',$
                                  UVALUE='Shift All',$
                                  FONT=buttonfont)

          box5_base = widget_base(col2_base,$
                                  /COLUMN,$
                                  /FRAME)

             label = widget_label(box5_base,$
                                  VALUE='5.  Write File',$
                                  FONT=buttonfont,$
                                  /ALIGN_LEFT)
             
             outformat_bg = cw_bgroup(box5_base,$
                                      FONT=buttonfont,$
                                      ['Text','FITS','Obj Struct'],$
                                      /ROW,$
                                      /RETURN_NAME,$
                                      /NONEXCLUSIVE,$
                                      LABEL_LEFT='',$
                                      UVALUE='Output Format',$
                                      SET_VALUE=[0,0,1])

            oname = coyote_field2(box5_base,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='Object File:',$
                                  VALUE=outfile, $
                                  UVALUE='Object File Oname',$
                                  xsize=18,$
                                  textID=textid)
            state.w.objoname_fld = [oname,textid]
            

            addoutput_bg = cw_bgroup(box5_base,$
                                     FONT=buttonfont,$
                                     ['Telluric','A0 V'],$
                                     /ROW,$
                                     /RETURN_NAME,$
                                     /NONEXCLUSIVE,$
                                     LABEL_LEFT='Additional:',$
                                     UVALUE='Additional Output',$
                                     SET_VALUE=[1,0])

            write = widget_button(box5_base,$
                                  VALUE='Write File',$
                                  UVALUE='Write File',$
                                  FONT=buttonfont)

   help = widget_button(state.w.fire_xtellcor_base,$
                        VALUE='Help',$
                        UVALUE='Help',$
                        FONT=buttonfont)
      
; Get things running.  Center the widget using the Fanning routine.
            
centertlb,state.w.fire_xtellcor_base
widget_control, state.w.fire_xtellcor_base, /REALIZE

; Start the Event Loop. This will be a non-blocking program.

;; Load spectra
fire_xtellcor_loadspec,state

tmpstate=state

widget_control, state.w.fire_xtellcor_base, SET_UVALUE=state, /NO_COPY

XManager, 'fire_xtellcor', $
          tmpstate.w.fire_xtellcor_base, $
          CLEANUP='fire_xtellcor_cleanup',$
          NO_BLOCK=1

end
