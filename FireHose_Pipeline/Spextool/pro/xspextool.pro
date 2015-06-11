;+
; NAME:
;     xspextool
;
; PURPOSE:
;     Widget to drive the SpeX data reduction.
;    
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     xspextool,FAB=fab
;    
; INPUTS:
;     None
;   
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     FAB - Set to launch Spextool with the Flat and Arc Base
;     
; OUTPUTS:
;     Writes SpeX spectral FITS images to disk
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
;
;-

;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro xspextool_menuevent,event

  widget_control, event.top, GET_UVALUE = state, /NO_COPY
  widget_control, event.id,  GET_UVALUE = uvalue

  if uvalue eq 'Help' then begin
     
     xspextool_help,state
     goto, cont
     
  endif
  
  widget_control, state.w.flat_base,       MAP = 0
  widget_control, state.w.sky_base,        MAP = 0
  widget_control, state.w.other_base,      MAP = 0
  if not state.w.general then widget_control, state.w.eng_base, MAP = 0
  widget_control, state.w.path_base,       MAP = 0
  widget_control, state.w.ps_base,         MAP = 0
  widget_control, state.w.xs_base,         MAP = 0
  if not state.w.general then widget_control, state.w.arc_base, MAP = 0
  widget_control, state.w.imageinput_base, MAP = 0
  widget_control, state.w.combineimgs_base,MAP = 0
  widget_control, state.w.doall_base,      MAP = 0
  widget_control, state.w.cals_base,       MAP = 0
  
  widget_control, state.w.outprefix_fld[0], SENSITIVE=0        
  widget_control, state.w.inprefix_fld[0],  SENSITIVE=0        
  widget_control, state.w.outname_fld[0],   SENSITIVE=0        
  widget_control, state.w.outformat_bg,     SENSITIVE=0        
  
  val = cfld(state.w.inprefix_fld,7,CANCEL=cancel)
  if cancel then return
  if val ne 'flat' and val ne 'arc' then state.r.prefix = strtrim(val,2)
  
  case uvalue of 
            
     'Arc': begin
        
        widget_control, state.w.arc_base, /MAP
        if state.r.filereadmode eq 'Index' then $
           widget_control, state.w.inprefix_fld[0], /SENSITIVE
        
     end
     
     'Cals': begin
        
        widget_control, state.w.cals_base, /MAP
        widget_control, state.w.inprefix_fld[0],SENSITIVE=1
        
     end
     'Combine Images': begin
        
        widget_control, state.w.combineimgs_base, /MAP
        if state.r.filereadmode eq 'Index' then $
           widget_control, state.w.inprefix_fld[0], /SENSITIVE
        widget_control, state.w.inprefix_fld[1], $
                        SET_VALUE=state.r.prefix
        
     end
     
     'Eng.': widget_control, state.w.eng_base, /MAP
     
     'Extended Source': begin
        
        state.w.base = 'Extended Source'
        widget_control, state.w.xs_base,         /MAP
        widget_control, state.w.imageinput_base, /MAP
        widget_control, state.w.doall_base,    /MAP
        widget_control, state.w.outformat_bg,    /SENSITIVE
        if state.r.filereadmode eq 'Index' then begin
           
           widget_control, state.w.outprefix_fld[0], /SENSITIVE
           widget_control, state.w.inprefix_fld[0],  /SENSITIVE
           widget_control, state.w.inprefix_fld[1], $
                           SET_VALUE=state.r.prefix
           
        endif
        if state.r.filereadmode eq 'Filename' then $
           widget_control, state.w.outname_fld[0], /SENSITIVE
        
     end
     'Flat': begin
        
        widget_control, state.w.flat_base, /MAP
        if state.r.filereadmode eq 'Index' then $
           widget_control, state.w.inprefix_fld[0], /SENSITIVE
        
     end
     
     'Other': widget_control, state.w.other_base, /MAP
     
     'Paths': begin
        
        widget_control, state.w.path_base, /MAP
        if state.r.filereadmode eq 'Index' then $
           widget_control, state.w.inprefix_fld[0], /SENSITIVE
        
     end
     'Point Source': begin
        
        state.w.base = 'Point Source'
        widget_control, state.w.ps_base,         /MAP
        widget_control, state.w.imageinput_base, /MAP
        widget_control, state.w.doall_base,    /MAP
        widget_control, state.w.outformat_bg,    /SENSITIVE
        if state.r.filereadmode eq 'Index' then begin
           
           widget_control, state.w.outprefix_fld[0], /SENSITIVE
           widget_control, state.w.inprefix_fld[0], /SENSITIVE
           widget_control, state.w.inprefix_fld[1], $
                           SET_VALUE=state.r.prefix
           
        endif
        if state.r.filereadmode eq 'Filename' then $
           widget_control, state.w.outname_fld[0], /SENSITIVE
        
     end
     
     'Sky': begin
        
        widget_control, state.w.sky_base, /MAP
        if state.r.filereadmode eq 'Index' then $
           widget_control, state.w.inprefix_fld[0], /SENSITIVE
        
     end
     
  endcase
  
cont:
  
;  Put state variable into the user value of the top level base.
  
widget_control, state.w.xspextool_base, SET_UVALUE=state, /NO_COPY

end
;
;******************************************************************************
;
pro xspextool_event, event

widget_control, event.id,  GET_UVALUE = uvalue
if uvalue eq 'Quit' then begin

    widget_control, event.top, /DESTROY
    goto, getout

endif

widget_control, event.top, GET_UVALUE = state, /NO_COPY
widget_control, /HOURGLASS

case uvalue of
  
    'Auto X-Correlate': state.r.autoxcorr = event.select

    '2D Extraction': begin

        state.r.e2d = event.select
        set = (state.r.e2d eq 1) ? 0:1

        state.r.optextract = set
        state.r.psbgsub    = 'On'

        widget_control, state.w.optext_bg, SET_VALUE=set
        widget_control, state.w.optext_bg,SENSITIVE=set
        widget_control, state.w.psbgsub_bg,SENSITIVE=event.select

        widget_control, state.w.psfradius_fld[0],SENSITIVE=set

    end

    'Arc Combination Statistic': state.r.arccombinestat = event.value

    'Arc Flat Field Button': begin

        path =dialog_pickfile(DIALOG_PARENT=state.w.xspextool_base,$
                              PATH=state.r.calpath,/MUST_EXIST,$
                              FILTER='*.fits',/FIX_FILTER)

        if path ne '' then begin

            widget_control,state.w.arcflat_fld[1],$
              set_value = strmid(path,strpos(path,state.w.sep,/REVERSE_S)+1)
            setfocus, state.w.arconame_fld
            
        endif

    end
    
    'Arc Reduction Mode': begin

        state.r.arcreductionmode = event.value
        setfocus, state.w.arcimages_fld
        if event.value eq 'ShortXD' then begin

            widget_control, state.w.arcskyrow, SENSITIVE=0

        endif else begin

            widget_control, state.w.arcskyrow, /SENSITIVE
            
        endelse

    end

    'Arc Sky Button': begin

        path =dialog_pickfile(DIALOG_PARENT=state.w.xspextool_base,$
                              PATH=state.r.datapath,/MUST_EXIST,$
                              /MULTIPLE_FILES,FILTER='*.fits',/FIX_FILTER)

        case (size(path))[1] of 

            1: begin

                if path[0] ne '' then begin
                    
                    widget_control,state.w.arcsky_fld[1],$
                      set_value = strmid(path[0],strpos(path,state.w.sep,$
                                                        /REVERSE_S)+1)
                    setfocus, state.w.arcflat_fld
                    
                endif

            end
            2: begin

                tmp1 = strmid(path[0],strpos(path[0],state.w.sep,/REVERSE_S)+1)
                tmp2 = strmid(path[1],strpos(path[1],state.w.sep,/REVERSE_S)+1)
                widget_control,state.w.arcsky_fld[1],$
                  set_value = strjoin([tmp1,tmp2],',',/SINGLE)
                setfocus, state.w.arcflat_fld

            end

            else: begin

                mess = 'Only two images please for the time being.'
                ok = dialog_message(mess,/ERROR,$
                                    DIALOG_PARENT=state.w.xspextool_base)

            end

        endcase
    end

;    'Buttons': begin
;     
;        if event.value eq 'Help' then begin
;
;            xspextool_help,state
;            goto, cont
;
;        endif
;
;        widget_control, state.w.flat_base,       MAP = 0
;        widget_control, state.w.sky_base,        MAP = 0
;        widget_control, state.w.other_base,      MAP = 0
;        if not state.w.general then widget_control, state.w.eng_base, MAP = 0
;        widget_control, state.w.path_base,       MAP = 0
;        widget_control, state.w.ps_base,         MAP = 0
;        widget_control, state.w.xs_base,         MAP = 0
;        if not state.w.general then widget_control, state.w.arc_base, MAP = 0
;        widget_control, state.w.imageinput_base, MAP = 0
;        widget_control, state.w.combineimgs_base,MAP = 0
;        widget_control, state.w.doall_base,      MAP = 0
;        widget_control, state.w.cals_base,       MAP = 0
;
;        widget_control, state.w.outprefix_fld[0], SENSITIVE=0        
;        widget_control, state.w.inprefix_fld[0],  SENSITIVE=0        
;        widget_control, state.w.outname_fld[0],   SENSITIVE=0        
;        widget_control, state.w.outformat_bg,     SENSITIVE=0        
;
;        val = cfld(state.w.inprefix_fld,7,CANCEL=cancel)
;        if cancel then return
;        if val ne 'flat' and val ne 'arc' then state.r.prefix = strtrim(val,2)
;
;        case event.value of 
;            
;            'Arc': begin
;
;                widget_control, state.w.arc_base, /MAP
;                if state.r.filereadmode eq 'Index' then $
;                  widget_control, state.w.inprefix_fld[0], /SENSITIVE
;
;            end
;
;            'Cals': begin
;
;                widget_control, state.w.cals_base, /MAP
;                widget_control, state.w.inprefix_fld[0],SENSITIVE=1
;
;            end
;            'Combine Images': begin
;
;                widget_control, state.w.combineimgs_base, /MAP
;                if state.r.filereadmode eq 'Index' then $
;                  widget_control, state.w.inprefix_fld[0], /SENSITIVE
;                widget_control, state.w.inprefix_fld[1], $
;                  SET_VALUE=state.r.prefix
;                
;            end
;
;            'Eng.': widget_control, state.w.eng_base, /MAP
;
;            'Extended Source': begin
;                
;                state.w.base = 'Extended Source'
;                widget_control, state.w.xs_base,         /MAP
;                widget_control, state.w.imageinput_base, /MAP
;                widget_control, state.w.doall_base,    /MAP
;                widget_control, state.w.outformat_bg,    /SENSITIVE
;                if state.r.filereadmode eq 'Index' then begin
;
;                    widget_control, state.w.outprefix_fld[0], /SENSITIVE
;                    widget_control, state.w.inprefix_fld[0],  /SENSITIVE
;                    widget_control, state.w.inprefix_fld[1], $
;                      SET_VALUE=state.r.prefix
;
;                endif
;                if state.r.filereadmode eq 'Filename' then $
;                  widget_control, state.w.outname_fld[0], /SENSITIVE
;
;            end
;            'Flat': begin
;
;                widget_control, state.w.flat_base, /MAP
;                if state.r.filereadmode eq 'Index' then $
;                  widget_control, state.w.inprefix_fld[0], /SENSITIVE
;
;            end
;
;            'Other': widget_control, state.w.other_base, /MAP
;
;            'Paths': begin
;                
;                widget_control, state.w.path_base, /MAP
;                if state.r.filereadmode eq 'Index' then $
;                  widget_control, state.w.inprefix_fld[0], /SENSITIVE
;                
;            end
;            'Point Source': begin
;                
;                state.w.base = 'Point Source'
;                widget_control, state.w.ps_base,         /MAP
;                widget_control, state.w.imageinput_base, /MAP
;                widget_control, state.w.doall_base,    /MAP
;                widget_control, state.w.outformat_bg,    /SENSITIVE
;                if state.r.filereadmode eq 'Index' then begin
;
;                  widget_control, state.w.outprefix_fld[0], /SENSITIVE
;                  widget_control, state.w.inprefix_fld[0], /SENSITIVE
;                  widget_control, state.w.inprefix_fld[1], $
;                    SET_VALUE=state.r.prefix
;
;                endif
;                if state.r.filereadmode eq 'Filename' then $
;                  widget_control, state.w.outname_fld[0], /SENSITIVE
;
;            end
;
;            'Sky': begin
;                
;                widget_control, state.w.sky_base, /MAP
;                if state.r.filereadmode eq 'Index' then $
;                  widget_control, state.w.inprefix_fld[0], /SENSITIVE
;                
;            end
;
;        end
;
;    end

    'Cal Combination Statistic': state.r.calcombinestat = event.value

    'Cal Path Button': begin

        path =dialog_pickfile(/DIRECTORY,DIALOG_PARENT=state.w.xspextool_base,$
                             TITLE='Select Path',/MUST_EXIST)

        if path ne '' then begin

            widget_control,state.w.calpath_fld[1],SET_VALUE = path
            setfocus, state.w.procpath_fld
            
        endif

     end

    'Check Seeing':state.r.checkseeing = event.select

    'Clear Data Path': begin

        widget_control, state.w.datapath_fld[1], SET_VALUE=''
        setfocus, state.w.datapath_fld

    end

    'Clear Proc Path': begin

        widget_control, state.w.procpath_fld[1], SET_VALUE=''
        setfocus, state.w.procpath_fld

    end
    'Clear Cal Path': begin

        widget_control, state.w.calpath_fld[1], SET_VALUE=''
        setfocus, state.w.calpath_fld

    end

    'Clear Table': widget_control, state.w.table,$
      SET_VALUE=strarr(2,state.r.tablesize)

    'Combine Apertures': state.r.combineaps = event.value
    
    'Combine BG Subtraction': state.r.combbgsub = event.value

    'Combine Combination Statistic': begin
        
        state.r.combcombinestat = event.value
        setfocus, state.w.comboname_fld

    end

    'Combine Flat Field Button': begin

        path= dialog_pickfile(DIALOG_PARENT=state.w.xspextool_base,$
                              /MUST_EXIST,PATH=state.r.calpath,$
                              FILTER='*.fits',/FIX_FILTER)

        if path ne '' then begin

            widget_control,state.w.combflat_fld[1],$
              SET_VALUE = strmid(path,strpos(path,state.w.sep,/REVERSE_S)+1)
 
        endif

    end

    'Combine Images': xspextool_combine,state,/BEEP

    'Combine Reduction Mode': begin

        state.r.combreductionmode = event.value
        setfocus, state.w.combimages_fld

    end

    'Construct Calibration Frames': xspextool_mkcals,state,/BEEP

    'Construct Flat': xspextool_mkflat,state,/BEEP

    'Construct Super Sky': xspextool_mksky,state,/BEEP

    'Data Path Button': begin

        path= dialog_pickfile(/DIRECTOR,DIALOG_PARENT=state.w.xspextool_base,$
                              TITLE='Select Path',/MUST_EXIST)
        
        if path ne '' then begin

            widget_control,state.w.datapath_fld[1],SET_VALUE = path
            setfocus, state.w.datapath_fld
        
        endif

    end

    'Determine Solution': begin

        state.r.detsol = event.select
        widget_control, state.w.wavebox_base, SENSITI=(event.select eq 1) ? 1:0

    end

    'Do All Steps': xspextool_go, state

    'Error Propagation': state.r.errorprop = event.select

    'Fix Bad Pixels': state.r.fixbdpx = event.select

    'Flat Combination Statistic': state.r.flatcombinestat = event.value

    'Flat Field': state.r.flatfield = event.select

    'Full Wavecal Name Button': begin

        path= dialog_pickfile(DIALOG_PARENT=state.w.xspextool_base,$
                              /MUST_EXIST,PATH=state.r.calpath,$
                              FILTER='*.fits',/FIX_FILTER)

        if path ne '' then begin

            widget_control,state.w.wavecal_fld[1],$
              SET_VALUE = strmid(path,strpos(path,state.w.sep,/REVERSE_S)+1)
            setfocus,state.w.sourceimages_fld
 
        endif

    end

    'Full Flat Name Button': begin

        path= dialog_pickfile(DIALOG_PARENT=state.w.xspextool_base,$
                              /MUST_EXIST,PATH=state.r.calpath,$
                              FILTER='*.fits',/FIX_FILTER)

        if path ne '' then begin

            widget_control,state.w.flatfield_fld[1],$
              SET_VALUE = strmid(path,strpos(path,state.w.sep,/REVERSE_S)+1)
            if state.r.wavecal then setfocus,state.w.wavecal_fld else $
              setfocus,state.w.sourceimages_fld
 
        endif

    end

    'Linearity Correction': state.r.lc = event.select

    'Load Setup Data': xspextool_loadsetupdata,state

    'Load Single Image': xspextool_load1image,state,CANCEL=cancel

    'Make Arcs': xspextool_mkarcs,state,/BEEP
    
    'Make Order Images': xspextool_mkprofiles,state,/BEEP,/DISPLAY

    'Make Spatial Profiles': xspextool_mkprofiles,state,/BEEP

    'Normalize Flat': state.r.normalizeflat = event.value

    'N Apertures': state.r.psnaps = event.index+1

    'Optimal Extraction': begin

        state.r.optextract = event.select
        sensitive = (event.select eq 1) ? 1:0
        widget_control, state.w.psfradius_fld[0],SENSITIVE=sensitive
        sensitive = (event.select eq 1) ? 0:1
        widget_control, state.w.psbgsub_bg,SENSITIVE=sensitive
        if sensitive eq 0 then begin

            state.r.psbgsub = 'On'
            widget_control, state.w.psbgsub_bg, SET_VALUE=0
            widget_control, state.w.psbginfo_base, SENSITIVE=1

        endif

    end
    'Output Format': begin

        if event.value eq 'Text' and event.select then state.r.textoutput=1
        if event.value eq 'Text' and not event.select then state.r.textoutput=0
        if event.value eq 'FITS' and event.select then state.r.fitsoutput=1
        if event.value eq 'FITS' and not event.select then state.r.fitsoutput=0

    end

    'Override Signs': xspextool_overridesigns,state

    'Package Path Button': begin

        path =dialog_pickfile(/DIRECTORY,DIALOG_PARENT=state.w.xspextool_base,$
                              TITLE='Select Path',/MUST_EXIST)

        if path ne '' then begin

            widget_control,state.w.packagepath_fld[1],SET_VALUE = path
            setfocus, state.w.datapath_fld

        endif
        
    end

    'Plot Auto X-Correlate': state.r.plotautoxcorr = event.select

    'Plot Line Finding': state.r.plotlinefind = event.select

    'Plot Profiles': xspextool_updateappos,event,state

    'Plot Residuals':state.r.plotresiduals =event.select

    'Plot Saturated Pixels':state.r.plotsatpix = event.select
    
    'Proc Path Button': begin

        path= dialog_pickfile(/DIRECTORY,DIALOG_PARENT=state.w.xspextool_base,$
                              TITLE='Select Path',/MUST_EXIST)

        if path ne '' then begin

            widget_control,state.w.procpath_fld[1],SET_VALUE = path
            setfocus, state.w.datapath_fld
        
        endif

    end

    'PS BG Subtraction': begin

        state.r.psbgsub = event.value
        if event.value eq 'On' then widget_control, state.w.psbginfo_base,$
          /SENSITIVE
        if event.value eq 'Off' then widget_control, state.w.psbginfo_base,$
          SENSITIVE=0

    end

    'PS Define Apertures': xspextool_psdefineap,state

    'PS Extract': xspextool_psextract,state

    'PS Find Positions': xspextool_psapfind,state

    'PS Manual Find Mode': begin

        state.r.psapfindmanmode = event.value
        sensitive = (event.value eq 'Auto') ? 1:0
        widget_control, state.w.psnaps_dl, SENSITIVE=sensitive

    end

    'PS Object Find Mode': begin

        widget_control,state.w.psapfindman_base, MAP=0
        widget_control,state.w.psapfindtrace_base,  MAP=0
        state.r.psapfindmode = event.value
        if event.value eq 'Import Coeffs' then begin
            
            widget_control, state.w.psapfindtrace_base, /MAP 
            setfocus, state.w.psitracefile_fld
            widget_control, state.w.pscol2_base, SENSITIVE=0
            widget_control, state.w.pstrace_base, SENSITIVE=0
            
        endif else begin
            
            widget_control, state.w.psapfindman_base, /MAP
            widget_control, state.w.pscol2_base, /SENSITIVE
            widget_control, state.w.pstrace_base, /SENSITIVE
            
        endelse

    end
    'PS Orders': begin

        z = where( (*state.r.orders) eq event.value)
        test = (*state.r.psdoorders)
        test[z] = event.select
        z = where(test eq 1,count)
        if count lt 1 then begin
            
            ok = dialog_message('Must select at least one Order.',$
                                /ERROR,DIALOG_PARENT=state.w.xspextool_base)
            widget_control, state.w.psorder_bg, $
              SET_VALUE=reverse(*state.r.psdoorders)
            goto, cont
            
        endif else (*state.r.psdoorders) = test

    end

    'PS Trace Objects': xspextool_pstrace,state,/BEEP

    'Readmode': begin
        
        widget_control, state.w.outprefix_fld[0], SENSITIVE=0
        widget_control, state.w.inprefix_fld[0],  SENSITIVE=0
        widget_control, state.w.outname_fld[0],   SENSITIVE=0
        if event.value eq 'Filename' then begin
            
            state.r.filereadmode = event.value
            if state.w.base eq 'Point Source' or $
              state.w.base eq 'Extended Source' then $
              widget_control, state.w.outname_fld[0],   SENSITIVE=1
            setfocus,state.w.outname_fld
            
        endif else begin
            
            state.r.filereadmode = event.value
            if state.w.base eq 'Point Source' or $
              state.w.base eq 'Extended Source' then begin

                widget_control, state.w.outprefix_fld[0], SENSITIVE=1
                widget_control, state.w.inprefix_fld[0], SENSITIVE=1
                setfocus,state.w.inprefix_fld

            endif else begin

                widget_control, state.w.inprefix_fld[0], SENSITIVE=1
                setfocus,state.w.inprefix_fld
                
            endelse

        endelse
        
    end
    'Reduction Mode': begin
        
        state.r.reductionmode = event.value
        setfocus, state.w.sourceimages_fld
        if event.value eq 'A-B' or event.value eq 'A' then $
          widget_control,state.w.skyimage_base, SENSITIVE=0
        if event.value eq 'A-Sky' then widget_control,$
          state.w.skyimage_base, /SENSITIVE
        
    end

    'Shift Images': begin

        state.r.shiftimages = strtrim(event.value,2)

        if state.r.shiftimages eq 'Yes' then widget_control, $
          state.w.combshift_base, SENSITIVE=1

        if state.r.shiftimages eq 'No' then widget_control, $
          state.w.combshift_base, SENSITIVE=0


    end

    'Sky Combination Statistic': begin
        
        state.r.skycombinestat = event.value
        setfocus, state.w.skyoname_fld

    end

    'Sky Flat Field Button': begin

        path= dialog_pickfile(DIALOG_PARENT=state.w.xspextool_base,$
                              /MUST_EXIST,PATH=state.r.calpath,$
                              FILTER='*.fits',/FIX_FILTER)

        if path ne '' then begin

            widget_control,state.w.skyflat_fld[1],$
              SET_VALUE = strmid(path,strpos(path,state.w.sep,/REVERSE_S)+1)
 
        endif

    end

    'Sky Image Button': begin

        path= dialog_pickfile(DIALOG_PARENT=state.w.xspextool_base,$
                              /MUST_EXIST,PATH=state.r.calpath,$
                              FILTER='*.fits',/FIX_FILTER)

        if path ne '' then begin

            widget_control,state.w.skyimage_fld[1],$
              SET_VALUE = strmid(path,strpos(path,state.w.sep,/REVERSE_S)+1)
              setfocus,state.w.flatfield_fld

        endif

    end

    'Smooth Spectra': state.r.smooth = event.value

    'Source Images Button': begin

        path= dialog_pickfile(DIALOG_PARENT=state.w.xspextool_base,$
                             /MUST_EXIST,PATH=state.r.datapath,$
                             FILTER=['*.fits','*gz'],/FIX_FILTER)

        if path ne '' then begin

            widget_control,state.w.sourceimages_fld[1],$
              SET_VALUE = strmid(path,strpos(path,state.w.sep,/REVERSE_S)+1)
            if state.r.reductionmode  eq 'A-B' then $
              setfocus,state.w.flatfield_fld
            if state.r.reductionmode  eq 'A' then $
              setfocus,state.w.flatfield_fld
            if state.r.reductionmode eq 'A-Sky' then $
              setfocus,state.w.skyimage_fld

        endif

    end

    'Spectra Combine Mode': begin

        widget_control, state.w.combbox1_base,map=0
        widget_control, state.w.combbox2_base,map=0
        state.r.speccombmode = event.value
        if event.value eq 'File' then widget_control, state.w.combbox2_base,$
          /MAP
        if event.value eq 'Manual' then widget_control, state.w.combbox1_base,$
          /MAP

    end

    'Spectral Combination Statistic': state.r.speccombinestat = event.value

    'Spectra Files Button': begin

        fullpath = dialog_pickfile(DIALOG_PARENT=state.w.xspextool_base,$
                                   PATH=state.r.procpath,/MUST_EXIST,$
                                   FILTER='*.fits',/MULTIPLE_FILES)
        
        case (size(fullpath))[1] of 

            1: begin

                if fullpath[0] ne '' then begin
                    
                    widget_control,state.w.combspecfiles_fld[1],$
                      set_value = strmid(fullpath[0],$
                                         strpos(fullpath,state.w.sep,$
                                                /REVERSE_S)+1)
                    
                endif

            end

            else: begin

                for i =0,(size(fullpath))[1]-1 do begin
 
                    tmp = strmid(fullpath[i],strpos(fullpath[i],state.w.sep,$
                                                    /REVERSE_S)+1)
                    arr = (i eq 0) ? tmp:[arr,tmp]

                endfor
                widget_control,state.w.combspecfiles_fld[1],$
                  set_value = strjoin(arr,',',/SINGLE)

            end

        endcase

    end

    'Spectra List File Name': begin

        path =dialog_pickfile(DIALOG_PARENT=state.w.xspextool_base,$
                              PATH=state.r.calpath,$
                              /MUST_EXIST)

        if path ne '' then begin

            widget_control,state.w.speclist_fld[1],$
              SET_VALUE = strmid(path[0],strpos(path,state.w.sep,/REVERSE_S)+1)

        endif

    end

;    'Sub Scattered Light': state.r.subscatlgt = event.select

    'Wavelength Calibration': begin

        state.r.wavecal = event.value
        if event.value eq 1 then widget_control,state.w.arcimage_base,$
          /SENSITIVE
        if event.value eq 0 then widget_control,state.w.arcimage_base,$
          SENSITIVE=0

    end
    'Write Trace': xspextool_writetrace,state

    'Ximgtool': state.r.ximgtool = event.value

    'XS BG Subtraction': begin

        state.r.xsbgsub = event.value
        if event.value eq 'On' then widget_control, state.w.xsbginfo_base,$
          /SENSITIVE
        if event.value eq 'Off' then widget_control, state.w.xsbginfo_base,$
          SENSITIVE=0

    end

    'Update Paths': begin

        xspextool_message,state,'Updating paths...'

;  Get other paths

        path = cfld(state.w.datapath_fld,7,CANCEL=cancel)
        path = cpath(path,CANCEL=cancel)
        if cancel then begin

            ok = dialog_message('Data path does not exist.',/ERROR,$
                                DIALOG_PARENT=event.id)
            setfocus, state.w.datapath_fld
            goto, cont

        endif
        state.r.datapath = path

        path = cfld(state.w.calpath_fld,7,CANCEL=cancel)
        path = cpath(path,CANCEL=cancel)
        if cancel then begin
            
            ok = dialog_message('Cal path does not exist.',/ERROR,$
                                DIALOG_PARENT=event.id)
            setfocus, state.w.calpath_fld
            goto, cont

        endif
        state.r.calpath = path

        path = cfld(state.w.procpath_fld,7,CANCEL=cancel)
        path = cpath(path,CANCEL=cancel)
        if cancel then begin
            
            ok = dialog_message('Proc path does not exist.',/ERROR,$
                                DIALOG_PARENT=event.id)
            setfocus,state.w.procpath_fld
            goto, cont
            
        endif
        state.r.procpath = path

        case !version.os_family of 
            
            'unix': home = getenv('HOME')

            'MacOS': home = filepath('',ROOT_DIR=state.r.packagepath,$
                                     SUBDIR='data')
            
            'Windows': home = filepath('',ROOT_DIR=state.r.packagepath,$
                                       SUBDIR='data')

        endcase

        openw, lun, filepath('.spextool_paths.txt',ROOT_DIR=home),/get_lun
        printf, lun, state.r.datapath
        printf, lun, state.r.calpath
        printf, lun, state.r.procpath
        free_lun, lun

        xspextool_message, state, 'Loading complete.'
        
    end

    'Use Stored Solution': state.r.usestoredwc = event.select

    'XS Define Apertures': xspextool_xsdefineap,state

    'XS Extract': xspextool_xsextract,state

    'XS Find Positions': xspextool_xsapfind,state

    'XS Object Find Mode': begin

        widget_control,state.w.xsapfindman_base, MAP=0
        widget_control,state.w.xsapfindtrace_base, MAP=0
        state.r.xsapfindmode = event.value
        if event.value eq 'Import Coeffs' then begin
            
            widget_control, state.w.xsapfindtrace_base, /MAP 
            setfocus, state.w.xsitracefile_fld
            widget_control, state.w.xscol2_base, SENSITIVE=0
            widget_control, state.w.xstrace_base, SENSITIVE=0
            
        endif else begin
            
            widget_control, state.w.xsapfindman_base, /MAP
            setfocus, state.w.xsappos_fld
            widget_control, state.w.xscol2_base, /SENSITIVE
            widget_control, state.w.xstrace_base, /SENSITIVE
            
        endelse

    end
    'XS Orders': begin

        z = where( (*state.r.orders) eq event.value)
        test = (*state.r.xsdoorders)
        test[z] = event.select
        z = where(test eq 1,count)
        if count lt 1 then begin
            
            ok = dialog_message('Must select at least one Order.',$
                                /ERROR,DIALOG_PARENT=state.w.xspextool_base)
            widget_control, state.w.xsorder_bg, $
              SET_VALUE=reverse(*state.r.xsdoorders)
            goto, cont
            
        endif else (*state.r.xsdoorders) = test

    end

    'XS Trace Objects': xspextool_xstrace,state

    else:

end
 
;  Put state variable into the user value of the top level base.
 
cont: 
widget_control, state.w.xspextool_base, SET_UVALUE=state, /NO_COPY
getout:
end
;
;******************************************************************************
;
pro xspextool_fields, event

widget_control, event.top, GET_UVALUE = state, /NO_COPY
widget_control, event.id,  GET_UVALUE = uvalue

case uvalue of 

    'Cal Path Field': setfocus, state.w.procpath_fld

    'Data Path Field': setfocus, state.w.calpath_fld

    'Full Arc Name': setfocus, state.w.sourceimages_fld

    'Full Flat Name': 
    
    'Input Prefix': setfocus, state.w.outprefix_fld

    'Proc Path Field': setfocus, state.w.datapath_fld

    'PS Aperture': setfocus,state.w.psbgstart_fld
    
    'PS Bg Start': setfocus,state.w.psbgwidth_fld

    'PS Bg Width': setfocus, state.w.psbgfitorder_fld

    'PS Bg Fit Order': setfocus, state.w.psapradius_fld

    'Super Sky Image': setfocus, state.w.flatfield_fld
    
    'Source Images': begin

        if state.r.reductionmode eq 'A-B'  then setfocus, state.w.flatfield_fld
        if state.r.reductionmode eq 'A '   then setfocus, state.w.flatfield_fld
        if state.r.reductionmode eq 'A-Sky' then setfocus, state.w.skyimage_fld

    end
    'XS Aperture': setfocus,state.w.xsbg_fld

    'XS Bg Regions': setfocus,state.w.xsbgfitorder_fld

    'XS Bg Fit Order': setfocus, state.w.xsapradius_fld

else:

endcase

;  Put state variable into the user value of the top level base.

widget_control, state.w.xspextool_base, SET_UVALUE=state, /NO_COPY

end
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xspextool_cleanup,xspextool_base

widget_control, xspextool_base, GET_UVALUE = state, /NO_COPY

if n_elements(state) ne 0 then begin

    ptr_free, state.r.appos
    ptr_free, state.r.apsign
    ptr_free, state.r.edgecoeffs
    ptr_free, state.r.hdrinfo
    ptr_free, state.r.orders
    ptr_free, state.r.psdoorders
    ptr_free, state.r.rms
    ptr_free, state.r.tracecoeffs
    ptr_free, state.r.workfiles
    ptr_free, state.r.xranges
    ptr_free, state.r.xsdoorders
    ptr_free, state.r.profiles

    ptr_free, state.d.wavecal
    ptr_free, state.d.bdpxmask
    ptr_free, state.d.disp
    ptr_free, state.d.spectra
    ptr_free, state.d.varimage
    ptr_free, state.d.workimage
        
endif
state = 0B

end
;
;******************************************************************************
;
pro xspextool_combine, state, CANCEL=cancel, BEEP=beep

cancel = 0


;  Get file readmode info.

index    = (state.r.filereadmode eq 'Index') ? 1:0
filename = (state.r.filereadmode eq 'Filename') ? 1:0
if index then begin

    prefix = cfld(state.w.inprefix_fld,7,/EMPTY,CANCEL=cancel)
    if cancel then return
    state.r.prefix = strtrim(prefix,2)

endif

;  Get user inputs

images    = cfld(state.w.combimages_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
images    = fsextract(images,INDEX=index,FILENAME=filename,CANCEL=cancel)
if cancel then return
imagepath = mkfullpath(state.r.datapath,images,INDEX=index,FILENAME=filename,$
                       NI=state.r.nint,PREFIX=prefix,SUFFIX='*.fits',$
                       WIDGET_ID=state.w.xspextool_base,/EXIST,CANCEL=cancel)
if cancel then return

oname = cfld(state.w.comboname_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

flatname = cfld(state.w.combflat_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
flatpath = cfile(state.r.calpath+flatname,$
                 WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
if cancel then return

if state.r.lc then restore, $
  filepath('lc_coeff.sav',ROOT_DIR=state.r.packagepath,SUBDIR='data')

readflat,flatpath,flat,ncols,nrows,modename,slith_pix,$
  slith_arc,slitw_pix,slitw_arc,norders,orders,edgecoeffs,xranges,rms,$
  rotation

pair = (state.r.combreductionmode eq 'A') ? 0:1
if pair then imagepath = reorder(imagepath,CANCEL=cancel)
if cancel then return

;  Check to see what combination statistic we are using.  

if state.r.plotsatpix then sat = cfld(state.w.saturation_fld,4,/EMPTY,$
                                      CANCEL=cancel)

if state.r.combcombinestat eq 'Median' then begin

    xspextool_message,state,'Loading the images...'

    readmfits,imagepath,data,hdrinfo,var,KEYWORDS=state.r.keywords,$
      ROTATE=rotation,ITIME=state.r.itime,COADDS=state.r.coadds,$
      NDRS=state.r.ndrs,GAIN=state.r.gain,PAIR=pair,$
      READNOISE=state.r.readnoise,LC_COEFF=lc_coeff,SLOWCNT=state.r.slowcnt,$
      WIDGET_ID=state.w.xspextool_base,SATURATION=sat,MASK=mask,CANCEL=cancel
    if cancel then return
    
;  Subtract sky if requested.
    
    if state.r.combbgsub eq 'On' then begin
        
        subspecsky,data,var,edgecoeffs,norders,3,xranges,/UPDATE,$
          WIDGET_ID=state.w.xspextool_base,CANCEL=cancel
        if cancel then return
        
    endif
    
    xspextool_message,state,'Combining the Images...'
    
    medcomb,data,mean,mvar,/UPDATE,WIDGET_ID=state.w.xspextool_base,$
      CANCEL=cancel

    if state.r.plotsatpix then begin

        mask = total(mask,3)
        z = where(mask gt 1,count)
        if count gt 0 then mask[z] = 1

    endif else mask = fltarr(ncols,nrows)

endif else begin

    if state.r.combbgsub eq 'On' then begin

        skyinfo = {edgecoeffs:edgecoeffs,norders:norders,xranges:xranges}

    endif
    weighted = (state.r.combcombinestat eq 'Weighted Mean') ? 1:0
    readcmfits,imagepath,mean,hdrinfo,mvar,KEYWORDS=state.r.keywords,$
      ROTATE=rotation,ITIME=state.r.itime,COADDS=state.r.coadds,$
      NDRS=state.r.ndrs,GAIN=state.r.gain,PAIR=pair,$
      READNOISE=state.r.readnoise,WEIGHTED=weighted,/UPDATE,SKYINFO=skyinfo,$
      LC_COEFF=lc_coeff,SLOWCNT=state.r.slowcnt,$
      WIDGET_ID=state.w.xspextool_base,SATURATION=sat,MASK=mask,CANCEL=cancel
    
    if state.r.plotsatpix then begin
        
        mask = total(mask,3)
        z = where(mask gt 1,count)
        if count gt 0 then mask[z] = 1
        
    endif else mask = fltarr(ncols,nrows)

endelse
if cancel then return



;  Write the results to disk

junk = tag_exist(hdrinfo[*].vals,state.r.irafname,INDEX=idx)

if state.r.combbgsub eq 'On' then begin

    history='This image was created by subtracting the sky off the images '+$
            strjoin(hdrinfo[*].vals.(idx), ', ')+' and then '+ $
            state.r.combcombinestat+' combining the images.' 
    
endif else begin

    history='This image was created by '+state.r.combcombinestat+$
      ' combining the images, '+strjoin(hdrinfo[*].vals.(idx), ', ')   

endelse

if state.r.lc then history = history+'  The raw data was '+$
  'first corrected for non-linearity.'

newhdr = avehdrs(hdrinfo,TIME_OBS=state.r.time,POSANGLE=state.r.posangle,$
                 HA=state.r.ha,AIRMASS=state.r.airmass,CANCEL=cancel)

newhdr.vals.(idx) = oname+'.fits'

writecomb,unrotate(mean,rotation,CANCEL=cancel), $
          state.r.procpath+oname+'.fits',newhdr,HISTORY=history
if cancel then return

;  Write variance image out.

fxhmake,hdr,mvar
fxaddpar,hdr,'HISTORY','This is the variance image for image '+$
  state.r.procpath+oname+'.fits'

writefits,state.r.procpath+oname+'_var.fits',unrotate(mvar,rotation),hdr

xspextool_message,state,$
  [[['Wrote combined images to ']],[[state.r.procpath+oname+'.fits']]]

xspextool_message,state,'**NOTE:  THIS PROCESS DID NOT FLAT-FIELD THE IMAGE**'

;scale = (state.r.ximgtool eq 0) float(state.r.itime):1.0
ximgtool,mean,BUFFER=1,/AUTORANGE,WID=wid,ZWINPOS='None', $
         GROUP_LEADER=state.w.xspextool_base,MASK=mask

if keyword_set(BEEP) then beep

end
;
;******************************************************************************
;
pro xspextool_go,state

index    = (state.r.filereadmode eq 'Index') ? 1:0
filename = (state.r.filereadmode eq 'Filename') ? 1:0

;  Get user inputs.

files = cfld(state.w.sourceimages_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
files = fsextract(files,INDEX=index,FILENAME=filename,CANCEL=cancel)
if cancel then return

if state.r.reductionmode eq 'A-B' then begin

    if filename then begin

        mess = 'Cannot use A-B mode when filename is selected'
        ok = dialog_message(mess,/ERROR,DIALOG_PARENT=state.w.xspextool_base)
        cancel = 1
        return
        
    endif
    if n_elements(files) mod 2 ne 0 then begin
        
        mess = 'Must be an even number of images for pair subtraction.'
        ok = dialog_message(mess,/ERROR,DIALOG_PARENT=state.w.xspextool_base)
        setfocus,state.w.sourceimages_fld
        cancel = 1
        return
        
    endif

endif

;  Get loop size

loopsize = (state.r.reductionmode eq 'A-B') ? (n_elements(files)/2.):$
                                              (n_elements(files))

for i = 0, loopsize-1 do begin

    subset = (state.r.reductionmode eq 'A-B') ? files[i*2:i*2+1]:files[i]

    *state.r.workfiles = subset
    xspextool_loadimages,state,CANCEL=cancel    
    if cancel then return
    xspextool_mkprofiles,state,CANCEL=cancel
    if cancel then return
    if state.w.base eq 'Point Source' then begin

        xspextool_psapfind,state,CANCEL=cancel
        if cancel then return
        if state.r.psapfindmode eq 'Manual' then xspextool_pstrace,state,$
          CANCEL=cancel
        if cancel then return
        xspextool_psdefineap,state,CANCEL=cancel
        if cancel then return
        state.r.doallsteps = 1
        xspextool_psextract,state,CANCEL=cancel
        state.r.doallsteps = 0
        if cancel then return


    endif
    if state.w.base eq 'Extended Source' then begin

        xspextool_xsapfind,state,CANCEL=cancel
        if cancel then return
        if state.r.xsapfindmode eq 'Manual' then xspextool_xstrace,state,$
          CANCEL=cancel
        xspextool_xsdefineap,state,CANCEL=cancel
        if cancel then return
        state.r.doallsteps = 1
        xspextool_xsextract,state,CANCEL=cancel
        state.r.doallsteps = 0
        if cancel then return        

    endif

endfor

xspextool_message,state,'Do All Steps Complete.'

beep
beep
beep

end
;
;******************************************************************************
;
pro xspextool_help,state

openr, lun, filepath('xspextool_helpfile.txt',ROOT_DIR=state.r.packagepath, $
                     SUBDIR='helpfiles'),/GET_LUN
nlines = numlines(filepath('xspextool_helpfile.txt',$
                           ROOT_DIR=state.r.packagepath,$
                           SUBDIR='helpfiles'))
array = strarr(nlines)
readf, lun, array
free_lun, lun

xmc_displaytext,array,TITLE='XSpextool Help File', $
                GROUP_LEADER=state.w.xspextool_base

end
;
;******************************************************************************
;
pro xspextool_load1image,state,CANCEL=cancel

cancel = 0

;  File Read Mode.

index    = (state.r.filereadmode eq 'Index') ? 1:0
filename = (state.r.filereadmode eq 'Filename') ? 1:0

;  Get user inputs.

files = cfld(state.w.sourceimages_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
files = fsextract(files,INDEX=index,FILENAME=filename,CANCEL=cancel)
if cancel then return

case state.r.reductionmode of 

    'A': begin

        if n_elements(files) ne 1 then begin
        
            mess = 'Can only load 1 image at a time with this button.'
            ok = dialog_message(mess,/error,dialog_pare=state.w.xspextool_base)
            setfocus,state.w.sourceimages_fld
            cancel = 1
            return
            
        endif

     end
     'A-B': begin

         if filename then begin
             
             mess = 'Cannot use A-B mode when filename is selected.'
             ok = dialog_message(mess,/error,dialog_par=state.w.xspextool_base)
             cancel = 1
             return
             
         endif
         if n_elements(files) ne 2 then begin
             
             mess = 'Can only load 2 images at a time with this button.'
             ok = dialog_message(mess,/error,dialog_par=state.w.xspextool_base)
             setfocus,state.w.sourceimages_fld
             cancel = 1
             return
             
         endif

     end

     'A-Sky': begin

         if n_elements(files) ne 1 then begin
             
             mess = 'Can only load 1 image at a time with this button.'
             ok = dialog_message(mess,/error,dialog_par=state.w.xspextool_base)
             setfocus,state.w.sourceimages_fld
             cancel = 1
             return
             
         endif



     end

 endcase

*state.r.workfiles = files
xspextool_loadimages,state,CANCEL=cancel,/BEEP

end
;
;******************************************************************************
;
pro xspextool_loadimages,state,CANCEL=cancel,BEEP=beep

cancel = 0

;  File Read Mode.

index    = (state.r.filereadmode eq 'Index') ? 1:0
filename = (state.r.filereadmode eq 'Filename') ? 1:0
if index then begin

    prefix = cfld(state.w.inprefix_fld,7,/EMPTY,CANCEL=cancel)
    if cancel then return
    state.r.prefix = strtrim(prefix,2)

endif

;  Get user inputs.

fullpaths = mkfullpath(state.r.datapath,*state.r.workfiles,INDEX=index,$
                       FILENAME=filename,NI=state.r.nint,PREFIX=prefix,$
                       SUFFIX='*.fits',WIDGET_ID=state.w.xspextool_base,$
                       /EXIST,CANCEL=cancel)
if cancel then setfocus,state.w.sourceimages_fld
if cancel then return

flatname = cfld(state.w.flatfield_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
flatpath = cfile(state.r.calpath+flatname,WIDGET_ID=state.w.xspextool_base,$
                 CANCEL=cancel)
if cancel then setfocus,state.w.flatfield_fld
if cancel then return

if state.r.reductionmode eq 'A-Sky' then begin

    skyname = cfld(state.w.skyimage_fld,7,/EMPTY,CANCEL=cancel)
    if cancel then return
    skypath = cfile(state.r.calpath+skyname,WIDGET_ID=state.w.xspextool_base,$
                     CANCEL=cancel)
    if cancel then return

endif

if not state.w.general then begin

    arcname = cfld(state.w.wavecal_fld,7,CANCEL=cancel)
    if cancel then return
    state.r.wavecal = (strtrim(arcname) eq '') ? 0:1
    
    if state.r.wavecal then begin
        
        arcpath = cfile(state.r.calpath+arcname,$
                        WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
        if cancel then setfocus,state.w.arcimage_fld
        if cancel then return        
        
    endif 
    
endif

str = 'Spextool '+state.w.version+' for '+state.r.instr

;  Write title of Spextool

if state.r.reductionmode eq 'A' or state.r.reductionmode eq 'A-Sky' then begin
    
    str = str+'- '+strmid(fullpaths[0],strpos(fullpaths[0],state.w.sep,$
                                              /REVERSE_S)+1)
    widget_control, state.w.xspextool_base, tlb_set_title = str
    
endif else begin
    
    str = str+'- '+strmid(fullpaths[0],strpos(fullpaths[0],state.w.sep,$
                                              /REVERSE_S)+1)+state.w.sep+$
      strmid(fullpaths[1],strpos(fullpaths[1],state.w.sep,/REVERSE_S)+1)
    widget_control, state.w.xspextool_base, tlb_set_title = str
    
endelse

;  Now read flat field and load info.

readflat,flatpath,flat,ncols,nrows,modename,slith_pix,slith_arc,$
  slitw_pix,slitw_arc,norders,orders,edgecoeffs,xranges,rms,rotation

; Load necessary info into memory and check if obsmode has changed.

*state.r.edgecoeffs = edgecoeffs
*state.r.orders     = orders
state.r.norders     = norders
state.r.slith_arc   = slith_arc
state.r.slitw_arc   = slitw_arc
state.r.slith_pix   = slith_pix
state.r.slitw_pix   = slitw_pix
state.r.modename    = modename
*state.r.xranges    = xranges
state.r.rotation    = rotation
state.r.ncols       = ncols
state.r.nrows       = nrows

;  Change the obsmode if necessary.

xspextool_message, state, 'Checking Obs Mode...'


if state.r.obsmode ne modename then begin

   *state.d.bdpxmask = rotate(*state.d.mask,rotation)
   
   widget_control, state.w.pscol2_base,tlb_get_size = size
   widget_control, state.w.pscol2_base, UPDATE=0
   widget_control, state.w.psorder_bg, /DESTROY
   widget_control, state.w.xscol2_base, UPDATE=0
   widget_control, state.w.xsorder_bg, /DESTROY
   
   getfonts,font
   
   labels = strtrim(reverse(*state.r.orders),2)
   set = intarr(state.r.norders)+1
   
   if state.w.general then begin
      
      state.w.psorder_bg = cw_bgroup(state.w.pscol2_base,$
                                     FONT=font,$
                                     labels,$
                                     X_SCROLL_SIZE=100,$
                                     XSIZE=100,$
                                     Y_SCROLL_SIZE=200,$
                                     /NONEXCLUSIVE,$
                                     /COLUMN,$
                                     /RETURN_NAME,$
                                     /SCROLL,$
                                     SET_VALUE=set,$
                                     UVALUE='PS Orders')
      
      state.w.xsorder_bg = cw_bgroup(state.w.xscol2_base,$
                                     FONT=font,$
                                     labels,$
                                     /NONEXCLUSIVE,$
                                     /COLUMN,$
                                     X_SCROLL_SIZE=100,$
                                     XSIZE=100,$
                                     Y_SCROLL_SIZE=200,$
                                     /SCROLL,$
                                     /RETURN_NAME,$
                                     SET_VALUE=set,$
                                     UVALUE='XS Orders')
      
   endif else begin
      
      state.w.psorder_bg = cw_bgroup(state.w.pscol2_base,$
                                     FONT=font,$
                                     labels,$
                                     /NONEXCLUSIVE,$
                                     /COLUMN,$
                                     /RETURN_NAME,$
                                     SET_VALUE=set,$
                                     UVALUE='PS Orders')
      
      state.w.xsorder_bg = cw_bgroup(state.w.xscol2_base,$
                                     FONT=font,$
                                     labels,$
                                     /NONEXCLUSIVE,$
                                     /COLUMN,$
                                     /RETURN_NAME,$
                                     SET_VALUE=set,$
                                     UVALUE='XS Orders')
      
   endelse

    widget_control, state.w.pscol2_base, /UPDATE
    widget_control, state.w.xscol2_base, /UPDATE

;  Now check mode for wavelength solution

    if modename eq 'LowRes15' or modename eq 'LowRes60' then begin

        state.r.usestoredwc = 1
        widget_control, state.w.storedwc_bg,SET_VALUE=1
        widget_control, state.w.storedwc_bg,SENSITIVE=0
        
    endif 
   
    *state.r.psdoorders = set
    *state.r.xsdoorders = set
    state.r.obsmode = modename
    widget_control, state.w.start_fld[1],set_value = strtrim(min(xranges),2)
    widget_control, state.w.stop_fld[1],set_value  = strtrim(max(xranges),2)


endif
 
;  Load the arcimage

if state.r.wavecal then *state.d.wavecal = readfits(arcpath)

;  Load the data

xspextool_message, state, 'Loading the data...'    

;  Load the linearity correction coefficients if necessary

if state.r.lc then restore,filepath('lc_coeff.sav',$
                                    ROOT_DIR=state.r.packagepath,SUBDIR='data')

if state.r.plotsatpix then sat = cfld(state.w.saturation_fld,4,/EMPTY,$
                                      CANCEL=cancel)

case state.r.reductionmode of 
    
    'A': begin

;  Check to see if the image has a variance image already.

        idx = strpos(fullpaths,'.fits')
        root = strjoin(strmid(fullpaths,0,idx))
        varfile = cfile(root+'_var.fits',CANCEL=cancel)

        if not cancel then begin
            
            data    = rotate(readfits(fullpaths,hdr),state.r.rotation)
            hdrinfo = gethdrinfo(hdr,state.r.keywords)
            var     = rotate(readfits(varfile),state.r.rotation)

        endif else begin

            readmfits,fullpaths,data,hdrinfo,var,$
              KEYWORDS=state.r.keywords,ROTATE=state.r.rotation,$
              ITIME=state.r.itime,COADDS=state.r.coadds,NDRS=state.r.ndrs,$
              GAIN=state.r.gain,READNOISE=state.r.readnoise,$
              LC_COEFF=lc_coeff,SLOWCNT=state.r.slowcnt,$
              WIDGET_ID=state.w.xspextool_base,SAT=sat,MASK=mask,CANCEL=cancel
            if cancel then return            
            
        endelse

    end
    'A-B': begin
        
        if not state.w.general then begin

            fullpaths = reorder(fullpaths,FILES=files,CANCEL=cancel)
            if cancel then return
            *state.r.workfiles = (*state.r.workfiles)[files]

        endif

        readmfits,fullpaths,data,hdrinfo,var,$
          KEYWORDS=state.r.keywords,ROTATE=state.r.rotation,$
          ITIME=state.r.itime,COADDS=state.r.coadds,NDRS=state.r.ndrs,$
          GAIN=state.r.gain,READNOISE=state.r.readnoise,$
          LC_COEFF=lc_coeff,SLOWCNT=state.r.slowcnt,$
          WIDGET_ID=state.w.xspextool_base,$
          /PAIR,SAT=sat,MASK=mask,CANCEL=cancel

    end

    'A-Sky': begin

        readmfits,fullpaths,data,hdrinfo,var,$
          KEYWORDS=state.r.keywords,ROTATE=state.r.rotation,$
          ITIME=state.r.itime,COADDS=state.r.coadds,NDRS=state.r.ndrs,$
          GAIN=state.r.gain,READNOISE=state.r.readnoise,$
          LC_COEFF=lc_coeff,SLOWCNT=state.r.slowcnt,$$
          WIDGET_ID=state.w.xspextool_base,SAT=sat,MASK=mask,CANCEL=cancel
        if cancel then return            
        
        sky = rotate(readfits(skypath),state.r.rotation)
        data = temporary(data)-sky
        
        idx     = strpos(skypath,'.fits')
        root    = strmid(skypath,0,idx)
        varfile = cfile(root+'_var.fits',CANCEL=cancel)
        skyvar  = rotate(readfits(varfile),state.r.rotation)
        var     = temporary(var)+skyvar

    end
    
endcase

if state.r.flatfield then begin

    xspextool_message,state,'Flat-fielding the data...'

    data = temporary(data)/flat
    var  = temporary(var)/flat^2

endif else xspextool_message,state,'Data not flat-fielded...'

if state.r.errorprop eq 0 then var[*] = 1.

*state.d.workimage = data
*state.r.hdrinfo   = hdrinfo
*state.d.varimage  = var

delvarx, data
delvarx, flat
delvarx, var
if state.r.lc then delvarx, lc_coeff

;  Determine Julian date of the observation

if not state.w.general then begin

    date = strsplit(hdrinfo[0].vals.DATE_OBS,'-',/EXTRACT)
    state.r.juldate = julday(date[1],date[2],date[0])
    
endif

;  Display image

ximgtool,*state.d.workimage,/AUTORANGE,BUFFER=1,WID=wid,$
         GROUP_LEADER=state.w.xspextool_base,MASK=mask,ZWINPOS='None', $
         STD=state.r.ncols

state.r.pscontinue = 1
state.r.xscontinue = 1

if keyword_set(BEEP) then beep

end
;
;******************************************************************************
;
pro xspextool_message,state,message

widget_control,state.w.message_window, /append,set_value=message
print, message

end
;
;******************************************************************************
;
pro xspextool_mkarcs, state, CANCEL=cancel, BEEP=beep

cancel = 0

;  Check filemode.

index    = (state.r.filereadmode eq 'Index') ? 1:0
filename = (state.r.filereadmode eq 'Filename') ? 1:0

if index then prefix = cfld(state.w.inprefix_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

;  Get user inputs.

arcfiles = cfld(state.w.arcimages_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
arcfiles = fsextract(arcfiles,INDEX=index,FILENAME=filename,CANCEL=cancel)
if cancel then return

arcfiles = mkfullpath(state.r.datapath,arcfiles,INDEX=index,FILENAME=filename,$
                      NI=state.r.nint,PREFIX=prefix,SUFFIX='*.fits',$
                      WIDGET_ID=state.w.xspextool_base,/EXIST,CANCEL=cancel)
if cancel then setfocus,state.w.arcimages_fld
if cancel then return

if state.r.arcreductionmode eq 'LongXD' then begin


    arcfiles = reorder(arcfiles,CANCEL=cancel)
    if cancel then return
    sky = cfld(state.w.arcsky_fld,7,/EMPTY,CANCEL=cancel)
    if cancel then return
    sky = fsextract(sky,/FILENAME,NFILES=nfiles,CANCEL=cancel)
    if cancel then return
    sky = mkfullpath(state.r.datapath,sky,/FILENAME,$
                     WIDGET_ID=state.w.xspextool_base,/EXIST,$
                     CANCEL=cancel)
    if cancel then return
    pairsky = (nfiles eq 2) ? 1:0

    
endif

flatname = cfld(state.w.arcflat_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
flatpath = cfile(state.r.calpath+flatname,WIDGET_ID=state.w.xspextool_base,$
                 CANCEL=cancel)
if cancel then setfocus,state.w.arcflat_fld
if cancel then return

arconame = cfld(state.w.arconame_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

wconame = cfld(state.w.wconame_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

;  Read images.

xspextool_message, state, 'Combining the arcs...'

readflat,flatpath,flat,ncols,nrows,obsmode,slith_pix,slith_arc,$
  slitw_pix,slitw_arc,norders,orders,edgecoeffs,xranges,rms,rotation,edgedeg

pair = (state.r.arcreductionmode eq 'ShortXD') ? 0:1

case state.r.arccombinestat of

    'Weighted Mean': begin

        readcmfits,arcfiles,arc,hdrinfo,mvar,KEYWORDS=state.r.keywords,$
          ROTATE=rotation,ITIME=state.r.itime,$
          COADDS=state.r.coadds,NDRS=state.r.ndrs,GAIN=state.r.gain,$
          PAIR=pair,READNOISE=state.r.readnoise,WEIGHTED=1,$
          /UPDATE,WIDGET_ID=state.w.xspextool_base,CANCEL=cancel
        if cancel then return

    end

    'Mean': begin
        
        readcmfits,arcfiles,arc,hdrinfo,mvar,KEYWORDS=state.r.keywords,$
          ROTATE=rotation,ITIME=state.r.itime,$
          COADDS=state.r.coadds,NDRS=state.r.ndrs,GAIN=state.r.gain,$
          PAIR=pair,READNOISE=state.r.readnoise,WEIGHTED=0,$
          /UPDATE,WIDGET_ID=state.w.xspextool_base,CANCEL=cancel
        if cancel then return

    end

    'Median': begin

        readmfits,arcfiles,data,hdrinfo,var,KEYWORDS=state.r.keywords,$
          ROTATE=rotation,ITIME=state.r.ITIME,COADDS=state.r.coadds,$
          NDRS=state.r.ndrs,GAIN=state.r.gain,READNOISE=state.r.readnoise,$
          NIMAGES=nimages,WIDGET_ID=state.w.xspextool_base,$
          PAIR=pair,CANCEL=cancel
        if cancel then return
        if nimages ne 1 then begin

            arc = median(data,/EVEN,DIMENSION=3)

        endif else arc = reform(data)

    end

endcase

;  If SpeX, then check for LongXD modes.

case obsmode of 

    'LongXD1.9': begin

        if pairsky then begin

            readmfits,sky[0],skya,hdra,ROTATE=rotation,$
              ITIME=state.r.itime,COADDS=state.r.coadds,NDRS=state.r.ndrs,$
              WIDGET_ID=state.w.xspextool_base,CANCEL=cancel
            if cancel then return
            readmfits,sky[1],skyb,hdrb,ROTATE=rotation,$
              ITIME=state.r.itime,COADDS=state.r.coadds,NDRS=state.r.ndrs,$
              WIDGET_ID=state.w.xspextool_base,CANCEL=cancel
            if cancel then return

            sky = (skya+skyb)-abs(skya-skyb)
            skyhistory = '  The sky frame are '+strjoin([hdra.vals.IRAFNAME,$
                                                      hdrb.vals.IRAFNAME],',')

        endif else begin
       
            readmfits,sky[0],sky,hdra,ROTATE=rotation,$
              ITIME=state.r.itime,COADDS=state.r.coadds,NDRS=state.r.ndrs,$
              WIDGET_ID=state.w.xspextool_base,CANCEL=cancel
            if cancel then return
            skyhistory = '  The sky frame is '+strjoin(hdra.vals.IRAFNAME)

        endelse 

        arc = concatimgs(arc,sky,edgecoeffs,xranges,orders,$
                         [5,7,8,9,10],[6])
        
    end

    'LongXD2.1': begin

        if pairsky then begin

            readmfits,sky[0],skya,hdra,ROTATE=rotation,$
              ITIME=state.r.itime,COADDS=state.r.coadds,NDRS=state.r.ndrs,$
              WIDGET_ID=state.w.xspextool_base,CANCEL=cancel
            if cancel then return
            readmfits,sky[1],skyb,hdrb,ROTATE=rotation,$
              ITIME=state.r.itime,COADDS=state.r.coadds,NDRS=state.r.ndrs,$
              WIDGET_ID=state.w.xspextool_base,CANCEL=cancel
            if cancel then return

            sky = (skya+skyb)-abs(skya-skyb)
            skyhistory = '  The sky frame are '+strjoin([hdra.vals.IRAFNAME,$
                                                       hdrb.vals.IRAFNAME],',')

        endif else begin
       
            readmfits,sky[0],sky,hdra,ROTATE=rotation,$
              ITIME=state.r.itime,COADDS=state.r.coadds,NDRS=state.r.ndrs,$
              WIDGET_ID=state.w.xspextool_base,CANCEL=cancel
            if cancel then return
            skyhistory = '  The sky frame is '+strjoin(hdra.vals.IRAFNAME)

        endelse 

        arc = concatimgs(arc,sky,edgecoeffs,xranges,orders,$
                        [5,7,8,9],[4,6])
        
    end

    'LongXD2.3': begin

        if pairsky then begin
            
            readmfits,sky[0],skya,hdra,ROTATE=rotation,$
              ITIME=state.r.itime,COADDS=state.r.coadds,NDRS=state.r.ndrs,$
              WIDGET_ID=state.w.xspextool_base,CANCEL=cancel
            if cancel then return
            readmfits,sky[1],skyb,hdrb,ROTATE=rotation,$
              ITIME=state.r.itime,COADDS=state.r.coadds,NDRS=state.r.ndrs,$
              WIDGET_ID=state.w.xspextool_base,CANCEL=cancel
            if cancel then return

            sky = (skya+skyb)-abs(skya-skyb)
            skyhistory = '  The sky frame are '+strjoin([hdra.vals.IRAFNAME,$
                                                       hdrb.vals.IRAFNAME],',')

        endif else begin

            readmfits,sky[0],sky,hdra,ROTATE=rotation,$
              ITIME=state.r.itime,COADDS=state.r.coadds,NDRS=state.r.ndrs,$
              WIDGET_ID=state.w.xspextool_base,CANCEL=cancel
            if cancel then return
            skyhistory = '  The sky frame is '+strjoin(hdra.vals.IRAFNAME)

        endelse 

        arc = concatimgs(arc,sky,edgecoeffs,xranges,orders,$
                         [5,7,8],[4,6])
        
    end

    else: skyhistory = ' '

endcase

;  Clean the bad pixels

xspextool_message, state,'Cleaning bad pixels...' 

arc = fixbdpx_median(arc,rotate(*state.d.mask,rotation),edgecoeffs,norders,$
                     xranges,20,CANCEL=cancel)
if cancel then return

newhdr = hdrinfo[0]
junk = tag_exist(hdrinfo[*].vals,state.r.irafname,INDEX=idx)
newhdr.vals.(idx) = arconame+'.fits'

history='This arc was created by '+state.r.arccombinestat+$
  ' combining '+strjoin(hdrinfo[*].vals.(idx), ', ')+'.'+skyhistory

writearc,unrotate(arc,rotation),state.r.calpath+arconame+'.fits',newhdr,$
  state.w.version,HISTORY=history

;  Update message board and beep.

xspextool_message, state, 'Wrote arc to '+state.r.calpath+arconame+'.fits'

ximgtool,arc,WID=wid,GROUP_LEADER=state.w.xspextool_base,/AUTORANGE, $
         BUFFER=1,ZWINPOS='None'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    xspextool_message, state, 'Wavelength Calibrating...'

;    xranges[0,*] = xranges[0,*] > start
;    xranges[1,*] = xranges[1,*] < stop
    
    fitthresh = cfld(state.w.wavecalthresh_fld,4,CANCEL=cancel,/EMPTY)
    if cancel then return

    date = strsplit(hdrinfo[0].vals.DATE_OBS,'-',/EXTRACT)
    juldate = julday(date[1],date[2],date[0])

;  Determine cal file name

    if obsmode eq 'ShortXD' then begin

        root = (juldate gt 2451758) ? 'ShortXD_g2':'ShortXD_g1'
        file = filepath(root+'.fits',ROOT_DIR=state.r.packagepath,$
                        SUBDIR='data')

    endif else file = filepath(obsmode+'.fits',$
                               ROOT_DIR=state.r.packagepath,SUBDIR='data')

    if obsmode eq 'LowRes15' or obsmode eq 'LowRes60' then begin

        state.r.usestoredwc = 1
        widget_control, state.w.storedwc_bg,SET_VALUE=1
;        widget_control, state.w.storedwc_bg,SENSITIVE=0
        
    endif else begin

;        state.r.usestoredwc = 0
;        widget_control, state.w.storedwc_bg,SET_VALUE=0
;        widget_control, state.w.storedwc_bg,SENSITIVE=1

    endelse

;  Read cal files and get standard arc spectra for X-correlation.

    rdwavecal,file,xcor_spectra,xcor_orders,linetypes,l2pcoeffs,homeorder,$
      dispdeg,ordrdeg,resppix,p2lcoeffs,rms,CANCEL=cancel

;  Create trace

    imginfo,arc,ncols,nrows
    x = findgen(ncols)
    posarc = fltarr(1,norders)
    posarc[*] = slith_arc/2.
    tracecoeffs = aptotrace(edgecoeffs,slith_arc,posarc,xranges,5,edgedeg,$
                            nrows,WID=wid)

    wavespec = extractspec_xs(arc,fltarr(ncols,nrows)+1.,edgecoeffs,$
                              tracecoeffs,norders,1,0,ncols-1,xranges,$
                              slith_arc,[1],/UPDATE,$
                              WIDGET_ID=state.w.xspextool_base,$
                              CANCEL=cancel)

;  Do the cross-correlation.
        
    match,orders,xcor_orders,idx_arc,idx_xcor
    
    arc_x             = findgen(ncols)
    arc_y             = fltarr(ncols)
    arc_y             = reform(wavespec[*,0,idx_arc[0]])
    
    xcor_x            = reform(xcor_spectra[*,0,idx_xcor[0]])
    xcor_y            = reform(xcor_spectra[*,1,idx_xcor[0]])
    
;  Protect against NaNs

    z = where(finite(arc_y) eq 0,count)
    if count ne 0 then arc_y[z] = 0.0 
    
    z = where(finite(xcor_y) eq 0,count)
    if count ne 0 then xcor_y[z] = 0.0 
        
;  Smooth standard arc spectrum for X-correlation with the IP
        
    nkernel = fix(slitw_pix*4)
    nkernel = (nkernel mod 2 eq 1) ? nkernel:nkernel+1
    
    case float(slitw_arc) of 
            
        0.3: parms = [0.0,0.967311,0.777740]
        
        0.5: parms = [0.0,1.61542,0.724913]
        
        0.8: parms = [0.0,2.59912,0.762567]
        
        1.6: parms = [0.0,5.16330,0.782718]
        
        3.0: parms = [0.0,9.63019,0.834605]
        
    endcase
    
    kernel = instrprof(findgen(nkernel)-nkernel/2,parms)
    kernel = kernel/total(kernel)
    xcor_y = convol(xcor_y,kernel)
                
    if state.r.autoxcorr then begin
                
        lag = findgen(ncols)-fix(ncols/2.)
        corr = c_correlate(xcor_y,arc_y,lag)
        max = max(corr,idx)
                
        fit = gaussfit(lag[idx-20:idx+20],corr[idx-20:idx+20],a,$
                       NTERMS=4)
        offset = a[1]
        if state.r.plotautoxcorr then begin
            
            window, /FREE
            wid = !d.window
            !p.multi[0] = 2
            !p.multi[2] = 2
            
            plot, lag,corr,/xsty,/ysty
            plot,lag[idx-20:idx+20],corr[idx-20:idx+20],/xsty,/ysty,$
              TITLE='!5Offset = '+strtrim(offset,2)
            oplot, lag[idx-20:idx+20],fit,color=2
            
            result = dialog_message('Is this ok?',$
                                    /QUESTION)
            if strtrim(result,2) eq 'No' then begin
                
                wdelete, wid
                goto, manual1
                
            endif else wdelete, wid
                    
        endif
        message = 'Cross-correlation offset = '+strtrim(offset,2)+' pixels.'
        xspextool_message,state, message 
                
    endif else begin 
        
        manual1:
        beep
        message = 'Please Identify the same line in each spectrum...'
        xspextool_message,state,message
        xgetoffset,xcor_x,xcor_y,arc_x,arc_y,offset,ORDER=$
          total(xcor_orders[idx_xcor[0]])
        message = 'Cross-correlation offset = '+$
          strtrim(offset,2)+' pixels.'
        xspextool_message,state, message 
        
    endelse
    
    if not state.r.usestoredwc then begin

;  Check to make sure the ORDER degree is ok.
        
        ordrdeg = (norders-2) < ordrdeg
        ordrdeg = 0 > ordrdeg
        
;  Get lines
        
        file = filepath('lines.dat',ROOT_DIR=state.r.packagepath,SUBDIR='data')
        rdll,file,plines,ptypes,RES=resppix/state.r.slitw_pix,THRESH=3,$
          CANCEL=cancel

;  Perform wavelength calibration

        p2lcoeffs = wavecal(x,reform(wavespec[*,0,*]),norders,1,l2pcoeffs,$
                            linetypes,offset,fix(orders),homeorder,slitw_pix,$
                            dispdeg,ordrdeg,plines,ptypes,fitthresh,RMS=rms,$
                            /UPDATE,WIDGET_ID=state.w.xspextool_base,$
                            PLOTRESID=state.r.plotresiduals,$
                            PLOTLINEFIND=state.r.plotlinefind,CANCEL=cancel)
        if cancel then return

        xspextool_message, state,'The RMS deviation for each aperture are '+$
          strjoin(strtrim(rms*1e4,2), ', ')+' Angstroms.'

    endif else begin

        x = x-offset
              
    endelse

    spectra = fltarr(ncols,3,norders)
    disp    = fltarr(norders)

    for j = 0, norders-1 do begin
            
        result = poly2d(x,replicate(fix(orders[j]),n_elements(x)),dispdeg,$
                        ordrdeg,p2lcoeffs)
        spectra[*,0,j] = result*(float(homeorder)/float(orders[j]))
        
        spectra[*,1,j]= wavespec[*,0,j]
        spectra[*,2,j]= sqrt(wavespec[*,1,j])

        if state.r.errorprop eq 0 then spectra[*,2,j] = 1.
            
        coeff = poly_fit1d(x,spectra[*,0,j],1,/SILENT)
        disp[j] = coeff[1]
        
    endfor
   
res    = resppix/slitw_pix

yunits='DN / s'
xunits= 'um'

xtitle='!7k!5 (!7l!5m)'
ytitle='f (!5DN s!u-1!N)'


arc = readfits(state.r.calpath+arconame+'.fits',hdr)
hdrinfo = gethdrinfo(hdr,state.r.keywords)

junk          = tag_exist(hdrinfo.vals,state.r.irafname,INDEX=idx)
hdrinfo.vals.(idx) = strmid(state.r.calpath+wconame,$
                            strpos(state.r.calpath+wconame,state.w.sep,$
                                   /REVERSE_S)+1)+'.fits'

writespec_xs,spectra,state.r.calpath+wconame,arconame+'.fits','None', $
             flatname,1,fix(orders),0,ncols-1,hdrinfo,[7.5],[1],obsmode, $
             slith_pix,slith_arc,slitw_pix,slitw_arc,xunits,yunits,xtitle,$
             ytitle,state.w.version,DISP=disp,/FITS,RMS=rms,RES=res
        
getout:

if keyword_set(BEEP) then beep

end
;
;******************************************************************************
;
pro xspextool_mkcals,state,BEEP=beep

widget_control, state.w.table, get_value=array

prefix = cfld(state.w.inprefix_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

;  Inspect all of the cal sets to make sure they are really cal sets

flatinfo = strarr(2,state.r.tablesize)
arcinfo  = strarr(6,state.r.tablesize)

for i = 0, state.r.tablesize-1 do begin

    files = strtrim(array[0,i],2)
    if files eq '' then goto, cont1

    xspextool_message,state,'Inspecting Cal Set '+strcompress(i+1,/RE)+'...'

    files = fsextract(files,/INDEX,CANCEL=cancel)
    if cancel then return
    
    fullpaths = mkfullpath(state.r.datapath,files,/INDEX,NI=4,PREFIX='*',$
                           SUFFIX='*.fits',WIDGET_ID=state.w.xspextool_base,$
                           /EXIST,CANCEL=cancel)
    if cancel then begin

        xspextool_message,state,'Cal Set '+strcompress(i+1,/RE)+$
          ' does not exist.'
        return

    endif
    fullpaths = findfiles(fullpaths)

    flats = findspexflats(fullpaths,MASK=flatmask,CANCEL=cancel)
 
    if cancel then return
    z = where(flatmask eq 1,count)

    flatoname = 'flat'+strcompress(files[z[0]],/RE)+'-'+$
      strcompress(files[z[count-1]],/RE)+'.fits'

    
    flatinfo[0,i] = strjoin(flats,',')
    flatinfo[1,i] = flatoname

    arcs = findspexarcs(fullpaths,MASK=arcmask,AB=ab,CANCEL=cancel)
    if cancel then return
    arcinfo[0,i] = strjoin(arcs,',')
    arcinfo[1,i] = flatoname
    arcinfo[2,i] = (AB eq 0) ? string(0):string(1)
    
    hdr     = headfits(arcs[0])
    obsmode = strcompress(fxpar(hdr,'GRAT'),/RE)
    if obsmode eq 'LongXD1.9' or obsmode eq 'LongXD2.1' or obsmode eq $
      'LongXD2.3' then begin

        skys = strtrim(array[1,i],2)
        if skys eq '' then begin

            xspextool_message, state, $
              'Please load a sky frame(s) for Cal Set '+strtrim(i+1,2)+'.'
            return

        endif
            
        skys = fsextract(skys,/INDEX,CANCEL=cancel)
        IF cancel THEN return
        skys = mkfullpath(state.r.datapath,skys,/INDEX,NI=4,PREFIX=prefix,$
                          SUFFIX='*.fits',WIDGET_ID=state.w.xspextool_base,$
                          /EXIST,CANCEL=cancel)
        if cancel then return
                        
        if n_elements(skys) gt 1 then skys = strjoin(skys,',')
        arcinfo[3,i] = skys
                
    endif
    z = where(arcmask eq 1,count)
    arconame = 'arc'+strcompress(files[z[0]],/RE)+'-'+$
      strcompress(files[z[count-1]],/RE)+'.fits'        
    arcinfo[4,i] = arconame

    waveoname = 'wavecal'+strcompress(files[z[0]],/RE)+'-'+$
      strcompress(files[z[count-1]],/RE)       
    arcinfo[5,i] = waveoname
    
    cont1:

endfor

xspextool_message,state,'Inspection complete.'

; Construct the flat

if state.r.lc then restore, $
  filepath('lc_coeff.sav',ROOT_DIR=state.r.packagepath,SUBDIR='data')

for i = 0, state.r.tablesize-1 do begin

    if flatinfo[0,i] eq '' then goto,getout
    
    xspextool_message,state,'Constructing flat in Cal Set '+$
      strcompress(i+1,/RE)+'...'

    files = strsplit(flatinfo[0,i],',',/EXTRACT)
    nfiles = n_elements(files)

;  Read info from mode_flat.dat

    readmfits,files[0],junk1,hdrinfo,KEYWORDS=state.r.keywords,CANCEL=cancel
    if cancel then return

    date = strsplit(hdrinfo[0].vals.DATE_OBS,'-',/EXTRACT)
    juldate = julday(date[1],date[2],date[0])

    dit = strtrim(hdrinfo[0].vals.DIT,2)

    obsmode = strcompress( (hdrinfo[0]).vals.GRAT,/re )

    readcal,state.r.packagepath+'data/'+obsmode+'.dat',modename,rotation,$
      norders,orders,start,stop,slith_pix,slith_pix_range,slith_arc,edgedeg,$
      norm_nxg,norm_nyg,oversamp,norm_ybuffer,fixed,guesspos,CANCEL=cancel
    if cancel then return

    if dit eq '0.900000' then begin

        norders = 4
        orders = [3,4,5,6]
        guesspos = guesspos[*,0:3]

    endif


    slitw_arc = float( strmid(hdrinfo[0].vals.SLIT,0,3) )
    slitw_pix = slitw_arc / float(hdrinfo[0].vals.PLATE_SC)

    readmfits,files,data,hdrinfo,var,KEYWORDS=state.r.keywords,$
      ROTATE=rotation,ITIME=state.r.itime,COADDS=state.r.coadds,$
      NDRS=state.r.ndrs,GAIN=state.r.gain,READNOISE=state.r.readnoise,$
      WIDGET_ID=state.w.xspextool_base,CANCEL=cancel
    if cancel then return

    if nfiles ne 1 then begin

;  Scale the the images (each set of orders) to the same level.

        xspextool_message,state,'Scaling the flats...'

        scaleimages,data,var,CANCEL=cancel
        if cancel then return

;  Combine the images together.
        
        xspextool_message, state,'Combining the flats...'
        
        case state.r.calcombinestat of 
            
            'Weighted Mean': meancomb,data,mean,mvar,DATAVAR=var,CANCEL=cancel

            'Mean': meancomb,data,mean,mvar,CANCEL=cancel

            'Median': mean = median(data,/EVEN,DIMENSION=3)

        endcase
        
    endif else mean = data
    if cancel then return
;    delvarx,mvar
;    delvarx,data

;  Find orders

;    scale = (state.r.ximgtool eq 0) float(state.r.itime):1.0
    ximgtool, mean, SCALING='Hist Eq',WID=wid, $
              GROUP_LEADER=state.w.xspextool_base,$
              RANGE='0-Max',BUFFER=1,ZWINPOS='None'

    xspextool_message, state,'Locating the orders...'

    frac = cfld(state.w.frac_fld,4,/EMPTY,CANCEL=cancel)
    if cancel then return

    findorders,mean,guesspos,start,stop,5,slith_pix_range,edgedeg,$
      edgecoeffs,xranges,FRAC=frac,WID=wid,CANCEL=cancel
    if cancel then return

;    scale = (state.r.ximgtool eq 0) float(state.r.itime):1.0
    ximgtool,mean,SCALING='Hist Eq',GROUP_LEADER=state.w.xspextool_base,$
      RANGE='0-Max',BUFFER=1,ZWINPOS='None'

;  Normalize flat

    xspextool_message, state,'Normalizing the flat...'
    
    mean = normspecflat(mean,edgecoeffs,xranges,slith_arc,norm_nxg,norm_nyg,$
                        oversamp,norm_ybuffer,RMS=rms,MODEL=model,/UPDATE,$
                        WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
    if cancel then return    

;  Replace bad pixels with 

    z = where(rotate(*state.d.mask,rotation) eq 0,count)
    if count ne 0 then mean[z] = 1.0

    ximgtool, mean,ZRANGE=[0.9,1.1],GROUP_LEADER=state.w.xspextool_base, $
              BUFFER=1,ZWINPOS='None'

;  Write the results to disk.

    junk = tag_exist(hdrinfo[*],state.r.irafname,INDEX=idx)
    
    history='This flat was created by 1. scaling '+$
      strjoin(hdrinfo[*].vals.(idx), ', ')+' using the median and then 2. '+$
      state.r.calcombinestat+' combining the resulting images.'
    
    newhdr = avehdrs(hdrinfo,TIME_OBS=state.r.time,POSANGLE=state.r.posangle,$
                     HA=state.r.ha,AIRMASS=state.r.airmass,CANCEL=cancel)
    if cancel then return
    
    newhdr.vals.(idx) = flatinfo[1,i]


    writeflat,unrotate(mean,7),newhdr,7,fix(orders),edgecoeffs,$
      xranges,slith_pix,slith_arc,slitw_pix,slitw_arc,modename,rms,$
      state.w.version,state.r.calpath+flatinfo[1,i],HISTORY=history
    
    Xspextool_message,state,'Wrote flat to '+state.r.calpath+flatinfo[1,i]

;************Arc

    xspextool_message,state,'Constructing arc in Cal Set '+strtrim(i+1,2)+'...'


    files = strsplit(arcinfo[0,i],',',/EXTRACT)
    nfiles = n_elements(files)

    pair = (fix(arcinfo[2,i]) eq 0) ? 0:1

    readcmfits,files,arc,hdrinfo,mvar,KEYWORDS=state.r.keywords,$
      ROTATE=rotation,ITIME=state.r.itime,$
      COADDS=state.r.coadds,NDRS=state.r.ndrs,GAIN=state.r.gain,$
      PAIR=pair,READNOISE=state.r.readnoise,WEIGHTED=0,$
      WIDGET_ID=state.w.xspextool_base,CANCEL=cancel
    if cancel then return

;  Get sky if necessary

    if arcinfo[3,i] ne '' then begin
        
        sky = strsplit(arcinfo[3,i],',',/EXTRACT)

        if n_elements(sky) gt 1 then begin

            readmfits,sky[0],skya,hdrskya,KEYWORDS=['IRAFNAME'],ROTATE=7,$
              ITIME=state.r.itime,COADDS=state.r.coadds,NDRS=state.r.ndrs,$
              WIDGET_ID=state.w.xspextool_base,CANCEL=cancel
            if cancel then return
            readmfits,sky[1],skyb,hdrskyb,KEYWORDS=['IRAFNAME'],ROTATE=7,$
              ITIME=state.r.itime,COADDS=state.r.coadds,NDRS=state.r.ndrs,$
              WIDGET_ID=state.w.xspextool_base,CANCEL=cancel
            if cancel then return
            sky = (skya+skyb)-abs(skya-skyb)
            skyhistory = '  The sky frame is '+strjoin([hdrskya.vals.IRAFNAME,$
                                                  hdrskyb.vals.IRAFNAME],',')
            
        endif else begin
            
            readmfits,sky[0],sky,skyhdr,KEYWORDS=['IRAFNAME'],ROTATE=7,$
              ITIME=state.r.itime,COADDS=state.r.coadds,NDRS=state.r.ndrs,$
              WIDGET_ID=state.w.xspextool_base,CANCEL=cancel
            if cancel then return
            skyhistory = '  The sky frame is '+strjoin(skyhdr.vals.IRAFNAME)
            
        endelse 

        case obsmode of 
        
            'LongXD1.9': arc = concatimgs(arc,sky,edgecoeffs,xranges,$
                                         float(orders),$
                                         [5,7,8,9,10],[6])
            
            'LongXD2.1': arc = concatimgs(arc,sky,edgecoeffs,xranges,$
                                         float(orders),[5,7,8,9],[4,6])
            
            'LongXD2.3': arc = concatimgs(arc,sky,edgecoeffs,xranges,$
                                         float(orders),[5,7,8],[4,6])

        else:
        
        endcase

    endif else skyhistory = ''

    arc = fixbdpx_median(arc,rotate(*state.d.mask,rotation),edgecoeffs,$
                         norders,xranges,20,CANCEL=cancel)

    if cancel then return
    ximgtool,arc,/AUTORANGE,WID=wid,GROUP_LEADER=state.w.xspextool_base, $
             BUFFER=1,ZWINPOS='None'
    
    junk = tag_exist(hdrinfo[*].vals,state.r.irafname,INDEX=idx)
    
    newhdr = hdrinfo[0]
    newhdr.vals.(idx) = arcinfo[4,i]

    history='This arc was created by mean'+$
      ' combining '+strjoin(hdrinfo[*].vals.(idx), ', ')+'.'+skyhistory
    
    writearc,unrotate(arc,7),state.r.calpath+arcinfo[4,i],newhdr,$
      state.w.version,HISTORY=history

    xspextool_message, state, 'Wrote arc to '+state.r.calpath+arcinfo[4,i]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    xspextool_message, state, 'Wavelength Calibrating...'

;    xranges[0,*] = xranges[0,*] > start
;    xranges[1,*] = xranges[1,*] < stop
    
    fitthresh = cfld(state.w.wavecalthresh_fld,4,CANCEL=cancel,/EMPTY)
    if cancel then return

;  Determine cal file name

    if obsmode eq 'ShortXD' then begin

        root = (juldate gt 2451758) ? 'ShortXD_g2':'ShortXD_g1'
        file = filepath(root+'.fits',ROOT_DIR=state.r.packagepath,$
                        SUBDIR='data')

    endif else file = filepath(obsmode+'.fits',$
                               ROOT_DIR=state.r.packagepath,SUBDIR='data')

    if obsmode eq 'LowRes15' or obsmode eq 'LowRes60' then begin

        state.r.usestoredwc = 1
        widget_control, state.w.storedwc_bg,SET_VALUE=1
        
    endif else begin

;        state.r.usestoredwc = 0
;        widget_control, state.w.storedwc_bg,SET_VALUE=0
;        widget_control, state.w.storedwc_bg,SENSITIVE=1

    endelse

;  Read cal files and get standard arc spectra for X-correlation.

    rdwavecal,file,xcor_spectra,xcor_orders,linetypes,l2pcoeffs,$
      homeorder,dispdeg,ordrdeg,resppix,p2lcoeffs,rms,CANCEL=cancel

;  Create trace

    imginfo,arc,ncols,nrows
    x = findgen(ncols)
    posarc = fltarr(1,norders)
    posarc[*] = slith_arc/2.
    tracecoeffs = aptotrace(edgecoeffs,slith_arc,posarc,xranges,5,edgedeg,$
                            nrows,WID=wid)

    wavespec = extractspec_xs(arc,fltarr(ncols,nrows)+1.,edgecoeffs,$
                              tracecoeffs,norders,1,0,ncols-1,xranges,$
                              slith_arc,[1],/UPDATE,$
                              WIDGET_ID=state.w.xspextool_base,$
                              CANCEL=cancel)

;  Do the cross-correlation.
        
    match,orders,xcor_orders,idx_arc,idx_xcor
    
    arc_x             = findgen(ncols)
    arc_y             = fltarr(ncols)
    arc_y             = reform(wavespec[*,0,idx_arc[0]])
    
    xcor_x            = reform(xcor_spectra[*,0,idx_xcor[0]])
    xcor_y            = reform(xcor_spectra[*,1,idx_xcor[0]])
    
;  Protect against NaNs

    z = where(finite(arc_y) eq 0,count)
    if count ne 0 then arc_y[z] = 0.0 
    
    z = where(finite(xcor_y) eq 0,count)
    if count ne 0 then xcor_y[z] = 0.0 
        
;  Smooth standard arc spectrum for X-correlation with the IP
        
    nkernel = fix(slitw_pix*4)
    nkernel = (nkernel mod 2 eq 1) ? nkernel:nkernel+1

    case slitw_arc of 
            
        0.3: parms = [0.0,0.967311,0.777740]
        
        0.5: parms = [0.0,1.61542,0.724913]
        
        0.8: parms = [0.0,2.59912,0.762567]
        
        1.6: parms = [0.0,5.16330,0.782718]
        
        3.0: parms = [0.0,9.63019,0.834605]
        
    endcase
    
    Kernel = instrprof(findgen(nkernel)-nkernel/2,parms)
    kernel = kernel/total(kernel)
    xcor_y = convol(xcor_y,kernel)
                
    if state.r.autoxcorr then begin
                
        lag = findgen(ncols)-fix(ncols/2.)
        corr = c_correlate(xcor_y,arc_y,lag)
        max = max(corr,idx)
                
        fit = gaussfit(lag[idx-20:idx+20],corr[idx-20:idx+20],a,$
                       NTERMS=4)
        offset = a[1]
        if state.r.plotautoxcorr then begin
            
            window, /FREE
            wid = !d.window
            !p.multi[0] = 2
            !p.multi[2] = 2
            
            plot, lag,corr,/xsty,/ysty
            plot,lag[idx-20:idx+20],corr[idx-20:idx+20],/xsty,/ysty,$
              TITLE='!5Offset = '+strtrim(offset,2)
            oplot, lag[idx-20:idx+20],fit,color=2
            
            result = dialog_message('Is this ok?',$
                                    /QUESTION)
            if strtrim(result,2) eq 'No' then begin
                
                wdelete, wid
                goto, manual1
                
            endif else wdelete, wid
                    
        endif
        message = 'Cross-correlation offset = '+strtrim(offset,2)+' pixels.'
        xspextool_message,state, message 
                
    endif else begin 
        
        manual1:
        beep
        message = 'Please Identify the same line in each spectrum...'
        xspextool_message,state,message
        xgetoffset,xcor_x,xcor_y,arc_x,arc_y,offset,ORDER=$
          total(xcor_orders[idx_xcor[0]])
        message = 'Cross-correlation offset = '+$
          strtrim(offset,2)+' pixels.'
        xspextool_message,state, message 
        
    endelse
    
    if not state.r.usestoredwc then begin

;  Check to make sure the ORDER degree is ok.
        
        ordrdeg = (norders-2) < ordrdeg
        ordrdeg = 0 > ordrdeg
        
;  Get lines
        
        file = filepath('lines.dat',ROOT_DIR=state.r.packagepath,SUBDIR='data')
        rdll,file,plines,ptypes,RES=resppix/state.r.slitw_pix,THRESH=3,$
          CANCEL=cancel

;  Perform wavelength calibration

        p2lcoeffs = wavecal(x,reform(wavespec[*,0,*]),norders,1,l2pcoeffs,$
                            linetypes,offset,fix(orders),homeorder,slitw_pix,$
                            dispdeg,ordrdeg,plines,ptypes,fitthresh,RMS=rms,$
                            /UPDATE,WIDGET_ID=state.w.xspextool_base,$
                            PLOTRESID=state.r.plotresiduals,$
                            PLOTLINEFIND=state.r.plotlinefind,CANCEL=cancel)
        if cancel then return

        xspextool_message, state,'The RMS deviation for is '+$
          strjoin(strtrim(rms*1e4,2), ', ')+' Angstroms.'

    endif else begin

        x = x-offset
              
    endelse

    spectra = fltarr(ncols,3,norders)
    disp    = fltarr(norders)

    for j = 0, norders-1 do begin
            
        result = poly2d(x,replicate(fix(orders[j]),n_elements(x)),dispdeg,$
                        ordrdeg,p2lcoeffs)
        spectra[*,0,j] = result*(float(homeorder)/float(orders[j]))
        
        spectra[*,1,j]= wavespec[*,0,j]
        spectra[*,2,j]= sqrt(wavespec[*,1,j])

        if state.r.errorprop eq 0 then spectra[*,2,j] = 1.
            
        coeff = poly_fit1d(x,spectra[*,0,j],1,/SILENT)
        disp[j] = coeff[1]
        
    endfor
    
    res    = resppix/slitw_pix
    yunits = 'DN / s'
    xunits = 'um'
    
    xtitle='!7k!5 (!7l!5m)'
    ytitle='f (!5DN s!u-1!N)'

    arc = readfits(state.r.calpath+arcinfo[4,i],hdr)
    hdrinfo = gethdrinfo(hdr,state.r.keywords)
    
    junk          = tag_exist(hdrinfo,state.r.irafname,INDEX=idx)
    hdrinfo.vals.(idx) = strmid(arcinfo[5,i],strpos(arcinfo[5,i],state.w.sep,$
                                                    /REVERSE_S)+1)+'.fits'

    appos = fltarr(1,norders)
    appos[0,*] = state.r.slith_arc/2.
    writespec_xs,spectra,state.r.calpath+arcinfo[5,i],arcinfo[4,i],'None', $
                 flatinfo[1,i],1,fix(orders),0,ncols-1,hdrinfo, $
                 appos,[1], $
                 obsmode,slith_pix,slith_arc,slitw_pix,slitw_arc, $
                 xunits,yunits,xtitle,ytitle,state.w.version, $
                 WAVECAL=arcinfo[5,i],DISP=disp,/FITS,RMS=rms,RES=res
        
endfor

xspextool_message, state,'Mkcals is finished.'
beep

getout:

end
;
;******************************************************************************
;
pro xspextool_mkflat, state, CANCEL=cancel, BEEP=beep

cancel = 0

;  Check file readmode.

index    = (state.r.filereadmode eq 'Index') ? 1:0
filename = (state.r.filereadmode eq 'Filename') ? 1:0

if index then prefix = cfld(state.w.inprefix_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

;  Get user inputs.

files = cfld(state.w.flats_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
files = fsextract(files,INDEX=index,FILENAME=filename,NFILES=nfiles,$
                  CANCEL=cancel)
if cancel then return

files = mkfullpath(state.r.datapath,files,INDEX=index,FILENAME=filename,$
                   NI=state.r.nint,PREFIX=prefix,SUFFIX='*.fits',$
                   WIDGET_ID=state.w.xspextool_base,/EXIST,CANCEL=cancel)
if cancel then return


flatoname = cfld(state.w.flatoname_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

step = cfld(state.w.tracestepsize_fld,3,/EMPTY,CANCEL=cancel)
if cancel then return
step = crange(step,[1,20],'Step',/KGE,/KLE,WIDGET_ID=state.w.xspextool_base,$
              CANCEL=cancel)
if cancel then return

;  Read info from cal file

if state.w.general then begin

    calfile = cfld(state.w.calfile_fld,7,/EMPTY,CANCEL=cancel)
    if cancel then return
    calfile = cfile(filepath(calfile,ROOT_DIR=state.r.packagepath,$
                             SUBDIR='data'),WIDGET_ID=state.w.xspextool_base,$
                    CANCEL=cancel)
    if cancel then return

    readcal,calfile,modename,rotation,norders,orders,start,stop,slith_pix,$
      slith_pix_range,slith_arc,edgedeg,norm_nxg,norm_nyg,oversamp,$
      norm_ybuffer,fixed,$
      guesspos,CANCEL=cancel
    if cancel then return

    slitw_arc = cfld(state.w.slitw_arc_fld,4,/EMPTY,CANCEL=cancel)
    if cancel then return
    slitw_pix = cfld(state.w.slitw_pix_fld,4,/EMPTY,CANCEL=cancel)
    if cancel then return

endif else begin

    readmfits,files[0],junk1,hdrinfo,KEYWORDS=state.r.keywords

    dit = strtrim(hdrinfo[0].vals.DIT,2)

    mode = strcompress( (hdrinfo[0]).vals.GRAT,/re )
    readcal,state.r.packagepath+'data/'+mode+'.dat',modename,rotation,$
      norders,orders,start,stop,slith_pix,slith_pix_range,slith_arc,edgedeg,$
      norm_nxg,norm_nyg,oversamp,norm_ybuffer,fixed,guesspos,CANCEL=cancel
    if cancel then return

    if dit eq '0.900000' then begin
        
        norders = 4
        orders = [3,4,5,6]
        guesspos = guesspos[*,0:3]
        
    endif

    slitw_arc = float( strmid(strtrim(hdrinfo[0].vals.SLIT,2),0,3) )
    slitw_pix = slitw_arc / float(hdrinfo[0].vals.PLATE_SC)

endelse

xspextool_message,state,'Loading the flats...'

if state.r.lc then restore, $
  filepath('lc_coeff.sav',ROOT_DIR=state.r.packagepath,SUBDIR='data') 

readmfits,files,data,hdrinfo,var,KEYWORDS=state.r.keywords,$
  ROTATE=rotation,ITIME=state.r.ITIME,COADDS=state.r.coadds,$
  NDRS=state.r.ndrs,GAIN=state.r.gain,READNOISE=state.r.readnoise,$
  NIMAGES=nimages,WIDGET_ID=state.w.xspextool_base,CANCEL=cancel
if cancel then return

;  Find orders

if nimages ne 1 then begin
        
;  Scale the the images (each set of orders) to the same level.
    
    xspextool_message,state,'Scaling the flats...'
    
    scaleimages,data,var,CANCEL=cancel
    if cancel then return
    
;  Combine the images together.
        
    xspextool_message, state,'Combining the flats...'

    case state.r.flatcombinestat of 
        
        'Weighted Mean': meancomb,data,mean,DATAVAR=var,CANCEL=cancel

        'Mean': meancomb,data,mean,CANCEL=cancel

        'Median': mean = median(data,/EVEN,DIMENSION=3)

    endcase
    
endif else mean = data
if cancel then return
delvarx,data

;  Find orders

ximgtool, mean, SCALING='Hist Eq',WID=wid, ZWINPOS='None',$
          GROUP_LEADER=state.w.xspextool_base,RANGE='0-Max',BUFFER=1, $
          STDIMAGE=state.r.ncols

xspextool_message, state,'Locating the orders...'

if state.w.general and fixed eq 'Yes' then begin

    plots, !x.crange,[guesspos[0],guesspos[0]],color=2
    plots, !x.crange,[guesspos[1],guesspos[1]],color=2
    xranges = intarr(2,1)
    xranges = [start,stop]
    edgecoeffs = fltarr(2,2,1)
    edgecoeffs[0,*,0] = guesspos
    edgecoeffs[1,*,0] = [0.,0.]

endif else begin

    frac = cfld(state.w.frac_fld,4,/EMPTY,CANCEL=cancel)
    if cancel then return
    findorders,mean,guesspos,start,stop,5,slith_pix_range,edgedeg,$
      edgecoeffs,xranges,FRAC=frac,WID=wid,CANCEL=cancel
    if cancel then return

endelse



;scale = (state.r.ximgtool eq 0) float(state.r.itime):1.0
ximgtool,mean,SCALING='Hist Eq',GROUP_LEADER=state.w.xspextool_base,$
         RANGE='0-Max',BUFFER=1,ZWINPOS='None'

if state.r.normalizeflat eq 'Yes' then begin
    
    xspextool_message, state,'Normalizing the flat...'
    
;    if state.w.general then begin
;
;        tmp = mean
;        result = maskorders(mean,edgecoeffs,-1,XRANGES=xranges,CANCEL=cancel)
;        z = where(finite(result) eq 0)
;        med = median(mean[z],/EVEN)
;        mean[z] = mean[z]/med
;        rms = 1.482*median(mean[z]-med,/EVEN)
;        z = where(finite(result) eq 1)
;        mean[z] = 1.0
;        z = where(mean le 0.0,cnt)
;        if cnt ne 0 then mean[z] = 1.0
;
;    endif else begin
    
        mean = normspecflat(mean,edgecoeffs,xranges,slith_arc,norm_nxg,$
                            norm_nyg,oversamp,norm_ybuffer,RMS=rms,/UPDATE,$
                            WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
        if cancel then return    
    
;    endelse

;  Replace bad pixels with 
    
    z = where(rotate(*state.d.mask,rotation) eq 0,count)
    if count ne 0 then mean[z] = 1.0
    
endif else rms = replicate(0.0,norders)

;  Write the results to disk.

junk = tag_exist(hdrinfo[*].vals,state.r.irafname,INDEX=idx)

history='This flat was created by 1. scaling '+$
  strjoin(hdrinfo[*].vals.(idx), ', ')+' using the median and then 2. '+$
  state.r.flatcombinestat+' combining the resulting images.'

newhdr = hdrinfo[0]
newhdr.vals.(idx) = flatoname+'.fits'

writeflat,unrotate(mean,rotation),newhdr,rotation,fix(orders),edgecoeffs,$
  xranges,slith_pix,slith_arc,slitw_pix,slitw_arc,modename,rms,$
  state.w.version,state.r.calpath+flatoname+'.fits',HISTORY=history

xspextool_message,state,'Wrote flat to '+state.r.calpath+flatoname+'.fits'

ximgtool,state.r.calpath+flatoname+'.fits', ZWINPOS='None',$
         ZRANGE=[0.9,1.1],GROUP_LEADER=state.w.xspextool_base,BUFFER=1

if keyword_set(BEEP) then beep

end
;
;******************************************************************************
;
pro xspextool_mkprofiles,state,CANCEL=cancel,BEEP=beep,DISPLAY=display

cancel = 0

if state.r.pscontinue lt 1 or state.r.xscontinue lt 1 then begin

    ok = dialog_message('Previous steps not complete.',/ERROR,$
                        DIALOG_PARENT=state.w.xspextool_base)
    cancel = 1
    return

endif

;  Get user inputs

start = cfld(state.w.start_fld,3,/EMPTY,CANCEL=cancel)
if cancel then return
start = crange(start,[0,state.r.ncols-1],'Start',/KGE,/KLE,$
               WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
if cancel then return

stop  = cfld(state.w.stop_fld,3,/EMPTY,CANCEL=cancel)
if cancel then return
stop  = crange(stop,[0,state.r.ncols-1],'Stop',/KGE,/KLE,$
               WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
if cancel then return

oversamp = cfld(state.w.oversamp_fld,3,/EMPTY,CANCEL=cancel)
if cancel then return
oversamp = crange(oversamp,[1,10],'Oversamp',/KGE,/KLE,$
                  WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
if cancel then return

ybuffer = cfld(state.w.ybuffer_fld,3,/EMPTY,CANCEL=cancel)
if cancel then return

maskwin = cfld(state.w.maskwindow_fld,3,/EMPTY,CANCEL=cancel)
if cancel then return

xranges      = *state.r.xranges
xranges[0,*] = xranges[0,*] > start
xranges[1,*] = xranges[1,*] < stop

;  Construct the Profiles

xspextool_message,state,'Constructing Super Profiles...'

zero = (state.w.base eq 'Point Source') ? 1:0

coeffs = mkspatcoeffs(*state.d.workimage,*state.r.edgecoeffs,$
                      xranges,state.r.slith_arc,oversamp,ybuffer,$
                      maskwin,SPROFILES=sprofiles,SMAP=smap,ZERO=zero,/UPDATE,$
                      WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
if cancel then return

*state.r.profcoeffs = coeffs
*state.r.profiles = sprofiles

;  Display the order images

if keyword_set(DISPLAY) then ximgtool, smap,/AUTORANGE,BUFFER=1

;  Display the profiles

xplotprofiles,*state.r.profiles,*state.r.orders,intarr(state.r.norders)+1, $
              state.r.slith_arc, $
              NOTIFY=[state.w.plotprofiles,state.w.xspextool_base],$
              GROUP_LEADER=state.w.xspextool_base

state.r.pscontinue = 2
state.r.xscontinue = 2

if keyword_set(BEEP) then beep

end
;
;******************************************************************************
;
pro xspextool_mksky,state,CANCEL=cancel, BEEP=beep

cancel = 0

;  Get file readmode info.

index    = (state.r.filereadmode eq 'Index') ? 1:0
filename = (state.r.filereadmode eq 'Filename') ? 1:0
if index then begin

    prefix = cfld(state.w.inprefix_fld,7,/EMPTY,CANCEL=cancel)
    if cancel then return
    state.r.prefix = strtrim(prefix,2)

endif

;  Get user inputs

images    = cfld(state.w.skyimages_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
images    = fsextract(images,INDEX=index,FILENAME=filename,CANCEL=cancel)
if cancel then return
imagepath = mkfullpath(state.r.datapath,images,INDEX=index,FILENAME=filename,$
                       NI=state.r.nint,PREFIX=prefix,SUFFIX='*.fits',$
                       WIDGET_ID=state.w.xspextool_base,/EXIST,CANCEL=cancel)
if cancel then return

flat = cfld(state.w.skyflat_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

oname = cfld(state.w.skyoname_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

readflat,state.r.calpath+flat,flat,ncols,nrows,modenam,slith_pix,$
  slith_arc,slitw_pix,slitw_arc,norders,orders,edgecoeffs,xranges,rms,rotation

if state.r.lc then restore, $
  filepath('lc_coeff.sav',ROOT_DIR=state.r.packagepath,SUBDIR='data')

xspextool_message,state,'Loading the images...'

readmfits,imagepath,data,hdrinfo,var,KEYWORDS=state.r.keywords,$
  ROTATE=rotation,ITIME=state.r.itime,COADDS=state.r.coadds,$
  NDRS=state.r.ndrs,GAIN=state.r.gain,PAIR=0,READNOISE=state.r.readnoise,$
  LC_COEFF=lc_coeff,SLOWCNT=state.r.slowcnt,WIDGET_ID=state.w.xspextool_base,$
  CANCEL=cancel
if cancel then return

xspextool_message,state,'Scaling the skys...'

scalespecsky,data,var,edgecoeffs,xranges,/UPDATE,$
  WIDGET_ID=state.w.xspextool_base,CANCEL=cancel
if cancel then return

xspextool_message,state,'Combining the skys...'

case state.r.skycombinestat of 
    
    'Mean': meancomb,data,mean,mvar,CANCEL=cancel

    'Median': medcomb,data,mean,mvar,/UPDATE,$
                      WIDGET_ID=state.w.xspextool_base,CANCEL=cancel
    
    'Weighted Mean': meancomb,data,mean,mvar,DATAVAR=var,CANCEL=cancel

    endcase 
    if cancel then return

;scale = (state.r.ximgtool eq 0) float(state.r.itime):1.0
ximgtool,mean,WID=wid,GROUP_LEADER=state.w.xspextool_base,/AUTORANGE, $
         BUFFER=1,ZWINPOS='None'

;  Write the results to disk

junk = tag_exist(hdrinfo[*],state.r.irafname,INDEX=idx)

history='This sky was created by '+state.r.skycombinestat+$
  ' combining the skys, '+strjoin(hdrinfo[*].vals.(idx), ', ')   

if state.r.lc then history = history+'  The raw data was'+$ 
  'first corrected for non-linearity.'

newhdr = avehdrs(hdrinfo,TIME_OBS=state.r.time,POSANGLE=state.r.posangle,$
                 HA=state.r.ha,AIRMASS=state.r.airmass,CANCEL=cancel)

newhdr.vals.(idx) = oname+'.fits'

writecomb,unrotate(mean,rotation),state.r.calpath+oname+'.fits',$
  newhdr,HISTORY=history

;  Write variance image out.

fxhmake,hdr,mvar
fxaddpar,hdr,'HISTORY','This is the variance image for image '+$
  state.r.calpath+oname+'.fits'

writefits,state.r.calpath+oname+'_var.fits',unrotate(mvar,rotation),hdr

xspextool_message,state,$
  [[['Wrote super sky to ']],[[state.r.calpath+oname+'.fits']]]

if keyword_set(BEEP) then beep

end
;
;******************************************************************************
;
pro xspextool_overridesigns,state

if state.r.pscontinue lt 4 then begin

    ok = dialog_message('Previous steps not complete.',/error,$
                        dialog_parent=state.w.xspextool_base)
    cancel = 1
    return

endif

string = cfld(state.w.psapsigns_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
str = strsplit(string,',',/EXTRACT)

apsigns = intarr(state.r.psnaps)
z = where(str eq '+',count)
if count ne 0 then apsigns[z] = 1
z = where(str eq '-',count)
if count ne 0 then apsigns[z] = -1

*state.r.apsign = apsigns

xspextool_message, state, 'Overriding Aperture signs: '+string

end
;
;******************************************************************************
;
pro xspextool_packagepath,state,CANCEL=cancel

widget_control, /HOURGLASS

xspextool_message, state, 'Updating paths and loading the bad pixel mask...' 

;  Check to see if the path file exists.

case !version.os_family of 

    'unix': home = getenv('HOME')
    
    'MacOS': home = filepath('',ROOT_DIR=state.r.packagepath,SUBDIR='data')

    'Windows': home = filepath('',ROOT_DIR=state.r.packagepath,SUBDIR='data')

endcase

file = cfile(filepath('.spextool_paths.txt',ROOT_DIR=home),CANCEL=cancel)
if not cancel then begin

    paths = strarr(3)
    openr, lun, file,/GET_LUN
    readf, lun, paths
    free_lun, lun
    
    widget_control,state.w.datapath_fld[1],SET_VALUE=paths[0]
    widget_control,state.w.calpath_fld[1],SET_VALUE=paths[1]
    widget_control,state.w.procpath_fld[1],SET_VALUE=paths[2]
    
    state.r.datapath = paths[0]
    state.r.calpath  = paths[1]
    state.r.procpath = paths[2]

endif

if state.r.bdpxmask ne 'None' then begin

    bdpxmk = readfits(filepath(state.r.bdpxmask,ROOT_DIR=state.r.packagepath,$
                               SUBDIRECTORY='data'))
    *state.d.mask = byte(bdpxmk)
    
endif else *state.d.mask = intarr(state.r.ncols,state.r.nrows)+1

xspextool_message,state,'Loading complete.'

end
;
;******************************************************************************
;
pro xspextool_plottrace,state,CANCEL=cancel

cancel = 0

start = cfld(state.w.start_fld,3,CANCEL=cancel,/EMPTY)
if cancel then return
start = crange(start,[0,state.r.ncols-1],'Start',/KGE,/KLE,$
               WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
if cancel then return

stop  = cfld(state.w.stop_fld,3,CANCEL=cancel,/EMPTY)
if cancel then return
if cancel then return
stop = crange(stop,[0,state.r.ncols-1],'Stop',/KGE,/KLE,$
               WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
if cancel then return

ximgtool,*state.d.workimage,WID=wid,GROUP_LEADER=state.w.xspextool_base,$
         BUFFER=1,ZWINPOS='None',/NOUPDATE

if state.w.base eq 'Point Source' then begin

    naps = state.r.psnaps
    z = where(*state.r.psdoorders eq 1,norders)
    x = findgen(stop-start+1)+start

    for i = 0, norders-1 do begin
        
        botedge = poly(x,(*state.r.edgecoeffs)[*,0,z[i]])        
        topedge = poly(x,(*state.r.edgecoeffs)[*,1,z[i]])        

        for j = 0, naps-1 do begin

            good = where(botedge gt -0.5 and topedge lt state.r.nrows-0.5)
            
            trace = poly(x[good],(*state.r.tracecoeffs)[*,i*naps+j])
            oplot,x[good],trace,color=3
            
        endfor

    endfor

endif else begin

    z = where(*state.r.xsdoorders eq 1,norders)
    x = findgen(stop-start+1)+start
    for i = 0, norders*state.r.xsnaps-1 do begin
        
        trace = poly(x,(*state.r.tracecoeffs)[*,i])
        oplot,x,trace,color=3
        
    endfor

endelse

end
;
;******************************************************************************
;
pro xspextool_psapfind,state,CANCEL=cancel

cancel = 0

if state.r.pscontinue lt 2 then begin

    ok = dialog_message('Previous steps not complete.',/error,$
                        dialog_parent=state.w.xspextool_base)
    cancel = 1
    return

endif

if state.r.psapfindmode eq 'Manual' then begin

    xspextool_message, state, 'Finding apertures positions...'

    if state.r.psapfindmanmode eq 'Auto' then begin
        
        findpeaks,*state.r.profiles,state.r.psnaps,intarr(state.r.norders)+1,$
          positions,apsign,/AUTO
        nselected = state.r.norders

    endif else begin

        pos = cfld(state.w.psappos_fld,7,CANCEL=cancel)
        if cancel then return

        if pos ne '' then begin

           pos = float( strsplit(temporary(pos),',',/extract) )
           pos = crange(pos,[0,state.r.slith_arc],'Pos',/KGE,/KLE,$
                        WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
           if cancel then setfocus, state.w.psappos_fld
           if cancel then return    
           
           state.r.psnaps = n_elements(pos)
           pos = rebin(pos,state.r.psnaps,state.r.norders)


        endif else begin

           xplotprofiles,GETAPPOS=x
           z = where(finite(x) eq 1,cnt)
           if cnt eq 0 then begin

              cancel = 1
              mess = 'No apertures selected.'
              ok = dialog_message(mess, /ERROR,$
                                  DIALOG_PARENT=state.w.xspextool_base)
              return

           endif

;  Now find the order with the most apertures 

           tmp = x
           tmp[z] = 1.0
           tmp = total(tmp,1,/NAN)
           state.r.psnaps = max(tmp,/NAN)

           pos = fltarr(state.r.psnaps,state.r.norders)
           for i =0,state.r.norders-1 do pos[*,i] = $
              x[sort(x[0:(state.r.psnaps)-1,i]),i]

        endelse

;  Now eliminate any orders that were not selected

        tmp = total(pos,1,/NAN)
        z = where(tmp gt 0,nselected)

        (*state.r.psdoorders)[*] = 0
        (*state.r.psdoorders)[z] = 1
        widget_control, state.w.psorder_bg, $
                        SET_VALUE=reverse(*state.r.psdoorders)

;  Now check to make sure there are an equal number of apertuers in
;  the orders selected

        z = where(finite(pos[*,z]) eq 0,cnt)
        if cnt ne 0 then begin
           
           cancel = 1
           mess = 'Not an equal number of apertures in each order.'
           ok = dialog_message(mess, /ERROR,$
                               DIALOG_PARENT=state.w.xspextool_base)
           return
           
        endif
        
        if state.r.psnaps gt 4 then begin
           
           cancel = 1
           mess = 'Cannot have more than four apertures per order.'
           ok = dialog_message(mess, /ERROR,$
                               DIALOG_PARENT=state.w.xspextool_base)
           return
           
        endif

        widget_control, state.w.psnaps_dl,SET_DROPLIST_SELECT=state.r.psnaps-1

        if state.r.psapfindmanmode eq 'Fix' then $
          findpeaks,*state.r.profiles,state.r.psnaps,$
          intarr(state.r.norders)+1,positions,apsign,FIXED=pos
        
        if state.r.psapfindmanmode eq 'Guess' then $
          findpeaks,*state.r.profiles,state.r.psnaps,$
          intarr(state.r.norders)+1,positions,apsign,GUESS=pos
        
    endelse 

    *state.r.appos = positions
    print, ' '
    print,'Aperture Positions:'
    print, ' '

    for i = 0,state.r.norders-1 do begin
       
       if (*state.r.psdoorders)[i] then print, $
          'Order '+string((*state.r.orders)[i],FORMAT='(i2.2)')+': ', $
          positions[*,i]
       
    endfor
    print, ' '


    avepos = (state.r.norders gt 1) ? $
             total(positions,2,/NAN)/float(nselected):positions

    profiles = *state.r.profiles
    widget_control, state.w.psappos_fld[1], $
                    SET_VALUE=strjoin(strtrim(avepos,2),',')

    xplotprofiles,*state.r.profiles,*state.r.orders,$
      intarr(state.r.norders)+1,state.r.slith_arc,APPOS=positions,$
      GROUP_LEADER=state.w.xspextool_base
    state.r.pscontinue = 3

endif

if state.r.psapfindmode eq 'Import Coeffs' then begin

;  Get user inputs.

    start = cfld(state.w.start_fld,3,CANCEL=cancel,/EMPTY)
    if cancel then return
    start = crange(start,[0,state.r.ncols-1],'Start',/KGE,/KLE,$
                   WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
    if cancel then return

    stop = cfld(state.w.stop_fld,3,CANCEL=cancel,/EMPTY)
    if cancel then return
    stop = crange(stop,[0,state.r.ncols-1],'Stop',/KGE,/KLE,$
                   WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
    if cancel then return

    filename = cfld(state.w.psitracefile_fld,7,CANCEL=cancel,/EMPTY)
    if cancel then setfocus,state.w.psitracefile_fld
    if cancel then return

    step = cfld(state.w.tracestepsize_fld,3,CANCEL=cancel,/EMPTY)
    if cancel then return
    step = crange(step,[1,20],'Step Size',/KGE,/KLE,$
                  WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
    if cancel then return

    filename = cfile(state.r.calpath+filename,$
                     WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
    if cancel then setfocus,state.w.psitracefile_fld
    if cancel then return

    xranges = *state.r.xranges
    xranges[0,*] = xranges[0,*] > start
    xranges[1,*] = xranges[1,*] < stop

;  Read trace file.

    readtrace,filename,mode,orders,naps,fitorder,tracecoeffs
    state.r.psnaps = naps

    *state.r.psdoorders = intarr(state.r.norders)
    match,*state.r.orders,orders,z
    (*state.r.psdoorders)[z] = 1
    widget_control, state.w.psorder_bg, set_value=reverse(*state.r.psdoorders)

    *state.r.tracecoeffs = tracecoeffs
    xspextool_plottrace,state,CANCEL=cancel
    if cancel then return

    z = where(*state.r.psdoorders eq 1,norders)
    positions = tracetoap((*state.r.edgecoeffs)[*,*,z],*state.r.tracecoeffs,$
                          norders,state.r.psnaps,state.r.slith_arc,$
                          xranges[*,z],step,state.r.ncols)


    pos = fltarr(state.r.psnaps,state.r.norders)
    pos[*,z] = positions
    *state.r.appos = pos
    
    findpeaks,*state.r.profiles,state.r.psnaps,*state.r.psdoorders,$
      positions,apsign,FIXED=pos
    *state.r.apsign = fix(apsign)


    string = strarr(state.r.psnaps)
    zz = where(*state.r.apsign eq -1,count)
    if count ne 0 then string[zz] = '-'
    zz = where(*state.r.apsign eq 1,count)
    if count ne 0 then string[zz] = '+'
    
    widget_control, state.w.psapsigns_fld[1],SET_VALUE=strjoin(string,',')
    xspextool_message, state, 'Aperture signs: '+strjoin(string,',')

    xplotprofiles,*state.r.profiles,*state.r.orders,*state.r.psdoorders,$
      state.r.slith_arc,APPOS=pos,GROUP_LEADER=state.w.xspextool_base
    state.r.pscontinue = 4

endif

end
;
;******************************************************************************
;
pro xspextool_psdefineap,state,CANCEL=cancel

cancel = 0

if state.r.pscontinue lt 4 then begin

    ok = dialog_message('Previous steps not complete.',/error,$
                        dialog_parent=state.w.xspextool_base)
    cancel = 1
    return

endif

xspextool_message, state, 'Defining apertures...'

start = cfld(state.w.start_fld,3,CANCEL=cancel,/EMPTY)
if cancel then return
start = crange(start,[0,state.r.ncols-1],'Start',/KGE,/KLE,$
               WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
if cancel then return

stop = cfld(state.w.stop_fld,3,CANCEL=cancel,/EMPTY)
if cancel then return
stop = crange(stop,[0,state.r.ncols-1],'Stop',/KGE,/KLE,$
              WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
if cancel then return

if state.r.optextract then begin
    
    psfradius = cfld(state.w.psfradius_fld,4,/EMPTY,CANCEL=cancel)
    if cancel then return
    psfradius = crange(psfradius,[0,state.r.slith_arc],'PSF Radius',/KGE,$
                       /KLE,WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
    if cancel then return

endif

apradius = cfld(state.w.psapradius_fld,4,CANCEL=cancel,/EMPTY)
if cancel then return

range = (state.r.optextract eq 0) ? state.r.slith_arc:psfradius

apradius = crange(apradius,range,'Ap Radius',$
                  /KLE,WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
if cancel then return

xranges = *state.r.xranges
xranges[0,*] = xranges[0,*] > start
xranges[1,*] = xranges[1,*] < stop

if state.r.psbgsub eq 'On' then begin
    
    bgstart = cfld(state.w.psbgstart_fld,4,/EMPTY,CANCEL=cancel)
    if cancel then return
    low = (state.r.optextract eq 0) ? apradius:psfradius

    bgstart = crange(bgstart,low,'Background Start',/KGE,$
                     WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
    if cancel then setfocus,state.w.psbgstart_fld
    if cancel then return
    
    bgwidth = cfld(state.w.psbgwidth_fld,4,CANCEL=cancel,/EMPTY)
    if cancel then return
    bgwidth = crange(bgwidth,[0,state.r.slith_arc],'Background Width',/KGE,$
                     /KLE,WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
    if cancel then setfocus,state.w.psbgwidth_fld
    if cancel then return

    bgfit = cfld(state.w.psbgfitorder_fld,3,CANCEL=cancel,/EMPTY)
    if cancel then return
    bgfit = crange(bgfit,[0,7],'Background Fit Order',/KGE,/KLE,$
                   WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
    if cancel then setfocus,state.w.xsbgfitorder_fld
    if cancel then return
    
endif

sapradius = fltarr(state.r.psnaps,state.r.norders)

apsigndat = fltarr(state.r.psnaps)

for i = 0, state.r.norders-1 do begin

    profile = (*state.r.profiles).(i)
    if (*state.r.psdoorders)[i] eq 1 then begin

        m  = mkmask_ps(profile[*,0],(*state.r.appos)[*,i],$
                       apradius,BGSTART=bgstart,BGWIDTH=bgwidth,$
                       WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
        if cancel then return
        word = 'mask'+strtrim(i+1,2)
        mask = (i eq 0) ? create_struct(word,[[reform(profile[*,0])],[m]]):$
          create_struct(mask,word,[[reform(profile[*,0])],[m]]) 

;  Determine the aperture signs, not cute, just works

        for j = 0,state.r.psnaps-1 do begin

            apidx = where(m gt float(j) and m le float(j+1),count)
            apdat = profile[apidx,1]

            zpos = where(apdat gt 0,cntpos)
            zneg = where(apdat lt 0,cntneg)

            if cntpos ne 0 then apdat[zpos] = 1
            if cntneg ne 0 then apdat[zneg] = -1
            
            apsigndat[j] = apsigndat[j]+total(apdat)

        endfor

    endif else begin
        
        word = 'mask'+strtrim(i+1,2)
        b    = fltarr(n_elements(profile[*,1]))
        mask = (i eq 0) ? create_struct(word,[[reform(profile[*,0])],[b]]):$
          create_struct(mask,word,[[reform(profile[*,0])],[b]]) 

    endelse
    sapradius[*,i] = replicate(apradius,state.r.psnaps)

endfor

zpos = where(apsigndat gt 0,cntpos)
zneg = where(apsigndat lt 0,cntneg)

if cntpos ne 0 then apsigndat[zpos] = 1
if cntneg ne 0 then apsigndat[zneg] = -1 

*state.r.apsign = fix(apsigndat)

string = strarr(state.r.psnaps)
zz = where(apsigndat eq -1,count)
if count ne 0 then string[zz] = '-'
zz = where(apsigndat eq 1,count)
if count ne 0 then string[zz] = '+'

widget_control, state.w.psapsigns_fld[1],SET_VALUE=strjoin(string,',')
xspextool_message, state, 'Aperture signs: '+strjoin(string,',')


;  Plot on ximgtool.

z = where(*state.r.psdoorders eq 1,norders)
ximgtool, *state.d.workimage, WID=wid,GROUP_LEADER=state.w.xspextool_base,$
  /NOUPDATE,BUFFER=1,ZWINPOS='None'


plotap,(*state.r.edgecoeffs)[*,*,z],xranges[*,z],$
  *state.r.tracecoeffs,norders,state.r.psnaps,state.r.slith_arc,$
  sapradius[*,z],state.r.nrows,wid

;  Plot on xplotprofiles.

xplotprofiles,*state.r.profiles,*state.r.orders,*state.r.psdoorders,$
  state.r.slith_arc,APPOS=*state.r.appos,MASK=mask,$
  GROUP_LEADER=state.w.xspextool_base,BGFIT=bgfit,PSFAP=psfradius

state.r.pscontinue=5

end
;
;******************************************************************************
;
pro xspextool_psextract,state,CANCEL=cacncel,BEEP=beep

cancel = 0

if state.r.pscontinue lt 5 then begin
   
    ok = dialog_message('Previous steps not complete.',/error,$
                        dialog_parent=state.w.xspextool_base)
    cancel = 1
    return
    
endif

;  If filename mode, check to make sure the outname is filled in.

filename = (state.r.filereadmode eq 'Filename') ? 1:0
if filename then file = cfld(state.w.outname_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

if state.r.optextract then begin
    
    psfradius = cfld(state.w.psfradius_fld,4,/EMPTY,CANCEL=cancel)
    if cancel then return
    psfradius = crange(psfradius,[0,state.r.slith_arc],'PSF Radius',/KGE,$
                       /KLE,WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
    if cancel then return

endif

apradius = cfld(state.w.psapradius_fld,4,CANCEL=cancel,/EMPTY)
if cancel then return

range = (state.r.optextract eq 0) ? state.r.slith_arc:psfradius

apradius = crange(apradius,range,'Ap Radius',$
                  /KLE,WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
if cancel then return

if state.r.psbgsub eq 'On' then begin

    if state.r.optextract then begin

        psfradius = cfld(state.w.psfradius_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then return

    endif

    bgstart = cfld(state.w.psbgstart_fld,4,/EMPTY,CANCEL=cancel)
    if cancel then return
    low = (state.r.optextract eq 0) ? apradius:psfradius

    bgstart = crange(bgstart,low,'Background Start',/KGE,$
                     WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
    if cancel then setfocus,state.w.psbgstart_fld
    if cancel then return
    
    bgwidth = cfld(state.w.psbgwidth_fld,4,CANCEL=cancel,/EMPTY)
    if cancel then return
    bgwidth = crange(bgwidth,[0,state.r.slith_arc],'Background Width',/KGE,$
                     /KLE,WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
    if cancel then setfocus,state.w.psbgwidth_fld
    if cancel then return
    
    bgfit = cfld(state.w.psbgfitorder_fld,3,CANCEL=cancel,/EMPTY)
    if cancel then return
    bgfit = crange(bgfit,[0,7],'Background Fit Order',/KGE,/KLE,$
                   WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
    if cancel then setfocus,state.w.psbgfitorder_fld
    if cancel then return

endif
start = cfld(state.w.start_fld,3,CANCEL=cancel,/EMPTY)
if cancel then return
start = crange(start,[0,state.r.ncols-1],'Start',/KGE,/KLE,$
               WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
if cancel then return

stop = cfld(state.w.stop_fld,3,CANCEL=cancel,/EMPTY)
if cancel then return
stop = crange(stop,[0,state.r.ncols-1],'Stop',/KGE,/KLE,$
              WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
if cancel then return

xranges = *state.r.xranges
xranges[0,*] = xranges[0,*] > start
xranges[1,*] = xranges[1,*] < stop

z = where(*state.r.psdoorders eq 1,norders)

workimage = *state.d.workimage
varimage  = *state.d.varimage

;  Check seeing if optimal extraction is running


if state.r.psnaps eq 2 and state.r.optextract and state.r.checkseeing then $
   begin

   result = mc_getseeing(*state.r.profiles,*state.r.orders,$
                         *state.r.psdoorders,*state.r.appos,/GAUSSIAN, $
                         CANCEL=cancel)
   if cancel then return
   print, ' '
   print, 'Profile FWHM (arcseconds):'
   print, ' '
   for i = 0,state.r.norders-1 do begin
      
      if (*state.r.psdoorders)[i] then print, $
         'Order '+string((*state.r.orders)[i],FORMAT='(i2.2)')+': ',result[*,i]
      
   endfor
   print, ' '
   
   thresh = cfld(state.w.seeingthresh_fld,4,/EMPTY,CANCEL=cancel)
   if cancel then return

;  Now check against user input

   tmp = result/(state.r.slith_arc/float(state.r.slith_pix))
   junk = where(tmp lt thresh,cnt)
   if cnt ne 0 then begin

      message = [[string(cnt,FORMAT='(i2.2)')+' of the apertures ' + $
                  'have FWHM value that may be too'],$
                 ['narrow for optimal extraction to work properly.']]
      junk = dialog_message(message,DIALOG_PARENT=state.w.xspextool_base, $
                            /INFORMATION)
      
   endif

endif

;  Now do extraction 

xspextool_message,state,'Extracting spectra...'

if state.r.optextract or state.r.fixbdpx then begin

    idx=0
    for i = 0,state.r.norders-1 do begin
        
        if (*state.r.psdoorders)[i] eq 1 then begin
            
            key = 'Order'+string(i,format='(i2.2)')
            scoeff= (idx eq 0) ? create_struct(key,(*state.r.profcoeffs).(i)):$
              create_struct(scoeff,key,(*state.r.profcoeffs).(i))
            idx = idx + 1
            
        endif
        
    endfor

    bdpxmk = *state.d.bdpxmask

    bdpxthresh = cfld(state.w.bdpxthresh_fld,4,/EMPTY,CANCEL=cancel)
    if cancel then return

endif

if state.r.e2d then begin

    spectra = extractspec2d_ps(workimage,varimage,$
                               (*state.r.edgecoeffs)[*,*,z],$
                               *state.r.tracecoeffs,norders,state.r.psnaps,$
                               start,stop,xranges[*,z],state.r.slith_arc,$
                               state.r.slith_pix,apradius,*state.r.apsign,$
                               SPATCOEFFS=scoeff,BGSTART=bgstart,$
                               BGWIDTH=bgwidth,BGORDER=bgfit,$
                               BDPXMK=bdpxmk,BDPIXTHRESH=bdpxthresh,$
                               /UPDATE,WIDGET_ID=state.w.xspextool_base,$
                               CANCEL=cancel)

    s = size(spectra)
    state.r.aph_arc = s[2]*state.r.slith_arc/state.r.slith_pix

endif else begin

    spectra = extractspec_ps(workimage,varimage,(*state.r.edgecoeffs)[*,*,z],$
                             *state.r.tracecoeffs,norders,state.r.psnaps,$
                             start,stop,xranges[*,z],state.r.slith_arc,$
                             apradius,*state.r.apsign,SPATCOEFFS=scoeff,$
                             BGSTART=bgstart,BGWIDTH=bgwidth,BGORDER=bgfit,$
                             PSFWIDTH=psfradius,BDPXMK=bdpxmk,$
                             BDPIXTHRESH=bdpxthresh,BGSUBIMG=bgsubimg,$
                             ERRORPROP=state.r.errorprop,/UPDATE,$
                             WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)

endelse
if cancel then return

ximgtool, bgsubimg, WID=wid,GROUP_LEADER=state.w.xspextool_base,$
          /NOUPDATE,BUFFER=1,ZWINPOS='None'

*state.d.spectra = spectra
state.r.pscontinue=6

xspextool_wavecal,state,CANCEL=cancel
if cancel then return

xspextool_writespec,state

if keyword_set(BEEP) then beep

end
;
;******************************************************************************
;
pro xspextool_pstrace, state, CANCEL=cancel, BEEP=beep

cancel = 0

if state.r.pscontinue lt 3 then begin

    ok = dialog_message('Previous steps not complete.',/error,$
                        dialog_parent=state.w.xspextool_base)
    cancel = 1
    return

endif

;  Get user inputs

start = cfld(state.w.start_fld,3,CANCEL=cancel,/EMPTY)
if cancel then return
start = crange(start,[0,state.r.ncols-1],'Start',/KGE,/KLE,$
               WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
if cancel then return

stop = cfld(state.w.stop_fld,3,CANCEL=cancel,/EMPTY)
if cancel then return
stop = crange(stop,[0,state.r.ncols-1],'Stop',/KGE,/KLE,$
               WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
if cancel then return

step = cfld(state.w.tracestepsize_fld,3,CANCEL=cancel,/EMPTY)
if cancel then return
step = crange(step,[1,20],'Step Size',/KGE,/KLE,$
               WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
if cancel then return

sumap = cfld(state.w.sumap_fld,3,CANCEL=cancel,/EMPTY)
if cancel then return
sumap = crange(sumap,[1,2*sumap-1],'Colums Add',/KGE,/KLE,/ODD,$
               WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
if cancel then return

fitorder = cfld(state.w.polyfitorder_fld,3,CANCEL=cancel,/EMPTY)
if cancel then return
fitorder = crange(fitorder,[0,8],'Poly Fit Order',/KGE,/KLE,$
               WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
if cancel then return

xranges = *state.r.xranges
xranges[0,*] = xranges[0,*] > start
xranges[1,*] = xranges[1,*] < stop

xspextool_message, state, 'Tracing objects...'

ximgtool,*state.d.workimage, WID=wid,GROUP_LEADER=state.w.xspextool_base,$
  /NOUPDATE,BUFFER=1,ZWINPOS='None'

z = where(*state.r.psdoorders eq 1,norders)

;  Get apsign

;findpeaks,*state.r.profiles,state.r.psnaps,*state.r.psdoorders,$
;  pos,apsign,FIXED=*state.r.appos
;*state.r.apsign = fix(apsign)

;string = strarr(state.r.psnaps)
;zz = where(apsign eq -1,count)
;if count ne 0 then string[zz] = '-'
;zz = where(apsign eq 1,count)
;if count ne 0 then string[zz] = '+'

;widget_control, state.w.psapsigns_fld[1],SET_VALUE=strjoin(string,',')
;xspextool_message, state, 'Aperture signs: '+strjoin(string,',')

if state.r.psapfindmanmode eq 'Fix' then begin

    *state.r.tracecoeffs = aptotrace((*state.r.edgecoeffs)[*,*,z],$
                                     state.r.slith_arc,*state.r.appos,$
                                     xranges[*,z],step,fitorder,$
                                     state.r.nrows,WID=wid,CANCEL=cancel)
    if cancel then return


endif else begin

    thresh = cfld(state.w.tracethresh_fld,4,/EMPTY,CANCEL=cancel)
    if cancel then return

    *state.r.tracecoeffs = tracespec(*state.d.workimage,*state.d.varimage,$
                                     (*state.r.edgecoeffs)[*,*,z],norders,$
                                     state.r.psnaps,state.r.slith_arc,$
                                     (*state.r.appos)[*,z],xranges[*,z],step,$
                                     sumap,thresh,fitorder,WID=wid, $
                                     CANCEL=cancel)
    if cancel then return

endelse

state.r.pscontinue=4

if keyword_set(BEEP) then beep

end
;
;******************************************************************************
;
pro xspextool_updateappos,event,state

case state.w.base of 

    'Point Source': widget_control, state.w.psappos_fld[1],SET_VALUE=''

    'Extended Source': widget_control, state.w.xsappos_fld[1],SET_VALUE=''

endcase

state.r.pscontinue = 2
state.r.xscontinue = 2

end
;
;******************************************************************************
;
pro xspextool_wavecal,state,CANCEL=cancel

cancel = 0

case state.w.base of 

    'Extended Source': begin

        doorders = *state.r.xsdoorders
        naps     = state.r.xsnaps
        apradius = cfld(state.w.xsapradius_fld,7,/EMPTY,CANCEL=cancel)
        apradius = float( strsplit(temporary(apradius),',',/EXTRACT) )
        good     = where(*state.r.xsdoorders eq 1,norders)

    end

    'Point Source': begin

        doorders = *state.r.psdoorders
        naps     = state.r.psnaps
        apradius = cfld(state.w.psapradius_fld,4,/EMPTY,CANCEL=cancel)
        apsign   = replicate(1,n_elements(*state.r.apsign))
        good     = where(*state.r.psdoorders eq 1,norders)

    end

endcase

;  Get user inputs.

start = cfld(state.w.start_fld,3,/EMPTY,CANCEL=cancel)
stop  = cfld(state.w.stop_fld,3,CANCEL=cancel,/EMPTY)
x     = findgen(stop-start+1)+start


if not state.r.wavecal then begin

    z = where(doorders eq 1,count)
    nspec   = naps*count

    s = size(*state.d.spectra)
    nspat = s[2]/2

    spectra = fltarr(stop-start+1,s[2]+1,nspec)
    disp    = fltarr(nspec)

    disp    = 0.
    rms = replicate(0.0,naps)
    for i = 0,nspec-1 do begin
        
        spectra[*,0,i] = x
        spectra[*,1:nspat,i]= (*state.d.spectra)[*,0:(nspat-1),i]
        spectra[*,(nspat+1):*,i] = sqrt((*state.d.spectra)[*,nspat:*,i])
        if state.r.errorprop eq 0 then spectra[*,(nspat+1):*,i] = 1.

    endfor
    


endif else begin

    s = size(*state.d.spectra)
    nspat = s[2]/2

    spectra = fltarr(stop-start+1,s[2]+1,naps*norders)
    disp    = fltarr(naps*norders)

    for i = 0, naps-1 do begin
        
        for j = 0, norders-1 do begin
                        
            spectra[*,0,i+j*naps] = (*state.d.wavecal)[start:stop,0,good[j]]
            
            spectra[*,1:nspat,i+j*naps]= $
              (*state.d.spectra)[*,0:(nspat-1),i+j*naps]
            spectra[*,(nspat+1):*,i+j*naps] = $
              sqrt((*state.d.spectra)[*,nspat:*,i+j*naps])
            if state.r.errorprop eq 0 then spectra[*,(nspat+1):*,i+j*naps] = 1.
            
            coeff = poly_fit1d(x,spectra[*,0,i+j*naps],1,/SILENT)
            disp[i+j*naps] = coeff[1]
            
        endfor
        
    endfor    

endelse

;*state.r.rms     = rms
*state.d.spectra = spectra
*state.d.disp    = disp

if state.w.base eq 'Point Source' then state.r.pscontinue=7
if state.w.base eq 'Extended Source' then state.r.xscontinue=7


end
;
;******************************************************************************
;
pro xspextool_writespec,state,CANCEL=cancel

cancel = 0

if state.w.base eq 'Point Source' and state.r.pscontinue lt 7 then begin

    ok = dialog_message('Previous steps not complete.',/error,$
                        dialog_parent=state.w.xspextool_base)
   cancel = 1
    return

endif

if state.w.base eq 'Extended Source' and state.r.xscontinue lt 7 then begin

    ok = dialog_message('Previous steps not complete.',/error,$
                        dialog_parent=state.w.xspextool_base)
   cancel = 1
    return

endif

;  Get user inputs.

start = cfld(state.w.start_fld,3,CANCEL=cancel,/EMPTY)
if cancel then return
start = crange(start,[0,state.r.ncols-1],'Start',/KGE,/KLE,$
               WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
if cancel then return

stop = cfld(state.w.stop_fld,3,CANCEL=cancel,/EMPTY)
if cancel then return
stop = crange(stop,[0,state.r.ncols-1],'Stop',/KGE,/KLE,$
               WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
if cancel then return

if state.w.base eq 'Point Source' then begin

    apradius = cfld(state.w.psapradius_fld,4,CANCEL=cancel,/EMPTY)
    if cancel then return
    apradius = crange(apradius,[0,state.r.slith_arc],'Aperture Radius',/KGE,$
                      /KLE,WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
    if cancel then setfocus,state.w.psapradius_fld
    if cancel then return

    if state.r.psbgsub eq 'On' then begin
    
        bgstart = cfld(state.w.psbgstart_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then return
        bgstart = crange(bgstart,apradius,'Background Start',/KGT,$
                         WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
        if cancel then setfocus,state.w.psbgstart_fld
        if cancel then return
        
        bgwidth = cfld(state.w.psbgwidth_fld,4,CANCEL=cancel,/EMPTY)
        if cancel then return
        bgwidth = crange(bgwidth,[0,state.r.slith_arc],'Background Width',$
                         /KGE,$
                         /KLE,WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
        if cancel then setfocus,state.w.psbgwidth_fld
        if cancel then return
        
        bgorder = cfld(state.w.psbgfitorder_fld,4,CANCEL=cancel,/EMPTY)
        if cancel then return
        bgorder = crange(bgorder,[0,7],'Background Fit Order',/KGE,/KLE,$
                         WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
        if cancel then setfocus,state.w.psbgfitorder_fld
        if cancel then return
        
    endif

    if state.r.optextract then begin

        psfradius = cfld(state.w.psfradius_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then return

    endif

    good = where(*state.r.psdoorders eq 1,norders)

    if state.r.psapfindmode eq 'Import Coeffs' then begin
        
        tracefile = cfld(state.w.psitracefile_fld,7,CANCEL=cancel,/EMPTY)
        if cancel then return
        junk = cfile(state.r.calpath+tracefile,$
                     WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
        if cancel then return
        
    endif
    naps = state.r.psnaps

endif
if state.w.base eq 'Extended Source' then begin

    appos = (*state.r.appos)[*,0]

    apradius = cfld(state.w.xsapradius_fld,7,CANCEL=cancel,/EMPTY)
    if cancel then return
    apradius = float( strsplit(temporary(apradius),',',/extract) )
    if n_elements(apradius) ne state.r.xsnaps then begin
        
        cancel = 1
        mess = 'Number of Apradii must equal number of Apertures.'
        ok = dialog_message(mess,/error,dialog_parent=state.w.xspextool_base)
        setfocus,state.w.xsapradius_fld
        return
        
        
    endif
    apradius = crange(apradius,[0,state.r.slith_arc],'Apradius',/KGE,/KLE,$
                      WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
    if cancel then setfocus,state.w.xsapradius_fld
    if cancel then return
    
    if state.r.xsbgsub eq 'On' then begin

        bgr  = cfld(state.w.xsbg_fld,7,CANCEL=cancel,/EMPTY)
        if cancel then return
        bgfit = cfld(state.w.xsbgfitorder_fld,3,CANCEL=cancel,/EMPTY)
        if cancel then return
        bgfit = crange(bgfit,[0,7],'Background Fit Order',/KGE,/KLE,$
                       WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
        if cancel then setfocus,state.w.xsbgfitorder_fld
        if cancel then return
        
    endif

    good = where(*state.r.xsdoorders eq 1,norders)

    if state.r.xsapfindmode eq 'Import Coeffs' then begin
        
        tracefile = cfld(state.w.xsitracefile_fld,7,CANCEL=cancel,/EMPTY)
        if cancel then return
        junk = cfile(state.r.calpath+tracefile,$
                     WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
        if cancel then return
        
    endif
    naps = state.r.xsnaps

endif

disp = (state.r.wavecal eq 1) ? *state.d.disp:intarr(norders*naps)

;  Get file read mode.

index    = (state.r.filereadmode eq 'Index') ? 1:0
filename = (state.r.filereadmode eq 'Filename') ? 1:0

;  Get outpaths.

if index then begin

    oprefix = cfld(state.w.outprefix_fld,7,/EMPTY,CANCEL=cancel)
    if cancel then return
    files = *state.r.workfiles
    opaths = mkfullpath(state.r.procpath,files,INDEX=index,$
                        FILENAME=filename,NI=state.r.nint,PREFIX=oprefix,$
                        WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)

endif
if filename then begin

    file = cfld(state.w.outname_fld,7,/EMPTY,CANCEL=cancel)
    if cancel then return
    opaths = state.r.procpath+file

endif

text = (state.r.textoutput eq 1) ? 1:0
fits = (state.r.fitsoutput eq 1) ? 1:0

;  Get orders, flat name and arc name.

orders  = (*state.r.orders)[good]
appos = (*state.r.appos)[*,good]

flat = cfld(state.w.flatfield_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

if state.r.wavecal then begin

    wavecal = cfld(state.w.wavecal_fld,7,CANCEL=cancel,/EMPTY)
    if cancel then return
;    rms = *state.r.rms

;  Read resolution

    if state.r.obsmode eq 'ShortXD' then begin

        root = (state.r.juldate gt 2451758) ? 'ShortXD_g2':'ShortXD_g1'
        file = filepath(root+'.fits',ROOT_DIR=state.r.packagepath,$
                        SUBDIR='data')

    endif else file = filepath(state.r.obsmode+'.fits',$
                               ROOT_DIR=state.r.packagepath,SUBDIR='data')

   rdwavecal,file,aa,bb,cc,dd,ee,ff,gg,resppix,CANCEL=cancel
   res = resppix/state.r.slitw_pix

    
endif else begin

    rms = intarr(naps)

endelse 

yunits='DN / s'
xunits= (state.r.wavecal eq 1) ? 'um':'pixels'

xtitle=(state.r.wavecal eq 1) ? '!7k!5 (!7l!5m)':'!7k!5 (pixels)' 
ytitle='f (!5DN s!u-1!N)'

if state.r.reductionmode eq 'A' then begin

    xspextool_message,state,[[['Writing spectra to file']],[[opaths[0]]]]

    sky                = 'None'
    hdrinfo            = (*state.r.hdrinfo)[0] 
    junk               = tag_exist(hdrinfo.vals,state.r.irafname,INDEX=idx)
    aimage             = hdrinfo.vals.(idx)
    hdrinfo.vals.(idx) = strmid(opaths[0],strpos(opaths[0],state.w.sep,$
                                                 /REVERSE_S)+1)+'.fits'
    
    case state.w.base of 

        'Point Source': writespec_ps,*state.d.spectra,opaths[0],$
          aimage,sky,flat,naps,orders,start,stop,hdrinfo,appos,apradius,$
          state.r.modename,state.r.slith_pix,state.r.slith_arc,$
          state.r.slitw_pix,state.r.slitw_arc,xunits,yunits,xtitle,ytitle,$
          state.w.version,PSFRADIUS=psfradius,BGSTART=bgstart,$
          BGWIDTH=bgwidth,BGORDER=bgorder,TRACEFILE=tracefile,$
          WAVECAL=wavecal,DISP=disp,TEXT=text,FITS=fits,RES=res,$
          LINEARITY=state.r.lc,EXT2D=state.r.e2d

        'Extended Source': writespec_xs,*state.d.spectra,opaths[0],aimage, $
          sky,flat,naps,orders,start,stop,hdrinfo,appos,apradius, $
          state.r.modename,state.r.slith_pix,state.r.slith_arc,$
          state.r.slitw_pix,state.r.slitw_arc,xunits,yunits,xtitle,ytitle, $
          state.w.version,BGORDER=bgorder,BGR=bgr,TRACEFILE=tracefile, $
          WAVECAL=wavecal,DISP=disp,TEXT=text,FITS=fits,RES=res, $
          LINEARITY=state.r.lc,EXT2D=state.r.e2d
        
    endcase
    
    xvspec,opaths[0]+'.fits'
    
endif

if state.r.reductionmode eq 'A-B' then begin

    z = where( (*state.r.apsign) lt 0,count)
    if count eq 0 then begin                             ;  A-Sky 
        
        xspextool_message,state,[[['Writing spectra to file']],$
                                 [[opaths[0]+'.fits']]]

        hdrinfo = (*state.r.hdrinfo)[0] 
        junk    = tag_exist(hdrinfo.vals,state.r.irafname,INDEX=idx)
        aimage  = hdrinfo.vals.(idx)
        sky     = (*state.r.hdrinfo)[1].vals.(idx)
        
        hdrinfo.vals.(idx) = strmid(opaths[0],strpos(opaths[0],state.w.sep,$
                                                     /REVERSE_S)+1)+'.fits'
        case state.w.base of 
            
            'Point Source': writespec_ps,*state.d.spectra,opaths[0],aimage,$
              sky,flat,naps,orders,start,stop,hdrinfo,appos,apradius,$
              state.r.modename,state.r.slith_pix,state.r.slith_arc,$
              state.r.slitw_pix,state.r.slitw_arc,xunits,yunits,xtitle, $
              ytitle,state.w.version,PSFRADIUS=psfradius, $
              BGSTART=bgstart,BGWIDTH=bgwidth,BGORDER=bgorder, $
              TRACEFILE=tracefile,WAVECAL=wavecal,DISP=disp,TEXT=text, $
              FITS=fits,RES=res,LINEARITY=state.r.lc,EXT2D=state.r.e2d
            
            'Extended Source': writespec_xs,*state.d.spectra,opaths[0], $
              aimage,sky,flat,naps,orders,start,stop,hdrinfo,appos,apradius, $
              state.r.modename,state.r.slith_pix,state.r.slith_arc, $
              state.r.slitw_pix,state.r.slitw_arc,xunits,yunits,xtitle, $
              ytitle,state.w.version,BGR=bgr,BGORDER=bgorder, $
              TRACEFILE=tracefile,WAVECAL=wavecal,DISP=disp,TEXT=text, $
              FITS=fits,RES=res,LINEARITY=state.r.lc,EXT2D=state.r.e2d
            
        endcase

        xvspec,opaths[0]+'.fits'

    endif else begin                                 ; A-B(object) Must be PS

;  Do positive apertures.       

        xspextool_message,state,[[['Writing spectra to file']],$
                                 [[opaths[0]+'.fits']]]

        hdrinfo            = (*state.r.hdrinfo)[0] 
        junk               = tag_exist(hdrinfo.vals,state.r.irafname,INDEX=idx)
        aimage             = hdrinfo.vals.(idx)
        sky                = (*state.r.hdrinfo)[1].vals.(idx)
        hdrinfo.vals.(idx) = strmid(opaths[0],strpos(opaths[0],state.w.sep,$
                                                     /REVERSE_S)+1)+'.fits'

        pos     = where(*state.r.apsign eq 1,naps)
        apsign  = replicas(*state.r.apsign,norders)
        z       = where(apsign eq 1)
        
        writespec_ps,(*state.d.spectra)[*,*,z],opaths[0],aimage,sky,flat, $
                     naps,orders,start,stop,hdrinfo,appos[pos,*],apradius,$
                     state.r.modename,state.r.slith_pix,state.r.slith_arc,$
                     state.r.slitw_pix,state.r.slitw_arc,xunits,yunits, $
                     xtitle,ytitle,state.w.version,PSFRADIUS=psfradius, $
                     BGSTART=bgstart,BGWIDTH=bgwidth,BGORDER=bgorder, $
                     TRACEFILE=tracefile,WAVECAL=wavecal,DISP=disp[z], $
                     TEXT=text,FITS=fits,RES=res,LINEARITY=state.r.lc, $
                     EXT2D=state.r.e2d
        
        xvspec,opaths[0]+'.fits'


;  Do negative apertures.

        xspextool_message,state,[[['Writing spectra to file']],$
                                 [[opaths[1]+'.fits']]]

        hdrinfo            = (*state.r.hdrinfo)[1] 
        junk               = tag_exist(hdrinfo.vals,state.r.irafname,INDEX=idx)
        aimage             = hdrinfo.vals.(idx)
        sky                = (*state.r.hdrinfo)[0].vals.(idx)
        hdrinfo.vals.(idx) = strmid(opaths[1],strpos(opaths[1],state.w.sep,$
                                                     /REVERSE_S)+1)+'.fits'

        neg       = where((*state.r.apsign) eq -1,naps)
        apsign  = replicas(*state.r.apsign,norders)
        z       = where(apsign eq -1)

        writespec_ps,(*state.d.spectra)[*,*,z],opaths[1],aimage,sky,flat, $
                     naps,orders,start,stop,hdrinfo,appos[neg,*],apradius,$
                     state.r.modename,state.r.slith_pix,state.r.slith_arc,$
                     state.r.slitw_pix,state.r.slitw_arc,xunits,yunits, $
                     xtitle,ytitle,state.w.version,PSFRADIUS=psfradius, $
                     BGSTART=bgstart,BGWIDTH=bgwidth,BGORDER=bgorder,$
                     TRACEFILE=tracefile,WAVECAL=wavecal,DISP=disp[z], $
                     TEXT=text,FITS=fits,RES=res,LINEARITY=state.r.lc, $
                     EXT2D=state.r.e2d

    endelse

endif

if state.r.reductionmode eq 'A-Sky' then begin

    sky                = cfld(state.w.skyimage_fld,7,/EMPTY,CANCEL=cancel)
    hdrinfo            = (*state.r.hdrinfo)[0] 
    junk               = tag_exist(hdrinfo.vals,state.r.irafname,INDEX=idx)
    aimage             = hdrinfo.vals.(idx)
    hdrinfo.vals.(idx) = strmid(opaths[0],strpos(opaths[0],state.w.sep,$
                                                 /REVERSE_S)+1)+'.fits'

    xspextool_message,state,[[['Writing spectra to file']],$
                             [[opaths[0]+'.fits']]]

    case state.w.base of 

        'Point Source': writespec_ps,*state.d.spectra,opaths[0],aimage,sky, $
          flat,naps,orders,start,stop,hdrinfo,appos,apradius,state.r.modename,$
          state.r.slith_pix,state.r.slith_arc,state.r.slitw_pix, $
          state.r.slitw_arc,xunits,yunits,xtitle,ytitle,state.w.version, $
          PSFRADIUS=psfradius,BGSTART=bgstart,BGWIDTH=bgwidth,$
          BGORDER=bgorder,TRACEFILE=tracefile,WAVECAL=wavecal,DISP=disp, $
          TEXT=text,FITS=fits,RES=res,LINEARITY=state.r.lc,EXT2D=state.r.e2d

        'Extended Source': writespec_xs,*state.d.spectra,opaths[0],aimage, $
          sky,flat,naps,orders,start,stop,hdrinfo,appos,apradius, $
          state.r.modename,state.r.slith_pix,state.r.slith_arc, $
          state.r.slitw_pix,state.r.slitw_arc,xunits,yunits,xtitle,ytitle, $
          state.w.version,BGORDER=bgorder,BGR=bgr,TRACEFILE=tracefile, $
          WAVECAL=wavecal,DISP=disp,TEXT=text,FITS=fits,RES=res, $
          LINEARITY=state.r.lc,EXT2D=state.r.e2d
        
    endcase
    
    xvspec,opaths[0]+'.fits'
    
endif

end
;
;******************************************************************************
;
pro xspextool_writetrace,state,CANCEL=cancel

cancel = 0

if state.w.base eq 'Point Source' and state.r.pscontinue lt 4 then begin

    ok = dialog_message('Previous steps not complete.',/error,$
                        dialog_parent=state.w.xspextool_base)
    cancel = 1
    return

endif
if state.w.base eq 'Extended Source' and state.r.xscontinue lt 4 then begin

    ok = dialog_message('Previous steps not complete.',/error,$
                        dialog_parent=state.w.xspextool_base)
    cancel = 1
    return

endif

if state.w.base eq 'Point Source' then begin

    filename   = cfld(state.w.psotracefile_fld,7,/EMPTY,CANCEL=cancel)
    if cancel then return
    z          = where(*state.r.psdoorders eq 1, gnorders)
    goodorders = (*state.r.orders)[z]
    naps       = state.r.psnaps

endif
if state.w.base eq 'Extended Source' then begin

    filename   = cfld(state.w.xsotracefile_fld,7,/EMPTY,CANCEL=cancel)
    if cancel then return
    z          = where(*state.r.xsdoorders eq 1, gnorders)
    goodorders = (*state.r.orders)[z]
    naps       = state.r.xsnaps

endif
fitorder = cfld(state.w.polyfitorder_fld,3,CANCEL=cancel,/EMPTY)
if cancel then return
fitorder = crange(fitorder,[0,8],'Poly Fit Order',/KGE,/KLE,$
               WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
if cancel then return

fullpath = state.r.calpath+filename

xspextool_message,state, [[['Writing trace information to']],[[fullpath]]]

openw, lun, fullpath,/get_lun

printf, lun, 'OBSMODE='+strtrim(state.r.obsmode,2)
printf, lun, 'ORDERS='+strjoin(strtrim(goodorders,2),',')
printf, lun, 'NAPS='+strtrim(naps,2)

for i = 0, gnorders-1 do begin

    idx = i*naps
    for j = 0, naps-1 do begin

        printf, lun, 'ORDER'+string(goodorders[i],format='(i2.2)')+$
          '_AP'+string(j+1,format='(i2.2)')+'= '+$
          strjoin( strtrim( (*state.r.tracecoeffs)[*,idx],2),'  ' )
        idx = idx + 1

    endfor

endfor


;for i = 0, gnorders*naps-1 do $
;  printf, lun, (*state.r.tracecoeffs)[*,i]
    
close, lun
free_lun, lun

end
;
;******************************************************************************
;
pro xspextool_xsapfind,state,CANCEL=cancel

cancel = 0

if state.r.xscontinue lt 2 then begin

    ok = dialog_message('Previous steps not complete.',/error,$
                        dialog_parent=state.w.xspextool_base)
    cancel = 1
    return

endif

if state.r.xsapfindmode eq 'Manual' then begin

    xspextool_message, state, 'Finding aperture positions...'
    
    pos = cfld(state.w.xsappos_fld,7,CANCEL=cancel)
    if cancel then return

    if pos ne '' then begin
       
       pos = float( strsplit(temporary(pos),',',/extract) )
       pos = crange(pos,[0,state.r.slith_arc],'Pos',/KGE,/KLE,$
                    WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
       if cancel then setfocus, state.w.xsappos_fld
       if cancel then return    
       
       state.r.xsnaps = n_elements(pos)
       pos = rebin(pos,state.r.xsnaps,state.r.norders)
            
    endif else begin

       xplotprofiles,GETAPPOS=x
       z = where(finite(x) eq 1,cnt)
       if cnt eq 0 then begin
          
          cancel = 1
          mess = 'No apertures selected.'
          ok = dialog_message(mess, /ERROR,$
                              DIALOG_PARENT=state.w.xspextool_base)
          return
          
       endif
       
;  Now find the order with the most apertures 
       
       tmp = x
       tmp[z] = 1.0
       tmp = total(tmp,1,/NAN)
       state.r.xsnaps = max(tmp,/NAN)
       
       pos = fltarr(state.r.xsnaps,state.r.norders)
       for i =0,state.r.norders-1 do pos[*,i] = $
          x[sort(x[0:(state.r.xsnaps)-1,i]),i]
       
    endelse
    
;  Now eliminate any orders that were not selected
    
    tmp = total(pos,1,/NAN)
    z = where(tmp gt 0,cnt)
    
    (*state.r.xsdoorders)[*] = 0
    (*state.r.xsdoorders)[z] = 1
    widget_control, state.w.xsorder_bg, $
                    SET_VALUE=reverse(*state.r.xsdoorders)
    
;  Now check to make sure there are an equal number of apertuers in
;  the orders selected
    
    z = where(finite(pos[*,z]) eq 0,cnt)
    if cnt ne 0 then begin
       
       cancel = 1
       mess = 'Not an equal number of apertures in each order.'
       ok = dialog_message(mess, /ERROR,$
                           DIALOG_PARENT=state.w.xspextool_base)
       return
       
    endif
    
    *state.r.apsign = intarr(state.r.xsnaps)+1
    
    *state.r.appos = pos
    print, ' '
    print,'Aperture Positions:'
    print, ' '

    for i = 0,state.r.norders-1 do begin
       
       if (*state.r.xsdoorders)[i] then print, $
          'Order '+string((*state.r.orders)[i],FORMAT='(i2.2)')+': ', $
          pos[*,i]
       
    endfor
    print, ' '

    widget_control, state.w.xsappos_fld[1], $
                    SET_VALUE=strjoin(strtrim(pos[*,0],2),',')

    xplotprofiles,*state.r.profiles,*state.r.orders,$
    intarr(state.r.norders)+1,state.r.slith_arc,APPOS=pos,$
    GROUP_LEADER=state.w.xspextool_base
    state.r.xscontinue = 3

endif
if state.r.xsapfindmode eq 'Import Coeffs' then begin

;  Get user inputs.

    start = cfld(state.w.start_fld,3,CANCEL=cancel,/EMPTY)
    if cancel then return
    start = crange(start,[0,state.r.ncols-1],'Start',/KGE,/KLE,$
                   WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
    if cancel then return

    stop = cfld(state.w.stop_fld,3,CANCEL=cancel,/EMPTY)
    if cancel then return
    stop = crange(stop,[0,state.r.ncols-1],'Stop',/KGE,/KLE,$
                   WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
    if cancel then return

    filename = cfld(state.w.xsitracefile_fld,7,CANCEL=cancel,/EMPTY)
    if cancel then setfocus,state.w.xsitracefile_fld
    if cancel then return

    step = cfld(state.w.tracestepsize_fld,3,CANCEL=cancel,/EMPTY)
    if cancel then return
    step = crange(step,[1,20],'Step Size',/KGE,/KLE,$
                  WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
    if cancel then return

    filename = cfile(state.r.calpath+filename,$
                     WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
    if cancel then setfocus,state.w.psitracefile_fld
    if cancel then return

    xranges = *state.r.xranges
    xranges[0,*] = xranges[0,*] > start
    xranges[1,*] = xranges[1,*] < stop

;  Read trace file.

    readtrace,filename,mode,orders,naps,fitorder,tracecoeffs
    state.r.xsnaps = naps

    *state.r.xsdoorders = intarr(state.r.norders)
    match,*state.r.orders,orders,z
    (*state.r.xsdoorders)[z] = 1
    widget_control, state.w.xsorder_bg, set_value=reverse(*state.r.xsdoorders)

    *state.r.tracecoeffs = tracecoeffs
    xspextool_plottrace,state,CANCEL=cancel
    if cancel then return

    z = where(*state.r.xsdoorders eq 1,norders)
    positions = tracetoap((*state.r.edgecoeffs)[*,*,z],*state.r.tracecoeffs,$
                          norders,state.r.xsnaps,state.r.slith_arc,$
                          xranges[*,z],step,state.r.ncols)

;****************FIX THIS LATER

    pos = fltarr(state.r.xsnaps,state.r.norders)
    pos[*,z] = positions
    *state.r.appos = pos

    for i = 0,state.r.norders-1 do begin
       
       if (*state.r.xsdoorders)[i] then print, $
          'Order '+string((*state.r.orders)[i],FORMAT='(i2.2)')+': ', $
          pos[*,i]
       
   endfor

;    findpeaks,*state.r.profiles,state.r.xsnaps,positions,apsign,GUESS=pos[*,0]
    *state.r.apsign = intarr(state.r.xsnaps)+1

;****************

    xplotprofiles,*state.r.profiles,*state.r.orders,$
      *state.r.xsdoorders,state.r.slith_arc,APPOS=pos,$
      GROUP_LEADER=state.w.xspextool_base
    state.r.xscontinue = 4

endif

end
;
;******************************************************************************
;
pro xspextool_xsdefineap,state,CANCEL=cancel

cancel = 0

if state.r.xscontinue lt 4 then begin

    ok = dialog_message('Previous steps not complete.',/ERROR,$
                        DIALOG_PARENT=state.w.xspextool_base)
    cancel = 1
    return

endif

xspextool_message, state, 'Defining apertures...'

;  Get user inputs.

start = cfld(state.w.start_fld,3,CANCEL=cancel,/EMPTY)
if cancel then return
start = crange(start,[0,state.r.ncols-1],'Start',/KGE,/KLE,$
               WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
if cancel then return

stop = cfld(state.w.stop_fld,3,CANCEL=cancel,/EMPTY)
if cancel then return
stop = crange(stop,[0,state.r.ncols-1],'Stop',/KGE,/KLE,$
              WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
if cancel then return

xranges = *state.r.xranges
xranges[0,*] = xranges[0,*] > start
xranges[1,*] = xranges[1,*] < stop

apradius = cfld(state.w.xsapradius_fld,7,CANCEL=cancel,/EMPTY)
if cancel then return
apradius = float( strsplit(temporary(apradius),',',/extract) )
if n_elements(apradius) ne state.r.xsnaps then begin

    cancel = 1
    mess = 'Number of Apradii must equal number of Apertures.'
    ok = dialog_message(mess,/error,dialog_parent=state.w.xspextool_base)
    setfocus,state.w.xsapradius_fld
    return


endif
apradius = crange(apradius,[0,state.r.slith_arc],'Apradius',/KGE,/KLE,$
                  WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
if cancel then setfocus,state.w.xsapradius_fld
if cancel then return

if state.r.xsbgsub eq 'On' then begin

    bg = cfld(state.w.xsbg_fld,7,CANCEL=cancel,/EMPTY)
    if cancel then return
    bg = strsplit(temporary(bg),',',/extract) 
    for i = 0, n_elements(bg)-1 do begin

        junk = float( strsplit(temporary(bg[i]),'-',/extract) )
        bgr = (i eq 0) ? junk:[[bgr],[junk]]
        
    endfor

    bgfit = cfld(state.w.xsbgfitorder_fld,3,CANCEL=cancel,/EMPTY)
    if cancel then return
    bgfit = crange(bgfit,[0,7],'Background Fit Order',/KGE,/KLE,$
                   WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
    if cancel then setfocus,state.w.xsbgfitorder_fld
    if cancel then return

endif

sapradius = fltarr(state.r.xsnaps,state.r.norders)
for i = 0, state.r.norders-1 do begin

    profile = (*state.r.profiles).(i)
    if (*state.r.xsdoorders)[i] eq 1 then begin

         m    = mkmask_xs(profile[*,0],(*state.r.appos)[*,i],$
                            apradius,BG=bgr,WIDGET_ID=state.w.xspextool_base,$
                          CANCEL=cancel)
         if cancel then return
         word = 'mask'+strtrim(i+1,2)
         mask = (i eq 0) ? create_struct(word,[[reform(profile[*,0])],[m]]):$
           create_struct(mask,word,[[reform(profile[*,0])],[m]]) 
        
    endif else begin
        
        word = 'mask'+strtrim(i+1,2)
        b    = fltarr(n_elements(profile[*,1]))
        mask = (i eq 0) ? create_struct(word,[[reform(profile[*,0])],[b]]):$
          create_struct(mask,word,[[reform(profile[*,0])],[b]]) 

    endelse
    sapradius[*,i] = apradius

endfor

;  Plot on ximgtool.

z = where(*state.r.xsdoorders eq 1,norders)
ximgtool, *state.d.workimage, WID=wid,GROUP_LEADER=state.w.xspextool_base,$
  /NOUPDATE,BUFFER=1,ZWINPOS='None'

plotap,(*state.r.edgecoeffs)[*,*,z],xranges[*,z],$
  *state.r.tracecoeffs,norders,state.r.xsnaps,state.r.slith_arc,$
  sapradius[*,z],state.r.nrows,wid

;  Plot on xplotprofiles.

xplotprofiles,*state.r.profiles,*state.r.orders,*state.r.xsdoorders,$
  state.r.slith_arc,APPOS=*state.r.appos,MASK=mask,$
  GROUP_LEADER=state.w.xspextool_base,BGFIT=bgfit


state.r.xscontinue=5

end
;
;******************************************************************************
;
pro xspextool_xsextract,state,CANCEL=cancel,BEEP=beep

cancel = 0

if state.r.xscontinue lt 5 then begin
    
    ok = dialog_message('Previous steps not complete.',/ERROR,$
                        DIALOG_PARENT=state.w.xspextool_base)
    cancel = 1
    return
    
endif

;  Get user inputs.

start = cfld(state.w.start_fld,3,/EMPTY,CANCEL=cancel)
if cancel then return
start = crange(start,[0,state.r.ncols-1],'Start',/KGE,/KLE,$
               WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
if cancel then return

stop = cfld(state.w.stop_fld,3,/EMPTY,CANCEL=cancel)
if cancel then return
stop = crange(stop,[0,state.r.ncols-1],'Stop',/KGE,/KLE,$
              WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
if cancel then return

xranges = *state.r.xranges
xranges[0,*] = xranges[0,*] > start
xranges[1,*] = xranges[1,*] < stop

apradius = cfld(state.w.xsapradius_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
apradius = float( strsplit(temporary(apradius),',',/EXTRACT) )
if n_elements(apradius) ne state.r.xsnaps then begin

    cancel = 1
    ok = dialog_message('Number of Apradii must equal number of Apertures.',$
                        /ERROR,DIALOG_PARENT=state.w.xspextool_base)
    setfocus,state.w.xsapradius_fld
    return


endif
apradius = crange(apradius,[0,state.r.slith_arc],'Apradius',/KGE,/KLE,$
                  WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
if cancel then setfocus,state.w.xsapradius_fld
if cancel then return

if state.r.xsbgsub eq 'On' then begin

    bg  = cfld(state.w.xsbg_fld,7,/EMPTY,CANCEL=cancel)
    if cancel then return
    bg = strsplit(temporary(bg),',',/EXTRACT) 
    for i = 0, n_elements(bg)-1 do begin

        junk = float( strsplit(temporary(bg[i]),'-',/EXTRACT) )
        bgr = (i eq 0) ? junk:[[bgr],[junk]]
        
    endfor

    bgfit = cfld(state.w.xsbgfitorder_fld,3,/EMPTY,CANCEL=cancel)
    if cancel then return
    bgfit = crange(bgfit,[0,7],'Background Fit Order',/KGE,/KLE,$
                   WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
    if cancel then setfocus,state.w.xsbgfitorder_fld
    if cancel then return

endif

;  If filename mode, check to make sure the outname is filled in.

filename = (state.r.filereadmode eq 'Filename') ? 1:0
if filename then file = cfld(state.w.outname_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

;  Extract spectra

z = where(*state.r.xsdoorders eq 1, norders)

workimage = *state.d.workimage
varimage  = *state.d.varimage

xspextool_message,state,'Extracting spectra...'

if state.r.fixbdpx then begin

    idx=0
    for i = 0,state.r.norders-1 do begin
        
        if (*state.r.xsdoorders)[i] eq 1 then begin
            
            key = 'Order'+string(i,format='(i2.2)')
            scoeff= (idx eq 0) ? create_struct(key,(*state.r.profcoeffs).(i)):$
              create_struct(scoeff,key,(*state.r.profcoeffs).(i))
            idx = idx + 1
            
        endif
        
    endfor

    bdpxmk = *state.d.bdpxmask

    bdpxthresh = cfld(state.w.bdpxthresh_fld,4,/EMPTY,CANCEL=cancel)
    if cancel then return

endif

if state.r.e2d then begin

    spectra = extractspec2d_xs(workimage,varimage,$
                               (*state.r.edgecoeffs)[*,*,z],$
                               *state.r.tracecoeffs,norders,state.r.xsnaps,$
                               start,stop,xranges[*,z],state.r.slith_arc,$
                               state.r.slith_pix,apradius,$
                               SPATCOEFFS=scoeff,BGR=bgr,BGORDER=bgfit,$
                               BDPIXTHRESH=bdpxthresh,$
                               /UPDATE,WIDGET_ID=state.w.xspextool_base,$
                               CANCEL=cancel)

    s = size(spectra)
    state.r.aph_arc = s[2]*state.r.slith_arc/state.r.slith_pix

endif else begin

    spectra = extractspec_xs(workimage,varimage,(*state.r.edgecoeffs)[*,*,z],$
                             *state.r.tracecoeffs,norders,state.r.xsnaps,$
                             start,stop,xranges[*,z],state.r.slith_arc,$
                             apradius,BGR=bgr,BGORDER=bgfit,$
                             SPATCOEFFS=scoeff,BDPIXTHRESH=bdpxthresh, $
                             BGSUBIMG=bgsubimg,ERRORPROP=state.r.errorprop, $
                             BDPXMK=bdpxmk,/UPDATE,$
                             WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)

endelse
if cancel then return

ximgtool, bgsubimg, WID=wid,GROUP_LEADER=state.w.xspextool_base,$
          /NOUPDATE,BUFFER=1,ZWINPOS='None'

*state.d.spectra = spectra
state.r.xscontinue=6

xspextool_wavecal,state,CANCEL=cancel
if cancel then return

xspextool_writespec,state,CANCEL=cancel
if cancel then return

if keyword_set(BEEP) then beep

end
;
;******************************************************************************
;
pro xspextool_xstrace,state,CANCEL=cancel

cancel = 0

if state.r.xscontinue lt 3 then begin

    ok = dialog_message('Previous steps not complete.',/ERROR,$
                        DIALOG_PARENT=state.w.xspextool_base)
    cancel = 1
    return

endif

;  Get user inputs.

start = cfld(state.w.start_fld,3,/EMPTY,CANCEL=cancel)
if cancel then return
start = crange(start,[0,state.r.ncols-1],'Start',/KGE,/KLE,$
               WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
if cancel then return

stop = cfld(state.w.stop_fld,3,/EMPTY,CANCEL=cancel)
if cancel then return
stop = crange(stop,[0,state.r.ncols-1],'Stop',/KGE,/KLE,$
              WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
if cancel then return

step = cfld(state.w.tracestepsize_fld,3,/EMPTY,CANCEL=cancel)
if cancel then return
step = crange(step,[1,20],'Step Size',/KGE,/KLE,$
              WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
if cancel then return

fitorder = cfld(state.w.polyfitorder_fld,3,/EMPTY,CANCEL=cancel)
if cancel then return
fitorder = crange(fitorder,[0,8],'Fit Order',/KGE,/KLE,$
              WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
if cancel then return

xranges = *state.r.xranges
xranges[0,*] = xranges[0,*] > start
xranges[1,*] = xranges[1,*] < stop

;  Create trace.

xspextool_message,state,'Tracing objects...'

ximgtool,*state.d.workimage, WID=wid,GROUP_LEADER=state.w.xspextool_base,$
  /NOUPDATE,BUFFER=1,ZWINPOS='None'

z = where(*state.r.xsdoorders eq 1,norders)
*state.r.tracecoeffs = aptotrace((*state.r.edgecoeffs)[*,*,z],$
                                 state.r.slith_arc,*state.r.appos,$
                                 xranges[*,z],step,fitorder,state.r.nrows,$
                                 WID=wid)

state.r.xscontinue = 4

end
;
;******************************************************************************
;
pro xspextool,instrfile,FAB=fab,ENG=eng,SMALL=small

;  Determine the instrument in use, default is SpeX.

general = 0
wavecal = 1
if n_elements(instrfile) ne 0 then begin

    general = 1 
    wavecal = 0

endif else instrfile = 'SpeX.dat'

;  get Spextool path.

getosinfo,dirsep,strsep

last   = strpos(!path,'Spextool')
first  = strpos(!path,strsep,last,/REVERSE_SEARCH)
result = strmid(!path,first+1,last-first+7)

path = cpath(result,CANCEL=cancel)
if cancel then return

readinstrfile,filepath(instrfile,ROOT_DIR=path,SUBDIRECTORY='data'),instr,$
  irafname,gain,readnoise,itime,coadds,ndrs,slowcnt,readtime,time,posangle,$
  ha,airmass,nint,bdpxmk,keywords,ncols,nrows

;  Set the fonts and create the color table.

getfonts,buttonfont,textfont
if keyword_set(SMALL) then begin

    buttonfont = ''
    textfont = ''

endif

mkct

;  Build three structures which will hold important info.
;  w - contains info pertaining to widget operations.
;  r - contains info pertaining to the reduction process.
;  d - contains all of the data.

w = {arc_base:0,$
     arcimages_fld:[0,0],$
     arcflat_fld:[0,0],$
     arconame_fld:[0,0],$
     arcsky_fld:[0,0],$
     arcskyrow:0,$
     autoxcor_bg:0L,$
     base:'',$
     bdpxthresh_fld:[0,0],$
     calfile_fld:[0,0],$
     calpath_fld:[0,0],$
     cals_base:0L,$
     combflat_base:0L,$
     combflat_fld:[0,0],$
     combimages_fld:[0,0],$
     combineimgs_base:0,$
     comboname_fld:[0,0],$
     datapath_fld:[0,0],$
     doall_base:0,$
     eng_base:0L,$
     flat_base:0,$
     flatfield_fld:[0,0],$
     flatoname_fld:[0,0],$
     flats_fld:[0,0],$
     frac_fld:[0L,0L],$
     general:general,$
     help_base:0,$
     helptext:0,$
     imageinput_base:0,$
     inprefix_fld:[0,0],$
     maskwindow_fld:[0,0],$
     message_window:0,$
     optext_bg:0L,$
     other_base:0,$
     outformat_bg:0,$
     outname_fld:[0,0],$
     outprefix_fld:[0,0],$
     oversamp_fld:[0,0],$
     path_base:0,$
     polyfitorder_fld:[0,0],$
     plotprofiles:0L,$
     procpath_fld:[0,0],$
     psapfindman_base:0,$
     psapfindtrace_base:0,$
     psappos_fld:[0,0],$
     psapradius_fld:[0,0],$
     psapsigns_fld:[0L,0L],$
     psbginfo_base:0,$
     psbgstart_fld:[0,0],$
     psbgsub_bg:0L,$
     psbgwidth_fld:[0,0],$
     psbgfitorder_fld:[0,0],$
     pscol2_base:0,$
     psfradius_fld:[0,0],$
     psitracefile_fld:[0,0],$
     psnaps_dl:0,$
     psorder_bg:0L,$
     psotracefile_fld:[0,0],$
     pstrace_base:0,$
     ps_base:0,$
     saturation_fld:[0L,0L],$
     seeingthresh_fld:[0L,0L],$
     sep:dirsep,$
     sky_base:0,$
     skyflat_fld:[0,0],$
     skyimage_base:0,$
     skyimage_fld:[0,0],$
     skyoname_fld:[0,0],$
     skyimages_fld:[0,0],$
     slitw_arc_fld:[0,0],$
     slitw_pix_fld:[0,0],$
     sourceimages_fld:[0,0],$
     specfiles_fld:[0,0],$
     speclist_fld:[0,0],$
     start_fld:[0,0],$
     stop_fld:[0,0],$
     storedwc_bg:0L,$
     sumap_fld:[0,0],$
     table:0L,$
     tracestepsize_fld:[0,0],$
     tracethresh_fld:[0L,0L],$
     version:'v3.4',$
     wavecalthresh_fld:[0,0],$
     wavecal_fld:[0,0],$
     wconame_fld:[0,0],$
     xsapfindman_base:0,$
     xsapfindtrace_base:0,$
     xsappos_fld:[0,0],$
     xsapradius_fld:[0,0],$
     xsbgfitorder_fld:[0,0],$
     xsbginfo_base:0,$
     xsbg_fld:[0,0],$
     xs_base:0,$
     xscol2_base:0,$
     xsitracefile_fld:[0,0],$
     xsorder_bg:0L,$
     xsotracefile_fld:[0,0],$
     xstrace_base:0,$
     xspextool_base:0L,$
     ybuffer_fld:[0L,0L]}

r = {airmass:airmass,$
     aph_arc:1.0,$
     appos:ptr_new(fltarr(2)),$
     tmpappos:ptr_new(fltarr(2)),$
     apsign:ptr_new(intarr(2)),$
     arccombinestat:'Median',$
     arcreductionmode:'ShortXD',$
     autoxcorr:1,$
     bdpxmask:bdpxmk,$
     calcombinestat:'Median',$
     calpath:'',$
     checkseeing:1,$
     coadds:coadds,$
     combbgsub:'Off',$
     combcombinestat:'Median',$
     combineaps:'No',$
     comborder:0,$
     combreductionmode:'A',$
     datapath:'',$
     detsol:0,$
     doallsteps:0,$
     e2d:0,$
     edgecoeffs:ptr_new(fltarr(2,2)),$
     errorprop:1,$
     fixbdpx:1,$
     filereadmode:'Index',$
     fitsoutput:1,$
     flatcombinestat:'Median',$
     flatfield:1,$
     gain:gain,$
     hdrinfo:ptr_new(intarr(2)),$
     ha:ha,$
     ximgtool:0,$
     instr:instr,$
     irafname:irafname,$
     itime:itime,$
     juldate:0.0,$
     keywords:keywords,$
     lc:1,$
     modename:'',$,$
     ncols:ncols,$
     ndrs:ndrs,$
     nint:nint,$
     norders:0L,$
     normalizeflat:'Yes',$
     ntotaps:4,$
     nrows:nrows,$
     obsmode:'',$
     offset:0.,$
     optextract:1,$
     orders:ptr_new([0,1,2,3,4,5]),$
     packagepath:path,$
     plotautoxcorr:0,$
     plotlinefind:0,$
     plotresiduals:0,$
     plotsatpix:1,$
     posangle:posangle,$
     prefix:'spc',$
     procpath:'',$
     profcoeffs:ptr_new(fltarr(2)),$
     psbgsub:'On',$
     psapfindmode:'Manual',$
     psapfindmanmode:'Auto',$
     pscontinue:0L,$
     psdoorders:ptr_new([1]),$
     psnaps:2L,$
     readnoise:readnoise,$
     readtime:readtime,$
     reductionmode:'A-B',$
     rms:ptr_new(fltarr(2)),$
     rotation:0,$
     shiftimages:'Yes',$
     slith_arc:0.,$
     slith_pix:0.,$
     slitw_arc:0.,$
     slitw_pix:0.,$
     slowcnt:slowcnt,$
     skycombinestat:'Median',$
     skyscalingstat:'Median',$
     subscatlgt:0,$
     tablesize:10,$
     textoutput:0,$
     time:time,$
     tracecoeffs:ptr_new(fltarr(2)),$
     usestoredwc:0L,$
     wavecal:wavecal,$
     workfiles:ptr_new(strarr(2)),$
     xranges:ptr_new(fltarr(2)),$
     xsapfindmode:'Manual',$
     xsbgsub:'On',$
     xscontinue:0L,$
     xsdoorders:ptr_new([1]),$
     xsnaps:0L,$
     profiles:ptr_new(fltarr(2))}

d = {wavecal:ptr_new(fltarr(2,2)),$
     bdpxmask:ptr_new(intarr(2,2)),$
     disp:ptr_new(fltarr(2)),$
     mask:ptr_new(fltarr(2)),$
     spectra:ptr_new(fltarr(2)),$
     varimage:ptr_new(fltarr(2)),$
     workimage:ptr_new(fltarr(2,2))}

;  Load the three structures in the state structure.

state = {w:w,r:r,d:d}

;  Build the widget.

title = 'Spextool '+state.w.version+' for '+state.r.instr
state.w.xspextool_base = widget_base(TITLE=title, $
                                     /COLUMN)

   quit_button = widget_button(state.w.xspextool_base,$
                               FONT=buttonfont,$
                               VALUE='Quit',$
                               UVALUE='Quit')

; Message Window.

   ysize = (keyword_set(SMALL)) ? 1:2

   mess = 'Welcome to Spextool '+state.w.version+'.'
   state.w.message_window = widget_text(state.w.xspextool_base, $
                                        FONT=textfont,$
                                        VALUE=mess, $
                                        /SCROll,$
                                        YSIZE=ysize)
   
; Main base, which is always showing.

   main_base = widget_base(state.w.xspextool_base,$
                           /ROW,$
                           FRAME=5)

      col1_base = widget_base(main_base,$
                              /COLUMN,$
                              /BASE_ALIGN_RIGHT,$
                              /FRAME)
      
         bg = cw_bgroup(col1_base,$
                        ['Filename','Index'],$
                        /ROW,$
                        LABEL_LEFT='File Read Mode:',$
                        /RETURN_NAME,$
                        /NO_RELEASE,$
                        UVALUE='Readmode',$
                        FONT=buttonfont,$
                        /EXCLUSIVE,$
                        SET_VALUE=1)
         
         field = coyote_field2(col1_base,$
                               LABELFONT=buttonfont,$
                               FIELDFONT=textfont,$
                               TITLE='Input Prefix:',$
                               VALUE=state.r.prefix,$
                               UVALUE = 'Input Prefix',$
                               XSIZE=15,$
                               EVENT_PRO = 'xspextool_fields',$
                               /CR_ONLY,$
                               TEXTID=textid)
         state.w.inprefix_fld = [field,textid]
         
         field = coyote_field2(col1_base,$
                               LABELFONT=buttonfont,$
                               FIELDFONT=textfont,$
                               TITLE='Output Prefix:',$
                               VALUE='spectra',$
                               UVALUE = 'Output Prefix',$
                               XSIZE=15,$
                               EVENT_PRO = 'xspextool_fields',$
                               /CR_ONLY,$
                               textid=textid)
         state.w.outprefix_fld = [field,textid]
         widget_control, field, sensitive=0
         
         field = coyote_field2(col1_base,$
                               LABELFONT=buttonfont,$
                               FIELDFONT=textfont,$
                               TITLE='Output File Name:',$
                               VALUE='',$
                               uval = 'Output File Name',$
                               XSIZE=15,$
                               EVENT_PRO = 'xspextool_fields',$
                               /CR_ONLY,$
                               textid=textid)
         state.w.outname_fld = [field,textid]
         widget_control, field, sensitive=0

         state.w.outformat_bg = cw_bgroup(col1_base,$
                                          font=buttonfont,$
                                          ['FITS','Text'],$
                                          /row,$
                                          /return_name,$
                                          /nonexclusive,$
                                          label_left='Output Format:',$
                                          UVALUE='Output Format',$
                                          set_VALUE=[1,0])

        state.w.plotprofiles = widget_label(col1_base,$
                                             UVALUE='Plot Profiles',$
                                             VALUE='')

      state.w.imageinput_base = widget_base(main_base,$
                                            /column,$
                                            frame=1,$
                                            map=0)

         bg = cw_bgroup(state.w.imageinput_base,$
                        font=buttonfont,$
                        ['A','A-B','A-Sky'],$
                        /row,$
                        /return_name,$
                        /no_release,$
                        /exclusive,$
                        label_left='Reduction Mode:',$
                        UVALUE='Reduction Mode',$
                        set_VALUE=1)
         
         base = widget_base(state.w.imageinput_base,$
                            /column,$
                            /base_align_right)
         
            row = widget_base(base,$
                              /row,$
                              /base_align_center)
            
               button = widget_button(row,$
                                      font=buttonfont,$
                                      VALUE='Source Images',$
                                      UVALUE='Source Images Button',$
                                      EVENT_PRO = 'xspextool_event')
               
               
               sourceimages = coyote_field2(row,$
                                            LABELFONT=buttonfont,$
                                            FIELDFONT=textfont,$
                                            TITLE=':',$
                                            UVALUE='Source Images',$
                                            XSIZE=19,$
                                            EVENT_PRO = 'xspextool_fields',$
                                            /CR_ONLY,$
                                            textID=textid)
               state.w.sourceimages_fld = [sourceimages,textid]
            
            state.w.skyimage_base = widget_base(base,$
                                                /row,$
                                                /base_align_center)

               button = widget_button(state.w.skyimage_base,$
                                      font=buttonfont,$
                                      VALUE='Sky Image',$
                                      UVALUE='Sky Image Button',$
                                      EVENT_PRO = 'xspextool_event')

               skyimage = coyote_field2(state.w.skyimage_base,$
                                        LABELFONT=buttonfont,$
                                        FIELDFONT=textfont,$
                                        TITLE=':',$
                                        UVALUE='Super Sky Image',$
                                        XSIZE=19,$
                                        VALUE='',$
                                        EVENT_PRO = 'xspextool_fields',$
                                        /CR_ONLY,$
                                        textID=textid)
               state.w.skyimage_fld = [skyimage,textid]
               widget_control, state.w.skyimage_base, sensitive=0
               
            row = widget_base(base,$
                              /row,$
                              /base_align_center)
            
               button = widget_button(row,$
                                      font=buttonfont,$
                                      VALUE='Full Flat Name',$
                                      UVALUE='Full Flat Name Button',$
                                      EVENT_PRO = 'xspextool_event')

               flatfield = coyote_field2(row,$
                                         LABELFONT=buttonfont,$
                                         FIELDFONT=textfont,$
                                         TITLE=':',$
                                         UVALUE='Full Flat Name',$
                                         XSIZE=19,$
                                         EVENT_PRO = 'xspextool_fields',$
                                         /CR_ONLY,$
                                         textID=textID)
               state.w.flatfield_fld = [flatfield,textID]

            if not state.w.general then begin

                wavecal_base = widget_base(base,$
                                           /ROW,$
                                           /BASE_ALIGN_CENTER)

                button = widget_button(wavecal_base,$
                                       font=buttonfont,$
                                       VALUE='Full Wavecal Name',$
                                       UVALUE='Full Wavecal Name Button',$
                                       EVENT_PRO = 'xspextool_event')

                fld = coyote_field2(wavecal_base,$
                                    LABELFONT=buttonfont,$
                                    FIELDFONT=textfont,$
                                    TITLE=':',$
                                    UVALUE='Full Wavecal Name',$
                                    XSIZE=19,$
                                    EVENT_PRO = 'xspextool_fields',$
                                    /CR_ONLY,$
                                         textid=textid)
                state.w.wavecal_fld = [fld,textid]
                
                
                

;                state.w.arcimage_base = widget_base(base,$
;                                                    /row,$
;                                                    /base_align_center)

;                button = widget_button(state.w.arcimage_base,$
;                                       font=buttonfont,$
;                                       VALUE='Full Arc Name',$
;                                       UVALUE='Full Arc Name Button',$
;                                       EVENT_PRO = 'xspextool_event')

;                arcimage = coyote_field2(state.w.arcimage_base,$
;                                         LABELFONT=buttonfont,$
;                                         FIELDFONT=textfont,$
;                                         TITLE=':',$
;                                         UVALUE='Full Arc Name',$
;                                         XSIZE=15,$
;                                         EVENT_PRO = 'xspextool_fields',$
;                                         /CR_ONLY,$
;                                         textid=textid)
;                state.w.arcimage_fld = [arcimage,textid]
                
             endif

         button = widget_button(state.w.imageinput_base,$
                                font=buttonfont,$
                                VALUE='Load Image',$
                                UVALUE='Load Single Image')

      state.w.doall_base = widget_base(main_base,$
                                       map=0,$
                                       /column,$
                                       frame=1)
      
         button = widget_button(state.w.doall_base,$
                                font=buttonfont,$
                                VALUE='Do All Steps',$
                                UVALUE='Do All Steps')
         

             
; Menu bar

   menubar = widget_base(state.w.xspextool_base,$
                         EVENT_PRO='xspextool_menuevent',$
                         /row)                         

      TITLES = ['Paths','Cals','Sky','Combine Images','Point Source',$
                'Extended Source','Other','Help']

      if state.w.general then TITLES = ['Paths','Flat','Combine Images',$
                                        'Point Source','Extended Source',$
                                        'Other','Help']

      if keyword_set(FAB) then TITLES = ['Paths','Flat','Arc','Sky',$
                                         'Combine Images','Point Source',$
                                         'Extended Source','Other','Help']
      
      if keyword_set(ENG) then TITLES = ['Paths','Cals','Sky', $
                                         'Combine Images','Point Source',$
                                         'Extended Source','Other','Eng.', $
                                         'Help']
      row = widget_base(menubar,$
                        /ROW,$
                        /TOOLBAR,$
                        /EXCLUSIVE,$
                        /BASE_ALIGN_CENTER)

      for i = 0,n_elements(TITLES)-2 do begin

         button = widget_button(row,$
                                VALUE=TITLES[i],$
                                UVALUE=TITLES[i],$
                                /NO_RELEASE,$
                                FONT=buttonfont)

         if i eq 0 then widget_control, button, /SET_BUTTON

      endfor
      button = widget_button(menubar,$
                             VALUE='Help',$
                             UVALUE='Help',$
                             FONT=buttonfont)

;      menu_bg = cw_bgroup(menubar,$
;                          TITLES,$
;                          /row,$
;                          font=buttonfont,$
;                          /return_name,$
;                          /no_release,$
;                          UVALUE='Buttons')

;  Now build the work spaces.

   work_base = widget_base(state.w.xspextool_base,$
                           frame=5)


; Paths base.

      state.w.path_base = widget_base(work_base,$
                                      /COLUMN,$
                                      /MAP)
      
         row = widget_base(state.w.path_base,$
                           /ROW,$
                           /BASE_ALIGN_CENTER)
         
            button = widget_button(row,$
                                   FONT=buttonfont,$
                                   XSIZE=110,$                             
                                   VALUE='Data Path',$
                                   UVALUE='Data Path Button',$
                                   EVENT_PRO='xspextool_event')
            
            path = coyote_field2(row,$
                                 LABELFONT=buttonfont,$
                                 FIELDFONT=textfont,$
                                 TITLE=':',$
                                 VALUE = state.r.datapath,$
                                 UVALUE='Data Path Field',$
                                 XSIZE=70,$
                                 /CR_ONLY,$
                                 EVENT_PRO='xspextool_fields',$
                                 TEXTID=textid)
            state.w.datapath_fld = [path,textid]
            
            clear = widget_button(row,$
                                  FONT=buttonfont,$
                                  VALUE='Clear',$
                                  UVALUE='Clear Data Path',$
                                  EVENT_PRO='xspextool_event')
            
         row = widget_base(state.w.path_base,$
                           /ROW,$
                           /BASE_ALIGN_CENTER)
         
            button = widget_button(row,$
                                   FONT=buttonfont,$
                                   XSIZE=110,$
                                   VALUE='Cal Path',$
                                   UVALUE='Cal Path Button',$
                                   EVENT_PRO='xspextool_event')
            
            path = coyote_field2(row,$
                                 LABELFONT=buttonfont,$
                                 FIELDFONT=textfont,$
                                 TITLE=':',$
                                 VALUE=state.r.calpath,$
                                 UVALUE='Cal Path Field',$
                                 XSIZE=70,$
                                 /CR_ONLY,$
                                 EVENT_PRO='xspextool_fields',$
                                 TEXTID=textid)
            state.w.calpath_fld = [path,textid]
            
            clear = widget_button(row,$
                                  FONT=buttonfont,$
                                  VALUE='Clear',$
                                  UVALUE='Clear Cal Path',$
                                  EVENT_PRO='xspextool_event')
            
         row = widget_base(state.w.path_base,$
                           /ROW,$
                           /BASE_ALIGN_CENTER)
         
            button = widget_button(row,$
                                   FONT=buttonfont,$
                                   XSIZE=110,$
                                   VALUE='Proc Path',$
                                   UVALUE='Proc Path Button',$
                                   EVENT_PRO='xspextool_event')
            
            path = coyote_field2(row,$
                                 LABELFONT=buttonfont,$
                                 FIELDFONT=textfont,$
                                 TITLE=':',$
                                 VALUE=state.r.procpath,$
                                 UVALUE='Proc Path Field',$
                                 XSIZE=70,$
                                 /CR_ONLY,$
                                 EVENT_PRO='xspextool_fields',$
                                 TEXTID=textid)
            state.w.procpath_fld = [path,textid]
            
            clear = widget_button(row,$
                                  FONT=buttonfont,$
                                  VALUE='Clear',$
                                  UVALUE='Clear Proc Path',$
                                  EVENT_PRO='xspextool_event')
            
         load = widget_button(state.w.path_base,$
                              VALUE='Update Paths',$
                              FONT=buttonfont,$
                              UVALUE='Update Paths')

;  Flat base

      state.w.flat_base = widget_base(work_base,$
                                      MAP=0)

         row = widget_base(state.w.flat_base,$
                           /ROW)
      
            col1_base = widget_base(row,$
                                    /COLUMN,$
                                    /FRAME)
            
               if state.w.general then begin
                   
                   flatimages = coyote_field2(col1_base,$
                                              LABELFONT=buttonfont,$
                                              FIELDFONT=textfont,$
                                              TITLE='Cal File:',$
                                              UVALUE='Cal File',$
                                              XSIZE=15,$
                                              VALUE='cal.dat',$
                                              EVENT_PRO = 'xspextool_fields',$
                                              /CR_ONLY,$
                                              TEXTID=textid) 
                   state.w.calfile_fld = [flatimages,textid]
                   
               endif
               
               flatimages = coyote_field2(col1_base,$
                                          LABELFONT=buttonfont,$
                                          FIELDFONT=textfont,$
                                          TITLE='Flat Images:',$
                                          UVALUE='Flat Images',$
                                          XSIZE=15,$
                                          EVENT_PRO='xspextool_fields',$
                                          /CR_ONLY,$
                                          TEXTID=textid) 
               state.w.flats_fld = [flatimages,textid]
               
               combine_bg = cw_bgroup(col1_base,$
                                      FONT=buttonfont,$
                                      ['Weighted Mean','Mean','Median'],$
                                      /ROW,$
                                      /RETURN_NAME,$
                                      /NO_RELEASE,$
                                      /EXCLUSIVE,$
                                      LABEL_TOP='Combination Statistics:',$
                                      UVALUE='Flat Combination Statistic',$
                                      SET_VALUE=2)
               
               normal_bg = cw_bgroup(col1_base,$
                                     FONT=buttonfont,$
                                     ['Yes','No'],$
                                     /ROW,$
                                     /RETURN_NAME,$
                                     /NO_RELEASE,$
                                     /EXCLUSIVE,$
                                     LABEL_LEFT='Normalize Flat:',$
                                     UVALUE='Normalize Flat',$
                                     SET_VALUE=0)

;               bg = cw_bgroup(col1_base,$
;                              font=buttonfont,$
;                              ['Subtract Scattered Light'],$
;                              /ROW,$
;                              /RETURN_INDEX,$
;                              /NONEXCLUSIVE,$
;                              UVALUE='Sub Scattered Light',$
;                              set_VALUE=1)
               
               flatoname= coyote_field2(col1_base,$
                                        LABELFONT=buttonfont,$
                                        FIELDFONT=textfont,$
                                        TITLE='Output Flat Name:',$
                                        UVALUE='Output Flat Name',$
                                        XSIZE=15,$
                                        TEXTID=textid) 
               state.w.flatoname_fld = [flatoname,textid]
               
               if state.w.general then begin
                   
                   col2_base = widget_base(row,$
                                           /COLUMN,$
                                           /FRAME,$
                                           /BASE_ALIGN_RIGHT)
                   
                   slitw_pix = coyote_field2(col2_base,$
                                             LABELFONT=buttonfont,$
                                             FIELDFONT=textfont,$
                                             TITLE='Slit Width Pix:',$
                                             UVALUE='Slit Width Pix',$
                                             XSIZE=15,$
                                             EVENT_PRO='xspextool_fields',$
                                             /CR_ONLY,$
                                             TEXTID=textid) 
                   state.w.slitw_pix_fld = [slitw_pix,textid]
                   
                   slitw_arc = coyote_field2(col2_base,$
                                             LABELFONT=buttonfont,$
                                             FIELDFONT=textfont,$
                                             TITLE='Slit Width Arc:',$
                                             UVALUE='Slit Width Arc',$
                                             XSIZE=15,$
                                             EVENT_PRO='xspextool_fields',$
                                             /CR_ONLY,$
                                             TEXTID=textid) 
                   state.w.slitw_arc_fld = [slitw_pix,textid]
                   
               endif

               mkflat_button = widget_button(col1_base,$
                                             FONT=buttonfont,$
                                             VALUE='Construct Flat',$
                                             UVALUE='Construct Flat')

;  Arc base
               
      state.w.arc_base = widget_base(work_base,$
                                     /COLUMN,$
                                     MAP=0)
      
         row_base = widget_base(state.w.arc_base,$
                                /ROW)
         
            col1_base = widget_base(row_base,$
                                    /COLUMN,$
                                    /FRAME,$
                                    /BASE_ALIGN_RIGHT)
            
               label = widget_label(col1_base,$
                                    VALUE='Arc Images:',$
                                    FONT=buttonfont,$         
                                    /ALIGN_CENTER)
               
               bg = cw_bgroup(col1_base,$
                              FONT=buttonfont,$
                              ['ShortXD','LongXD'],$
                              /ROW,$
                              /RETURN_NAME,$
                              /NO_RELEASE,$
                              /EXCLUSIVE,$
                              LABEL_LEFT='Arc Reduction Mode:',$
                              UVALUE='Arc Reduction Mode',$
                              SET_VALUE=0)
               
               images = coyote_field2(col1_base,$
                                      LABELFONT=buttonfont,$
                                      FIELDFONT=textfont,$
                                      TITLE='Arc Images:',$
                                      UVALUE='Arc Images',$
                                      XSIZE=15,$
                                      EVENT_PRO='xspextool_fields',$
                                      /CR_ONLY,$
                                      TEXTID=textid) 
               state.w.arcimages_fld = [images,textid]
               
;               combine_bg = cw_bgroup(col1_base,$
;                                      FONT=buttonfont,$
;                                      ['Weighted Mean','Mean','Median'],$
;                                      /ROW,$
;                                      /RETURN_NAME,$
;                                      /NO_RELEASE,$
;                                      /EXCLUSIVE,$
;                                      LABEL_TOP='Combination Statistics:',$
;                                      UVALUE='Arc Combination Statistic',$
;                                      SET_VALUE=2)
               
               row = widget_base(col1_base,$
                                 /ROW,$
                                 /BASE_ALIGN_CENTER)
               
                  button = widget_button(row,$
                                         FONT=buttonfont,$
                                         VALUE='Full Flat Name',$
                                         UVALUE='Arc Flat Field Button',$
                                         EVENT_PRO='xspextool_event')
                  
                  flatimages = coyote_field2(row,$
                                             LABELFONT=buttonfont,$
                                             FIELDFONT=textfont,$
                                             TITLE=':',$
                                             UVALUE='Arc Flat Field',$
                                             XSIZE=15,$
                                             EVENT_PRO='xspextool_fields',$
                                             /CR_ONLY,$
                                             TEXTID=textid) 
                  state.w.arcflat_fld = [flatimages,textid]
                  
               arconame= coyote_field2(col1_base,$
                                       LABELFONT=buttonfont,$
                                       FIELDFONT=textfont,$
                                       TITLE='Output Arc Name:',$
                                       UVALUE='Output Arc Name',$
                                       XSIZE=15,$
                                       TEXTID=textid) 
               state.w.arconame_fld = [arconame,textid]

               fld = coyote_field2(col1_base,$
                                   LABELFONT=buttonfont,$
                                   FIELDFONT=textfont,$
                                   TITLE='Output Wavecal Name:',$
                                   UVALUE='Output Wavecal Name',$
                                   XSIZE=15,$
                                   TEXTID=textid) 
               state.w.wconame_fld = [fld,textid]
               
               
            col2_base = widget_base(row_base,$
                                    /COLUMN,$
                                    /FRAME,$
                                    /BASE_ALIGN_RIGHT)
            
               label = widget_label(col2_base,$
                                    VALUE='Sky Images:',$
                                    FONT=buttonfont,$
                                    /ALIGN_CENTER)
               
               state.w.arcskyrow = widget_base(col2_base,$
                                               /ROW,$
                                               /BASE_ALIGN_CENTER)
               
                  button = widget_button(state.w.arcskyrow,$
                                         FONT=buttonfont,$
                                         VALUE='Full Sky Name',$
                                         UVALUE='Arc Sky Button',$
                                         EVENT_PRO='xspextool_event')
                  
                  sky = coyote_field2(state.w.arcskyrow,$
                                      LABELFONT=buttonfont,$
                                      FIELDFONT=textfont,$
                                      TITLE=':',$
                                      UVALUE='Arc Sky Image',$
                                      XSIZE=20,$
                                      EVENT_PRO='xspextool_fields',$
                                      /CR_ONLY,$
                                      TEXTID=textid)
                  state.w.arcsky_fld = [sky,textid]
                  widget_control, state.w.arcskyrow, SENSITIVE=0
                  
         mkarc_button = widget_button(state.w.arc_base,$
                                         FONT=buttonfont,$
                                         VALUE='Make Arcs',$
                                         UVALUE='Make Arcs')

;  Cal Base

      state.w.cals_base = widget_base(work_base,$
                                      /COLUMN,$
                                      MAP=0)

         row_base = widget_base(state.w.cals_base,$
                                /ROW)

            col1_base = widget_base(row_base,$
                                    /COLUMN,$
                                    FRAME=1)
            
               combine_bg = cw_bgroup(col1_base,$
                                      FONT=buttonfont,$
                                      ['Weighted Mean','Mean','Median'],$
                                      /ROW,$
                                      /RETURN_NAME,$
                                      /NO_RELEASE,$
                                      /EXCLUSIVE,$
                                      LABEL_TOP='Combination Statistic:',$
                                      UVALUE='Cal Combination Statistic',$
                                      SET_VALUE=2)

;               state.w.autoxcor_bg = cw_bgroup(col1_base,$
;                                               font=buttonfont,$
;                                               ['Auto X-Correlate'],$
;                                               /ROW,$
;                                               /RETURN_NAME,$
;                                               /NONEXCLUSIVE,$
;                                               UVALUE='Auto X-Correlate',$
;                                               SET_VALUE=1)
               
;               bg = cw_bgroup(col1_base,$
;                              font=buttonfont,$
;                              ['Plot X-Correlate'],$
;                              /ROW,$
;                              /RETURN_NAME,$
;                              /NONEXCLUSIVE,$
;                              UVALUE='Plot Auto X-Correlate',$
;                              SET_VALUE=0)
               
;               sigma = coyote_field2(col1_base,$
;                                     LABELFONT=buttonfont,$
;                                     FIELDFONT=textfont,$
;                                     TITLE='Thresh:',$
;                                     UVALUE='Wavecal Sigma Thresh',$
;                                     XSIZE=5,$
;                                     VALUE='4.0',$
;                                     EVENT_PRO= 'xspextool_returns',$
;                                     /CR_ONLY,$
;                                     textid=textid)
;               state.w.wavecalthresh_fld = [sigma,textid]
               
;               wavecal_bg = cw_bgroup(col1_base,$
;                                      font=buttonfont,$
;                                      ['Plot Residuals'],$
;                                      /row,$
;                                      /nonexclusive,$
;                                      /return_name,$
;                                      UVALUE='Plot Residuals',$
;                                      SET_VALUE=0)
               
;               if keyword_set(ENG) then begin
                   
;                   wavecal_bg = cw_bgroup(col1_base,$
;                                          font=buttonfont,$
;                                          ['Plot Line Finding'],$
;                                          /row,$
;                                          /nonexclusive,$
;                                          /return_name,$
;                                          UVALUE='Plot Line Finding',$
;                                          SET_VALUE=0)
                   
;               endif
               
;               state.w.storedwc_bg = cw_bgroup(col1_base,$
;                                               font=buttonfont,$
;                                               ['Use Stored Solution'],$
;                                               /row,$
;                                               /nonexclusive,$
;                                               /return_name,$
;                                               UVALUE='Use Stored Solution',$
;                                               SET_VALUE=0)

;               bg = cw_bgroup(col1_base,$
;                              font=buttonfont,$
;                              ['Subtract Scattered Light'],$
;                              /ROW,$
;                              /RETURN_INDEX,$
;                              /NONEXCLUSIVE,$
;                              UVALUE='Sub Scattered Light',$
;                              set_VALUE=1)
               
            col2_base = widget_base(row_base,$
                                    /COLUMN,$
                                    /FRAME)

              clear_button = widget_button(col2_base,$
                                            FONT=buttonfont,$
                                            VALUE='Clear Table',$
                                            UVALUE='Clear Table')      

               rowlab='Cal Set '+strcompress(indgen(state.r.tablesize)+1,/RE)
               state.w.table = widget_table(col2_base,$
                                            COLUMN_LABEL=['Cals Files',$
                                                          'Sky Files'],$
                                            ROW_LABEL=rowlab,$
                                            COLUMN_WIDTHS=[150,150],$
                                            XSIZE=2,$
                                            YSIZE=state.r.tablesize,$
                                            ALIGNMENT=1,$
                                            VALUE=strarr(2,state.r.tablesize),$
                                            /EDITABLE,$
                                            UVALUE='Table')

         button = widget_button(state.w.cals_base,$
                                FONT=buttonfont,$
                                VALUE='Construct Calibration Frames',$
                                UVALUE='Construct Calibration Frames')

; Sky base.         

      state.w.sky_base = widget_base(work_base,$
                                     /ROW,$
                                     MAP=0)

         col1_base = widget_base(state.w.sky_base,$
                                 /COLUMN,$
                                 /FRAME,$
                                 /BASE_ALIGN_CENTER)

            skyimages = coyote_field2(col1_base,$
                                      LABELFONT=buttonfont,$
                                      FIELDFONT=textfont,$
                                      TITLE='Sky Images:',$
                                      UVALUE='Sky Images',$
                                      XSIZE=45,$
                                      EVENT_PRO='xspextool_fields',$
                                      /CR_ONLY,$
                                      TEXTID=textid) 
            state.w.skyimages_fld = [skyimages,textid]

            row = widget_base(col1_base,$
                              /ROW,$
                              /BASE_ALIGN_CENTER)
               
               button = widget_button(row,$
                                      FONT=buttonfont,$
                                      VALUE='Full Flat Name',$
                                      UVALUE='Sky Flat Field Button',$
                                      EVENT_PRO='xspextool_event')
                  
               flatimages = coyote_field2(row,$
                                          LABELFONT=buttonfont,$
                                          FIELDFONT=textfont,$
                                          TITLE=':',$
                                          UVALUE='Sky Flat Field',$
                                          XSIZE=15,$
                                          EVENT_PRO='xspextool_fields',$
                                          /CR_ONLY,$
                                          TEXTID=textid) 
               state.w.skyflat_fld = [flatimages,textid]

            combine_bg = cw_bgroup(col1_base,$
                                   FONT=buttonfont,$
                                   ['Weighted Mean','Mean','Median'],$
                                   /ROW,$
                                   /RETURN_NAME,$
                                   /NO_RELEASE,$
                                   /EXCLUSIVE,$
                                   LABEL_TOP='Combination Statistics:',$
                                   UVALUE='Sky Combination Statistic',$
                                   SET_VALUE=2)

            skyoname= coyote_field2(col1_base,$
                                    LABELFONT=buttonfont,$
                                    FIELDFONT=textfont,$
                                    TITLE='Sky Name:',$
                                    UVALUE='Full Sky Name',$
                                    XSIZE=15,$
                                    TEXTID=textid) 
            state.w.skyoname_fld = [skyoname,textid]

         mkflat_button = widget_button(col1_base,$
                                       FONT=buttonfont,$
                                       VALUE='Construct Super Sky',$
                                       UVALUE = 'Construct Super Sky',$
                                       /ALIGN_CENTER)
               
;  Combine Images base

      state.w.combineimgs_base = widget_base(work_base,$
                                             /COLUMN,$
                                             MAP=0)
      
         row_base = widget_base(state.w.combineimgs_base,$
                                /ROW)

            col1_base = widget_base(row_base,$
                                    /COLUMN,$
                                    /FRAME)
            
               bg = cw_bgroup(col1_base,$
                              FONT=buttonfont,$
                              ['A','A-B'],$
                              /ROW,$
                              /RETURN_NAME,$
                              /NO_RELEASE,$
                              /EXCLUSIVE,$
                              LABEL_LEFT='Reduction Mode:',$
                              UVALUE='Combine Reduction Mode',$
                              SET_VALUE=0)
               
               images = coyote_field2(col1_base,$
                                      LABELFONT=buttonfont,$
                                      FIELDFONT=textfont,$
                                      TITLE='Images:',$
                                      UVALUE='Combine Images',$
                                      XSIZE=70,$
                                      EVENT_PRO='xspextool_fields',$
                                      /CR_ONLY,$
                                      TEXTID=textid) 
               state.w.combimages_fld = [images,textid]
               
               state.w.combflat_base = widget_base(col1_base,$
                                                   /row,$
                                                   /base_align_center)
               
                  button = widget_button(state.w.combflat_base,$
                                         font=buttonfont,$
                                         VALUE='Full Flat Name',$
                                         UVALUE='Combine Flat Field Button',$
                                         EVENT_PRO = 'xspextool_event')
                  
                  flatimages = coyote_field2(state.w.combflat_base,$
                                          LABELFONT=buttonfont,$
                                          FIELDFONT=textfont,$
                                          TITLE=':',$
                                          UVALUE='Combine Flat Field',$
                                          XSIZE=15,$
                                          EVENT_PRO = 'xspextool_fields',$
                                          /CR_ONLY,$
                                          textid=textid) 
                  state.w.combflat_fld = [flatimages,textid]

               bgsub = cw_bgroup(col1_base,$
                                 FONT=buttonfont,$
                                 ['On','Off'],$
                                 /ROW,$
                                 /RETURN_NAME,$
                                 /NO_RELEASE,$
                                 /EXCLUSIVE,$
                                 LABEL_LEFT='BG Subtraction:',$
                                 UVALUE='Combine BG Subtraction',$
                                 SET_VALUE=1)
               
               combine_bg = cw_bgroup(col1_base,$
                                      FONT=buttonfont,$
                                      ['Weighted Mean','Mean','Median'],$
                                      /ROW,$
                                      /return_name,$
                                      /no_release,$
                                      /exclusive,$
                                      LABEL_TOP='Combination Statistics:',$
                                      UVALUE='Combine Combination Statistic',$
                                      SET_VALUE=2)

               comboname= coyote_field2(col1_base,$
                                        LABELFONT=buttonfont,$
                                        FIELDFONT=textfont,$
                                        TITLE='Output Name:',$
                                        UVALUE='Combine Output Name',$
                                        XSIZE=15,$
                                        textid=textid) 
               state.w.comboname_fld = [comboname,textid]

               combine_button = widget_button(col1_base,$
                                              FONT=buttonfont,$
                                              VALUE='Combine Images',$
                                              UVALUE='Combine Images')
         
;  Point Source Base

      state.w.ps_base = widget_base(work_base,$
                                    /COLUMN,$
                                    MAP=0)
      
         row1_base = widget_base(state.w.ps_base,$
                                 /ROW)


            col1_base = widget_base(row1_base,$
                                    /COLUMN,$
                                    /BASE_ALIGN_LEFT,$
                                    FRAME=1)
            
               label = widget_label(col1_base,$
                                    VALUE='1.  Find Apertures',$
                                    FONT=buttonfont,$
                                    /ALIGN_LEFT)

               mkprofile_button = widget_button(col1_base,$
                                                FONT=buttonfont,$
                                                VALUE='Make Spatial Profiles',$
                                               UVALUE='Make Spatial Profiles',$
                                                /ALIGN_CENTER)

               bg = cw_bgroup(col1_base,$
                              font=buttonfont,$
                              ['Manual','Import Coeffs'],$
                              /exclusive,$
                              /row,$
                              /return_name,$
                              /no_release,$
                              set_VALUE=0,$
                              UVALUE='PS Object Find Mode')
               
               base1 = widget_base(col1_base)

                  state.w.psapfindman_base = widget_base(base1,$
                                                         /column,$
                                                         /base_align_center,$
                                                         map=1)   
                  
                     Bg = cw_bgroup(state.w.psapfindman_base,$
                                    font=buttonfont,$
                                    ['Auto','Guess','Fix'],$
                                    /exclusive,$
                                    /row,$
                                    /return_name,$
                                    /no_release,$
                                    set_VALUE=0,$
                                    UVALUE='PS Manual Find Mode')

            state.w.psnaps_dl = widget_droplist(state.w.psapfindman_base,$
                                                font=buttonfont,$
                                                VALUE=['1','2','3','4'],$
                                                UVALUE='N Apertures',$
                                                TITLE='N Apertures:')
            widget_control, state.w.psnaps_dl,SET_DROPLIST_SELECT=1


            appos = coyote_field2(state.w.psapfindman_base,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='Apertures:',$
                                  UVALUE='PS Aperture Positions',$
                                  XSIZE=10,$
                                  VALUE='3,10',$
                                  textid=textid) 
            state.w.psappos_fld = [appos,textid]

                                                                            
                  state.w.psapfindtrace_base = widget_base(base1,$
                                                           /COLUMN,$
                                                           /BASE_ALIGN_RIGHT,$
                                                           MAP=0) 
                  
                     traceifile = coyote_field2(state.w.psapfindtrace_base,$
                                                LABELFONT=buttonfont,$
                                                FIELDFONT=textfont,$
                                                TITLE='Trace File:',$
                                                UVALUE='PS Trace File',$
                                                XSIZE=10,$
                                                VALUE='trace.dat',$
                                                textid=textid) 
                     state.w.psitracefile_fld = [traceifile,textid]
                     
               findaps_button = widget_button(col1_base,$
                                                 font=buttonfont,$
                                            VALUE='Find/Store Ap Positions',$ 
                                                 UVALUE='PS Find Positions',$
                                                 /align_center)        
                  
            state.w.pscol2_base = widget_base(row1_base,$
                                              /column,$
                                              /base_align_center,$
                                              frame=1)
        
               label = widget_label(state.w.pscol2_base,$
                                    font=buttonfont,$
                                    VALUE='2.  Choose Orders',$
                                    /align_center)

               state.w.psorder_bg = cw_bgroup(state.w.pscol2_base,$
                                              FONT=font,$
                                              strtrim(1,2),$
                                              /NONEXCLUSIVE,$
                                              /COLUMN,$
                                              /RETURN_NAME,$
                                              SET_VALUE=[1],$
                                              UVALUE='PS Orders')
               widget_control, state.w.psorder_bg,SENSITIVE=0

            state.w.pstrace_base = widget_base(row1_base,$
                                               /column,$              
                                               frame=1,$
                                               /base_align_center)
            
               label = widget_label(state.w.pstrace_base,$
                                    font=buttonfont,$
                                    VALUE='3.  Trace Objects',$
                                    /align_center)
               
               trace_button = widget_button(state.w.pstrace_base,$
                                            font=buttonfont,$
                                            VALUE='Trace Objects',$
                                            UVALUE='PS Trace Objects',$
                                            /align_center)
                  
               traceofile = coyote_field2(state.w.pstrace_base,$
                                          LABELFONT=buttonfont,$
                                          FIELDFONT=textfont,$
                                          TITLE='Filename:',$
                                          UVALUE='PS Trace Filename',$
                                          XSIZE=10,$
                                          textid=textid)
               state.w.psotracefile_fld = [traceofile,textid]

               writetrace = widget_button(state.w.pstrace_base,$
                                          font=buttonfont,$
                                          VALUE='Write Trace',$
                                          UVALUE='Write Trace',$
                                          /align_center)

            pscol4_base = widget_base(row1_base,$
                                      /column,$                                
                                      frame=1,$
                                      /base_align_right)
            
               label = widget_label(pscol4_base,$
                                    font=buttonfont,$
                                    VALUE='4.  Define Apertures',$
                                    /align_center)     

               fld = coyote_field2(pscol4_base,$
                                   LABELFONT=buttonfont,$
                                   FIELDFONT=textfont,$
                                   TITLE='PSF Radius:',$
                                   UVALUE='PSF Aperture',$
                                   VALUE='2.2',$
                                   XSIZE=8,$
                                   EVENT_PRO = 'xspextool_fields',$
                                   /CR_ONLY,$
                                   textid=textid)
               state.w.psfradius_fld = [fld,textid]                       
               
               apradius = coyote_field2(pscol4_base,$
                                        LABELFONT=buttonfont,$
                                        FIELDFONT=textfont,$
                                        TITLE='Ap Radius:',$
                                        UVALUE='PS Aperture',$
                                        VALUE='1.0',$
                                        XSIZE=8,$
                                        EVENT_PRO = 'xspextool_fields',$
                                        /CR_ONLY,$
                                        textid=textid)
               state.w.psapradius_fld = [apradius,textid]
            
               state.w.psbgsub_bg = cw_bgroup(pscol4_base,$
                                              /row,$
                                              label_left='BG Sub:',$
                                              font=buttonfont,$
                                              ['On','Off'],$
                                              /exclusive,$
                                              /return_name,$
                                              /no_release,$
                                              set_VALUE=0,$
                                              UVALUE='PS BG Subtraction')
               widget_control, state.w.psbgsub_bg, SENSITIVE=0
               
               state.w.psbginfo_base = widget_base(pscol4_base,$
                                                   /column,$
                                                   /base_align_right)

                     fld = coyote_field2(state.w.psbginfo_base,$
                                         LABELFONT=buttonfont,$
                                         FIELDFONT=textfont,$
                                         TITLE='Bg Start:',$
                                         UVALUE='PS Bg Start',$
                                         VALUE='2.2',$
                                         XSIZE=5,$
                                         EVENT_PRO = 'xspextool_fields',$
                                         /CR_ONLY,$
                                         textid=textid)
                     state.w.psbgstart_fld = [fld,textid]
                  
                     fld= coyote_field2(state.w.psbginfo_base,$
                                        LABELFONT=buttonfont,$
                                        FIELDFONT=textfont,$
                                        TITLE='BG Width:',$
                                        UVALUE='PS Bg Width',$
                                        VALUE='2',$
                                        XSIZE=5,$
                                        EVENT_PRO = 'xspextool_fields',$
                                        /CR_ONLY,$
                                        textid=textid)                
                     state.w.psbgwidth_fld  = [fld,textid]
                  
                  bgfitorder = coyote_field2(state.w.psbginfo_base,$
                                             LABELFONT=buttonfont,$
                                             FIELDFONT=textfont,$
                                             TITLE='Bg Fit Degree:',$
                                             UVALUE='PS Bg Fit Order',$
                                             XSIZE=4,$
                                             VALUE='0',$
                                             EVENT_PRO = 'xspextool_fields',$
                                             /CR_ONLY,$
                                             textID=textID)
                  state.w.psbgfitorder_fld = [bgfitorder,textID]
                  
               display_button = widget_button(pscol4_base,$
                                              font=buttonfont,$
                                              VALUE='Define Apertures',$
                                              UVALUE='PS Define Apertures',$
                                              /align_center)
               
         button = widget_button(state.w.ps_base,$
                                font=buttonfont,$
                                VALUE='Extract',$
                                UVALUE='PS Extract')




;  Extended Source Base

      state.w.xs_base = widget_base(work_base,$
                                    /column,$
                                    map = 0)

         row1_base = widget_base(state.w.xs_base,$
                                 /row)

            col1_base = widget_base(row1_base,$
                                    /column,$
                                    /base_align_center,$
                                    frame=1)
            
               label = widget_label(col1_base,$
                                    VALUE='1.  Find Aperture Positions',$
                                    font=buttonfont,$
                                    /align_center)

                mkprofile_button = widget_button(col1_base,$
                                                 font=buttonfont,$
                                                VALUE='Make Spatial Profiles',$
                                               UVALUE='Make Spatial Profiles',$
                                                 /align_center)

                bg = cw_bgroup(col1_base,$
                               font=buttonfont,$
                               ['Manual','Import Coeffs'],$
                               /exclusive,$
                               /row,$
                               /return_name,$
                               /no_release,$
                               set_VALUE=0,$
                               UVALUE='XS Object Find Mode')
                              
                base = widget_base(col1_base)

                   state.w.xsapfindman_base = widget_base(base,$
                                                          /column,$
                                                          /base_align_center,$
                                                          map=1) 

                   appos = coyote_field2(state.w.xsapfindman_base,$
                                         LABELFONT=buttonfont,$
                                         FIELDFONT=textfont,$
                                         TITLE='Positions',$
                                         UVALUE='XS Aperture Position',$
                                         VALUE='6,10',$
                                         XSIZE=10,$
                                         EVENT_PRO = 'xspextool_fields',$
                                         /CR_ONLY,$
                                         textid=textid)
                   state.w.xsappos_fld = [appos,textid]  
                   
                     
                  state.w.xsapfindtrace_base = widget_base(base,$
                                                           /column,$
                                                           /base_align_right,$
                                                           map=0) 
                  
                     traceifile = coyote_field2(state.w.xsapfindtrace_base,$
                                                LABELFONT=buttonfont,$
                                                FIELDFONT=textfont,$
                                                TITLE='Trace File:',$
                                                UVALUE='XS Trace File',$
                                                XSIZE=10,$
                                                VALUE='trace.dat',$
                                                textid=textid) 
                     state.w.xsitracefile_fld = [traceifile,textid]
                     
               findaps_button = widget_button(col1_base,$
                                                 font=buttonfont,$
                                                 VALUE='Store Ap Positions',$ 
                                                 UVALUE='XS Find Positions',$
                                                 /align_center) 

            state.w.xscol2_base = widget_base(row1_base,$
                                              /column,$
                                              /base_align_center,$
                                              frame=1)
        
               label = widget_label(state.w.xscol2_base,$
                                    font=buttonfont,$
                                    VALUE='2.  Choose Orders',$
                                    /align_center)
               
               state.w.xsorder_bg = cw_bgroup(state.w.xscol2_base,$
                                              FONT=font,$
                                              strtrim(1,2),$
                                              /NONEXCLUSIVE,$
                                              /COLUMN,$
                                              /RETURN_NAME,$
                                              SET_VALUE=[1],$
                                              UVALUE='XS Orders')
               widget_control, state.w.xsorder_bg,SENSITIVE=0

            state.w.xstrace_base = widget_base(row1_base,$
                                               /column,$              
                                               frame=1,$
                                               /base_align_center)
            
               label = widget_label(state.w.xstrace_base,$
                                    font=buttonfont,$
                                    VALUE='3.  Trace Objects',$
                                    /align_center)
               
               trace_button = widget_button(state.w.xstrace_base,$
                                            font=buttonfont,$
                                            VALUE='Trace Objects',$
                                            UVALUE='XS Trace Objects',$
                                            /align_center)
                  
               traceofile = coyote_field2(state.w.xstrace_base,$
                                          LABELFONT=buttonfont,$
                                          FIELDFONT=textfont,$
                                          TITLE='Filename:',$
                                          UVALUE='XS Trace Filename',$
                                          XSIZE=10,$
                                          textid=textid)
               state.w.xsotracefile_fld = [traceofile,textid]
               
               writetrace = widget_button(state.w.xstrace_base,$
                                          font=buttonfont,$
                                          VALUE='Write Trace',$
                                          UVALUE='Write Trace',$
                                          /align_center)

            xscol4_base = widget_base(row1_base,$
                                      /column,$                                
                                      frame=1,$
                                      /base_align_right)
            
               label = widget_label(xscol4_base,$
                                    font=buttonfont,$
                                    VALUE='4.  Define Apertures',$
                                    /align_center)  
                                           
               apradius = coyote_field2(xscol4_base,$
                                        LABELFONT=buttonfont,$
                                        FIELDFONT=textfont,$
                                        TITLE='Ap Radius:',$
                                        UVALUE='XS Aperture',$
                                        VALUE='1,1',$
                                        XSIZE=8,$
                                        EVENT_PRO = 'xspextool_fields',$
                                        /CR_ONLY,$
                                        textid=textid)
               state.w.xsapradius_fld = [apradius,textid]
               
               bg = cw_bgroup(xscol4_base,$
                              /row,$
                              label_left='BG Sub:',$
                              font=buttonfont,$
                              ['On','Off'],$
                              /exclusive,$
                              /return_name,$
                              /no_release,$
                              set_VALUE=0,$
                              UVALUE='XS BG Subtraction')
               
               state.w.xsbginfo_base = widget_base(xscol4_base,$
                                                   /column,$
                                                   /base_align_right)
               
                  bg = coyote_field2(state.w.xsbginfo_base,$
                                     LABELFONT=buttonfont,$
                                     FIELDFONT=textfont,$
                                     TITLE='Bg:',$
                                     UVALUE='XS Bg Regions',$
                                     VALUE='0-2,13-15',$
                                     XSIZE=15,$
                                     EVENT_PRO = 'xspextool_fields',$
                                     /CR_ONLY,$
                                     textid=textid)
                  state.w.xsbg_fld = [bg,textid]
                  
                  bgfitorder = coyote_field2(state.w.xsbginfo_base,$
                                             LABELFONT=buttonfont,$
                                             FIELDFONT=textfont,$
                                             TITLE='Bg Fit Degree:',$
                                             UVALUE='XS Bg Fit Order',$
                                             XSIZE=8,$
                                             VALUE='1',$
                                             EVENT_PRO = 'xspextool_fields',$
                                             /CR_ONLY,$
                                             textID=textID)
                  state.w.xsbgfitorder_fld = [bgfitorder,textID]
                  
               display_button = widget_button(xscol4_base,$
                                              font=buttonfont,$
                                              VALUE='Define Apertures',$
                                              UVALUE='XS Define Apertures',$
                                              /align_center)
               
         button = widget_button(state.w.xs_base,$
                                font=buttonfont,$
                                VALUE='Extract',$
                                UVALUE='XS Extract')

;  Other Base

   state.w.other_base = widget_base(work_base,$
                                   /row,$
                                   map=0)

      row_base = widget_base(state.w.other_base,$
                             /row)

         col1_base = widget_base(row_base,$
                                 /column,$
                                 /base_align_right,$
                                 frame=1)
         
            label = widget_label(col1_base,$
                                 VALUE='Array Parameters:',$
                                 font=buttonfont,$
                                 /align_left)
            
            start = coyote_field2(col1_base,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='Start Column:',$
                                  UVALUE='Start',$
                                  XSIZE=4,$
                                  EVENT_PRO = 'xspextool_returns',$
                                  /CR_ONLY,$
                                  textID=textid)
            state.w.start_fld = [start,textid]
            
            stop = coyote_field2(col1_base,$
                                 LABELFONT=buttonfont,$
                                 FIELDFONT=textfont,$
                                 TITLE='Stop Column:',$
                                 UVALUE='Stop',$
                                 XSIZE=4,$
                                 EVENT_PRO = 'xspextool_returns',$
                                 /CR_ONLY,$
                                 textID=textid)
            state.w.stop_fld = [stop,textid]

           fitorder = coyote_field2(col1_base,$
                                     LABELFONT=buttonfont,$
                                     FIELDFONT=textfont,$
                                     TITLE='Order Fit Deg:',$
                                     UVALUE='Poly Fit Order',$
                                     XSIZE=5,$
                                     VALUE=4,$
                                     textid=textid)
            state.w.polyfitorder_fld = [fitorder,textid]

            label = widget_label(col1_base,$
                                 VALUE='Trace Parameters:',$
                                 font=buttonfont,$
                                 /align_left)
            
            stepsize = coyote_field2(col1_base,$
                                     LABELFONT=buttonfont,$
                                     FIELDFONT=textfont,$
                                     TITLE='Trace Step Size:',$
                                     UVALUE='Trace Step Size',$
                                     VALUE='7',$
                                     XSIZE=4,$
                                     EVENT_PRO = 'xspextool_returns',$
                                     /CR_ONLY,$
                                     textID=textid)
            state.w.tracestepsize_fld = [stepsize,textid]
            
            sumap = coyote_field2(col1_base,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='Sumap:',$
                                  UVALUE='Sumap',$
                                  XSIZE=4,$
                                  VALUE=7,$
                                  EVENT_PRO = 'xspextool_fields',$
                                  /CR_ONLY,$
                                  textid=textid)
            state.w.sumap_fld = [sumap,textid]

            fld = coyote_field2(col1_base,$
                                LABELFONT=buttonfont,$
                                FIELDFONT=textfont,$
                                TITLE='Threshold:',$
                                UVALUE='Trace Threshold',$
                                XSIZE=4,$
                                VALUE=5,$
                                EVENT_PRO = 'xspextool_fields',$
                                /CR_ONLY,$
                                textid=textid)
            state.w.tracethresh_fld = [fld,textid]
                       
         col2_base = widget_base(row_base,$
                                 /column,$
                                 /base_align_right,$
                                 frame=1)
         



            label = widget_label(col2_base,$
                                 VALUE='Misc. Parameters:',$
                                 font=buttonfont,$
                                 /align_left)

               frac = coyote_field2(col2_base,$
                                    LABELFONT=buttonfont,$
                                    FIELDFONT=textfont,$
                                    TITLE='Flat Frac:',$
                                    UVALUE='Frac',$
                                    XSIZE=5,$
                                    VALUE=0.85,$
                                    textid=textid)
               state.w.frac_fld = [frac,textid]

               fld = coyote_field2(col2_base,$
                                   LABELFONT=buttonfont,$
                                   FIELDFONT=textfont,$
                                   TITLE='Bad Pix Thresh:',$
                                   UVALUE='Bad Pix Thresh',$
                                   XSIZE=5,$
                                   VALUE=7,$
                                   textid=textid)
               state.w.bdpxthresh_fld = [fld,textid]

                  bg = cw_bgroup(col2_base,$
                                 font=buttonfont,$
                                 [' Plot Saturated Pixels'],$
                                 /ROW,$
                                 /NONEXCLUSIVE,$
                                 /RETURN_NAME,$
                                 UVALUE='Plot Saturated Pixels',$
                                 SET_VALUE=1)        
                  
                  fld = coyote_field2(col2_base,$
                                      LABELFONT=buttonfont,$
                                      FIELDFONT=textfont,$
                                      TITLE='Sat. (DN):',$
                                      UVALUE='Saturation Level',$
                                      XSIZE=5,$
                                      VALUE=5000,$
                                      textid=textid)
                  state.w.saturation_fld = [fld,textid]    


                  bg = cw_bgroup(col2_base,$
                                 FONT=buttonfont,$
                                 ['Check Seeing'],$
                                 /ROW,$
                                 /NONEXCLUSIVE,$
                                 /RETURN_NAME,$
                                 UVALUE='Check Seeing',$
                                 SET_VALUE=state.r.checkseeing)        
                  
                  fld = coyote_field2(col2_base,$
                                      LABELFONT=buttonfont,$
                                      FIELDFONT=textfont,$
                                      TITLE='Seeing Thresh:',$
                                      UVALUE='Seeing Thresh',$
                                      XSIZE=5,$
                                      VALUE=3,$
                                      textid=textid)
                  state.w.seeingthresh_fld = [fld,textid]    

                  if state.w.general then begin
                     
                     state.r.plotsatpix = 0
                     widget_control, bg, SET_VALUE=0
                     
                  endif

                  psapsigns = coyote_field2(col2_base,$
                                            LABELFONT=buttonfont,$
                                            FIELDFONT=textfont,$
                                            TITLE='Ap Signs:',$
                                            UVALUE='PS Aperture Signs',$
                                            XSIZE=5,$
                                            textid=textid)
                  state.w.psapsigns_fld = [psapsigns,textid]
                  
                  button = widget_button(col2_base,$
                                         VALUE='Override Signs',$
                                         UVALUE='Override Signs',$
                                         FONT=buttonfont)


         col3_base = widget_base(row_base,$
                                 /column,$
                                 /base_align_left,$
                                 frame=1)

            label = widget_label(col3_base,$
                                 /ALIGN_LEFT,$
                                 VALUE='Reduction Steps:',$
                                 font=buttonfont)

            lc_bg = cw_bgroup(col3_base,$
                              font=buttonfont,$
                              ['Linearity Correction'],$
                              /ROW,$
                              /RETURN_NAME,$
                              /NONEXCLUSIVE,$
                              UVALUE='Linearity Correction',$
                              SET_VALUE=1)
            if state.w.general then widget_control, lc_bg, SET_VALUE=0
            if state.w.general then state.r.lc = 0

            errors_bg = cw_bgroup(col3_base,$
                                  font=buttonfont,$
                                  ['Error Propagation'],$
                                  /ROW,$
                                  /RETURN_NAME,$
                                  /NONEXCLUSIVE,$
                                  UVALUE='Error Propagation',$
                                  SET_VALUE=1)

            flat_bg = cw_bgroup(col3_base,$
                                font=buttonfont,$
                                ['Flat Fielding'],$
                                /row,$
                                /return_name,$
                                /NONexclusive,$
                                UVALUE='Flat Field',$
                                set_VALUE=1)


            flat_bg = cw_bgroup(col3_base,$
                                FONT=buttonfont,$
                                ['Fix Bad Pixels'],$
                                /ROW,$
                                /return_name,$
                                /nonexclusive,$
                                UVALUE='Fix Bad Pixels',$
                                set_VALUE=1)

            bg = cw_bgroup(col3_base,$
                           font=buttonfont,$
                           ['2D Extraction'],$
                           /ROW,$
                           /RETURN_NAME,$
                           /NONEXCLUSIVE,$
                           UVALUE='2D Extraction',$
                           SET_VALUE=0)

            state.w.optext_bg = cw_bgroup(col3_base,$
                                          font=buttonfont,$
                                          ['Optimal Extraction'],$
                                          /ROW,$
                                          /RETURN_NAME,$
                                          /NONEXCLUSIVE,$
                                          UVALUE='Optimal Extraction',$
                                          SET_VALUE=state.r.optextract)
            
            if not state.w.general then begin
            
               col4_base = widget_base(row_base,$
                                       /column,$
                                       /base_align_left,$
                                       frame=1)
               
               label = widget_label(col4_base,$
                                    /ALIGN_CENTER,$
                                    VALUE='Wave Cal Parameters:',$
                                    font=buttonfont)
               
               state.w.autoxcor_bg = cw_bgroup(col4_base,$
                                               font=buttonfont,$
                                               ['Auto X-Correlate'],$
                                               /ROW,$
                                               /RETURN_NAME,$
                                               /NONEXCLUSIVE,$
                                               UVALUE='Auto X-Correlate',$
                                               SET_VALUE=1)
               
               bg = cw_bgroup(col4_base,$
                              font=buttonfont,$
                              ['Plot X-Correlate'],$
                              /ROW,$
                              /RETURN_NAME,$
                              /NONEXCLUSIVE,$
                              UVALUE='Plot Auto X-Correlate',$
                              SET_VALUE=0)
               
               sigma = coyote_field2(col4_base,$
                                     LABELFONT=buttonfont,$
                                     FIELDFONT=textfont,$
                                     TITLE='Thresh:',$
                                     UVALUE='Wavecal Sigma Thresh',$
                                     XSIZE=5,$
                                     VALUE='4.0',$
                                     EVENT_PRO= 'xspextool_returns',$
                                     /CR_ONLY,$
                                     textid=textid)
               state.w.wavecalthresh_fld = [sigma,textid]
               
               wavecal_bg = cw_bgroup(col4_base,$
                                      font=buttonfont,$
                                      ['Plot Residuals'],$
                                      /row,$
                                      /nonexclusive,$
                                      /return_name,$
                                      UVALUE='Plot Residuals',$
                                      SET_VALUE=0)
               
               state.w.storedwc_bg = cw_bgroup(col4_base,$
                                               font=buttonfont,$
                                               ['Use Stored Solution'],$
                                               /row,$
                                               /nonexclusive,$
                                               /return_name,$
                                               UVALUE='Use Stored Solution',$
                                               SET_VALUE=0)
               
               
            endif
;  Eng Base
               
  state.w.eng_base = widget_base(work_base,$
                                 /ROW,$
                                 MAP=0)
  
      row_base = widget_base(state.w.eng_base,$
                             /ROW)

         col1_base = widget_base(row_base,$
                                 /COLUMN,$
                                 /BASE_ALIGN_RIGHT,$
                                 FRAME=1)


            label = widget_label(col1_base,$
                                 VALUE='Profile Parameters:',$
                                 font=buttonfont,$
                                 /align_left)

            fld = coyote_field2(col1_base,$
                                LABELFONT=buttonfont,$
                                FIELDFONT=textfont,$
                                TITLE='Ybuffer:',$
                                UVALUE='Ybuffer',$
                                VALUE='5',$
                                XSIZE=5,$
                                /CR_ONLY,$
                                textID=textid)
            state.w.ybuffer_fld = [fld,textid]


            fld = coyote_field2(col1_base,$
                                LABELFONT=buttonfont,$
                                FIELDFONT=textfont,$
                                TITLE='Oversampling:',$
                                UVALUE='Oversampling',$
                                XSIZE=5,$
                                VALUE=1,$
                                textid=textid)
            state.w.oversamp_fld = [fld,textid]

            fld = coyote_field2(col1_base,$
                                LABELFONT=buttonfont,$
                                FIELDFONT=textfont,$
                                TITLE='Mask Window:',$
                                UVALUE='Mask Window',$
                                XSIZE=5,$
                                VALUE=10,$
                                textid=textid)
            state.w.maskwindow_fld = [fld,textid]

            button = widget_button(col1_base,$
                                   FONT=buttonfont,$
                                   VALUE='Make Order Images',$
                                   UVALUE='Make Order Images')

            wavecal_bg = cw_bgroup(col1_base,$
                                   font=buttonfont,$
                                   ['Plot Line Finding'],$
                                   /row,$
                                   /nonexclusive,$
                                   /return_name,$
                                   UVALUE='Plot Line Finding',$
                                   SET_VALUE=0)
            



;            wavecal_bg = cw_bgroup(col4_base,$
;                                   FONT=buttonfont,$
;                                   ['Determine Solution'],$
;                                   /ROW,$
;                                   /nonexclusive,$
;                                   /return_name,$
;                                   UVALUE='Determine Solution',$
;                                   SET_VALUE=0)

;            state.w.wavebox_base = widget_base(col4_base,$
;                                               /COLUMN)

;               sigma = coyote_field2(state.w.wavebox_base,$
;                                     LABELFONT=buttonfont,$
;                                     FIELDFONT=textfont,$
;                                     TITLE='Thresh:',$
;                                     UVALUE='Wavecal Sigma Thresh',$
;                                     XSIZE=5,$
;                                     VALUE='4.0',$
;                                     EVENT_PRO= 'xspextool_returns',$
;                                     /CR_ONLY,$
;                                     textid=textid)
;               state.w.wavecalthresh_fld = [sigma,textid]

;               wavecal_bg = cw_bgroup(state.w.wavebox_base,$
;                                      font=buttonfont,$
;                                      ['Plot Residuals'],$
;                                      /row,$
;                                      /nonexclusive,$
;                                      /return_name,$
;                                      UVALUE='Plot Residuals',$
;                                      SET_VALUE=0)
;
;               if keyword_set(ENG) then begin
                   
;                   wavecal_bg = cw_bgroup(state.w.wavebox_base,$
;                                          font=buttonfont,$
;                                          ['Plot Line Finding'],$
;                                          /row,$
;                                          /nonexclusive,$
;                                          /return_name,$
;                                          UVALUE='Plot Line Finding',$
;                                          SET_VALUE=0)
;                   
;               endif
               
;               widget_control, state.w.wavebox_base,SENSITIVE=0
               
;        endif
        
;  Help base

;   state.w.help_base = widget_base(work_base,$
;                                   map=0,$
;                                   /column)

;   state.w.helptext = widget_text(state.w.help_base,$
;                                  /scroll,$
;                                  font='7x14',$
;                                  XSIZE=100,$
;                                  YSIZE=18)

; Get things running.  Center the widget using the Fanning routine.

centertlb,state.w.xspextool_base

widget_control, state.w.xspextool_base, /realize

; Start the Event Loop. This will be a non-blocking program.

XManager, 'xspextool', $
  state.w.xspextool_base, $
  /No_Block,$
  cleanup = 'xspextool_cleanup'

;  Load package path

xspextool_packagepath,state

; Put state variable into the user value of the top level base.

widget_control, state.w.xspextool_base, set_UVALUE=state, /no_copy


end
     
