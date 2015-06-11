;+ 
; NAME:
;  wfc3_g280_qa_trace
;
; PURPOSE:
;   This code generates QA for the tracing algorithm.
;
; CALLING SEQUENCE:
;   
;  wfc3_g280_qa_trace, psfile, trace, specim, NAME=
;
; INPUTS:
;   trace  -- Trace structure
;   specim -- 2D spectral image
;
; RETURNS:
;
; OUTPUTS:
;   psfile -- QA file
;
; OPTIONAL KEYWORDS:
;   NAME= -- Name printed on the figure [default: 'QSO/']
;
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;
; EXAMPLES:
;  wfc3_g280_qa_trace, psfile, trace, specim, NAME=name
;
; PROCEDURES CALLED:
;  plotting stuff
;
; REVISION HISTORY:
;   23-Dec-2010 Written by JXP/JMO
;------------------------------------------------------------------------------
pro wfc3_g280_qa_trace, psfile, trace, specim, NAME=name 

  if (N_params() LT 3) then begin 
     print, 'Syntax - ' + $
            'wfc3_g280_qa_trace, psfile, trace, specim, NAME= [v1.0]'
    return
  endif 

  if not keyword_set(NAME) then name = 'QSO'

  ;; PSFILE
  x_psopen, psfile, /maxs

  ;; Image
  xmn = min(trace.trace_xa, max=xmx)
  xmnx = round([xmn,xmx])
  ymn = min(trace.trace_ya_orig, max=ymx)
  YBUFF = 10L
  ymn = ymn-YBUFF*3
  ymx = ymx+YBUFF
  ymnx = round([ymn,ymx])
  img_cut = specim[xmnx[0]:xmnx[1], ymnx[0]:ymnx[1]]
  szc = size(img_cut,/dimen)

  devicefactor=2540. ;; cm to inch
  imsize = 9.0       ;; inch
  irange = [-10, 300.]
  
  xpos1 = 1.0
  ypos1 = 2.
  
  ;; Linear
  if not keyword_set(XNCOLORS) then xncolors=200L
  scaled = bytscl(img_cut, min=irange[0], max=irange[1], $
                  top=(xncolors - 1))
     
  ;; Plot
  ctload, 0, ncolors=xncolor, /rever
  tv, scaled, xpos1, ypos1, xsize=imsize, /inches

  ;; Axes
  dims = size(scaled,/dim)
  xlabel = findgen(dims[0]+1)
  ylabel = findgen(dims[1]+1)
  thisPosition = devicefactor*[xpos1, ypos1, $
                               xpos1+imsize, $
                               ypos1+(imsize*dims[1]/dims[0])]
  ctload, 0, ncolors=xncolor
  plot, xlabel, ylabel, /nodata, /device, /noerase, $
        position=thisPosition, $
        xrange=[min(xlabel),max(xlabel)], $
        yrange=[min(ylabel),max(ylabel)], $
        xstyle=5, ystyle=5
  plot, [0], [0], /device, /noerase, xrange=xmnx, $
        yrange=ymnx, xtitle='Column', charsiz=csz2, ytickn=yspaces, $
        ytitle='Row', /nodata, xsty=1, ysty=1, ytickint=10, $
        position=thisPosition ;, ytickname=['-2','-1','0','+1','+2']
  
  clr = getcolor(/load)
  oplot, trace.trace_xa, trace.trace_ya_orig, color=clr.red, thick=1, linest=1
  oplot, trace.trace_xa, trace.trace_yafit, color=clr.cyan, thick=1, linest=2

  ;; Label
  xyouts, 0.5, 0.6, NAME+' Trace', color=clr.black, charsiz=2., /norma, align=0.5 
  x_psclose

  return
end
