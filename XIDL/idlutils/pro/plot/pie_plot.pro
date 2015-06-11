;+
; NAME:
;   pie_plot
; PURPOSE:
;   given r and theta, plot a pie diagram
; CALLING SEQUENCE:
;   pie_plot, radius, theta [, rrange=, thrange=, keywords for djs_plot]
; INPUTS:
;   radius - [N] the radius (arbitrary units)
;   theta - [N] the angle (degrees)
; OPTIONAL INPUTS:
;   rrange - [2] range of radius (default [0., 1.05*max(radius)])
;   thrange - [2] range of angle (deg; default [0.,360.])
;   thaxes - [Naxes] angle(s) to put radial axes at
;   saxes - [Naxes] +1 or -1, desired sidedness of axes
;   rlabel - label to put on r axis (default none)
;   rotate - rotate plot by this angle in deg (default 0.)
; OPTIONAL KEYWORDS:
;   /hours - mark ticks in hours, not degrees
;   /over - overplot (no axes)
;   /nodata - do not plot the data
;   /plots - use plots, not djs_oplot, for points
; COMMENTS:
;   Gets rid of initial axes by plotting in white. (Better way?)
;   Only tested with PostScript device.
; BUGS:
;   Alpha code!
; REVISION HISTORY:
;   5-Aug-2008 MRB, NYU
;-
;------------------------------------------------------------------------------
pro pie_plot, radius, theta, rrange=rrange, thrange=thrange, $
              hours=hours, rlabel=rlabel, _EXTRA=_extra_for_djs_oplot, $
              thaxes=thaxes, saxes=saxes, over=over, rotate=rotate, $
              nodata=nodata, plots=plots, color=color, rcharsize=rcharsize, $
              thcharsize=thcharsize, charthick=charthick, axisthick=axisthick

if(NOT keyword_set(nrticks)) then $
  nrticks=5L
if(NOT keyword_set(rrange)) then $
  rrange=[0., max(radius)*1.05]
if(NOT keyword_set(rlabel)) then $
  rlabel=''
if(NOT keyword_set(rcharsize)) then $
  rcharsize=1.3
if(NOT keyword_set(thcharsize)) then $
  thcharsize=1.3
if(NOT keyword_set(rotate)) then $
  rotate=0.
if(NOT keyword_set(thrange)) then $
  thrange=[0.,360.]

if(NOT keyword_set(over)) then begin
    !X.OMARGIN=0.
    !Y.OMARGIN=0.

;; set up plot (but in white)
    loadct,0
    plot, [0], [0], color=255, /nodata, $
      xra=[-1.,1.]*rrange[1]*1.1, yra=[-1., 1.]*rrange[1]*1.1

    for irad=0L, 1L do begin

        if(rrange[irad] gt 0.) then begin

            if(irad eq 0) then $
              signtick=-1. $
            else $
              signtick=1.
            
            ;; overplot circle
            ncirc=1000L
            ang= dindgen(ncirc)/(double(ncirc)-1.)*(thrange[1]-thrange[0])+$
              thrange[0]
            x= rrange[irad]* cos((ang+rotate)*!DPI/180.D)
            y= rrange[irad]* sin((ang+rotate)*!DPI/180.D)
            djs_oplot, x, y, thick=axisthick
            
            ;; small tick marks
            firsttick= 0L
            lasttick= 360L
            smalltick= 15L
            stick= 0.02*signtick
            for ang= firsttick, lasttick, smalltick do begin
                fang= float(ang)
                if(fang ge thrange[0] and fang le thrange[1]) then begin
                    x1= rrange[irad]* cos((fang+rotate)*!DPI/180.)
                    y1= rrange[irad]* sin((fang+rotate)*!DPI/180.)
                    x2= rrange[irad]*(1.-stick)* cos((fang+rotate)*!DPI/180.)
                    y2= rrange[irad]*(1.-stick)* sin((fang+rotate)*!DPI/180.)
                    djs_oplot, [x1, x2], [y1, y2], thick=axisthick
                endif
            endfor

            ;; big tick marks
            bigtick= 60L
            btick= 0.05*signtick
            roffset= 0.05*signtick
            for ang= firsttick, lasttick-0.01*bigtick, bigtick do begin
                fang= float(ang)
                if(fang ge thrange[0] and fang le thrange[1]) then begin
                    x1= rrange[irad]* cos((fang+rotate)*!DPI/180.)
                    y1= rrange[irad]* sin((fang+rotate)*!DPI/180.)
                    x2= rrange[irad]*(1.-btick)* cos((fang+rotate)*!DPI/180.)
                    y2= rrange[irad]*(1.-btick)* sin((fang+rotate)*!DPI/180.)
                    x3= rrange[irad]*(1.+roffset)* cos((fang+rotate)*!DPI/180.)
                    y3= rrange[irad]*(1.+roffset)* sin((fang+rotate)*!DPI/180.)
                    djs_oplot, [x1, x2], [y1, y2], thick=axisthick
                    if(NOT keyword_set(hours)) then $
                      tmark=textoidl(strtrim(string(ang),2)+'^\circ') $
                    else $
                      tmark=textoidl(strtrim(string(long(ang/15.)),2)+'h')
                    if(irad eq 1) then $
                      djs_xyouts, [x3], [y3], tmark, $
                      /noclip, orientation= fang+rotate-90., align=0.5, $
                      charsize=thcharsize, charthick=charthick
                endif
            endfor
        endif
    endfor

;; plot sides
    if((thrange[1] mod 360.) ne (thrange[0] mod 360.) AND $
       n_elements(thaxes) eq 0) then begin
        thaxes=thrange
        saxes=[1., -1]
    endif 
    if(n_elements(thaxes) gt 0) then begin
        if(n_elements(saxes) eq 0) then $
          saxes=replicate(1., n_elements(thaxes))
        for ith=0L, n_elements(thaxes)-1L do begin
            signtick= saxes[ith]
            x1= rrange*cos((thaxes[ith]+rotate)*!DPI/180.)
            y1= rrange*sin((thaxes[ith]+rotate)*!DPI/180.)
            djs_oplot, x1, y1, thick=axisthick
            tickint= hogg_interval(rrange, nticks=nrticks)
            if(alog10(tickint) lt 0) then begin
                digit= long(-alog10(tickint))
                tformat='(f60.'+strtrim(string(digit+1L),2)+')'
            endif else begin
                digit= long(alog10(tickint))-1L
                tformat='(i)'
            endelse 
            if(ith eq 0) then $
              offtick=-0.25 $
            else $
              offtick=1.50*(1.+0.40*digit)
            btick=0.02*rrange[1]
            roffset= 0.05*rrange[1]
            eps=(rrange[1]-rrange[0])*0.01
            for rtick=0., rrange[1], tickint do begin
                if(rtick gt rrange[0]+eps and rtick lt rrange[1]-eps) then begin
                    x1= rtick*cos((thaxes[ith]+rotate)*!DPI/180.)
                    y1= rtick*sin((thaxes[ith]+rotate)*!DPI/180.)
                    x2= $
                      rtick*cos(((thaxes[ith]+rotate)+signtick*1.)*!DPI/180.)
                    y2= $
                      rtick*sin(((thaxes[ith]+rotate)+signtick*1.)*!DPI/180.)
                    dr= sqrt((x1-x2)^2+(y1-y2)^2)
                    dx= (x2-x1)/dr
                    dy= (y2-y1)/dr
                    djs_oplot, x1+[0., dx]*btick, y1+[0., dy]*btick, $
                      thick=axisthick
                    x3= (rtick-rrange[1]*0.015)* $
                      cos(((thaxes[ith]+rotate)+signtick*1.)*!DPI/180.)
                    y3= (rtick-rrange[1]*0.015)* $
                      sin(((thaxes[ith]+rotate)+signtick*1.)*!DPI/180.)
                    x4= x3-dx*roffset*(1.+offtick)
                    y4= y3-dy*roffset*(1.+offtick)
                    rmark= strtrim(string(rtick, format=tformat),2)
                    djs_xyouts, [x4], [y4], rmark, $
                      /noclip, orientation= thaxes[ith]+rotate-90., $
                      charsize=rcharsize, charthick=charthick
                endif
            endfor
            rtick= 0.5*(rrange[1]+rrange[0])
            x1= rtick*cos((thaxes[ith]+rotate)*!DPI/180.)
            y1= rtick*sin((thaxes[ith]+rotate)*!DPI/180.)
            x2= rtick*cos(((thaxes[ith]+rotate)+signtick*1.)*!DPI/180.)
            y2= rtick*sin(((thaxes[ith]+rotate)+signtick*1.)*!DPI/180.)
            dr= sqrt((x1-x2)^2+(y1-y2)^2)
            dx= (x2-x1)/dr
            dy= (y2-y1)/dr
            x3= (rtick-rrange[1]*0.015)* $
              cos(((thaxes[ith]+rotate)+signtick*1.)*!DPI/180.)
            y3= (rtick-rrange[1]*0.015)* $
              sin(((thaxes[ith]+rotate)+signtick*1.)*!DPI/180.)
            x4= x3-dx*roffset*4.
            y4= y3-dy*roffset*4.
            djs_xyouts, x4, y4, rlabel, /noclip, charsize=rcharsize, $
              orientation= thaxes[ith]+rotate, align=0.5, charthick=charthick
        endfor
    endif
endif 

;; plot points
if(NOT keyword_set(nodata)) then begin
    iin=where(radius le rrange[1] and radius ge rrange[0] and $
              theta le thrange[1] and theta ge thrange[0], nin)
    if(nin gt 0) then begin
        xdata= radius* cos((theta+rotate)*!DPI/180.)
        ydata= radius* sin((theta+rotate)*!DPI/180.)
        if(keyword_set(plots)) then $
          plots, xdata[iin], ydata[iin], color=color[iin], $
          _EXTRA=_extra_for_djs_oplot $
        else $
          djs_oplot, xdata[iin], ydata[iin], color=color, $
          _EXTRA=_extra_for_djs_oplot
    endif
endif

end
;------------------------------------------------------------------------------
