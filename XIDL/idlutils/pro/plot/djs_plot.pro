;-----------------------------------------------------------------------
;+
; NAME:
;   djs_plot
;
; PURPOSE:
;   Modified version of PLOT
;
; CALLING SEQUENCE:
;   djs_plot, [x,] y, [bin= ]
;
; INPUT:
;   x:
;   y:
;
; OPTIONAL KEYWORDS:
;   bin        - If set, then plot an evenly-spaced subsample of BIN points,
;                or 100 points if BIN=1
;   rightaxis, leftaxis, topaxis, bottomaxis 
;              - if set, draws labels on the specified axis/axes BUT 
;                DRAWS NO OTHERS
; OUTPUTS:
;
; COMMENTS:
;   Pass COLOR, PSYM, and SYMSIZE to djs_oplot.
;
; PROCEDURES CALLED:
;   djs_oplot
;   TeXtoIDL()
;
; REVISION HISTORY:
;   Written by D. Schlegel, 27 September 1997, Durham
;   bin keyword added, 26 March 2008 - D. Finkbeiner
;-
;-----------------------------------------------------------------------
pro djs_plot, x, y, xtitle=xtitle, ytitle=ytitle, title=title, $
 color=color, psym=psym, symsize=symsize, nodata=nodata, $
 bin=bin, rightaxis=rightaxis, topaxis=topaxis, leftaxis=leftaxis, $
 bottomaxis=bottomaxis, xcharsize=xcharsize, ycharsize=ycharsize, $
 _EXTRA=KeywordsForPlot

   if(n_elements(xcharsize) gt 0) then $
     tmp_xcharsize=xcharsize
     
   if(n_elements(ycharsize) gt 0) then $
     tmp_ycharsize=ycharsize

   if(keyword_set(rightaxis) gt 0 OR $
      keyword_set(leftaxis) gt 0 OR $
      keyword_set(topaxis) gt 0 OR $
      keyword_set(bottomaxis) gt 0) then begin
       tmp_xcharsize=0.0001
       tmp_ycharsize=0.0001
   endif

   ; If X values don't exist, then create them as PLOT or OPLOT would do
   npt = N_elements(x)
   if (n_elements(y) GT 0) then begin
      xtmp = x
      ytmp = y
   endif else begin
      xtmp = lindgen(npt)
      ytmp = x
   endelse

   if (keyword_set(xtitle)) then xtitle_tex = TeXtoIDL(xtitle)
   if (keyword_set(ytitle)) then ytitle_tex = TeXtoIDL(ytitle)
   if (keyword_set(title)) then title_tex = TeXtoIDL(title)

;   plot, xtmp, ytmp, xtitle=xtitle_tex, ytitle=ytitle_tex, $
;    title=title_tex, _EXTRA=KeywordsForPlot
   plot, xtmp, ytmp, xtitle=xtitle_tex, ytitle=ytitle_tex, $
    title=title_tex,xcharsize=tmp_xcharsize, ycharsize=tmp_ycharsize, $ 
    _EXTRA=KeywordsForPlot, /nodata

   if(keyword_set(rightaxis)) then $
     axis, !X.CRANGE[1], !Y.CRANGE[0], yaxis=1, $
           ytitle=ytitle_tex, ycharsize=ycharsize, $
           _EXTRA=KeywordsForPlot
   if(keyword_set(leftaxis)) then $
     axis, !X.CRANGE[0], !Y.CRANGE[0], yaxis=0, $
           ytitle=ytitle_tex, ycharsize=ycharsize, $
           _EXTRA=KeywordsForPlot
   if(keyword_set(topaxis)) then $
     axis, !X.CRANGE[0], !Y.CRANGE[1], xaxis=1, $
           xtitle=xtitle_tex, xcharsize=xcharsize, $
           _EXTRA=KeywordsForPlot
   if(keyword_set(bottomaxis)) then $
     axis, !X.CRANGE[0], !Y.CRANGE[0], xaxis=0, $
           xtitle=xtitle_tex, xcharsize=xcharsize, $
           _EXTRA=KeywordsForPlot

   if (NOT keyword_set(nodata)) then $
    djs_oplot, xtmp, ytmp, color=color,psym=psym, symsize=symsize, $
     bin=bin, _EXTRA=KeywordsForPlot

   return
end 
;-----------------------------------------------------------------------
