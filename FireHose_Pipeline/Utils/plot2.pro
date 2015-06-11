Function plot2, x, y, MARGIN=margin, XTICKLEN=xticklen, YTICKLEN=yticklen, _EXTRA=extra, NIRSPECTRA=nirspectra, OPTSPECTRA=optspectra, XTITLE=xtitle, YTITLE=ytitle, NOSETXRANGE=nosetxrange, NOSETYRANGE=nosetyrange, XRANGE=xrange, YRANGE=yrange, ALLX=allx, ALLY=ally
  if keyword_set(optspectra) then begin
    if ~keyword_set(xtitle) then $
      xtitle = 'Wavelength ($\AA$)'
    if ~keyword_set(ytitle) then $
      ytitle = 'Relative Flux (F$_\lambda$)'
  endif
  if keyword_set(nirspectra) then begin
    if ~keyword_set(xtitle) then $
      xtitle = 'Wavelength ($\mu$m)'
    if ~keyword_set(ytitle) then $
      ytitle = 'Relative Flux (F$_\lambda$)'
  endif
  if ~keyword_set(xrange) and ~keyword_set(nosetxrange) then begin
    if ~keyword_set(allx) then allx = x
    xrange = [min(allx,/nan),max(allx,/nan)]
    xrange += [-1,1]*(max(xrange)-min(xrange))*.05
  endif
  if ~keyword_set(yrange) and ~keyword_set(nosetyrange) then begin
    if ~keyword_set(ally) then ally = y
    yrange = [min(ally,/nan),max(ally,/nan)]
    yrange += [-1,1]*(max(yrange)-min(yrange))*.05
  endif

  if ~keyword_set(margin) then margin = [.12,.1,.023,.023]
  ;if margin eq !NULL then margin = [.12,.1,.023,.023]
  if ~keyword_set(xticklen) then xticklen = .01
  if ~keyword_set(yticklen) then yticklen = .01
  return, plot(x, y, MARGIN=margin, XTICKLEN=xticklen, YTICKLEN=yticklen, _EXTRA=extra, XTITLE=xtitle, YTITLE=ytitle, XRANGE=xrange, YRANGE=yrange)
End