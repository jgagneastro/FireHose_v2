pro psf_tvstack, stamps, window=wnum, min=min, max=max, bot=bot, top=top, $
                 status=status

  if n_elements(wnum) EQ 0 then wnum = 1
  if n_elements(min)  EQ 0 then min = -.03
  if n_elements(max)  EQ 0 then max =  .03
  if n_elements(top)  EQ 0 then top = 255
  if n_elements(bot)  EQ 0 then bot = 10
  s = psf_grid(stamps, 1, locs=locs, boxsize=boxsize)
  sz = size(s, /dimens)
  bx=sz[0];*2
  by=sz[1];*2

  sbig = rebin(s, bx, by, /sample)

  if (!d.window EQ wnum) AND (!d.x_size EQ bx) AND (!d.y_size EQ by) then begin 
;     erase
  endif else begin 
     window, wnum, xsize=bx, ysize=by
  endelse 

  tv, bytscl(sbig,min=min,max=max, top=(top-bot))+bot

  if n_elements(status) gt 0 then begin
     if n_elements(status) ne n_elements(locs[0,*]) then begin
        splog, 'status and locs arrays must agree in size, failing'
        return
     endif
     color = replicate('black', n_elements(status))
     w = where(status ne 0)
     if w[0] ne -1 then begin
        color[w] = 'red'
     endif
     for i=0l, n_elements(status)-1 do begin
        djs_xyouts, locs[0,i], locs[1,i], string(status[i], format='(I3)'), $
                    /normal, color=color[i]
     endfor
  endif

  return
end
