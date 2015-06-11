;+
; NAME:
;  hogg_healpix_greyscale
; PURPOSE:
;  make a greyscale plot from a healpix array
; BUGS:
;  - Assumes "ring" ordering.
;-
pro hogg_healpix_greyscale, map,filename,histeq=histeq
npix= n_elements(map)
nside= round(sqrt(npix/12.0))
if (npix NE (12L*nside*nside)) then begin
    splog, 'ERROR: number of pixels in map makes no sense'
    splog, npix,nside
    stop
endif

if keyword_set(histeq) then begin
    mapcolor= bytarr(npix)
    mapcolor[sort(map)]= floor(255.999*findgen(npix)/float(npix))
endif else begin
    mapcolor= floor(255.999*(map-min(map))/(max(map)-min(map)))
endelse

set_plot, 'ps'
xsize= 7.5 & ysize= xsize/2.0
device, file=filename,/inches,xsize=xsize,ysize=ysize, $
  xoffset=(8.5-xsize)/2.0,yoffset=(11.0-ysize)/2.0,bits=8
hogg_plot_defaults
loadct,0,/silent
xrange=[0,2]
!Y.RANGE=[0,1]
!X.OMARGIN=[0,0]
!Y.OMARGIN=[0,0]

xxpix= fltarr(npix)
xbighalf= (xrange[1]-xrange[0])/8.0
xhalf= xbighalf/float(nside)
yypix= fltarr(npix)
ybighalf= (!Y.RANGE[1]-!Y.RANGE[0])/4.0
yhalf= ybighalf/float(nside)
nring= nside*4L-1L
kk= 0L
for rr=0L,nring-1L do begin
    if (rr LT nside) then begin
        npixinring= 4L*(rr+1)
        for qq=0,3 do begin
            xxpix[kk+qq*rr+qq:kk+qq*rr+qq+rr]= xrange[0]+xbighalf $
              +qq*2.0*xbighalf-rr*xhalf+findgen(rr+1)*2.0*xhalf
        endfor
    endif else if (rr LT 3L*nside-1) then begin
        npixinring= 4L*nside
        xxpix[kk:kk+npixinring-1]= xrange[0] $
          +((rr-nside) mod 2)*xhalf+findgen(npixinring)*2.0*xhalf
    endif else begin
        pp= (nring-rr-1)
        npixinring= 4L*(pp+1)
        for qq=0,3 do begin
            xxpix[kk+qq*pp+qq:kk+qq*pp+qq+pp]= xrange[0]+xbighalf $
              +qq*2.0*xbighalf-pp*xhalf+findgen(pp+1)*2.0*xhalf
        endfor
    endelse
    yypix[kk:kk+npixinring-1]= !Y.RANGE[1] $
      +(!Y.RANGE[0]-!Y.RANGE[1])*(float(rr)+1.0)/float(nring+1)
    kk= kk+npixinring
endfor

plot, xxpix,yypix,/nodata, $
  xrange=reverse(minmax(xxpix)+[-xhalf,xhalf]),xstyle=!X.STYLE+4, $
  ystyle=!Y.STYLE+4,/isotropic
for pp=0L,npix-1L do begin
    xx= [-1,0,1,0,-1]*xhalf+xxpix[pp]
    yy= [0,1,0,-1,0]*yhalf+yypix[pp]
    polyfill, xx,yy,color=mapcolor[pp]
    oplot, xx,yy,psym=0,thick=0.25,/noclip
endfor

device, /close
return
end
