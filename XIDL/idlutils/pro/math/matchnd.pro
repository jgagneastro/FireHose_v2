;+
; NAME:
;   matchnd
; PURPOSE:
;   match two sets of points in N dimensions
; CALLING SEQUENCE:
;   matchnd, x1, x2, distance [, m1=, m2=, d12=, nmatch= ]
; INPUTS:
;   x1 - [M,N1] positions in M-dimensions
;   x2 - [M,N2] positions in M-dimensions
;   distance - match distance
; OPTIONAL INPUTS:
;   maxmatch - MRB: please explain!
;   /silent  - don't splog anything
; OUTPUTS:
;   m1 - [nmatch] matches to x1
;   m2 - [nmatch] matches to x2
;   d12 - [nmatch] distance between matches
;   nmatch - number of matches
; COMMENTS:
;   This code is BETA! Use at your own risk.
; REVISION HISTORY:
;   12-Oct-2005  Written by Mike Blanton, NYU
;-
;------------------------------------------------------------------------------
pro matchnd, x1, x2, distance, m1=m1, m2=m2, d12=d12, nmatch=nmatch, $
             maxmatch=maxmatch, nd=nd, silent=silent

if(n_elements(maxmatch) eq 0) then maxmatch=1

if((size(x1))[0] eq 1) then begin
    if(NOT keyword_set(nd)) then begin
        mm=1
        nn1=n_elements(x1)
        x1=reform(x1, 1, nn1)
    endif else begin
        mm=nd
        nn1=n_elements(x1)/mm
        x1=reform(x1, mm, nn1)
    endelse 
endif else begin
    mm=(size(x1,/dim))[0]
    nn1=(size(x1,/dim))[1]
endelse

nn2=n_elements(x2)/mm
x2=reform(x2, mm, nn2)

ibad=where(finite(x1) eq 0, nbad)
if(nbad gt 0) then $
  message, 'Infinite x1 values in matchnd!'
ibad=where(finite(x2) eq 0, nbad)
if(nbad gt 0) then $
  message, 'Infinite x2 values in matchnd!'

maxdiff=-1.
for i=0L, mm-1L do begin
    if(max(x1[i,*])-min(x1[i,*]) gt maxdiff) then $
      maxdiff=max(x1[i,*])-min(x1[i,*])
endfor
maxn=long((10.*n_elements(x1)+n_elements(x2))^(1./float(mm))) > $
  (long(1000000.^(1./float(mm))))
binsize=(maxdiff/float(maxn)) > (distance)
if (not keyword_set(silent)) then splog, binsize

distance2=distance^2

gridnd, x1, ix=ix1, binsize=binsize, grid=grid1, ngrid=ngrid1, nx=nx, $
  igrid=igrid1, xminmax=xminmax, nd=nd
gridnd, x2, ix=ix2, binsize=binsize, grid=grid2, ngrid=ngrid2, nx=nx, $
  igrid=igrid2, xminmax=xminmax, nd=nd

ifilled=where(ngrid1 gt 0, nfilled)

tmpnmatch=0L
tmpm1=-1L
tmpm2=-1L
tmpd12=-1.
for i=0L, nfilled-1L do begin

    ;; get ii1, the index in each dimension of this cell
    i1tmp=*(grid1[ifilled[i]])
    ii1=lonarr(mm)
    itmp=ifilled[i]
    for j=0L, mm-1L do begin
        ii1[j]=itmp mod nx[j]
        itmp=itmp/nx[j]
    endfor

    ;; now loop over adjacent cells in this dimension

    ;;   what are ranges in each dim?
    iist=(ii1-1L) > 0L
    iind=(ii1+1L) < (nx-1L)

    ;;   how many are there?
    nii=1L
    for j=0L, mm-1L do $
      nii=(iind[j]-iist[j]+1L)*nii

    ;; now actually loop
    ii=lonarr(mm)
    ii2=lonarr(mm)
    for j=0L, nii-1L do begin

        ;; find the grid number for the adjacent cell
        igrid2=0L
        ibase=1L
        for k=0L, mm-1L do begin
            igrid2=igrid2+ibase*(ii2[k]+iist[k])
            ibase=ibase*nx[k]
        endfor
        
        ;; now collect the actual matches 
        if(ngrid2[igrid2] gt 0) then begin 
            i2tmp=*(grid2[igrid2])
            for k=0L, ngrid2[igrid2]-1L do begin
                dtmp=fltarr(mm, n_elements(i1tmp))
                for l=0L, mm-1L do $
                  dtmp[l,*]=(x1[l,i1tmp]-x2[l,i2tmp[k]])
                dtmp2=total(dtmp^2,1)
                imtmp=where(dtmp2 lt distance2, nmtmp)
                if(nmtmp gt 0) then begin
                    if(tmpnmatch gt 0) then begin
                        tmpm1=[tmpm1, i1tmp[imtmp]]
                        tmpm2=[tmpm2, replicate(i2tmp[k], nmtmp)]
                        tmpd12=[tmpd12, sqrt(dtmp2[imtmp])]
                    endif else begin
                        tmpm1=i1tmp[imtmp]
                        tmpm2=replicate(i2tmp[k], nmtmp)
                        tmpd12=sqrt(dtmp2[imtmp])
                    endelse
                    tmpnmatch=tmpnmatch+nmtmp
                endif
            endfor
        endif

        ;; now update to next cell
        iidim=0L
        nnx=iind-iist+1L
        ii2[iidim]=(ii2[iidim]+1L) mod nnx[iidim]
        while(ii2[iidim] eq 0 and iidim lt mm-1L) do begin
            iidim=iidim+1L
            ii2[iidim]=(ii2[iidim]+1L) mod nnx[iidim]
        endwhile
    endfor
endfor

;; finally, sort by match distance and trim
if(tmpnmatch gt 0) then begin
    isort=sort(tmpd12)
    if(maxmatch gt 0) then begin
        gotten1=lonarr(nn1)
        gotten2=lonarr(nn2)
        nmatch=0L
        for j=0L, tmpnmatch-1L do begin
            if((gotten1[tmpm1[isort[j]]] lt maxmatch) AND $
               (gotten2[tmpm2[isort[j]]] lt maxmatch)) then begin
                gotten1[tmpm1[isort[j]]]=gotten1[tmpm1[isort[j]]]+1L
                gotten2[tmpm2[isort[j]]]=gotten2[tmpm2[isort[j]]]+1L
                nmatch=nmatch+1L
            endif
        endfor
        gotten1=lonarr(nn1)
        gotten2=lonarr(nn2)
        m1=lonarr(nmatch)
        m2=lonarr(nmatch)
        d12=fltarr(nmatch)
        nmatch=0L
        for j=0L, tmpnmatch-1L do begin
            if((gotten1[tmpm1[isort[j]]] lt maxmatch) AND $
               (gotten2[tmpm2[isort[j]]] lt maxmatch)) then begin
                gotten1[tmpm1[isort[j]]]=gotten1[tmpm1[isort[j]]]+1L
                gotten2[tmpm2[isort[j]]]=gotten2[tmpm2[isort[j]]]+1L
                m1[nmatch]=tmpm1[isort[j]]
                m2[nmatch]=tmpm2[isort[j]]
                d12[nmatch]=tmpd12[isort[j]]
                nmatch=nmatch+1L
            endif
        endfor
    endif else begin
        nmatch=n_elements(tmpm1)
        m1=tmpm1[isort]
        tmpm1=0
        m2=tmpm2[isort]
        tmpm2=0
        if(arg_present(d12)) then d12=tmpd12[isort]
        tmpd12=0
    endelse
endif else begin
    m1=-1
    m2=-1
    d12=0.
    nmatch=0
endelse

;; free the grids!
for i=0L, n_elements(grid1)-1L do $
  ptr_free, grid1[i]
grid1=0
for i=0L, n_elements(grid2)-1L do $
  ptr_free, grid2[i]
grid2=0

end
