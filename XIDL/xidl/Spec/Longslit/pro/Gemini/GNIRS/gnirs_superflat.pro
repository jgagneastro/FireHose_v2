PRO gnirs_superflat, flatfiles, objfiles, superflatfile $
                     , XD32 = XD32 $
                     , VERBOSE = verbose, SHIFT8 = SHIFT8

if (N_params() LT 1) then begin
    print, "Syntax: gnirs_superflat,filename, [YEAR=, VERBOSE=, XD32=,]"
    return
endif

if not keyword_set(verbose) then verbose = 0

nflats = n_elements(flatfiles)      
lamparr   = strarr(nflats)
filtarr   = strarr(nflats)
gcalfiltarr = strarr(nflats)
exparr    = fltarr(nflats)

for j = 0, nflats-1 do begin
    hdr = xheadfits(flatfiles[j])
    gcalfilt  = sxpar(hdr, 'GCALFILT')
    lamp    = sxpar(hdr, 'GCALLAMP')
    exptime = sxpar(hdr, 'EXPTIME')
    filter  = sxpar(hdr, 'FILTER2')
    lamparr[j] = lamp
    filtarr[j] = filter
    exparr[j]  = float(exptime)
    gcalfiltarr[j] = gcalfilt
    if (verbose) then begin
        print, "lamp="+strtrim(lamp, 2)+', GCALfilt='+strtrim(gcalfilt, 2)+' filter='+strtrim(filter, 2)+' exptime='+strtrim(exptime, 2)+' '+flatfiles[j]
    endif
endfor
jd = 2400000.5D + double(sxpar(hdr, 'MJD_OBS'))
daycnv, jd, year, mn, day, hdr0
;; Kludge data with bad headers on 12-01-2004
IF double(sxpar(hdr, 'MJD_OBS')) EQ 0.0 THEN BEGIN
    year = 2004
    mn = 12
ENDIF
;          JFH 2006
IF KEYWORD_SET(XD32) THEN BEGIN
    IF year LE 2005 THEN BEGIN
        IF year EQ 2004 AND mn LT 11 THEN BEGIN
            red = WHERE((strtrim(gcalfiltarr) EQ 'ND3.0' OR $
                         strtrim(gcalfiltarr) EQ 'ND5.0') AND $
                        strtrim(lamparr)  EQ 'IRhigh', nred)
            ord5 = WHERE(strtrim(lamparr) EQ 'QH' AND $
                         strtrim(filtarr) EQ 'XD_G0507' AND $
                         exparr LE 1.0, n5)
            ord6 = ord5
            n6 = n5
            blue = WHERE(strtrim(lamparr) EQ 'QH' AND $
                         strtrim(filtarr) EQ 'XD_G0507' AND $
                         exparr GT 1.0, nblue)
            IF nblue EQ 0 THEN blue = ord5
            conv = '2004'
        ENDIF ELSE BEGIN
            red = WHERE((strtrim(gcalfiltarr) EQ 'ND3.0' OR $
                         strtrim(gcalfiltarr) EQ 'ND5.0') AND $
                        strtrim(lamparr)  EQ 'IRhigh', nred)
            ord5 =  WHERE(strtrim(lamparr) EQ 'QH' AND $
                          strtrim(filtarr) EQ 'J_G0505' AND $
                          exparr GT 1.0, n6)
            ord6 = WHERE(strtrim(lamparr) EQ 'QH' AND $
                         strtrim(filtarr) EQ 'X_G0506', nblue)
            blue = WHERE(strtrim(lamparr) EQ 'QH' AND $
                         strtrim(filtarr) EQ 'XD_G0507' AND $
                         exparr GT 1.0, nblue)
            conv = '2005'
        ENDELSE
    endif else  if (year EQ 2006) then begin
        ind = where(exparr EQ 0.8, nold)
        IF nold GT 0 THEN BEGIN ;   Old 2006 convention
            red  = where(exparr EQ 0.6, nred)
            if (nred EQ 0) then $
              red  = where(exparr EQ 0.8, nred)
            ord5 = where(exparr EQ 0.8, n5)
            ord6 = where(exparr EQ 1.1, n6)
            blue = where(exparr EQ 15.0, nblue)
            if (nblue EQ 0) then $
                  blue = ord6
            conv = '2006_old'
        ENDIF ELSE BEGIN        ; New 2006-2007 convention
            red = where(strmatch(lamparr, '*IRhigh*'))
            blue =  where(strmatch(lamparr, '*QH*'))
            ord5 = blue
            ord6 = blue
            conv = '2007'
        ENDELSE
    ENDIF
ENDIF
; ENDIF ELSE if (year EQ 2005) then begin
; ;; this is from simcoe not sure if it is still valid????    
;     red  = where(strtrim(lamparr) EQ "IRhigh" AND $
;                  strtrim(filtarr) EQ "XD_G0507", nred)
;     blue = where(strtrim(lamparr) EQ "QH" AND $
;                  strtrim(filtarr) EQ "XD_G0507", nblue)
;     ord5 = where(strtrim(lamparr) EQ "QH" AND $
;                  strtrim(filtarr) EQ "J_G0505", n5)
;     ord6 = where(strtrim(lamparr) EQ "QH" AND $
;                  strtrim(filtarr) EQ "X_G0506", n6)
;     conv = '2005_rob'


;    ENDIF ELSE BEGIN
;        red  = where(exparr EQ 2.1, nred)
;        if (nred EQ 0) then $
;          red  = where(exparr EQ 0.6, nred)
;        ord5 = where(exparr EQ 0.6, n5)
;        ord6 = where(exparr EQ 4.5, n6)
;        blue = where(exparr EQ 15.0, nblue)
;        if (nblue EQ 0) then $
;          blue = ord6
;    ENDELSE

if (verbose) then begin
    print, "Red flats:"
    for j = 0, nred-1 do begin
        print, flatfiles[red[j]]
    endfor
    
    print, "Blue flats:" 
    for j = 0, nblue-1 do begin
        print, flatfiles[blue[j]]
    endfor
    
    print, "Order 5 flats:" 
    for j = 0, n5-1 do begin
        print, flatfiles[ord5[j]]
    endfor
    
    print, "Order 6 flats:" 
    for j = 0, n6-1 do begin
        print, flatfiles[ord6[j]]
    endfor
    
endif

gnirs_mkflat, flatfiles[blue], flatfiles[red] $
              , flatfiles[ord5], flatfiles[ord6], objfiles $
              , tset_slits = tset_slits $
              , flat = flat, verbose = verbose, CHK = CHK, CONV = CONV $
              , SHIFT8 = SHIFT8

print, "gnirs_superflat: Writing output: ", superflatfile
mwrfits, flat, superflatfile, /create
mwrfits, tset_slits, superflatfile
print, "gnirs_superflat: all done!"

end


