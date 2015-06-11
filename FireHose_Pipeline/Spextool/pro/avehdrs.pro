;+
; NAME:
;     avehdrs
;
; PURPOSE:
;     Averages the values of the PA, HA, airmass and time in a FITS header.
;
; CATEGORY:
;     File I/O
;
; CALLING SEQUENCE:
;     result = avehdrs(hdrs,TIME_OBS=time_obs,POSANGLE=posangle,$
;                      HA=ha,AIRMASS=airmass,CANCEL=cancel)
;
; INPUTS:
;     hdrs - An array of structures containing the keywords and values
;            of FITS headers created using gethdrinfo.pro
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     TIME_OBS - The time obs keyword
;     POSANGLE - The position angle keyword
;     HA       - The hour angle keyword
;     AIRMASS  - The airmass keyword
;     CANCEL   - Set on return if there is a problem
;
; OUTPUTS:
;     Returns a single structure of FITS header keywords and values 
;     with the average values of TIME_OBS, POSANGLE, HA, and AIRMASS
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
;     Averages the parameters requested by the user.  A keyword of 
;     'None' indicates the parameter does not exist.  The values of 
;     keywords other than the ones noted above are taken from the 
;     first structure.
;     
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2001-03-30 - Written by M. Cushing, Institute for Astronomy, UH
;     2005-07-04 - Modified to accept new hdr structures from gethdrinfo
;-
function avehdrs,hdrs,TIME_OBS=time_obs,POSANGLE=posangle,HA=ha,$
                 AIRMASS=airmass,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 1 then begin
    
    print, 'Syntax -  result = avespexhdrs(hdr,TIME_OBS=time_obs,$'
    print, '                               POSANGLE=posangle,HA=ha,$'
    print, '                               AIRMASS=airmass,CANCEL=cancel'
    cancel = 1
    return, -1

endif
cancel = cpar('avehdrs',hdrs,1,'Hdrs',8 ,1)
if cancel then return, -1

;  Get setup info

n = n_elements(hdrs.vals)
names = tag_names(hdrs[0].vals)

gotimeobs  = 0
goha       = 0
goposangle = 0
goairmass  = 0

if n_elements(TIME_OBS) ne 0 then begin

    if strlowcase(TIME_OBS) eq 'none' then goto, cont1 
    zt = total(where(names eq strupcase(TIME_OBS),count))
    if count ne 1 then goto, cont1    

    values = dblarr(3,n)
    for i = 0, n-1 do values[*,i] = double(strsplit(hdrs[i].vals.(zt),':', $
                                                    /EXTRACT))

;  Check to see if the times are bracketed around midnight.
    
    zbot = where(values[0,*] eq 23.0,countbot)
    ztop = where(values[0,*] eq 0.0,counttop)

    if countbot ne 0 and counttop ne 0 then begin

        values[*,ztop]=values[*,ztop] + 24.
        result = tenv(values[0,*],values[1,*],values[2,*])
        ave = total(result)/float(n)
        if ave gt 24. then ave = ave - 24.

    endif else begin

        result = tenv(values[0,*],values[1,*],values[2,*])
        ave = total(result)/float(n)

    endelse 

    result = sixty(ave)
    newTIME_OBS = string(fix(result[0]),FORMAT='(i2.2)')+':'+$
      string(fix(result[1]),FORMAT='(i2.2)')+':'+strcompress(result[2],/RE)

    gotimeobs = 1

    cont1:

endif
if n_elements(POSANGLE) ne 0 then begin

    if strlowcase(POSANGLE) eq 'none' then goto, cont2 

    zp = total(where(names eq strupcase(POSANGLE),count))
    if count ne 1 then goto, cont2
    if size(hdrs[0].vals.(zp),/TYPE) eq 7 then goto, cont2
    pos  = hdrs[*].vals.(zp)

;  Check to see if it is centered around 0.

    zbot = where(fix(pos) eq 359,countbot)
    ztop = where(fix(pos) eq 0,counttop)

    if counttop ne 0 and countbot ne 0 then begin
   
        pos[ztop] = pos[ztop]+360.
        newPOSANGLE = total(pos)/float(n)
        if newPOSANGLE gt 360. then newPOSANGLE = newPOSANGLE - 360.

    endif else newPOSANGLE = total(pos)/float(n)

    goposangle = 1

    cont2:

endif

if n_elements(HA) ne 0 then begin

    if strlowcase(HA) eq 'none' then goto, cont3

    zh = total(where(names eq strupcase(HA),count))
    if count ne 1 then goto, cont3
    values = dblarr(3,n)
    pm     = intarr(n)

    for i = 0, n-1 do begin

        values[*,i] = double(strsplit(hdrs[i].vals.(zh),':',/EXTRACT))
        pm[i] = (strmid(hdrs[i].vals.(zh),0,1) eq '-') ? -1:1

    endfor

    result = tenv(values[0,*],values[1,*],values[2,*])*float(pm)
    ave = total(result)/float(n)
    pm = (ave lt 0) ? '-':'+'
    result = sixty(abs(ave))

    newHA = pm+string(fix(result[0]),FORMAT='(i2.2)')+':'+$
      string(fix(result[1]),FORMAT='(i2.2)')+':'+strcompress(result[2],/RE)
    
    goha = 1

    cont3:

endif

if n_elements(AIRMASS) ne 0 then begin

    if strlowcase(AIRMASS) eq 'none' then goto, cont4

    za = total(where(names eq strupcase(AIRMASS),count))
    if size(hdrs[0].vals.(za),/TYPE) eq 7 then goto, cont4
    if count ne 1 then goto, cont4
    newAIRMASS = total(hdrs[*].vals.(za))/float(n)
    goairmass = 1
    cont4:

endif

newhdr = hdrs[0]

if gotimeobs then begin

    newhdr.vals.(zt) = newTIME_OBS
    newhdr.coms.(zt) = ' Average '+strtrim(newhdr.coms.(zt),2)

endif

if goposangle then begin

    newhdr.vals.(zp) = newPOSANGLE
    newhdr.coms.(zp) = ' Average '+strtrim(newhdr.coms.(zp),2)

endif

if goha then begin

    newhdr.vals.(zh) = newHA
    newhdr.coms.(zh) = ' Average '+strtrim(newhdr.coms.(zh),2)


endif

if goairmass then begin

    newhdr.vals.(za) = newAIRMASS
    newhdr.coms.(za) = ' Average '+strtrim(newhdr.coms.(za),2)

endif

return, newhdr

end

