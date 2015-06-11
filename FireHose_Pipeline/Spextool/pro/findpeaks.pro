;+
; NAME:
;     findpeaks
;
; PURPOSE:
;     Identifies the peaks in the spatial profile of a point source.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     findpeaks,profiles,npeaks,doorders,positions,apsign,AUTO=auto,$
;               GUESS=guess,FIXED=fixed,CANCEL=cancel
;
; INPUTS:
;     profiles - An array [norders] of structures where 
;                struc = [[arcseconds],[data]]
;     npeaks   - Number of peaks to find
;     doorders - An array [norders] of 1s and 0s.  If 0, the findpeaks
;                skips the order
;     
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     AUTO   - Automatically find the peaks
;     GUESS  - An array [naps,norders] of the guess positions in units of x
;     FIXED  - An array [naps,norders] of the fixed positions in units of x
;     CANCEL - Set on return if there is a problem
;     
; OUTPUTS:
;     positions - An array with the positions of the peaks
;     apsign    - An array with +1 if the peak is positive and -1 if
;                 the peak is negative.
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
;     Three modes: 1)  Automatically finds the peaks by fitting
;     gaussian around the maximum pixel in abs(profiles),
;     subtracting the resulting fit off and itterating until npeaks
;     peaks are found.  2) Manually will fit gaussians to the guess
;     positions.  3) Simply fixes the apertures to the guess positions.
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2000-04-11 - Written by M. Cushing, Institute for Astronomy, UH
;     2001-01-09 - Fixed bug which would not allow the program to work
;                  on a single order.
;     2002-11-20 - Modified profiles structure in accordance with 
;                  new spextool definition.
;     2004-03-31 - Modified to remove NaNs.
;     2005-07-01 - Added gaussian subtraction to GUESS mode
;     2006-06-09 - Modified definition of guess and fixed inputs so
;                  that each order has a guess or fixed set of values
;-
pro findpeaks,profiles,npeaks,doorders,positions,apsign,AUTO=auto,GUESS=guess,$
              FIXED=fixed,CANCEL=cancel
;
;  Check parameters

if n_params() lt 3 then begin
    
    print, 'Syntax - findpeaks,profiles,npeaks,doorders,positions,apsign,$'
    print, '                   AUTO=auto,GUESS=guess,FIXED=fixed,CANCEL=cancel'
    cancel = 1
    return

endif
cancel = cpar('findpeaks',profiles,1,'Profiles',8,[0,1])
if cancel then return
cancel = cpar('findpeaks',npeaks,2,'Npeaks',[2,3,4,5],0)
if cancel then return
cancel = cpar('findpeaks',doorders,3,'Doorders',[2,3,4,5],[0,1])
if cancel then return

norders = n_tags(profiles)

if n_elements(GUESS) ne 0 then begin

    positions = replicate(!values.f_nan,npeaks,norders)
    apsign    = intarr(npeaks,norders)
    for i = 0, norders-1 do begin
        
        if doorders[i] eq 0 then goto, cont1
        profile = profiles.(i)
        x = profile[*,0]
;        y = abs(profile[*,1])
        y = profile[*,1]

        weights = replicate(1.,n_elements(y))
        for j =0,npeaks-1 do begin
            
            tabinv, x, guess[j,i], xpix

            result = mpfitpeak(x,y,fit,NTERMS=4, $
                               ESTIMATES=[y[xpix],guess[j,i],0.5,0])

;            window, 2
;            plot,x,y,/XSTY,/YSTY
;            oplot,x,result,COLOR=2
;            re = ' '
;            read, re

            positions[j,i] = fit[1]
            tabinv, x, fit[1], xpix
            apsign[j,i]    = (y[xpix] > 0 ) ? 1:(-1)
            y = y - (fit[3]+gaussian(x,fit[0:2]))
            
        endfor
        cont1:

    endfor
    
endif
if keyword_set(AUTO) then begin
    
    positions = replicate(!values.f_nan,npeaks,norders)
    apsign    = intarr(npeaks,norders)
    for i = 0, norders-1 do begin
        
        if doorders[i] eq 0 then goto, cont2
        profile = profiles.(i)
        x = profile[*,0]
        y = abs(profile[*,1])
        z = where(finite(y) eq 1)
        x = x[z]
        y = y[z]

        
        weights = replicate(1.,n_elements(y))
        for j = 0, npeaks-1 do begin

            max = max(y,xmax)


            result = mpfitpeak(x,y,fit,NTERMS=4,ESTIMATES=[max,x[xmax],1,0])
            positions[j,i] = fit[1]

            tabinv, x, fit[1], xpix
            apsign[j,i] = (profile[xpix,1] > 0 ) ? 1:(-1)
            y = y - (fit[3]+gaussian(x,fit[0:2]))


        endfor
        cont2:
        
    endfor

endif

if n_elements(FIXED) ne 0 then begin

    positions = replicate(!values.f_nan,npeaks,norders)
    apsign = intarr(npeaks,norders)
    for i = 0, norders-1 do begin

        if doorders[i] eq 0 then goto, cont3
        profile = profiles.(i)
        x = profile[*,0]
        for j = 0, npeaks-1 do begin

            positions[j,i] = fixed[j,i]
            tabinv, x, fixed[j,i], xpix
            apsign[j,i] = (profile[xpix,1] > 0 ) ? 1:(-1)

        endfor
        cont3:

    endfor

endif

for i = 0, norders-1 do begin

    if doorders[i] eq 1 then begin

        order = sort(positions[*,i])
        positions[*,i] = positions[order,i]
        apsign[*,i]  = apsign[order,i]

    endif

endfor

junk = where(doorders eq 1,count)
if norders gt 1 then apsign = total(apsign,2)/long(count) 
for i = 0, npeaks-1 do apsign[i] = (apsign[i] ge 0) ? 1:-1

end





