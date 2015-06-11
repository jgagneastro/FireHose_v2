FUNCTION add_rotation, ww,ss,vsini

;  This function adds vsini rotational broading consistent
;  with a standard Gray rotational broadening profile.
; INPUT SPECTRA MUST BE IN ANGSTROMS!!!!
;  from Russel White, Dec. 2000, then Greg Doppmann, Jul 2003.

; don't do any rotational broadening for 0 or negative rotations
IF (vsini LE 0) THEN BEGIN
	ssm = ss
	RETURN, ssm
ENDIF

;ROTATIONAL BROADENING WITH VELOCITY,V (sin i) and NRES*2 points in the
;rotational profile, (there are nres pts per Doppler width)
;spectrum,SS and wave. WW, smoothed spectrum in SSM
;note:  do not use too wide a wavelength span at one time (1000 Dop. wids)
;PLOT:  1 on, 0 off
;**programers' note: the explosion at the end of ssm is a problem
;Oct-94 GB Modified to change NRES default to increase with V

;definitions
epsilon = 0.6		;set default values - epsilon = .6 (normal limb darkening)
n_res = MAX([10,vsini]) ; - n_res (# of points in model rotational profile) = 10 or vsini, whichever is larger. 
c = 2.998E5             ; - c = speed of light in km/sec

;learn stuff about the spectra and profile we'll be using
n_lambda = N_ELEMENTS(ww)       ; figure out the size of our spectra   
start_lambda = ww(0)            ; figure out the first element in our spectra
dlam = ww(n_lambda/2)*vsini/c   ; figure out the maximum doppler shift possible for this vsini
dc = n_res/dlam                 ; determine the linear density of points in the profile
span = ww(n_lambda-1)-ww(0)     ; determine the width of the spectrum we're looking at
n_proper = long(span*dc)       ; figure out the number of points in a full spectrum
                                ; that has n_res points in 1 doppler width.

;set up input spectra with proper wavelength resolution
IF n_proper LE n_lambda THEN BEGIN               ; test to see if proper res. spectra is smaller than input spectra
    w=ww  & s=ss                                 ; if smaller, copy the input spectra
ENDIF ELSE BEGIN                                 ; if proper res spectra is bigger than input spectra ...
    in=FINDGEN(n_proper) 
    w=start_lambda+in/dc ; make a finer wavelength scale for input spectra
    s = INTERPOL(ss,ww,w)			         ;interpolate flux onto finer wavelength scale
ENDELSE

;pad out ends of the proper spectra so broadening right at the ends of the input spectra can occur
sec1 = fltarr(n_res + 2)                        ;make an array two points wider than the rotation profile
sec1 = sec1 + RANDOMN(seed,n_res + 2)*1.e-8	;call randomn to make tiny offsets to each location
sec2 = sec1                                     ;copy the randomness
sec1 = sec1 + s(0)                              ;make an array of randomness centered on the first flux value
sec2 = sec2 + s(n_proper-1)                     ;as above but for the last flux value in the proper spectra
s = [sec1,s,sec2]                               ;add the randomness to each end of the proper spectra

;set up stuff to store profile information
n_proper = N_ELEMENTS(s)                        ;reflect the new length of the proper spectra
nw = n_res + 2                                  ;store the length of the 'wings' added to the proper spectra
sum = 0.0                                       ;setup a counter term to aid in normalizing the profile
pr = FLTARR(nw)                                 ;make an array to store half of the symmetric rotation profile

;***compute ROTATIONAL BROADENING FUNCTION (Gray)
con1 = 2*(1.-epsilon)/(!pi*dlam*(1.-epsilon/3.));calculate constant 1
con2 = epsilon/(2.*(1.-epsilon/3.)*dlam)        ;calculate constant 2
;WARNING***the doppler width is not adjusted for shifting central lambda
FOR n=0,nw-1 DO BEGIN                           ;calculate height of the profile at each point in its width
    dl = n/dc                                   ;distance from zero doppler shift
    dls = 1.-(dl/dlam)*(dl/dlam)                ;make an r^2 relationship
    IF dls LE 0. THEN dls = 1.e-10              ;set negative values to insanely small positives
    pr(n) = con1 *sqrt(dls) + con2 * dls        ;calculate the profile itself
    sum = sum + pr(n) * 2                       ;crudely integrate under the profile width
ENDFOR

;normalize the profile
sum = sum - pr(0)         ;subtract off the initial value to finish integration
pr = pr/sum               ;normalize the profile
cen = pr(0)               ;define height of the center of the profile
prf = pr(1:nw-1)          ;make an array for one half of the profile
prb = REVERSE(prf)        ;flip the half of the profile we calculated around
rotpr = [prb,cen,prf]     ;assemble the total profile
npr = N_ELEMENTS(rotpr)   ;define the width of the profile

;***CONVOLVE WITH SPECTRUM
sm = convol(s,rotpr)			;call convol

;***clip back to original spectrum
	sm = sm(n_res + 2:n_proper-n_res-3)
	ssm = sm(0:n_lambda-2)
	nw = ww(0:n_lambda-2)
	ssm = spline(w,sm,ww)
	if keyword_set(pl) then begin
		plot,ww,ss
		oplot,ww,ssm-.5
		!p.multi=0
            endif

RETURN, ssm

END
