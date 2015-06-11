;+
;NAME:
; HEALNPIX()
;PURPOSE:
; Returns the number of healpix pixels for a given resolution.
;CALLING SEQUENCE:
; npix = healnpix(res,[/nside])
;KEYWORDS:
; If nside is set, then it returns nside
;REVISION HISTORY:
; Nikhil Padmanabhan, Princeton, July 29,2003
; Revised documentation, B. A. Weaver, NYU, 2011-11-30
;-
FUNCTION healnpix, res, nside=nside
    IF (N_ELEMENTS(res) EQ 0) THEN MESSAGE,'ERROR : Set resolution'
    side = 2L^res
    IF KEYWORD_SET(nside)) THEN RETURN, side $
    ELSE BEGIN
        npix = 12L*side*side
        RETURN, npix
    ENDELSE
END
