;+
; NAME:
;   yanny_free
;
; PURPOSE:
;   Free memory allocated from reading a Yanny parameter file
;
; CALLING SEQUENCE:
;   yanny_free, pdata
;
; INPUTS:
;   pdata      - Array of pointers to all strucutures read by YANNY_READ.
;
; OPTIONAL INPUTS:
;
; OUTPUT:
;
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;
; EXAMPLES:
;
; BUGS:
;
; PROCEDURES CALLED:
;
; REVISION HISTORY:
;   15-Nov-1999  Written by David Schlegel, Princeton.
;-
;------------------------------------------------------------------------------
PRO yanny_free, pdata

    IF (N_PARAMS() LT 1) THEN BEGIN
        PRINT, 'Syntax - yanny_free, pdata'
        RETURN
    ENDIF

    IF ~KEYWORD_SET(pdata) THEN RETURN

    FOR i=0, N_ELEMENTS(pdata)-1 DO PTR_FREE, pdata[i]

    pdata = 0

    RETURN
END
;------------------------------------------------------------------------------
