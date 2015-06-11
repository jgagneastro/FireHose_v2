;+
; NAME:
;   numlines() 
;
; PURPOSE:
;   Returns the number of lines in a file.
;
; CALLING SEQUENCE:
;   lines = numlines( infile [,/compress, /noexpand_path] )
;
; INPUT:
;   infile:      Input file name
;
; OPTIONAL INPUTS:
;   compress:      If set, assume the file is GZIP compressed.
;   noexpand_path: If set, no wildcard expansion will be
;                  performed on the filename.
;   
; OUTPUTS:
;   lines:       Number of lines in the file.
;
; PROCEDURES CALLED:
;   FILE_LINES()
;
; NOTE:
;   This is purely a wrapper on FILE_LINES(), but
;   is retained for backwards-compatibility
;
; REVISION HISTORY:
;   Replaced numlines which has been removed from Goddard, B. A. Weaver, 2012-06-21   
;-
FUNCTION numlines, infile, _EXTRA=extra
    ;
    ; Need 1 parameter
    ;
    IF N_PARAMS() LT 1 THEN BEGIN
        PRINT, 'Syntax - Lines = numlines( infile )'
        RETURN, -1
    ENDIF
    MESSAGE, 'You should be using the IDL built-in FILE_LINES() instead of numlines().', /INFORMATIONAL
    RETURN, FILE_LINES(infile,_EXTRA=extra)
END
