FUNCTION is_it_number, instring, good_string=good_string

;+
; NAME: 
;   READ_IPAC_TABLE
;
; PURPOSE: determine whether a string contains a valid numeric value that can 
;     be converted to another type with e.g. FIX
;
; EXPLANATION:  Failed type conversion (attempting to convert a non-numeric string
;     to e.g. a FLOAT) will crash a program at run time.  IS_IT_NUMBER will check 
;     the string in advance to determine if it contains a valid numeric value for conversion.
;
; CALLING SEQUENCE:  answer = is_it_number(instring, [good_string=good_string])
;
; INPUTS:  
;     INSTRING -- the string to be evaluated
;
; OPTIONAL INPUT:
;     GOOD_STRING -- a string value that is good for another reason.  If the string does
;     not contain a numeric value, but does match GOOD_STRING, then a value of FALSE
;     is returne as 2 rather than 0
;
; OUTPUTS: 
;     ANSWER --  0 = does not contain a valid number for convserion
;                1 = contains a valid number for conversion
;                2 = does not contain a valid number for conversion but does match GOOD_STRING exactly
;
; EXAMPLES: 
;
; PROCEDURES USED:
;
; NOTES:
;
; MODIFICATION HISTORY:
;      Harry Teplitz   September 2010 


;;;;  check to see if a string contains a valid number for conversion 
;;;;  by IDL functions (e.g. FLOAT).  Return 1 if yes, 0 if no
;;;;
;;;;  OPTIONAL INPUTS
;;;;
;;;;  GOOD_STRING:  if set, this keyword defines a non-numeric string that will case the 
;;;                 routine to return a value of 2, instead of 0.
 
; Declare error label. 

on_ioerror, BAD_NUMBER

test = float(instring)

; If we get here, then the it is a number, so return 1

return, 1

; Exception label.  If we get here it is not a number
; so return 0

BAD_NUMBER:    IF keyword_set(good_string) THEN BEGIN 
   IF instring EQ good_string THEN return, 2 ELSE return, 0
ENDIF $
   ELSE return, 0

END
