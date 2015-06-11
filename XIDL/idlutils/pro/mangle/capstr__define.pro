;+
; NAME:
;   capstr__define
; PURPOSE:
;   Create the structure for a cap
; CALLING SEQUENCE:
;   poly={capstr}
; INPUTS:
; OPTIONAL INPUTS:
; OUTPUTS:
; OPTIONAL INPUT/OUTPUTS:
; COMMENTS:
; EXAMPLES:
; BUGS:
; PROCEDURES CALLED:
; VERSION:
;   $Id: capstr__define.pro 122754 2011-03-28 15:24:24Z weaver $
; REVISION HISTORY:
;   2011-03-27 Written by B. A. Weaver (NYU)
;-
;------------------------------------------------------------------------------
PRO capstr__define
    foo = {capstr, x:DBLARR(3), cm:0.0D}
    RETURN
END
