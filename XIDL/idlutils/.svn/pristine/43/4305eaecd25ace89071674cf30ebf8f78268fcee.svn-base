;+
; NAME:
;   hogg_unquoted_regex
; PURPOSE:
;   return the regex which matches the first occurence of the given
;     regex not inside quotemarks
; INPUT:
;   regex      - naked regular expression
; OPTIONAL INPUT:
;   quotemark  - thing to use as the quotation mark, default to '"'
; BUGS:
;   quotemark is not actually used.
;   This function may not really work as advertised at all.
; REVISION HISTORY:
;   2002-10-11  written - Hogg
;   2013-11-27  code cleanup - BAW
;-
FUNCTION hogg_unquoted_regex, regex, quotemark=quotemark
    IF ~KEYWORD_SET(quotemark) THEN quotemark='"'
    RETURN, '('+regex+')((.*(\".*\")+.*$)|([^\"]*$))'
END
