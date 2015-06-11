;+
; NAME:
;   struct_print
;
; PURPOSE:
;   Formatted print of a structure to standard out, a file, or an array.
;
; CALLING SEQUENCE:
;   struct_print, struct, [ lun=, filename=, tarray=, css=, /no_head, /html, $
;    fdigit=, ddigit=, alias=, formatcodes= ]
;
; INPUTS:
;   struct     - Structure
;
; OPTIONAL INPUTS:
;   filename   - Output file name; open and close this file
;   lun        - LUN number for an output file if one is already open
;                (overrides FILENAME)
;   no_head    - Do not print the header lines that label the columns,
;                and do not increase the width of a column to occomodate
;                the column name.
;   html       - If set, then output as an XHTML table
;   fdigit     - Number of digits for type FLOAT numbers; default to 5.
;   ddigit     - Number of digits for type DOUBLE numbers; default to 7.
;   alias      - Set up aliases to convert from the IDL structure
;                to the FITS column name.  The value should be
;                a STRARR(2,*) value where the first element of
;                each pair of values corresponds to a column
;                in the structure and the second is the name
;                to be used in the FITS file.
;                The order of the alias keyword is compatible with
;                use in MWRFITS,MRDFITS.
;   formatcodes- Explicit format codes for specific structure elements.
;                The value should be a STRARR(2,*) value where teh first
;                element of each pair of values corresponds to a column
;                name (before applying any ALIAS), and the second is the
;                format code, such as "a10" for a 10-character string,
;                or "f10.5" for a floating-point value with 5 places after
;                the decimal point.  Note that this may truncate the
;                names in the two-line header if the format is fewer
;                characters than that name length.
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;   tarray     - String array for output
;   css        - String array containing CSS information to make the
;                XHTML table look nice.
;
; COMMENTS:
;   If neither FILENAME or LUN is set and TARRAY is not returned,
;   then write to the standard output.
;
; EXAMPLES:
;
; BUGS:
;   If FORMATCODES is used, then it is possible to have numeric values
;   that do not fit within the specified format, which are then printed
;   as asterisks.  For example, printing the value 123.4 with the format
;   code "f3.1" will result in printing "***".
;
; PROCEDURES CALLED:
;
; INTERNAL SUPPORT ROUTINES:
;   struct_checktype()
;
; REVISION HISTORY:
;   01-Nov-2000  Written by David Schlegel, Princeton.
;   2009-12-04 Now emits valid XHTML 1.1 tables.
;-
;------------------------------------------------------------------------------
;
; This code is copied from MWR_CHECKTYPE from within "mwrfits.pro".
;
FUNCTION struct_checktype, tag, alias=alias
    IF ~KEYWORD_SET(alias) THEN RETURN, tag
    sz = SIZE(alias)
    ;
    ; 1 or 2 D string array with first dimension of 2
    ;
    IF (sz[0] EQ 1 || sz[1] EQ 2) && sz[1] EQ 2 && sz[sz[0]+1] EQ 7 THEN BEGIN
        w = WHERE(tag EQ alias[0,*])
        IF (w[0] EQ -1) THEN RETURN, tag $
        ELSE RETURN, alias[1,w[0]]
    ENDIF ELSE BEGIN
        PRINT, 'Warning: Alias values not strarr(2) or strarr(2,*)'
    ENDELSE
    RETURN, tag
END
;
;------------------------------------------------------------------------------
;
PRO struct_print, struct, filename=filename, lun=lun_in, tarray=tarray, $
    no_head=no_head, html=html, fdigit=fdigit, ddigit=ddigit, alias=alias, $
    formatcodes=formatcodes, css=css, debug=debug
    ;
    IF (SIZE(struct,/TNAME) NE 'STRUCT') THEN RETURN
    nrow = N_ELEMENTS(struct)
    IF (nrow EQ 0) THEN RETURN
    IF (KEYWORD_SET(filename) && KEYWORD_SET(lun_in) EQ 0) THEN BEGIN
        OPENW, lun, filename, /GET_LUN
    ENDIF ELSE BEGIN
        IF KEYWORD_SET(lun_in) THEN lun = lun_in
    ENDELSE
    IF (~KEYWORD_SET(lun) && ~ARG_PRESENT(tarray)) THEN lun = -1
    IF ~KEYWORD_SET(fdigit) THEN fdigit = 5
    IF ~KEYWORD_SET(ddigit) THEN ddigit = 7
    tags = TAG_NAMES(struct)
    ntag = N_ELEMENTS(tags)
    IF KEYWORD_SET(html) THEN BEGIN
        htmhdr = '<table>'
        hdr1 = '<tr>'
        hdr2 = ''
        rowstart = '<tr>'
        rowend = '</tr>'
        colstart = '<td>'
        colend = '</td>'
        lastline = '</table>'
    ENDIF ELSE BEGIN
        hdr1 = ''
        hdr2 = ''
        rowstart = ''
        rowend = ''
        colstart = ''
        colend = ''
    ENDELSE
    IF KEYWORD_SET(html) && ARG_PRESENT(css) THEN BEGIN
        css = ['<style type="text/css">', $
            'table {', $
            '    border-collapse: collapse;', $
            '}', $
            'th {', $
            '    padding: 2px;', $
            '    text-align: right;', $
            '    border: 1px solid black;', $
            '    font-weight: bold;', $
            '}', $
            'td {', $
            '    padding: 2px;', $
            '    text-align: right;', $
            '    border: 1px solid black;', $
            '}', $
            '</style>']
    ENDIF
    ;
    ; Construct the header lines and format string
    ;
    FOR itag=0L, ntag-1L DO BEGIN
        narr = N_ELEMENTS(struct[0].(itag))
        FOR iarr=0L, narr-1L DO BEGIN
            IF (itag EQ 0 && iarr EQ 0) THEN BEGIN
                format = '(%"' + rowstart
            ENDIF ELSE BEGIN
                hdr1 = hdr1 + ' '
                hdr2 = hdr2 + ' '
                format = format + ' '
            ENDELSE
            thisname = struct_checktype(tags[itag], alias=alias)
            IF (narr GT 1) THEN thisname = thisname + STRTRIM(STRING(iarr),2)
            IF KEYWORD_SET(no_head) THEN namelen = 1 $
            ELSE namelen = STRLEN(thisname)
            tname = SIZE(struct[0].(itag),/TNAME)
            IF (tname EQ 'BYTE' || tname EQ 'INT' || tname EQ 'LONG' $
                || tname EQ 'LONG64' || tname EQ 'UINT' || tname EQ 'ULONG' $
                || tname EQ 'ULONG64') THEN BEGIN
                minval = MIN( struct.(itag)[iarr] )
                maxval = MAX( struct.(itag)[iarr] )
                nchar = STRLEN(STRTRIM(STRING(minval),2)) $
                    > STRLEN(STRTRIM(STRING(maxval),2))
                nchar = nchar > namelen
                thiscode = '%' + STRTRIM(STRING(nchar),2) + 'd'
            ENDIF ELSE IF (tname EQ 'FLOAT') THEN BEGIN
                minval = MIN( struct.(itag)[iarr] )
                IF (minval LT 0) THEN nchar = fdigit + 7 $
                ELSE nchar = fdigit + 6
                nchar = nchar > namelen
                thiscode = '%' + STRTRIM(STRING(nchar),2) + '.' $
                    + STRTRIM(STRING(fdigit),2) + 'g'
            ENDIF ELSE IF (tname EQ 'DOUBLE') THEN BEGIN
                minval = MIN( struct.(itag)[iarr] )
                IF (minval LT 0) THEN nchar = ddigit + 7 $
                ELSE nchar = ddigit + 6
                nchar = nchar > namelen
                thiscode = '%' + STRTRIM(STRING(nchar),2) + '.' $
                    + STRTRIM(STRING(ddigit),2) + 'g'
            ENDIF ELSE IF (tname EQ 'STRING') THEN BEGIN
                nchar = MAX(STRLEN( struct.(itag)[iarr] )) > namelen
                thiscode = '%' + STRTRIM(STRING(nchar),2) + 's'
            ENDIF ELSE BEGIN
                MESSAGE, 'Unsupported type code: ' + tname
            ENDELSE
            IF KEYWORD_SET(formatcodes) THEN BEGIN
                jj = (WHERE(STRUPCASE(formatcodes[0,*]) EQ tags[itag], ct))[0]
                IF (ct NE 0) THEN BEGIN
                    thiscode = formatcodes[1,jj]
                    nchar = STRLEN( $
                        STRING(struct[0].(itag)[iarr], FORMAT='('+formatcodes[1,jj]+')') )
                ENDIF
            ENDIF
            schar = STRTRIM(STRING(nchar),2)
            IF KEYWORD_SET(html) THEN BEGIN
                hdr1 = hdr1 + '<th>' + thisname + '</th>'
            ENDIF ELSE BEGIN
                hdr1 = hdr1 + STRING(thisname, FORMAT='(A' + schar + ')')
                hdr2 = hdr2 + STRING(REPLICATE('-',nchar), $
                    FORMAT='(' + schar + 'A)')
            ENDELSE
            format = format + colstart + thiscode + colend
        ENDFOR
    ENDFOR
    format = format + rowend + '")'
    hdr1 = hdr1 + rowend
    IF KEYWORD_SET(debug) THEN PRINT, format
    ;
    ; Now print one row at a time
    ;
    IF KEYWORD_SET(lun) THEN BEGIN
        IF KEYWORD_SET(htmhdr) THEN PRINTF, lun, htmhdr
        IF ~KEYWORD_SET(no_head) THEN BEGIN
            PRINTF, lun, hdr1
            PRINTF, lun, hdr2
        ENDIF
        FOR irow=0L, nrow-1L DO PRINTF, lun, struct[irow], FORMAT=format
        IF KEYWORD_SET(lastline) THEN PRINTF, lun, lastline
        IF (KEYWORD_SET(filename) && KEYWORD_SET(lun_in) EQ 0) THEN BEGIN
            CLOSE, lun
            FREE_LUN, lun
        ENDIF
    ENDIF
    IF ARG_PRESENT(tarray) THEN BEGIN
        tarray = STRARR(nrow)
        FOR irow=0L, nrow-1 DO tarray[irow] = STRING(struct[irow], FORMAT=format)
        IF ~KEYWORD_SET(no_head) THEN tarray = [hdr1, hdr2, tarray]
        IF KEYWORD_SET(htmhdr) THEN tarray = [htmhdr, tarray]
        IF KEYWORD_SET(lastline) THEN tarray = [tarray, lastline]
    ENDIF
    RETURN
END
;------------------------------------------------------------------------------
