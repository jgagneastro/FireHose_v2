;+
; NAME:
;   yanny_read
;
; PURPOSE:
;   Read a Yanny parameter file into an IDL structure.
;
; CALLING SEQUENCE:
;   yanny_read, filename, [ pdata, hdr=, enums=, structs=, $
;    /anonymous, stnames=, /quick, errcode= ]
;
; INPUTS:
;   filename   - Input file name for Yanny parameter file
;
; OPTIONAL INPUTS:
;
; OUTPUT:
;
; OPTIONAL OUTPUTS:
;   pdata      - Array of pointers to all structures read.  The i-th data
;                structure is then referenced with "*pdata[i]".  If you want
;                to pass a single structure (eg, FOOBAR), then pass
;                ptr_new(FOOBAR).
;   hdr        - Header lines in Yanny file, which are usually keyword pairs.
;   enums      - All "typedef enum" structures.
;   structs    - All "typedef struct" structures, which define the form
;                for all the PDATA structures.
;   anonymous  - If set, then all returned structures are anonymous; set this
;                keyword to avoid possible conflicts between named structures
;                that are actually different.
;   stnames    - Names of structures.  If /ANONYMOUS is not set, then this
;                will be equivalent to the IDL name of each structure in PDATA,
;                i.e. tag_names(PDATA[0],/structure_name) for the 1st one.
;                This keyword is useful for when /ANONYMOUS must be set to
;                deal with structures with the same name but different defns.
;   quick      - This keyword is only for backwards compatability, and
;                has no effect.
;   errcode    - Returns as non-zero if there was an error reading the file.
;
; COMMENTS:
;   If the file name suffix is '.gz' or '.Z', the uncompress the file
;   while reading.  The gunzip command is spawned for '.gz' files, and
;   'uncompress' is spawned for '.Z' files.
;
;   Return 0's if the file does not exist.
;
;   Read and write variables that are denoted INT in the Yanny file
;   as IDL-type LONG, and LONG as IDL-type LONG64.  This is because
;   Yanny files assume type INT is a 4-byte integer, whereas in IDL
;   that type is only 2-byte.
;
;   I special-case the replacement of {{}} with "", since it seems
;   that some bonehead used the former to denote blank strings in
;   the idReport files.  Otherwise, any curly-brackets are always ignored
;   (as they should be).
;
; EXAMPLES:
;
; BUGS:
;   The reading could probably be sped up by setting a format string for
;   each structure to use in the read.
;
;   Not set up yet to deal with multi-dimensional arrays, but neither
;   is the fTCL-based reader.
;
; PROCEDURES CALLED:
;   fileandpath()
;   hogg_strsplit
;   hogg_unquoted_regex()
;   mrd_struct
;
; INTERNAL SUPPORT ROUTINES:
;   yanny_strip_commas()
;   yanny_add_comment
;   yanny_getwords()
;   yanny_add_pointer
;   yanny_readstring
;   yanny_nextline()
;
; REVISION HISTORY:
;   05-Sep-1999  Written by David Schlegel, Princeton.
;   18-Jun-2001  Fixed bug to allow semi-colons within double quotes
;                C. Tremonti (added yanny_inquotes, modifed yanny_strip_commas,
;                yanny_nextline)
;   11-Oct-2002  Major changes by D. Schlegel and D. Hogg to get rid
;                of the 2048-char line limit, dramatically speed up the
;                code when reading large files by pre-allocating the memory,
;                and use regular-expression matching for speed, robustness,
;                and clarity.
;   08-Mar-2012  Replace numlines() with FILE_LINES().
;   06-Nov-2013  Turn off splitting of lines by semicolons.
;-
;------------------------------------------------------------------------------
; All this function actually does is trim any semi-colons at the end
; of a line.  This is to prevent the semi-colon from becoming part
; of a structure's name.
FUNCTION yanny_strip_commas, rawline
    pos = STREGEX(rawline+' ', '; +$', LENGTH=len)
    IF (pos[0] EQ -1) THEN RETURN, rawline $
    ELSE RETURN, STRMID(rawline, 0, pos)
END
;------------------------------------------------------------------------------
PRO yanny_add_comment, rawline, comments
    IF (SIZE(comments,/tname) EQ 'INT') THEN comments = rawline $
    ELSE comments = [comments, rawline]
    RETURN
END
;------------------------------------------------------------------------------
; Procedure to read the next line from a file into a string.
; This piece of code used to not use READF, since READF used to
; fail if the last non-whitespace character was a backslash.
PRO yanny_readstring, ilun, sline
    COMMON yanny_linenumber, lastlinenum ; Only for debugging
    sline = ''
    READF, ilun, sline
    lastlinenum = lastlinenum + 1
    RETURN
END
;------------------------------------------------------------------------------
; Read the next line from the input file, appending several lines if there
; are continuation characters (backslashes) at the end of lines.
FUNCTION yanny_nextline, ilun
    COMMON yanny_lastline, lastline
    ;----------
    ; If we had already parsed the last line read into several lines (by
    ; semi-colon separation), then return the next of those.
    IF KEYWORD_SET(lastline) THEN BEGIN
        sline = lastline[0]
        nlast = N_ELEMENTS(lastline)
        IF (nlast EQ 1) THEN lastline = '' $
        ELSE lastline = lastline[1:nlast-1]
        RETURN, sline
    ENDIF
    ;----------
    ; Read the next line.  If the last non-whitespace character is
    ; a backslash, then read and append the next line.  Do that
    ; recursively until there are no more continuation lines.
    sline = ''
    yanny_readstring, ilun, sline
    sline = STRTRIM(sline) ; Remove only trailing whitespace
    nchar = STRLEN(sline)
    WHILE (STRMID(sline, nchar-1) EQ '\') DO BEGIN
        STRPUT, sline, ' ', nchar-1 ; Replace the '\' with a space
        yanny_readstring, ilun, stemp
        stemp = STRTRIM(stemp) ; Remove only trailing whitespace
        sline = sline + stemp
        nchar = STRLEN(sline)
    ENDWHILE
    ;----------
    ; Now parse this line into several lines by semi-colon separation,
    ; keeping those semi-colons at the end of each.  Ignore any semi-colons
    ; inside double-quotes.
    ;
    ; This form of line splitting is deprecated and is in any case very rare.
    ; Turned off, BAW 2013-11-06
    ;
    lastline = ''
    ;rgx = hogg_unquoted_regex(';')
    ;WHILE (strlen(sline) GT 0) DO BEGIN
    ;   pos = STRSPLIT(sline, rgx, /REGEX, LENGTH=len)
    ;   ; By using len+1 below, we are adding back in the semi-colon.
    ;   IF ~KEYWORD_SET(lastline) THEN lastline = STRMID(sline, 0, len+1) $
    ;   ELSE lastline = [lastline, STRMID(sline, 0, len+1)]
    ;   sline = STRMID(sline, len+1)
    ;ENDWHILE
    ;sline = lastline[0]
    ;IF (N_ELEMENTS(lastline) EQ 1) THEN lastline = '' $
    ;ELSE lastline = lastline[1:N_ELEMENTS(lastline)-1]
    RETURN, sline
END
;------------------------------------------------------------------------------
; Append another pointer NEWPTR to the array of pointers PDATA.
; Also add its name to PNAME.
PRO yanny_add_pointer, stname, newptr, pcount, pname, pdata, pnumel
    IF (pcount EQ 0) THEN BEGIN
        pname = stname
        pdata = newptr
        pnumel = 0L
    ENDIF ELSE BEGIN
        pname = [pname, stname]
        pdata = [pdata, newptr]
        pnumel = [pnumel, 0L]
    ENDELSE
    pcount = pcount + 1
    RETURN
END
;-----------------------------------------------------------------------------
; Split SLINE into words using whitespace.  Anything inside double-quotes
; is protected from being split into separate words, and is returned as
; a single word without the quotes.
FUNCTION yanny_getwords, sline_in
    sline = sline_in ; Make a copy of this, since we modify it below
    ;----------
    ; First, we need to replace any empty double curly-bracket,
    ; like "{ { } }" or "{{}}" with a double-quoted empty string.
    pos = 0
    WHILE (pos GE 0) DO BEGIN
        pos = STREGEX(sline,'\{ *\{ *\} *\}', LENGTH=len)
        IF (pos GE 0) THEN $
            sline = STRMID(sline,0,pos) + ' "" ' + STRMID(sline,pos+len)
    ENDWHILE
    ;----------
    ; Dispose of any commas, semi-colons, or curly-brackets
    ; that are not inside double-quotes.  Replace them with spaces.
    ; First, split up into sections separated by double quotes.
    ; Then, in every OTHER section remove offending characters.
    ; (Start at zeroth section if first character is not double quote,
    ; first section otherwise)
    sections= STRSPLIT(sline, '"', /EXTRACT, /PRESERVE_NULL)
    FOR i=0L, N_ELEMENTS(sections)-1L, 2L DO $
         sections[i]=STRJOIN(STRSPLIT(sections[i], ',;{}',/EXTRACT),' ')
    sline= STRJOIN(sections, '"')
    ;----------
    ; Split this line into words, protecting anything inside
    ; double-quotes as a single word.
    hogg_strsplit, sline, words
    IF ~KEYWORD_SET(words) THEN words = ''
    RETURN, words
END
;------------------------------------------------------------------------------
; Main program
PRO yanny_read, filename, pdata, hdr=hdr, enums=enums, structs=structs, $
    anonymous=anonymous, stnames=stnames, quick=quick, errcode=errcode
    COMMON yanny_linenumber, lastlinenum ; Only for debugging
    lastlinenum = 0
    IF (N_PARAMS() LT 1) THEN BEGIN
        PRINT, 'Syntax - yanny_read, filename, [ pdata, hdr=, enums=, '
        PRINT, ' structs=, /anonymous, stnames=, /quick, errcode= ]'
        RETURN
    ENDIF
    tname = ['char', 'short', 'int', 'long', 'float', 'double']
    ; Read and write variables that are denoted INT in the Yanny file
    ; as IDL-type LONG, and LONG as IDL-type LONG64.  This is because
    ; Yanny files assume type INT is a 4-byte integer, whereas in IDL
    ; that type is only 2-byte.
    tvals = ['""'  , '0'    , '0L'  , '0LL'  , '0.0'  , '0.0D'  ]
    tarrs = ['strarr(' , $
            'intarr(' , $
            'lonarr(' , $
            'lon64arr(' , $
            'fltarr(' , $
            'dblarr(' ]
    hdr = 0
    enums = 0
    structs = 0
    errcode = 0
    stnames = 0
    ; List of all possible structures
    pcount = 0       ; Count of structure types defined
    pname = 0        ; Name of each structure
    pdata = 0        ; Pointer to each structure
    pnumel = 0       ; Number of elements in each structure
    ;----------
    ; If the file does not exist, then return but not with an error
    junk = FILE_SEARCH(filename[0], COUNT=ct)
    IF (ct EQ 0) THEN BEGIN
        errcode = 0
        RETURN
    ENDIF
    ;----------
    ; Count the number of lines in the file, then open the file for
    ; reading one line at a time.
    shortname = fileandpath(filename[0])
    ww = STRSPLIT(shortname,'.',/EXTRACT)
    nword = N_ELEMENTS(ww)
    IF (nword GT 1) THEN uncmps = ww[nword-1] ELSE uncmps = ''
    CASE uncmps OF
        'Z': BEGIN
            SPAWN, 'uncompress -c '+filename+'|wc -l', maxlen
            maxlen = LONG(maxlen[0]) > 1
            ; I can use /NOSHELL below only if the spawn command is
            ; passed as an array of words.
            SPAWN, ['uncompress','-c',filename], UNIT=ilun, /NOSHELL
            err = 0
        END
        'gz': BEGIN
            SPAWN, 'gunzip -c '+filename+'|wc -l', maxlen
            maxlen = LONG(maxlen[0]) > 1
            OPENR, ilun, filename, ERROR=err, /GET_LUN, /COMPRESS
        END
        ELSE: BEGIN
            maxlen = FILE_LINES(filename[0]) > 1
            OPENR, ilun, filename, ERROR=err, /GET_LUN
        END
    ENDCASE
    IF (err NE 0) THEN BEGIN
        IF KEYWORD_SET(ilun) THEN BEGIN
            CLOSE, ilun
            FREE_LUN, ilun
        ENDIF
        errcode = -2L
        RETURN
    ENDIF
    ;----------
    ; Loop over all lines in the file
    sline = ''
    WHILE ~EOF(ilun) DO BEGIN
        qdone = 0
        ; Read the next line
        rawline = yanny_nextline(ilun)
        sline = yanny_strip_commas(rawline)
        words = yanny_getwords(sline) ; Divide into words and strings
        nword = N_ELEMENTS(words)
        IF (nword GE 2) THEN BEGIN
            ; LOOK FOR "typedef enum" lines and add to structs
            IF (words[0] EQ 'typedef' && words[1] EQ 'enum') THEN BEGIN
                WHILE (STRMID(sline,0,1) NE '}') DO BEGIN
                    yanny_add_comment, rawline, enums
                    rawline = yanny_nextline(ilun)
                    sline = yanny_strip_commas(rawline)
                ENDWHILE
                yanny_add_comment, rawline, enums
                qdone = 1 ; This last line is still part of the enum string
            ; LOOK FOR STRUCTURES TO BUILD with "typedef struct"
            ENDIF ELSE IF (words[0] EQ 'typedef' && words[1] EQ 'struct') THEN BEGIN
                ntag = 0
                yanny_add_comment, rawline, structs
                rawline = yanny_nextline(ilun)
                sline = yanny_strip_commas(rawline)
                WHILE (STRMID(STRTRIM(sline,2),0,1) NE '}') DO BEGIN
                    ww = yanny_getwords(sline)
                    IF (N_ELEMENTS(ww) GE 2) THEN BEGIN
                        i = WHERE(ww[0] EQ tname, ct)
                        i = i[0]
                        ; If the type is "char", then remove the string length
                        ; from the defintion, since IDL does not need a string
                        ; length.  For example, change "char foo[20]" to "char foo",
                        ; or change "char foo[10][20]" to "char foo[10]".
                        ; Also handle old-school '<>' brackets, but don't assume
                        ; they will be mixed.
                        IF (i EQ 0) THEN BEGIN
                            j1 = STRPOS(ww[1], '[', /REVERSE_SEARCH)
                            IF (j1 NE -1) THEN ww[1] = STRMID(ww[1], 0, j1)
                            j2 = STRPOS(ww[1], '<', /REVERSE_SEARCH)
                            IF (j2 NE -1) THEN ww[1] = STRMID(ww[1], 0, j2)
                        ENDIF
                        ; Force an unknown type to be a "char"
                        ; This will handle enum types.
                        IF (i[0] EQ -1) THEN i = 0
                        ; Test to see if this should be an array
                        ; (Only 1-dimensional arrays supported here.)
                        j1 = STRPOS(ww[1], '[')
                        IF (j1 NE -1) THEN BEGIN
                            addname = STRMID(ww[1], 0, j1)
                            j2 = STRPOS(ww[1], ']')
                            addval = tarrs[i] + STRMID(ww[1], j1+1, j2-j1-1) + ')'
                        ENDIF ELSE BEGIN
                            j1 = STRPOS(ww[1], '<')
                            IF (j1 NE -1) THEN BEGIN
                                addname = STRMID(ww[1], 0, j1)
                                j2 = STRPOS(ww[1], '>')
                                addval = tarrs[i] + STRMID(ww[1], j1+1, j2-j1-1) + ')'
                            ENDIF ELSE BEGIN
                                addname = ww[1]
                                addval = tvals[i]
                            ENDELSE
                        ENDELSE
                        IF (ntag EQ 0) THEN BEGIN
                            names = addname
                            values = addval
                        ENDIF ELSE BEGIN
                            names = [names, addname]
                            values = [values, addval]
                        ENDELSE
                        ntag = ntag + 1
                    ENDIF
                    yanny_add_comment, rawline, structs
                    rawline = yanny_nextline(ilun)
                    sline = yanny_strip_commas(rawline)
                ENDWHILE
                yanny_add_comment, rawline, structs
                ; Now for the structure name - get from the last line read
                ; Force this to uppercase
                ww = yanny_getwords(sline)
                stname1 = STRUPCASE(ww[0])
                IF ~KEYWORD_SET(stnames) THEN stnames = stname1 $
                ELSE stnames = [stnames, stname1]
                ; Create the actual structure
                ; Pre-allocate a large array of length MAXLEN, which is
                ; the longest it could possibly be (the number of lines
                ; in the file).  At the end of this proc, we trim this
                ; to only the actual number of elements.
                IF KEYWORD_SET(anonymous) THEN structyp='' ELSE structyp=stname1
                ptr1 = REPLICATE(mrd_struct(names, values, 1, structyp=structyp), maxlen)
                yanny_add_pointer, stname1, $
                    PTR_NEW(ptr1), pcount, pname, pdata, pnumel
                qdone = 1 ; This last line is still part of the structure def'n

            ; LOOK FOR A STRUCTURE ELEMENT
            ; Only look if some structures already defined
            ; Note that the structure names should be forced to uppercase
            ; such that they are not case-sensitive.
            ENDIF ELSE IF (pcount GT 0) THEN BEGIN
                ; If PDATA is not to be returned, then we need to read this
                ; file no further.
                IF ~ARG_PRESENT(pdata) THEN BEGIN
                    CLOSE, ilun
                    FREE_LUN, ilun
                    RETURN
                ENDIF
                idat = WHERE(strupcase(words[0]) EQ pname[0:pcount-1], ct)
                IF (ct EQ 1) THEN BEGIN
                    idat = idat[0]
                    ; Add an element to the idat-th structure
                    ; Note that if this is the first element encountered,
                    ; then we already have an empty element defined.
                    ; (No longer any need to do this, since we pre-allocate
                    ; a large array.)
                    ; IF (pnumel[idat] GT 0) THEN $
                    ; *pdata[idat] = [*pdata[idat], (*pdata[idat])[0]]
                    ; Split this text line into words
                    ww = yanny_getwords(sline)
                    i = 1 ; Counter for which word we're currently reading
                          ; Skip the 0-th word since it's the structure name.
                    ; Now fill in this structure from the line of text
                    ntag = N_TAGS( *pdata[idat] )
                    FOR itag=0, ntag-1 DO BEGIN
                        ; This tag could be an array - see how big it is
                        sz = N_ELEMENTS( (*pdata[idat])[0].(itag) )
                        ; Error-checking code below
                        IF (i+sz GT N_ELEMENTS(ww)) THEN BEGIN
                            splog, 'Last line number read: ', lastlinenum
                            splog, 'Last line read: "' + rawline + '"'
                            splog, 'ABORT: Invalid Yanny file ' + filename $
                                + ' at line number ' $
                                + strtrim(string(lastlinenum),2) + ' !!'
                            CLOSE, ilun
                            FREE_LUN, ilun
                            yanny_free, pdata
                            errcode = -3L
                            RETURN
                        ENDIF
                        FOR j=0, sz-1 DO BEGIN
                            (*pdata[idat])[pnumel[idat]].(itag)[j] = ww[i]
                            i = i + 1
                        ENDFOR
                    ENDFOR
                    pnumel[idat] = pnumel[idat] + 1
                ENDIF
                qdone = 1 ; This last line was a structure element
            ENDIF
        ENDIF
        IF (qdone EQ 0) THEN yanny_add_comment, rawline, hdr
    ENDWHILE
    ;----------
    ; Close the file
    CLOSE, ilun
    FREE_LUN, ilun
    ;----------
    ; Trim the structures from their maximum length to only the actual
    ; number of elements.  Be sure to free memory from the untrimmed,
    ; unused pointers.
    FOR icount=0, pcount-1 DO BEGIN
        oldptr = pdata[icount]
        pdata[icount] = PTR_NEW( (*oldptr)[0:(pnumel[icount]-1)>0] )
        PTR_FREE, oldptr
    ENDFOR
    RETURN
END
;------------------------------------------------------------------------------
