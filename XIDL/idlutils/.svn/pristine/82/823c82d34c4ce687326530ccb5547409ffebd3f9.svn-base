;+
; NAME:
;    json_to_struct
;
; PURPOSE:
;    Convert a string or array of strings JSON notation, & return it as
;    an IDL structure.
;
; CALLING SEQUENCE
;    structure = json_to_struct(json,[/nodelete])
;
; INPUTS:
;    json - A string or array of strings in JSON notation.
;
; OPTIONAL INPUTS:
;    nodelete - Do not remove any temporary files created.
;
; OUTPUTS:
;    structure - An IDL structure or array of structures.
;
; NOTES:
;    JSON keywords beginning with '$' (e.g., "$oid":) will be replaced with
;    '_$'.
;
;    Named structures cannot contain anonymous sub-structures, so we will not
;    attempt to create a named structure.
;
;    Integer and real values get converted to INT & FLOAT respectively,
;    which may result in loss of precision.
;
; PROCEDURES CALLED:
;
; REVISION HISTORY:
;    2011-06-07 Initial version by B. A. Weaver, NYU
;
; VERSION:
;    $Id$
;
;-
FUNCTION json_to_struct, json, nodelete=nodelete
    ;
    ; Check the type
    ;
    IF SIZE(json,/TYPE) NE 7 THEN $
        MESSAGE, 'Input JSON must be of type STRING'
    FOR k = 0, N_ELEMENTS(json) - 1 DO BEGIN
        ;
        ; Create a working copy of the string
        ;
        ; jj = STRCOMPRESS(STRTRIM(json[k],2))
        jj = json[k]
        ;
        ; Search for (possibly hex) numbers in quotes.  IDL doesn't like
        ; hex numbers in double quotes, but is ok with them in single quotes
        ;
        WHILE STREGEX(jj,'"[0-9a-f]+"',/FOLD_CASE,/BOOLEAN) DO BEGIN
            o = STREGEX(jj,'"[0-9a-f]+"',/FOLD_CASE,LENGTH=l)
            STRPUT, jj, "'", o
            STRPUT, jj, "'", o+l-1
        ENDWHILE
        ;
        ; Remove spaces between keywords & colons.
        ;
        WHILE STREGEX(jj, '"[^"]+" +:',/BOOLEAN) DO BEGIN
            o = STREGEX(jj, '"[^"]+" +:',LENGTH=l)
            nq = STRPOS(jj,'"',o+1)
            jj = STRMID(jj,0,nq+1)+STRMID(jj,STRPOS(jj,':',nq))
        ENDWHILE
        ;
        ; Find keywords of the form $foo.  These will be converted to _$foo
        ;
        WHILE STREGEX(jj, '"\$[^"]+":',/BOOLEAN) DO BEGIN
            o = STREGEX(jj, '"\$[^"]+":',LENGTH=l)
            jj = STRMID(jj,0,o+1)+'_'+STRMID(jj,o+1)
        ENDWHILE
        ;
        ; Search for keywords & remove quotes around them.
        ;
        WHILE STREGEX(jj, '"[^"]+":',/BOOLEAN) DO BEGIN
            o = STREGEX(jj, '"[^"]+":',LENGTH=l)
            STRPUT, jj, " ", o
            STRPUT, jj, " ", o+l-2
        ENDWHILE
        ;
        ; Now we have string containing a valid IDL structure (in theory).
        ; Normally we would just EXECUTE() it, but EXECUTE() has a limit of
        ; 131 characters.
        ;
        ; PRINT, jj
        ; r = EXECUTE('str='+jj)
        ; IF r THEN BEGIN
        ;     IF ~KEYWORD_SET(structure) THEN structure = REPLICATE(str,N_ELEMENTS(json))
        ;     structure[k] = str
        ; ENDIF ELSE BEGIN
        ;     MESSAGE, 'Error converting string '+jj+' to structure!'
        ; ENDELSE
        ;
        ; Instead, we will write the data to a temporary function, then
        ; evaluate that function.
        ;
        ; First, modify the !PATH
        ;
        IF k EQ 0 THEN BEGIN
            dirsep = PATH_SEP()
            tmp = '/tmp'+dirsep+GETENV('USER')+dirsep+'pro'
            IF STRPOS(!PATH,tmp) LT 0 THEN BEGIN
                FILE_MKDIR, tmp
                IF ~FILE_TEST(tmp+dirsep+'IDL_NOCACHE') THEN $
                    SPAWN, ['touch', tmp+dirsep+'IDL_NOCACHE'], /NOSHELL
                pathsep = PATH_SEP(/SEARCH_PATH)
                !PATH = tmp + pathsep + !PATH
            ENDIF
            fname = 'jts_'+STRTRIM(STRING(ULONG64(SYSTIME(1))),2)
            filelist = FILE_SEARCH(tmp+dirsep+fname+'.pro',COUNT=nfile)
            WHILE nfile GT 0 DO BEGIN
                fname += 'x'
                filelist = FILE_SEARCH(tmp+dirsep+fname+'.pro',COUNT=nfile)
            ENDWHILE
            OPENW, unit,tmp+dirsep+fname+'.pro',/GET_LUN
            PRINTF, unit, 'FUNCTION '+fname
            PRINTF, unit, '    structure = [ $'
        ENDIF
        IF k EQ N_ELEMENTS(json) - 1 THEN eol = ']' ELSE eol = ', $'
        PRINTF, unit, jj+eol
    ENDFOR
    PRINTF, unit, '    RETURN, structure'
    PRINTF, unit, 'END'
    FREE_LUN, unit
    RESOLVE_ROUTINE, fname, /IS_FUNCTION
    structure = CALL_FUNCTION(fname)
    IF KEYWORD_SET(nodelete) THEN $
        MESSAGE, 'Created temporary file '+tmp+dirsep+fname+'.pro', /INF $
        ELSE FILE_DELETE, tmp+dirsep+fname+'.pro'
    RETURN, structure
END
