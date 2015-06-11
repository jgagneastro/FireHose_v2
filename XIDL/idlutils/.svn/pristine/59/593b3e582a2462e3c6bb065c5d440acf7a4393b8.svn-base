;+
; NAME:
;    struct_to_json
;
; PURPOSE:
;    Convert a structure to JSON notation, & return it as a string.
;
; CALLING SEQUENCE
;    json = struct_to_json(structure,[map=map],[oned=oned])
;
; INPUTS:
;    structure - A single structure or an array of structures.
;
; OPTIONAL INPUTS:
;    map  - A two-dimensional string array.  This can be used to override
;           the default output tag names. For example, if you need
;           a mixed-case tag name in the output map=[['SQUARE','Square']]
;    oned - A one-dimensional string array. This is used to force
;           named substructures to be printed as arrays, even if they only
;           contain one element.
;
; OUTPUTS:
;    json - A string in JSON notation, or an array of strings, one for
;           each element of the input structure. Structure tags will be
;           converted to lowercase, unless overridden by the map input.
;
; NOTES:
;    There is an ambiguity in IDL for substructures, since structures are
;    never scalar.  So substructures that have only one element are converted
;    to sub-objects, rather than an array of sub-objects with one element.
;    This behaviour can be overridden with the oned keyword.
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
FUNCTION struct_to_json, structure, map=map, oned=oned
    ;
    ; Check the type
    ;
    IF SIZE(structure,/TYPE) NE 8 THEN $
        MESSAGE, 'Input structure must be of type STRUCT.'
    IF KEYWORD_SET(map) THEN BEGIN
        IF SIZE(map,/TYPE) NE 7 THEN $
            MESSAGE, 'map keyword must be of type STRING.'
        IF SIZE(map,/N_DIMENSIONS) NE 2 THEN $
            MESSAGE, 'map keyword must be a 2d STRING array.'
    ENDIF
    IF KEYWORD_SET(oned) THEN BEGIN
        IF SIZE(oned,/TYPE) NE 7 THEN $
            MESSAGE, 'oned keyword must be of type STRING.'
        IF SIZE(oned,/N_DIMENSIONS) NE 1 THEN $
            MESSAGE, 'oned keyword must be a 1d STRING array.'
    ENDIF
    ;
    ; Create the return array
    ;
    json = STRARR(N_ELEMENTS(structure))
    ;
    ; Create a meta-structure containing all the tag information
    ; name holds the tag name
    ; type holds the IDL type
    ; isobject is true (1B) if the tag appears to be some sort of object
    ; isarray is true (1B) if the tag is an array (1d only, please!)
    ; shape holds the length of the array
    ; delim holds the delimiter to use if the tag is an array
    ;
    names = TAG_NAMES(structure)
    n = N_TAGS(structure)
    meta = REPLICATE({name:'', type:0L, isobject:0B, isarray:0B, shape:0L, delim:''}, $
        N_TAGS(structure))
    FOR k = 0, n - 1 DO BEGIN
        meta[k].name = STRLOWCASE(names[k])
        IF KEYWORD_SET(map) THEN BEGIN
            wmap = WHERE(STRMATCH(map[0,*],names[k]),nmap)
            IF nmap GT 0 THEN meta[k].name = map[1,wmap]
        ENDIF
        IF STREGEX(meta[k].name,'_\$.+',/BOOLEAN) THEN meta[k].name = STRMID(meta[k].name,1)
        meta[k].type = SIZE(structure[0].(k),/TYPE)
        IF meta[k].type EQ 6 OR meta[k].type EQ 9 THEN $
            MESSAGE, 'Complex types are not supported!'
        IF meta[k].type EQ 10 OR meta[k].type EQ 11 THEN $
            MESSAGE, 'Pointer and object types are not supported!'
        ndim = SIZE(structure[0].(k),/N_DIMENSIONS)
        IF ndim GT 1 THEN $
            MESSAGE, 'Multi-dimensional structure tags are not (yet) supported!'
        IF ndim EQ 1 THEN BEGIN
            meta[k].shape = SIZE(structure[0].(k),/N_ELEMENTS)
            IF meta[k].type EQ 8 AND meta[k].shape EQ 1 THEN BEGIN
                IF KEYWORD_SET(oned) THEN BEGIN
                    woned = WHERE(STRMATCH(oned,names[k]),noned)
                    IF noned GT 0 THEN meta[k].isarray = 1B
                ENDIF
            ENDIF ELSE meta[k].isarray = 1B
            IF meta[k].type EQ 7 THEN BEGIN
                ;
                ; Is this an 'object'?
                ;
                IF STREGEX((structure[0].(k))[0],'^[a-z_]+\(',/BOOLEAN,/FOLD_CASE) THEN BEGIN
                    meta[k].delim = ','
                    meta[k].isobject = 1B
                ENDIF ELSE meta[k].delim = '","'
            ENDIF ELSE meta[k].delim = ','
        ENDIF ELSE BEGIN
            IF meta[k].type EQ 7 THEN BEGIN
                IF STREGEX(structure[0].(k),'^[a-z_]+\(',/BOOLEAN,/FOLD_CASE) THEN meta[k].isobject = 1B
            ENDIF
        ENDELSE
    ENDFOR
    ;
    ; Loop over structure
    ;
    FOR k = 0, N_ELEMENTS(structure) - 1 DO BEGIN
        json[k] = '{'
        FOR tag = 0, n - 1 DO BEGIN
            ;
            ; Add the tag name
            ;
            json[k] += '"'+meta[tag].name+'":'
            IF meta[tag].isarray THEN json[k] += '['
            IF meta[tag].type EQ 8 THEN BEGIN
                jj = struct_to_json(structure[k].(tag),map=map,oned=oned)
                IF meta[tag].isarray THEN jj = STRJOIN(jj,',')
                json[k] += jj
            ENDIF ELSE BEGIN
                IF meta[tag].type EQ 7 AND ~meta[tag].isobject THEN json[k] += '"'
                IF meta[tag].isarray THEN $
                    json[k] += STRJOIN(STRTRIM(STRING(structure[k].(tag)),2),meta[tag].delim) $
                ELSE $
                    json[k] += STRTRIM(STRING(structure[k].(tag)),2)
                IF meta[tag].type EQ 7 AND ~meta[tag].isobject THEN json[k] += '"'
            ENDELSE
            IF meta[tag].isarray THEN json[k] += ']'
            IF tag LT n-1 THEN json[k] += ','
        ENDFOR
        json[k] += '}'
    ENDFOR
    ;
    ; And finish
    ;
    RETURN, json
END
