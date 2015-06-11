; $Id: make_html_help.pro,v 1.1 2000-09-26 21:23:45 dfink Exp $
;
; Copyright (c) 1995-1997, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       MAKE_HTML_HELP
;
; PURPOSE:
;       Given a list of IDL procedure files (.PRO), VMS text library
;       files (.TLB), or directories that contain such files, this procedure
;       generates a file in the HTML format that contains the documentation
;       for those routines that contain a DOC_LIBRARY style documentation
;       template.  The output file is compatible with World Wide Web browsers.
;
; CATEGORY:
;       Help, documentation.
;
; CALLING SEQUENCE:
;       MAKE_HTML_HELP, Sources, Outfile
;
; INPUTS:
;     Sources:  A string or string array containing the name(s) of the
;               .pro or .tlb files (or the names of directories containing
;               such files) for which help is desired.  If a source file is
;               a VMS text library, it must include the .TLB file extension.
;               If a source file is an IDL procedure, it must include the .PRO
;               file extension.  All other source files are assumed to be
;               directories.
;     Outfile:  The name of the output file which will be generated.
;
; KEYWORDS:
;       TITLE:  If present, a string which supplies the name that
;               should appear as the Document Title for the help.
;     VERBOSE:  Normally, MAKE_HTML_HELP does its work silently.
;               Setting this keyword to a non-zero value causes the procedure
;               to issue informational messages that indicate what it
;               is currently doing. !QUIET must be 0 for these messages
;               to appear.
;      STRICT:  If this keyword is set to a non-zero value, MAKE_HTML_HELP will
;               adhere strictly to the HTML format by scanning the
;               the document headers for characters that are reserved in
;               HTML (<,>,&,").  These are then converted to the appropriate
;               HTML syntax in the output file. By default, this keyword
;               is set to zero (to allow for faster processing).
;   LINKFILES:  If this keyword is set to a non-zero value,
;               MAKE_HTML_HELP will generate "file" links to the idl
;               procedures documented on the web page created by this
;               code.
;     VERSION:  If set to a string, MAKE_HTML_HELP will search for a function
;               called version+'_version.pro' and add the value returned by
;               that function to the output file.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       A help file with the name given by the Outfile argument is
;       created.
;
; RESTRICTIONS:
;       The following rules must be followed in formatting the .pro
;       files that are to be searched.
;               (a) The first line of the documentation block contains
;                   only the characters ";+", starting in column 1.
;               (b) There must be a line which contains the string "NAME:",
;                   which is immediately followed by a line containing the
;                   name of the procedure or function being described in
;                   that documentation block.  If this NAME field is not
;                   present, the name of the source file will be used.
;               (c) The last line of the documentation block contains
;                   only the characters ";-", starting in column 1.
;               (d) Every other line in the documentation block contains
;                   a ";" in column 1.
;
;       Note that a single .pro file can contain multiple procedures and/or
;       functions, each with their own documentation blocks. If it is desired
;       to have "invisible" routines in a file, i.e. routines which are only
;       for internal use and should not appear in the help file, simply leave
;       out the ";+" and ";-" lines in the documentation block for those
;       routines.
;
;       No reformatting of the documentation is done.
;
; MODIFICATION HISTORY:
;       July 5, 1995, DD, RSI. Original version.
;       July 13, 1995, Mark Rivers, University of Chicago. Added support for
;               multiple source directories and multiple documentation
;               headers per .pro file.
;       July 17, 1995, DD, RSI. Added code to alphabetize the subjects;
;               At the end of each description block in the HTML file,
;               added a reference to the source .pro file.
;       July 18, 1995, DD, RSI. Added STRICT keyword to handle angle brackets.
;       July 19, 1995, DD, RSI. Updated STRICT to handle & and ".
;               Changed calling sequence to accept .pro filenames, .tlb
;               text librarie names, and/or directory names.
;               Added code to set default subject to name of file if NAME
;               field is not present in the doc header.
;       February 10, 1998, MWC, UC Berkeley. Added purpose to the line
;               which is output for each routine in the tableof
;               contents. This used to just contain the name.
;       April 1, 1998, MWC, UC Berkeley.  Added option to create file
;               link to the location of the procedure being documented.
;       2009-07-17, BAW, NYU.  Emits XHTML
;       2011-11-29, BAW, NYU.  Adds a version string to the file if requested.
;               Strict keyword now has no effect, but it is retained for
;               backwards-compatibility.
;
;-
;
;----------------------------------------------------------------------------
;
; Replaces any occurrence of HTML reserved characters (<,>,&,") in the
; given text lines with the appropriate HTML counterpart.
;
; entry:
;       txtlines - String array containing the text line(s) to be altered.
; exit:
;       txtlines - Same as input except that reserved characters have been
;                  replaced with the appropriate HTML syntax.
;
PRO makehh_strict, txtlines
    count = N_ELEMENTS(txtlines)
    FOR i=0,count-1 DO BEGIN
        txt = txtlines[i]
        ;
        ; Ampersands get replaced with &amp;.  Must do ampersands first because
        ; they are used to replace other reserved characters in HTML.
        ;
        spos = STRPOS(txt,'&')
        WHILE (spos NE -1) DO BEGIN
            newtxt = STRMID(txt,0,spos)+'&amp;'+STRMID(txt,spos+1,STRLEN(txt)-spos+1)
            txt = newtxt
            spos = STRPOS(txt,'&',spos+1)
        ENDWHILE
        txtlines[i] = txt
        ;
        ; Left angle brackets get replaced with &lt;
        ;
        spos = STRPOS(txt,'<')
        WHILE (spos NE -1) DO BEGIN
            newtxt = STRMID(txt,0,spos)+'&lt;'+STRMID(txt,spos+1,STRLEN(txt)-spos+1)
            txt = newtxt
            spos = STRPOS(txt,'<',spos+1)
        ENDWHILE
        txtlines[i] = txt
        ;
        ; Right angle brackets get replaced with &gt;
        ;
        spos = STRPOS(txt,'>')
        WHILE (spos NE -1) DO BEGIN
            newtxt = STRMID(txt,0,spos)+'&gt;'+STRMID(txt,spos+1,STRLEN(txt)-spos+1)
            txt = newtxt
            spos = STRPOS(txt,'>',spos+1)
        ENDWHILE
        txtlines[i] = txt
        ;
        ; Double quotes get replaced with &quot;
        ; This is not strictly necessary in XHTML, but it can't hurt.
        ;
        spos = STRPOS(txt,'"')
        WHILE (spos NE -1) DO BEGIN
            newtxt = STRMID(txt,0,spos)+'&quot;'+STRMID(txt,spos+1,STRLEN(txt)-spos+1)
            txt = newtxt
            spos = STRPOS(txt,'"',spos+1)
        ENDWHILE
        txtlines[i] = txt
    ENDFOR
    RETURN
END
;
;----------------------------------------------------------------------------
;
; Parentheses () & square brackets [] are invalid in XHTML id attributes
; The brackets result from duplicate routine names & should really be replaced
; with non-duplicate names.  The parentheses are often used to designate
; functions to distinguish them from procedures.  This function converts a
; subject name containing parentheses into a form that is valid XHTML.
; It has no effect on subjects that do not contain parentheses.
;
FUNCTION makehh_fix_subject, subject
    ;
    ;
    ;
    newsubject = subject
    IF (STREGEX(newsubject,'\(\)',/BOOLEAN)) THEN BEGIN
        spos = STRPOS(newsubject,'(')
        newsubject = STRMID(newsubject,0,spos)+'_F'
    ENDIF
    IF (STREGEX(newsubject,'\[[0-9]+\]',/BOOLEAN)) THEN BEGIN
        spos = STRPOS(newsubject,'[')
        id = STREGEX(newsubject,'\[([0-9]+)\]',/SUBEXPR,/EXTRACT)
        newsubject = STRMID(newsubject,0,spos)+'_'+id[1]+'_'
    ENDIF
    RETURN, newsubject
END
;
;----------------------------------------------------------------------------
;
; Searches an input file for all text between the ;+ and ;- comments, and
; updates the scratch text file appropriately. Note that this routine
; will extract multiple comment blocks from a single source file if they are
; present.
;
; entry:
;         name - Name of file containing documentation header(s).
;       dict[] - Dictionary entries for each documentation block in the .PRO
;                file.  Each dictionary entry is a structure with an index to
;                the source filename, an index to the extracted library
;                filename (useful only for VMS text libraries), a subject name,
;                scratch file offset, unique id (for duplicate names), and
;                number of lines of documentation text.
;                This parameter may be originally undefined at entry.
;  infile_indx - Index of the source .pro or .tlb filename.
; libfile_indx - Index of extracted library filename.  If the source
;                filename was not a VMS text library, this value should be
;                set to -1L.
;     txt_file - Scratch file to which the documentation header will
;                be written.
;      verbose - TRUE if the routine should output a descriptive message
;                when it finds the documentation header.
;       strict - If nonzero, the routine will adhere strictly to HTML format.
;                The document headers will be scanned for characters that are
;                reserved in HTML (<,>,&,"), which are then converted to the
;                appropriate HTML syntax in the output file.
;
; exit:
;  txt_file -  Updated as necessary. Positioned at EOF.
;    dict[] - Updated array of new dictionary entries.
;
; Under DOS, formatted output ends up with a carriage return linefeed
; pair at the end of every record. The resulting file would not be
; compatible with Unix. Therefore, we use unformatted output, and
; explicity add the linefeed, which has ASCII value 10.
;
PRO makehh_grab_hdr,name,dict,infile_indx,libfile_indx,txt_file,verbose,$
    strict
    ;
    ;
    ;
    LF=10B
    IF (libfile_indx NE -1L) THEN $
        OPENR, in_file, /GET, FILEPATH('mkhtmlhelp.scr',/TMP), /DELETE $
    ELSE $
        OPENR, in_file, /GET, name
    IF (verbose NE 0) THEN MESSAGE,/INFO, 'File = '+name
    WHILE (1) DO BEGIN
        ;
        ; Find the opening line of the next header.
        ;
        tmp = ''
        found = 0
        num = 0
        header = ''
        ON_IOERROR, DONE
        WHILE (NOT found) DO BEGIN
            READF, in_file, tmp
            IF (STRMID(tmp,0,2) EQ ';+') THEN found = 1
        ENDWHILE
        IF (found) THEN BEGIN
            ;
            ; Find the matching closing line of the header.
            ;
            found = 0
            WHILE (NOT found) DO BEGIN
                READF,in_file,tmp
                IF (STRMID(tmp,0,2) EQ ';-') THEN BEGIN
                    found =1
                ENDIF ELSE BEGIN
                    tmp = strmid(tmp, 1, 1000)
                    header = [header, tmp]
                    num = num + 1
                ENDELSE
            ENDWHILE
            IF (strict) THEN makehh_strict,header
            ;
            ; Done with one block of header
            ;
            ; Keep track of current scratch file offset, then write doc text.
            ;
            POINT_LUN,-txt_file,pos
            FOR i=1, num DO BEGIN
                WRITEU, txt_file, header[i],LF
            ENDFOR
            ;
            ; Search for the subject. It is the line following name.
            ;
            ; index = WHERE(STRTRIM(header, 2) EQ 'NAME:', count)
            ; index = WHERE(STREGEX(STRTRIM(header, 2),'NAME *:',/FOLD_CASE,/BOOLEAN), count)
            index = WHERE(STRMATCH(STRTRIM(header, 2),'name*:',/FOLD_CASE) EQ 1, count)
            IF (count EQ 1) THEN BEGIN
                sub = STRUPCASE(STRTRIM(header[index[0]+1], 2))
                IF (verbose NE 0) THEN MESSAGE,/INFO, 'Routine = '+sub
                ;
                ; If the NAME field was not present, set the subject to
                ; the name of the source text file.
            ENDIF ELSE BEGIN
                MESSAGE,/INFO,'Properly formatted NAME entry not found...'
                ifname = name
                tok = PATH_SEP()
                ;
                ; Cut the path.
                ;
                sp0 = 0
                spos = STRPOS(ifname,tok,sp0)
                WHILE (spos NE -1) DO BEGIN
                    sp0 = spos+1
                    spos = STRPOS(ifname,tok,sp0)
                ENDWHILE
                ifname = STRMID(ifname,sp0,(STRLEN(ifname)-sp0))
                ;
                ; Cut the suffix.
                ;
                spos = STRPOS(ifname,'.')
                IF (spos NE -1) THEN ifname = STRMID(ifname,0,spos[0])
                IF (strict) THEN makehh_strict, ifname
                sub = STRUPCASE(ifname)
                MESSAGE,/INFO,'  Setting subject to filename: '+sub+'.'
            ENDELSE
            ; index = WHERE(STRTRIM(header, 2) EQ 'PURPOSE:', count)
            ; index = WHERE(STREGEX(STRTRIM(header, 2),'PURPOSE *:',/FOLD_CASE,/BOOLEAN), count)
            index = WHERE(STRMATCH(STRTRIM(header, 2),'purpose*:',/FOLD_CASE) EQ 1, count)
            IF (count EQ 1) THEN BEGIN
                purpose = STRTRIM(header[index[0]+1], 2)
            ENDIF ELSE BEGIN
                purpose = ''
                MESSAGE, /INFO, 'Purposeless routine processed!'
            ENDELSE
            ;
            ; Calculate unique id in case of duplicate subject names.
            ;
            IF (N_ELEMENTS(dict) EQ 0) THEN $
                ndup=0 $
            ELSE BEGIN
                dpos = WHERE(dict.subject EQ sub,ndup)
                IF (ndup GT 0) THEN BEGIN
                    FOR i=1,ndup DO dict[dpos[i-1]].id = i
                    ndup = ndup + 1
                ENDIF
            ENDELSE
            ;
            ; Create a dictionary entry for the document header.
            ;
            entry = {DICT_STR_PURP,subject:sub,purpose:purpose, $
                indx:infile_indx,lib:libfile_indx,$
                id:ndup,offset:pos,nline:num}
            IF (N_ELEMENTS(dict) EQ 0) THEN dict = [entry] ELSE dict = [dict,entry]
        ENDIF
    ENDWHILE
DONE:
    ON_IOERROR, NULL
    FREE_LUN, in_file
    RETURN
END
;
;----------------------------------------------------------------------------
;
; Build a .HTML file with the constituent parts.
;
; entry:
;     dict - Array of dictionary entries. Each entry is a structure
;            with a subject name, scratch file offset, number of lines
;            of documentation text, etc.
;  infiles - String array containing the name(s) of .pro or .tlb files
;            for which help is being generated.
; libfiles - String array containing the name(s) of .pro files extracted
;            from any .tlb files in the infiles array.
; txt_file - Scratch file containing the documentation text.
;  outfile - NAME of final HELP file to be generated.
;  verbose - TRUE if the routine should output a descriptive message
;            when it finds the documentation header.
;    title - Scalar string containing the name to go at the top of the
;            HTML help page.
;   strict - If nonzero, the routine will adhere strictly to HTML format.
;            The document headers will be scanned for characters that are
;            reserved in HTML (<,>,&,"), which are then converted to the
;            appropriate HTML syntax in the output file.  This should always
;            be nonzero now that we're generating XHTML.
;  version - If nonzero, a version string will be inserted into the XHTML file.
;
; exit:
;    outfile has been created.
;    txt_file has been closed via FREE_LUN.
;
PRO makehh_gen_file,dict,txt_file,infiles,libfiles,outfile,verbose,title,strict,LinkCode,version
    ;
    ; Append unique numbers to any duplicate subject names.
    ;
    dpos = WHERE(dict.id GT 0,ndup)
    IF (verbose NE 0) THEN MESSAGE,/INFO,'Found '+STRTRIM(STRING(ndup),2)+' duplicates!'
    FOR i=0,ndup-1 DO BEGIN
        entry = dict[dpos[i]]
        dict[dpos[i]].subject = entry.subject+'['+STRTRIM(STRING(entry.id),2)+']'
    ENDFOR
    ;
    ; Sort the subjects alphabetically.
    ;
    count = N_ELEMENTS(dict)
    indices = SORT(dict.subject)
    ;
    ; Open the final file.
    ;
    OPENW,final_file,outfile,/STREAM,/GET_LUN
    IF (verbose NE 0) THEN MESSAGE,/INFO,'Building '+outfile+'...'
    ;
    ; Header stuff.
    ;
    PRINTF,final_file,'<?xml version="1.0" encoding="UTF-8"?>'
    PRINTF,final_file,'<!DOCTYPE html'
    PRINTF,final_file,'     PUBLIC "-//W3C//DTD XHTML 1.1//EN"'
    PRINTF,final_file,'     "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">'
    PRINTF,final_file,'<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">'
    ;
    ; Title.
    ;
    PRINTF,final_file,'<head>'
    PRINTF,final_file,'<title>',title,'</title>'
    PRINTF,final_file,'<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />'
    PRINTF,final_file,'</head>'
    ;
    ; Title and intro info.
    ;
    iv = idlutils_version()
    PRINTF,final_file,'<body>'
    PRINTF,final_file,'<h1>',title,'</h1>'
    PRINTF,final_file,'<h2>Overview</h2>'
    PRINTF,final_file,'<p>This page was created by the IDL library routine'
    PRINTF,final_file,'<code>make_html_help</code> (from version '+iv+' of'
    PRINTF,final_file,'idlutils).  For more information on this routine,'
    PRINTF,final_file,'refer to the IDL Online Help Navigator or type: </p>'
    PRINTF,final_file,'<pre>IDL&gt; ? make_html_help</pre>'
    PRINTF,final_file,'<p>at the IDL command line prompt.</p>'
    ;
    ; Add version string
    ;
    IF (version NE '') THEN BEGIN
        version_function = version+'_version'
        ;
        ; See if version_function exists
        ;
        version_exists = FILE_WHICH(version_function+'.pro')
        IF (version_exists NE '') THEN BEGIN
            myversion = CALL_FUNCTION(version_function)
            PRINTF,final_file,'<p>This file is based on version <strong>'+myversion+'</strong> of '+version+'.</p>'
        ENDIF
    ENDIF
    ;
    ; Add last modified string
    ;
    PRINTF,final_file,'<p><strong>Last modified: </strong>',SYSTIME(),'.</p>'
    PRINTF,final_file,'<hr />'
    ;
    ; Index.
    ;
    PRINTF,final_file,'<h2 id="ROUTINELIST">List of Routines</h2>'
    PRINTF,final_file,'<ul>'
    FOR i=0,count-1 DO BEGIN
        entry = dict[indices[i]]
        IF (entry.nline GT 0) THEN $
            PRINTF,final_file,$
                '<li><a href="#',makehh_fix_subject(entry.subject),'">',entry.subject,'</a>',  $
                ' : ', entry.purpose,'</li>'
    ENDFOR
    PRINTF,final_file,'</ul>'
    PRINTF,final_file,'<hr />'
    ;
    ; Descriptions.
    ;
    PRINTF,final_file,'<h2>Routine Descriptions</h2>'
    ON_IOERROR,TXT_DONE
    FOR i=0,count-1 DO BEGIN
        entry = dict[indices[i]]
        IF (entry.nline GT 0) THEN BEGIN
            PRINTF,final_file,'<h3 id="',makehh_fix_subject(entry.subject),'">',entry.subject,'</h3>'
            prev_i = i - 1
            IF (prev_i LT 0) THEN dostep = 0 $
            ELSE BEGIN
                prev_ent = dict[indices[prev_i]]
                dostep = prev_ent.nline EQ 0
            ENDELSE
            WHILE dostep DO BEGIN
                prev_i = prev_i - 1
                IF (prev_i LT 0) THEN dostep = 0 $
                ELSE BEGIN
                    prev_ent = dict[indices[prev_i]]
                    dostep = prev_ent.nline EQ 0
                ENDELSE
            ENDWHILE
            IF (prev_i GE 0) THEN $
                PRINTF,final_file,'<p><a href="#',makehh_fix_subject(prev_ent.subject),'">[Previous Routine]</a></p>'
            next_i = i + 1
            IF (next_i GE count) THEN dostep = 0 $
            ELSE BEGIN
                next_ent = dict[indices[next_i]]
                dostep = next_ent.nline EQ 0
            ENDELSE
            WHILE dostep DO BEGIN
                next_i = next_i + 1
                IF (next_i GE count) THEN dostep = 0 $
                ELSE BEGIN
                    next_ent = dict[indices[next_i]]
                    dostep = next_ent.nline EQ 0
                ENDELSE
            ENDWHILE
            IF (next_i LT count) THEN $
                PRINTF,final_file,'<p><a href="#',makehh_fix_subject(next_ent.subject),'">[Next Routine]</a></p>'
            PRINTF,final_file,'<p><a href="#ROUTINELIST">[List of Routines]</a></p>'
            PRINTF,final_file,'<pre>'
            tmp = ''
            POINT_LUN,txt_file,entry.offset
            FOR j=1,entry.nline DO BEGIN
                READF,txt_file,tmp
                PRINTF,final_file,tmp
            ENDFOR
            PRINTF,final_file,'</pre>'
            IF (entry.lib NE -1L) THEN BEGIN
                fname = libfiles[entry.lib]
                lname = infiles[entry.indx]
                IF (strict) THEN BEGIN
                    makehh_strict,fname
                    makehh_strict,lname
                ENDIF
                PRINTF,final_file,'<p><strong>(See '+fname+' in '+lname+')</strong></p>'
            ENDIF ELSE BEGIN
                fname = infiles[entry.indx]
                IF (strict) THEN makehh_strict,fname
                IF (LinkCode) THEN $
                    PRINTF,final_file,'<p><strong>(See <a href="file:'+fname+'">'+fname+'</a>)</strong></p>' $
                ELSE  $
                    PRINTF,final_file,'<p><strong>(See '+fname+')</strong></p>'
            ENDELSE
            PRINTF,final_file,'<hr />'
        ENDIF
    ENDFOR
TXT_DONE:
    ON_IOERROR,NULL
    FREE_LUN,txt_file
    ;
    ; Footer.
    ;
    PRINTF,final_file,'</body>'
    PRINTF,final_file,'</html>'
    FREE_LUN,final_file
    RETURN
END
;
;----------------------------------------------------------------------------
;
; The main routine
;
PRO make_html_help, sources, outfile, verbose=verbose, title=title, $
    strict=strict, link_files=LinkFiles, version=version
    ;
    ;
    ;
    IF ~KEYWORD_SET(verbose) THEN verbose=0
    IF ~KEYWORD_SET(title) THEN title="Extended IDL Help"
    IF ~KEYWORD_SET(strict) THEN strict=1
    IF ~KEYWORD_SET(LinkFiles) THEN LinkFiles = 0
    IF ~KEYWORD_SET(version) THEN version=''
    infiles = ''
    istlb = 0B
    count = N_ELEMENTS(sources)
    IF (count EQ 0) THEN BEGIN
        MESSAGE,/INFO,'No source IDL directories found.'
        RETURN
    ENDIF
    ;
    ; Open a temporary file for the documentation text.
    ;
    OPENW, txt_file, FILEPATH('userhtml.txt', /TMP), /STREAM, /GET_LUN, /DELETE
    ;
    ; Loop on sources.
    ;
    FOR i=0, count-1 DO BEGIN
        src = sources[i]
        ;
        ; Strip any version numbers from the source so we can check for the
        ; VMS .tlb or .pro extension.
        ;
        vpos = STRPOS(src,';')
        IF (vpos NE -1) THEN vsource = STRMID(src,0,vpos) ELSE vsource = src
        ;
        ; Test if the source is a VMS text library.
        ;
        IF (!VERSION.OS EQ 'vms') AND (STRLEN(vsource) GT 4) AND $
            (STRUPCASE(STRMID(vsource, STRLEN(vsource)-4,4)) EQ '.TLB') THEN BEGIN
            infiles = [infiles,src]
            istlb = [istlb, 1B]
        ENDIF ELSE BEGIN
            ;
            ; Test if the file is a .PRO file.
            ;
            IF (STRUPCASE(STRMID(vsource, STRLEN(vsource)-4,4)) EQ '.PRO') THEN BEGIN
                infiles = [infiles,src]
                istlb = [istlb, 0B]
            ENDIF ELSE BEGIN
                ;
                ; If not a VMS text library or .PRO file, it must be a directory name.
                ;
                tok = PATH_SEP()
                ;
                ; Get a list of all .pro files in the directory.
                ;
                flist = FINDFILE(src+tok+'*.pro',COUNT=npro)
                IF (npro GT 0) THEN BEGIN
                    infiles = [infiles,flist]
                    istlb = [istlb, REPLICATE(0b,npro)]
                ENDIF
                ;
                ; Get a list of all .tlb files in the directory.
                ;
                flist = FINDFILE(src+tok+'*.tlb',COUNT=ntlb)
                IF (ntlb GT 0) THEN BEGIN
                    infiles = [infiles,flist]
                    istlb = [istlb, REPLICATE(1b,ntlb)]
                ENDIF
            ENDELSE
        ENDELSE
    ENDFOR
    count = N_ELEMENTS(infiles)
    IF (count EQ 1) THEN BEGIN
        MESSAGE,/INFO,'No IDL files found.'
        RETURN
    ENDIF
    infiles = infiles[1:*]
    istlb = istlb[1:*]
    count = count-1
    ;
    ; Loop on all files.
    ;
    FOR i=0,count-1 DO BEGIN
        src = infiles[i]
        IF (istlb[i]) THEN BEGIN
            ;
            ; If it is a text library, get a list of routines by spawning
            ; a LIB/LIST command.
            ;
            SPAWN,'LIBRARY/TEXT/LIST ' + src,files
            lib_count = N_ELEMENTS(files)
            j=0
            WHILE ((j LT lib_count) AND (STRLEN(files[j]) NE 0)) DO j = j + 1
            lib_count = lib_count - j - 1
            IF (count GT 0) THEN files = files[j+1:*]
            ;
            ; We do a separate extract for each potential routine. This is
            ; pretty slow, but easy to implement. This routine is generally
            ; run once in a long while, so I think it's OK.
            ;
            lib_total = N_ELEMENTS(libfiles)
            IF (lib_total EQ 0) THEN libfiles = files ELSE libfiles = [libfiles, files]
            FOR j=0, lib_count-1 DO BEGIN
                name = FILEPATH('mkhtmlhelp.scr',/TMP)
                SPAWN,'LIBRARY/TEXT/EXTRACT='+files[j]+'/OUT='+name+' '+src
                makehh_grab_hdr,files[j],dict,i,lib_total+j,txt_file,verbose,strict
            ENDFOR
        ENDIF ELSE BEGIN
            name = infiles[i]
            makehh_grab_hdr,name,dict,i,-1L,txt_file,verbose,strict
        ENDELSE
    ENDFOR
    ;
    ; Generate the HTML file.
    ;
    makehh_gen_file,dict,txt_file,infiles,libfiles,outfile,verbose,title,strict,LinkFiles,version
    RETURN
END
