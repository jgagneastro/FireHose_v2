;+
; NAME:
;     mkpslist
;    
; PURPOSE:
;     Creates a postscript file with the program names and their purposes.
;
; CATEGORY:
;     Miscellaneous
;
; CALLING SEQUENCE:
;     mkpslist
;
; INPUTS:
;     None
;    
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL   - Set on return if there is a problem
;     
; OUTPUTS:
;     Writes a ps file to disk
;
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     None
;
; SIDE EFFECTS:
;     None
;
; RESTRICTIONS:
;     None
;
; PROCEDURE:
;     None
;
; EXAMPLE:
;     None
;     
; MODIFICATION HISTORY:
;     2003-03-25 - Written by M. Cushing, Institute for Astronomy, UH
;-
pro mkpslist,CANCEL=cancel

result = findfile('*.pro',COUNT=count)
print, result


cats = ['File I/O','Fitting Data','Image Manipulation','Mathematical',$
        'Miscellaneous','Plotting and Image Display','Spectroscopy',$
        'Statistics','Utility','Widget']

other = ['centertlb','cmps_form','coyote_field2','diagovec','findidx',$
         'gaussj','interpolv','rieke_unred','savitzky_golay',$
         'showprogress__define','sincinterp','str_size','tvimage',$
         'wheretomulti']+'.pro'


num = 0
for i = 0,count-1 do begin

    idx = strmatch(other,result[i])
    if total(idx) ne 1 then begin

        str = strarr(9)
        openr,lun,result[i],/GET_LUN
        readf,lun,str 
        free_lun, lun

        prog = strtrim(result[i],2)
        cat  = strtrim(strmid(str[8],1),2)
        desc = strtrim(strmid(str[5],1),2)

;  Check for underscores

        pos = strpos(prog,'_')
        if pos ne -1 then prog = strjoin(strsplit(prog,'_',/EXTRACT),'\_')

        pos = strpos(desc,'_')
        if pos ne -1 then desc = strjoin(strsplit(desc,'_',/EXTRACT),'\_')

        arrinfo = (num eq 0) ? [prog,cat,desc]:[[arrinfo],[prog,cat,desc]]
        num = num + 1
        
    endif

endfor

openw,lun,'../list.tex',/GET_LUN
printf, lun, '\documentclass[11pt]{article}'
printf, lun, '\usepackage{longtable}
printf, lun, '\begin{document}'
printf, lun, '\oddsidemargin=0.0in'
printf, lun, '\marginparwidth=0.0in'
printf, lun, '\marginparsep=0.0in'
printf, lun, '\textwidth=6.25in'
printf, lun, '\textheight=9.0in'
printf, lun, '\parskip=0.1in'


printf, lun, '\begin{center}'
printf, lun, '\textbf{Listing of Spextool Procedures and Functions}'
printf, lun, '\end{center}'
printf, lun, ' '

printf, lun, '\begin{longtable}{ll}'

for i = 0,n_elements(cats)-1 do begin
    
    printf, lun, '\textbf{'+strtrim(cats[i],2)+'} & \\'
    z = where(arrinfo[1,*] eq cats[i],count)    

    for j = 0,count-1 do printf, lun, strtrim(arrinfo[0,z[j]],2)+$
      ' & '+strtrim(arrinfo[2,z[j]],2)+' \\'

    printf, lun, ' & \\'

endfor

printf, lun, '\textbf{Borrowed or Modified} & \\'
for i = 0, n_elements(other)-1 do begin

    pos = strpos(other[i],'_')
    if pos ne -1 then other[i] = strjoin(strsplit(other[i],'_',/EXTRACT),'\_')
    printf, lun, strtrim(other[i],2)+' & \\'

endfor
printf, lun, '\end{longtable}'

printf, lun, '\end{document}'
free_lun,lun



end
