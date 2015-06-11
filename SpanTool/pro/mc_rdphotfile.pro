;+
; NAME:
;     mc_rdphotfile
;    
; PURPOSE:
;     To read in the photometry file used by xsyntphot
;    
; CATEGORY:
;     File I/O
;
; CALLING SEQUENCE:
;     mc_rdphotfile,filename,nsystems,systems,filters,filenames,photons,
;                   vegazero,CANCEL=cancel
;    
; INPUTS:
;     filename  - The name of the photsystem file.
;         
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;     
; OUTPUTS:
;     nsystems  - The number of photometric systems
;     systems   - A string array with the names of the photometric systems
;     fiilters  - A structure where each field is a string array of
;                 the names of filters for each system
;     filenames - A structure where each field is a string array of 
;                 the files name with the transmissions of each filter
;                 for each system
;     photons   - An array of 1s and 0s.  
;                 1 - The filter curves should be corrected for the
;                     photon-counting nature of detectors.
;                 0 - The filter curves should not be corrected for the
;                     photon-counting nature of detectors.
;     vega      - An array of magnitude zero points for Vega
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
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2002-02-10 - Written by M. Cushing, Institute for Astronomy, UH
;     2003-05-20 - Added the photons input parameter, M. Cushing
;
;-
pro mc_rdphotfile,filename,nsystems,systems,filters,filenames,photons, $
                  vegazero,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 1 then begin
    
    cancel = 1
    print, 'Syntax - filename,nsystems,systems,filters,filenames,photons,$'
    print, '                  vegazero,CANCEL=cancel'
    return

endif

cancel = cpar('readphotfile',filename, 1,'Filename',7,0)
if cancel then return

nsystems = file_lines(filename)

openr,lun,filename,/GET_LUN
line = ''

for i = 0,nsystems-1 do begin
    
    readf,lun,line
    parse = strsplit(line,' ',/EXTRACT)
    key = 'system'+string(i+1,format='(i2.2)')
    if i eq 0 then begin

        systems   = strtrim(parse[0],2) 
        filters   = create_struct(key,strsplit(parse[1],',',/EXTRACT))
        filenames = create_struct(key,strsplit(parse[2],',',/EXTRACT))
        photons   = fix(parse[3])
        vegazero  = float(parse[4])


    endif else begin

        systems   = [systems,strtrim(parse[0],2)]
        filters   = create_struct(filters,key,strsplit(parse[1],',',/EXTRACT))
        filenames =create_struct(filenames,key,strsplit(parse[2],',',/EXTRACT))
        photons   = [photons,fix(parse[3])]
        vegazero  = [vegazero,float(parse[4])]

    endelse

endfor

end
