;+
; NAME:
;     mc_chwunits
;
; PURPOSE:
;     To convert wavelengths between various units
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = mc_chwunits(wave,iwunit,owunit,OWTYPE=owtype,CANCEL=cancel)
;
; INPUTS:
;     wave   - The input wavelength array
;     iwunit - The input wavelength unit
;              0 = microns
;              1 = nanometers
;              2 = Angstroms 
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     owtype - The type of the output wavelength unit 
;              1 = vacuum
;              2 = air
;              ***Note*** This assumes the wavelengths are in the
;              other format!!!!!
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     Returns the modified wavelength array
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
;     Requires the IDL Astronomy User's Library
;
; PROCEDURE:
;     Obvious
;
; EXAMPLE:
;     None     
;
; MODIFICATION HISTORY:
;     2004-11-21 - Written by M. Cushing, NASA Ames Research Center
;
;-
function mc_chwunits,wave,iwunit,owunit,OWTYPE=owtype,CANCEL=cancel

cancel = 0

if n_params() lt 3 then begin

    print, 'Syntax - results = mc_chwunits(wave,iwunit,owunit,OWTYPE=owtype,$'
    print, '                               CANCEL=cancel)'
    cancel = 1
    return,-1


endif

cancel = cpar('mc_chwunits',wave,1,'Wave',[2,3,4,5],1)
if cancel then return,-1
cancel = cpar('mc_chwunits',iwunit,2,'Iwunit',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('mc_chwunits',owunit,3,'Owunit',[2,3,4,5],0)
if cancel then return,-1

;  Convert wavelengths to Angstroms

c = 2.99792458e+8

case iwunit of 

    0: nwave = wave*10000.

    1: nwave = wave*10.

    2: nwave = wave

    3: nwave = 1./wave*1e8

    else: begin

        print, ' '
        print, 'Unknown input wavelength code.'
        print, ' '
        cancel = 1
        return, -1

    end

endcase



;  Convert air/vaccum

if n_elements(OWTYPE) ne 0 then begin

    case owtype of 

        1: airtovac,nwave

        2: vactoair,nwave

        else:  begin

            print, ' '
            print, 'Unknown wavelength code.'
            print, ' '
            cancel = 1
            return, -1

        end

    endcase
endif


;  Convert to output units

case owunit of 

    0: nwave = temporary(nwave)/10000.

    1: nwave = temporary(nwave)/10.

    2: 

    3:  nwave = 1./temporary(nwave)*1e8

    else: begin

        print, ' '
        print, 'Unknown output wavelength code.'
        print, ' '
        cancel = 1
        return, -1

    end

endcase

return, nwave



end
