;+
; NAME:
;     mc_chfunits (CHange Flux UNITS)
;
;
; PURPOSE:
;     To convert the flux density units of a spectrum
;
;
; CATEGORY:
;     Spectroscopy
;
;
; CALLING SEQUENCE:
;     result = mc_chfunits(iwave,iflux,iwunit,ifunit,ofunit,IERROR=ierror,$
;                         OERROR=error,CANCEL=cancel)
;
;
; INPUTS:
;     iwave   - The wavelength array
;     iflux   - The flux array 
;     iwunit  - The units of the wavelength array
;                 0 = microns
;                 1 = nanometers
;                 2 = Angstroms
;     ifunit  - The units of the input flux array
;                 0 = W m-2 um-1
;                 1 = ergs s-1 cm-2 A-1
;                 2 = W m-2 Hz-1
;                 3 = ergs s-1 cm-2 Hz-1
;                 4 = Jy
;                 5 = W m-2
;                 6 = ergs s-1 cm-2
;     ofunit  - The units of the output flux array
;                 0 = W m-2 um-1
;                 1 = ergs s-1 cm-2 A-1
;                 2 = W m-2 Hz-1
;                 3 = ergs s-1 cm-2 Hz-1
;                 4 = Jy
;                 5 = W m-2
;                 6 = ergs s-1 cm-2
;                 7 = photons s-1 m-2 um-1
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     IERROR - If given, the error array is converted as well.
;     OERROR - The output error array if IERROR is given.
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     results - Contains the modified flux array
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
;     Convert the input wavelength and flux density array to units of
;     microns and ergs s-1 cm-2 A-1 and then converts the flux density
;     array to the requested units.
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
;     2004-02-25 - Written by M. Cushing, NASA Ames Research Center
;     2004-11-19 - Added the IERROR and OERROR keywords.
;
;-
function mc_chfunits,wave,flux,iwunit,ifunit,ofunit,IERROR=ierror,$
                     OERROR=oerror,CANCEL=cancel

  cancel = 0
  
  if n_params() lt 5 then begin
     
     print, 'Syntax - results = mc_chfunits(wave,flux,iwunit,ifunit,ofunit,$'
     print, '                               IERROR=ierror,OERROR=oerror,$'
     print, '                               CANCEL=cancel)'
     cancel = 1
     return,-1
     
     
  endif

  cancel = cpar('mc_chfunits',wave,1,'Wave',[2,3,4,5],[0,1,2])
  if cancel then return,-1
  cancel = cpar('mc_chfunits',flux,2,'Flux',[2,3,4,5],[0,1,2])
  if cancel then return,-1
  cancel = cpar('mc_chfunits',iwunit,3,'Iwunit',[2,3,4,5],0)
  if cancel then return,-1
  cancel = cpar('mc_chfunits',ifunit,4,'Ifunit',[2,3,4,5],0)
  if cancel then return,-1
  cancel = cpar('mc_chfunits',ofunit,5,'Ofunit',[2,3,4,5],0)
  if cancel then return,-1
  
  if n_elements(IERROR) ne 0 then begin
     
     cancel = cpar('mc_chfunits',ierror,6,'IERROR',[2,3,4,5],[0,1,2])
     if cancel then return,-1
     doerr = 1
     
  endif else doerr = 0

  if ifunit eq ofunit then begin

     if doerr then oerror = ierror
     return, flux

  endif

  h = 6.6260755D-34             ;  J s-1
  c = 2.99792458D+8             ;  m s-1
  
;  Convert input wavelength to microns
  
  case iwunit of 
     
     0: nwave = wave
     
     1: nwave = wave/1000.
     
     2: nwave = wave/10000.
     
     else: begin
        
        print, 'Unkown input wavelength code.'
        cancel = 1
        return, -1
        
     end
     
  endcase
  
;  Convert input flux to erg s-1 cm-2 A-1
  
  case ifunit of 
     
     0: begin
        
        nflux = flux/10D
        if doerr then nerror = ierror/10D
        
     end
     
     1: begin
        
        nflux = flux
        if doerr then nerror = ierror
        
     end
     
     2: begin
        
        nflux = flux / nwave^2 * (c / 1.0D-5)
        if doerr then nerror = ierror / nwave^2 * (c / 1.0D-5)
        
     end
     
     3: begin
        
        nflux = flux / nwave^2 * (c / 1.0D-2)
        if doerr then nerror = ierror / nwave^2 * (c / 1.0D-2)
        
     end
     
     4: begin
        
        nflux = flux / nwave^2 * (c / 1.0D21)
        if doerr then nerror = ierror / nwave^2 * (c / 1.0D21)
        
     end
     
     5: begin
        
        nflux = flux / nwave / 10D
        if doerr then nerror = ierror / nwave / 10D
        
     end
     
     6: begin
        
        nflux = flux / (nwave*10000.)
        if doerr then nerror = ierror / (nwave*10000.)
        
     end
     
     else: begin
        
        print, ' '
        print, 'Unkown input flux density code.'
        print, ' '
        cancel = 1
        return, -1
        
     end
     
  endcase
  
;  Now convert to requested units
  
  case ofunit of 
     
     0: begin
        
        oflux = nflux*10D
        if doerr then oerror = nerror*10D
        
     end
     
     1: begin
        
        oflux = nflux
        if doerr then oerror = nerror
        
     end
     
     2: begin
        
        oflux = nflux * nwave^2 * (1.0D-5 / c)
        if doerr then oerror = nerror * nwave^2 * (1.0D-5 / c)
        
     end
     
     3: begin
        
        oflux = nflux * nwave^2 * (1.0D-2 / c)
        if doerr then oerror = nerror * nwave^2 * (1.0D-2 / c)
        
     end
     
     4: begin
        
        oflux = nflux * nwave^2 * (1.0D21 / c)
        if doerr then oerror = nerror * nwave^2 * (1.0D21 / c)
                
     end
     
     5: begin
        
        oflux = nflux*10D*nwave
        if doerr then oerror = nerror*10D*nwave
              
     end
     
     6: begin
        
        oflux = nflux*nwave*10000.
        if doerr then oerror = nerror*nwave*10000.
                
     end
     
     7: begin
        
        oflux = nflux * 10. * nwave / (h*c*10D^6)
        if doerr then oerror = nerror * 10D * nwave / (h*c*10D^6)
        
     end
     
     else: begin
        
        print, ' '
        print, 'Unkown output flux density code.'
        print, ' '
        cancel = 1
        return,-1
        
     end
     
  endcase

  return, oflux
  
end
