;+
; NAME:
;     mc_mkomag2flux
;
; PURPOSE:
;     To convert a MKO-NIR magnitude to its equivalent integrated flux.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     mc_mkomag2flux,mag,magerr,band,flux,fluxerr,CANCEL=cancel
;
; INPUTS:
;     mag    - The MKO-NIR magnitude
;     magerr - The error in the MKO-NIR magnitude
;     band   - The MKO-NIR band, 'J', 'H', 'K', 'Lp','Mp'
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem.
;
; OUTPUTS:
;     flux    - The integrated flux in W m-2
;     fluxerr - The error in the integrated flux in W m-2.  
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
;     Converts the MKO-NIR magnitude to an integrated flux via,
;
;     flux = Fvega * 10^{-0.4*(mag+zp)}
;
;     where flux is the integrated flux of the source in W m-2, Fvega
;     is the integrated flux of Vega in W m-2, mag is the observed
;     MKO-NIR magnitude, and zp is the zero point.  We assume that the
;     zero point is zero for all bandpasses.  An integrated flux is
;     given by,
;
;     F = int( lambda * F_lambda * RSR * dlambda)
;
;     where lambda converts the energy flux density to a photon flux
;     density (to account for the photon counting nature of
;     detectors).  The units are a bit weird so I need to think about
;     how to report this later.
;
; EXAMPLE:
;     Later
;
; MODIFICATION HISTORY:
;     2009-02-18 - Written by M. Cushing, Institute for Astronomy, UH
;-
pro mc_mkomag2flux,mag,magerr,band,flux,fluxerr,CANCEL=cancel

  cancel = 0

  if n_params() lt 2 then begin
     
     print, 'Syntax - mc_mkomag2flux(mag,magerr,flux,fluxerr,CANCEL=cancel)'
     cancel = 1
     return

  endif

  cancel = cpar('mc_mkomag2flux',mag,'Mag',1,[1,2,3,4,5],0)
  if cancel then return
  cancel = cpar('mc_mkomag2flux',magerr,'Magerr',2,[1,2,3,4,5],0)
  if cancel then return
  cancel = cpar('mc_mkomag2flux',band,'Band',3,7,0)
  if cancel then return

;  Load Vega and convert to microns and W m-2 um-1

  spantoolpath = file_dirname(file_dirname(file_which('spantool.pro'),/MARK))
  spextoolpath = file_dirname(file_dirname(file_which('spextool.pro'),/MARK))

  restore, filepath('lvega99.sav',ROOT_DIR=spextoolpath,SUBDIR='data')
  wvin = temporary(wvin)/10000.
  fvin = temporary(fvin)*10.


;  Load filter info

  case band of 

     'J': begin

        mzp = 0.0
        readcol,filepath('atMKO_J.dat',ROOT_DIR=spantoolpath, $
                         SUBDIR='data'), $
                wtrans,ttrans,FORMAT='D,D',COMMENT='#',/SILENT


     end

     'H': begin

        mzp = 0.0
        readcol,filepath('atMKO_H.dat',ROOT_DIR=spantoolpath, $
                         SUBDIR='data'), $
                wtrans,ttrans,FORMAT='D,D',COMMENT='#',/SILENT

     end

     'K': begin

        mzp = 0.0
        readcol,filepath('atMKO_K.dat',ROOT_DIR=spantoolpath, $
                         SUBDIR='data'), $
                wtrans,ttrans,FORMAT='D,D',COMMENT='#',/SILENT
        
     end

     'Ks': begin

        mzp = 0.0
        readcol,filepath('atMKO_Ks.dat',ROOT_DIR=spantoolpath, $
                         SUBDIR='data'), $
                wtrans,ttrans,FORMAT='D,D',COMMENT='#',/SILENT
        
     end

     'Kp': begin

        mzp = 0.0
        readcol,filepath('atMKO_Kp.dat',ROOT_DIR=spantoolpath, $
                         SUBDIR='data'), $
                wtrans,ttrans,FORMAT='D,D',COMMENT='#',/SILENT
        
     end

     'Lp': begin

        mzp = 0.0
        readcol,filepath('UKIRT-MKO_Lp.dat',ROOT_DIR=spantoolpath, $
                         SUBDIR='data'), $
                wtrans,ttrans,FORMAT='D,D',COMMENT='#',/SILENT
        
     end

     'Mp': begin

        mzp = 0.0
        readcol,filepath('UKIRT-MKO_Mp.dat',ROOT_DIR=spantoolpath, $
                         SUBDIR='data'), $
                wtrans,ttrans,FORMAT='D,D',COMMENT='#',/SILENT
        
     end

     else:  begin

        print, 'Unknown bandpasses.  Choices are J, H, K, Ks, Kp, Lp, Mp.'
        cancel = 1
        return

     end

  endcase

;  Interpolate Vega onto the filter profile

  linterp,wvin,fvin,wtrans,rfvin

;  Compute quantities

  ivega   = int_tabulated(wtrans,wtrans*rfvin*ttrans)
  flux    = ivega * 10^(-0.4*(mag+mzp))
  fluxerr = flux*0.4*alog(10)*magerr


end
