;+ 
; NAME:
; wfc3_g280_mkcalibstrct
;
; PURPOSE:
;    Creates a structure for guiding WFC3/G280 data reduction
;  First code in our data reduction pipeline.  In particular, contains
;  hard-coded polynomials for tracing and wavelength solution.
;
; CALLING SEQUENCE:
;   
;   wfc3_strct = wfc3_g280_mkcalibstrct() 
;
; INPUTS:
;
; RETURNS:
;
; OUTPUTS:
;   wfc3_strct -- A structure for guiding data reduction
;
; OPTIONAL KEYWORDS:
;   /AXE1  -- Use the first AXE team solution for BeamA (not
;             recommended)
;
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;
; EXAMPLES:
;   wfc3_strct = wfc3_g280_mkcalibstrct() 
;
; PROCEDURES CALLED:
;
; REVISION HISTORY:
;   23-Dec-2010 Written by JXP/JMO
;------------------------------------------------------------------------------

function wfc3_g280_mkcalibstrct, AXE1=axe1

  ;make the structure
    struct = { $
             READNO: 3.1, $  ; electrons
             ;;BEAM A
             BEAMA: [-280L,10L], $
             DYDX_A_0:  0.0, $	
             DYDX_A_1: 5.41282E-01, $  ;; Values from November 2010
             DYDX_A_2: 5.28737E-03 , $
             DYDX_A_3: 2.89442E-05 , $
             DYDX_A_4: 8.76667E-08 , $
             DYDX_A_5: 1.10904E-10, $
             XOFF_A:  0.0, $
             YOFF_A: 1.91246E+02, $
             XOFF_A2: 0, $  ; KLUDGE OFFSET
             YOFF_A2: 0, $  ;KLUDGE OFFSET
             DLDP_A_0:  1.70511E+03, $  ;; Values from November 2010  
             DLDP_A_1: -8.58640E+00, $
             DLDP_A_2:  5.23899E-02, $
             DLDP_A_3:  2.17713E-04, $  
             DLDP_A_4:  3.43804E-07, $
             ;; BEAM B
             BEAMB: [90L,115L], $
             DYDX_B_0:  0.20708362, $
             DYDX_B_1:  1.6027558, $
             XOFF_B: 0, $
             XOFF_B2: 4, $
             YOFF_B: 0, $
             YOFF_B2: 8, $
             ;;BEAM C
             BEAMC: [225L,480L], $
             DYDX_C_0: 0.0, $	
             DYDX_C_1: -8.1558871, $	
             DYDX_C_2:  0.031526679, $	
             DYDX_C_3: -5.4800719e-05, $
             DYDX_C_4:  3.5737877e-08, $
             XOFF_C:  0, $
             XOFF_C2:  -3, $
             YOFF_C:  950.05922, $
             YOFF_C2:  -1, $
             DLDP_C_0:  -4.33783E+05, $
             DLDP_C_1:   1.75512E+03, $
             DLDP_C_2:  -2.68146E+00, $
             DLDP_C_3:   1.83546E-03, $
             DLDP_C_4:  -4.70866E-07, $
             ;;sensitivity 
;             sensawave: sensa.wavelength, $
;             sensaflux: sensa.sensitivity, $
;             senscwave: sensc.wavelength, $
;             senscflux: sensc.sensitivity, $
             ;;qso templates
;             sdss_templatewave: sdsswave, $
;             sdss_templateflux: sdssflux, $
;             zheng_templatewave: zhengwave, $
;             zheng_templateflux: zhengflux, $
             ;;trace polynomials
             tracea_poly: [0.,-0.0123543,3.45339e-6], $
             tracec_poly:[0.,-0.0102587,3.48597e-5], $
             sigmaa_poly: [1.2287,0.00085121,-2.07422e-05,5.87603e-08] $
             }

    if keyword_set(AXE1) then begin ;; First set of AXE team values
       struct.DYDX_A_1=  5.30394E-01 
       struct.DYDX_A_2=  4.97180E-03	
       struct.DYDX_A_3=  2.51954E-05
       struct.DYDX_A_4=  6.84991E-08
       struct.DYDX_A_5=  7.60109E-11
       struct.YOFF_A= 1.91133E+02
       ;; Wavelengths
       struct.DLDP_A_0=  1.68368E+03 
       struct.DLDP_A_1= -9.66614E+00
       struct.DLDP_A_2= 3.65270E-02
       struct.DLDP_A_3= 1.29000E-04
       struct.DLDP_A_4=  1.76563E-07
    endif

    return, struct

end
