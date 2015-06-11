;+
; NAME:
;     cpar (Modified zparcheck from the Astronomer Users Library)
;       
; PURPOSE:
;     Checks user parameters passed to a procedure.
;
; CATEGORY:
;     File I/O
;
; CALLING SEQUENCE:
;     cancel = cpar(progname,parameter,parname,parnum,types,dimens)
;
; INPUTS: 
;     progname  - Scalar string name of calling procedure
;     parameter - Parameter passed to the routine
;     parnum    - Integer parameter number
;     parname   - String parameter name
;     types     - Integer scalar or vector of valid types
;                  1 - byte        2 - integer   3 - int*4
;                  4 - real*4      5 - real*8    6 - complex
;                  7 - string      8 - structure 9 - double complex
;                 10 - pointer    11 - object ref 12 - Unsigned integer
;                 13 - unsigned int*4 
;                 14 - int*8  
;                 15 - Unsigned int*8
;     dimens    - Integer scalar or vector giving number
;                 of allowed dimensions
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;       
; OUTPUTS: 
;     None
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
; PROCEDURES CALLED:
;     Requires the Astronomy User's Library
;     Spextool package
;    
; EXAMPLE:
;     mczparcheck,'meancomb',images,1,'Images',[2,3,4,5],2,CANCEl=cancel
;     if cancel then return
; 
; PROCEDURE: 
;     Check parameter against user inputs
;
; REVISION HISTORY:
;     2003-03-21 - Written by M. Cushing, Institute for Astronomy, UH
;                  Modified zparcheck 
;     2006-01-29 - Modified to allow the user to NOT enter a limit on
;                  the dimensions.
;                                       
FUNCTION cpar,progname,parameter,parnum,parname,types,dimens

cancel = 0

IF n_params() LT 5 THEN BEGIN

    print, 'Syntax - cancel = cpar(progname,parameter,parnum,parname,$'
    print, '                       types,dimens)'

endif

; get type and size of parameter

  s = size(parameter)
  ndim = s[0]
  type = s[ndim+1]

; check if parameter defined.

  if type EQ 0 then begin
        err = ' is undefined.'
        goto, ABORT 
  endif

; check for valid dimensions

  if n_params() eq 6 then begin

     valid = where( ndim EQ dimens, Nvalid)
     if Nvalid LT 1 then begin
        err = 'has wrong number of dimensions.'
        goto, ABORT   
     endif

  endif

; check for valid type

  valid = where(type EQ types, Ngood)
  if ngood lt 1 then begin
        err = 'is an invalid data type.'
        goto, ABORT   
  endif

  return,cancel

; bad parameter

ABORT:
  cancel = 1
  print,string(7b) + 'Parameter '+strtrim(parnum,2) +', '+$
    strtrim(parname,2)+', of routine ', strupcase(progname) + ' ', err
  sdim = ' '
  for i = 0,N_elements(dimens)-1 do begin
        if dimens[i] eq 0 then sdim = sdim + 'scalar' $
                          else sdim = sdim + string(dimens[i],'(i3)')
  end
  print,'Valid dimensions are:'+sdim

  stype = ' '
  for i = 0, N_elements( types )-1 do begin
        case types[i] of
                1: stype = stype + ' byte'
                2: stype = stype + ' int*2'
                3: stype = stype + ' int*4'
                4: stype = stype + ' real*4'
                5: stype = stype + ' real*8'
                6: stype = stype + ' complex'
                7: stype = stype + ' string'
                8: stype = stype + ' structure'
                9: stype = stype + ' dcomplex'
               10: stype = stype + ' pointer'
               11: stype = stype + ' Object'
               12: stype = stype + ' Unsigned(i*2)'
               13: stype = stype + ' Unsigned(i*4)'
               14: stype = stype + ' int*8'
               15: stype = stype + ' Unsigned(i*8)'
        endcase
  endfor
  print,'Valid types are:' + stype
  return, cancel

end
