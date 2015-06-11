Function create_nan, type, DIMENSION=dimension, STRING=string
;+
; NAME:
;       CREATE_NAN()
;       
; PURPOSE:
;       Creates a "Not a Number" value of the desired data type.
;       
; CALLING SEQUENCE:
;       nan = CREATE_NAN(type[, DIMENSION=dimension])
;
; INPUTS:
;       TYPES = Integer describing the type identifier for the "Not a Number" value 
;               to be created. Use the following command to get the type identifier 
;               for variable A : print, size(A,/type)
; 
; OPTIONAL INPUT:
;       DIMENSION - Integer array describing dimensions of the output "Not a Number" values.
;
; OUTPUTS:
;       Here is a table describing possible outputs, depending on desired data type :
;       
;   Data type identifier | Data type             | Returned value 
;   ---------------------------------------------------------
;                     0  | UNDEFINED             | Error
;                     1  | BYTE                  | 0B
;                     2  | INTEGER               | -1
;                     3  | LONG                  | -1L
;                     4  | FLOAT                 | !values.F_NAN
;                     5  | DOUBLE                | double(!values.F_NAN)
;                     6  | COMPLEX               | complex(1,1)*!values.f_nan
;                     7  | STRING                | 'NaN'
;                     8  | STRUCTURE             | Error
;                     9  | DOUBLE COMPLEX        | dcomplex(1,1)*!values.f_nan
;                     10 | POINTER               | Error
;                     11 | OBJECT                | Error
;                     12 | UNSIGNED INTEGER      | 0U
;                     13 | UNSIGNED LONG         | 0UL
;                     14 | 64-BYES LONG          | -1LL
;                     15 | 64-BYES UNSIGNED LONG | 0ULL
; 
; RESTRICTIONS:
;       (1) See table in OUTPUTS section for accepted data types.
;
; PROCEDURES USED:
;       NONE.
;
; MODIFICATION HISTORY:
;       WRITTEN, Jonathan Gagne, Februrary, 12 2012
;-
  
  compile_opt hidden
  on_error, 2
  
  if keyword_set(string) then begin
    
    if n_elements(type) gt 1 then begin
      retval = strarr(n_elements(type))
      for i=0L, n_elements(type)-1L do begin
        retvali = create_nan(type[i], DIMENSION=dimension, /STRING)
        retval[i] = retvali
      endfor
      return, retval
    endif
    
    case type of
      1 : retval = '0B'
      2 : retval = '-1'
      3 : retval = '-1L'
      4 : retval = '!values.f_nan'
      5 : retval = '!values.d_nan'
      6 : retval = 'complex(1,1)*!values.f_nan'
      7 : retval = '''NaN'''
      9 : retval = 'dcomplex(1,1)*!values.d_nan'
      10 : retval = '(ptrarr(1,/allocate))[0]'
      12 : retval = '0U'
      13 : retval = '0UL'
      14 : retval = '-1LL'
      15 : retval = '0ULL'
      else : message, ' Type #'+strtrim(type,2)+' was not recognized.'
    endcase
    
  endif else begin
    
    case type of
      1 : retval = 0B
      2 : retval = -1
      3 : retval = -1L
      4 : retval = !values.f_nan
      5 : retval = !values.d_nan
      6 : retval = complex(1,1)*!values.f_nan
      7 : retval = 'NaN'
      9 : retval = dcomplex(1,1)*!values.d_nan
      10 : retval = (ptrarr(1,/allocate))[0]
      12 : retval = 0U
      13 : retval = 0UL
      14 : retval = -1LL
      15 : retval = 0ULL
      else : message, ' Type #'+strtrim(type,2)+' was not recognized.'
    endcase
    
  endelse
  
  if keyword_set(dimension) then $
    void = execute('retval = replicate(retval,'+strjoin(strtrim(dimension,2),',')+')') 
  
  return, retval
  
End