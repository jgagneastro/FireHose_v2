;
;	NAME:
;		ml_expand
;
;	PURPOSE:
;		expands an array of size [NBLOCK] to size [TOTFIBER], where NBLOCK is the number of blocks on a spectrograph and TOTFIBER is the total number of fibers
;
;	INPUTS:
;		fp - a structure containing arrays of fiber parameters for each block on a spectrograph ; e.g. number of fibers within each block, or fiber sizes within each block
;			 if options set WIDTH or GENERIC, then fp can be an 1d or 2d array that gets expanded
;				WIDTH   - fp must be an array of size [NROWS, NBLOCK] containing the line profile widths for each block, for all rows on the CCD
;				GENERIC - fp can be any 1d or 2d array 
; 
;		nfiber - an array of size NBLOCK containing the number of fibers within each block
;
;	OPTIONAL INPUTS:
;		totfiber  - a scalar of the total number of fibers on a spectrograph
;		
; 	OPTIONAL OUTPUTS:
;		BUNDLEID - an array of size TOTFIBER, containing the block id for each fiber 
;		RADIUS   - an array of size TOTFIBER, containing the radius in pixels used for boxcar fiber extraction
;		FSIZE    - an array of size TOTFIBER, containing the size of the fibers in arcsec
;		WIDTH    - a 2d array of size [NROWS,TOTFIBER], containing the line widths (sigmas) for each fiber, across all rows on the CCD
;		GENERIC  - any array that has been expanded. If used, then input FP is any array of size NBLOCK
;
;	EXAMPLE:
;
;   NORMAL mode - using a structure of parameters
;		fp = { totfiber:101, $
;			    nfiber:[30,19,37,15], $
;               radius:[2.0,2.0,2.0,3.0], $
;               fsize:[2.0,3.0,2.0,5.0], $
;               fiberspace:[6.5,4.0,6.5,8.1], $
;               bundlegap:[12.5,11.0,15.5,16.3]}
;       help, fp,/str
;   ** Structure <30b09a8>, 6 tags, length=76, data length=74, refs=1:
;   TOTFIBER        INT            101
;   NFIBER          INT       Array[4]
;   RADIUS          FLOAT     Array[4]
;   FSIZE           FLOAT     Array[4]
;   FIBERSPACE        FLOAT     Array[4]
;   BUNDLEGAP          FLOAT     Array[4]
;
;	ml_expand, fp, fp.nfiber, radius=radius, fsize=fsize, bundleid=bid
;   help, radius, fsize, bid
;	RADIUS          FLOAT     = Array[101]
;	FSIZE           FLOAT     = Array[101]
;	BID             LONG      = Array[101]
;
;   print, bid
;           0           0           0           0           0           0           0           0           0           0           0           0           0           0           0
;           0           0           0           0           0           0           0           0           0           0           0           0           0           0           0
;           1           1           1           1           1           1           1           1           1           1           1           1           1           1           1
;           1           1           1           1           2           2           2           2           2           2           2           2           2           2           2
;           2           2           2           2           2           2           2           2           2           2           2           2           2           2           2
;           2           2           2           2           2           2           2           2           2           2           2           3           3           3           3
;           3           3           3           3           3           3           3           3           3           3           3
;	print, radius
;		2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0
;		2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 3.0 3.0 3.0 3.0 3.0 3.0 3.0 3.0
;		3.0 3.0 3.0 3.0 3.0 3.0 3.0
;
;   GENERIC mode  - expand an array containing the fiber sizes in arcsec for each block
;   	nfiber = [30, 19, 37, 15]
;		totfiber = 101    
;		fsize = [2.0, 3.0, 2.0, 5.0]
;   	ml_expand, fsize, nfiber, totfiber, generic=fullfsize
;   	print, fullfsize
;			2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 3.0 3.0 3.0 3.0 3.0 3.0 3.0 3.0 3.0 3.0 3.0 3.0 3.0 3.0 3.0 3.0 3.0
;			3.0 3.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 5.0 5.0 5.0 5.0 5.0 5.0 5.0 5.0
;			5.0 5.0 5.0 5.0 5.0 5.0 5.0
;
;	NOTES:
;		currently limited to expanding a few parameters inside the structure ; use GENERIC mode to expand any array - in this mode however, cannot output more than one array at a time 
;
;	HISTORY:
;		Jan 2013 - written by B. Cherinka
;	

pro ml_expand, fp, nfiber, totfiber, BUNDLEID=bundleid, RADIUS=radius, FSIZE=fsize, WIDTH=width, GENERIC=GENERIC

on_error,0
compile_opt idl2
compile_opt idl2, hidden

if n_elements(NFIBER) eq 0 then begin
 print, 'ERROR: MUST PROVIDE NFIBER array!
 return
endif

if n_elements(totfiber) eq 0 then totfiber = total(nfiber)

d=lindgen(totfiber)
h=histogram(total(nfiber,/cumul)-1,min=0,/binsize,reverse_indices=ri)
ind=ri[0:n_elements(h)-1]-ri[0]

if ~arg_present(width) and ~arg_present(generic) and size(fp,/type) ne 8 then begin
	print, 'ERROR: INPUT IS WRONG TYPE! MUST BE A STRUCTURE'
	return
endif

if (arg_present(width) or arg_present(generic)) and size(fp,/type) eq 8 then begin
	print, 'ERROR: INPUT IS WRONG TYPE! MUST BE AN ARRAY!'
	return
endif

bundleid = d[ind]    ;bundleid
if arg_present(radius) then radius = fp.radius[ind] ;fiber extraction radius
if arg_present(fsize) then fsize = fp.fsize[ind]   ;fiber size
if arg_present(fibspace) then fibspace = fp.fiberspace[ind] ;fiber spacing
if arg_present(bungap) then bungap = fp.bundlegap[ind] ;block spacing

;width of profiles
if arg_present(width) then begin
  sz = size(fp,/dimens)
  width = fltarr(sz[0],totfiber)
  for row=0,sz[0]-1 do width[row,*] = fp[row,ind]
endif

;generic input array
if arg_present(generic) then begin
	if size(fp,/n_dim) eq 1 then generic=fp[ind] else begin
		sz = size(fp,/dimens)
		generic = fltarr(sz[0], totfiber)
		for row=0,sz[0]-1 do generic[row,*] = fp[row,ind]
	endelse
endif

return

end