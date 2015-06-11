1- path = 'something/something/'
	Include everything from ~ and /raw/ at the end
2- Create directories 'redux' and 'raw' in 'path', put raw data in 'raw'
3- Combine flats_red, flats_blue and arcs, with e.g., combine_fireprism_flats, ind_start, ind_end, path, 'flat_comb_blue'
	Lower exposure times / Low-Voltage are for "red" flats
	Higher exposure times / Hi-Voltage are for "blue" flats
	Use "ff=file_search(path+'*.fits*') & printarr,file_basename(ff),sxpar_mul(ff,'OBJECT'),sxpar_mul(ff,'GRISM'),sxpar_mul(ff,'EXPTIME'),sxpar_mul(ff,'SLIT'),/jus" for a manual log file
	Visually inspect/compare the output flats !
4- Remove /raw at the end of path and then "firehose_ld, path"
5- Leave default "Optimal Weighting" options and extract science data only
	Do the extraction ; Choose "Object File", "Arc File", leave "Sky" blank.
	"Trace slit"" -> "Solve Arc" -> "Make Flat" is only needed once.
	Then "Find Object" -> "Extract", change filename, "Find Object" -> "Extract", repeat.
	For the "solve_arc" part, Use "l" to cut the left part (before ~ 1500), and consult "/21-FireHose/REF_WV_SOL_PRISM_ZOOMK.png" to identify proper lines. Use "t" to cut the top off.
		Use "W" to reset zoom
		Use "d" to delete lines
		Add lines by left-clicking
		Then use "F" to fit the lines
		Click "Done".
		Compare the lines by re-zooming.
		Then press "Done" again.
	"Make Flat" takes like a minute or so, and ends with a display of the flat file.
	"Find Object" ends with a "Compressing" IDL-Console message.
	"Extract" ends in the same way (also with "Elapsed time = X sec")
	Always verify that counts are below 20-25 K (for gain=1.2 e/DN "normal" low-gain mode), especially in PRISM mode and for A standards.
	If an IDL error message occurs, throw computer in the nearest river.
6- quit firehose
7- firehose_ld, path
8- Keep extraction to "optimal".
	Choose the same "Arc file"
	Choose "Object File"
9- Extract tellurics only (no need to redo all steps, just start from "Find Object" and "Extract")
	Quit Firehose when done.
10- fire_xcombspec,path=path+'redux/Object/',prefix_input='ObjStr_',files_input=
	"Files_input" is a string value like "1-15" that will be passed to xspextool.
	No need to close window between each combination, but **DONT FORGET TO CHANGE OUTPUT NAME**
	Choose output name like "[OBJECT]_comb"
11- xtellcor_general,path=path+'redux/Object/'
	FWHM = slit_width (0.6") / (0.15" per pixel) * 0.001 um/pixel = 0.004 um
	Choose output name like "[OBJECT]_tc"
	**DONT FORGET TO CHANGE FILE NAME !**