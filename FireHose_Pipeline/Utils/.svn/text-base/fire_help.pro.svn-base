PRO FIRE_HELP, SETUP=setup, TRACE=trace, FLATS=flats, STRUCTURE=structure, EXTRACT=extract, TELLURIC=telluric, COMBINE=combine, QUICKLOOK=quicklook, HOTKEYS=hotkeys, WIDGET=widget, BOTH=both, _EXTRA = keys

	func_name = "fire_help"
	
	bars = "-----------------------------------------------"


	;------------------------------------------------------
	;; HELP WITH FIREHOSE SECTIONS	
	;------------------------------------------------------
	
	if keyword_set(SETUP) then begin
		msg = [ bars, func_name + ": 'Setup' tab" ]
		msg = [ msg, "  Raw Directory: Directory containing raw data files.  (Type Ctrl+d to view the current raw data directory.)"]
		msg = [ msg, "  Redux Directory: Directory containing all reduced data (intermediate and final)."]
		msg = [ msg, "  Observing catalog: Magellan telescope operator format.  Used to" + $
			" help determine and match up science object files when creating the 'firestrct'" + $
			" data structure.  Not required, but helps reduce time needed to edit the structure." ]
		msg = [ msg, bars ]
	endif	

	if keyword_set(TRACE) then begin
		msg = [ bars, func_name + ": 'Trace' tab" ]
		msg = [ msg, "  Description:  The first task in the reduction is to locate the edges of the orders in " + $
			"our images.  Here we perform that task, as well as create an order mask that labels orders, masks " + $
			"out pixels between orders, and masks out pixels outside a certain fraction of the free spectral range." ]
		msg = [ msg, "  User actions: Use the 'Browse' button to select one or more flats from your data set.  " + $
			"Then hit 'Trace Orders' to create the order structure which contains polynomial fits to the trace edges." ]
		msg = [ msg, "  Code details: Calls fire_findslits.pro.  Outputs an order structure (containing polynomial " + $
			"fits to order edges) to Flat/OStr_fire_*.fits and an order mask to Flat/Orders_*.fits, where * is a range " + $
			"of file numbers." ]
		msg = [ msg, "  ***** WARNING! *****  This code is not very robust!  Many perfectly fine looking flat fields " + $
			"produce bad solutions.  INSPECT YOUR SOLUTIONS.  Small deviations are fine; large deviations are not!" + $
			" If the solutions look bad, then use the 'Interactive' option to edit slit edges by hand." ]
		msg = [ msg, bars ]
	endif
	
	if keyword_set(FLATS) then begin
		msg = [ bars, func_name + ": 'Flats' tab" ]
		msg = [ msg, "  Description:  Creates internal flat and twilight flat images to account for variations" + $
			" between pixels, and creates a slit tilt image that encodes information about constant wavelength cuts " + $
			"through data images.  Because of mechanical drags caused by switching FIRE's slits when it's cryogenically" + $
			" cooled, these flat fields should be created every time the slit is changed."  ]
		msg = [ msg, "  User Actions:  Use the 'Browse' button to select the following (all mandatory):" ]
		msg = [ msg, "    Flat Field Files: One or more internal flat files." ]
		msg = [ msg, "    Illum Flat Files: One or more sky/twilight flat files." ]
		msg = [ msg, "    Slit Tilt File: One file used to determine the tilt of the slit; eg, a science file with " + $
			"OH sky lines burned in." ]
		msg = [ msg, "    Order Mask: One order mask file created when tracing orders in the 'Trace' tab." ]
		msg = [ msg, "Once the above files have been selected, hit 'Make Flat Field' to generate the flat pixel " + $
			"images.  Toggle the 'Interactive' button to print output to screen." ]
		msg = [ msg, "  Code details:  Calls fire_makeflat.pro.  Outputs a twilight flat to Illumflat_+_*.fits" + $
			", an internal pixel flat to Pixflat_*.fits, and slit tilt calibration image to piximg_flats_*.fits, " + $
			"where + is the slit description and * is a range of file numbers." ]
		msg = [ msg, bars ]
	endif
	
	if keyword_set(STRUCTURE) then begin
		msg = [ bars, func_name + ": 'Structure' tab" ]
		msg = [ msg, "  Description: Generate, load, and/or save IDL 'firestrct' structures, which hold all relevent information regarding data processing." ]
		msg = [ msg, "  User Actions: " ]
		msg = [ msg, "    Structure Filename:  Type the desired name of the firestrct structure to load and/or save." ]
		msg = [ msg, "    Generate Structure:  Generate a new firestrct structure from data stored in your data directory. (To print the current value of your data directory, type Ctrl+D.  To change this value, visit the 'Setup' tab.)  This reads in the headers from all data files, determines exposure types, matches flat and arc files to science objects and tellurics, matches tellurics to science objects, assigns unique IDs to all science objects, and fills in the firestrct structure with all this and other general information.  Select the Pixel Flats, Illumination Flats, and Order Masks to use when matching from their respective boxes before beginning.  Also, visit the 'Setup' tab and input your Magellan Observation Catalog to facilitate the science object identification process. (This is optional; toggle on the 'Do not use Catalog' button to generate a structure without the catalog)." ]
		msg = [ msg, "    Load Structure (Short cut: Ctrl+l):  Load a previously stored firestrct structure, the name of which is drawn from the 'Structure Filename' box (must be located in the current directory)." ]
		msg = [ msg, "    Edit (and Save) Structure:  Manually edit the current firestrct structure using firehose's built in GUI.  Saves changes upon exit." ]
		msg = [ msg, "    Run script (and Save):  Runs a user-generated script which contains information on changes to make.  The script should be called 'firestrct_script.txt' and located in your current directory.  See $FIRE_DIR/Doc/help_firestrct_script.txt for documentation/an example of such a script." ]
		msg = [ msg, "    Save Structure (Short cut: Ctrl+s):  Save the currently loaded structure to the name provided in the 'Structure Filename' box (saved to current directory)." ]
		msg = [ msg, "    Pixel Flats, Illum Flats, Order Masks:  Mandatory inputs when generating a structure.  These are all created in the 'Flats' tab. " ]
		msg = [ msg, "    Verbose?/Loud?:  Print out some/a lot of information when running programs." ]
		msg = [ msg, "    Do not use Catalog:  Toggle on to avoid both a warning and a forced IDL stop when generating a structure without inputting a catalog." ] 
		msg = [ msg, "  Code details:  'Generate Structure' calls fire_mkstrct.pro.  'Edit (and Save) Structure' calls fire_editstrct.pro.  'Run Script (and Save)' calls run_firestrct_script.pro." ]
		msg = [ msg, bars ]
	endif
	
	if keyword_set(EXTRACT) then begin
		msg = [ bars, func_name + ": 'Extract' tab" ]
		msg = [ msg, "  Description:  Generates a wavelength solution and sky model for all selected science objects (and their tellurics), and finds then extracts these science objects (and tellurics)." ]
		msg = [ msg, "  User Actions: " ]
		msg = [ msg, "    Run Pipeline (Short cut: Control+e): Generates a wavelength solution, fits a 2D sky model, finds, and extracts all chosen science obects and their tellurics.  Science objects are chosen from the 'Reduce ONLY these objects' box on the right.  Tellurics are stored in the firestrct structures for these objects, and may be edited using either the GUI associated with the 'Edit (and Save) Structure' button or a user-generated ascii file and the 'Run Script (and Save)' button in the 'Structure' tab." ]
		msg = [ msg, "    Singles:  Not currently implemented." ]
		msg = [ msg, "    Interactive:  Plots more intermediate data to screen than usual." ]
		msg = [ msg, "    Clobber:  Generate all extraction data from scratch, even when the appropriate files already exist." ]
		msg = [ msg, "    Verbose:  Print status messages to screen when running the pipeline. " ]
		msg = [ msg, "  Code details: Calls fire_pipe.pro, which in turn makes calls to fire_arc.pro (which generates a wavelength solution), fire_skymodel.pro (which fits a 2D sky to the data), fire_findobj.pro (which finds the object), and fire_echextobj.pro (which extracts the object)." ] 
		msg = [ msg, bars ]
	endif
	
	if keyword_set(TELLURIC) then begin
		msg = [ bars, func_name + ": 'Telluric' tab" ]
		msg = [ msg, "  Description:  Runs a telluric correction on chosen science objects." ]
		msg = [ msg, "  User Actions:  " ]
		msg = [ msg, "    Correct Tellurics (Short cut: Ctrl+t):  Make the telluric corrections on the science objects chosen from the tree in the 'Choose targets to calibrate tellurics' box.  Calls Xtellcor or XtellcorFinish (see 'Never use Xtellcor Finish' below)." ]
		msg = [ msg, "    Never use Xtellcor Finish:  Usually, if a telluric calibration file already exists for a science object's telluric (usually generated for a different science exposure of the same object), then XtellcorFinish is called instead of Xtellcor, which avoids the need to re-construct the kernal.  If this button is toggled to the 'on' position, then Xtellcor is run no matter what.  This option serves as a 'Clobber' feature for telluric correction." ]
		msg = [ msg, "    Rebuild Telluric File Tree:  Searches through the firestrct structure to determine which science objects have been extracted, and re-generates the tree (located in the box on the right) which holds the names of these science files." ]
		msg = [ msg, "  Code details:  Calls fire_telluric.pro, which is a wrapper that calls Xtellcor or XtellcorFinish." ]
		msg = [ msg, bars ]
	endif
	
	if keyword_set(COMBINE) then begin
		msg = [ bars, func_name + ": 'Combine' tab" ]
		msg = [ msg, "  Description: Combines extracted, telluric corrected spectra from multiple exposures of the same science object into one, single exposure." ]
		msg = [ msg, "  User Actions: "]
		msg = [ msg, "    Combine to 1D (Short cut: Ctrl+1): Combines the spectra previously generated for the target highlighted in the 'Choose targets to combine' box." ]
		msg = [ msg, "  Code details:  Calls fire_guicombine.pro.  Outputs a flux spectrum called FSpec/#_F.fits and an error spectrum called FSpec/#_E.fits, where # is the object name." ]
		msg = [ msg, bars ]
	endif
	
	if keyword_set(QUICKLOOK) then begin
		msg = [ bars, func_name + ": 'Quicklook' tab" ]
		msg = [ msg, "  The Quicklook feature has not been implemented yet.  Sorry..." ]
		msg = [ msg, bars ]
	endif

	if keyword_set(HOTKEYS) then begin
		msg = [ bars, func_name + ": Hot keys list" ]
		msg = [ msg, "    Clear firehose output: Ctrl+Shift+c" ]
		msg = [ msg, "    Load firestrct: Ctrl+l" ]
		msg = [ msg, "    Save firestrct: Ctrl+s" ]
		msg = [ msg, "    Display raw data directory: Ctrl+d" ]
		msg = [ msg, "    Run extraction pipeline: Ctrl+e" ]
		msg = [ msg, "    Run telluric correction: Ctrl+t" ]
		msg = [ msg, "    Combine to 1D: Ctrl+1" ]		
		msg = [ msg, "    Quit firehose: Ctrl+q" ]
		msg = [ msg, bars ]	
	endif

	if NOT keyword_set(WIDGET) OR keyword_set(BOTH) then begin
		print, msg
	endif
	
	if keyword_set(WIDGET) then begin
		WIDGET_CONTROL, widget, set_value=msg, _EXTRA = keys	
	endif

END
