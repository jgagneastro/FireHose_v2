;; Sets the defaults for firestrct structures created by fire_create_struct.

FUNCTION firestrct_set_defaults, strct, TAGNAME=tagname
;; Sets the default values for structures created by strct = {"firestrct"} 
;; or strct = create_struct(name="firestrct")

	func_name = "firestrct_set_defaults()"

	tags = tag_names(strct)
	ntags = n_elements(tags)
	
	FOR i=0, ntags -1 DO BEGIN
	
		tag = tags[i]
		CASE tag OF
		
			;; Specially chosen values.
			'USE': strct.use = 1
			'AIRMASS': strct.airmass = -1.0d
			'RA_DEG': strct.ra_deg = 999.0d
			'DEC_DEG': strct.dec_deg = 999.0d
			'OBJ_ID': strct.obj_id = -1
			'TBMAGS': strct.tbmags = fire_string(!Values.F_NAN)
			'TVMAGS': strct.tvmags = fire_string(!Values.F_NAN)
			
			;; Set default values for tags not
			;; listed above.  These default values
			;; are deteremined by data types			
			ELSE: BEGIN

				;; Determine data type associated with this tag
				a = EXECUTE("type = size(strct." + tag + ", /type)")

				CASE type OF
					
					;; int
					2: a = EXECUTE("strct." + tag + " = 0")
					
					;; long
					3: a = EXECUTE("strct." + tag + " = 0L")

					;; float
					4: a = EXECUTE("strct." + tag + " = 0.0")
					
					;; double
					5: a = EXECUTE("strct." + tag + " = 0.0d")
					
					;; string
					7: a = EXECUTE("strct." + tag + " = ' '")

					ELSE: fire_siren, func_name + ": data type " + fire_string(type) + " has no default value!"					
				ENDCASE
			
			END
			
			
		ENDCASE
		
	ENDFOR

	RETURN, strct

END
