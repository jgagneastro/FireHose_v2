FUNCTION fire_get_tells, firestrct, SPOTS=spots, NSPOTS=nspots

	func_name = "fire_get_tells"

	RETURN, fire_get_scis(firestrct, /TELLS, SPOTS=spots, NSPOTS=nspots, FUNC_NAME=func_name)
	
END
