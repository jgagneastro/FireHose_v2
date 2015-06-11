;; Creates a fire structure and sets the defaults
FUNCTION FIRE_CREATE_STRUCT

	tmp = create_struct(name="firestrct")
	tmp = firestrct_set_defaults(tmp)

	RETURN, tmp

END
