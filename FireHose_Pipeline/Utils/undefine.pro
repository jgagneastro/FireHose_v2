;; Sets the value of the input variable to undefined (taken from http://www.dfanning.com/tips/variable_undefine.html)
PRO undefine, var
	var1 = size(TEMPORARY(var))
END
