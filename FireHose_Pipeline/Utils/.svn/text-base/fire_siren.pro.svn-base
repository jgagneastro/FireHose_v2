PRO FIRE_SIREN, warning_message, QUIET=quiet, WIDGET=widget, BOTH=both, _EXTRA = keys
;; This prints a warning message to output in a very conspicuous manner.
;; If the keyword /QUIET is passed, then it only prints the message.
;; If the keyword WIDGET is set, then the output is printed to that widget ID
;;		using WIDGET_CONTROL (notice the keyword inheritance _EXTRA: any keywords
;;		for WIDGET_CONTROL may also be passed to this function).
;; If the keyword /BOTH is passed in addition to widget, then the message is
;;		also printed to the terminal

	;;nl = fire_newline() 

	if NOT KEYWORD_SET(quiet) then begin
		stars = '***********************************************'
		;;message = nl + stars + nl + warning_message + nl + stars + nl
		message = [ "", stars, warning_message, stars, "" ]
	endif else begin
		message = warning_message
	endelse

	if NOT keyword_set(WIDGET) OR keyword_set(BOTH) then begin
		print, message
	endif
	
	if keyword_set(WIDGET) then begin
		WIDGET_CONTROL, widget, set_value=message, _EXTRA = keys
	endif

	RETURN
	
END
