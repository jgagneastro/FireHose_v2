;; Prints message to widget if both verbose and widget are passed.
pro fire_print_GUI, message, VERBOSE=verbose, $
                             WIDGET=widget, _EXTRA = keys

  if keyword_set(VERBOSE) AND keyword_set(WIDGET) then begin
     WIDGET_CONTROL, widget, set_value=message, _EXTRA = keys
  endif
  
  RETURN
  
end
