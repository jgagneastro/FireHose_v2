###############################################################################
# Sloan Digital Sky Survey III (SDSS-III)
# Code for product: idlutils
#
# This Makefile & all Makefiles in this product are GNU make compliant.
# Please help keep them that way.  See
# http://www.gnu.org/software/make/manual/make.html
#
# $Id$
#
###############################################################################
#
# This line helps prevent make from getting confused in the case where you
# have a file named 'clean'.
#
.PHONY : clean
#
# This should compile all code prior to it being installed.
#
all : www/idlutils_doc.html www/goddard_doc.html

#
# Replace mk_doc
#
www/idlutils_doc.html :
	idl -e 'make_html_help, getenv("IDLUTILS_DIR")+"/pro/*/", "www/idlutils_doc.html", /strict, title="IDL Help for IDLUTILS", /verbose, version="idlutils"'

www/goddard_doc.html :
	idl -e 'make_html_help, getenv("IDLUTILS_DIR")+"/goddard/pro/*/", "www/goddard_doc.html", /strict, title="IDL Help for Goddard Utilities", /verbose, version="idlutils"'
#
# GNU make pre-defines $(RM).  The - in front of $(RM) causes make to
# ignore any errors produced by $(RM).
#
clean :
	- $(RM) www/idlutils_doc.html www/goddard_doc.html

