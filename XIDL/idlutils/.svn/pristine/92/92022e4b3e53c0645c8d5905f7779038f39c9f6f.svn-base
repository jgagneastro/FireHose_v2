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
# By default, make compiles C code with the rule
#
#%.o : %.c
#	$(CC) -c $(CPPFLAGS) $(CFLAGS) $<
#
# You can override these variables here or even override the rule itself,
# but be sure you know what you are doing, & also export the overrides
# to sub-makes appropriately.  For example
#
# export CFLAGS = -W -Wall -pedantic -O2 -DCHECK_LEAKS -I../include
#
# For more details:
# http://www.gnu.org/software/make/manual/make.html#Implicit-Rules
#
# This is a list of subdirectories that make should descend into.  Makefiles
# in these subdirectories should also understand 'make all' & 'make clean'.
# This list can be empty, but should still be defined.
#
# You may also have source code in this directory, but be sure to modify
# the all & clean targets appropriately.
#
# Note that mangle2.2/src must be built before idlmangle!
#
SUBDIRS = image mangle2.2/src mangle1.4/src idlmangle math spheregroup trace dimage
#
# This line helps prevent make from getting confused in the case where you
# have a file named 'clean'.
#
.PHONY : clean
#
# This should compile all code prior to it being installed.
#
all :
	@ for f in $(SUBDIRS); do $(MAKE) -C $$f all ; done
#
# GNU make pre-defines $(RM).  The - in front of $(RM) causes make to
# ignore any errors produced by $(RM).
#
clean :
	- $(RM) *~ core
	@ for f in $(SUBDIRS); do $(MAKE) -C $$f clean ; done
