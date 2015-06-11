###############################################################################
# Sloan Digital Sky Survey III (SDSS-III)
# Code for product: idlutils
#
# trace code
#
# S. Burles & D. Schlegel
#
# This Makefile & all Makefiles in this product are GNU make compliant.
# Please help keep them that way.  See
# http://www.gnu.org/software/make/manual/make.html
#
# $Id$
#
# Dec 2012 - B. Cherinka - added ml_trace_crude and ml_trace_fweight to Makefile
#
###############################################################################
#
# Pass these flags to the C compiler
#
# SDSS_CFLAGS X_CFLAGS X_LD_FLAGS SO_EXT are defined by evilmake
# CCCHK is NOT defined by evilmake but is defined by sdssmake
# CCCHK = -Wall is the standard for Linux
#
INC = $(IDLUTILS_DIR)/include
LIB = $(IDLUTILS_DIR)/lib
CFLAGS = $(SDSS_CFLAGS) -DCHECK_LEAKS -I$(INC)
CCCHK = -Wall
#
# How to make objects from c files
#
%.o : %.c
	$(CC) -c $(CCCHK) $(CFLAGS) $(X_CFLAGS) $<
#
# Objects to compile
#
OBJECTS = \
	evilmath.o \
	trace_crude.o \
	ml_trace_crude.o \
	trace_fweight.o \
	ml_trace_fweight.o \
	trace_gweight.o

#
# SDSS-III Makefiles should always define this target.
#
all : $(LIB)/libtrace.$(SO_EXT)

$(LIB)/libtrace.$(SO_EXT): $(OBJECTS)
	$(LD) $(X_LD_FLAGS) -o $(LIB)/libtrace.$(SO_EXT) $(OBJECTS)
#	nm -s $(LIB)/libtrace.$(SO_EXT)

#
# GNU make pre-defines $(RM).  The - in front of $(RM) causes make to
# ignore any errors produced by $(RM).
#
clean :
	- $(RM) *~ core *.o $(LIB)/libtrace.$(SO_EXT)
