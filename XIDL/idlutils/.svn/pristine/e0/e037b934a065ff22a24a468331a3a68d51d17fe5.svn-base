###############################################################################
# Sloan Digital Sky Survey III (SDSS-III)
# Code for product: idlutils
#
# Routines for interfacing idl with mangle
#
# M.R. Blanton
#
# This Makefile & all Makefiles in this product are GNU make compliant.
# Please help keep them that way.  See
# http://www.gnu.org/software/make/manual/make.html
#
# $Id$
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
MANGLE = $(IDLUTILS_DIR)/src/mangle1.4/src
CFLAGS = $(SDSS_CFLAGS) -DCHECK_LEAKS -I$(INC) -I$(MANGLE)
CCCHK = -Wall
#
# How to make objects from c files
#
%.o : %.c
	$(CC) -c $(CCCHK) $(CFLAGS) $(X_CFLAGS) $(IDL_FLAG) $<
#
# Objects to compile
#
OBJECTS = \
	idl_garea.o \
	idl_where_polygons_overlap.o \
	idl_vmid.o \
	idl_vmidc.o \
	idl_gverts.o

all : $(LIB)/libidlmangle.$(SO_EXT)

$(LIB)/libidlmangle.$(SO_EXT): $(OBJECTS) $(LIB)/libmangle14.a $(MANGLE)/polygon.h
	$(LD) $(X_LD_FLAGS) -o $(LIB)/libidlmangle.$(SO_EXT) $(OBJECTS) \
		-L$(LIB) -lmangle14

#
# GNU make pre-defines $(RM).  The - in front of $(RM) causes make to
# ignore any errors produced by $(RM).
#
clean :
	- $(RM) *~ core *.o $(LIB)/libidlmangle.$(SO_EXT)
