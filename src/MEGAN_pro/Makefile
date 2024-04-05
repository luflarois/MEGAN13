.SUFFIXES: .o .f90

F90    = ftn
LIBS   = #
INCLUDE_MODULES = #-I$(NETCDF_DIR)/include
FFLAGS = -g

OBJS_BIO_EMISS = misc_definitions_module.o\
       constants_module.o\
       bio_types.o\
       area_mapper.o\
       bio_emiss.o

OBJS_SURF_XFORM = surfdata_xform.o

OBJS_MEGAN_XFORM = megan_xform.o

.f90.o:
		${F90} ${FFLAGS} -c ${INCLUDE_MODULES} $<

##dependencies

megan_bio_emiss: ${OBJS_BIO_EMISS}
		 ${F90} -o $@ ${FFLAGS} ${OBJS_BIO_EMISS} ${LIBS} 

surfdata_xform: ${OBJS_SURF_XFORM}
		${F90} -o $@ ${FFLAGS} ${OBJS_SURF_XFORM} ${LIBS} 

megan_xform:    ${OBJS_MEGAN_XFORM}
		${F90} -o $@ ${FFLAGS} ${OBJS_MEGAN_XFORM} ${LIBS} 

cleanup:
		rm -f *.o *.mod

clean:
		rm -f core *.o *.mod
