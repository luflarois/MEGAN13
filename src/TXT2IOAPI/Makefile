PROG =	../../bin/txt2ioapi

SRCS =	checkmem.f90 cnvt_efs.f90 cnvt_lai.f90 cnvt_pft.f90 FDESC3.f90 \
	IODECL3.f90 LAI_MGN.f90 PARMS3.f90 parsline.f90 PFT_MGN.f90 \
	SPC_MGN.f90 txt2ioapi.f90

OBJS =	checkmem.o cnvt_efs.o cnvt_lai.o cnvt_pft.o FDESC3.o IODECL3.o \
	LAI_MGN.o PARMS3.o parsline.o PFT_MGN.o SPC_MGN.o txt2ioapi.o

LIBS =	-L/stornext/home/luiz.flavio/web31/tupan_pgi -lioapi

CC = ftn
CFLAGS = -O
FC = ftn
FFLAGS = -O3
F90 = ftn
F90FLAGS = -O3
LDFLAGS = -s

all: $(PROG)

$(PROG): $(OBJS)
	$(F90) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

clean:
	rm -f $(PROG) $(OBJS) *.mod

.SUFFIXES: $(SUFFIXES) .f90

.f90.o:
	$(F90) $(F90FLAGS) -c $<

cnvt_efs.o: FDESC3.o IODECL3.o PARMS3.o SPC_MGN.o
cnvt_lai.o: FDESC3.o IODECL3.o LAI_MGN.o PARMS3.o
cnvt_pft.o: FDESC3.o IODECL3.o PARMS3.o PFT_MGN.o
FDESC3.o: PARMS3.o
txt2ioapi.o: FDESC3.o IODECL3.o LAI_MGN.o PARMS3.o PFT_MGN.o SPC_MGN.o
