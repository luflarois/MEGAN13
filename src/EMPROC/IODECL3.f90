module iodecl3
!.........................................................................
! Version "@(#)$Header$"
!    EDSS/Models-3 I/O API.  Copyright (C) 1992-2002 MCNC
!    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
!    See file "LGPL.txt" for conditions of use.
!....................................................................
!  INCLUDE FILE  IODECL3.EXT
!
!
!  DO NOT EDIT !!
!
!       The EDSS/Models-3 I/O API depends in an essential manner
!       upon the contents of this INCLUDE file.  ANY CHANGES are
!       likely to result in very obscure, difficult-to-diagnose
!       bugs caused by an inconsistency between standard "libioapi.a"
!       object-libraries and whatever code is compiled with the
!       resulting modified INCLUDE-file.
!
!       By making any changes to this INCLUDE file, the user
!       explicitly agrees that in the case any assistance is
!       required of MCNC or of the I/O API author, Carlie J. Coats, Jr.
!       as a result of such changes, THE USER AND/OR HIS PROJECT OR
!       CONTRACT AGREES TO REIMBURSE MCNC AND/OR THE I/O API AUTHOR,
!       CARLIE J. COATS, JR., AT A RATE TRIPLE THE NORMAL CONTRACT
!       RATE FOR THE SERVICES REQUIRED.
!
!  CONTAINS:  declarations and usage comments for the Models-3 (M3)
!             Interprocess Communication Applications Programming
!             Interface (API)
!
!  DEPENDENT UPON:  consistency with the API itself.
!
!  RELATED FILES:  PARM3.EXT, FDESC3.EXT
!
!  REVISION HISTORY:
!       prototype 3/1992 by Carlie J. Coats, Jr., MCNC Environmental
!       Programs
!
!       Modified  2/2002 by CJC:  updated dates, license, compatibility
!       with both free and fixed Fortran 9x source forms
!
!....................................................................

LOGICAL :: check3  !  is JDATE:JTIME available for FNAME?
LOGICAL :: close3  !  close FNAME
LOGICAL :: desc3   !  Puts M3 file descriptions into FDESC3.EXT
LOGICAL :: filchk3 ! check file type and dimensions
INTEGER :: init3   !  Initializes M3 API and returns unit for log
LOGICAL :: shut3   !  Shuts down API
LOGICAL :: open3   !  opens an M3 file
LOGICAL :: read3   !  read M3 file for variable,layer,timestep
LOGICAL :: write3  !  write timestep to M3 file
LOGICAL :: xtract3 !  extract window from timestep in a M3 file
LOGICAL :: interp3 !  do time interpolation from a M3 file
LOGICAL :: ddtvar3 !  do time derivative from M3 file

LOGICAL :: interpx !  time interpolation from a window
!  extraction from an M3 gridded file
!!        LOGICAL      PINTERPB !  parallel time interpolation from an
!  M3 boundary file

LOGICAL :: inqatt3 !  inquire attributes in M3 file
LOGICAL :: rdatt3  !  read numeric attributes by name from M3 file
LOGICAL :: wratt3  !  add new numeric attributes "
LOGICAL :: rdattc  !  read CHAR attributes       "
LOGICAL :: wrattc  !  add new CHAR attributes    "

LOGICAL :: sync3   !  flushes file to disk, etc.

EXTERNAL        check3 , close3,  desc3  , filchk3, init3  ,    &
    shut3  , open3  , read3  , write3 , xtract3,    &
    interp3, ddtvar3, inqatt3, rdatt3 , wratt3 ,    &
    rdattc , wrattc,  sync3,   interpx ! , PINTERPB

!.......................................................................
!..................  API FUNCTION USAGE AND EXAMPLES  ..................
!.......
!.......   In the examples below, names (FILENAME, PROGNAME, VARNAME)
!.......   should be CHARACTER*16, STATUS and RDFLAG are LOGICAL, dates
!.......   are INTEGER, coding the Julian date as YYYYDDD, times are
!.......   INTEGER, coding the time as HHMMSS, and LOGDEV is the FORTRAN
!.......   INTEGER unit number for the program's log file; and layer,
!.......   row, and column specifications use INTEGER FORTRAN array
!.......   index conventions (in particular, they are based at 1, not
!.......   based at 0, as in C).
!.......   Parameter values for "ALL...", for grid and file type IDs,
!.......   and for API dimensioning values are given in PARMS3.EXT;
!.......   file descriptions are passed via commons BDESC3 and CDESC3
!.......   in file FDESC3.EXT.
!.......
!.......   CHECK3():  check whether timestep JDATE:JTIME is available
!.......   for variable VNAME in file FILENAME.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = CHECK3 ( FILENAME, VNAME, JDATE, JTIME )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (data-record not available in file FNAME)
!.......       END IF
!.......
!.......   CLOSE3():  check whether timestep JDATE:JTIME is available
!.......   for variable VNAME in file FILENAME.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = CLOSE3 ( FILENAME )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... could not flush file to disk successfully,
!.......           or else file not currently open.
!.......       END IF
!.......
!.......   DESC3():   return description of file FILENAME to the user
!.......   in commons BDESC3 and CDESC3, file FDESC3.EXT.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = DESC3 ( FILENAME )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (file not yet opened)
!.......       END IF
!.......       ...
!.......       (Now common FDESC3 (file FDESC3.EXT) contains the descriptive
!.......       information for this file.)
!.......
!.......   FILCHK3():   check whether file type and dimensions for file
!.......   FILENAME match the type and dimensions supplied by the user.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = FILCHK3 ( FILENAME, FTYPE, NCOLS, NROWS, NLAYS, NTHIK )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (file type and dimensions do not match
!.......                the supplied FTYPE, NCOLS, NROWS, NLAYS, NTHIK)
!.......       END IF
!.......       ...
!.......
!.......   INIT3():  set up the M3 API, open the program's log file, and
!.......   return the unit FORTRAN number for log file.  May be called
!.......   multiple times (in which case, it always returns the log-file's
!.......   unit number).  Note that block data INITBLK3.FOR must also be
!.......   linked in.
!.......   FORTRAN usage is:
!.......
!.......       LOGDEV = INIT3 ( )
!.......       IF ( LOGDEV .LT. 0 ) THEN
!.......           ... (can't proceed:  probably can't open the log.
!.......                Stop the program)
!.......       END IF
!.......
!.......   OPEN3():  open file FILENAME from program PROGNAME, with
!.......   requested read-write/old-new status.  For files opened for WRITE,
!.......   record program-name and other history info in their headers.
!.......   May be called multiple times for the same file (in which case,
!.......   it returns true unless the request is for READ-WRITE status
!.......   for a file already opened READ-ONLY).  Legal statuses are:
!.......   FSREAD3: "old read-only"
!.......   FSRDWR3: "old read-write"
!.......   FSNEW3:  "new (read-write)"
!.......   FSUNKN3: "unknown (read_write)"
!.......   FORTRAN usage is:
!.......
!.......       STATUS = OPEN3 ( FILENAME, FSTATUS, PROGNAME )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (process the error)
!.......       END IF
!.......
!.......   READ3():  read data from FILENAME for timestep JDATE:JTIME,
!.......   variable VNAME, layer LAY, into location  ARRAY.
!.......   If VNAME==ALLVARS3=='ALL         ', reads all variables;
!.......   if LAY==ALLAYS3==-1, reads all layers.
!.......   Offers random access to the data by filename, date&time, variable,
!.......   and layer.  For DICTIONARY files, logical name for file being
!.......   requested maps into the VNAME argument.  For time-independent
!.......   files (including DICTIONARY files), JDATE and JTIME are ignored.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = READ3 ( FILENAME, VNAME, LAY, JDATE, JTIME, ARRAY )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (read failed -- process this error.)
!.......       END IF
!.......
!.......   SHUT3():  Flushes and closes down all M3 files currently open.
!.......   Must be called before program termination; if it returns FALSE
!.......   the run must be considered suspect.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = SHUT3 ( )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (Flush of files to disk probably didn't work;
!.......                look at netCDF error messages)
!.......       END IF
!.......
!.......   WRITE3():  write data from ARRAY to file FILENAME for timestep
!.......   JDATE:JTIME.  For GRIDDED, BUONDARY, and CUSTOM files, VNAME
!.......   must be a variable found in the file, or else ALLVARS3=='ALL'
!.......   to write all variables from ARRAY.  For other file types,
!.......   VNAME _must_ be ALLVARS3.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = WRITE3 ( FILENAME, VNAME, JDATE, JTIME, ARRAY )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (write failed -- process this error.)
!.......       END IF
!.......
!.......   XTRACT3():  read/extract gridded data into location  ARRAY
!.......   from FILENAME for time step JDATE:JTIME, variable VNAME
!.......   and the data window defined by
!.......       LOLAY  <=  layer   <=  HILAY,
!.......       LOROW  <=  row     <=  HIROW,
!.......       LOCOL  <=  column  <=  HICOL
!.......   FORTRAN usage is:
!.......
!.......       STATUS = XTRACT3 ( FILENAME, VNAME,
!.......   &                      LOLAY, HILAY,
!.......   &                      LOROW, HIROW,
!.......   &                      LOCOL, HICOL,
!.......   &                      JDATE, JTIME, ARRAY )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (extract failed -- process this error.)
!.......       END IF
!.......
!.......   INTERP3():  read/interpolate gridded, boundary, or custom data
!.......   into location  ARRAY from FILENAME for time JDATE:JTIME, variable
!.......   VNAME, and all layers.  Note use of ASIZE = transaction size =
!.......   size of ARRAY, for error-checking.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = INTERPX ( FILENAME, VNAME, CALLER, JDATE, JTIME,
!.......   &                      ASIZE, ARRAY )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (interpolate failed -- process this error.)
!.......       END IF
!.......
!.......   INTERPX():  read/interpolate/window gridded, boundary, or custom
!.......   data into location  ARRAY from FILENAME for time JDATE:JTIME,
!.......   variable VNAME, and all layers.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = INTERPX ( FILENAME, VNAME, CALLER,
!.......   &                      COL0, COL1, ROW0, ROW1, LAY0, LAY1,
!.......   &                      JDATE, JTIME, ARRAY )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (windowed interpolate failed -- process this error.)
!.......       END IF
!.......
!.......   DDTVAR3():  read and calculate mean time derivative (per second)
!.......   for gridded, boundary, or custom data.  Put result into location
!.......   ARRAY from FILENAME for time JDATE:JTIME, variable VNAME, and all
!.......   layers.  Note use of ASIZE = transaction size = size of ARRAY,
!.......   for error-checking.  Note  d/dt( time-independent )==0.0
!.......   FORTRAN usage is:
!.......
!.......       STATUS = DDTVAR3 ( FILENAME, VNAME, JDATE, JTIME,
!.......   &                      ASIZE, ARRAY )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (operation failed -- process this error.)
!.......       END IF
!.......
!.......   INQATT():  inquire how many attributes there are for a
!.......   particular file and variable (or for the file globally,
!.......   if the variable-name ALLVAR3 is used)), and what the
!.......   names, types, and array-dimensions of these attributes are.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = INQATT3( FNAME, VNAME, MXATTS,
!.......   &                     NATTS, ANAMES, ATYPES, ASIZES )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (operation failed -- process this error.)
!.......       END IF
!.......
!.......   RDATT3():  Reads an INTEGER, REAL, or DOUBLE attribute by name
!.......   for a specified file and variable into a user-specified array.
!.......   If variable name is ALLVAR3, reads the file-global attribute.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = RDATT3( FNAME, VNAME, ANAME, ATYPE, AMAX,
!.......   &                    ASIZE, AVAL )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (operation failed -- process this error.)
!.......       END IF
!.......
!.......   WRATT3():  Writes an INTEGER, REAL, or DOUBLE attribute by name
!.......   for a specified file and variable.  If variable name is ALLVAR3,
!.......   reads the file-global attribute.
!.......
!.......       STATUS =  WRATT3( FNAME, VNAME,
!.......   &                     ANAME, ATYPE, AMAX, AVAL )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (operation failed -- process this error.)
!.......       END IF
!.......
!.......   RDATTC():  Reads a CHARACTER string attribute by name
!.......   for a specified file and variable into a user-specified array.
!.......   If variable name is ALLVAR3, reads the file-global attribute.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = RDATTC( FNAME, VNAME, ANAME, CVAL )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (operation failed -- process this error.)
!.......       END IF
!.......
!.......   WRATT3():  Writes a CHARACTER string attribute by name
!.......   for a specified file and variable.  If variable name is ALLVAR3,
!.......   reads the file-global attribute.
!.......
!.......       STATUS =  WRATTC( FNAME, VNAME, ANAME, CVAL )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (operation failed -- process this error.)
!.......       END IF
!.......
!.......   SYNC3():   Synchronize FILENAME with disk (flush output;
!.......   re-read header and invalidate data-buffers for input.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = SYNC3 ( FILENAME )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (file not yet opened, or disk-synch failed)
!.......       END IF
!.......       ...
!.......
!................   end   IODECL3.EXT   ....................................

end module iodecl3
