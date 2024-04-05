module parms3
!.........................................................................
! Version "@(#)$Header$"
!    EDSS/Models-3 I/O API.
!    Copyright (C) 1992-2003 MCNC and Carlie J. Coats, Jr, and
!    (C) 2003-2004 Baron Advanced Meteorological Systems LLC.
!    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
!    See file "LGPL.txt" for conditions of use.
!....................................................................
!  INCLUDE FILE  PARMS3.EXT
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
!  CONTAINS:
!       Fortran dimensioning parameters, standard file-type, grid-type, etc.
!       token values for Models-3 I/O System API
!
!  DEPENDENT UPON:
!       M3INT, M3REAL, M3DBLE consistent with NETCDF.EXT
!       Consistent with C include file "parms3.h"
!
!  REVISION HISTORY:
!       prototype 3/1992 by Carlie J. Coats, Jr., MCNC Environmental
!       Programs
!
!       Modified 12/1992 by CJC:  new map projection type STEGRD3.
!
!       Modified  6/1994 by CJC:  I/O API Revisions.
!
!       Modified 12/1996 by CJC:  support for new file types
!       Modified  2/2002 by CJC:  updated dates, license; compatibility
!       with both free and fixed Fortran 9x source forms
!
!       Modified 10/2003 by CJC for I/O AI version 3:  support for
!       native-binary BINFILE3 file type
!....................................................................

!...........   Dimensioning parameters:

INTEGER :: mxdlen3   !  description line length
INTEGER :: namlen3   !  name length (logical names, etc.)
INTEGER :: mxfile3   !  max number of open files
INTEGER :: mxvars3   !  max number of variables per file
INTEGER :: mxdesc3   !  max number of description lines
INTEGER :: mxlays3   !  max # of layers per file
INTEGER :: mxatts3   !  max # ATDSC3.EXT attributes per variable

!...........   Token-values ("magic numbers"):

INTEGER :: custom3   !  file type value "user-structured custom"
INTEGER :: grdded3   !  file type value "gridded"
INTEGER :: bndary3   !  file type value "boundary-condition"
INTEGER :: iddata3   !  file type value "ID-referenced data"
INTEGER :: profil3   !  file type value "rawind vertical profiles"
INTEGER :: grnest3   !  file type value "nested-grid"
INTEGER :: smatrx3   !  file type value "sparse matrix"
INTEGER :: dctnry3   !  file type value "dictionary"
INTEGER :: dgraph3   !  file type value "directed graph"
INTEGER :: kfevnt3   !  file type value "KF cloud event"
INTEGER :: tsries3   !  file type value "time series"
INTEGER :: ptrfly3   !  file type value "pointer-flyer"

INTEGER :: m3char    !  variable type value "CHARACTER-string"
INTEGER :: m3byte    !  variable type value "Byte"
INTEGER :: m3int     !  variable type value "INTEGER"
INTEGER :: m3real    !  variable type value "REAL"
INTEGER :: m3dble    !  variable type value "DOUBLE PRECISION"

INTEGER :: buffil3   !  "buffered-file" value for CDFID
INTEGER :: virfil3   !  "virtual-file"  value for CDFID
INTEGER :: lstfil3   !  "file-list"     value for CDFID
INTEGER :: binfil3   !  "native-binary" value for CDFID

INTEGER :: fsread3   !  OPEN3() flag:  "old read-only" file
INTEGER :: fsrdwr3   !  "old read-write" file
INTEGER :: fsnew3    !  "new (read-write)" file
INTEGER :: fsunkn3   !  "unknown (read_write)" file
INTEGER :: fscrea3   !  "unknown (write):  truncate if exists" file

INTEGER :: latgrd3   !  grid type value:  lat-lon coords
INTEGER :: lamgrd3   !  grid type value:  Lambert conformal conic (e.g., RAdm)
INTEGER :: mergrd3   !  grid type value:  (general) Mercator
INTEGER :: stegrd3   !  grid type value:  (tangent) stereographic
INTEGER :: utmgrd3   !  grid type value:  UTM (special case Merc.)
INTEGER :: polgrd3   !  grid type value:  polar stereographic
INTEGER :: eqmgrd3   !  grid type value: equatorial Mercator
INTEGER :: trmgrd3   !  grid type value: transverse Mercator
INTEGER :: albgrd3   !  grid type value: Albers conic Equal Area
INTEGER :: leqgrd3   !  grid type value: Lambert Azimuthal Equal Area

INTEGER :: vgsgph3   !  vert coord type 1:  hydrostatic sigma-P
INTEGER :: vgsgpn3   !  vert coord type 2:  non-h sigma-P
INTEGER :: vgsigz3   !  vert coord type 3:  sigma-Z
INTEGER :: vgpres3   !  vert coord type 4:  pressure (mb)
INTEGER :: vgzval3   !  vert coord type 5:  Z (m) (above sea lvl)
INTEGER :: vghval3   !  vert coord type 6:  H (m) (above ground)
INTEGER :: vgwrfem   !  vert coord type 7:  WRF mass-core sigma
INTEGER :: vgwrfnm   !  vert coord type 8:  WRF NMM

INTEGER :: allays3   !  Flag value: read all layers
CHARACTER (LEN=16) :: allvar3   !  Flag value: read all variables

REAL :: badval3   !  real flag value: "bad" or "missing"
REAL :: amiss3    !  BADVAL3 < AMISS3 on all machines
INTEGER :: okflag3   !  int flag value: "good" values
INTEGER :: imiss3    !  int flag value: "bad" or "missing"
CHARACTER (LEN=16) :: cmiss3    !  char flag value:  "missing"

INTEGER :: xstat0    !  Normal, successful completion
INTEGER :: xstat1    !  File I/O error
INTEGER :: xstat2    !  Execution error
INTEGER :: xstat3    !  Special  error

INTEGER :: little_endian
INTEGER :: big_endian
INTEGER :: pdp_endian


!.......   Dimensioning parameters:

PARAMETER    ( mxfile3 =  64 , mxvars3 = 120 ,                  &
    mxdesc3 =  60 , mxdlen3 =  80 ,                  &
    mxatts3 =  20 , mxlays3 = 100 ,                  & 
    namlen3 =  16 )

!.......   Token-value parameters:

PARAMETER    ( kfevnt3 =  -3 , dgraph3 =  -2 , custom3 =  -1 ,  &
    dctnry3 =   0 , grdded3 =   1 , bndary3 =   2 ,  &
    iddata3 =   3 , profil3 =   4 , grnest3 =   5 ,  &
    smatrx3 =   6 , tsries3 =   7 , ptrfly3 =   8 ,  &
    m3byte  =   1 , m3char  =   2 , m3int   =   4 ,  &
    m3real  =   5 , m3dble  =   6 ,                  &
    buffil3 =  -1 , virfil3 =  -2 , lstfil3 =  -3 ,  &
    binfil3 =  -4 , fsread3 =   1 , fsrdwr3 =   2 ,  &
    fsnew3  =   3 , fsunkn3 =   4 ,                  & 
    fscrea3 =   5 )

PARAMETER    ( latgrd3 =   1 , lamgrd3 =   2 ,                  &
    mergrd3 =   3 , stegrd3 =   4 ,                  &
    utmgrd3 =   5 , polgrd3 =   6 ,                  &
    eqmgrd3 =   7 , trmgrd3 =   8 ,                  &
    albgrd3 =   9 , leqgrd3 =  10 )

PARAMETER    ( vgsgph3 =   1 , vgsgpn3 =   2 ,                  &
    vgsigz3 =   3 , vgpres3 =   4 ,                  &
    vgzval3 =   5 , vghval3 =   6 ,                  &
    vgwrfem =   7 , vgwrfnm =   8 )

PARAMETER    ( allays3 =    -1 , allvar3 = 'ALL',               &
    okflag3 =  5461 ,                                &
    imiss3  = -9999 ,                                &
    amiss3  =  -9.000E36 ,                           &
    badval3 =  -9.999E36 ,                           &
    cmiss3  =  '????????????????' )

PARAMETER    ( xstat0 = 0 ,                                     &
    xstat1 = 1 ,                                     &
    xstat2 = 2 ,                                     & 
    xstat3 = 3 )

!.......   These match BSD-style "endian.h" values:

PARAMETER    ( little_endian = 1234,                            &
    big_endian    = 4321 ,                           & 
    pdp_endian    = 3412 )

!................   end   PARMS3.EXT   ....................................

end module parms3
