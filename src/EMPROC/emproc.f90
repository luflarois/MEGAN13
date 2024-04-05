PROGRAM emproc

!***********************************************************************
!   This program computes biogenic emission using input emission
!   capacity maps and MCIP output variables.
!   The emission capacity map (inpname) are gridded in netCDF-IOAPI format
!   with all the daily average ppfd and daily average temperature.
!
!   Note: The projection and input grids of the two files must be
!   identical.
!
!   Requirement:
!      Requires libnetcdf.a and libioapi.a to compile
!
!      setenv EFMAPS  <input netCDF-ioapi emission capacity map>
!      setenv PFTS16  <input netCDF-ioapi PFT data>
!      setenv LAIS46  <input netCDF-ioapi LAI data>
!      setenv MGNMET  <input netCDF-ioapi MEGAN met>
!      setenv EROUT <megan emission output>
!      setenv SDATE <start date>
!      setenv STIME <start time>
!      setenv RLENG <run length>
!
!      If SDATE,STIME and RLENG are not set, use values from MET input
!
!   CALL:
!      CHECKMEM
!      CKGDIOAPI2D
!      MODULE GAMMA_ETC
!         GAMMA_LAI
!         GAMMA_P
!         GAMMA_TLD
!         GAMMA_TLI
!         GAMMA_A
!         GAMMA_S
!
!   History:
!   Created by Jack Chen 11/04
!   Modified by Tan 11/21/06 for MEGAN v2.0
!   Modified by Xuemei Wang 11/04/2007 for MEGAN2.1
!   Modified by Julia Lee-Taylor 03/18/2008 for MEGAN2.1
!   Modified by Xuemei Wang 09/30/2008 for MEGAN2.1
!   Modified by Tan         07/28/2011 for MEGAN2.1
!
!***********************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     Scientific algorithm
!
!             Emission = [EF][GAMMA][RHO]
!           where [EF]    = emission factor (ug/m2h)
!                 [GAMMA] = emission activity factor (non-dimension)
!                 [RHO]   = production and loss within plant canopies
!                           (non-dimensional)
!                 Assumption: [RHO] = 1  (11/27/06) (See PDT_LOT_CP.EXT)
!
!             GAMMA  = [GAMMA_CE][GAMMA_age][GAMMA_SM]
!           where [GAMMA_CE]  = canopy correction factor
!                 [GAMMA_AGE] = leaf age correction factor
!                 [GAMMA_SM]  = soil moisture correction factor
!                 Assumption: [GAMMA_SM]  = 1  (11/27/06)

!             GAMMA_CE = [GAMMA_LAI][GAMMA_P][GAMMA_T]
!           where [GAMMA_LAI] = leaf area index factor
!                 [GAMMA_P]   = ppfd emission activity factor
!                 [GAMMA_T]   = temperature response factor
!
!             Emission = [EF][GAMMA_LAI][GAMMA_P][GAMMA_T][GAMMA_AGE]
!        Derivation:
!             Emission = [EF][GAMMA](1-LDF) + [EF][GAMMA][LDF][GAMMA_P]
!             Emission = [EF][GAMMA]{ (1-LDF) + [LDF][GAMMA_P] }
!             Emission = [EF][GAMMA]{ (1-LDF) + [LDF][GAMMA_P] }
!           where LDF = light dependent function (non-dimension)
!                               (See LD_FCT.EXT)
!
!        Final Equation
!             Emission = [EF][GAMMA_LAI][GAMMA_AGE]*
!    { (1-LDF)[GAMMA_TLI] + [LDF][GAMMA_P][GAMMA_TLD] }  !for MEGAN2.1
!         where GAMMA_TLI is light independent
!               GAMMA_TLD is light dependent
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

use gamma_etc                 ! Module containing gamma functions
USE PARMS3          !  I/O API parameters
USE IODECL3         !  I/O API function declarations
USE FDESC3          !  I/O API file description data structures
USE EACO            ! emissions factor type definitions
USE CONST_MEGAN
IMPLICIT NONE

!...  EXTERNAL FUNCTIONS and their descriptions:
INTEGER,EXTERNAL   ::  findc, envint
LOGICAL,EXTERNAL   ::  ckgdioapi2d, envyn
INTEGER :: promptffile, str2int
REAL :: str2real
INTEGER :: index1
EXTERNAL     promptffile, str2int
EXTERNAL     str2real
EXTERNAL     index1
LOGICAL :: dscgrid
EXTERNAL     dscgrid

!...  Program I/O files: From run script
! Program name
CHARACTER (LEN=16) ::progname = 'EMPROC'
! Netcdf file
CHARACTER (LEN=16) ::efmaps = 'EFMAPS'     ! Output EF file logical name
CHARACTER (LEN=16) ::pfts16 = 'PFTS16'     ! Output PFT file logical name
CHARACTER (LEN=16) ::lais46 = 'LAIS46'     ! Output LAI file logical name
! Met files
CHARACTER (LEN=16) ::mgnmet = 'MGNMET'     ! Met file logical name
! Output file
CHARACTER (LEN=16) ::mgners = 'MGNERS'     ! Output file logical name

!...  Parameters for file units
INTEGER :: logdev                      ! Logfile unit number

!...  External parameters
! From run script
INTEGER :: sdate          ! Start date YYYYDDD
INTEGER :: stime          ! Start time HHMMSS
INTEGER :: rleng          ! Run length HHMMSS

LOGICAL :: onln_dt        ! Daily average, temperature
LOGICAL :: onln_ds        ! Daily average, solar radiation

! I/O API file parameters
INTEGER ::jdate        ! Date YYYYDDD from inpname
INTEGER ::jtime        ! Time HHMMSS from inpname
INTEGER ::ncols        ! Number of columns
INTEGER ::nrows        ! Number of rows
INTEGER ::nlays        ! Number of vertical layers
INTEGER ::mxrec        ! Total number of timesteps
INTEGER ::tstep        ! Time step

!...  Internal parameters
! Internal parameters (status and buffer)
INTEGER :: ios                    ! i/o status
CHARACTER (LEN=256) :: mesg                   ! message buffer

! Parameter for output species
INTEGER,PARAMETER :: nemis = n_mgn_spc
! number of output emission variables
! number of MEGAN species

! Local variables and their descriptions:
CHARACTER (LEN=16) :: gdnam
CHARACTER (LEN=16) :: cname        ! Coord name

REAL :: garea           ! Area in one grid (metre^2)
INTEGER :: aveby           ! Divider for daily average

REAL :: ldf             ! Light dependent factor
REAL :: rho             ! Production and loss within canopy

REAL,allocatable :: er( :,: )      ! Output emission buffer
REAL,allocatable :: non_dimgarma (:,:,:)
REAL,allocatable :: lat( :,: )     ! Latitude of grid cell
REAL,allocatable :: long( :,: )    ! Longitude of grid cell

REAL,allocatable :: laip( :,: )    ! Previous monthly LAI
REAL,allocatable :: laic( :,: )    ! Current monthly LAI

REAL,allocatable :: temp( :,: )    ! Temperature (K)
REAL,allocatable :: ppfd( :,: )    ! Calculated PAR (umol/m2.s)

REAL,allocatable :: d_ppfd( :,: )  ! Daily PAR (umol/m2.s)
REAL,allocatable :: d_temp( :,: )  ! Daily temperature (K)
REAL,allocatable :: gam_lht( :,: ) ! LAI correction factor
REAL,allocatable :: gam_age( :,: ) ! leaf age correction factor
REAL,allocatable :: gam_smt( :,: ) ! Soil moisture correction factor
REAL,allocatable :: wind( :,: )
REAL,allocatable :: pres( :,: )
REAL,allocatable :: di( :,: )
REAL,allocatable :: qv( :,: )
REAL,allocatable :: precadj( :,: )

INTEGER :: i_pft
INTEGER :: laip_dy, laip_hr, laic_dy, laic_hr
INTEGER :: mxpft, mxlai

! Number of LAT, LONG, and PFT factor variables
REAL,allocatable :: pftf( :, :, : )   ! PFT factor array
REAL,allocatable :: gam_tld(:,:)
REAL,allocatable :: gam_tli(:,:)
REAL :: adjust_factor_ld,adjust_factor_li
REAL :: gamma_td,gamma_ti, totalpft

! loop indices
INTEGER ::  t, s, i, j , k, n     ! Counters
INTEGER ::  nmap            ! Index
INTEGER ::  idate           ! Looping
INTEGER ::  itime           ! Looping
! SOIL NOx
LOGICAL :: lsoil = .true.
INTEGER,allocatable :: sltyp( :,: )
REAL,allocatable    :: rstyp( :,: )
REAL,allocatable    :: soilm( :,: )
REAL,allocatable    :: soilt( :,: )
REAL,allocatable    :: cfno( :,: )
REAL,allocatable    :: cfnog( :,: )

!...  Constants
! Length of the time step  (days)
INTEGER,PARAMETER :: tstlen = 30

! parameter for unit conversion
REAL,PARAMETER :: ug2tonne = 1E-12  ! convert microgram to metric tonne
REAL,PARAMETER :: hr2sec = 3600     ! convert hr to second
REAL,PARAMETER :: ug2g = 1E-6       ! convert microgram to gram


!***********************************************************************

!--=====================================================================
!...  Begin program
!--=====================================================================

!-----------------------------------------------------------------------
!.....1) File set up and assign I/O parameters
!-----------------------------------------------------------------------
!...  Initialize log file unit
logdev = init3()
!  Now I/O API is set up, and LOGUNIT is the unit number
!  for the log file (or it 6 for st'd output).

!...  Check logical variables
mesg = 'Use daily average temperature'
onln_dt    = envyn ( 'ONLN_DT', mesg, .true., ios )

mesg = 'Use daily average PAR'
onln_ds    = envyn ( 'ONLN_DS', mesg, .true., ios )

!...  Get input parameters from run script
CALL envstr( 'GDNAM3D', mesg, 'ASACA36km', gdnam, ios )
IF( .NOT. dscgrid( gdnam, cname, gdtyp3d,  &
      p_alp3d, p_bet3d, p_gam3d, xcent3d, ycent3d,  &
      xorig3d, yorig3d, xcell3d, ycell3d, ncols3d, nrows3d, nthik3d ) ) THEN
  mesg = 'Could not get grid description.'
  CALL m3exit ( progname, 0, 0, mesg, 2 )
END IF

!...  Open files
WRITE(mesg,1030) 'Checking up files',0,0,0
CALL m3mesg( mesg )
IF ( .NOT. open3( pfts16, fsread3, progname ) ) THEN
  CALL nameval (pfts16, mesg)  ! get input file name and path
  mesg = 'Could not open file '//trim(mesg)
  CALL m3exit( progname, 0, 0, mesg, 2 )
END IF
! Check grid
IF ( .NOT. filchk3 ( pfts16, grdded3, ncols3d, nrows3d, 1, nthik3d))  THEN
  mesg = 'PFTS16 has differenet grid definition'
  CALL m3exit( progname, 0, 0, mesg, 2 )
END IF
IF ( .NOT. desc3( pfts16 ) ) THEN
  CALL nameval (pfts16, mesg)  ! get input file name and path
  mesg = 'Could not get description of '//trim(mesg)
  CALL m3exit( progname, 0, 0, mesg, 2 )
END IF
mxpft = mxrec3d


IF ( .NOT. open3( lais46, fsread3, progname ) ) THEN
  CALL nameval (lais46, mesg)  ! get input file name and path
  mesg = 'Could not open file '//trim(mesg)
  CALL m3exit( progname, 0, 0, mesg, 2 )
END IF
! Check grid
IF ( .NOT. filchk3 ( lais46, grdded3, ncols3d, nrows3d, 1, nthik3d))  THEN
  mesg = 'LAIS46 has differenet grid definition'
  CALL m3exit( progname, 0, 0, mesg, 2 )
END IF
IF ( .NOT. desc3( lais46 ) ) THEN
  CALL nameval (lais46, mesg)  ! get input file name and path
  mesg = 'Could not get description of '//trim(mesg)
  CALL m3exit( progname, 0, 0, mesg, 2 )
END IF
mxlai = mxrec3d

IF ( .NOT. open3( efmaps, fsread3, progname ) ) THEN
  CALL nameval (efmaps, mesg)  ! get input file name and path
  mesg = 'Could not open file '//trim(mesg)
  CALL m3exit( progname, 0, 0, mesg, 2 )
END IF
! Check grid
IF ( .NOT. filchk3 ( efmaps, grdded3, ncols3d, nrows3d, 1, nthik3d))  THEN
  mesg = 'EFMAPS has differenet grid definition'
  CALL m3exit( progname, 0, 0, mesg, 2 )
END IF
IF ( .NOT. desc3( efmaps ) ) THEN
  CALL nameval (efmaps, mesg)  ! get input file name and path
  mesg = 'Could not get description of '//trim(mesg)
  CALL m3exit( progname, 0, 0, mesg, 2 )
END IF

IF ( .NOT. open3( mgnmet, fsread3, progname ) ) THEN
  CALL nameval (mgnmet, mesg)  ! get input file name and path
  mesg = 'Could not open file '//trim(mesg)
  CALL m3exit( progname, 0, 0, mesg, 2 )
END IF
! Check grid
IF ( .NOT. filchk3 ( mgnmet, grdded3, ncols3d, nrows3d, 1, nthik3d))  THEN
  mesg = 'MGNMET has differenet grid definition'
  CALL m3exit( progname, 0, 0, mesg, 2 )
END IF
IF ( .NOT. desc3( mgnmet ) ) THEN
  CALL nameval (mgnmet, mesg)  ! get input file name and path
  mesg = 'Could not get description of '//trim(mesg)
  CALL m3exit( progname, 0, 0, mesg, 2 )
END IF


ncols = ncols3d
nrows = nrows3d
tstep = tstep3d
garea = xcell3d * ycell3d

!...  Get input parameters from run script
mesg = 'Model start date (YYYYDDD)'
sdate = envint( 'SDATE', mesg, jdate, ios )

mesg = 'Model start time (HHMMSS)'
stime = envint( 'STIME', mesg, jtime, ios )

mesg = 'Model run length (HHMMSS)'
rleng = envint( 'RLENG', mesg, mxrec*10000, ios )

!...  Check start date, start time, end date, end time in MGNMET
WRITE(mesg,1030) 'Checking up MGNMET',0,0,0
CALL m3mesg( mesg )
idate = sdate; itime = stime
IF ( .NOT. check3( mgnmet, 'TEMP2', idate, itime ) ) THEN
  mesg = 'Starting time not on met file'
  CALL m3exit( progname, 0, 0, mesg, 2 )
END IF
CALL nextime ( idate, itime, rleng-10000 )
IF ( .NOT. check3( mgnmet, 'TEMP2', idate, itime ) ) THEN
  mesg = 'Ending time not on met file'
  CALL m3exit( progname, 0, 0, mesg, 2 )
END IF
idate = sdate; itime = stime

!...  Set output parameters that are different from met file and open file
sdate3d = sdate                ! From run-script
stime3d = stime                ! From run-script
mxrec3d = rleng / 10000        ! From run-script
mxrec   = mxrec3d
nvars3d = nemis + 7

DO s = 1, nemis
  vname3d(s) = trim( mgn_spc( s ) )
  vdesc3d(s) = 'Environmental activity factor for '// trim( mgn_spc(s) )
  units3d(s) = 'Non-Dimension '
  vtype3d(s) = m3real
!         print*,'VNAME=', vname3d(s),VDESC3d(s),UNITS3d(s)
END DO

vname3d(nemis+1) = 'D_TEMP'
units3d(nemis+1) = 'K'
vtype3d(nemis+1) = m3real
vdesc3d(nemis+1) = 'Variable  '//'K'

vname3d(nemis+2) = 'D_PPFD'
units3d(nemis+2) = 'umol/m2.s'
vtype3d(nemis+2) = m3real
vdesc3d(nemis+2) = 'Variable  '//'umol/m2.s'

vname3d(nemis+3) = 'LAT'
units3d(nemis+3) = ' '
vtype3d(nemis+3) = m3real
vdesc3d(nemis+3) = ' '

vname3d(nemis+4) = 'LONG'
units3d(nemis+4) = ' '
vtype3d(nemis+4) = m3real
vdesc3d(nemis+4) = ' '

vname3d(nemis+5) = 'CFNO'
units3d(nemis+5) = ' '
vtype3d(nemis+5) = m3real
vdesc3d(nemis+5) = ' '

vname3d(nemis+6) = 'CFNOG'
units3d(nemis+6) = ' '
vtype3d(nemis+6) = m3real
vdesc3d(nemis+6) = ' '

vname3d(nemis+7) = 'SLTYP'
units3d(nemis+7) = ' '
vtype3d(nemis+7) = m3int
vdesc3d(nemis+7) = ' '

CALL nameval (efmaps, mesg)  ! get input file name and path
fdesc3d(3) = 'Input EFMAPS file: '//trim(mesg)

CALL nameval (mgnmet, mesg)  ! get input file name and path
fdesc3d(4) = 'Input MGNMET file: '//trim(mesg)


IF ( .NOT. open3( mgners, fscrea3, progname ) ) THEN
  CALL nameval (mgnmet, mesg)  ! get input file name and path
  mesg = 'Could not open file '//trim(mesg)
  CALL m3exit( progname, 0, 0, mesg, 2 )
END IF
!-----------------------------------------------------------------------
!.....2) Process emission rates
!-----------------------------------------------------------------------
!...  Allocate memory
allocate ( er    ( ncols, nrows ), stat = ios )
CALL checkmem    ( ios, 'ER',     progname )
allocate ( lat   ( ncols, nrows ), stat = ios )
CALL checkmem    ( ios, 'LAT',    progname )
allocate ( long  ( ncols, nrows ), stat = ios )
CALL checkmem    ( ios, 'LONG',   progname )
allocate ( laip  ( ncols, nrows ), stat = ios )
CALL checkmem    ( ios, 'LAIp',   progname )
allocate ( laic  ( ncols, nrows ), stat = ios )
CALL checkmem    ( ios, 'LAIc',   progname )
allocate ( d_ppfd( ncols, nrows ), stat = ios )
CALL checkmem    ( ios, 'D_PPFD', progname )
allocate ( d_temp( ncols, nrows ), stat = ios )
CALL checkmem    ( ios, 'D_TEMP', progname )
allocate ( ppfd  ( ncols, nrows ), stat = ios )
CALL checkmem    ( ios, 'PPFD',   progname )
allocate ( temp  ( ncols, nrows ), stat = ios )
CALL checkmem    ( ios, 'TEMP',   progname )
allocate ( wind  ( ncols, nrows ), stat = ios )
CALL checkmem    ( ios, 'WIND',   progname )
allocate ( pres  ( ncols, nrows ), stat = ios )
CALL checkmem    ( ios, 'PRES',   progname )
allocate ( di    ( ncols, nrows ), stat = ios )
CALL checkmem    ( ios, 'DI',     progname )
allocate ( soilt ( ncols, nrows ), stat = ios )
CALL checkmem    ( ios, 'SOILT',  progname )
allocate ( qv    ( ncols, nrows ), stat = ios )
CALL checkmem    ( ios, 'QV',     progname )
allocate ( sltyp ( ncols, nrows ), stat = ios )
CALL checkmem    ( ios, 'SLTYP',  progname )
allocate ( rstyp ( ncols, nrows ), stat = ios )
CALL checkmem    ( ios, 'RSTYP',  progname )
allocate ( soilm ( ncols, nrows ), stat = ios )
CALL checkmem    ( ios, 'SOILM',  progname )
allocate ( cfno  ( ncols, nrows ), stat = ios )
CALL checkmem    ( ios, 'CFNO',   progname )
allocate ( cfnog ( ncols, nrows ), stat = ios )
CALL checkmem    ( ios, 'CFNOG',  progname )

allocate ( gam_lht( ncols, nrows ), stat = ios )
CALL checkmem ( ios, 'GAM_LHT', progname )
allocate ( gam_age( ncols, nrows ), stat = ios )
CALL checkmem ( ios, 'GAM_AGE', progname )
allocate ( gam_smt( ncols, nrows ), stat = ios )
CALL checkmem ( ios, 'GAM_SMT', progname )
allocate ( gam_tld ( ncols, nrows ), stat = ios )
CALL checkmem ( ios, 'GAM_TLD', progname )
allocate ( gam_tli ( ncols, nrows ), stat = ios )
CALL checkmem ( ios, 'GAM_TLI', progname )

allocate ( precadj( ncols, nrows ), stat = ios )
CALL checkmem     ( ios, 'PRECADJ', progname )
allocate ( pftf( nrtyp, ncols, nrows ), stat = ios )
CALL checkmem ( ios, 'PFTF', progname )

allocate ( non_dimgarma (nemis, ncols, nrows ), stat = ios )
CALL checkmem ( ios, 'NON_DIMGARMA', progname )

!...  Read EFMAPS

IF ( .NOT. read3(efmaps,'LAT',1,1,1,lat)) THEN
  mesg = 'Error reading LAT'
  CALL m3exit(progname,idate,itime,mesg,2)
END IF

IF ( .NOT. read3(efmaps,'LONG',1,1,1,long)) THEN
  mesg = 'Error reading LONG'
  CALL m3exit(progname,idate,itime,mesg,2)
END IF

!...  Read PFTS16
DO n = 1, mxpft
  IF ( .NOT. read3(pfts16,'PFTS',1,0,(n-1)*10000,pftf(n,:,:))) THEN
    mesg = 'Error reading PFTS'
    CALL m3exit(progname,idate,itime,mesg,2)
  END IF
END DO

!...  Choosing temperature and PAR
IF ( onln_dt ) THEN
! Use  daily temperature
  aveby = MIN(24,mxrec)
! Start the loop over the time period
  idate = sdate
  itime = stime
  d_temp = 0.0
  DO t = 1, aveby
    IF ( .NOT. read3(mgnmet,'TEMP2',1,idate,itime,temp)) THEN
      mesg = 'Error reading TEMP2'
      CALL m3exit(progname,idate,itime,mesg,2)
    END IF
    d_temp = d_temp + temp
    CALL nextime( idate, itime, tstep )
  END DO
  d_temp = d_temp/aveby
ELSE
  IF ( .NOT. read3(efmaps,'D_TEMP',1,1,1,d_temp)) THEN
    mesg = 'Error reading D_TEMP'
    CALL m3exit(progname,idate,itime,mesg,2)
  END IF
END IF

IF ( onln_ds ) THEN
! Use daily PAR
  aveby = MIN(24,mxrec)
! Start the loop over the time period
  idate = sdate
  itime = stime
  d_ppfd = 0.0
  DO t = 1, aveby
    IF ( .NOT. read3(mgnmet,'PAR',1,idate,itime,ppfd)) THEN
      mesg = 'Error reading PAR'
      CALL m3exit(progname,idate,itime,mesg,2)
    END IF
!PPFD = PPFD * 4.766
    ppfd = ppfd * 4.5
    d_ppfd = d_ppfd + ppfd
    CALL nextime( idate, itime, tstep )
  END DO
  d_ppfd = d_ppfd/aveby
ELSE
  IF ( .NOT. read3(efmaps,'D_SRAD',1,1,1,d_ppfd)) THEN
    mesg = 'Error reading D_SRAD'
    CALL m3exit(progname,idate,itime,mesg,2)
  END IF
!       ppfd: srad - short wave from sun (W/m2)
!       assuming 4.766 (umol m-2 s-1) per (W m-2)
!       assume 1/2 of srad is in 400-700nm band
!D_PPFD = D_PPFD * 4.766 * 0.5
  d_ppfd = d_ppfd * 4.5 * 0.5
END IF


!...  Start the loop over the time period
idate = sdate
itime = stime
DO t = 1, mxrec
  WRITE(mesg,1030) 'Processing: ',t,idate,itime
  CALL m3mesg( mesg )
!...  Initialize hourly variables
  temp = 0.
  ppfd = 0.
  laip = 0.
  laic = 0.
  
  IF ( .NOT. read3(mgnmet,'TEMP2',  allays3,idate,itime,temp)) THEN
    mesg = 'Error reading temperature'
    CALL m3exit(progname,idate,itime,mesg,2)
  END IF
  
  IF ( .NOT. read3(mgnmet,'PAR',   allays3,idate,itime,ppfd)) THEN
    mesg = 'Error reading PAR'
    CALL m3exit(progname,idate,itime,mesg,2)
  END IF
!PPFD = PPFD * 4.766
  ppfd = ppfd * 4.5
  
  IF( .NOT. read3(mgnmet,'WINDSPD',allays3,idate,itime,wind)) THEN
    mesg = 'Error reading wind speed'
    CALL m3exit(progname,idate,itime,mesg,2)
  END IF
  
  IF ( .NOT. read3(mgnmet,'PRES',  allays3,idate,itime,pres)) THEN
    mesg = 'Error reading pressure'
    CALL m3exit(progname,idate,itime,mesg,2)
  END IF
  
  IF ( .NOT. read3(mgnmet,'QV',    allays3,idate,itime,qv)) THEN
    mesg = 'Error reading QV'
    CALL m3exit(progname,idate,itime,mesg,2)
  END IF
  
  IF ( .NOT. read3(mgnmet,'PREC_ADJ',allays3,idate,itime,precadj)) THEN
    mesg = 'Error reading precipitation adjustment'
    CALL m3exit(progname,idate,itime,mesg,2)
  END IF
  
! Find LAIp and LAIc from date
  CALL findlai(idate,mxlai,laip_dy,laip_hr,laic_dy,laic_hr)
  WRITE(mesg,1020) 'Found LAI current period for YYYYJJJ : ',  &
      idate,laic_dy,laic_hr
  CALL m3mesg( mesg )
  WRITE(mesg,1020) 'Found LAI previous period for YYYYJJJ : ',  &
      idate,laip_dy,laip_hr
  CALL m3mesg( mesg )
  IF ( .NOT. read3(lais46,'LAIS',allays3,laip_dy,laip_hr,laip)) THEN
    mesg = 'Error reading precipitation adjustment'
    CALL m3exit(progname,idate,itime,mesg,2)
  END IF
  IF ( .NOT. read3(lais46,'LAIS',allays3,laic_dy,laic_hr,laic)) THEN
    mesg = 'Error reading precipitation adjustment'
    CALL m3exit(progname,idate,itime,mesg,2)
  END IF
  
! Go over all the chemical species
  DO s = 1, nemis
! Initialize variables
    er = 0.
    gam_lht = 1.
    gam_age = 1.
    gam_smt = 1.
    gam_tld = 1.
    gam_tli = 1.
    cfno    = 1.
    cfnog   = 1.
    
    CALL gamma_lai (ncols, nrows, laic, gam_lht)
    
    CALL gamma_a (idate, itime, ncols, nrows,  vname3d(s),  &
        laip, laic, tstlen, d_temp, gam_age)
    
    CALL gamma_s (ncols, nrows, gam_smt)
    
    WRITE(mesg,1030) 'Entering CANOPY: ',s,idate,itime
    CALL m3mesg( mesg )
    DO i=1, ncols
      DO j=1,nrows
        adjust_factor_ld = 0.0
        adjust_factor_li = 0.0
        totalpft = 0.0
        DO i_pft = 1,nrtyp   !canopy types
          totalpft = totalpft + pftf(i_pft,i,j) * 0.01
        END DO   ! ENDDO I_PFT
        
        IF (totalpft > 0.0) THEN
          
          DO i_pft = 1,nrtyp   !canopy types
            IF (pftf(i_pft,i,j) /= 0.0) THEN
              CALL gamme_ce(idate,itime,lat(i,j),long(i,j),  &
                  temp(i,j),d_temp(i,j), d_temp(i,j),  &
                  ppfd(i,j),d_ppfd(i,j), d_ppfd(i,j), wind(i,j),qv(i,j),  &
                  i_pft,laic(i,j),pres(i,j),di(i,j),  &
                  nrcha,nrtyp,canopychar, vname3d(s), gamma_td,gamma_ti)
            END IF
            adjust_factor_ld = adjust_factor_ld +  &
                0.01*pftf(i_pft,i,j)*gamma_td
            adjust_factor_li = adjust_factor_li +  &
                0.01*pftf(i_pft,i,j)*gamma_ti
            
          END DO   ! ENDDO I_PFT
          gam_tld(i,j) = adjust_factor_ld/totalpft
          gam_tli(i,j) = adjust_factor_li/totalpft
          
        ELSE IF (totalpft == 0.0) THEN
          
          gam_tld(i,j) = 1.0
          gam_tli(i,j) = 1.0
          
        ELSE IF (totalpft < 0.0) THEN
          
          CALL m3err(progname,idate,itime, 'TotalPFT is less than 0.0',.true.)
          
        END IF
        
      END DO   ! ENDDO J
    END DO   ! ENDDO I
    WRITE(mesg,1030) 'Exited CANOPY: ',s,idate,itime
    CALL m3mesg( mesg )
    
    nmap = index1( vname3d(s),n_ldf_spc,ldf_spc)
    ldf  = ldf_fct(nmap)
    nmap = index1( vname3d(s),n_rho_spc,rho_spc)
    rho  = rho_fct(nmap)
    
!...  Calculate emission
    er(:,:) = gam_age * gam_smt * rho * ((1-ldf) * gam_tli * gam_lht +  &
        ldf * gam_tld)
    where(er(:,:) > 0.0)
    non_dimgarma (s,:,:) = er(:,:)
    elsewhere
    non_dimgarma (s,:,:) = 0.0
    endwhere
  END DO
  
!...  Estimate CFNO and CFNOG
  WRITE(mesg,1030) 'Estimating soil NOx adj: ',t,idate,itime
  CALL m3mesg( mesg )
  IF ( read3(mgnmet,'SOIM1', allays3,idate,itime,soilm) .AND.  &
        read3(mgnmet,'SOIT1', allays3,idate,itime,soilt) .AND.  &
        read3(mgnmet,'SLTYP', allays3,idate,itime,rstyp) ) THEN
    
    mesg = 'Using SOIL parameters in NOx adjustment'
    CALL m3mesg( mesg )
    lsoil = .true.
    sltyp = INT(rstyp)
  ELSE
    mesg = 'SOIL parameters are not available'
    CALL m3mesg( mesg )
    lsoil = .false.
  END IF
  CALL soilnox(idate,itime,ncols,nrows, temp,lsoil,sltyp, soilm, soilt,  &
      laic, lat, precadj,  &
      cfno, cfnog )
  WRITE(mesg,1030) 'Finished soil NOx adj: ',t,idate,itime
  CALL m3mesg( mesg )
  
!-----------------------------------------------------------------------
!.....3) Write out the calculated ER and met data
!-----------------------------------------------------------------------
!... Write emission to file
  WRITE(mesg,1030) 'Writing emission at ',t,idate,itime
  CALL m3mesg( mesg )
  DO s = 1,nemis
    IF (.NOT. write3(mgners, vname3d(s),idate,itime,  &
          non_dimgarma (s,:,:))) THEN
      CALL nameval (mgners, mesg)  ! get input file name and path
      mesg = 'Error writing to file: '//trim(mesg)
      CALL m3exit(progname,idate,itime,mesg,2)
    END IF
    
  END DO ! End loop for emission species (S)
  
! #1
  IF ( .NOT. write3(mgners,vname3d(nemis+1),idate,itime, d_temp(:,:))) THEN
    CALL nameval (mgners, mesg)  ! get input file name and path
    mesg = 'Error writing to file: '//trim(mesg)
    CALL m3exit(progname,idate,itime,mesg,2)
  END IF
  
! #2
  IF ( .NOT. write3(mgners,vname3d(nemis+2),idate,itime, d_ppfd(:,:))) THEN
    CALL nameval (mgners, mesg)  ! get input file name and path
    mesg = 'Error writing to file: '//trim(mesg)
    CALL m3exit(progname,idate,itime,mesg,2)
  END IF
  
! #3
  IF ( .NOT. write3(mgners,vname3d(nemis+3),idate,itime, lat(:,:))) THEN
    CALL nameval (mgners, mesg)  ! get input file name and path
    mesg = 'Error writing to file: '//trim(mesg)
    CALL m3exit(progname,idate,itime,mesg,2)
  END IF
  
! #4
  IF ( .NOT. write3(mgners,vname3d(nemis+4),idate,itime, long(:,:))) THEN
    CALL nameval (mgners, mesg)  ! get input file name and path
    mesg = 'Error writing to file: '//trim(mesg)
    CALL m3exit(progname,idate,itime,mesg,2)
  END IF
  
! #5
  IF ( .NOT. write3(mgners,vname3d(nemis+5),idate,itime, cfno(:,:))) THEN
    CALL nameval (mgners, mesg)  ! get input file name and path
    mesg = 'Error writing to file: '//trim(mesg)
    CALL m3exit(progname,idate,itime,mesg,2)
  END IF
  
! #6
  IF ( .NOT. write3(mgners,vname3d(nemis+6),idate,itime, cfnog(:,:))) THEN
    CALL nameval (mgners, mesg)  ! get input file name and path
    mesg = 'Error writing to file: '//trim(mesg)
    CALL m3exit(progname,idate,itime,mesg,2)
  END IF
  
! #7
  IF ( .NOT. write3(mgners,vname3d(nemis+7),idate,itime, sltyp(:,:))) THEN
    CALL nameval (mgners, mesg)  ! get input file name and path
    mesg = 'Error writing to file: '//trim(mesg)
    CALL m3exit(progname,idate,itime,mesg,2)
  END IF
  
  CALL nextime( idate, itime, tstep )
END DO ! End loop for time step (T)

!... Exit and close file
CALL m3exit(progname,0,0,' ',0)

deallocate ( er      )   ! output emission buffer
deallocate ( non_dimgarma)
deallocate ( lat     )   ! input latitude of grid cell
deallocate ( long    )   ! input longitude of grid cell

deallocate ( laip    )   ! previous monthly LAI
deallocate ( laic    )   ! current monthly LAI

deallocate ( temp    )   ! input hourly temperature (K)
deallocate ( ppfd    )   ! calculated PAR (umol/m2.s)

deallocate ( d_ppfd  )   ! daily PAR (umol/m2.s)
deallocate ( d_temp  )   ! input daily temperature (K)

deallocate ( gam_lht )   ! LAI correction factor
deallocate ( gam_age )   ! leaf age correction factor
deallocate ( gam_smt )   ! Soil moilture correction factor

deallocate ( wind    )
deallocate ( pres    )
deallocate ( di      )
deallocate ( sltyp   )
deallocate ( rstyp   )
deallocate ( soilm   )
deallocate ( soilt   )
deallocate ( qv      )
deallocate ( precadj )
deallocate ( pftf    )
deallocate ( gam_tld )
deallocate ( gam_tli )
deallocate ( cfno    )
deallocate ( cfnog   )
!--=====================================================================
!...  FORMAT
!--=====================================================================
1000  FORMAT( a )
1010  FORMAT( 43( a, :, i8, :, 1X ) )
1020  FORMAT (a40,i8,x,i8,x,i8)
1030  FORMAT (a20,i8,x,i8,x,i8)

!--=====================================================================
!...  End program
!--=====================================================================
END PROGRAM emproc
