PROGRAM mgn2mech
 
!***********************************************************************
!   This program does chemical speciation and MECHANISM conversion.
!   The output from megan.f is converted from 20 to 150 species which
!   are then lumped according to the MECHANISM assigned in the run script.
!   The program loops through all timesteps of the input file.
!
!   Procedure
!   1) File set up and assign I/O parameters
!   2) Conversion from MGN 20 to speciated 150
!   3) Conversion from speciated species to MECHANISM species
!   4) Convert to tonne/hour if needed
!
!   The input file gives variables in units of g-species/sec.
!   All outputs are in mole/sec or tonne/hr depending on assignment.
!
!
!   INPUT:
!           1) MEGAN output (netCDF-ioapi)
!
!   OUTPUT:
!           1) MEGAN speciation or MECHANISM species (netCDF-ioapi)
!
!   Requirement:
!      Requires libnetcdf.a and libioapi.a to compile
!
!      setenv MGERFILE    <DEFANGED_input MEGAN output for emission activity factors>
!      setenv OUTPFILE    <output speciated emission>
!
!   CALLS:  CHECKMEM
!
!   Originally created by Jack Chen 11/04 for MEGAN v.0
!   For MEGAN v2.0 created by Tan 12/01/06
!   For MEGAN v2.1 created by Xuemei Wang 11/04/07
!   For MEGAN v2.1 to use 150 species created by Xuemei Wang 09/30/09
!
!  History:
!  08/14/07 Tan    - Move to MEGANv2.02 with no update
!  08/29/07 modified by A. Guenther to correct error in assigning
!           emission factor. This version is called MEGANv2.03
!  10/29/07 modified by A. Guenther to correct omission of diurnal variation
!           factor. This version is called MEGANv2.04
!  11/04/07 modified by Xuemei Wang to give two options for MAP or lookup table for
!           the emission factors. Also gives options for different chemical MECHANISMs
!           in the code: user modifies the external script to assign MECHANISM.
!           This version is called MEGANv2.1.0
!  06/04/08 modified by J. Lee-Taylor to accept vegetation-dependent speciation factors
!           in table format (RESHAPE tables) rather than from DATA statements.
!  09/30/08  modified by Xuemei Wang to give options for input file and test different mechanisms
!  09/27/11  Tan&Xuemei MEGANv2.10 includes soil NOx adjustment and a lot of updates
!***********************************************************************
USE PARMS3    !  I/O API parameters
USE IODECL3   !  I/O API function declarations
USE FDESC3    !  I/O API file description data structures.

!Modules of Mechanism and conversion Map
USE EFS_PFT
USE MAP_CV2MECHANISM !  Conversion map and factors for different Mechanism
USE SPC_NOCONVER
USE PFT_MGN
USE SPC_SAPRC99
USE MAP_CV2SAPRC99
USE SPC_SAPRC99X
USE MAP_CV2SAPRC99X
USE SPC_SAPRC99Q
USE MAP_CV2SAPRC99Q
USE SPC_CB05
USE MAP_CV2CB05
USE SPC_CB6
USE MAP_CV2CB6
USE SPC_SOAX
USE MAP_CV2SOAX

IMPLICIT NONE

!...........   EXTERNAL FUNCTIONS and their descriptions:
INTEGER :: promptffile, str2int
REAL :: str2real
INTEGER :: index1
EXTERNAL     promptffile,str2int
EXTERNAL     str2real
EXTERNAL     index1
LOGICAL :: envyn
EXTERNAL     envyn
LOGICAL :: dscgrid
EXTERNAL     dscgrid


!...  Program I/O files
! Program name
CHARACTER (LEN=16) :: progname = 'MGN2MECH'
! Input MEGAN ER file
CHARACTER (LEN=16) :: mgners   = 'MGNERS'    ! Input MEGAN ER file logical name
! Netcdf file
CHARACTER (LEN=16) :: efmaps   = 'EFMAPS'    !  EFMAP input file  name
CHARACTER (LEN=16) :: pfts16   = 'PFTS16'    ! Input PFT file logical
! Output file
CHARACTER (LEN=16) :: mgnout   = 'MGNOUT'    ! Output file logical name
! Parameters for file units
INTEGER :: logdev                      ! Logfile unit number

!...  Program I/O parameters
!...  External parameters

! from run script
LOGICAL :: tonphr      = .false.  ! Output in tons/hr flag
LOGICAL :: conversion  = .false.  ! Mechanism conversion flag
!LOGICAL :: NONPFT_EF   = .FALSE.

CHARACTER (LEN=16) :: mechanism              ! Mechanism name

! I/O API file parameters
INTEGER :: jdate          ! Looping date YYYYDDD
INTEGER :: jtime          ! Looping time HHMMSS
INTEGER :: ncols          ! Number of columns
INTEGER :: nrows          ! Number of rows
INTEGER :: mxrec          ! Total number of time steps
INTEGER :: sdate          ! Start date YYYYDDD
INTEGER :: stime          ! Start time HHMMSS
INTEGER :: tstep          ! Time step


!...  Internal parameters
! internal paramters (status and buffer)
INTEGER :: ios                          ! i/o status
CHARACTER (LEN=256) :: mesg                  ! Message buffer

INTEGER :: cid, inx, iny               ! Input grid x and y
INTEGER :: mxpft

! local variables and their descriptions:
CHARACTER (LEN=16) :: gdnam
CHARACTER (LEN=16) :: cname        ! Coord name

INTEGER :: t, s, i, n, c, r             ! Counters
INTEGER :: nmpmg, nmpsp, nmpmc          ! Counters
REAL,allocatable :: lat( :,: )         ! Latitude of grid cell
REAL,allocatable :: inper(:,:,:)       ! Input emission buffer
REAL,allocatable :: tmper(:,:,:)       ! Temp emission buffer
REAL,allocatable :: outer(:,:,:)       ! Output emission buffer
REAL,allocatable :: ef(:,:,:)          ! Emission factor
REAL,allocatable :: tmp1(:,:),tmp2(:,:),tmp3(:,:)
REAL :: tmo1,tmo2,tmo3
REAL,allocatable :: cfnog(:,:), cfno(:,:)
REAL,allocatable :: pft(:,:,:)         ! PFT factor array
INTEGER :: n_scon_spc
INTEGER :: nvar
INTEGER :: gday, glen, ino, iisop
REAL :: garea , g2cratio
INTEGER,allocatable :: garr(:,:)

INTEGER,allocatable ::  spmh_map(:),mech_map(:)   ! speciated species name
CHARACTER (LEN=16),allocatable :: mech_spc(:)
REAL,allocatable :: conv_fac(:)
REAL,allocatable :: mech_mwt(:)

REAL,PARAMETER :: ug2g = 1E-6          ! convert microgram to metric gram
REAL,PARAMETER :: g2tonne = 1E-6       ! convert microgram to metric ton
REAL,PARAMETER :: hr2sec = 3600        ! convert hr to second
REAL,PARAMETER :: n2no   = 2.142857    ! convert hr to second


!***********************************************************************

!=======================================================================
!...  Begin program
!=======================================================================


!-----------------------------------------------------------------------
!.....1) File set up and assign I/O parameters
!-----------------------------------------------------------------------
!...  Initialize log file unit
logdev = init3()

!...  Get input parameters from run script
CALL envstr( 'GDNAM3D', mesg, 'ASACA36km', gdnam, ios )
IF( .NOT. dscgrid( gdnam, cname, gdtyp3d,  &
      p_alp3d, p_bet3d, p_gam3d, xcent3d, ycent3d,  &
      xorig3d, yorig3d, xcell3d, ycell3d, ncols3d, nrows3d, nthik3d ) ) THEN
  mesg = 'Could not get grid description.'
  CALL m3exit ( progname, 0, 0, mesg, 2 )
END IF

!...  Open files
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

IF ( .NOT. open3( mgners, fsread3, progname ) ) THEN
  mesg = 'Could not open input file '//trim(mgners)
  CALL m3exit( progname, 0, 0, mesg, 2 )
END IF
! Check grid
IF ( .NOT. filchk3 ( mgners, grdded3, ncols3d, nrows3d, 1, nthik3d))  THEN
  mesg = 'MGNERS has differenet grid definition'
  CALL m3exit( progname, 0, 0, mesg, 2 )
END IF
IF ( .NOT. desc3( mgners ) ) THEN
  mesg = 'Could not get description of '//trim(mgners)
  CALL m3exit( progname, 0, 0, mesg, 2 )
END IF

sdate = sdate3d
stime = stime3d
tstep = tstep3d
mxrec = mxrec3d
ncols = ncols3d
nrows = nrows3d
garea = xcell3d * ycell3d
jdate = sdate
jtime = stime
PRINT *,' Area per grid cell (m2): ',garea

!...  Set output MECHANISM variable variables
! Check to run MECHANISM conversion
mesg = 'Convert to any ATM MECHANISM'
conversion = envyn ( 'RUN_CONVERSION', mesg, .false., ios )

! Check tonne per hour output
mesg = 'Output in Tonne per hour'
tonphr = envyn ( 'SPCTONHR', mesg, .false., ios )
!MESG = 'Emission factor with based on PFT or not'
!NONPFT_EF = ENVYN ( 'SPC_EF', MESG, .FALSE., IOS )


! Set attribute and variables for output
IF ( conversion ) THEN
  mesg = 'Mechanism Name'
  CALL envstr( 'MECHANISM', mesg, 'SAPRCII', mechanism, ios )
  
  select case ( trim(mechanism) )
  case ('SAPRCII')
  n_scon_spc = n_saprcii
  nvars3d = n_saprcii_spc
  case ('SAPRC99')
  n_scon_spc = n_saprc99
  nvars3d = n_saprc99_spc
  case ('RADM2')
  n_scon_spc = n_radm2
  nvars3d = n_radm2_spc
  case ('RACM')
  n_scon_spc = n_racm
  nvars3d = n_racm_spc
  case ('CBMZ')
  n_scon_spc = n_cbmz
  nvars3d = n_cbmz_spc
  case ('SAPRC99X')
  n_scon_spc = n_saprc99_x
  nvars3d = n_saprc99_x_spc
  case ('SAPRC99Q')
  n_scon_spc = n_saprc99_q
  nvars3d = n_saprc99_q_spc
  case ('CB05')
  n_scon_spc = n_cb05
  nvars3d = n_cb05_spc
  case ('CB6')
  n_scon_spc = n_cb6
  nvars3d = n_cb6_spc
  case ('SOAX')
  n_scon_spc = n_soax
  nvars3d = n_soax_spc
  case default
  mesg = 'Error: Mechanism conversion, invalid MECHANISM: ' //trim(mechanism)
  CALL m3exit(progname, 0, 0,mesg,2)
  endselect
  
  allocate ( spmh_map(n_scon_spc), stat = ios )
  CALL checkmem ( ios, 'spmh_map', progname )
  allocate ( mech_map(n_scon_spc), stat = ios )
  CALL checkmem ( ios, 'mech_map', progname )
  allocate ( conv_fac(n_scon_spc), stat = ios )
  CALL checkmem ( ios, 'conv_fac', progname )
  allocate ( mech_spc(nvars3d ), stat = ios )
  CALL checkmem ( ios, 'mech_spc', progname )
  allocate ( mech_mwt(nvars3d ), stat = ios )
  CALL checkmem ( ios, 'mech_mwt', progname )
  
  select case ( trim(mechanism) )
  
  case ('SAPRCII')
  spmh_map(1:n_scon_spc) = spmh_map_saprcii(1:n_scon_spc)
  mech_map(1:n_scon_spc) = mech_map_saprcii(1:n_scon_spc)
  conv_fac(1:n_scon_spc) = conv_fac_saprcii(1:n_scon_spc)
  mech_spc(1:nvars3d)    = mech_spc_saprcii(1:nvars3d)
  mech_mwt(1:nvars3d)    = mech_mwt_saprcii(1:nvars3d)
  vname3d(1:nvars3d)     = mech_spc(1:nvars3d)
  case ('SAPRC99')
  spmh_map(1:n_scon_spc) = spmh_map_saprc99(1:n_scon_spc)
  mech_map(1:n_scon_spc) = mech_map_saprc99(1:n_scon_spc)
  conv_fac(1:n_scon_spc) = conv_fac_saprc99(1:n_scon_spc)
  mech_spc(1:nvars3d)    = mech_spc_saprc99(1:nvars3d)
  mech_mwt(1:nvars3d)    = mech_mwt_saprc99(1:nvars3d)
  vname3d(1:nvars3d)     = mech_spc(1:nvars3d)
  case ('RADM2')
  spmh_map(1:n_scon_spc) = spmh_map_radm2(1:n_scon_spc)
  mech_map(1:n_scon_spc) = mech_map_radm2(1:n_scon_spc)
  conv_fac(1:n_scon_spc) = conv_fac_radm2(1:n_scon_spc)
  mech_spc(1:nvars3d)    = mech_spc_radm2(1:nvars3d)
  mech_mwt(1:nvars3d)    = mech_mwt_radm2(1:nvars3d)
  vname3d(1:nvars3d)     = mech_spc(1:nvars3d)
  case ('RACM')
  spmh_map(1:n_scon_spc) = spmh_map_racm(1:n_scon_spc)
  mech_map(1:n_scon_spc) = mech_map_racm(1:n_scon_spc)
  conv_fac(1:n_scon_spc) = conv_fac_racm(1:n_scon_spc)
  mech_spc(1:nvars3d)    = mech_spc_racm(1:nvars3d)
  mech_mwt(1:nvars3d)    = mech_mwt_racm(1:nvars3d)
  vname3d(1:nvars3d)     = mech_spc(1:nvars3d)
  case ('CBMZ')
  spmh_map(1:n_scon_spc) = spmh_map_cbmz(1:n_scon_spc)
  mech_map(1:n_scon_spc) = mech_map_cbmz(1:n_scon_spc)
  conv_fac(1:n_scon_spc) = conv_fac_cbmz(1:n_scon_spc)
  mech_spc(1:nvars3d)    = mech_spc_cbmz(1:nvars3d)
  mech_mwt(1:nvars3d)    = mech_mwt_cbmz(1:nvars3d)
  vname3d(1:nvars3d)     = mech_spc(1:nvars3d)
  case ('SAPRC99X')
  spmh_map(1:n_scon_spc) = spmh_map_saprc99_x(1:n_scon_spc)
  mech_map(1:n_scon_spc) = mech_map_saprc99_x(1:n_scon_spc)
  conv_fac(1:n_scon_spc) = conv_fac_saprc99_x(1:n_scon_spc)
  mech_spc(1:nvars3d)    = mech_spc_saprc99_x(1:nvars3d)
  mech_mwt(1:nvars3d)    = mech_mwt_saprc99_x(1:nvars3d)
  vname3d(1:nvars3d)     = mech_spc(1:nvars3d)
  case ('SAPRC99Q')
  spmh_map(1:n_scon_spc) = spmh_map_saprc99_q(1:n_scon_spc)
  mech_map(1:n_scon_spc) = mech_map_saprc99_q(1:n_scon_spc)
  conv_fac(1:n_scon_spc) = conv_fac_saprc99_q(1:n_scon_spc)
  mech_spc(1:nvars3d)    = mech_spc_saprc99_q(1:nvars3d)
  mech_mwt(1:nvars3d)    = mech_mwt_saprc99_q(1:nvars3d)
  vname3d(1:nvars3d)     = mech_spc(1:nvars3d)
  case ('CB05')
  spmh_map(1:n_scon_spc) = spmh_map_cb05(1:n_scon_spc)
  mech_map(1:n_scon_spc) = mech_map_cb05(1:n_scon_spc)
  conv_fac(1:n_scon_spc) = conv_fac_cb05(1:n_scon_spc)
  mech_spc(1:nvars3d)    = mech_spc_cb05(1:nvars3d)
  mech_mwt(1:nvars3d)    = mech_mwt_cb05(1:nvars3d)
  vname3d(1:nvars3d)     = mech_spc(1:nvars3d)
  case ('CB6')
  spmh_map(1:n_scon_spc) = spmh_map_cb6(1:n_scon_spc)
  mech_map(1:n_scon_spc) = mech_map_cb6(1:n_scon_spc)
  conv_fac(1:n_scon_spc) = conv_fac_cb6(1:n_scon_spc)
  mech_spc(1:nvars3d)    = mech_spc_cb6(1:nvars3d)
  vname3d(1:nvars3d)     = mech_spc(1:nvars3d)
  case ('SOAX')
  spmh_map(1:n_scon_spc) = spmh_map_soax(1:n_scon_spc)
  mech_map(1:n_scon_spc) = mech_map_soax(1:n_scon_spc)
  conv_fac(1:n_scon_spc) = conv_fac_soax(1:n_scon_spc)
  mech_spc(1:nvars3d)    = mech_spc_soax(1:nvars3d)
  mech_mwt(1:nvars3d)    = mech_mwt_soax(1:nvars3d)
  vname3d(1:nvars3d)     = mech_spc(1:nvars3d)
  endselect
ELSE IF ( .NOT. conversion ) THEN
  PRINT*,'No conversion'
  nvars3d = n_spca_spc
  vname3d(1:nvars3d) = spca_spc(1:nvars3d)
ELSE
  mesg = 'Error: Conversion flag is not assigned.'
  CALL m3exit(progname, 0, 0,mesg,2)
END IF

! Change the unit according to TONPHR flag
IF ( tonphr ) THEN
  units3d(1:nvars3d) = 'tons/hr'
ELSE
  units3d(1:nvars3d) = 'mol/s'
END IF
vdesc3d(1:nvars3d) = ' '
vtype3d(1:nvars3d) = m3real
DO s = 1, nvars3d
  PRINT*,'Output variable:',vname3d(s),units3d(s)
END DO
ftype3d = grdded3
sdate3d = sdate
stime3d = stime
tstep3d = tstep
mxrec3d = mxrec

IF ( conversion ) THEN
  mesg = 'Mechanism Name'
  CALL envstr( 'MECHANISM', mesg, 'SAPRCII', mechanism, ios )
  select case ( trim(mechanism) )
  case ('SAPRCII')
  fdesc3d( 1 ) = 'Chemical specation of MEGAN output to ' //'SAPRCII Species:'
  case ('SAPRC99')
  fdesc3d( 1 ) = 'Chemical specation of MEGAN output to ' //'SAPRC99 Species:'
  case ('RADM2')
  fdesc3d( 1 ) = 'Chemical specation of MEGAN output to ' //'RADM2 Species:'
  case ('RACM')
  fdesc3d( 1 ) = 'Chemical specation of MEGAN output to ' //'RACM Species:'
  case ('CBMZ')
  fdesc3d( 1 ) = 'Chemical specation of MEGAN output to ' //'CBMZ Species:'
  case ('SAPRC99X')
  fdesc3d( 1 ) = 'Chemical specation of MEGAN output to '  &
      //'SAPRC99X Species:'
  case ('SAPRC99Q')
  fdesc3d( 1 ) = 'Chemical specation of MEGAN output to '  &
      //'SAPRC99Q Species:'
  case ('CB05')
  fdesc3d( 1 ) = 'Chemical specation of MEGAN output to ' //'CB05 Species:'
  case ('CB6')
  fdesc3d( 1 ) = 'Chemical specation of MEGAN output to ' //'CB6 Species:'
  case ('SOAX')
  fdesc3d( 1 ) = 'Chemical specation of MEGAN output to ' //'SOAX Species:'
  case default
  mesg = 'Error: Mechanism conversion, invalid MECHANISM: ' //trim(mechanism)
  CALL m3exit(progname,jdate,jtime,mesg,2)
  endselect
  
ELSE IF ( .NOT. conversion ) THEN
  fdesc3d( 1 ) = 'Chemical specation of MEGAN output to '  &
      //'MEGAN 150 species'
ELSE
  mesg = 'Error: Conversion flag is not assigned.'
  CALL m3exit(progname, 0, 0,mesg,2)
  
END IF

CALL nameval ( mgners , mesg )  ! get input file name and path
fdesc3d( 2 ) = 'Input MEGAN file: '//trim(mesg)

vname3d(nvars3d+1) = 'GDAY'
units3d(nvars3d+1) = 'day'
vdesc3d(nvars3d+1) = ' '
vtype3d(nvars3d+1) = m3int
nvar = nvars3d
nvars3d = nvars3d + 1
IF ( .NOT. open3( mgnout, fscrea3, progname ) ) THEN
  mesg = 'Could not open file '//trim(mgnout)
  CALL m3exit( progname, 0, 0, mesg, 2 )
END IF

!...  Allocate memory
allocate ( ef   ( n_mgn_spc, ncols, nrows ), stat = ios )
CALL checkmem ( ios, 'EF',    progname )

allocate ( tmp1 (  ncols, nrows ), stat = ios )
CALL checkmem ( ios, 'tmp1',  progname )

allocate ( tmp2 (  ncols, nrows ), stat = ios )
CALL checkmem ( ios, 'tmp2',  progname )

allocate ( tmp3 (  ncols, nrows ), stat = ios )
CALL checkmem ( ios, 'tmp3',  progname )

allocate ( lat  (  ncols, nrows ), stat = ios )
CALL checkmem ( ios, 'LAT',   progname )

allocate ( cfno (  ncols, nrows ), stat = ios )
CALL checkmem ( ios, 'CFNO',  progname )

allocate ( cfnog(  ncols, nrows ), stat = ios )
CALL checkmem ( ios, 'CFNOG', progname )

allocate ( pft  ( mxpft, ncols, nrows ), stat = ios )
CALL checkmem ( ios, 'PFT',   progname )

allocate ( garr ( ncols, nrows ), stat = ios )
CALL checkmem ( ios, 'GARR', progname )


!...  Read EFMAPS

IF ( .NOT. read3(efmaps,'LAT',1,1,1,lat)) THEN
  mesg = 'Error reading LAT'
  CALL m3exit(progname,jdate,jtime,mesg,2)
END IF

DO n = 1, n_mgn_spc
  IF ( .NOT. read3(efmaps,'EF_'//mgn_spc(n),1,1,1,ef(n,:,:))) THEN
    mesg = 'Error reading '//trim('EF_'//mgn_spc(n))
    CALL m3exit(progname,jdate,jtime,mesg,2)
  END IF
END DO

!...  Read PFTS16
DO n = 1, mxpft
  IF ( .NOT. read3(pfts16,'PFTS',1,0,(n-1)*10000, pft(n,:,:))) THEN
    mesg = 'Error reading PFTS'
    CALL m3exit(progname,0,(n-1)*10000,mesg,2)
  END IF
END DO

!.....2) Conversion from MGN 20 to speciated 150
!-----------------------------------------------------------------------
!...  Allocate memory
allocate ( inper(n_mgn_spc, ncols, nrows ), stat = ios )
CALL checkmem ( ios, 'inper', progname )
allocate ( tmper( n_spca_spc, ncols, nrows ), stat = ios )
CALL checkmem ( ios, 'tmper', progname )
allocate ( outer( nvar, ncols, nrows ), stat = ios )
CALL checkmem ( ios, 'outer', progname )


ino = index1('NO',n_mgn_spc,mgn_spc)
iisop = index1('ISOP',n_mgn_spc,mgn_spc)
!...  Loop through time
jdate = sdate
jtime = stime
DO t = 1, mxrec
  inper = 0.
  tmper = 0.
  outer = 0.
  
  DO n = 1, n_mgn_spc
    IF ( .NOT. read3(mgners,trim(mgn_spc(n)),1,  &
          jdate,jtime,inper(n,:,:))) THEN
      mesg = 'Error reading '//trim(mgn_spc(nmpmg))
      CALL m3exit(progname,jdate,jtime,mesg,2)
    END IF
  END DO
  
  IF ( .NOT. read3(mgners,'CFNO',1,jdate,jtime,cfno(:,:))) THEN
    mesg = 'Error reading CFNO'
    CALL m3exit(progname,jdate,jtime,mesg,2)
  END IF
  
  IF( .NOT. read3(mgners,'CFNOG',1,jdate,jtime,cfnog(:,:))) THEN
    mesg = 'Error reading CFNOG'
    CALL m3exit(progname,jdate,jtime,mesg,2)
  END IF
  
  DO s = 1, n_smap_spc
    
    nmpmg = mg20_map(s)
    nmpsp = spca_map(s)
    IF (t == 1) THEN
      mesg='Convert '//mgn_spc(nmpmg)//' to '//spca_spc(nmpsp)
      CALL m3mesg( mesg )
    END IF
    
    IF ( nmpmg /= ino ) THEN
!...  Not NO
      IF ( ef_all(1,nmpmg) < 0.0) THEN
!... Use EFMAPS
        mesg = 'Use EFMAPS for '//mgn_spc(nmpmg)
        CALL m3mesg( mesg )
        tmp1(:,:) = 0.0
        tmp2(:,:) = 0.0
        DO i = 1,n_mgn_pft
          tmp1 = tmp1 + pft(i,:,:)
          tmp2 = tmp2 + effs_all(i,nmpsp) * pft(i,:,:)
        END DO
        where(tmp1(:,:) == 0.0)
        tmper(nmpsp,:,:) = 0.0
        elsewhere
        tmper(nmpsp,:,:) = inper(nmpmg,:,:) * ef(nmpmg,:,:)  &
            * tmp2(:,:) / tmp1(:,:)
        endwhere
      ELSE
!... Use PFT-EF
        tmp3(:,:) = 0.0
        DO i = 1,n_mgn_pft
          tmp3 = tmp3 + ef_all(i,nmpmg) * effs_all(i,nmpsp) * pft(i,:,:)/100.0
        END DO
        tmper(nmpsp,:,:) = inper(nmpmg,:,:) * tmp3(:,:)
      END IF
    ELSE IF ( nmpmg == ino ) THEN
!!-----------------NO Stuff-----------------------
!...  NO when nmpmg = 17
      mesg = 'Special loop for '//mgn_spc(nmpmg)
      CALL m3mesg( mesg )
      IF (ef_all(1,ino) < 0.0) THEN
        mesg = 'Use EFMAPS for '//mgn_spc(nmpmg)
        CALL m3mesg( mesg )
      END IF
      DO c = 1,ncols
        DO r = 1,nrows
          CALL growseason(jdate,lat(c,r),gday,glen)
          garr(c,r) = gday
! check for growing season
          IF (gday == 0) THEN
! non growing season
! CFNOG for everywhere
! Override crop with grass warm = 14
            IF (ef_all(1,ino) < 0.0) THEN
! with EFMAPS
              tmo1 = 0.0
              tmo2 = 0.0
              DO i = 1,14
                tmo1 = tmo1 + pft(i,c,r)
                tmo2 = tmo2 + effs_all(i,nmpsp) * pft(i,c,r)
              END DO
              DO i = 15,n_mgn_pft
                tmo1 = tmo1 + pft(i,c,r)
                g2cratio = ef_all(14,ino)/ef_all(i,ino)
                tmo2 = tmo2 + effs_all(i,nmpsp) * pft(i,c,r) * g2cratio
              END DO
              IF (tmo1 == 0.0) THEN
                tmper(nmpsp,c,r) = 0.0
              ELSE
!                 tmper(nmpsp,C,R) = inper(INO,C,R) * EF(INO,C,R)
!     &                            * CFNOG(C,R) * TMO2 / TMO1
                tmper(nmpsp,c,r) = inper(ino,c,r) * ef(ino,c,r)  &
                    * cfnog(c,r) * tmo2 / tmo1 * n2no
              END IF
            ELSE
! without EFMAPS
              tmo3 = 0.0
              DO i = 1,14
                tmo3 = tmo3 + ef_all(i,ino)*effs_all(i,nmpsp)  &
                    * pft(i,c,r)/100.0
              END DO
              DO i = 15,n_mgn_pft
                tmo3 = tmo3 + ef_all(14,ino) * effs_all(i,nmpsp)  &
                    * pft(i,c,r)/100.0
              END DO
!               tmper(nmpsp,C,R) = inper(INO,C,R) * CFNOG(C,R) * TMO3
              tmper(nmpsp,c,r) = inper(ino,c,r) * cfnog(c,r) * tmo3 * n2no
            END IF
            
          ELSE IF (gday > 0 .AND. gday <= 366) THEN
! growing season
! CFNOG for everywhere except crops
! CFNO for crop and corn
            IF (ef_all(1,ino) < 0.0) THEN
! with EFMAPS
              tmo1 = 0.0
              tmo2 = 0.0
              DO i = 1,14
                tmo1 = tmo1 + pft(i,c,r)
                tmo2 = tmo2 + effs_all(i,nmpsp)*pft(i,c,r) * cfnog(c,r)
              END DO
              DO i = 15,n_mgn_pft
                tmo1 = tmo1 + pft(i,c,r)
                tmo2 = tmo2 + effs_all(i,nmpsp)*pft(i,c,r) *cfno(c,r)
              END DO
              IF (tmo1 == 0.0) THEN
                tmper(nmpsp,c,r) = 0.0
              ELSE
!                 tmper(nmpsp,C,R) = inper(INO,C,R) * EF(INO,C,R) *
!     &                              TMO2 / TMO1
                tmper(nmpsp,c,r) = inper(ino,c,r) * ef(ino,c,r) *  &
                    tmo2 / tmo1 * n2no
              END IF
            ELSE
! without EFMAPS
              tmo3 = 0.0
              DO i = 1,14
                tmo3 = tmo3 + ef_all(i,ino)*effs_all(i,nmpsp)*  &
                    pft(i,c,r)/100.0* cfnog(c,r)
              END DO
              DO i = 15,n_mgn_pft
                tmo3 = tmo3 + ef_all(i,ino)*effs_all(i,nmpsp)*  &
                    pft(i,c,r)/100.0* cfno(c,r)
              END DO
!               tmper(nmpsp,C,R) = inper(INO,C,R) * TMO3
              tmper(nmpsp,c,r) = inper(ino,c,r) * tmo3 * n2no
            END IF
          ELSE
! bad GDAY
            WRITE(mesg,*) 'Bad GDAY ',gday
            CALL m3exit(progname,jdate,jtime,mesg,2)
          END IF
        END DO  !DO R = 1,nrows
      END DO  !DO C = 1,ncols
!-----------------end of NO----------------------
    END IF     !IF ( nmpmg .NE. INO ) then
    
  END DO ! End species loop
  
!-----------------------------------------------------------------------
!.....3) Conversion from speciated species to MECHANISM species
!-----------------------------------------------------------------------
! convert from ug/m^2/hr to mol/m^2/hr using their MW
  DO s = 1, n_spca_spc
    tmper(s,:,:) = tmper(s,:,:) / spca_mwt(s)*ug2g
    IF (t == 1) THEN
      PRINT*, trim(spca_spc(s))//' = '//  &
          trim(spca_spc(s))//' / MW ',spca_mwt(s)
    END IF
  END DO
  
  IF ( conversion ) THEN
! lumping to MECHANISM species
    
    DO s = 1, n_scon_spc
      nmpsp = spmh_map(s)         ! Mapping value for SPCA
      nmpmc = mech_map(s)         ! Mapping value for MECHANISM
      
      IF (t == 1) THEN
        mesg='Convert '//spca_spc(nmpsp)//' to '//mech_spc(nmpmc)
        CALL m3mesg( mesg )
      END IF
      
      outer(nmpmc,:,:) = outer(nmpmc,:,:) + (tmper(nmpsp,:,:)*conv_fac(s))*  &
          garea/hr2sec
! units of these species are in mole/s
    END DO ! End species loop
    
  ELSE
! get all 150 species into the output array
    outer(:,:,:) = tmper(:,:,:)* garea/hr2sec
! units of these species are in mole/s/grid
    
  END IF
  
!-----------------------------------------------------------------------
!.....4) Convert to tonne/hour if needed
!-----------------------------------------------------------------------
  IF ( tonphr ) THEN
    IF ( conversion ) THEN
! convert from mol/s to ton/hr
      DO s = 1, nvar
        outer(s,:,:) = outer(s,:,:)*mech_mwt(s)* g2tonne*hr2sec
      END DO
    ELSE IF ( .NOT. conversion ) THEN
! convert from mol/s to ton/hr
      DO s = 1, nvar
        outer(s,:,:) = outer(s,:,:)*spca_mwt(s)* g2tonne*hr2sec
      END DO
    END IF
  END IF
  
!-----------------------------------------------------------------------
!.....5) Write out the calculated emissions
!-----------------------------------------------------------------------
  PRINT*,'VARS=',nvar,n_scon_spc
  DO s = 1, nvar
    IF ( .NOT. write3(mgnout,vname3d(s),jdate,jtime,  &
          outer(s,:,:)    )  ) THEN
      mesg = 'Error writing to file: '//trim(mgnout)
      CALL m3exit(progname,jdate,jtime,mesg,2)
    END IF
  END DO
  
  s = nvar + 1
  IF ( .NOT. write3(mgnout,vname3d(s),jdate,jtime, garr(:,:)    )  ) THEN
    mesg = 'Error writing to file: '//trim(mgnout)
    CALL m3exit(progname,jdate,jtime,mesg,2)
  END IF
!-----------------------------------------------------------------------
  CALL nextime(jdate,jtime,tstep)
END DO ! End time loop

!... Exit and close file
CALL m3exit(progname,0,0,' ',0)

deallocate ( spmh_map )
deallocate ( mech_map )
deallocate ( conv_fac )
deallocate ( mech_spc )
deallocate ( mech_mwt )
deallocate (tmp1)
deallocate (tmp2)
deallocate (tmp3)
deallocate (lat)
deallocate (ef)
deallocate (cfno)
deallocate (cfnog)
deallocate (pft)
deallocate (outer)
deallocate (inper)
deallocate (tmper)
deallocate (garr)

!=======================================================================
!...  FORMAT
!=======================================================================


!=======================================================================
!...  End program
!=======================================================================
endprogram
