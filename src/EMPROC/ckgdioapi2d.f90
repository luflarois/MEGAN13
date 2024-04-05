LOGICAL FUNCTION ckgdioapi2d( infile1, infile2 )

!***********************************************************************
!
!  DESCRIPTION:
!      This function compares two ioapi file for
!       - grid/projection parameters
!       - TRUE if all match
!
!  PRECONDITIONS REQUIRED:
!       - INFILE1 and INFILE2 are already opened
!
!  SUBROUTINES AND FUNCTIONS CALLED:
!
!  REVISION  HISTORY:
!    started 10/04 by Jack Chen prototype
!
!**************************************************************************
!...........   MODULES for public variables
USE fdesc3   !  I/O API file description data structures.
USE iodecl3  !  I/O API function declarations
use parms3   !  I/O API parameters

IMPLICIT NONE

!............  External Functions:
!     CHARACTER*16   PROMPTMFILE
!     LOGICAL        CHKMETEM
!     EXTERNAL       PROMPTMFILE, CHKMETEM

!........... Argument variables:
CHARACTER (LEN=*), intent (in) :: infile1
CHARACTER (LEN=*), intent (in) :: infile2

!........... Local Variables
INTEGER :: ncols1 = 0      ! number of columns in grid
INTEGER :: ngrid1 = 0      ! number of cells in grid
INTEGER :: nlays1 = 0      ! number of layers
INTEGER :: nrows1 = 0      ! number of rows in grid
INTEGER :: nthik1 = 1
INTEGER :: gdtyp1 = -1     ! i/o api grid type code
REAL :: p_alp1 = 0.d0   ! projection alpha
REAL :: p_bet1 = 0.d0   ! projection beta
REAL :: p_gam1 = 0.d0   ! projection gamma
REAL :: xcent1 = 0.d0   ! x-center of projection
REAL :: ycent1 = 0.d0   ! y-center of projection
REAL :: xorig1 = 0.d0   ! x-origin of grid
REAL :: yorig1 = 0.d0   ! y-origin of grid
REAL :: xcell1 = 0.d0   ! x-dim of cells
REAL :: ycell1 = 0.d0   ! y-dim of cells
INTEGER :: vgtyp1 = -1     ! type of vertical coordinates
REAL :: vgtop1 = 0.0    ! model-top, for sigma coord types
REAL,allocatable,DIMENSION(:)  :: vglvs1     ! vertical coordinate values

INTEGER :: ncols2 = 0      ! number of columns in grid
INTEGER :: ngrid2 = 0      ! number of cells in grid
INTEGER :: nlays2 = 0      ! number of layers
INTEGER :: nrows2 = 0      ! number of rows in grid
INTEGER :: nthik2 = 1
INTEGER :: gdtyp2 = -1     ! i/o api grid type code
REAL :: p_alp2 = 0.d0   ! projection alpha
REAL :: p_bet2 = 0.d0   ! projection beta
REAL :: p_gam2 = 0.d0   ! projection gamma
REAL :: xcent2 = 0.d0   ! x-center of projection
REAL :: ycent2 = 0.d0   ! y-center of projection
REAL :: xorig2 = 0.d0   ! x-origin of grid
REAL :: yorig2 = 0.d0   ! y-origin of grid
REAL :: xcell2 = 0.d0   ! x-dim of cells
REAL :: ycell2 = 0.d0   ! y-dim of cells
INTEGER :: vgtyp2 = -1     ! type of vertical coordinates
REAL :: vgtop2 = 0.0    ! model-top, for sigma coord types
REAL,allocatable,DIMENSION(:) :: vglvs2     ! vertical coordinate values

INTEGER :: STATUS
INTEGER :: l
CHARACTER (LEN=256) :: mesg
CHARACTER (LEN=16) :: progname = 'CKGDIOAPI2D' ! Program name

!........... Begin subroutine

!... Open 1st input file and store variables
IF( .NOT. desc3( infile1 ) ) THEN
  mesg = 'Could not get input file "'//trim(infile1) //'" description'
  CALL m3exit( progname, 0, 0, mesg, 2 )
END IF

gdtyp1 = gdtyp3d
p_alp1 = p_alp3d
p_bet1 = p_bet3d
p_gam1 = p_gam3d
xcent1 = xcent3d
ycent1 = ycent3d
xorig1 = xorig3d
yorig1 = yorig3d
nrows1 = nrows3d
ncols1 = ncols3d
xcell1 = xcell3d
ycell1 = ycell3d
nthik1 = nthik3d
nlays1 = nlays3d
IF( nlays1 > 1 ) THEN
  allocate ( vglvs1 ( nlays1 + 1 ), stat = STATUS )
  CALL checkmem( STATUS, 'VGLVS1', progname )
  vgtyp1 = vgtyp3d
  vglvs1 = vglvs3d  ! array
  vgtop1 = vgtop3d
END IF

!... Open 2nd input file and store variables
IF( .NOT. desc3( infile2 ) ) THEN
  mesg = 'Could not get input file "'//trim(infile2)// '" description'
  CALL m3exit( progname, 0, 0, mesg, 2 )
END IF

gdtyp2 = gdtyp3d
p_alp2 = p_alp3d
p_bet2 = p_bet3d
p_gam2 = p_gam3d
xcent2 = xcent3d
ycent2 = ycent3d
xorig2 = xorig3d
yorig2 = yorig3d
nrows2 = nrows3d
ncols2 = ncols3d
xcell2 = xcell3d
ycell2 = ycell3d
nthik2 = nthik3d
nlays2 = nlays3d
IF( nlays2 > 1 ) THEN
  allocate ( vglvs2 ( nlays2 + 1 ), stat = STATUS )
  CALL checkmem( STATUS, 'VGLVS2', progname )
  vgtyp2 = vgtyp3d
  vglvs2 = vglvs3d  ! array
  vgtop2 = vgtop3d
END IF

IF( gdtyp1 /= gdtyp2 ) THEN
  WRITE( mesg, * )'Differences in GDTYP: ',gdtyp1,' and ',gdtyp2
  CALL m3warn( progname, 0, 0, mesg )
  ckgdioapi2d = .false.
  RETURN
END IF

IF( nrows1 /= nrows2 ) THEN
  WRITE( mesg, * )'Differences in NROWS: ',nrows1,' and ',nrows2
  CALL m3warn( progname, 0, 0, mesg )
  ckgdioapi2d = .false.
  RETURN
END IF

IF( ncols1 /= ncols2 ) THEN
  WRITE( mesg, * )'Differences in NCOLS: ',ncols1,' and ',ncols2
  CALL m3warn( progname, 0, 0, mesg )
  ckgdioapi2d = .false.
  RETURN
END IF

IF( flterr ( p_alp1 , p_alp2 ) ) THEN
  WRITE( mesg, * )'Differences in P_ALP: ',p_alp1,' and ',p_alp2
  CALL m3warn( progname, 0, 0, mesg )
  ckgdioapi2d = .false.
  RETURN
END IF

IF( flterr ( p_bet1 , p_bet2 ) ) THEN
  WRITE( mesg, * )'Differences in P_BET: ',p_bet1,' and ',p_bet2
  CALL m3warn( progname, 0, 0, mesg )
  ckgdioapi2d = .false.
  RETURN
END IF

IF( flterr ( p_gam1 , p_gam2 ) ) THEN
  WRITE( mesg, * )'Differences in P_GAM: ',p_gam1,' and ',p_gam2
  CALL m3warn( progname, 0, 0, mesg )
  ckgdioapi2d = .false.
  RETURN
END IF

IF( flterr ( xcent1 , xcent2 ) ) THEN
  WRITE( mesg, * )'Differences in XCENT: ',xcent1,' and ',xcent2
  CALL m3warn( progname, 0, 0, mesg )
  ckgdioapi2d = .false.
  RETURN
END IF

IF( flterr ( ycent1 , ycent2 ) ) THEN
  WRITE( mesg, * )'Differences in YCENT: ',ycent1,' and ',ycent2
  CALL m3warn( progname, 0, 0, mesg )
  ckgdioapi2d = .false.
  RETURN
END IF

IF( flterr ( xorig1 , xorig2 ) ) THEN
  WRITE( mesg, * )'Differences in XORIG: ',xorig1,' and ',xorig2
  CALL m3warn( progname, 0, 0, mesg )
  ckgdioapi2d = .false.
  RETURN
END IF

IF( flterr ( yorig1 , yorig2 ) ) THEN
  WRITE( mesg, * )'Differences in YORIG: ',yorig1,' and ',yorig2
  CALL m3warn( progname, 0, 0, mesg )
  ckgdioapi2d = .false.
  RETURN
END IF

IF( flterr ( xcell1 , xcell2 ) ) THEN
  WRITE( mesg, * )'Differences in XCELLS: ',xcell1,' and ',xcell2
  CALL m3warn( progname, 0, 0, mesg )
  ckgdioapi2d = .false.
  RETURN
END IF

IF( flterr ( ycell1 , ycell2 ) ) THEN
  WRITE( mesg, * )'Differences in YCELL: ',ycell1,' and ',ycell2
  CALL m3warn( progname, 0, 0, mesg )
  ckgdioapi2d = .false.
  RETURN
END IF

IF( nthik1 /= nthik2 ) THEN
  WRITE( mesg, * )'Differences in NTHIK: ',nthik1,' and ',nthik2
  CALL m3warn( progname, 0, 0, mesg )
  ckgdioapi2d = .false.
  RETURN
END IF

IF( nlays1 /= nlays2 ) THEN
  WRITE( mesg, * )'Differences in NLAYS: ',nlays1,' and ',nlays2
  CALL m3warn( progname, 0, 0, mesg )
  ckgdioapi2d = .true.
  RETURN
END IF

IF( nlays1 > 1 ) THEN
  IF( vgtyp1 /= vgtyp2 ) THEN
    WRITE( mesg, * )'Differences in VGTYP: ',vgtyp1,' and ',vgtyp2
    CALL m3warn( progname, 0, 0, mesg )
    ckgdioapi2d = .true.
    RETURN
  END IF
  IF( vgtop1  /= vgtop2 ) THEN
    WRITE( mesg, * )'Differences in VGTOP: ',vgtop1,' and ',vgtop2
    CALL m3warn( progname, 0, 0, mesg )
    ckgdioapi2d = .true.
    RETURN
  END IF
  DO l = 1, nlays1+1
    IF( flterr ( vglvs1( l ) , vglvs2( l ) ) ) THEN
      WRITE( mesg, * )'Differences in VGLVS: ',vglvs1( l ),  &
          ' and ',vglvs2( l )
      CALL m3warn( progname, 0, 0, mesg )
      ckgdioapi2d = .true.
      RETURN
    END IF
  END DO
END IF ! NLAY

ckgdioapi2d = .true.

RETURN

CONTAINS

   !... Internal Subprogram - comparing two real values
   !... Copied from chkmetem.f of SMOKE program

      LOGICAL FUNCTION flterr( p, q )

      REAL,intent (in) ::  p, q

      flterr = ( (p - q)**2  >  1.0E-12*( p*p + q*q + 1.0E-5 ) )

      RETURN

   END FUNCTION flterr

END FUNCTION ckgdioapi2d

