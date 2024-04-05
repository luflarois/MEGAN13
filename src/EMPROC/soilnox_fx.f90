!=======================================================================

!     MODULE SOILNOX_FX
!
!     This module contain functions to assist soil NOx calculation.
!
!
!     CONTAINS: 1)FERTLZ_ADJ
!               2)VEG_ADJ
!               3)GROWSEASON
!
!     Note:
!
!     Requirement:
!
!
!     Imported from SMOKE-BEIS v3.14 and modified
!          by Tan 07/21/11 for MEGAN v2.10
!
!     Function PRECADJ is moved to MET2MGN
!              PULSETYPE is moved to MET2MGN
!              PRECIPFAC is moved to MET2MGN
!
!     History:
!
!=======================================================================

module soilnox_fx

IMPLICIT NONE

!...  Program I/O parameters

!...  External parameters

contains


!=======================================================================
!=======================================================================

REAL FUNCTION fertlz_adj( date, lat )

!***********************************************************************
!  DESCRIPTION:
!    This internal function computes a fertilizer adjustment factor
!    for the given date in yyyyddd format. If it is not growing
!    season, the adjustment factor is 0; otherwise, it ranges from
!    0.0 to 1.0.

!  CALL:
!    GROWSEASON

!  HISTORY:
!    07/21/11 : Imported from SMOKE-BEIS v3.14 and modified  (Tan)
!***********************************************************************
IMPLICIT NONE

INTEGER,intent(in) :: date
REAL,intent(in)    :: lat


!.... Local variables
INTEGER :: gday, glen

CHARACTER (LEN=256) ::   mesg         ! message buffer
!-----------------------------------------------------------------------------

CALL growseason( date, lat, gday, glen )

IF( gday == 0 ) THEN
  fertlz_adj = 0.
ELSE IF( gday >= 1 .AND. gday < 30 ) THEN
! first month of growing season
  fertlz_adj = 1.
ELSE IF( gday >= 30 .AND. gday <= 366) THEN
! later month of growing season
  fertlz_adj = 1. + 30. / FLOAT(glen) - FLOAT(gday) / FLOAT(glen)
ELSE
  WRITE( mesg,94010 ) 'Invalid date specified; date = ',  &
      date, 'growing season day = ', gday
  CALL m3exit( 'FERTLZ_ADJ', 0, 0, mesg, 2 )
END IF

!******************  FORMAT  STATEMENTS   ******************************
94010 FORMAT( a, f10.2, 1X, a, i3, ',', i3 )


RETURN

END FUNCTION fertlz_adj
!=======================================================================
!=======================================================================


!=======================================================================
!=======================================================================

REAL FUNCTION veg_adj( lai )

!***********************************************************************
!  DESCRIPTION
!    This internal function computes a vegetation adjustment factor
!    based on LAIv.  See Yienger and Levy 1995
!    VEG_ADJ = (EXP(-0.24*LAIv)+EXP(-0.0525*LAIv))*0.5

!  CALL
!    NONE

!  HISTORY:
!***********************************************************************
IMPLICIT NONE

REAL,intent(in)      :: lai


veg_adj = (EXP(-0.24*lai)+EXP(-0.0525*lai))*0.5

!******************  FORMAT  STATEMENTS   ******************************

RETURN
END FUNCTION veg_adj
!=======================================================================
!=======================================================================



!=======================================================================
!=======================================================================

SUBROUTINE growseason ( date, lat, gday, glen )

!***********************************************************************
!  DESCRIPTION
!    This internal function computes the day of the growing season
!    corresponding to the given date in yyyyddd format.

!  CALL
!    JULIAN

!  HISTORY:
!    07/21/11 : Imported from SMOKE-BEIS v3.14 and modified  (Tan)
!               Variation of growing season depends on latitude
!               (Guenther)
!***********************************************************************

IMPLICIT NONE

INTEGER,intent(in)    :: date
REAL,INTENT(IN)       :: lat
INTEGER,INTENT(OUT)   :: gday
INTEGER,INTENT(OUT)   :: glen

!.......  External functions
INTEGER,EXTERNAL :: julian

!.......  Local parameters
INTEGER :: gseason_start
INTEGER :: gseason_end

!.......  Local variables
INTEGER :: year, month, day
INTEGER :: jday
INTEGER :: gsjulian_start
INTEGER :: gsjulian_end
CHARACTER (LEN=256) :: mesg         ! message buffer

!-----------------------------------------------------------------------------

year = INT( FLOAT( date ) / 1000. )
jday = date - year * 1000

IF( jday < 1 .OR. jday > 366 ) THEN
  WRITE( mesg,94010 ) 'Invalid date specified; date = ', date, 'jday = ', jday
  CALL m3exit( 'GROWSEASON', 0, 0, mesg, 2 )
END IF

IF ( lat <= 23.0 .AND. lat >= -23.0 ) THEN
! tropical regions, year round
  gseason_start = 0101
  gseason_end   = 1231
  
  gsjulian_start = g2j(year, gseason_start)
  gsjulian_end   = g2j(year, gseason_end)
  gday = jday - gsjulian_start + 1
  glen = gsjulian_end - gsjulian_start + 1
ELSE IF ( lat < -23.0 ) THEN
! southern hemisphere
  IF ( lat < -60.0 ) THEN
! antarctic start = 0 end = 0, no growing
    gday = 0
    glen = 0
  ELSE
! southern hemisphere temperate, NOV, DEC, JAN-MAY
    IF (jday >= 1101 .AND. jday <= 1231 ) THEN
      gseason_start = 1101
      gseason_end   = 1231
      
      gsjulian_start = g2j(year, gseason_start)
      gsjulian_end   = g2j(year, gseason_end)
      gday = jday - gsjulian_start + 1
    ELSE IF (jday >= 0101 .AND. jday <= 0531) THEN
      gseason_start = 0101
      gseason_end   = 0531
      
      gsjulian_start = g2j(year, gseason_start)
      gsjulian_end   = g2j(year, gseason_end)
      gday = jday - gsjulian_start + 1 + 61
    ELSE
      gday = 0
    END IF
    glen = 30 + 31 + g2j(year,0531) - g2j(year,0101) + 1
    
  END IF
ELSE IF ( lat > 23.0 ) THEN
! northern hemisphere
  IF ( lat > 65.0 ) THEN
! arctic start = 0 end = 0, no growing season
    gday = 0
    glen = 0
  ELSE
! northern hemisphere temperate
! start= (lat-23)*4.5            189
! end = 365 -((lat-23)*3.3)      226
    gseason_start = 0
    gseason_end   = 1231
    
    gsjulian_start = 0
    gsjulian_end   = g2j(year, gseason_end)
    
    gsjulian_start = INT( (lat-23.0)*4.5 )
    gsjulian_end   = gsjulian_end - INT( (lat-23.0)*3.3 )
    IF (jday >= gsjulian_start .AND. jday <= gsjulian_end) THEN
      gday = jday - gsjulian_start + 1
    ELSE
      gday = 0
    END IF
    glen = gsjulian_end - gsjulian_start + 1
  END IF
ELSE
  mesg = 'Invalid LAT'
  CALL m3exit( 'GROWSEASON', 0, 0, mesg, 2 )
END IF



!******************  FORMAT  STATEMENTS   ******************************
94010 FORMAT( a, f10.2, 1X, a, i3, ',', i3 )

RETURN

END SUBROUTINE growseason
!=======================================================================
!=======================================================================


!=======================================================================
!=======================================================================

INTEGER FUNCTION g2j( yyyy, mmdd )
IMPLICIT NONE
INTEGER,INTENT(IN)        :: yyyy
INTEGER,INTENT(IN)        :: mmdd

!.......  External functions
INTEGER,EXTERNAL :: julian

!.......  Local parameters
INTEGER :: mm
INTEGER :: dd

mm = INT( FLOAT( mmdd ) / 100. )
dd = mmdd - mm * 100
g2j = julian( yyyy, mm , dd )

END FUNCTION g2j
!=======================================================================
!=======================================================================
END module soilnox_fx
