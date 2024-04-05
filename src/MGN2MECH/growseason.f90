
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
REAL,intent(in)           :: lat
INTEGER, INTENT(OUT)                     :: gday
INTEGER, INTENT(OUT)                     :: glen

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
CHARACTER (LEN=256) ::   mesg         ! message buffer
INTEGER :: g2j

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
