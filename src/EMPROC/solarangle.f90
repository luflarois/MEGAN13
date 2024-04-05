!-----------------------------------------------------------------------
!   SUBROUTINE: SOLARANGLE
!
!   Description: To calculate the solar zenith angle.  This will give
!                SIN(BETA), not the BETA.
!
!   Call: None
!
!   Require: None
!
!   Input:
!            1) Day of year
!            2) Latitude
!            3) Hour
!
!   Output: CALCBETA (Solar zenith angle)
!
!   Created by Tan 11/15/06  (based on xxxx's program)
!
!-----------------------------------------------------------------------

SUBROUTINE solarangle( day, shour, lat, sinbeta)

IMPLICIT NONE
INTEGER,intent(in) :: day
REAL,intent(in)       :: shour
REAL,intent(in)          :: lat
REAL,intent(out)      :: sinbeta


! Local
REAL :: beta                 ! solar elevation angle
REAL :: sindelta, cosdelta, a, b
! Constants
REAL,PARAMETER :: pi    = 3.14159, d2rad = pi/180.0,  &
    rad2d = 180.0/pi

! Calculation
sindelta = -SIN(0.40907) * COS( 6.28*(day+10)/365 )
cosdelta = (1-sindelta**2.)**0.5

a = SIN( lat*d2rad ) * sindelta
b = COS( lat*d2rad ) * cosdelta

sinbeta = a + b * COS(2*pi*(shour-12)/24)  ! This will be transfered
! to gamma_p function

beta = ASIN(sinbeta)*rad2d    ! This is not used.

RETURN
END SUBROUTINE solarangle
!-----------------------------------------------------------------------

