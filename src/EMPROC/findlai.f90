!-----------------------------------------------------------------------
!   SUBROUTINE: FINDLAI
!
!   Description: Find current LAI and previous LAI from LAIS46 and IDATE
!
!   Call: None
!
!   Require: None
!
!   Input:
!            1) IDATE : current julian date
!
!   Output:  1) LAIp_I
!            2) LAIc_I
!
!   Created by Tan 07/28/11
!
!-----------------------------------------------------------------------

SUBROUTINE findlai( idate, mxlai, laip_dy, laip_hr, laic_dy, laic_hr)

IMPLICIT NONE
INTEGER,intent(in)    :: idate
INTEGER,intent(in)    :: mxlai
INTEGER,INTENT(OUT)   :: laip_dy
INTEGER,INTENT(OUT)   :: laip_hr
INTEGER,INTENT(OUT)   :: laic_dy
INTEGER,INTENT(OUT)   :: laic_hr

! Local
INTEGER :: jjj
INTEGER :: laip_i, laic_i
REAL :: xxx

! Calculation

jjj = MOD(idate,1000)
xxx = jjj/8.0
laic_i = ceiling(xxx)

IF (laic_i == 1) THEN
  laip_i = mxlai
ELSE
  laip_i = laic_i - 1
END IF

laic_hr = (MOD(laic_i-1,24))*10000
xxx     = (laic_i-1)/24.0
laic_dy = floor(xxx)
laip_hr = (MOD(laip_i-1,24))*10000
xxx     = (laip_i-1)/24.0
laip_dy = floor(xxx)

RETURN
END SUBROUTINE findlai
!-----------------------------------------------------------------------
