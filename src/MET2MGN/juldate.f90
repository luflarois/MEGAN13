SUBROUTINE juldate(idate)
 
!-----JULDATE converts date from calender (YYMMDD) format to Julian
!     (YYJJJ) format
IMPLICIT NONE

INTEGER, INTENT(IN OUT)                  :: idate



INTEGER :: nday(12),iyear,imonth,iday,mday,n,jday

DATA nday/31,28,31,30,31,30,31,31,30,31,30,31/

!-----Entry point

iyear = idate/10000
imonth = (idate - iyear*10000)/100
iday = idate - iyear*10000 - imonth*100

nday(2) = 28
IF (MOD(iyear,4) == 0) nday(2) = 29
mday = 0
DO  n = 1,imonth-1
  mday = mday + nday(n)
END DO
jday = mday + iday
idate = iyear*1000 + jday

RETURN
END SUBROUTINE juldate
