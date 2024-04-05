SUBROUTINE caldate(idate)
 
!----CAMx v4.40 061025

!     CALDATE converts date from Julian (YYJJJ) format to calender
!     (YYMMDD) format

!     Copyright 1996-2006
!     ENVIRON International Corporation

!     Modifications:
!        none

!     Input arguments:
!        idate               julian date (YYJJJ)

!     Output arguments:
!        idate               calender date (YYMMDD)

!     Routines Called:
!        none

!     Called by:
!        DRYDEP
!        DRYDEPRT
!        CHRTIME
!        PIGDRIVE


INTEGER, INTENT(IN OUT)                  :: idate
INTEGER :: iyear,jday,imonth,nday,mday,iday,mody
DIMENSION nday(12)
DATA nday/31,28,31,30,31,30,31,31,30,31,30,31/

!-----Entry point

!-----If it is already in calender date, return

IF (idate > 100000) GO TO 9999
iyear = idate/1000
jday = idate - iyear*1000

IF(jday == 0) GO TO 30

nday(2) = 28
IF (MOD(iyear,4) == 0) nday(2) = 29
mday = 0
DO  imonth = 1,12
  mday = mday + nday(imonth)
  IF (mday >= jday) EXIT
END DO
20   iday = jday - (mday - nday(imonth))
idate = iyear*10000 + imonth*100 + iday

!-----Added to automatically correct for last and first day of year

30   mody = idate - (iyear*1000)
IF (mody == 0) idate = (iyear-1)*10000 + 1231

9999 RETURN
END SUBROUTINE caldate
