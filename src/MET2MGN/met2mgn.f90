PROGRAM met2mgn
!-----
!     met2mgn produces a Models3-IO/API file with various meteorological
!     variables to be read into the MEGAN biogenics emissions model.
!-----
!Converted to f90 by Luiz Flavio Rodrigues
 
USE IODECL3
USE PARMS3
USE FDESC3
USE param
USE fields

IMPLICIT NONE

INTEGER :: idate,sdate,edate,stime,etime,r_sdate,r_stime,epsdate
INTEGER :: istat,m1,p1
INTEGER :: addday,subday,logunit
CHARACTER (LEN=16) :: cname
CHARACTER (LEN=80) :: runmsg
CHARACTER (LEN=256) :: mesg
LOGICAL :: dscgrid,mm5_met,wrf_met,mcip_met,envyn
LOGICAL :: sat_par,mcip_rad,mm5_rad,wrf_rad

CHARACTER (LEN=16),PARAMETER  :: oname = 'OUTFILE'
CHARACTER (LEN=16),PARAMETER  :: pgname = 'met2mgn'
INTEGER :: envint,ios

DATA runmsg /'MEGAN meteorology preprocessor'/

logunit=init3()
mesg = 'Coordinate name: '
CALL envstr( 'GDNAM3D', mesg, 'RPO_36km', gdnam3d, ios )
IF( .NOT. dscgrid( gdnam3d, cname, gdtyp3d,  &
      p_alp3d, p_bet3d, p_gam3d, xcent3d, ycent3d,  &
      xorig3d, yorig3d, xcell3d, ycell3d, ncols3d, nrows3d, nthik3d ) ) THEN
  mesg = 'Could not get grid description.'
  CALL m3exit ( 'met2mgn', 0, 0, mesg, 2 )
END IF
gdtyp3d=gdtyp3d

IF (gdtyp3d /= 2.AND.gdtyp3d /= 5.AND.gdtyp3d /= 1) THEN
  mesg = 'Only LCP, UTM, and LATLON supported at this time.'
  CALL m3exit ( 'met2mgn', 0, 0, mesg, 2 )
END IF

nlays3d=1
ftype3d = grdded3
tstep3d = 10000

fdesc3d(1) = runmsg
gdnam3d = gdnam3d
vgtyp3d = 2
vgtop3d = 100.0        !millibars

!-----Get env variables and initialize
!
mcip_met = envyn ( 'MCIPMET', mesg, .false., ios )
mm5_met  = envyn ( 'MM5MET', mesg, .false., ios )
!      WRF_met  = ENVYN ( 'WRFMET', MESG, .FALSE., IOS )

sat_par  = envyn ( 'SATPAR', mesg, .false., ios )
mcip_rad = envyn ( 'MCIPRAD', mesg, .false., ios )
mm5_rad  = envyn ( 'MM5RAD', mesg, .false., ios )
!      WRF_rad  = ENVYN ( 'WRFRAD', MESG, .FALSE., IOS )
!
!-----Error handling for environment variable selection
!
IF(.NOT.mcip_met .AND. .NOT.mm5_met .AND. .NOT.wrf_met) THEN
  mesg = 'One meteorology input option must be selected'
  CALL m3exit('met2mgn',0,0,mesg,2)
END IF
m1=0
IF(mcip_met) m1=m1+1
!      if(MM5_met)  m1=m1+1
!      if(WRF_met)  m1=m1+1

IF(m1 >= 2) THEN
  mesg = 'Only one meteorology input option can be selected'
  CALL m3exit('met2mgn',0,0,mesg,2)
END IF

IF(.NOT.sat_par .AND. .NOT.mcip_rad .AND. .NOT.mm5_rad  &
      .AND. .NOT.wrf_rad) THEN
  mesg = 'One PAR/RAD option must be selected'
  CALL m3exit('met2mgn',0,0,mesg,2)
END IF

p1=0
!      if(SAT_par)  p1=p1+1
IF(mcip_rad) p1=p1+1
!      if(MM5_rad)  p1=p1+1
!      if(WRF_rad)  p1=p1+1

IF(p1 >= 2) THEN
  mesg = 'Only one PAR/RAD option can be selected'
  CALL m3exit('met2mgn',0,0,mesg,2)
END IF

sdate = envint('STDATE','Output start date',0,istat)
IF (istat > 0) THEN
  mesg = 'Bad value for STDATE'
  CALL m3exit('met2mgn',0,0,mesg,2)
END IF

edate = envint('ENDATE','Output end date',0,istat)
IF (istat > 0) THEN
  mesg = 'Bad value for ENDATE'
  CALL m3exit('met2mgn',0,0,mesg,2)
END IF

epsdate = envint('EPISODE_SDATE','Episode Start Date',0,istat)
IF (istat > 0) THEN
  mesg = 'Bad value for EPISODE_SDATE'
  CALL m3exit('met2mgn',0,0,mesg,2)
END IF

stime=MOD(sdate,100)
etime=MOD(edate,100)
sdate=sdate/100
edate=edate/100
epsdate=epsdate-2000000

!---Calculate rain start date/time (23 hours before target start time)
IF (epsdate == sdate) THEN
  r_stime=stime
  r_sdate=sdate
ELSE
  r_stime=stime+1
  r_sdate=subday(sdate)
END IF

IF(etime > 23) THEN
  etime=etime-24
  edate=addday(edate)
END IF
IF(stime > 23) THEN
  stime=stime-24
  sdate=addday(sdate)
END IF
IF(r_stime > 23) THEN
  r_stime=r_stime-24
  r_sdate=addday(r_sdate)
END IF

stime=100*stime
etime=100*etime
r_stime=100*r_stime

IF (mcip_met) THEN
  CALL readmcip(sdate,stime,edate,etime,r_sdate,r_stime)
ELSE IF (mm5_met) THEN
  CALL procmm5(sdate,stime,edate,etime)
END IF

WRITE(*,*) 'Normal completion by met2mgn'

STOP
END PROGRAM met2mgn
!
!-----Date functions
!

INTEGER FUNCTION addday(idate)
IMPLICIT NONE
INTEGER, INTENT(IN)                      :: idate

INTEGER :: iyr,idy

iyr = idate/1000
idy = idate - iyr*1000
IF ((MOD(iyr,4) == 0 .AND. idy == 366) .OR.  &
      (MOD(iyr,4) /= 0 .AND. idy == 365)) THEN
  iyr = iyr + 1
  IF (iyr > 99) iyr = 0
  addday = iyr*1000 + 1
ELSE
  addday = idate + 1
END IF
END FUNCTION addday
!

INTEGER FUNCTION subday(idate)
IMPLICIT NONE
INTEGER, INTENT(IN)                      :: idate

INTEGER :: iyr,idy

iyr = idate/1000
idy = idate - iyr*1000
IF (idy == 1) THEN
  iyr = iyr - 1
  IF (iyr < 0) iyr = 99
  IF (MOD(iyr,4) == 0) THEN
    idy = 366
  ELSE
    idy = 365
  END IF
  subday = iyr*1000 + idy
ELSE
  subday = idate - 1
END IF
END FUNCTION subday
