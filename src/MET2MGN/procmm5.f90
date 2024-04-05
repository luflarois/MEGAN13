SUBROUTINE procmm5 (sdate,stime,edate,etime)
use IODECL3
use PARMS3
use FDESC3
USE param
USE fields
!-----
!     procmm5 processes multiple MM5 files and extracts temperature and
!     PAR (SRAD/2)
!-----



INTEGER, INTENT(IN)                      :: sdate
INTEGER, INTENT(IN)                      :: stime
INTEGER, INTENT(IN OUT)                  :: edate
INTEGER, INTENT(IN OUT)                  :: etime
INTEGER, PARAMETER :: mxx=300
INTEGER, PARAMETER :: mxy=300
INTEGER, PARAMETER :: mxz=30
INTEGER :: kz1(mxz),kz2(mxz),kzin(mxz)
REAL :: tsurf(mxx,mxy),uwind(mxx,mxy,0:mxz),vwind(mxx,mxy,mxz)
REAL :: height(mxx,mxy,mxz),press(mxx,mxy,mxz),wind
REAL :: temp(mxx,mxy,mxz),dummy(mxx,mxy),rad(mxx,mxy)
REAL :: temp1,press1,uwind1,vwind1,fsurf(mxx,mxy,mxz)
REAL :: deltaz,temp0,press0,z0i
INTEGER :: i,j,k,l,m
INTEGER :: nx,ny,nz,jdate,jtime,idate,month,itzon
INTEGER :: numdays,iunit,ierr, hourn
INTEGER :: odate,ohour,istat,ioffset,joffset,dtout
INTEGER :: jhr,mm5hr,mm5date,nxc,nyc,nzc
INTEGER :: addday,subday,itime,zhr,zdate,izon,logunit
REAL :: xorg,yorg,dx,dy,tr,rgrndz,clonin,clatin,tlat1in,tlat2in
REAL :: dxcamx,dycamx,x0camx,y0camx,sumf,sumff
REAL :: phic,xlonc,tlat1,tlat2,alat,alon,xloc,yloc,deltax
REAL :: cellon(mxx,mxy),cellat(mxx,mxy)
REAL,allocatable :: temp_final(:,:,:)
REAL,allocatable :: par_final(:,:,:)
CHARACTER (LEN=80) :: runmsg,pgname
CHARACTER (LEN=16) :: tmcip,mm5file(10),gnamex
CHARACTER (LEN=256) :: mesg
CHARACTER (LEN=10) :: project,kvmeth
CHARACTER (LEN=16) :: oname = 'OUTFILE'
INTEGER :: envint,ios,nummm5

LOGICAL :: lstagw,lfirst,ltopo
LOGICAL :: dscgrid,mm5_met,mcip_met,mm5c_met,envyn
LOGICAL :: sat_par,mcip_rad,mm5_rad
DATA lfirst /.true./
DATA ltopo /.true./
DATA project /'LCP       '/
DATA kvmeth /'OB70       '/

DATA pgname /'met2mgn'/
DATA runmsg /'Met PAR and Temp bin2ioapi converter'/
CHARACTER (LEN=256) :: camxtp1,camxzp1,camxuv1,fname,mm5in
CHARACTER (LEN=256) :: camxtp2,camxzp2,camxuv2,camxlu

!      character*100 ifile,ifile_par,ifile_mcip

!-----Surface roughness (m) as a function of 11 landuse categories
!     and 5 seasons; based on AERMET model (ref EPA SCRAM website)

REAL :: z0lu(11,5)
DATA z0lu /1.0,0.20,0.100,1.3,1.3,1.30,0.0001,0.002,0.20,0.150,0.30,  &
    1.0,0.05,0.010,0.8,1.3,1.05,0.0001,0.002,0.20,0.030,0.30,  &
    1.0,0.05,0.010,0.8,1.3,1.05,0.0001,0.002,0.20,0.030,0.30,  &
    1.0,0.01,0.001,0.5,1.3,0.90,0.0001,0.002,0.05,0.006,0.15,  &
    1.0,0.03,0.050,1.0,1.3,1.15,0.0001,0.002,0.20,0.040,0.30/

!-----Season indices by month and latitude band
!     Season Indices            Latitude Bands
!     1 = summer                1 = <20    Tropical
!     2 = autumn                2 = 20-35  Sub-tropical
!     3 = winter w/o snow       3 = 35-50  Temperate
!     4 = winter w/ snow        4 = 50-75  Cool
!     5 = spring                5 = >75    Polar
!                    Latitude Band
INTEGER :: iseason(5,12)
DATA iseason / &
    1, 3, 3, 3, 3, &! Jan 
    1, 5, 3, 3, 3, &! Feb  
    1, 5, 5, 3, 3, &! Mar 
    1, 5, 5, 5, 3, &! Apr  
    1, 1, 5, 5, 3, &! May 
    1, 1, 1, 1, 5, &! Jun  
    1, 1, 1, 1, 1, &! Jul 
    1, 1, 1, 1, 2, &! Aug  
    1, 1, 2, 2, 3, &! Sep 
    1, 2, 2, 2, 3, &! Oct  
    1, 2, 2, 3, 3, &! Nov 
    1, 2, 3, 3, 3/  ! Dec

mm5file(1)  = 'MM5file1'
mm5file(2)  = 'MM5file2'
mm5file(3)  = 'MM5file3'
mm5file(4)  = 'MM5file4'
mm5file(5)  = 'MM5file5'
mm5file(6)  = 'MM5file6'
mm5file(7)  = 'MM5file7'
mm5file(8)  = 'MM5file8'
mm5file(9)  = 'MM5file9'
mm5file(10) = 'MM5file10'

mm5_rad  = envyn ( 'MM5RAD', mesg, .false., ios )

!-----
!-----Initialize variables

jdate = 0
jhr = 0
ioffset = -1
joffset = -1
dtout = 60

phic=ycent3d
xlonc=xcent3d
tlat1=p_alp3d
tlat2=p_bet3d
nxc=ncols3d
nyc=nrows3d
nzc=2
xorg=xorig3d/1000.
yorg=yorig3d/1000.
dx=xcell3d/1000.
dy=ycell3d/1000.
nlays3d=1

mxrec3d = 25
ftype3d = grdded3
tstep3d = 10000

fdesc3d(1) = runmsg
gdtyp3d = lamgrd3
gdnam3d = gnamex
vgtyp3d = 2
vgtop3d = 100.0        !millibars

!-----adjust starting date and time

zhr=stime         !Initialize GMT hr
zdate=sdate       !Initialize GMT date

allocate (temp_final(nxc,nyc,25))
allocate (par_final(nxc,nyc,25))

!-----Get lat/lon coords for each grid cell

DO j = 1,nyc
  yloc = yorg + dy*(FLOAT(j) - 0.5)
  DO  i = 1,nxc
    xloc = xorg + dx*(FLOAT(i) - 0.5)
    CALL lcpgeo(1,phic,xlonc,tlat1,tlat2,xloc,yloc,alon,alat)
    
    cellon(i,j) = alon
    cellat(i,j) = alat
  END DO
END DO

70   kzin(1) = 1
kzin(2) = 2
DO k = 1,2
  kz2(k) = kzin(k)
  IF (k == 1) THEN
    kz1(k) = 1
  ELSE
    kz1(k) = kz2(k-1) + 1
  END IF
END DO

dxcamx=dx
dycamx=dy
x0camx=xorg
y0camx=yorg
clonin=xlonc
clatin=phic
tlat1in=tlat1
tlat2in=tlat2
itzon=0
nummm5 = envint('numMM5','number of MM5 files',1,istat)
DO  nf=1,nummm5
  mm5in = mm5file(nf)
  CALL getenv(mm5in,fname)
  WRITE(*,*) fname
  iunit = 20 + nf
  ierr = 0
  OPEN(UNIT=iunit,FILE=fname,STATUS='old',FORM='unformatted')
  WRITE(*,*)
  WRITE(*,*)'Opened input MM5 file: ',fname
  
!-----Read raw MM5 data and convert to CAMx index/units convention
  
  100    CALL readmm5(iunit,lfirst,project,nxc,nyc,nzc,  &
      ioffset,joffset,dxcamx,dycamx,x0camx,y0camx,izone,  &
      clonin,clatin,tlat1in,tlat2in,mm5date,mm5hr,itzon,  &
      dtout,nx,ny,nz,deltax,ierr)
  
  IF (ierr == 1) CYCLE
  
  IF (mm5date < jdate .OR. (mm5date == jdate .AND. mm5hr <= jhr)) GO TO 100
  jdate = mm5date
  jhr = mm5hr
  hr = FLOAT(jhr)
  
  IF (jdate < sdate .OR. (jdate == sdate .AND. jhr < stime)) GO TO 100
  lstagw = .true.
  CALL interp_lcp(nx,ny,nz,nxc,nyc,nzc,kz1,kz2,  &
      ioffset,joffset,deltax,dxcamx,kvmeth)
  
!---------
  500  itime=hr+(itzon*100)
  idate=jdate
  IF (itime > 2300) THEN
    itime=itime-2400
    idate=addday(idate)
  END IF
  
  odate=jdate+2000000
  ohour=hr*100
  WRITE(*,800) odate, ohour
  
  IF(itime /= zhr.OR.idate /= zdate) GO TO 100
!-------
!   Determine roughness length, based on season and lat
!-------
  CALL caldate(idate)
  month = (idate - 10000*INT(idate/10000.))/100
  hourn=(zhr/100)+1
  
  DO j= 1,nyc
    DO i = 1,nxc
      temp_final(i,j,hourn)=tac(i,j,1)
      par_final(i,j,hourn)=rground(i,j)/2.
    END DO
  END DO
  
!-------
!     Write out temp (and optionally rad) to netCDF file
  
!-------
  sdate3d = 2000000+zdate  !current UTC date
  stime3d = zhr*100        !current UTC time
  xcell3d = 1000*dx
  ycell3d = 1000*dy
  xcent3d = xlonc
  ycent3d = phic
  
!----write temp to outfile
  IF(.NOT. open3(oname,fsunkn3, pgname)) THEN
    CALL m3err( 'met2mgn', sdate3d, stime3d,  &
        'Could not open or create '//oname//' file',.true.)
  END IF
  
  IF(.NOT.write3(oname,vname3d(1),sdate3d,stime3d,  &
        temp_final(:,:,hourn))) THEN
    CALL m3err( 'met2mgn', sdate3d, stime3d,  &
        'Could not write '//oname//' file', .true.)
  END IF
  
  IF (mm5_rad) THEN
    IF(.NOT.write3(oname,vname3d(2),sdate3d,stime3d,  &
          par_final(:,:,hourn))) THEN
      CALL m3err( 'met2mgn', sdate3d, stime3d,  &
          'Could not write '//oname//' file', .true.)
    END IF
  END IF
  
!-----increment hour/check end date/time
  IF(zhr == etime.AND.zdate == edate) GO TO 700
  
  zhr=zhr+100
  IF(zhr > 2300) THEN
    zhr=zhr-2400
    zdate=addday(zdate)
  END IF
  
  GO TO 100
END DO

700  deallocate (temp_final)
deallocate (par_final)
WRITE(*,*) 'Reach end date/hour; run complete'

STOP
800  FORMAT(5X,'Reading met for ',12X,i9.7,':',i6.6)
! 900  format(' nx ny nz dx dy ',3i5,2f6.2)

END SUBROUTINE procmm5
