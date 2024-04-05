!     readmcip.f
!     Read met variables from MCIP files and write
!     as variables to the netCDF file

!     Jeremiah Johnson
!     ENVIRON

!ccccccccccccccccccccccccccccccccccccc

SUBROUTINE readmcip(sdate,stime,edate,etime,r_sdate,r_stime)

USE IODECL3
USE PARMS3
USE FDESC3

INTEGER :: l,n,nx,ny,ios,isoilm,isoilt,isoiltyp
INTEGER :: jdate,jtime,sdate,stime,edate,etime,ejdate,ejtime
INTEGER :: epsdate,r_sdate,r_stime,r_jdate,r_jtime
INTEGER :: envint,istat

CHARACTER (LEN=256) :: mesg
CHARACTER (LEN=16),PARAMETER :: oname = 'OUTFILE'
CHARACTER (LEN=16),PARAMETER :: pname = 'PFILE'
CHARACTER (LEN=16),PARAMETER :: metcro2dnam1 = 'METCRO2Dfile1'
CHARACTER (LEN=16),PARAMETER :: metcro2dnam2 = 'METCRO2Dfile2'
CHARACTER (LEN=16),PARAMETER :: metcro3dnam = 'METCRO3Dfile'
CHARACTER (LEN=16),PARAMETER :: metdot3dnam = 'METDOT3Dfile'
CHARACTER (LEN=16),PARAMETER :: pgname = 'TPAR2IOAPI'
CHARACTER (LEN=16) :: tmcip

INTEGER,PARAMETER :: maxstypes = 11

LOGICAL :: mcip_rad,sat_par,envyn,initial_hour,ifsoil
REAL,allocatable ::  srad(:,:)
REAL,allocatable ::  par(:,:)
REAL,allocatable ::  reftemp(:,:)
REAL,allocatable ::  pres(:,:)
REAL,allocatable ::  soilm(:,:)
REAL,allocatable ::  soilt(:,:)
REAL,allocatable ::  soiltyp(:,:)
REAL,allocatable ::  qv(:,:)
REAL,allocatable ::  u_wind(:,:)
REAL,allocatable ::  v_wind(:,:)
REAL,allocatable ::  avewind(:,:)
REAL,allocatable ::  windspd(:,:)
REAL,allocatable ::  rn(:,:)
REAL,allocatable ::  rc(:,:)
REAL,allocatable ::  rain(:,:,:)
REAL,allocatable ::  rain_acc24(:,:,:)
REAL,allocatable ::  ptype(:,:)
REAL,allocatable ::  pulsedate(:,:)
REAL,allocatable ::  pulsetime(:,:)
REAL,allocatable ::  prec_adj(:,:)
!      real, allocatable::  precip_adjfac(:,:)
INTEGER :: pultype,puldate,pultime,soilcat
REAL :: precip_adjfac

REAL,EXTERNAL :: precipfact
INTEGER,EXTERNAL :: pulsetype


!-----get variables from output file
nx = ncols3d
ny = nrows3d

istime = stime/100
ietime = etime/100
IF (ietime == 0) THEN
  ietime = ietime+24
END IF
jdate = sdate+2000000
jtime = stime*100
ejdate = 2000000+edate
ejtime = etime*100

r_jdate = r_sdate+2000000
r_jtime = r_stime*100

CALL getenv ('TMCIP',tmcip)

mesg = 'Coordinate name: '
CALL envstr( 'GDNAM3D', mesg, 'TX_36km', gdnam3d, ios )
mcip_rad = envyn( 'MCIPRAD', mesg, .false., ios )
sat_par  = envyn ( 'SATPAR', mesg, .false., ios )

epsdate = envint('EPISODE_SDATE','Episode Start Date',0,istat)
IF (istat > 0) THEN
  mesg = 'Bad value for EPISODE_SDATE'
  CALL m3exit('met2mgn',0,0,mesg,2)
END IF

ifsoil = .true.

!-----open MCIP files

IF(.NOT. open3(metcro2dnam1,fsread3,pgname)) THEN
  CALL m3err( 'readmcip', sdate3d, stime3d,  &
      'Could not open or create '//metcro2dnam1//' file',.true.)
ELSE IF(.NOT. desc3(metcro2dnam1)) THEN
  CALL m3err( 'readmcip', sdate3d, stime3d,  &
      'Could not get description for '//metcro2dnam1//' file',.true.)
END IF

isoilm = index1('SOIM1',nvars3d,vname3d)
isoilt = index1('SOIT1',nvars3d,vname3d)
isoiltyp = index1('SLTYP',nvars3d,vname3d)
IF (isoilm <= 0 .OR. isoilt <= 0 .OR. isoiltyp <= 0) THEN
  ifsoil = .false.
END IF

IF(r_sdate /= sdate) THEN
  IF(.NOT. open3(metcro2dnam2,fsread3,pgname)) THEN
    CALL m3err( 'readmcip', sdate3d, stime3d,  &
        'Could not open or create '//metcro2dnam2//' file',.true.)
  END IF
  IF(.NOT. open3(pname,fsread3,pgname)) THEN
    CALL m3err( 'readmcip', sdate3d, stime3d,  &
        'Could not open or create '//pname//' file',.true.)
  END IF
END IF

IF(.NOT. open3(metcro3dnam,fsread3,pgname)) THEN
  CALL m3err( 'readmcip', sdate3d, stime3d,  &
      'Could not open or create '//metcro3dnam//' file',.true.)
END IF

IF(.NOT. open3(metdot3dnam,fsread3,pgname)) THEN
  CALL m3err( 'readmcip', sdate3d, stime3d,  &
      'Could not open or create '//metdot3dnam//' file',.true.)
END IF

allocate (reftemp(nx,ny))
allocate (pres(nx,ny))
allocate (soilm(nx,ny))
allocate (soilt(nx,ny))
allocate (soiltyp(nx,ny))
allocate (qv(nx,ny))
allocate (u_wind(nx+1,ny+1))
allocate (v_wind(nx+1,ny+1))
allocate (avewind(nx+1,ny+1))
allocate (windspd(nx,ny))
IF(mcip_rad) THEN
  allocate (srad(nx,ny))
  allocate (par(nx,ny))
END IF
allocate (rn(nx,ny))
allocate (rc(nx,ny))
allocate (rain(nx,ny,100))
allocate (rain_acc24(nx,ny,100))
allocate (ptype(nx,ny))
allocate (pulsedate(nx,ny))
allocate (pulsetime(nx,ny))
allocate (prec_adj(nx,ny))

!---Store total precip (RN+RC) in array starting from 23 hours prior to
!   target start time and ending at the target end time

DO n=1,100
  
!-------Target start time already reached
  IF(r_jdate >= jdate.AND.r_jtime >= jtime) THEN
    IF (r_sdate == sdate) THEN
      
!---First day (only one MCIP file to read in)
      
      IF (.NOT.read3(metcro2dnam1,'RC',1,r_jdate,r_jtime,rc)) THEN
        CALL m3err( 'readmcip', r_jdate, r_jtime,  &
            'Could not read RC from '//metcro2dnam1//' file', .true.)
      END IF
      IF (.NOT.read3(metcro2dnam1,'RN',1,r_jdate,r_jtime,rn)) THEN
        CALL m3err( 'readmcip', r_jdate, r_jtime,  &
            'Could not read RC from '//metcro2dnam1//' file', .true.)
      END IF
    ELSE
      
!---Past first day (read in second MCIP file)
      
      IF (.NOT.read3(metcro2dnam2,'RC',1,r_jdate,r_jtime,rc)) THEN
        CALL m3err( 'readmcip', r_jdate, r_jtime,  &
            'Could not read RC from '//metcro2dnam1//' file', .true.)
      END IF
      IF (.NOT.read3(metcro2dnam2,'RN',1,r_jdate,r_jtime,rn)) THEN
        CALL m3err( 'readmcip', r_jdate, r_jtime,  &
            'Could not read RC from '//metcro2dnam1//' file', .true.)
      END IF
    END IF
    
!-------Before target start time
  ELSE
    IF (.NOT.read3(metcro2dnam1,'RC',1,r_jdate,r_jtime,rc)) THEN
!            call m3err( 'readmcip', r_jdate, r_jtime,
!     &           'Could not read RC at '//METCRO2Dnam1//' file', .TRUE.)
      mesg = 'Could not read RC from '//metcro2dnam1//' file'
      CALL m3warn('readmcip', r_jdate, r_jtime, mesg)
    END IF
    IF (.NOT.read3(metcro2dnam1,'RN',1,r_jdate,r_jtime,rn)) THEN
!           call m3err( 'readmcip', r_jdate, r_jtime,
!     &           'Could not read RC at '//METCRO2Dnam1//' file', .TRUE.)
      mesg = 'Could not read RN from '//metcro2dnam1//' file'
      CALL m3warn('readmcip', r_jdate, r_jtime, mesg)
    END IF
  END IF
  
  DO j=1,ny
    DO i=1,nx
      rain(i,j,n)=rn(i,j)+rc(i,j)
    END DO
  END DO
  
  IF(r_jtime == ejtime.AND.r_jdate == ejdate) THEN
    deallocate(rc)
    deallocate(rn)
    GO TO 700
  END IF
  CALL nextime(r_jdate, r_jtime, 10000)
END DO

700   DO n=1,100
  
  IF(r_sdate == sdate) THEN  !First day
    IF (.NOT.read3(metcro2dnam1,tmcip,1,jdate,jtime, reftemp)) THEN
      CALL m3err( 'readmcip', jdate, jtime,  &
          'Could not read '//metcro2dnam1//' file', .true.)
    END IF
    
    IF (ifsoil) THEN
      IF (.NOT.read3(metcro2dnam1,'SOIM1',1,jdate,jtime,soilm)) THEN
        mesg = 'Could not read SOIM1 from '//metcro2dnam1//' file'
        CALL m3mesg(mesg)
        ifsoil = .false.
      END IF
      
      IF (.NOT.read3(metcro2dnam1,'SOIT1',1,jdate,jtime,soilt)) THEN
        mesg = 'Could not read SOIT1 from '//metcro2dnam1//' file'
        CALL m3mesg(mesg)
        ifsoil = .false.
      END IF
      
      IF (.NOT.read3(metcro2dnam1,'SLTYP',1,jdate,jtime,soiltyp)) THEN
        mesg = 'Could not read SLTYP from '//metcro2dnam1//' file'
        CALL m3mesg(mesg)
        ifsoil = .false.
      END IF
    END IF
    
    IF(mcip_rad) THEN
      IF (.NOT.read3(metcro2dnam1,'RGRND',1,jdate,jtime,srad)) THEN
        CALL m3err( 'readmcip', jdate, jtime,  &
            'Could not read '//metcro2dnam1//' file', .true.)
      END IF
      par(:,:)=srad(:,:)/2.
    END IF
    
  ELSE  !Not first day
    
!-----If first hour of the day (and not the first day of the episode), read in
!     pulse variables from current hour (last hour of previous day should
!     be the same as first hour of current day)
    
    IF((sdate+2000000) == jdate.AND.(stime*100) == jtime) THEN
      lfirst = .true.
    ELSE
      lfirst = .false.
    END IF
    IF (lfirst) THEN
      IF (.NOT.read3(pname,'PTYPE',1,jdate,jtime, ptype)) THEN
        CALL m3err( 'readmcip', jdate, jtime,  &
            'Could not read '//pname//' file', .true.)
      END IF
      IF (.NOT.read3(pname,'PULSEDATE',1,jdate,jtime, pulsedate)) THEN
        CALL m3err( 'readmcip', jdate, jtime,  &
            'Could not read '//pname//' file', .true.)
      END IF
      IF (.NOT.read3(pname,'PULSETIME',1,jdate,jtime, pulsetime)) THEN
        CALL m3err( 'readmcip', jdate, jtime,  &
            'Could not read '//pname//' file', .true.)
      END IF
      IF (.NOT.close3(pname)) THEN
        CALL m3err( 'readmcip', jdate, jtime,  &
            'Could not clsoe '//pname//' file', .true.)
      END IF
    END IF
    IF (.NOT.read3(metcro2dnam2,tmcip,1,jdate,jtime, reftemp)) THEN
      CALL m3err( 'readmcip', jdate, jtime,  &
          'Could not read '//metcro2dnam2//' file', .true.)
    END IF
    
    IF (ifsoil) THEN
      IF (.NOT.read3(metcro2dnam2,'SOIM1',1,jdate,jtime,soilm)) THEN
        mesg = 'Could not read SOIM1 from '//metcro2dnam1//' file'
        CALL m3mesg(mesg)
        ifsoil = .false.
      END IF
      
      IF (.NOT.read3(metcro2dnam2,'SOIT1',1,jdate,jtime,soilt)) THEN
        mesg = 'Could not read SOIT1 from '//metcro2dnam1//' file'
        CALL m3mesg(mesg)
        ifsoil = .false.
      END IF
      
      IF (.NOT.read3(metcro2dnam2,'SLTYP',1,jdate,jtime,soiltyp)) THEN
        mesg = 'Could not read SLTYP from '//metcro2dnam1//' file'
        CALL m3mesg(mesg)
        ifsoil = .false.
      END IF
    END IF
    
    IF(mcip_rad) THEN
      IF (.NOT.read3(metcro2dnam2,'RGRND',1,jdate,jtime,srad)) THEN
        CALL m3err( 'readmcip', jdate, jtime,  &
            'Could not read '//metcro2dnam2//' file', .true.)
      END IF
      par(:,:)=srad(:,:)/2.
    END IF
  END IF
  
  IF (.NOT.read3(metcro3dnam,'PRES',1,jdate,jtime,pres)) THEN
    CALL m3err( 'readmcip', jdate, jtime,  &
        'Could not read '//metcro3dnam//' file', .true.)
  END IF
  
  IF (.NOT.read3(metcro3dnam,'QV',1,jdate,jtime,qv)) THEN
    CALL m3err( 'readmcip', jdate, jtime,  &
        'Could not read '//metcro3dnam//' file', .true.)
  END IF
  
  IF (.NOT.read3(metdot3dnam,'UWIND',1,jdate,jtime,u_wind)) THEN
    CALL m3err( 'readmcip', jdate, jtime,  &
        'Could not read '//metdot3dnam//' file', .true.)
  END IF
  
  IF (.NOT.read3(metdot3dnam,'VWIND',1,jdate,jtime,v_wind)) THEN
    CALL m3err( 'readmcip', jdate, jtime,  &
        'Could not read '//metdot3dnam//' file', .true.)
  END IF
  
  
!----Calculate 24-hr rain accumulation
  
  IF(r_sdate == sdate) THEN
    DO l=1,n
      DO j=1,ny
        DO i=1,nx
          rain_acc24(i,j,n)=rain_acc24(i,j,n)+rain(i,j,l)
        END DO
      END DO
    END DO
  ELSE
    DO l=n,n+23,1
      DO j=1,ny
        DO i=1,nx
          rain_acc24(i,j,n)=rain_acc24(i,j,n)+rain(i,j,l)
        END DO
      END DO
    END DO
  END IF
  
!---Calculate adjustment to precip from "pulse"
  
  IF((r_sdate+2000000) == jdate.AND.(r_stime*100) == jtime) THEN
    initial_hour = .true.
  ELSE
    initial_hour = .false.
  END IF
  
!-----If initial hour (first hour of first day of episode),
!     then set pulse variables to zero
  
!      if(r_sdate.eq.sdate) then  !first day
  IF(initial_hour) THEN !first day *and* first hour
    DO j=1,ny
      DO i=1,nx
        ptype(i,j)=0.
        pulsedate(i,j)=0.
        pulsetime(i,j)=0.
        soilcat=soiltyp(i,j)
        IF(isoil) THEN
          IF( soilcat > 0 .AND. soilcat <= maxstypes ) THEN
            prec_adj(i,j) = 2.
          ELSE
            prec_adj(i,j) = 1.
          END IF
        ELSE
          prec_adj(i,j)=1.
        END IF
      END DO
    END DO
  ELSE              !first day but not first hour
    
!-----Calculate adjustment factor for all other hours
    
    DO j=1,ny
      DO i=1,nx
        pultype=ptype(i,j)
        puldate=pulsedate(i,j)
        pultime=pulsetime(i,j)
        IF(pulsetype(rain_acc24(i,j,n)) <= pultype) THEN
! no new rain
          precip_adjfac=precipfact(pultype,jdate,jtime, puldate,pultime)
        ELSE
! rain
          puldate = jdate
          pultime = jtime
          pultype = pulsetype(rain_acc24(i,j,n))
          precip_adjfac=precipfact(pultype,jdate,jtime, puldate,pultime)
        END IF
        ptype(i,j)=pultype
        pulsedate(i,j)=puldate
        pulsetime(i,j)=pultime
        prec_adj(i,j)=precip_adjfac
      END DO
    END DO
  END IF
  
!----Interpolate winds at cell corners to cell centers
  
  avewind(:,:)=SQRT(u_wind(:,:)**2+v_wind(:,:)**2)
  
  DO j=1,ny
    DO i=1,nx
      windspd(i,j)=(avewind(i,j)+avewind(i,j+1)+avewind(i+1,j)  &
          +avewind(i+1,j+1))/4.
    END DO
  END DO
  
  sdate3d=sdate+2000000
  stime3d=stime*100
  nlays3d=1
  nthik3d=1
  mxrec3d=0
  
!-----Initialize and define output variables and attributes
  IF (ifsoil) THEN
    nvars3d = 10
  ELSE
    nvars3d = 7
  END IF
  
  IF (ifsoil) THEN
    vname3d(1) =  'SOIM1           '
    vtype3d(1) =  m3real
    units3d(1) =  'M**3/M**3       '
    vdesc3d(1) =  'volumetric soil moisture in top cm'
    
    vname3d(2) =  'SOIT1           '
    vtype3d(2) =  m3real
    units3d(2) =  'K               '
    vdesc3d(2) =  'soil temperature in top cm'
    
    vname3d(3) =  'SLTYP           '
    vtype3d(3) =  m3real
    units3d(3) =  'CATEGORY        '
    vdesc3d(3) =  'soil texture type by USDA category'
  END IF
  
  vname3d(nvars3d-6) =  'TEMP2           '
  vtype3d(nvars3d-6) =  m3real
  units3d(nvars3d-6) =  'K               '
  
  IF (tmcip == 'TEMP1P5') THEN
    vdesc3d(nvars3d-6) =  'temperature at 1.5 m'
  ELSE
    vdesc3d(nvars3d-6) =  'temperature at 2 m'
  END IF
  
  vname3d(nvars3d-5) =  'PRES            '
  vtype3d(nvars3d-5) =  m3real
  units3d(nvars3d-5) =  'Pa              '
  vdesc3d(nvars3d-5) =  'pressure        '
  
  vname3d(nvars3d-4) =  'QV              '
  vtype3d(nvars3d-4) =  m3real
  units3d(nvars3d-4) =  'KG/KG           '
  vdesc3d(nvars3d-4) =  'water vapor mixing ratio'
  
  vname3d(nvars3d-3) =  'WINDSPD         '
  vtype3d(nvars3d-3) =  m3real
  units3d(nvars3d-3) =  'm/s             '
  vdesc3d(nvars3d-3) =  'Cell centered Windspeed'
  
  vname3d(nvars3d-2) =  'RAIN_ACC24       '
  vtype3d(nvars3d-2) =  m3real
  units3d(nvars3d-2) =  'cm               '
  vdesc3d(nvars3d-2) =  '24-hour accumulated rain'
  
  vname3d(nvars3d-1) =  'PREC_ADJ         '
  vtype3d(nvars3d-1) =  m3real
  units3d(nvars3d-1) =  'No dimension     '
  vdesc3d(nvars3d-1) =  'Precip adjustment factor'
  
  vname3d(nvars3d) =  'PAR             '
  vtype3d(nvars3d) =  m3real
  units3d(nvars3d) =  'WATTS/M**2      '
  vdesc3d(nvars3d) =  'Photosynthetically Active Radiation'
  
  IF(.NOT. open3(oname,fsunkn3, pgname)) THEN
    CALL m3err( 'readmcip', sdate3d, stime3d,  &
        'Could not open or create '//oname//' file',.true.)
  END IF
  
  IF (ifsoil) THEN
    IF(.NOT.write3(oname,vname3d(1),jdate,jtime,soilm)) THEN
      CALL m3err( 'readmcip', jdate, jtime,  &
          'Could not write '//oname//' file', .true.)
    END IF
    
    IF(.NOT.write3(oname,vname3d(2),jdate,jtime,soilt)) THEN
      CALL m3err( 'readmcip', jdate, jtime,  &
          'Could not write '//oname//' file', .true.)
    END IF
    
    IF(.NOT.write3(oname,vname3d(3),jdate,jtime,soiltyp)) THEN
      CALL m3err( 'readmcip', jdate, jtime,  &
          'Could not write '//oname//' file', .true.)
    END IF
  END IF
  
  IF(.NOT.write3(oname,vname3d(nvars3d-6),jdate,jtime, reftemp)) THEN
    CALL m3err( 'readmcip', jdate, jtime,  &
        'Could not write '//oname//' file', .true.)
  END IF
  
  IF(.NOT.write3(oname,vname3d(nvars3d-5),jdate,jtime,pres)) THEN
    CALL m3err( 'readmcip', jdate, jtime,  &
        'Could not write '//oname//' file', .true.)
  END IF
  
  IF(.NOT.write3(oname,vname3d(nvars3d-4),jdate,jtime,qv)) THEN
    CALL m3err( 'readmcip', jdate, jtime,  &
        'Could not write '//oname//' file', .true.)
  END IF
  
  IF(.NOT.write3(oname,vname3d(nvars3d-3),jdate,jtime,windspd)) THEN
    CALL m3err( 'readmcip', jdate, jtime,  &
        'Could not write '//oname//' file', .true.)
  END IF
  
  IF(.NOT.write3(oname,vname3d(nvars3d-2),jdate,jtime,  &
        rain_acc24(:,:,n))) THEN
    CALL m3err( 'readmcip', jdate, jtime,  &
        'Could not write '//oname//' file', .true.)
  END IF
  
  IF(.NOT.write3(oname,vname3d(nvars3d-1),jdate,jtime, prec_adj)) THEN
    CALL m3err( 'readmcip', jdate, jtime,  &
        'Could not write '//oname//' file', .true.)
  END IF
  
  IF(mcip_rad) THEN
    IF(.NOT.write3(oname,vname3d(nvars3d),jdate,jtime,par)) THEN
      CALL m3err('readmcip', jdate, jtime,  &
          'Could not write '//oname//' file', .true.)
      
    END IF
  END IF
  
!---Write pulse variables to temporary file (PNAME)
  
  IF(jtime == ejtime.AND.jdate == ejdate) THEN
    
    IF (sat_par) THEN
      CALL readpar(sdate,stime,edate,etime)
    END IF
    
    nvars3d = 3
    vname3d(1) =  'PTYPE           '
    vtype3d(1) =  m3real
    units3d(1) =  'No dimension     '
    vdesc3d(1) =  'type of NO pulse        '
    
    vname3d(2) = 'PULSEDATE        '
    vtype3d(2) =  m3real
    units3d(2) =  'No dimension     '
    vdesc3d(2) =  'Date of last NO pulse'
    
    vname3d(3) = 'PULSETIME        '
    vtype3d(3) =  m3real
    units3d(3) =  'No dimension            '
    vdesc3d(3) =  'Time of last NO pulse'
    sdate3d=jdate
    stime3d=jtime
    mxrec3d=1
    
!--------Write out last hour's pulse vars to intermediate file
    
    IF(.NOT. open3(pname,fscrea3, pgname)) THEN
      CALL m3err( 'readmcip', sdate3d, stime3d,  &
          'Could not open or create '//pname//' file',.true.)
    END IF
    IF(.NOT.write3(pname,vname3d(1),jdate,jtime, ptype)) THEN
      CALL m3err( 'readmcip', jdate, jtime,  &
          'Could not write '//pname//' file', .true.)
    END IF
    IF(.NOT.write3(pname,vname3d(2),jdate,jtime, pulsedate)) THEN
      CALL m3err( 'readmcip', jdate, jtime,  &
          'Could not write '//pname//' file', .true.)
    END IF
    IF(.NOT.write3(pname,vname3d(3),jdate,jtime, pulsetime)) THEN
      CALL m3err( 'readmcip', jdate, jtime,  &
          'Could not write '//pname//' file', .true.)
    END IF
    
    deallocate(reftemp)
    deallocate(pres)
    deallocate(soilm)
    deallocate(soilt)
    deallocate(soiltyp)
    deallocate(qv)
    deallocate(u_wind)
    deallocate(v_wind)
    deallocate(windspd)
    deallocate(rain_acc24)
    deallocate(ptype)
    deallocate(pulsedate)
    deallocate(pulsetime)
    deallocate(prec_adj)
    IF(mcip_rad) THEN
      deallocate(srad)
      deallocate(par)
    END IF
    GO TO 999
  END IF
  CALL nextime(jdate, jtime, 10000)
END DO
999  END


REAL FUNCTION precipfact( pultype, jdate, jtime, adate, atime )

!***********************************************************************
!  DESCRIPTION
!    This internal function computes a precipitation adjustment
!    factor from YL 1995 based on a rain rate. The pulse type is
!    and integer ranging from 0 to 3 indicating the type of
!    rainfall rate.

!  CALL
!    SECSDIFF  -  IOAPI

!  HISTORY:
!    07/21/11 : Imported from SMOKE-BEIS v3.14 and modified  (Tan)
!***********************************************************************


IMPLICIT NONE

!...  Function arguments
INTEGER,intent(in out) :: pultype
INTEGER,intent(in) :: jdate
INTEGER,intent(in) :: jtime
INTEGER,intent(in) :: adate
INTEGER,intent(in) :: atime

!...  External functions
INTEGER, EXTERNAL :: secsdiff

!...  Local variables
INTEGER :: hrdiff

!-----------------------------------------------------------------------------

hrdiff = secsdiff( adate, atime, jdate, jtime ) / 3600.

select case( pultype )
case( 0 )
precipfact = 1.
case( 1 )
IF( ( hrdiff / 24. ) < 2. ) THEN
  precipfact = 11.19 * EXP(-0.805*(hrdiff+24)/24.)
ELSE
  pultype = 0
  precipfact = 1.
END IF
case( 2 )
IF( ( hrdiff / 24. ) < 6. ) THEN
  precipfact = 14.68 * EXP(-0.384*(hrdiff+24)/24.)
ELSE
  pultype = 0
  precipfact = 1.
END IF
case default
IF( ( hrdiff / 24. ) < 13. ) THEN
  precipfact = 18.46 * EXP(-0.208*(hrdiff+24)/24.)
ELSE
  pultype = 0
  precipfact = 1.
END IF
END select

RETURN

END FUNCTION precipfact
!=======================================================================
!=======================================================================


!=======================================================================
!=======================================================================
INTEGER FUNCTION pulsetype( rrate )

!.....This internal function computes the pulse type from a rainfall rate.
!     (See YL 1995).

IMPLICIT NONE

!.....Function arguments
REAL,intent(in) :: rrate

!-----------------------------------------------------------------------
IF( rrate < 0.1 ) THEN
  pulsetype = 0
ELSE IF( rrate < 0.5 ) THEN
  pulsetype = 1
ELSE IF( rrate < 1.5 ) THEN
  pulsetype = 2
ELSE
  pulsetype = 3
END IF
RETURN
END FUNCTION pulsetype
