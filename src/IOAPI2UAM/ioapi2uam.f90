PROGRAM ioapi2uam
USE PARMS3
USE IODECL3
USE FDESC3
IMPLICIT NONE
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!   Copyright (C) 2007-2007  ENVIRON


!   This program is free software; you can redistribute it and/or
!   modify it under the terms of the GNU General Public License
!   as published by the Free Software Foundation; either version 2
!   of the License, or (at your option) any later version.

!   This program is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.

!   To obtain a copy of the GNU General Public License
!   write to the Free Software Foundation, Inc.,
!   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.


!   For comments and questions, send to bkoo@environcorp.com

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!     IOAPI2UAM converts CMAQ 1-D emissions files (I/O API) to CAMx
!     low-level emissions files (UAM-IV). It only converts file format
!     with no species mapping. If a CMAQ variable name is longer than
!     10 characters, it truncates the name. Emission rate is converted
!     from mol/s (or g/s) to mol/hr (or g/hr). It also shifts time-zone
!     from GMT to user-selected local time.

!     INPUT ENVIRONMENTAL VARIABLES:
!       INFILE1       - Logical name for input file 1 (current day)
!       INFILE2       - Logical name for input file 2 (next day)
!                       Required only if additional data is needed
!                       due to time zone shifting
!                       Map projection consistency won't be checked
!       OUTFILE       - Logical name for output file
!       TZONE         - Output time zone (8 for PST, etc.)
!       SDATE         - Output start date (YYJJJ)
!       STIME         - Output start time in TZONE (HHMMSS)
!       RLENG         - Output run length (HHMMSS)
!                       240000 for a CAMx daily emissions input

!     HISTORY:
!       created by bkoo (06/20/2007)

INTEGER :: logunit
INTEGER :: envint,time2sec,jstep3
INTEGER :: jdate,jtime,cdate,ctime,rleng
INTEGER,PARAMETER :: munit = 99

CHARACTER (LEN=16) , PARAMETER :: infile1 = 'INFILE1'
CHARACTER (LEN=16) , PARAMETER :: infile2 = 'INFILE2'
CHARACTER (LEN=16) , PARAMETER :: outfile = 'OUTFILE'
CHARACTER (LEN=16) , PARAMETER :: pgname  = 'IOAPI2UAM'
CHARACTER (LEN=16)  :: infile
CHARACTER (LEN=256) :: mesg

CHARACTER (LEN=4) , DIMENSION(10) :: NAME
CHARACTER (LEN=4) , DIMENSION(60) :: note
INTEGER :: iutm,nspec,ibdate,iedate,nx,ny,nz
REAL :: btime,etime,xorg,yorg,delx,dely
INTEGER,PARAMETER :: nseg = 1, iseg = 1, ixseg = 0, iyseg = 0
INTEGER,PARAMETER :: nzlowr = 0, nzuppr = 0
REAL,PARAMETER :: refx = 0.0, refy = 0.0
REAL,PARAMETER :: htsur = 0.0, htlow = 0.0, htupp = 0.0
REAL,PARAMETER :: sec2hr = 3600.
CHARACTER (LEN=10) , PARAMETER :: namecamx = 'EMISSIONS '
CHARACTER (LEN=10)  :: tmpnam

CHARACTER (LEN=4), allocatable :: mspec(:,:)
REAL,allocatable :: buff(:,:,:,:), em(:,:,:,:)
INTEGER :: istat

INTEGER :: itzone,nstep,jchk,jstep
INTEGER :: i,j,l,n

!     Initialize I/O-API

logunit = init3()

!     Open input file 1

infile = infile1
IF (.NOT.open3(infile,fsread3,pgname)) THEN
  mesg = 'Cannot open ' // trim(infile)
  CALL m3exit(pgname,0,0,mesg,1)
END IF

!     Get description of input file 1

IF (.NOT.desc3(infile)) THEN
  mesg = 'Cannot get description of ' // trim(infile)
  CALL m3exit(pgname,0,0,mesg,1)
END IF

!     Check header info

IF (gdtyp3d /= lamgrd3 .AND. gdtyp3d /= utmgrd3 .AND.  &
      gdtyp3d /= latgrd3 .AND. gdtyp3d /= polgrd3) THEN
  mesg = 'Grid type of ' // trim(infile) // ' is not supported'
  CALL m3exit(pgname,0,0,mesg,2)
END IF
iutm = 0
IF (gdtyp3d == utmgrd3) iutm = INT(p_alp3d)
IF (ftype3d /= grdded3) THEN
  mesg = 'Data type of ' // trim(infile) // ' must be GRDDED3'
  CALL m3exit(pgname,0,0,mesg,2)
END IF

!     Open output file

CALL nameval( outfile, mesg )
OPEN(munit,FILE=mesg,STATUS='NEW',FORM='UNFORMATTED')

!     Write output header

DO n = 1, 10
  NAME(n) = namecamx(n:n)
END DO
DO n = 1, 60
  note(n) = fdesc3d(1)(n:n) ! First line of CMAQ file description
END DO

itzone = envint('TZONE','Output Time Zone',0,istat)
IF (istat > 0) THEN
  mesg = 'Bad value for TZONE'
  CALL m3exit(pgname,0,0,mesg,2)
END IF
jdate = envint('SDATE','Output Start Date',sdate3d,istat)
IF (istat > 0) THEN
  mesg = 'Bad value for SDATE'
  CALL m3exit(pgname,0,0,mesg,2)
END IF
jtime = envint('STIME','Output Start Time',stime3d,istat)
IF (istat > 0) THEN
  mesg = 'Bad value for STIME'
  CALL m3exit(pgname,0,0,mesg,2)
END IF
rleng = envint('RLENG','Output Run Length',240000,istat)
IF (istat > 0) THEN
  mesg = 'Bad value for RLENG'
  CALL m3exit(pgname,0,0,mesg,2)
END IF

cdate = jdate
ctime = jtime

ibdate = MOD(cdate,100000)
btime = REAL(ctime/10000) + REAL(time2sec(MOD(ctime,10000)))/3600.

CALL nextime( cdate, ctime, rleng )

iedate = MOD(cdate,100000)
etime = REAL(ctime/10000) + REAL(time2sec(MOD(ctime,10000)))/3600.

WRITE(*,'(/,A)')'Output period (start date/time & end date/time):'
WRITE(*,'(2(i,f))') ibdate,btime,iedate,etime

nstep = rleng / tstep3d
xorg  = xorig3d
yorg  = yorig3d
delx  = xcell3d
dely  = ycell3d
nx    = ncols3d
ny    = nrows3d
nz    = 1       ! 1-D emissions
nspec = nvars3d

allocate (mspec(10,nspec), stat = istat)
IF (istat /= 0) THEN
  mesg = 'Memory allocation failed: MSPEC'
  CALL m3exit(pgname,0,0,mesg,2)
END IF

DO l = 1, nspec
  READ(vname3d(l),'(10a1)') (mspec(n,l),n=1,10)
END DO

WRITE(munit)NAME,note,nseg,nspec,ibdate,btime,iedate,etime
WRITE(munit)refx,refy,iutm,xorg,yorg,delx,dely,nx,ny,nz,  &
    nzlowr,nzuppr,htsur,htlow,htupp
WRITE(munit)ixseg,iyseg,nx,ny
WRITE(munit)((mspec(n,l),n=1,10),l=1,nspec)

!     Allocate buffer memnory

allocate (buff(nx,ny,nz,nspec), stat = istat)
IF (istat /= 0) THEN
  mesg = 'Memory allocation failed: BUFF'
  CALL m3exit(pgname,0,0,mesg,2)
END IF
allocate (em(nx,ny,nz,nspec), stat = istat)
IF (istat /= 0) THEN
  mesg = 'Memory allocation failed: EM'
  CALL m3exit(pgname,0,0,mesg,2)
END IF

!     Read/write time-dependent data

cdate = jdate
ctime = jtime
CALL nextime( cdate, ctime, itzone*10000 )

jchk = jstep3(cdate,ctime,sdate3d,stime3d,tstep3d)
IF ( jchk < 1 .OR. jchk > mxrec3d ) THEN
  mesg = 'Cannot find start date/time in ' // trim(infile)
  CALL m3exit(pgname,cdate,ctime,mesg,2)
END IF
!cc      write(*,'(a,2i)')'Reading ',CDATE,CTIME
IF (.NOT.read3(infile,allvar3,1,cdate,ctime,em)) THEN
  mesg = 'Cannot read data from ' // trim(infile)
  CALL m3exit(pgname,cdate,ctime,mesg,1)
END IF

DO jstep = 1, nstep
  
  CALL nextime( cdate, ctime, tstep3d )
  jchk = jstep3(cdate,ctime,sdate3d,stime3d,tstep3d)
  IF ( jchk < 1 .OR. jchk > mxrec3d ) THEN
    IF (infile == infile2) THEN
      mesg = 'Cannot find the following date/time in ' // trim(infile)
      CALL m3exit(pgname,cdate,ctime,mesg,2)
    END IF
    
!     Open input file 2
    
    infile = infile2
    IF (.NOT.open3(infile,fsread3,pgname)) THEN
      mesg = 'Cannot open ' // trim(infile)
      CALL m3exit(pgname,0,0,mesg,1)
    END IF
    
!     Get description of input file 2
    
    IF (.NOT.desc3(infile)) THEN
      mesg = 'Cannot get description of ' // trim(infile)
      CALL m3exit(pgname,0,0,mesg,1)
    END IF
    
!     Check file type and dimensions
    
    IF (.NOT.filchk3(infile,grdded3,nx,ny,allays3,1)) THEN
      mesg = 'Inconsistent file type and dimension between ' //  &
          trim(infile) // ' and ' // trim(infile1)
      CALL m3exit(pgname,0,0,mesg,2)
    END IF
    
!     Check species order
    
    IF (nvars3d /= nspec) THEN
      mesg = 'Different number of species between ' //  &
          trim(infile) // ' and ' // trim(infile1)
      CALL m3exit(pgname,0,0,mesg,2)
    END IF
    DO l = 1, nspec
      WRITE(tmpnam,'(10a1)') (mspec(n,l),n=1,10)
      IF (vname3d(l)(1:10) /= tmpnam) THEN
        WRITE(*,'(/,A)') ' No. OUTFILE     INFILE2 (first 10 characters only)'
        DO i = 1, nspec
          WRITE(*,'(i3,2x,10a1,2x,a10)') i,(mspec(n,i),n=1,10), vname3d(i)
        END DO
        mesg = 'Inconsistent species list between ' //  &
            trim(infile) // ' and ' // trim(infile1)
        CALL m3exit(pgname,0,0,mesg,2)
      END IF
    END DO
  END IF ! jchk
  
!cc        write(*,'(a,2i)')'Reading ',CDATE,CTIME
  IF (.NOT.read3(infile,allvar3,1,cdate,ctime,buff)) THEN
    mesg = 'Cannot read data from ' // trim(infile)
    CALL m3exit(pgname,cdate,ctime,mesg,1)
  END IF
  
  em = 0.5 * ( em + buff ) * sec2hr ! hourly averages (unit conversion)
  
  ibdate = MOD(jdate,100000)
  btime = REAL(jtime/10000) + REAL(time2sec(MOD(jtime,10000)))/3600.
  
  CALL nextime( jdate, jtime, tstep3d )
  
  iedate = MOD(jdate,100000)
  etime = REAL(jtime/10000) + REAL(time2sec(MOD(jtime,10000)))/3600.
  
  WRITE(munit) ibdate,btime,iedate,etime
  WRITE(*,'(a,2(i,f))')'Writing ',ibdate,btime,iedate,etime
  
  DO l = 1, nspec
    WRITE(munit) iseg,(mspec(n,l),n=1,10), ((em(i,j,1,l),i=1,nx),j=1,ny)
  END DO
  
  em = buff
  
END DO

!     Close output files

CLOSE(munit)

mesg = 'Successful completion of ' // pgname
CALL m3exit(pgname,0,0,mesg,0)

END PROGRAM ioapi2uam

