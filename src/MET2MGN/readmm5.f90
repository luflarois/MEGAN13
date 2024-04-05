SUBROUTINE readmm5(iomm5,lfirst,project,nxc,nyc,nzc,  &
        ioffset,joffset,dxcamx,dycamx,x0camx,y0camx,  &
        izone,clonin,clatin,tlat1in,tlat2in,mm5date,  &
        mm5hr,itzon,dtout,nx,ny,nz,deltax,ierr)
USE param
USE fields 
IMPLICIT NONE

!-----READMM5 reads hourly MM5 data from file, and converts gridded data into
!     conventions for CAMx


INTEGER, INTENT(IN OUT)                  :: iomm5
LOGICAL, INTENT(OUT)                     :: lfirst
CHARACTER (LEN=10), INTENT(OUT)          :: project
INTEGER, INTENT(IN)                      :: nxc
INTEGER, INTENT(IN)                      :: nyc
INTEGER, INTENT(IN OUT)                  :: nzc
INTEGER, INTENT(OUT)                     :: ioffset
INTEGER, INTENT(OUT)                     :: joffset
REAL, INTENT(IN OUT)                     :: dxcamx
REAL, INTENT(IN OUT)                     :: dycamx
REAL, INTENT(IN)                         :: x0camx
REAL, INTENT(IN)                         :: y0camx
INTEGER, INTENT(IN OUT)                  :: izone
REAL, INTENT(IN)                         :: clonin
REAL, INTENT(IN)                         :: clatin
REAL, INTENT(IN OUT)                     :: tlat1in
REAL, INTENT(IN OUT)                     :: tlat2in
INTEGER, INTENT(OUT)                     :: mm5date
INTEGER, INTENT(OUT)                     :: mm5hr
INTEGER, INTENT(IN)                      :: itzon
INTEGER, INTENT(IN)                      :: dtout
INTEGER, INTENT(OUT)                     :: nx
INTEGER, INTENT(OUT)                     :: ny
INTEGER, INTENT(OUT)                     :: nz
REAL, INTENT(OUT)                        :: deltax
INTEGER, INTENT(OUT)                     :: ierr


INTEGER :: addday,subday






INTEGER :: i,j,k,n,nxcrs,nycrs,nest,ic,jc,mshfac,mm5yr,mm5mo,  &
    mm5day, mm5mn,mm5sc,lu,kbot,ktop,inuf,nfrc
INTEGER, PARAMETER :: lucats=24
INTEGER :: maplu(lucats)
LOGICAL :: lnewhr,lconv
REAL :: rlu(mnx,mny),z0cat(lucats)
REAL :: pp(mnz),cc(mnz),tt(mnz),qq(mnz),pr(mnz),qp(mnz)
REAL :: rd,g,dxcrs,clat,clon,tlat1,tlat2,xcen,ycen,dxrat,riloc,  &
    rjloc,x0mm5,y0mm5,xfmm5,yfmm5,rlon,rlat,xsw,ysw,xse,yse,  &
    xne,yne,xnw,ynw,xmin,xmax,ymin,ymax,hrfrac,ps0,ts0,tlp,  &
    p0,term1,term2,rtmpr,rtmpc,tv,rho,tausum,dz,c1t,qcmean,  &
    precip,depth,drpdia,drpmas,pi,drpvel,cscav,zmin,cliq,time, xoffset,yoffset

!-----MM5 big header

INTEGER :: bhi(50,20)
CHARACTER (LEN=80) :: bhic(50,20)
REAL :: bhr(20,20)
CHARACTER (LEN=80) :: bhrc(20,20)
INTEGER :: iflag

!-----MM5 subheader

INTEGER :: ndim,istart(4),iend(4)
REAL*4  xtime,sigmid(mnz)
CHARACTER (LEN=4) :: cstag, ctype
CHARACTER (LEN=24) :: curdate
CHARACTER (LEN=8) :: cfield
CHARACTER (LEN=25) :: cunit
CHARACTER (LEN=46) :: cdescrip

DATA rd /287./
DATA g /9.8/

!-----Data statements to map MM5 24 category USGS landuse to 11 category
!     CAMx landuse

DATA maplu(1)  / 1/ !Urban
DATA maplu(2)  /10/ !Dry Crop/Pasture
DATA maplu(3)  /10/ !Irrigated Crop/Pasture
DATA maplu(4)  /10/ !Mixed Dry/Irrigated Crop/Pasture
DATA maplu(5)  /10/ !Crop/Grassland Mosaic
DATA maplu(6)  / 4/ !Crop/Woodland Mosaic
DATA maplu(7)  / 3/ !Grassland
DATA maplu(8)  / 3/ !Shrubland
DATA maplu(9)  / 3/ !Mix Shrub/Grass
DATA maplu(10) / 3/ !Savanna
DATA maplu(11) / 4/ !Deciduous Broadleaf Forest
DATA maplu(12) / 4/ !Deciduous Needleleaf Forest
DATA maplu(13) / 5/ !Evergreen Broadleaf Forest
DATA maplu(14) / 5/ !Evergreen Needleleaf Forest
DATA maplu(15) / 6/ !Mixed Forest
DATA maplu(16) / 7/ !Water
DATA maplu(17) / 9/ !Herbaceous Wetland
DATA maplu(18) / 5/ !Wooded Wetland
DATA maplu(19) / 8/ !Barren, Sparse Vegetation
DATA maplu(20) / 3/ !Herbaceous Tundra
DATA maplu(21) / 4/ !Wooded Tundra
DATA maplu(22) / 6/ !Mixed Tundra
DATA maplu(23) /11/ !Bare Ground Tundra
DATA maplu(24) / 8/ !Snow or Ice

lnewhr = .true.
pi = ACOS(-1.0)

!-----Read an MM5 header flag

10  CONTINUE
READ(iomm5,END=999) iflag

!-----This is the "big header" at the top of the file

IF (iflag == 0) THEN
  READ(iomm5) bhi,bhr,bhic,bhrc
  
!-----First time through, initialize cloud and calculate grid parameters
  
  IF (lfirst) THEN
    
!          do k = 1,nz
!c            do j = 1,ny-1
!              do i = 1,nx-1
!                ql(i,j,k) = 0.
!                qi(i,j,k) = 0.
!                qr(i,j,k) = 0.
!                qs(i,j,k) = 0.
!                qg(i,j,k) = 0.
!                qc(i,j,k) = 0.
!                qcold(i,j,k) = 0.
!                qpr(i,j,k) = 0.
!                qps(i,j,k) = 0.
!                qpg(i,j,k) = 0.
!              enddo
!            enddo
!          enddo
    
!-----MM5 coarse grid
    
    nycrs = bhi(5,1)
    nxcrs = bhi(6,1)
    dxcrs = bhr(1,1)/1000.
    clat  = bhr(2,1)
    clon  = bhr(3,1)
    tlat1 = bhr(5,1)
    tlat2 = bhr(6,1)
    xcen = 1 + FLOAT(nxcrs - 1)/2.
    ycen = 1 + FLOAT(nycrs - 1)/2.
    WRITE(*,*)
    WRITE(*,*)'Grid parameters for the Coarse MM5 domain'
    WRITE(*,'(a,2i10)')  '            NX,NY:',nxcrs,nycrs
    WRITE(*,'(a,f10.3)') '               DX:',dxcrs
    WRITE(*,'(a,2f10.3)')'  Central Lat/lon:',clat,clon
    WRITE(*,'(a,2f10.3)')'   True Latitudes:',tlat1,tlat2
    WRITE(*,'(a,2f10.3)')'Grid center (I,J):',xcen,ycen
    
!-----MM5 nest to be processed
    
    nest  = bhi(13,1)
    ny    = bhi(16,1)
    nx    = bhi(17,1)
    nz    = bhi(12,11)
    dxrat = FLOAT(bhi(20,1))
    rjloc = bhr(10,1)
    riloc = bhr(11,1)
    deltax = dxcrs/dxrat
    x0mm5 = dxcrs*(riloc - xcen)
    y0mm5 = dxcrs*(rjloc - ycen)
    xfmm5 = x0mm5 + FLOAT(nx-1)*deltax
    yfmm5 = y0mm5 + FLOAT(ny-1)*deltax
    WRITE(*,*)
    WRITE(*,*)'Grid parameters for the input MM5 domain'
    WRITE(*,'(a,i10)')   '          NEST ID:',nest
    WRITE(*,'(a,3i10)')  '         NX,NY,NZ:',nx,ny,nz
    WRITE(*,'(a,f10.3)') '               DX:',deltax
    WRITE(*,'(a,2f10.3)')'Location of (1,1):',riloc,rjloc
    WRITE(*,'(a,2f10.3)')'    SW x/y corner:',x0mm5,y0mm5
    WRITE(*,'(a,2f10.3)')'    NE x/y corner:',xfmm5,yfmm5
    IF (nx > mnx .OR. ny > mny .OR. nz > mnz) THEN
      WRITE(*,*)'MM5 dimensions too large for arrays'
      WRITE(*,*)'Increase array dimensions in param.inc ', 'and recompile'
      STOP
    END IF
    IF (project == 'LCP       ') THEN
      IF (clat == clatin .AND. clon == clonin .AND.  &
            ((tlat1 == tlat1in .AND. tlat2 == tlat2in) .OR.  &
            (tlat1 == tlat2in .AND. tlat2 == tlat1in))) THEN
        project = 'LCP1      '
        WRITE(*,'(a)')'Input LCP identical to MM5 LCP'
        xoffset = (x0camx - x0mm5)/deltax
        yoffset = (y0camx - y0mm5)/deltax
        ioffset = nint((x0camx - x0mm5)/deltax)
        joffset = nint((y0camx - y0mm5)/deltax)
        IF (ABS(xoffset-FLOAT(ioffset)) > 0.001 .OR.  &
              ABS(yoffset-FLOAT(joffset)) > 0.001) THEN
          WRITE(*,'(a)') 'Grid corners do not align with MM5 dot points'
          STOP
        END IF
        WRITE(*,'(a,2i10)')'      I,J offsets:',ioffset,joffset
      ELSE
        project = 'LCP2      '
        WRITE(*,'(a)')'Interpolating to input grid LCP'
      END IF
    END IF
    IF (project /= 'LCP1      ') THEN
      DO i = 1,nx
        xdot(i) = x0mm5 + deltax*(i - 1)
        xcrs(i) = x0mm5 + deltax*(i - 0.5)
      END DO
      DO j = 1,ny
        ydot(j) = y0mm5 + deltax*(j - 1)
        ycrs(j) = y0mm5 + deltax*(j - 0.5)
      END DO
      DO j = 1,ny
        DO i = 1,nx
          CALL lcpgeo(1,clat,clon,tlat1,tlat2,xdot(i),ydot(j), rlon,rlat)
          IF (project == 'LATLON    ') THEN
            xdprj(i,j) = rlon
            ydprj(i,j) = rlat
          ELSE IF (project == 'UTM       ') THEN
            CALL utmgeo(0,izone,xdprj(i,j),ydprj(i,j),rlon,rlat)
          ELSE IF (project == 'LCP2      ') THEN
            CALL lcpgeo(0,clatin,clonin,tlat1in,tlat2in,  &
                xdprj(i,j),ydprj(i,j),rlon,rlat)
          END IF
        END DO
      END DO
    END IF
    
!-----CAMx grid
    
    IF (project /= 'LCP1      ') THEN
      DO ic = 1,nxc
        xc(ic) = x0camx + dxcamx*(ic - 0.5)
      END DO
      DO jc = 1,nyc
        yc(jc) = y0camx + dycamx*(jc - 0.5)
      END DO
      DO jc = 1,nyc
        DO ic = 1,nxc
          IF (project == 'UTM       ') THEN
            CALL utmgeo(1,izone,xc(ic),yc(jc),rlon,rlat)
          ELSE IF (project == 'LCP2      ') THEN
            CALL lcpgeo(1,clatin,clonin,tlat1in,tlat2in,xc(ic),  &
                yc(jc),rlon,rlat)
          ELSE IF (project == 'LATLON    ') THEN
            rlon = xc(ic)
            rlat = yc(jc)
          END IF
          CALL lcpgeo(0,clat,clon,tlat1,tlat2,xclcp(ic,jc),  &
              yclcp(ic,jc),rlon,rlat)
        END DO
      END DO
      xsw = xclcp(1,1)
      ysw = yclcp(1,1)
      xse = xclcp(nxc,1)
      yse = yclcp(nxc,1)
      xne = xclcp(nxc,nyc)
      yne = yclcp(nxc,nyc)
      xnw = xclcp(1,nyc)
      ynw = yclcp(1,nyc)
    ELSE
      deltax = ANINT(deltax*1000.)/1000.
      dxcamx = ANINT(dxcamx*1000.)/1000.
      dycamx = ANINT(dycamx*1000.)/1000.
      IF (deltax == dxcamx) THEN
        xsw = x0mm5 + deltax*(ioffset + 0.5)
        ysw = y0mm5 + deltax*(joffset + 0.5)
      ELSE
        xsw = x0mm5 + deltax*ioffset + 0.5*dxcamx
        ysw = y0mm5 + deltax*joffset + 0.5*dxcamx
      END IF
      xne = xsw + dxcamx*(nxc - 1.)
      yne = ysw + dycamx*(nyc - 1.)
      xse = xne
      yse = ysw
      xnw = xsw
      ynw = yne
    END IF
    WRITE(*,*)
    WRITE(*,*)'Grid parameters for the input domain in LCP space'
    WRITE(*,'(a,3i10)')  '         NX,NY,NZ:',nxc,nyc,nzc
    WRITE(*,'(a,2f10.3)')'            DX,DY:',dxcamx,dycamx
    WRITE(*,'(a,2f10.3)')' SW cell midpoint:',xsw,ysw
    WRITE(*,'(a,2f10.3)')' SE cell midpoint:',xse,yse
    WRITE(*,'(a,2f10.3)')' NE cell midpoint:',xne,yne
    WRITE(*,'(a,2f10.3)')' NW cell midpoint:',xnw,ynw
    IF (project == 'LCP1      ') THEN
      IF (dxcamx /= dycamx) THEN
        WRITE(*,*) 'For matching LCP projections, input DX must equal DY'
        STOP
      END IF
      IF (deltax == dxcamx .AND.  &
            (nxc > nx-1 .OR. nyc > ny-1 .OR. nzc > nz)) THEN
        WRITE(*,*)'Input grid dimensions exceed MM5 grid', ' dimensions'
        WRITE(*,*)'It must range from 1 through nx-1 and ny-1'
        STOP
      ELSE IF (deltax /= dxcamx) THEN
        IF (AMOD(deltax,dxcamx) > 0.001) THEN
          WRITE(*,*)'For matching LCP projections,'
          WRITE(*,*)'MM5 DX must be integer multiple of input grid DX'
          STOP
        END IF
        mshfac = nint(deltax/dxcamx)
        IF (ioffset < 1 .OR. joffset < 1 .OR.  &
              ioffset+nxc/mshfac > nx-1 .OR. joffset+nyc/mshfac > ny-1) THEN
          WRITE(*,*)'For matching LCP projections,'
          WRITE(*,*)'Finer input grid mesh cannot span entire MM5 mesh'
          WRITE(*,*)'It must range from 2 through nx-1 and ny-1'
          STOP
        END IF
      END IF
    END IF
    
    IF (project /= 'LCP1      ') THEN
      xmin = x0mm5 + deltax/2.
      xmax = xfmm5 - deltax/2.
      ymin = y0mm5 + deltax/2.
      ymax = yfmm5 - deltax/2.
      IF (xsw < xmin .OR. xnw < xmin .OR. ysw < ymin .OR. yse < ymin .OR.  &
            xse > xmax .OR. xne > xmax .OR. ynw > ymax .OR. yne > ymax) THEN
        WRITE(*,*)'Input grid ranges outside MM5 grid'
        STOP
      END IF
      DO jc = 1,nyc
        DO ic = 1,nxc
          DO i = 1,nx
            IF (xdot(i) > xclcp(ic,jc)) THEN
              idot(ic,jc) = i
              GO TO 101
            END IF
          END DO
          WRITE(*,*)'Did not find idot',ic,jc,xclcp(ic,jc)
          STOP
          101            DO i = 1,nx
            IF (xcrs(i) > xclcp(ic,jc)) THEN
              icrs(ic,jc) = i
              GO TO 102
            END IF
          END DO
          WRITE(*,*)'Did not find icrs',ic,jc,xclcp(ic,jc)
          STOP
          102            DO j = 1,ny
            IF (ydot(j) > yclcp(ic,jc)) THEN
              jdot(ic,jc) = j
              GO TO 103
            END IF
          END DO
          WRITE(*,*)'Did not find jdot',ic,jc,yclcp(ic,jc)
          STOP
          103            DO j = 1,ny
            IF (ycrs(j) > yclcp(ic,jc)) THEN
              jcrs(ic,jc) = j
              GO TO 104
            END IF
          END DO
          WRITE(*,*)'Did not find jcrs',ic,jc,yclcp(ic,jc)
          STOP
          104            CONTINUE
        END DO
      END DO
    END IF
    
  END IF
  GO TO 10
  
!-----This is a data "sub-header"; read data fields.
  
ELSE IF (iflag == 1) THEN
  READ (iomm5) ndim,istart,iend,xtime,cstag,ctype,curdate,  &
      cfield,cunit,cdescrip
  IF (lnewhr) THEN
    lnewhr = .false.
    READ(curdate(3:4),'(i2)') mm5yr
    READ(curdate(6:7),'(i2)') mm5mo
    READ(curdate(9:10),'(i2)') mm5day
    READ(curdate(12:13),'(i2)') mm5hr
    READ(curdate(15:16),'(i2)') mm5mn
    READ(curdate(18:19),'(i2)') mm5sc
    
!-----Convert MM5 date to Julian day and convert hour to
!     user-specified time zone
    
    mm5mn = nint(FLOAT(mm5mn) + ANINT(FLOAT(mm5sc)/60.))
    nfrc = 60/dtout
    DO n = 0,nfrc
      IF (ABS(mm5mn - n*dtout) < 2) THEN
        mm5mn = n*dtout
        GO TO 20
      END IF
    END DO
    WRITE(*,'(a,a)')'MM5 clock is nuts! Cannot sychronize to',  &
        ' user-specified output inverval'
    STOP
    
    20       mm5date = mm5yr*10000 + mm5mo*100 + mm5day
    
    CALL juldate(mm5date)
    IF (mm5mn > 59) THEN
      mm5mn = mm5mn - 60
      mm5hr = mm5hr + 1
      IF (mm5hr > 23) THEN
        mm5hr = mm5hr - 24
        mm5date = addday(mm5date)
      END IF
    END IF
    mm5hr = mm5hr - itzon
    IF (mm5hr < 0) THEN
      mm5hr = mm5hr + 24
      mm5date = subday(mm5date)
    END IF
    IF (mm5hr > 23) THEN
      mm5hr = mm5hr - 24
      mm5date = addday(mm5date)
    END IF
    mm5hr = 100*mm5hr + mm5mn
!          write(*,'(/,a,t30,i6.5,i5.4)') ' MM5 date/time (YYJJJ HHMM):',
!     &                                   mm5date,mm5hr
  END IF
  
  IF (cfield == 'U       ') THEN
    READ(iomm5) (((ua(i,j,nz-k+1),j=1,ny),i=1,nx),k=1,nz)
  ELSE IF (cfield == 'V       ') THEN
    READ(iomm5) (((va(i,j,nz-k+1),j=1,ny),i=1,nx),k=1,nz)
  ELSE IF (cfield == 'T       ') THEN
    READ(iomm5) (((ta(i,j,nz-k+1),j=1,ny),i=1,nx),k=1,nz)
  ELSE IF (cfield == 'Q       ') THEN
    READ(iomm5) (((qa(i,j,nz-k+1),j=1,ny),i=1,nx),k=1,nz)
!        elseif (cfield.eq.'CLW     ') then
!          read(iomm5) (((ql(i,j,nz-k+1),j=1,ny),i=1,nx),k=1,nz)
!        elseif (cfield.eq.'ICE     ') then
!          read(iomm5) (((qi(i,j,nz-k+1),j=1,ny),i=1,nx),k=1,nz)
!        elseif (cfield.eq.'RNW     ') then
!          read(iomm5) (((qr(i,j,nz-k+1),j=1,ny),i=1,nx),k=1,nz)
!        elseif (cfield.eq.'SNOW    ') then
!          read(iomm5) (((qs(i,j,nz-k+1),j=1,ny),i=1,nx),k=1,nz)
!        elseif (cfield.eq.'GRAUPEL ') then
!          read(iomm5) (((qg(i,j,nz-k+1),j=1,ny),i=1,nx),k=1,nz)
!        elseif (cfield.eq.'RAIN CON') then
!          read(iomm5) ((rainc(i,j),j=1,ny),i=1,nx)
!        elseif (cfield.eq.'RAIN NON') then
!          read(iomm5) ((rainr(i,j),j=1,ny),i=1,nx)
!        elseif (cfield.eq.'TKE     ') then
!          read(iomm5) (((tke(i,j,nz-k+1),j=1,ny),i=1,nx),k=1,nz)
  ELSE IF (cfield == 'PP      ') THEN
    READ(iomm5) (((pa(i,j,nz-k+1),j=1,ny),i=1,nx),k=1,nz)
  ELSE IF (cfield == 'PSTARCRS') THEN
    READ(iomm5) ((psax(i,j),j=1,ny),i=1,nx)
  ELSE IF (cfield == 'GROUND T') THEN
    READ(iomm5) ((tsrf(i,j),j=1,ny),i=1,nx)
  ELSE IF (cfield == 'SWDOWN  ') THEN
    READ(iomm5) ((rgrnd(i,j),j=1,ny),i=1,nx)
!        elseif (cfield.eq.'PBL HGT ') then
!          read(iomm5) ((pbl(i,j),j=1,ny),i=1,nx)
  ELSE IF (cfield == 'TERRAIN ') THEN
    READ(iomm5) ((topo(i,j),j=1,ny),i=1,nx)
  ELSE IF (cfield == 'LAND USE') THEN
    READ(iomm5) ((rlu(i,j),j=1,ny),i=1,nx)
  ELSE IF (cfield == 'SFZ0    ') THEN
    READ(iomm5,ERR=30) (z0cat(i),i=1,lucats)
    GO TO 10
    30       WRITE(*,*)'Error reading MM5 surface roughness for 24 landuse'
    WRITE(*,*)'categories. You need to edit READMM5.F to'
    WRITE(*,*)'adjust the number of categories and to alter the'
    WRITE(*,*)'landuse mapping algorithm.'
  ELSE IF (cfield == 'SIGMAH  ') THEN
    READ(iomm5) (sigmid(nz-k+1),k=1,nz)
  ELSE
    READ(iomm5)
  END IF
  GO TO 10
  
!-----Finished reading data for the hour
  
ELSE IF (iflag == 2) THEN
!        write(*,*)
!        write(*,*) 'Finished reading MM5 output for ',curdate
END IF

!-----Map surface roughness to grid according to landuse map
!     and convert from cm to m.  Map MM5 landuse to input grid landuse.

DO j = 1,ny-1
  DO i = 1,nx-1
    lu = INT(rlu(i,j))
    z0(i,j) = z0cat(lu)/100.
    DO n = 1,11
      clu(i,j,n) = 0.
    END DO
    clu(i,j,maplu(lu)) = 1.
  END DO
END DO

!-----Calculate cartesian height from sigma levels, convert height MSL
!     to height AGL (m), and determine P* at dot points

IF (lfirst) THEN
  ptop = bhr(2,2)
  ps0 = bhr(2,5)
  ts0 = bhr(3,5)
  tlp = bhr(4,5)
  sigma(0) = 1.
  DO k = 1,nz
    sigma(k) = 2.*sigmid(k) - sigma(k-1)
  END DO
  DO j = 1,ny-1
    DO i = 1,nx-1
      p0 = sigma(0)*psax(i,j) + ptop
      term1 = rd*tlp/(2.*g)*(ALOG(p0/ps0))**2
      term2 = rd*ts0/g * ALOG(p0/ps0)
      trn(i,j) = -(term1+term2)
    END DO
  END DO
  DO k = 1,nz
    DO j = 1,ny-1
      DO i = 1,nx-1
        p0 = sigma(k)*psax(i,j) + ptop
        term1 = rd*tlp/(2.*g)*(ALOG(p0/ps0))**2
        term2 = rd*ts0/g * ALOG(p0/ps0)
        zh(i,j,k) = -(term1+term2)
        zh(i,j,k) = zh(i,j,k) - trn(i,j)
        IF (zh(i,j,k) < 0.) THEN
          WRITE(*,*)'ZH<0: at (i,j,k):',i,j,k,zh(i,j,k)
          STOP
        END IF
      END DO
    END DO
  END DO
  
  CALL xtod(psax,psad,nx,ny)
  
END IF

!-----Convert from pressure perturbation to pressure (mb) by the relation
!     p = pert + (p*)x(sigma) + ptop

DO k = 1,nz
  DO j = 1,ny-1
    DO i = 1,nx-1
      pa(i,j,k) = (pa(i,j,k) + psax(i,j)*sigmid(k) + ptop)/100.
    END DO
  END DO
END DO
100  CONTINUE

IF (lfirst) lfirst = .false.
RETURN

999   CONTINUE
ierr = 1
RETURN
END SUBROUTINE readmm5
