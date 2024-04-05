SUBROUTINE lcpgeo(iway,phic,xlonc,truelat1,truelat2,xloc,yloc,  &
        xlon,ylat)

!----CAMx v4.40 061025

!     LCPGEO performs Lambert Conformal to geodetic (lat/lon) translation

!     Code based on the TERRAIN preprocessor for MM5 v2.0,
!     developed by Yong-Run Guo and Sue Chen, National Center for
!     Atmospheric Research, and Pennsylvania State University
!     10/21/1993

!     Copyright 1996-2006
!     ENVIRON International Corporation

!     Modifications:
!        1/3/06              Now handles case with only 1 true lat

!     Input arguments:
!        iway                Conversion type
!                            0 = geodetic to Lambert Conformal
!                            1 = Lambert Conformal to geodetic
!        phic                Central latitude (deg, neg for southern hem)
!        xlonc               Central longitude (deg, neg for western hem)
!        truelat1            First true latitute (deg, neg for southern hem)
!        truelat2            Second true latitute (deg, neg for southern hem)
!        xloc/yloc           Projection coordinates (km)
!        xlon/ylat           Longitude/Latitude (deg)

!     Output arguments:
!        xloc/yloc           Projection coordinates (km)
!        xlon/ylat           Longitude/Latitude (deg)

!     Routines called:
!        none

!     Called by:
!        GRDPREP
IMPLICIT NONE

INTEGER, INTENT(IN OUT)                  :: iway
REAL, INTENT(IN OUT)                     :: phic
REAL, INTENT(IN)                         :: xlonc
REAL, INTENT(OUT)                        :: truelat1
REAL, INTENT(OUT)                        :: truelat2
REAL, INTENT(OUT)                        :: xloc
REAL, INTENT(OUT)                        :: yloc
REAL, INTENT(OUT)                        :: xlon
REAL, INTENT(OUT)                        :: ylat

REAL :: conv,a,SIGN,pole,xn,psi1,psi0,xc,yc,flp,flpp,r,cell,  &
    rxn,cel1,cel2,psx,ylon

DATA conv/57.29578/, a/6370./

!-----Entry Point

IF (phic < 0) THEN
  SIGN = -1.
ELSE
  SIGN = 1.
END IF
pole = 90.
IF (ABS(truelat1) > 90.) THEN
  truelat1 = 60.
  truelat2 = 30.
  truelat1 = SIGN*truelat1
  truelat2 = SIGN*truelat2
END IF
IF (truelat1 == truelat2) THEN
  xn = SIN(truelat2/conv)
ELSE
  xn = ALOG10(COS(truelat1/conv)) - ALOG10(COS(truelat2/conv))
  xn = xn/(ALOG10(TAN((45. - SIGN*truelat1/2.)/conv)) -  &
      ALOG10(TAN((45. - SIGN*truelat2/2.)/conv)))
END IF
psi1 = 90. - SIGN*truelat1
psi1 = psi1/conv
IF (phic < 0.) THEN
  psi1 = -psi1
  pole = -pole
END IF
psi0 = (pole - phic)/conv
xc = 0.
yc = -a/xn*SIN(psi1)*(TAN(psi0/2.)/TAN(psi1/2.))**xn

!-----Calculate lat/lon of the point (xloc,yloc)

IF (iway == 1) THEN
  xloc = xloc + xc
  yloc = yloc + yc
  IF (yloc == 0.) THEN
    IF (xloc >= 0.) flp = 90./conv
    IF (xloc < 0.) flp = -90./conv
  ELSE
    IF (phic < 0.) THEN
      flp = ATAN2(xloc,yloc)
    ELSE
      flp = ATAN2(xloc,-yloc)
    END IF
  END IF
  flpp = (flp/xn)*conv + xlonc
  IF (flpp < -180.) flpp = flpp + 360.
  IF (flpp > 180.) flpp = flpp - 360.
  xlon = flpp
  
  r = SQRT(xloc*xloc + yloc*yloc)
  IF (phic < 0.) r = -r
  cell = (r*xn)/(a*SIN(psi1))
  rxn  = 1.0/xn
  cel1 = TAN(psi1/2.)*cell**rxn
  cel2 = ATAN(cel1)
  psx  = 2.*cel2*conv
  ylat = pole - psx
  
!-----Calculate x/y from lat/lon
  
ELSE
  ylon = xlon - xlonc
  IF (ylon > 180.) ylon = ylon - 360.
  IF (ylon < -180.) ylon = ylon + 360.
  flp = xn*ylon/conv
  psx = (pole - ylat)/conv
  r = -a/xn*SIN(psi1)*(TAN(psx/2.)/TAN(psi1/2.))**xn
  IF (phic < 0.) THEN
    xloc = r*SIN(flp)
    yloc = r*COS(flp)
  ELSE
    xloc = -r*SIN(flp)
    yloc =  r*COS(flp)
  END IF
END IF

xloc = xloc - xc
yloc = yloc - yc

RETURN
END SUBROUTINE lcpgeo
