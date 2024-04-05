SUBROUTINE utmgeo(iway,iutmzon,rx4,ry4,rlon4,rlat4)

!-----UTMGEO performs UTM to geodetic (lat/lon) translation:

!     This is a Fortran version of the BASIC program "Transverse Mercator
!     Conversion", Copyright 1986, Norman J. Berls (Stefan Musarra, 2/94)
!     Based on algorithm taken from "Map Projections Used by the USGS"
!     by John P. Snyder, Geological Survey Bulletin 1532, USDI.

!     Input arguments:
!        iway                Conversion type
!                            0 = geodetic to UTM
!                            1 = UTM to geodetic
!        iutmzon             UTM zone
!        rx4                 UTM easting (km)
!        ry4                 UTM northing (km)
!        rlon4               Longitude (deg, negative for W)
!        rlat4               Latitude (deg)

!     Output arguments:
!        rx4                 UTM easting (km)
!        ry4                 UTM northing (km)
!        rlon4               Longitude (deg)
!        rlat4               Latitude (deg)
IMPLICIT REAL*8 (a-h,o-z)

INTEGER, INTENT(IN OUT)                  :: iway
INTEGER, INTENT(IN)                      :: iutmzon
REAL, INTENT(IN OUT)                     :: rx4
REAL, INTENT(IN OUT)                     :: ry4
REAL, INTENT(IN OUT)                     :: rlon4
REAL, INTENT(IN OUT)                     :: rlat4

LOGICAL :: lsouth

REAL, PARAMETER :: pi=3.14159265358979
REAL, PARAMETER :: degrad=pi/180.
REAL, PARAMETER :: raddeg=1./degrad
REAL, PARAMETER :: semimaj=6378206.4
REAL, PARAMETER :: semimin=6356583.8
!     parameter(e2=1.0-(semimin/semimaj)**2.)
!     parameter(e4=e2*e2, e6=e2*e4, ep2=e2/(1.-e2))
REAL, PARAMETER :: scfa=.9996
REAL*8, PARAMETER :: north=0.
REAL, PARAMETER :: east=500000.

e2=1.0-(semimin/semimaj)**2.0
e4=e2*e2
e6=e2*e4
ep2=e2/(1.-e2)

!-----Entry point

!-----Set Zone parameters

lsouth = .false.
IF( iutmzon < 0 ) lsouth = .true.
zone = ABS(iutmzon)
cm = zone*6.0 - 183.0
cmr = cm*degrad

!-----Convert inputs from single to double precision

IF (iway == 1) THEN
  xx = 1000.*rx4
  yy = 1000.*ry4
  IF (lsouth) yy = yy - 1.d7
ELSE
  dlat = rlat4
  dlon = rlon4
END IF

!-----Lat/Lon to UTM conversion

IF (iway == 0) THEN
  rlat = degrad*dlat
  rlon = degrad*dlon
  
  delam = dlon - cm
  IF (delam < -180.) delam = delam + 360.
  IF (delam > 180.) delam = delam - 360.
  delam = delam*degrad
  
  f1 = (1. - e2/4. - 3.*e4/64. - 5.*e6/256)*rlat
  f2 = 3.*e2/8. + 3.*e4/32. + 45.*e6/1024.
  f2 = f2*SIN(2.*rlat)
  f3 = 15.*e4/256.*45.*e6/1024.
  f3 = f3*SIN(4.*rlat)
  f4 = 35.*e6/3072.
  f4 = f4*SIN(6.*rlat)
  rm = semimaj*(f1 - f2 + f3 - f4)
  IF (dlat == 90. .OR. dlat == -90.) THEN
    xx = 0.
    yy = scfa*rm
  ELSE
    rn = semimaj/SQRT(1. - e2*SIN(rlat)**2)
    t = TAN(rlat)**2
    c = ep2*COS(rlat)**2
    a = COS(rlat)*delam
    
    f1 = (1. - t + c)*a**3/6.
    f2 = 5. - 18.*t + t**2 + 72.*c - 58.*ep2
    f2 = f2*a**5/120.
    xx = scfa*rn*(a + f1 + f2)
    f1 = a**2/2.
    f2 = 5. - t + 9.*c + 4.*c**2
    f2 = f2*a**4/24.
    f3 = 61. - 58.*t + t**2 + 600.*c - 330.*ep2
    f3 = f3*a**6/720.
    yy = scfa*(rm + rn*TAN(rlat)*(f1 + f2 + f3))
  END IF
  xx = xx + east
  yy = yy + north
  
!-----UTM to Lat/Lon conversion
  
ELSE
  xx = xx - east
  yy = yy - north
  e1 = SQRT(1. - e2)
  e1 = (1. - e1)/(1. + e1)
  rm = yy/scfa
  u = 1. - e2/4. - 3.*e4/64. - 5.*e6/256.
  u = rm/(semimaj*u)
  
  f1 = 3.*e1/2. - 27.*e1**3./32.
  f1 = f1*SIN(2.*u)
  f2 = 21.*e1**2/16. - 55.*e1**4/32.
  f2 = f2*SIN(4.*u)
  f3 = 151.*e1**3./96.
  f3 = f3*SIN(6.*u)
  rlat1 = u + f1 + f2 + f3
  dlat1 = rlat1*raddeg
  IF (dlat1 >= 90. .OR. dlat1 <= -90.) THEN
    dlat1 = DMIN1(dlat1,DBLE(90.) )
    dlat1 = DMAX1(dlat1,DBLE(-90.) )
    dlon = cm
  ELSE
    c1 = ep2*COS(rlat1)**2.
    t1 = TAN(rlat1)**2.
    f1 = 1. - e2*SIN(rlat1)**2.
    rn1 = semimaj/SQRT(f1)
    r1 = semimaj*(1. - e2)/SQRT(f1**3)
    d = xx/(rn1*scfa)
    
    f1 = rn1*TAN(rlat1)/r1
    f2 = d**2/2.
    f3 = 5.*3.*t1 + 10.*c1 - 4.*c1**2 - 9.*ep2
    f3 = f3*d**2*d**2/24.
    f4 = 61. + 90.*t1 + 298.*c1 + 45.*t1**2. - 252.*ep2 - 3.*c1**2
    f4 = f4*(d**2)**3./720.
    rlat = rlat1 - f1*(f2 - f3 + f4)
    dlat = rlat*raddeg
    
    f1 = 1. + 2.*t1 + c1
    f1 = f1*d**2*d/6.
    f2 = 5. - 2.*c1 + 28.*t1 - 3.*c1**2 + 8.*ep2 + 24.*t1**2.
    f2 = f2*(d**2)**2*d/120.
    rlon = cmr + (d - f1 + f2)/COS(rlat1)
    dlon = rlon*raddeg
    IF (dlon < -180.) dlon = dlon + 360.
    IF (dlon > 180.) dlon = dlon - 360.
  END IF
END IF

!-----Convert precision of outputs

IF (iway == 1) THEN
  rlat4 = REAL(dlat)
  rlon4 = REAL(dlon)
ELSE
  rx4 = REAL(xx/1000.)
  IF (lsouth) yy = yy + 1.d7
  ry4 = REAL(yy/1000.)
END IF

RETURN
END SUBROUTINE utmgeo
