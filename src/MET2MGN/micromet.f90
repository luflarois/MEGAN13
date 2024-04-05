SUBROUTINE micromet(temp,temp0,press,press0,uwind,vwind,deltaz,z0,  &
        tr)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2013-02-28  Time: 14:59:13

!-----MICROMET calculates surface layer micro-meteorological flux-gradient
!     relationships and variables based on Louis (1979), and diagnoses
!     new wind and temperatures at standard probe heights.

!     Modifications:
!        none

!     Input arguments:
!        temp                Layer 1 temperature (K)
!        temp0               Surface temperature (K)
!        press               Layer 1 pressure (mb)
!        press0              Surface pressure (mb)
!        uwind               Layer 1 midpoint U-component speed (m/s)
!        vwind               Layer 1 midpoint V-component speed (m/s)
!        deltaz              Layer 1 midpoint height (m)
!        z0                  Surface roughness length (m)

!     Output arguments:
!        tr                  temperature at 2 m reference height (K)


REAL, INTENT(IN)                         :: temp
REAL, INTENT(IN)                         :: temp0
REAL, INTENT(IN OUT)                     :: press
REAL, INTENT(IN)                         :: press0
REAL, INTENT(IN)                         :: uwind
REAL, INTENT(IN)                         :: vwind
REAL, INTENT(IN)                         :: deltaz
REAL, INTENT(IN OUT)                     :: z0
REAL, INTENT(OUT)                        :: tr
REAL :: theta,theta0,thetar,dtheta,thetabar,wind,ufrac,vfrac,ri
REAL :: zscale,cm,ch,fm,fh,ustar,ustar2,thstar,el
REAL :: w2,pr
DATA vk/0.4/, g/9.8/, gamma/0.286/

!-----Calculate potential temperature and richardson number

theta = temp*(1000./press)**gamma
theta0 = temp0*(1000./press0)**gamma
dtheta = theta - theta0
thetabar = (theta + theta0)/2.
wind = SQRT(uwind**2 + vwind**2)
ufrac = uwind/wind
vfrac = vwind/wind
ri = (g/thetabar)*deltaz*dtheta/wind**2

!-----Determine stability functions

zscale = vk/ALOG(deltaz/z0)
IF (ri < 0.) THEN
  cm    = 69.56*SQRT(deltaz/z0)*zscale**2
  ch    = 49.82*SQRT(deltaz/z0)*zscale**2
  fm    = 1. - 9.4*ri/(1. + cm*SQRT(ABS(ri)))
  fh    = 1. - 9.4*ri/(1. + ch*SQRT(ABS(ri)))
ELSE
  fm = 1./((1. + 4.7*ri)**2)
  fh = fm
END IF

!-----Calculate micromet variables

ustar2 = fm*(wind*zscale)**2
ustar2 = AMAX1(1.e-10,ustar2)
ustar = SQRT(ustar2)
thstar = 1.35*zscale**2*wind*dtheta*fh/ustar
el = ustar2*temp/(vk*g*thstar + 1.e-10)

!-----Determine T at 2m

w2 = ustar/(vk*SQRT(fm))*ALOG(2/z0)
thetar = theta0 + ustar*thstar/(1.35*w2*fh*(vk/ALOG(2/z0))**2)
pr = press0 + (2/deltaz)*(press - press0)
tr = thetar*(1000./pr)**(-gamma)
IF (thstar > 0 .AND. tr > temp) THEN
  tr = temp0 + (2/deltaz)*(temp - temp0)
ELSE IF (thstar < 0 .AND. tr < temp) THEN
  tr = temp0 + (2/deltaz)*(temp - temp0)
END IF

RETURN
END SUBROUTINE micromet
