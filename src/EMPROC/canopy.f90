!   Modified by Xuemei Wang--Nov. 2007

!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!
!   Input and output files must be selected before starting the program
!
!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!!
!   Input varibles
!
!   Day                  Julian day
!   Lat                  Latitude
!   Hour                 Hour of the day
!   Tc                   Temperature [C]
!   PPFD           Incoming photosynthetic active radiation [umol/m2/s1]
!   Wind                 Wind speed [m s-1]
!   Humidity             Relative humidity [%]
!   Cantypye             Defines set of canopy characteristics
!   LAI                  Leaf area index [m2 per m2 ground area]
!   DI                   ???
!   Pres                 Pressure [Pa]
!
!   Used variables:
!
!   PPFDfrac             Fraction of total solar radiation that is PPFD
!   Solar                Solar radiation [W/m2]
!   Maxsolar             Maximum of solar radiation
!   Beta                 Sin of solar angle above horizon
!   Sinbeta              Solar angle above horizon
!   TairK0               Above canopy air temperature [K]
!       TairK            Array of canopy air temperature [K]
!   Ws0                  Above canopy wind speed [m/s]
!   Ws                   Array of canopy wind speed [m/s]
!   HumidairPA0          Above canopy ambient humidity [Pa]
!       HumidairPa       Array of canopy ambient humidity in [Pa]
!   StomataDI            Index for water status of leaves. used to modify stomatal conductance
!   Transmis             Transmission of PPFD that is diffuse
!   Difffrac             Fraction of PPFD that is diffuse
!   PPFDfrac             Fraction of solar rad that is PPFD
!   Trate                Stability of boundary ???
!   SH                   Sensible heat flux ???
!       VPgausWt         Array of gaussian weighting factors
!   VPgausDis            Array of gaussian weighting factors
!   VPslwWT              Array of gaussian weighting factors
!   SunFrac              Array of the fraction of sun leaves. i = 1 is the top canopy layer, 2 is the next layer, etc.
!   SunPPFD              Array of incoming (NOT absorbed) PPFD on a sun leaf [umol/m2/s]
!   ShadePPFD            Array of incoming (NOT absorbed) PPFD on a shade leaf [umol/m2/s]
!   SunQv                Array of visible radiation (in and out) fluxes on sun leaves
!   ShadeQv              Array of absorbed visible radiation (in and out) fluxes on shade leaves
!   SunQn                Array of absorbed near IR radiation (in and out) fluxes on sun leaves
!   ShadeQn              Array of absorbed near IR radiation (in and out) fluxes on shade leaves
!   SunleafTK            Array of leaf temperature for sun leaves [K]
!   SunleafSH            Array of sensible heat flux for sun leaves [W/m2]
!   SunleafLH            Array of latent heat flux for sun leaves [W/m2]
!   SunleafIR            Array of infrared flux for sun leaves [W/m2]
!   ShadeleafTK          Array of leaf temperature for shade leaves [K]
!   ShadeleafSH          Array of sensible heat flux for shade leaves [W/m2]
!   ShadeleafLH          Array of latent heat flux for shade leaves [W/m2]
!   ShadeleafIR          Array of infrared flux for shade leaves [W/m2]
!   QbAbsV, QbAbsN       Absorbed direct beam light for visible and near infra red
!   QdAbsV, QdAbsN       Array of absorbed diffuse light for visible and near infra red
!   QsAbsV, QsAbsN       Array of absorbed scattered light for visible and near infra red
!   QBeamV, QBeamN       Above canopy beam (direct) light for visible and near infra red
!   QDiffV, QDiffN       Above canopy diffuse light for visible and near infra red
!       Ea1pLayer        Array of emission activity of light per layer
!       Ea1tLayer        Array of emission activity of temperature per layer
!       Ea1Layer         Array of companied emission activity
!       Ea1pCanopy       Total emission activity of light
!       EatiLayer        Array of emission activity of temperature indendent per layer
!   Ea1tCanopy           Total emission activity of temperature depedent factor
!   Ea1Canopy            Total companied emission activity
!   EatiCanopy           Total emission activity of temperature indepedent factor
!   Calcbeta             Function: Calculation of solar zenith angle
!   WaterVapPres         Function: Convert water mixing ratio (kg/kg) to water vapor pressure
!   Stability            Function: Temperature lapse rate
!   Ea1t99               Function: Temperature dependence activity factor for emission type 1
!   Ea1p99               Function: Light dependence activity factor for emission
!   Ealti                Function: Temperature independence activity factor for emission
!   DIstomata            Function:
!   CalcEccentricity     Function:
!
!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

SUBROUTINE gamme_ce(jdate,jtime,lat, long,tc,t24,t240,  &
    ppfd0, ppfd24, ppfd240, wind, humidity,cantype,lai,pres,di,nrcha,nrtyp,  &
    canopychar,spcname,ea1canopy,eaticanopy)

USE CONST_CANOPY
IMPLICIT NONE


! input
INTEGER, intent(in) :: jdate,jtime,nrcha,nrtyp
REAL, intent(in) :: long,lat,cantype
REAL, intent(in) :: tc, pres, wind,humidity, lai, di,  &
    ppfd0,ppfd24,ppfd240,t24,t240
! Array of canopy characteristics for NrTyp of canopy type
REAL, DIMENSION(nrcha,nrtyp),intent(in) :: canopychar
CHARACTER (LEN=16),intent(in) :: spcname

! output
REAL, intent(out) ::  ea1canopy,eaticanopy

! local variables
INTEGER :: i, jj, kk, k, day
REAL :: sinbeta,beta
REAL, DIMENSION(layers) ::  vpgauswt, vpgausdis2,  &
    vpgausdis, vpslwwt, sunfrac, qdabsv, qsabsv, qdabsn,  &
    qsabsn, sunqv, shadeqv, sunqn, shadeqn, sunppfd,  &
    shadeppfd, tairk, humidairpa, ws, sunleaftk, sunleafsh,  &
    sunleaflh,sunleafir, shadeleaftk, shadeleafsh,  &
    shadeleaflh,shadeleafir,ea1player, ea1tlayer, ea1layer, eatilayer

REAL :: hour, solar, maxsolar, difffrac,ppfd, ppfdfrac, qbabsn,  &
    trate, stomatadi, qbeamv,qdiffv, qbeamn, qdiffn,  &
    qbabsv,ea1tcanopy, ea1pcanopy, tairk0, humidairpa0,ws0, sh

REAL :: distomata, calceccentricity,watervappres,  &
    stability, ea1t99, ea1p99,calcbeta,ealti99

!---------------------------Header over--------------------------------

day  =  MOD(jdate,1000)
! Convert from XXXXXX format to XX.XX (solar hour)
! HOUR = 0 -> 23.xx
! Solar hour
hour  = jtime/10000 + long /15
!        Hour  = JTIME + LONG /15
IF ( hour  < 0.0 ) THEN
  hour  = hour  + 24.0
  day  = day  - 1
ELSE IF ( hour  > 24.0 ) THEN
  PRINT*,'Invalid hour: HOUR  is ', hour
END IF

beta   = calcbeta(day , lat , hour )
sinbeta    = SIN(beta  / 57.29578)
tairk0     = tc   !See the input unit(K)
ws0        = wind
ppfd  = ppfd0
stomatadi  = distomata(di )
!      Solar      = PPFD /ConvertWm2toUmolm2s*2
!      Solar      = PPFD/2.1
solar      = ppfd/2.25
maxsolar   = sinbeta  * solarconstant * calceccentricity(day )

! print*,Solar,Hour,Sinbeta
CALL gaussianintegration(vpgauswt, vpgausdis,vpgausdis2, layers)

CALL solarfractions(solar, maxsolar, qdiffv,qbeamv,qdiffn,qbeamn)

!      Qdiffv = PPFDfrac * Solar * Difffrac
!      Qbeamv = PPFDfrac * Solar * (1 - Difffrac)
!      Qdiffn = (1 - PPFDfrac ) * Solar * Difffrac
!See the input unit(K)
!      Qbeamn = (1 - PPFDfrac ) * Solar * (1 - Difffrac )
!  print*,Qdiffv,Qbeamv
CALL weightslw(vpgausdis, vpgauswt, lai, layers, vpslwwt)

CALL canopyrad(vpgausdis, layers, lai, sinbeta, qbeamv,  &
    qdiffv, qbeamn, qdiffn,cantype ,canopychar, sunfrac,  &
    qbabsv, qdabsv, qsabsv, qbabsn, qdabsn, qsabsn, sunqv,  &
    shadeqv, sunqn, shadeqn, sunppfd, shadeppfd, nrcha, nrtyp)


humidairpa0  =  watervappres(humidity , pres , waterairratio)
trate    =  stability(canopychar, cantype, solar , nrcha, nrtyp)

CALL canopyeb(trate, layers, vpgausdis, canopychar, cantype,  &
    stomatadi, tairk, humidairpa, ws, sunppfd,  &
    shadeppfd, sunqv, shadeqv, sunqn, shadeqn,  &
    sunleaftk, sunleafsh, sunleaflh, sunleafir,  &
    shadeleaftk,shadeleafsh,shadeleaflh,shadeleafir,  &
    nrcha, nrtyp, ws0, tairk0, humidairpa0)
!        print*, Trate, Shadeleaftk(1), Sunleaftk(1)

ea1tcanopy = 0.
ea1pcanopy = 0.
ea1canopy  = 0.
eaticanopy = 0.
sh         = 0.

DO i=1,layers
  
  ea1tlayer(i) = ea1t99(sunleaftk(i), t24, t240,spcname) * sunfrac(i) +  &
      ea1t99(shadeleaftk(i),t24,t240,spcname) * (1-sunfrac(i))
  
! pstd = 200 for sun leaves
! pstd = 50 for shade leaves
  ea1player(i) = ea1p99(sunppfd(i),ppfd24*0.5,  &
      ppfd240*0.5,pstd_sun)* sunfrac(i) + ea1p99(shadeppfd(i),ppfd24*0.16,  &
      ppfd240*0.16,pstd_shade)*(1-sunfrac(i))
  
  ea1layer(i)  = ea1t99(sunleaftk(i), t24, t240,spcname) *  &
      ea1p99(sunppfd(i),ppfd24*0.5, ppfd240*0.5,pstd_sun) * sunfrac(i) +  &
      ea1t99(shadeleaftk(i), t24, t240,spcname) *  &
      ea1p99(shadeppfd(i),ppfd24*0.16, ppfd240*0.16,pstd_shade)*(1-sunfrac(i))
  
  eatilayer(i) = ealti99(spcname,sunleaftk(i))* sunfrac(i)+  &
      ealti99(spcname,shadeleaftk(i))*(1-sunfrac(i))
  
  
END DO

ea1pcanopy = sum( ea1player(:) * vpslwwt(:) * vpgauswt(:) )
ea1tcanopy = sum( ea1tlayer(:) * vpslwwt(:) * vpgauswt(:) )
ea1canopy  = sum( ea1layer(:)  * vpslwwt(:) * vpgauswt(:) )
eaticanopy = sum( eatilayer(:) * vpslwwt(:) * vpgauswt(:) )
ea1canopy = ea1canopy *cce* lai

! this quantity is apparently not passed out of the subroutine
!      SH = SUM( (SunleafSH(:) * Sunfrac(:) +
!     &           ShadeleafSH(:) * (1 - Sunfrac(:))) *
!     &           LAI  * VPgausWt(:)                   )

RETURN
END SUBROUTINE  gamme_ce

!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   SUBROUTINE GaussianIntegration
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

SUBROUTINE gaussianintegration (weightgauss, distgauss,distgauss2, layers)

IMPLICIT NONE
INTEGER,PARAMETER :: real_x = selected_real_kind(p=14, r=30)

INTEGER, intent(in) ::  layers

REAL, DIMENSION(layers),intent(out) :: weightgauss, distgauss,distgauss2

! local variables
INTEGER :: i
!--------------------------------------------------------------------

IF (layers == 1) THEN
  weightgauss(1) = 1
  distgauss(1)   = 0.5
  distgauss2(1)   = 1
ELSE IF (layers == 3) THEN
  weightgauss(1) = 0.277778
  weightgauss(2) = 0.444444
  weightgauss(3) = 0.277778
  distgauss(1)   = 0.112702
  distgauss(2)   = 0.5
  distgauss(3)   = 0.887298
  distgauss2(1)   = 0.277778
  distgauss2(2)   = 0.722222
  distgauss2(3)   = 1
ELSE IF (layers == 5) THEN
  weightgauss(1) = 0.1184635
  weightgauss(2) = 0.2393144
  weightgauss(3) = 0.284444444
  weightgauss(4) = 0.2393144
  weightgauss(5) = 0.1184635
  distgauss(1)   = 0.0469101
  distgauss(2)   = 0.2307534
  distgauss(3)   = 0.5
  distgauss(4)   = 0.7692465
  distgauss(5)   = 0.9530899
  distgauss2(1)   = 0.1184635
  distgauss2(2)   = 0.3577778
  distgauss2(3)   = 0.6422222
  distgauss2(4)   = 0.881536
  distgauss2(5)   = 1.0
  
ELSE
  DO i = 1, layers
    weightgauss(i) = 1. / layers
    distgauss(i)   = (i - 0.5) / layers
    distgauss2(i) = i/layers
  END DO
END IF

RETURN
END SUBROUTINE gaussianintegration

!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   SUBROUTINE WeightSLW
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

SUBROUTINE weightslw(distgauss, weightgauss, lai, layers, slw)

IMPLICIT NONE
INTEGER, PARAMETER :: real_x = selected_real_kind(p=14, r=30)

INTEGER, intent(in) ::  layers
REAL, intent(in) ::  lai
REAL, DIMENSION(layers),intent(in) :: distgauss, weightgauss

REAL, DIMENSION(layers),intent(out) :: slw

! local variables
INTEGER :: i
!--------------------------------------------------

DO i = 1, layers
  slw(i) = 0.63 + 0.37 * EXP(-((lai  * distgauss(i)) - 1))
  
END DO


RETURN
END SUBROUTINE weightslw

!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   SUBROUTINE SolarFractions
!   Transmission, fraction of PPFD that is diffuse,
!        fraction of solar rad that is PPFD
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

SUBROUTINE solarfractions( solar, maxsolar, qdiffv,qbeamv, qdiffn, qbeamn)

IMPLICIT NONE
INTEGER, PARAMETER :: real_x = selected_real_kind(p=14, r=30)

!      INTEGER,INTENT(IN) :: Timeperiod
REAL, intent(in) :: solar, maxsolar

REAL, intent(out) ::  qdiffv,qbeamv, qdiffn, qbeamn
REAL :: fracdiff, ppfdfrac,ppfddiffrac,qv, qn

! internal variables
INTEGER :: i,j
REAL :: transmis
!-----------------------------------------------------

!      IF (Timeperiod .EQ. 1) THEN     ! Daily transmission
!        TransMin   = 0.26
!        TransSlope= 1.655
!      ELSE                       ! Hourly transmission
!        TransMin   = 0.26
!        TransSlope = 1.655
!      ENDIF

IF (maxsolar  <= 0) THEN
  transmis  = 0.5
ELSE IF (maxsolar < solar) THEN
  transmis  = 1.0
ELSE
  transmis  = solar  / maxsolar
END IF

! Estimate diffuse fraction based on daily transmission (Roderick 1999, Goudriann and Van Laar 1994- P.33)

!      IF (Transmis  > 0.81) THEN
!        FracDiff  = 0.05
!      ELSEIF (Transmis  > TransMin) THEN
!        FracDiff  = 0.96-TransSlope * (Transmis  - TransMin)
!      ELSE
!        FracDiff  = 0.96
!      ENDIF

! The fraction of total solar radiation that is PPFD (43% to 55%)
! G. and L. 84
!      PPFDfrac  = 0.43 + FracDiff  * 0.12

!FracDiff is based on Lizaso 2005
!modified by Xuemei 2010-01-26 according to Alex's document
fracdiff = 0.156 + 0.86/(1 + EXP(11.1*(transmis -0.53)))

!PPFDfrac is based on G.L. 84
!modified by Xuemei 2010-01-26 according to Alex's document
ppfdfrac  = 0.55 -transmis*0.12

!PPFDdifFrac is based on data in Jacovides 2007
!modified by Xuemei 2010-01-26 according to Alex's document
ppfddiffrac = fracdiff *(1.06 + transmis*0.4)

! Calculte  Qdiffv,Qbeamv, Qdiffn, Qbeamn in the subroutine
! modified by Xuemei 2010-01-26 according to Alex's document
IF (ppfddiffrac > 1.0) THEN
  ppfddiffrac  = 1.0
END IF

qv  = ppfdfrac * solar
qdiffv = qv * ppfddiffrac
qbeamv = qv - qdiffv
qn = solar - qv
qdiffn =  qn * fracdiff
qbeamn =  qn - qdiffn


RETURN
END SUBROUTINE solarfractions

!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   Subroutine CanopyRad
!
!   Canopy light environment model
!   Code developed by Alex Guenther, based on Spitters et al. (1986),
!   Goudrian and Laar (1994), Leuning (1997)
!   Initial code 8-99, modified 7-2000 and 12-2001
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

SUBROUTINE canopyrad(distgauss, layers, lai, sinbeta,  &
    qbeamv, qdiffv, qbeamn, qdiffn, cantype,  &
    canopychar, sunfrac, qbabsv, qdabsv, qsabsv, qbabsn, &
    qdabsn, qsabsn, sunqv,  &
    shadeqv, sunqn, shadeqn, sunppfd, shadeppfd, nrcha, nrtyp)

IMPLICIT NONE

! input
INTEGER, intent(in) :: layers, nrcha, nrtyp, cantype
REAL, intent(in) :: qbeamv,qdiffv,sinbeta,lai,qbeamn,qdiffn
REAL, DIMENSION(layers),intent(in) :: distgauss
REAL, DIMENSION(nrcha,nrtyp),intent(IN) :: canopychar
! output
REAL, intent(out) :: qbabsv, qbabsn

REAL, DIMENSION(layers),intent(out) :: shadeppfd, sunppfd,  &
    qdabsv, qsabsv, qsabsn, shadeqv,  sunqn, qdabsn, sunqv, shadeqn, sunfrac



! internal variables
INTEGER :: i
REAL :: scatv, scatn, refldv, refldn, reflbv, reflbn,  &
    kb, kd, kbpv, kbpn, kdpv, kdpn, laidepth, cluster,  &
    qdabsvl, qsabsvl, qdabsnl, qsabsnl

! Stefan-boltzman constant  W m-2 K-4
REAL, PARAMETER :: sb = 0.0000000567
!     REAL,PARAMETER :: ConvertPPFD = 4.766
REAL, PARAMETER :: convertshadeppfd = 4.6
REAL, PARAMETER :: convertsunppfd = 4.0
!---------------------------------------------------------------------

IF (((qbeamv  + qdiffv ) > 0.001) .AND. (sinbeta  > 0.00002) .AND.  &
      (lai  > 0.001)) THEN       ! Daytime
  
! Scattering coefficients (scatV,scatN), diffuse and beam reflection
! coefficients (ref..) for visible or near IR
  scatv   = canopychar(5,cantype)
  scatn   = canopychar(6,cantype)
  refldv  = canopychar(7,cantype)
  refldn  = canopychar(8,cantype)
  cluster = canopychar(9,cantype)
!        print*,'cluster',  Cluster
! Extinction coefficients for black leaves for beam (kb) or diffuse (kd)
  kb = cluster * 0.5 / sinbeta
! (0.5 assumes a spherical leaf angle distribution (0.5 = cos (60 deg))
  kd = 0.8 * cluster
! (0.8 assumes a spherical leaf angle distribution)
  
  CALL calcextcoeff(qbeamv,scatv,kb,kd,reflbv,kbpv,kdpv,qbabsv)
  CALL calcextcoeff(qbeamn,scatn,kb,kd,reflbn,kbpn,kdpn,qbabsn)
  
  DO i = 1,layers
! LAI depth at this layer
    laidepth   = lai  * distgauss(i)
!fraction of leaves that are sunlit
    sunfrac(i) = EXP(-kb * laidepth)
    
    CALL calcradcomponents(qdiffv , qbeamv , kdpv, kbpv, kb, scatv, refldv,  &
        reflbv, laidepth, qdabsvl, qsabsvl)
    
    CALL calcradcomponents(qdiffn , qbeamn , kdpn, kbpn, kb, scatn, refldn,  &
        reflbn, laidepth, qdabsnl, qsabsnl)
    
    shadeppfd(i) = (qdabsvl + qsabsvl) * convertshadeppfd/(1 - scatv)
    sunppfd(i) = shadeppfd(i) + (qbabsv* convertsunppfd/(1 - scatv))
    qdabsv(i) = qdabsvl
    qsabsv(i) = qsabsvl
    qdabsn(i) = qdabsnl
    qsabsn(i) = qsabsnl
    shadeqv(i) = qdabsvl + qsabsvl
    sunqv(i)   = shadeqv(i) + qbabsv
    shadeqn(i) = qdabsnl + qsabsnl
    sunqn(i)   = shadeqn(i) + qbabsn
  END DO
  
ELSE                           ! Night time
  
  qbabsv  = 0
  qbabsn   = 0
  
  sunfrac(:)   = 0.2
  sunqn(:)     = 0
  shadeqn(:)   = 0
  sunqv(:)     = 0
  shadeqv(:)   = 0
  sunppfd(:)   = 0
  shadeppfd(:) = 0
  qdabsv(:)    = 0
  qsabsv(:)    = 0
  qdabsn(:)    = 0
  qsabsn(:)    = 0
  
END IF

RETURN
END SUBROUTINE canopyrad

!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   Subroutine CalcExtCoeff
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

SUBROUTINE calcextcoeff(qbeam,scat,kb,kd,reflb, kbp,kdp,qbeamabsorb)

IMPLICIT NONE
INTEGER,PARAMETER :: real_x = selected_real_kind(p=14, r=30)

REAL, intent(in) :: qbeam, scat, kb, kd
REAL, intent(out) :: reflb, kbp, kdp, qbeamabsorb

! local variables
REAL :: p
!-------------------------------------------------------------------

p     = (1 - scat)**0.5
reflb = 1 - EXP((-2 * ((1 - p) / (1 + p)) * kb) / (1 + kb))

! Extinction coefficients
kbp   = kb * p
kdp   = kd * p
! Absorbed beam radiation
qbeamabsorb = kb * qbeam * (1 - scat)

RETURN
END SUBROUTINE calcextcoeff

!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   Subroutine CalcRadComponents
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

SUBROUTINE calcradcomponents(qdiff, qbeam, kdp, kbp, kb,  &
    scat, refld, reflb, laidepth, qdabs, qsabs)

IMPLICIT NONE
INTEGER,PARAMETER :: real_x = selected_real_kind(p=14, r=30)

REAL, intent(in) :: qdiff,qbeam,kdp,kbp,kb,scat, refld,reflb,laidepth
REAL, intent(out) :: qdabs, qsabs
!-------------------------------------------------------------------

qdabs = qdiff *   kdp * (1 - refld) * EXP(-kdp * laidepth)
qsabs = qbeam * ((kbp * (1 - reflb) * EXP(-kbp * laidepth)) -  &
    (kb * (1 - scat) * EXP(-kb * laidepth)))

RETURN
END SUBROUTINE calcradcomponents

!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   Subroutine CanopyEB
!
!   Canopy energy balance model for estimating leaf temperature
!   Code developed by Alex Guenther, based on Goudrian and Laar (1994),
!    Leuning (1997)
!   Initial code 8-99, modified 7-2000 and 12-2001
!
!   Note: i denotes an array containing a vertical profile through the
!         canopy with 0
!         (above canopy conditions) plus 1 to number of canopy layers
!oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

SUBROUTINE canopyeb(trate, layers, distgauss, canopychar,  &
    cantype, stomatadi, tairk, humidairpa, ws,  &
    sunppfd, shadeppfd, sunqv, shadeqv, sunqn, shadeqn,  &
    sunleaftk, sunleafsh, sunleaflh, sunleafir, shadeleaftk, shadeleafsh,  &
    shadeleaflh, shadeleafir, nrcha, nrtyp, ws0, tairk0, humidairpa0)

IMPLICIT NONE
INTEGER ,PARAMETER :: real_x = selected_real_kind(p=14, r=30)

! inputs
INTEGER, intent(in) :: nrcha, nrtyp, layers, cantype
REAL, intent(in) :: trate, stomatadi, tairk0, humidairpa0, ws0
REAL, DIMENSION(layers),intent(in) ::  distgauss, sunqv,shadeqv,  &
    sunqn, shadeqn, sunppfd, shadeppfd
REAL, DIMENSION(nrcha, nrtyp),intent(in)  :: canopychar

! outputs
REAL, DIMENSION(layers),intent(out) :: humidairpa,  &
    ws, sunleaftk, sunleafsh, sunleaflh, sunleafir, tairk,  &
    shadeleaftk, shadeleafsh, shadeleaflh, shadeleafir

! local variables
INTEGER :: i
REAL :: cdepth, lwidth, llength, cheight, eps, transpiretype,  &
    deltah, unexposedleafirin, exposedleafirin, irin,irout
REAL, DIMENSION(layers) :: ldepth, wsh
!-----------------------------------------------------------------------

cdepth        = canopychar(1, cantype)
lwidth        = canopychar(2, cantype)
llength       = canopychar(3, cantype)
cheight       = canopychar(4, cantype)
eps           = canopychar(10,cantype)
transpiretype = canopychar(11,cantype)

IF (tairk0  > 288) THEN
! Pa m-1  (humidity profile for T < 288)
  deltah =  canopychar(14, cantype) / cheight
ELSE IF (tairk0  > 278) THEN
  deltah =(canopychar(14,cantype)-((288-tairk0)/10) *  &
      (canopychar(14,cantype)-canopychar(15,cantype)))/cheight
ELSE
! Pa m-1  (humidity profile for T <278)
  deltah = canopychar(15, cantype) / cheight
END IF

ldepth(:)     = cdepth * distgauss(:)
tairk(:)      = tairk0  + (trate  * ldepth(:))      ! check this
humidairpa(:) = humidairpa0  + (deltah * ldepth(:))

wsh(:) = (cheight-ldepth(:)) - (canopychar(16,cantype) * cheight)
ws(:)  = (ws0*LOG(wsh(:))/LOG(cheight-canopychar(16,cantype) *cheight))
where (wsh(:) < 0.001) ws(:) = 0.05

DO i=1,layers
  irin     = unexposedleafirin(tairk(i), eps)
  shadeleafir(i) = 2 * irin
  sunleafir(i) = 0.5*exposedleafirin(humidairpa0,tairk0)+1.5*irin
  
! Sun
  CALL leafeb(sunppfd(i), sunqv(i) + sunqn(i),  &
      sunleafir(i), eps, transpiretype, lwidth, llength,  &
      tairk(i), humidairpa(i), ws(i), sunleaftk(i), sunleafsh(i),sunleaflh(i),  &
      irout,stomatadi )
  
  sunleafir(i) = sunleafir(i) - irout
  
! Shade
  CALL leafeb(shadeppfd(i), shadeqv(i)+shadeqn(i),  &
      shadeleafir(i),eps,transpiretype, lwidth,llength,  &
      tairk(i), humidairpa(i), ws(i),  &
      shadeleaftk(i), shadeleafsh(i),shadeleaflh(i), irout, stomatadi )
  
  shadeleafir(i) = shadeleafir(i) - irout
END DO

RETURN
END SUBROUTINE canopyeb

!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   Subroutine LeafEB
!
!   Leaf energy balance
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

SUBROUTINE leafeb(ppfd, q, irin, eps, transpiretype,  &
    lwidth, llength, tairk, humidairpa, ws, tleaf, sh, lh, irout, stomatadi)

IMPLICIT NONE
INTEGER, PARAMETER :: real_x = selected_real_kind(p=14, r=30)

REAL, intent(in) :: eps, transpiretype, lwidth, llength,ppfd, q,  &
    irin, tairk, humidairpa, stomatadi, ws
REAL, intent(out) :: irout, tleaf, sh, lh

! local variables

INTEGER :: i
REAL :: humidairkgm3,ghforced,stomres,iroutairt,lhv,lathv,ws1,  &
    lhairt,tdelt,balance,leafblc,leafh,leafle,leafirout,  &
    gh1,sh1,lh1,e1,converthumiditypa2kgm3,ressc,irout1,gh
!----------------------------------------------------

IF (ws <= 0) THEN
  ws1 = 0.001
ELSE
  ws1 = ws
END IF

! Air vapor density kg m-3
humidairkgm3 = converthumiditypa2kgm3(humidairpa, tairk)

! Heat convection coefficient (W m-2 K-1) for forced convection.
! Nobel page 366
ghforced = 0.0259 / (0.004 * ((llength / ws)**0.5))

! Stomatal resistence s m-1
stomres  = ressc(ppfd, stomatadi)

iroutairt = leafirout(tairk, eps)

! Latent heat of vaporization (J Kg-1)
lathv = lhv(tairk)

! Latent heat flux
lhairt = leafle(tairk,humidairkgm3,lathv,ghforced,stomres, transpiretype)

e1 = (q + irin - iroutairt - lhairt)
IF (e1 == 0.) THEN
  e1 = -1.
END IF

tdelt = 1.
balance = 10.
DO i = 1, 10
  IF (ABS(balance) > 2) THEN
! Boundary layer conductance
    gh1 = leafblc(ghforced, tdelt, llength)
! Convective heat flux
    sh1 = leafh(tdelt, gh1)
! Latent heat of vaporization (J Kg-1)
    lathv = lhv(tairk + tdelt)
    lh = leafle(tairk + tdelt, humidairkgm3,  &
        lathv, gh1, stomres, transpiretype)
    lh1 = lh - lhairt
    irout  = leafirout(tairk + tdelt, eps)
    irout1 = irout - iroutairt
    tdelt  = e1 / ((sh1 + lh1 + irout1) / tdelt)
    balance = q + irin - irout - sh1 - lh
  END IF
END DO

IF (tdelt > 10)  tdelt = 10
IF (tdelt < -10) tdelt = -10

tleaf = tairk + tdelt
gh    = leafblc(ghforced, tleaf - tairk, llength)
sh    = leafh(tleaf - tairk, gh)
lh    = leafle(tleaf, humidairkgm3, lathv, gh, stomres, transpiretype)
irout = leafirout(tleaf, eps)

RETURN
END SUBROUTINE leafeb

!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   FUNCTION Calcbeta
!   Calculates the solar zenith angle
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

FUNCTION calcbeta(day, lat, hour)

IMPLICIT NONE
INTEGER, PARAMETER :: real_x = selected_real_kind(p=14, r=30)

INTEGER :: day

REAL :: rpi, hour, lat, sindelta, cosdelta, a, b, sinbeta, calcbeta
REAL, PARAMETER :: pi = 3.14159, rpi180 = 57.29578
!--------------------------------------------------------------------
sindelta = -SIN(0.40907) * COS(6.28 * (day + 10) / (365))
cosdelta = (1 - sindelta**2.)**0.5

a = SIN(lat / rpi180) * sindelta
b = COS(lat / rpi180) * cosdelta
sinbeta = a + b * COS(2 * pi * (hour - 12) / 24)
calcbeta = ASIN(sinbeta) * 57.29578

END FUNCTION calcbeta

!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   FUNCTION DIstomata
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

FUNCTION distomata(di)

IMPLICIT NONE
INTEGER,PARAMETER :: real_x = selected_real_kind(p=14, r=30)
REAL :: di, distomata
! > -.5 incipient,  mild or no drought; < -4 extreme drought
REAL, PARAMETER :: dihigh = -0.5, dilow = -5
!--------------------------------------------------------------------

IF (di > dihigh) THEN
  distomata = 1  ! no drought
ELSE IF (di > dilow) THEN
! interpolate
  distomata = 1 - (0.9 * ((di - dihigh) / (dilow - dihigh)))
ELSE
  distomata = 0  ! Maximum drought, maximum stomatal resistance
END IF

END FUNCTION distomata

!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   FUNCTION CalcEccentricity
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

FUNCTION calceccentricity(day)

IMPLICIT NONE
INTEGER :: day
INTEGER, PARAMETER :: real_x = selected_real_kind(p=14, r=30)
REAL :: calceccentricity
!--------------------------------------------------------------------

calceccentricity = 1 + 0.033 * COS(2*3.14*(day-10)/365)

END FUNCTION calceccentricity

!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   FUNCTION UnexposedLeafIRin
!
!   Calculate IR into leaf that is not exposed to the sky
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

FUNCTION unexposedleafirin(tk, eps)

IMPLICIT NONE
INTEGER,PARAMETER :: real_x = selected_real_kind(p=14, r=30)
REAL :: eps, tk, unexposedleafirin
! Stefan-boltzman constant  W m-2 K-4
REAL, PARAMETER :: sb = 0.0000000567
!--------------------------------------------------------------------

unexposedleafirin = eps * sb * (tk**4.)

END FUNCTION unexposedleafirin

!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   FUNCTION ExposedLeafIRin
!
!   Calculate IR into leaf that is exposed to the sky
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

FUNCTION exposedleafirin(humidpa, tk)

IMPLICIT NONE
INTEGER,PARAMETER :: real_x = selected_real_kind(p=14, r=30)
REAL :: tk, humidpa, emissatm, exposedleafirin
! Stefan-boltzman constant  W m-2 K-4
REAL, PARAMETER :: sb = 0.0000000567
!--------------------------------------------------------------------

! Apparent atmospheric emissivity for clear skies:
! function of water vapor pressure (Pa)
! and ambient Temperature (K) based on Brutsaert(1975)
! referenced in Leuning (1997)

emissatm        = 0.642 * (humidpa / tk)**(1./7.)
exposedleafirin = emissatm * sb * (tk**4.)

END FUNCTION exposedleafirin

!oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   FUNCTION WaterVapPres
!
!   Convert water mixing ratio (kg/kg) to water vapor pressure
!   (Pa or Kpa depending on units of input )
!   Mixing ratio (kg/kg), temp (C), pressure (KPa)
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

FUNCTION watervappres(dens, pres, waterairratio)

IMPLICIT NONE
INTEGER,PARAMETER :: real_x = selected_real_kind(p=14, r=30)
REAL :: dens, pres, watervappres, waterairratio
!--------------------------------------------------------------------

watervappres = (dens / (dens + waterairratio)) * pres

END FUNCTION watervappres

!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   FUNCTION Stability
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

FUNCTION stability(canopychar, cantype, solar, nrcha, nrtyp)

IMPLICIT NONE
INTEGER :: cantype, nrcha, nrtyp
INTEGER,PARAMETER :: real_x = selected_real_kind(p=14, r=30)
REAL :: solar, trateboundary, stability
REAL, DIMENSION(nrcha, nrtyp)  :: canopychar
!--------------------------------------------------------------------

trateboundary = 500

IF (solar > trateboundary) THEN
! Daytime temperature lapse rate
  stability = canopychar(12, cantype)
ELSE IF (solar > 0) THEN
  stability = canopychar(12, cantype) -  &
      ((trateboundary - solar) / trateboundary) *  &
      (canopychar(12, cantype) - canopychar(13, cantype))
ELSE
! Nightime temperature lapse rate
  stability = canopychar(13, cantype)
END IF

END FUNCTION stability

!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   FUNCTION ConvertHumidityPa2kgm3
!
!   Saturation vapor density  (kg/m3)
!
!oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

FUNCTION converthumiditypa2kgm3(pa, tk)

IMPLICIT NONE
INTEGER, PARAMETER :: real_x = selected_real_kind(p=14, r=30)
REAL :: converthumiditypa2kgm3, pa, tk
!--------------------------------------------------------------------

converthumiditypa2kgm3 = 0.002165 * pa / tk

END FUNCTION converthumiditypa2kgm3

!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   FUNCTION ResSC
!
!   Leaf stomatal cond. resistance s m-1
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

FUNCTION ressc(par, stomatadi)

IMPLICIT NONE
INTEGER, PARAMETER :: real_x = selected_real_kind(p=14, r=30)
REAL :: par, stomatadi, scadj, ressc
!--------------------------------------------------------------------

scadj = stomatadi * ((0.0027 * 1.066 * par) /  &
    ((1 + 0.0027 * 0.0027 * par**2.)**0.5))

IF (scadj < 0.1) THEN
  ressc = 2000
ELSE
  ressc = 200 / scadj
END IF

END FUNCTION ressc

!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   FUNCTION LeafIROut
!
!   IR thermal radiation energy output by leaf
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

FUNCTION leafirout(tleaf, eps)

IMPLICIT NONE
INTEGER,PARAMETER :: real_x = selected_real_kind(p=14, r=30)
REAL :: tleaf, eps, leafirout
! Stefan-boltzman constant  W m-2 K-4
REAL, PARAMETER :: sb = 0.0000000567
!--------------------------------------------------------------------

leafirout = eps * sb * (2 * (tleaf**4.))

END FUNCTION leafirout

!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   FUNCTION LHV
!
!   Latent Heat of vaporization(J Kg-1) from Stull p641
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

FUNCTION lhv(tk)

IMPLICIT NONE
INTEGER,PARAMETER :: real_x = selected_real_kind(p=14, r=30)
REAL :: tk, lhv
!--------------------------------------------------------------------

lhv = 2501000 - (2370 * (tk - 273))

END FUNCTION lhv

!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   FUNCTION LeafLE
!
!   Latent energy term in Energy balance
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

FUNCTION leafle(tleaf, ambvap, lathv, gh, stomres, transpiretype)

IMPLICIT NONE
INTEGER,PARAMETER :: real_x = selected_real_kind(p=14, r=30)
REAL :: tleaf, ambvap, lathv, gh, stomres,  &
    transpiretype, svdtk,leafres, vapdeficit, leafle, LE
!--------------------------------------------------------------------

leafres    = (1 / (1.075 * (gh / 1231))) + stomres
vapdeficit = (svdtk(tleaf) - ambvap)

! Latent heat of vap (J Kg-1) * vap deficit(Kg m-3) /
!                 leaf resistence (s m-1)
LE = transpiretype * (1 / leafres) * lathv * vapdeficit
IF (LE < 0) THEN
  leafle = 0
ELSE
  leafle = LE
END IF

END FUNCTION  leafle

!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   FUNCTION LeafBLC
!
!   Boundary layer conductance
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

FUNCTION leafblc(ghforced, tdelta, llength)

IMPLICIT NONE
INTEGER,PARAMETER :: real_x = selected_real_kind(p=14, r=30)
REAL :: ghforced, tdelta, llength, ghfree, leafblc
!--------------------------------------------------------------------

! This is based on Leuning 1995 p.1198 except using molecular
! conductivity (.00253 W m-1 K-1 Stull p 640) instead of molecular
! diffusivity so that you end up with a heat convection coefficient
! (W m-2 K-1) instead of a conductance for free convection

IF (tdelta >= 0) THEN
  ghfree = 0.5 * 0.00253 * ((160000000 * tdelta /  &
      (llength**3.))**0.25) / llength
ELSE
  ghfree = 0
END IF
leafblc = ghforced + ghfree

END FUNCTION leafblc

!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   FUNCTION LeafH
!
!   Convective energy term in Energy balance (W m-2 heat flux from
!      both sides of leaf)
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

FUNCTION leafh(tdelta, gh)

IMPLICIT NONE
INTEGER, PARAMETER :: real_x = selected_real_kind(p=14, r=30)
REAL :: tdelta, gh, leafh
!--------------------------------------------------------------------

! 2 sides X conductance X Temperature gradient
leafh = 2 * gh * tdelta

END FUNCTION leafh

!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   FUNCTION SvdTk
!
!   Saturation vapor density  (kg/m3)
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

FUNCTION svdtk(tk)

IMPLICIT NONE
INTEGER,PARAMETER :: real_x = selected_real_kind(p=14, r=30)
REAL :: tk, svp, svdtk
!--------------------------------------------------------------------

! Saturation vapor pressure (millibars)
svp = 10**((-2937.4 / tk) - (4.9283 * LOG10(tk)) + 23.5518)
svdtk = 0.2165 * svp / tk

END FUNCTION  svdtk

!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   FUNCTION Ea1t99
!
!   Temperature dependence activity factor for emission type 1
!          (e.g. isoprene, MBO)
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

FUNCTION ea1t99(t1, t24, t240,spc_name)
USE CONST_CANOPY
IMPLICIT NONE

INTEGER, EXTERNAL :: index1
REAL, PARAMETER :: ctm2 = 230
REAL :: t1, t24, t240, ea1t99, topt, x, eopt
!--------------------------------------------------------------------
CHARACTER (LEN=16),intent(in) :: spc_name
INTEGER :: spcnum
spcnum = index1(spc_name,n_mgn_spc,mgn_spc)
IF (t1 < 260) THEN
  ea1t99 = 0
ELSE
! Energy of activation and deactivation
  
! Temperature at which maximum emission occurs
  topt = 312.5 + 0.6 * (t240 - 297)
  x    = ((1 / topt) - (1 / t1)) / 0.00831
  
! Maximum emission (relative to emission at 30 C)
  eopt = cleo(spcnum) * EXP(0.05 * (t24 - 297)) * EXP(0.05*(t240-297))
  ea1t99 = eopt * ctm2 * EXP(ctm1(spcnum) * x) /  &
      (ctm2 - ctm1(spcnum) * (1 - EXP(ctm2 * x)))
!       print*,'SPCNUM---',SPCNUM,CLeo(SPCNUM),Ctm1(SPCNUM)
END IF


END FUNCTION  ea1t99

!oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   FUNCTION Ea1pp
!
! pstd = 200 for sun leaves and 50 for shade leaves
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

FUNCTION ea1p99(ppfd1, ppfd24, ppfd240, pstd)

IMPLICIT NONE
REAL :: ppfd1, ppfd24, ppfd240, pstd, alpha, c1, ea1p99
!--------------------------------------------------------------------

IF (ppfd240 < 0.01) THEN
  ea1p99 = 0
ELSE
  alpha  = 0.004 - 0.0005 * LOG(ppfd240)
  c1     = 0.0468 * EXP(0.0005 * (ppfd24 - pstd)) * (ppfd240 ** 0.6)
  ea1p99 = (alpha * c1 * ppfd1) / ((1 + alpha**2. * ppfd1**2.)**0.5)
END IF


END FUNCTION  ea1p99

!oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
!   FUNCTION Ealti99
!
! calculate light indepent algorithms
! coded by Xuemei Wang 05 Nov. 2007
!--   GAMMA_TLI =  exp[BETA*(T-Ts)]
!           where BETA   = temperature dependent parameter
!                 Ts     = standard temperature (normally 303K, 30C)
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

FUNCTION ealti99(spcnam, temp)
USE EACO
IMPLICIT NONE


INTEGER :: spcnum                             ! Species number
INTEGER, EXTERNAL :: index1
CHARACTER (LEN=16) :: spcnam
REAL :: temp, ealti99
!     REAL,PARAMETER :: Ts = 303.0
REAL, PARAMETER :: ts = 303.15
!--------------------------------------------------------------------

spcnum = index1(spcnam,n_tdf_spc,tdf_spc)
ealti99 = EXP( tdf_prm(spcnum)*(temp-ts) )


END FUNCTION ealti99
!
!ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
