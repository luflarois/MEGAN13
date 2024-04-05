!=======================================================================
 
!     MODULE GAMMA
!
!     This module contain functions to calculate
!     GAMMA_P, GAMMA_T, GAMMA_L, GAMMA_A for BVOCs.
!
!     CONTAINS: 1)GAMMA_LAI
!               2)GAMMA_P
!               3)GAMMA_TLD
!               4)GAMMA_TLI
!               5)GAMMA_A
!               6)GAMMA_S
!               7)GAMMA_CO2
!               8)GAMMA_LAIbidir
!
!     Note:
!
!     Requirement:
!
!     CALLS: SOLARANGLE
!
!     Created by Tan 11/21/06 for MEGAN v2.0
!
!     History:
!     08/01/07 Guenther A. - Move to MEGANv2.02 with modification to
!                            correct calculation of GAMMA_P
!
!=======================================================================

module gamma_etc
USE EACO
IMPLICIT NONE

!...  Program I/O parameters

!...  External parameters

contains
!***********************************************************************

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     Scientific algorithm
!
!             Emission = [EF][GAMMA][RHO]
!           where [EF]    = emission factor (ug/m2h)
!                 [GAMMA] = emission activity factor (non-dimension)
!                 [RHO]   = production and loss within plant canopies
!                           (non-dimensino)
!                 Assumption: [RHO] = 1 (11/27/06) (See PDT_LOT_CP.EXT)
!
!             GAMMA  = [GAMMA_CE][GAMMA_age][GAMMA_SM]
!           where [GAMMA_CE]  = canopy correction factor
!                 [GAMMA_age] = leaf age correction factor
!                 [GAMMA_SM]  = soil moisture correction factor
!                 Assumption: [GAMMA_SM]  = 1 (11/27/06)
!
!             GAMMA_CE = [GAMMA_LAI][GAMMA_P][GAMMA_T]
!           where [GAMMA_LAI] = leaf area index factor
!                 [GAMMA_P]   = PPFD emission activity factor
!                 [GAMMA_T]   = temperature response factor
!
!             Emission = [EF][GAMMA_LAI][GAMMA_P][GAMMA_T][GAMMA_age][GAMMA_SM]
!        Derivation:
!             Emission = [EF][GAMMA_etc](1-LDF) + [EF][GAMMA_etc][LDF][GAMMA_P]
!             Emission = [EF][GAMMA_etc]{ (1-LDF) + [LDF][GAMMA_P] }
!             Emission = [EF][GAMMA_ect]{ (1-LDF) + [LDF][GAMMA_P] }
!           where LDF = light dependent function (non-dimension)
!
!     For ISOPRENE
!                 Assumption: LDF = 1 for isoprene            (11/27/06)
!
!        Final Equation
!             Emission = [EF][GAMMA_LAI][GAMMA_P][GAMMA_T][GAMMA_age][GAMMA_SM]
!
!     For NON-ISOPRENE
!        Final Equation
!             Emission = [EF][GAMMA_LAI][GAMMA_T][GAMMA_age][GAMMA_SM]*
!                        { (1-LDF) + [LDF][GAMMA_P] }
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!=======================================================================
!...  Begin module
!=======================================================================


!-----------------------------------------------------------------------
!.....1) Calculate GAM_L (GAMMA_LAI)
!-----------------------------------------------------------------------
!                            0.49[LAI]
!             GAMMA_LAI = ----------------    (non-dimension)
!                         (1+0.2LAI^2)^0.5
!
!     SUBROUTINE GAMMA_LAI returns the GAMMA_LAI values
!-----------------------------------------------------------------------

SUBROUTINE gamma_lai( ncols, nrows, lai, gam_l )

IMPLICIT NONE

INTEGER,intent(in) :: ncols
INTEGER,intent(in) :: nrows
REAL,DIMENSION(ncols,nrows),intent(in) :: lai
REAL,DIMENSION(ncols,nrows),intent(out) :: gam_l

gam_l = (0.49*lai) / ( (1+0.2*(lai**2))**0.5 )

END SUBROUTINE gamma_lai
!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
!.....2) Calculate GAM_P (GAMMA_P)
!-----------------------------------------------------------------------
!             GAMMA_P = 0.0         a<=0, a>=180, sin(a) <= 0.0
!
!             GAMMA_P = sin(a)[ 2.46*(1+0.0005(Pdaily-400))*PHI - 0.9*PHI^2 ]
!                                   0<a<180, sin(a) > 0.0
!           where PHI    = above canopy PPFD transmission (non-dimension)
!                 Pdaily = daily average above canopy PPFD (umol/m2s)
!                 a      = solar angle (degree)
!
!                 Note: AAA = 2.46*BBB*PHI - 0.9*PHI^2
!                       BBB = (1+0.0005(Pdaily-400))
!                       GAMMA_P = sin(a)*AAA
!
!                       Pac
!             PHI = -----------
!                   sin(a)*Ptoa
!           where Pac  = above canopy PPFD (umol/m2s)
!                 Ptoa = PPFD at the top of atmosphere (umol/m2s)
!
!             Pac =  SRAD * 4.766 mmmol/m2-s * 0.5
!
!             Ptoa = 3000 + 99*cos[2*3.14-( DOY-10)/365 )]
!           where DOY = day of year
!
!     SUBROUTINE GAMMA_P returns the GAMMA_P values
!-----------------------------------------------------------------------

SUBROUTINE gamma_p( jdate, jtime, lat, long,  &
    ncols, nrows, ppfd, d_ppfd, gam_p )
IMPLICIT NONE

INTEGER,intent(in)   :: jdate
INTEGER,intent(iN)   :: jtime
INTEGER,intent(in)   :: ncols
INTEGER,INTENT(IN)   :: nrows
REAL,DIMENSION(ncols,nrows),INTENT(IN) :: long
REAL,DIMENSION(ncols,nrows),INTENT(IN) :: d_ppfd
REAL,DIMENSION(ncols,nrows),intent(in) :: lat
! Photosynthetic Photon Flux Density: instantaneous, daily
REAL,DIMENSION(ncols,nrows),intent(in) :: ppfd
REAL,DIMENSION(ncols,nrows),intent(out) :: gam_p          ! GAMMA_P

! Local parameters
CHARACTER (LEN=256) :: mesg          ! message buffer
CHARACTER (LEN=16) :: funcname = 'GAMMA_P'

INTEGER,DIMENSION(ncols,nrows) :: day    ! DAY is DOY (JDATE)
REAL :: aaa, bbb
REAL :: beta                           ! Solar zenith angle
REAL :: sinbeta                        ! sin(beta)
REAL,DIMENSION(ncols,nrows) :: hour      ! HOUR is solar hour
REAL,DIMENSION(ncols,nrows) :: szangle   ! Solar zenith angle array
REAL,DIMENSION(ncols,nrows) :: ptoa, pac, phi

INTEGER :: i, j

!...  Constants
REAL,PARAMETER :: pi = 3.14159, d2rad = pi/180.0, rad2d = 180.0/pi

!...  Convert date and time format to local time
! DAY is Julian day
day(:,:)  =  MOD(jdate,1000)

! Convert from XXXXXX format to XX.XX (solar hour)
! HOUR = 0 -> 23.xx
! Solar hour
hour = jtime/10000 + long/15

where ( hour < 0.0 )
hour = hour + 24.0
day = day - 1
endwhere

!...  Begin estimating gamma_p
! getting solar radiation
pac = ppfd

!...  Initialize parameters
szangle = 0.
ptoa = 0.
phi = 0.

DO i = 1, ncols
  DO j = 1, nrows
! Get solar elevation angle
    CALL solarangle(day(i,j),hour(i,j),lat(i,j),sinbeta)
    beta = ASIN(sinbeta)*rad2d            ! Degree
    szangle(i,j) =  beta                  ! Degree
    IF (sinbeta <= 0.0) THEN
      gam_p(i,j) = 0.0
    ELSE IF (sinbeta > 0.0) THEN
      ptoa(i,j) = 3000.0 + 99.0 *COS(2*3.14*(day(i,j)-10)/365)
      
      phi(i,j) = pac(i,j)/(sinbeta * ptoa(i,j))
      
      bbb = 1 + 0.0005*( d_ppfd(i,j)-400 )
      aaa = ( 2.46 * bbb * phi(i,j) ) - ( 0.9 * phi(i,j)**2 )
      
      gam_p(i,j) = sinbeta * aaa
    ELSE
      mesg = 'Error: Solar angle is invalid'
      CALL m3exit(funcname,jdate,jtime,mesg,2)
      
    END IF
    
! Screening the unforced errors
! IF solar elevation angle is less than 1 THEN
! gamma_p can not be greater than 0.1.
    IF (beta < 1.0 .AND. gam_p(i,j) > 0.1) THEN
      gam_p(i,j) = 0.0
    END IF
    
  END DO    ! End loop for NROWS
END DO      ! End loop for NCOLS

RETURN
END SUBROUTINE gamma_p
!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
!.....3) Calculate GAM_T (GAMMA_T) for isoprene
!-----------------------------------------------------------------------
!                          Eopt*CT2*exp(CT1*x)
!             GAMMA_T =  ------------------------
!                        [CT2-CT1*(1-exp(CT2*x))]
!           where x      = [ (1/Topt)-(1/Thr) ] / 0.00831
!                 Eopt   = 1.75*exp(0.08(Tdaily-297)
!                 CT1    = 80
!                 CT2    = 200
!                 Thr    = hourly average air temperature (K)
!                 Tdaily = daily average air temperature (K)
!                 Topt   = 313 + 0.6(Tdaily-297)
!
!                 Note: AAA = Eopt*CT2*exp(CT1*x)
!                       BBB = [CT2-CT1*(1-exp(CT2*x))]
!                       GAMMA_T = AAA/BBB
!
!     SUBROUTINE GAMMA_TLD returns the GAMMA_T value for isoprene
!-----------------------------------------------------------------------

SUBROUTINE gamma_tld( ncols, nrows, temp, d_temp, gam_t,spc_name )

IMPLICIT NONE
INTEGER,intent(in)  :: ncols
INTEGER,intent(in)  :: nrows

INTEGER,EXTERNAL :: index1

REAL,DIMENSION(ncols,nrows),intent(in) :: temp,d_temp   ! daily, hourly surface temperature
REAL,DIMENSION(ncols,nrows),intent(out) :: gam_t       ! GAMMA_T
CHARACTER (LEN=16),intent(in) :: spc_name

REAL,PARAMETER :: ct2 =200.0
! Local parameters
INTEGER :: spcnum
REAL,DIMENSION(ncols,nrows) :: eopt, topt, x, aaa, bbb

spcnum = index1(spc_name,n_mgn_spc,mgn_spc)

eopt = cceo(spcnum) * EXP(0.08*(d_temp-297.0))
topt = 313.0 + ( 0.6*(d_temp-297.0) )
x = ( (1/topt)-(1/temp) ) / 0.00831

aaa = eopt*ct2*EXP(ct1(spcnum)*x)
bbb = ( ct2-ct1(spcnum)*( 1-EXP(ct2*x) ) )
gam_t = aaa/bbb

RETURN
END SUBROUTINE gamma_tld
!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
!.....4) Calculate GAM_T (GAMMA_T) for non-isoprene
!-----------------------------------------------------------------------
!
!             GAMMA_T =  exp[TDP_FCT*(T-Ts)]
!           where TDP_FCT = temperature dependent parameter ('beta')
!                 Ts     = standard temperature (normally 303K, 30C)
!
!     SUBROUTINE GAMMA_TLI returns the GAMMA_T value for non-isoprene
!-----------------------------------------------------------------------

SUBROUTINE gamma_tli( ncols, nrows, spcnam, temp, gam_t)

IMPLICIT NONE
integer, INTENT(IN)                  :: ncols
INTEGER, INTENT(IN)                  :: nrows
CHARACTER (LEN=16), INTENT(IN)       :: spcnam
REAL,INTENT(IN),DIMENSION(ncols,nrows) :: temp
REAL,DIMENSION(ncols,nrows), INTENT(OUT) :: gam_t

INTEGER,EXTERNAL :: index1

INTEGER :: spcnum                             ! Species number

REAL,PARAMETER :: ts = 303.0

!--end of declarations--

spcnum = index1(spcnam,n_tdf_spc,tdf_spc)
gam_t = EXP( tdf_prm(spcnum)*(temp-ts) )

RETURN
END SUBROUTINE gamma_tli
!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
!.....5) Calculate GAM_A (GAMMA_age)
!-----------------------------------------------------------------------
!
!             GAMMA_age = Fnew*Anew + Fgro*Agro + Fmat*Amat + Fold*Aold
!           where Fnew = new foliage fraction
!                 Fgro = growing foliage fraction
!                 Fmat = mature foliage fraction
!                 Fold = old foliage fraction
!                 Anew = relative emission activity for new foliage
!                 Agro = relative emission activity for growing foliage
!                 Amat = relative emission activity for mature foliage
!                 Aold = relative emission activity for old foliage
!
!
!             For foliage fraction
!             Case 1) LAIc = LAIp
!             Fnew = 0.0  , Fgro = 0.1  , Fmat = 0.8  , Fold = 0.1
!
!             Case 2) LAIp > LAIc
!             Fnew = 0.0  , Fgro = 0.0
!             Fmat = 1-Fold
!             Fold = (LAIp-LAIc)/LAIp
!
!             Case 3) LAIp < LAIc
!             Fnew = 1-(LAIp/LAIc)                       t <= ti
!                  = (ti/t) * ( 1-(LAIp/LAIc) )          t >  ti
!
!             Fmat = LAIp/LAIc                           t <= tm
!                  = (LAIp/LAIc) +
!                      ( (t-tm)/t ) * ( 1-(LAIp/LAIc) )  t >  tm
!
!             Fgro = 1 - Fnew - Fmat
!             Fold = 0.0
!
!           where
!             ti = 5 + (0.7*(300-Tt))                   Tt <= 303
!                = 2.9                                  Tt >  303
!             tm = 2.3*ti
!
!             t  = length of the time step (days)
!             ti = number of days between budbreak and the induction of
!                  emission
!             tm = number of days between budbreak and the initiation of
!                  peak emissions rates
!             Tt = average temperature (K) near top of the canopy during
!                  current time period (daily ave temp for this case)
!
!
!             For relative emission activity
!             Case 1) Constant
!             Anew = 1.0  , Agro = 1.0  , Amat = 1.0  , Aold = 1.0
!
!             Case 2) Monoterpenes
!             Anew = 2.0  , Agro = 1.8  , Amat = 0.95 , Aold = 1.0
!
!             Case 3) Sesquiterpenes
!             Anew = 0.4  , Agro = 0.6  , Amat = 1.075, Aold = 1.0
!
!             Case 4) Methanol
!             Anew = 3.0  , Agro = 2.6  , Amat = 0.85 , Aold = 1.0
!
!             Case 5) Isoprene
!             Anew = 0.05 , Agro = 0.6  , Amat = 1.125, Aold = 1.0
!
!     SUBROUTINE GAMMA_A returns GAMMA_A
!-----------------------------------------------------------------------

SUBROUTINE gamma_a( jdate, jtime, ncols, nrows, spc_name,  &
    laiarp, laiarc, tstlen, d_temp, gam_a )
IMPLICIT NONE

INTEGER,intent(in)    :: jdate
INTEGER,intent(in)    :: jtime
INTEGER,intent(in)    :: ncols
INTEGER,intent(in)    :: nrows
REAL,DIMENSION(ncols,nrows), INTENT(IN)     :: laiarp
REAL,DIMENSION(ncols,nrows), INTENT(IN)     :: laiarc
INTEGER,INTENT(IN)        :: tstlen

INTEGER,EXTERNAL :: index1

CHARACTER (LEN=16),intent(in) :: spc_name
REAL,DIMENSION(ncols,nrows),intent(in) :: d_temp
REAL,DIMENSION(ncols,nrows),intent(out) :: gam_a

! Local parameters
CHARACTER (LEN=16) :: funcname = 'GAMMA_A'

INTEGER :: aindx                            ! relative emission acitivity index
INTEGER :: spcnum
INTEGER :: t                                ! time step
CHARACTER (LEN=256) :: mesg                       ! message buffer

REAL,DIMENSION(ncols,nrows) :: fnew, fgro, fmat, fold
REAL,DIMENSION(ncols,nrows) :: laip,laic    ! LAI at previous,current time steps
REAL,DIMENSION(ncols,nrows) :: ti,tm        ! number of days between budbreak
! and induction of emission,
! initiation of peak emissions rates
REAL,DIMENSION(ncols,nrows) :: tt           ! daily average temperature (K)

!...  Choose relative emission activity
!--------code by Xuemei Wang 11/04/2007----------------
spcnum = index1(spc_name,n_mgn_spc,mgn_spc)
aindx =  rea_index(spcnum)
!---------------------------------------------------
! local parameter arrays
t = tstlen
laic = laiarc
laip = laiarp
tt   = d_temp

!... Calculate foliage fraction

where (laip < laic)

!        Calculate ti and tm
where (tt <= 303.0)
ti = 5.0 + 0.7*(300-tt)
elsewhere (tt > 303.0)
ti = 2.9
endwhere
tm = 2.3*ti

!       Calculate Fnew and Fmat, then Fgro and Fold
!       Fnew
where (ti >= t)
fnew = 1.0 - (laip/laic)
elsewhere (ti < t)
fnew = (ti/t) * ( 1-(laip/laic) )
endwhere

!       Fmat
where (tm >= t)
fmat = laip/laic
elsewhere (tm < t)
fmat = (laip/laic) + ( (t-tm)/t ) * ( 1-(laip/laic) )
endwhere

fgro = 1.0 - fnew - fmat
fold = 0.0

elsewhere (laip == laic)

fnew = 0.0
fgro = 0.1
fmat = 0.8
fold = 0.1

elsewhere (laip > laic)

fnew = 0.0
fgro = 0.0
fold = ( laip-laic ) / laip
fmat = 1-fold

endwhere

!...  Calculate GAMMA_A
gam_a = fnew*anew(aindx) + fgro*agro(aindx) +  &
    fmat*amat(aindx) + fold*aold(aindx)

RETURN
END SUBROUTINE gamma_a

!-----------------------------------------------------------------------
!.....6) Calculate GAM_SMT (GAMMA_SM)
!-----------------------------------------------------------------------
!
!             GAMMA_SM =     1.0   (non-dimension)
!
!
!     SUBROUTINE GAMMA_S returns the GAMMA_SM values
!-----------------------------------------------------------------------

SUBROUTINE gamma_s( ncols, nrows, gam_s )
IMPLICIT NONE

integer, INTENT(IN)                  :: ncols
INTEGER, INTENT(IN)                  :: nrows
REAL,DIMENSION(ncols,nrows),INTENT(OUT) :: gam_s

gam_s = 1.0

RETURN
END SUBROUTINE gamma_s

!=======================================================================
!-----------------------------------------------------------------------
!.....7) Calculate GAM_CO2(GAMMA_CO2)
!-----------------------------------------------------------------------
!
!             GAMMA_CO2 =     1.0   (non-dimension)
!             When CO2 =400ppm
!
!     SUBROUTINE GAM_CO2 returns the GAMMA_CO2 values
!    Xuemei Wang-2009-06-22
!-----------------------------------------------------------------------

SUBROUTINE gamma_co2( ncols, nrows,co2, gam_co2 )

IMPLICIT NONE
INTEGER, INTENT(IN)                  :: ncols
INTEGER, INTENT(IN)                  :: nrows
REAL,DIMENSION(ncols,nrows),INTENT(IN) :: co2
REAL, DIMENSION(ncols,nrows), INTENT(OUT)  :: gam_co2

REAL,DIMENSION(ncols,nrows) :: ci

REAL,PARAMETER :: ismax = 1.344, h=1.4614
REAL,PARAMETER :: cstar =585


ci = 0.7* co2
where (co2 == 400.0)
gam_co2 = 1.0
elsewhere
gam_co2 = ismax- ((ismax*ci**h) /(cstar**h+ci**h))
endwhere

RETURN
END SUBROUTINE gamma_co2

!=======================================================================
!=======================================================================
!-----------------------------------------------------------------------
!.....8) Calculate GAMMA_LAIbidir(gam_LAIbidir,LAI)
!-----------------------------------------------------------------------
!From Alex Guenther 2010-01-26
!If lai < 2 Then
!gammaLAIbidir= 0.5 * lai
!ElseIf lai <= 6 Then
!gammaLAIbidir= 1 - 0.0625 * (lai - 2)
!Else
!gammaLAIbidir= 0.75
!End If
!
!     SUBROUTINE GAMMA_LAIbidir returns the gam_LAIbidir values
!    Xuemei Wang-2010-01-28
!
!-----------------------------------------------------------------------

SUBROUTINE gamma_laibidir(ncols, nrows,lai,gam_laibidir)

IMPLICIT NONE
INTEGER,INTENT(IN)        :: ncols
INTEGER,INTENT(IN)        :: nrows
REAL,DIMENSION(ncols, nrows),intent(in) ::  lai
REAL,DIMENSION(ncols, nrows),intent(out) :: gam_laibidir

INTEGER :: i,j


DO i = 1,ncols
  DO j = 1, nrows
    IF(lai(i,j) < 2) THEN
      gam_laibidir =  0.5 * lai
    ELSE IF (lai(i,j) <= 6 .AND. lai(i,j) >= 2) THEN
      gam_laibidir = 1 - 0.0625 * (lai(i,j) - 2)
    ELSE
      gam_laibidir = 0.75
    END IF
    
  END DO
END DO

RETURN
END  SUBROUTINE gamma_laibidir
!=======================================================================

END module gamma_etc
