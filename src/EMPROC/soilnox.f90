
SUBROUTINE soilnox( jdate, jtime, nx, ny, ta, lsoil, isltyp, soilm, soilt,  &
    laic, lat, precadj,  &
    cfno, cfnog )

!***********************************************************************
!  DESCRIPTION:
!
!     Uses new NO algorithm NO = Normalized*Tadj*Padj*Fadj*Cadj
!     to estimate NO emissions
!     Information needed to estimate NO emissions
!     Julian Day          (integer)    JDATE
!     Surface Temperature (MCIP field) TA    (K)
!     Soil Moisture       (MCIP field) SOILM (M**3/M**3) (LSOIL)
!          (ratio of volume of water per volume of soil)
!     Soil Temperature    (MCIP field) SOILT (K)         (LSOIL)
!     Soil Type           (MCIP field) ISLTYP            (LSOIL)
!
!     saturation values for soil types (constants)       (LSOIL)
!     FOR PX Version, the Temperature adjustment factor accounts for wet and dry soils
!                and  the precipitation adjustment factor accounts for saturated soils
!     FOR the non-PX version, the basic algorithm remains with a temperature adjustment factor (dry soil)
!                     and no adjustment for saturated soils
!
!
!     The following arrays are updated after each call to SOILNOX
!     PULTYPE   type of NO emission pulse
!     PULSEDATE julian date for the beginning of an NO pulse
!     PULSETIME        time for the beginning of an NO pulse
!
!     The calculation are based on the following paper by J.J. Yienger and H. Levy II
!     J.J. Yienger and H. Levy II, Journal of Geophysical Research, vol 100,11447-11464,1995
!
!     The Temperature Adjustment Factor is based on section 4.2 for wet and dry soils with
!       the following modification (PX version):
!       Instead of classifying soils as either 'wet' or 'dry', the wet and dry adjustment is
!       calculated at each grid cell.  A linear interpolation between the wet and dry adjustment
!       factor is made using the relative amount of soil moisture in the top layer (1cm)
!       as the interpolating factor.  The relative amount of soil moisture is determined by
!       taking the MCIP soil moisture field and dividing by the saturation value defined for each
!       soil type in the PX version of MCIP
!       the soil temperature is used in PX version
!
!     The Precipation Adjustment factor is based on section 4.1 with the following modifications.
!       The rainrate is computed from the MCIP directly using a 24 hr daily total.
!       THe types of Pulses as described in YL95 were used to estimate the NO emission
!       rate.
!
!    Also see the following paper for more information:
!    Proceedings of the Air and Waste Management Association/U.S. Environmental Protection
!    Agency EMission Inventory Conference, Raleigh October 26-28, 1999 Raleigh NC
!    by Tom Pierce and Lucille Bender
!
!    REFERENCES
!
!    JACQUEMIN B. AND NOILHAN J. (1990), BOUND.-LAYER METEOROL., 52, 93-134.
!    J.J. Yienger and H. Levy II, Journal of Geophysical Research, vol 100,11447-11464,1995
!    T. Pierce and L. Bender, Examining the Temporal Variability of Ammonia and Nitric Oxide Emissions from Agricultural Processes
!       Proceedings of the Air and Waste Management Association/U.S. Environmental Protection
!        Agency EMission Inventory Conference, Raleigh October 26-28, 1999 Raleigh NC
!
!  PRECONDITIONS REQUIRED:
!     Normalized NO emissions, Surface Temperature, Soil Moisture, Soil type,
!     NO emission pulse type, soil moisture from previous time step, julian date
!     of NO emission pulse start, time of NO emission pulse start,
!     soil type, SOIL TYPES, Land use data

!  SUBROUTINES AND FUNCTIONS CALLED (directly or indirectly):
!     FERTILIZER_ADJ computes fertlizer adjustment factor
!     VEG_ADJ        computes vegatation adjustment factor
!     GROWSEASON     computes day of growing season

!  REVISION  HISTORY:
!    10/01 : Prototype by GAP
!    10/03 : modified transition to non growing season for jul-oct of the year
!    08/04 : Converted to SMOKE code style by C. Seppanen
!    07/21/11 : Imported form SMOKE-BEIS v3.14 for MEGAN v2.10

!***********************************************************************

use soilnox_fx
USE PARMS3      ! I/O API constants
USE FDESC3      ! I/O API file description data structure
USE IODECL3     ! I/O API function declarations
IMPLICIT NONE

INTEGER,intent (in)  :: nx  !  no. columns
INTEGER,intent (in)  :: ny  !  no. rows
REAL,intent (in)     :: ta( nx, ny )!  air temperature (K)
INTEGER,intent (in)  :: jdate   !  current simulation date (YYYYDDD)
INTEGER,intent (in)  :: jtime   !  current simulation time (HHMMSS)
LOGICAL,intent (in)  :: lsoil   ! true: using PX version of MCIP
INTEGER,intent (in)  :: isltyp( nx, ny ) !  soil type
REAL,intent (in)     :: soilm( nx, ny )  !  soil moisture (m3/m3)
REAL,intent (in)     :: soilt( nx, ny )  !  soil temperature (K)
REAL,intent (in)     :: laic( nx, ny )   !  soil temperature (K)
REAL,intent (in)     :: lat( nx, ny )    !  Latitude
REAL,intent (in)     :: precadj( nx, ny )!  precip adjustment
REAL,intent (in out) :: cfno( nx, ny )   !  NO correction factor
REAL,intent (in out) :: cfnog( nx, ny )  !  NO correction factor for grass

!.........  Local ARRAYS
! Saturation values for 11 soil types from pxpbl.F  (MCIP PX version)
!       PLEIM-XIU LAND-SURFACE AND PBL MODEL (PX-LSM)
! See JACQUEMIN B. AND NOILHAN J. (1990), BOUND.-LAYER METEOROL., 52, 93-134.
INTEGER,PARAMETER :: maxstypes = 11
REAL :: saturation( maxstypes )
DATA saturation / 0.395, 0.410, 0.435, 0.485, 0.451, 0.420, 0.477, 0.476,  &
    0.426, 0.482, 0.482        /

!.........  SCRATCH LOCAL VARIABLES and their descriptions:
INTEGER :: r, c, l      ! counters
INTEGER :: soilcat      ! soil category

REAL :: cf           ! NO correction factor
REAL :: cfg          ! NO correction factor for grasslands
REAL :: tair         ! surface temperature
REAL :: tsoi         ! soil temperature
REAL :: cfnowet, cfnodry, ratio


CHARACTER (LEN=256) :: mesg         ! message buffer

CHARACTER (LEN=16)  :: progname = 'SOILNOX'   !  program name

!***********************************************************************


!.....  Loop through cells
DO r = 1, ny
  DO c = 1, nx
    
    tair = ta( c, r )         ! unit in degree K
    
!.......  Check max and min bounds for temperature
    IF (tair < 200.0) THEN
      WRITE( mesg, 94010 ) 'TAIR=', tair, 'out of range at (C,R)=', c, r
      CALL m3exit( progname, jdate, jtime, mesg, 2 )
    END IF
    
    IF (tair > 315.0 ) THEN
      WRITE( mesg, 94020 ) 'TAIR=', tair, 'out of range at (C,R)=', c, r,  &
          ' resetting to 315K'
      CALL m3warn( progname, jdate, jtime, mesg )
      tair = 315.0
    END IF
    
!.......  CFNOG
    IF( tair > 303.00 ) tair = 303.00
    
    IF ( tair > 268.8690 ) THEN
      cfg = EXP( 0.04686 * tair - 14.30579 ) ! grass (from BEIS2)
    ELSE
      cfg = 0.0
    END IF
    
    cfnog(c,r) = cfg
    
!.......  CFNO
    IF( .NOT. lsoil ) THEN
! no soil
      
      tsoi = 0.72 * tair + 82.28
      IF (tsoi <= 273.16) tsoi = 273.16
      IF (tsoi >= 303.16) tsoi = 303.16
      
      cfnodry = (1./3.) * (1./30.) * (tsoi-273.16)  ! see YL 1995 Equa 9a p. 11452
      IF (tsoi <= 283.16) THEN         ! linear cold case
        cfnowet = (tsoi-273.16)*EXP(-0.103*30.0)*0.28 ! see YL 1995 Equ 7b
      ELSE                             ! exponential case
        cfnowet = EXP(0.103 * (tsoi-273.16)) * EXP(-0.103 * 30.0)
      END IF
      cf = 0.5 * cfnowet + 0.5 * cfnodry
      
    ELSE
! soil
      
      tsoi = soilt( c,r )
      IF (tsoi <= 273.16) tsoi = 273.16
      IF (tsoi >= 303.16) tsoi = 303.16
      
      cfnodry = (1./3.)*(1./30.)*(tsoi-273.16)  ! see YL 1995 Equa 9a p. 11452
      IF (tsoi <= 283.16) THEN         ! linear cold case
        cfnowet = (tsoi-273.16)*EXP(-0.103*30.0)*0.28 ! see YL 1995 Equ 7b
      ELSE                             ! exponential case
        cfnowet = EXP(0.103 * (tsoi-273.16)) * EXP(-0.103 * 30.0)
      END IF
      
      soilcat = INT( isltyp( c,r ) )
      IF( soilcat > 0 .AND. soilcat <= maxstypes ) THEN
        ratio = soilm( c,r ) / saturation( soilcat )
        cf = ratio*cfnowet + (1.-ratio)*cfnodry
      ELSE
        cf = 0.
      END IF
      
    END IF  ! Endif LSOIL
    
    cfno(c,r) = cf * fertlz_adj( jdate, lat(c,r) ) *  &
        veg_adj( laic(c,r) ) * precadj(c,r)
    
    
  END DO  ! loop over columns
END DO  ! loop over rows
!******************  FORMAT  STATEMENTS   ******************************
94010   FORMAT( a, f10.2, 1X, a, i3, ',', i3 )
94020   FORMAT( a, f10.2, 1X, a, i3, ',', i3, a )

RETURN

END SUBROUTINE soilnox

