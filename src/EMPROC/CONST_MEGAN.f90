MODULE const_megan
INTEGER,PARAMETER ::  nmon = 12
 
INTEGER,PARAMETER ::  nrtyp = 16, nrcha = 16
! 1  = canopy depth
! 2  = leaf width
! 3  = leaf length
! 4  = canopy height
! 5  = scattering coefficient for PPFD
! 6  = scattering coefficient for near IR
! 7  = reflection coefficient for diffuse PPFD
! 8  = reflection coefficient for diffuse near IR
! 9  = clustering coefficient (accounts for leaf clumping influence on mean
!    projected leaf area in the direction of the suns beam)
!    use 0.85 for default, corn=0.4-0.9; Pine=0.6-1.0; oak=0.53-0.67;
!    tropical rainforest=1.1
! 10 = leaf IR emissivity
! 11 = leaf stomata and cuticle factor: 1=hypostomatous, 2=amphistomatous,
!     1.25=hypostomatous but with some transpiration through cuticle
! 12 = daytime temperature lapse rate (K m-1)
! 13 = nighttime temperature lapse rate (K m-1)
! 14 = warm (>283K) canopy total humidity change (Pa)
! 15 = cool (>= 283K) canopy total humidity change (Pa)
! 16 = normalized canopy depth where wind is negligible
!     NT NT NT TF BT TF BT BT SB SB SB HB HB HB CR CR
REAL,DIMENSION(nrcha,nrtyp) :: canopychar = reshape(  &
    (/ 16.   , 16.   , 16.  ,    16., 16.   , 16.   , 16.  ,    16. ,  &
    1.   , 1.    ,  1.  ,    0.756, 0.756,  0.756 ,  1.  ,    1  ,  &
    0.05,  0.05 ,   0.05,    0.05, 0.05,  0.05 ,   0.05,    0.05,  &
    0.015,   0.015, 0.015,   0.015, 0.015,   0.015, 0.02 ,   0.02 ,  &
    0.1  ,  0.1  ,  0.1  ,   0.1  , 0.1  ,  0.1  ,  0.1  ,   0.1  ,  &
    0.1  ,  0.1  ,  0.1  ,   0.15 , 0.15 ,  0.15 ,  0.15 ,   0.15 ,  &
    24.  , 24.   ,  24 ,     24   , 24.  , 24.   ,  24 ,     24   ,  &
    2.   , 2     ,  2  ,     0.75 , 0.75 , 0.75  ,  1. ,     1    ,  &
    0.2  ,  0.2  ,  0.2  ,   0.2  , 0.2  ,  0.2  ,  0.2  ,   0.2  ,  &
    0.2  ,  0.2  ,  0.2  ,   0.2  , 0.2  ,  0.2  ,  0.2  ,   0.2  ,  &
    0.8  ,  0.8  ,  0.8  ,   0.8  , 0.8  ,  0.8  ,  0.8  ,   0.8  ,  &
    0.8  ,  0.8  ,  0.8  ,   0.8  , 0.8  ,  0.8  ,  0.8  ,   0.8  ,  &
    0.057,  0.057,  0.057,   0.057, 0.057,  0.057,  0.057,   0.057,  &
    0.057,  0.057,  0.057,   0.057, 0.057,  0.057,  0.057,   0.057,  &
    0.389,  0.389,  0.389,   0.389, 0.389,  0.389,  0.389,   0.389,  &
    0.389,  0.389,  0.389,   0.389, 0.389,  0.389,  0.389,   0.389,  &
    0.85 ,  0.85 ,  0.85 ,   1.1  , 0.95 ,  1.1  ,  0.95 ,   0.95 ,  &
    0.85 ,  0.85 ,  0.85 ,   0.76 , 0.76 ,  0.76 ,  0.65 ,   0.65 ,  &
    0.95 ,  0.95 ,  0.95 ,   0.95 , 0.95 ,  0.95 ,  0.95 ,   0.95 ,  &
    0.95 ,  0.95 ,  0.95 ,   0.95 , 0.95 ,  0.95 ,  0.95 ,   0.95 ,  &
    1.25 ,  1.25 ,  1.25 ,   1.25 , 1.25 ,  1.25 ,  1.25 ,   1.25 ,  &
    1.00 ,  1.00 ,  1.00 ,   1.25 , 1.25 ,  1.25 ,  1.25 ,   1.25 ,  &
    0.06 ,  0.06 ,  0.06 ,   0.06 , 0.06 ,  0.06 ,  0.06 ,   0.06 ,  &
    0.06 ,  0.06 ,  0.06 ,   0.06 , 0.06 ,  0.06 ,  0.06 ,   0.06 ,  &
    -0.06 , -0.06 , -0.06 ,  -0.06 , -0.06 , -0.06 , -0.06 ,  -0.06 ,  &
    -0.06 , -0.06 , -0.06 ,  -0.06 , -0.06 , -0.06 , -0.06 ,  -0.06 ,  &
    700. ,  700. ,  700. ,   700. , 700. ,  700. ,  700. ,   700. ,  &
    700. ,  700. ,  700. ,   700. , 700. ,  700. ,  700. ,   700. ,  &
    150. ,  150. ,  150. ,   150. , 150. ,  150. ,  150. ,   150. ,  &
    150. ,  150. ,  150. ,   150. , 150. ,  150. ,  150. ,   150. ,  &
    0.7  ,  0.7  ,  0.7  ,   0.7  , 0.7  ,  0.7  ,  0.7  ,   0.7  ,  &
    0.7  ,  0.7  ,  0.7  ,   0.7  , 0.7  ,  0.7  ,  0.7  ,   0.7    /)  &
    ,shape=(/nrcha,nrtyp/) ,order=(/2,1/)                      )

END MODULE const_megan