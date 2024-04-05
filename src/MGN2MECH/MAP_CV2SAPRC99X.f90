module map_cv2saprc99x
!=======================================================================
!  MAP_SAPRC99_X_CV2SAPRC99X.EXT
!  This include file contains conversion table for 138 speciated species
!  to SAPRC99 (CAMx) species


!  MEGAN v2.10
!  INPUT version x.x

!  History:
!  Who          When       What
!  ---------------------------------------------------------------------
!  bkoo         04/13/07 - Created
!  bkoo         05/23/07 - Modified speciation
!  Tan          07/18/11 - Updated for MEGANv2.10
!=======================================================================

CHARACTER (LEN=16) :: map_saprc99_xmech
PARAMETER     (map_saprc99_xmech = 'SAPRC99X        ')

INTEGER :: n_saprc99_x
PARAMETER     (n_saprc99_x = 145)        ! Number of map species

CHARACTER (LEN=16) :: spmh_nam_saprc99_x( n_saprc99_x )   ! speciated species name
INTEGER :: spmh_map_saprc99_x( n_saprc99_x )   ! speciated species name
! mapped to SPC_SPCAT.EXT
CHARACTER (LEN=16) :: mech_nam_saprc99_x( n_saprc99_x )   ! mechanism species
INTEGER :: mech_map_saprc99_x( n_saprc99_x )   ! mechanism species mapped
! to SPC_CB4Q.EXT
REAL :: conv_fac_saprc99_x( n_saprc99_x )   ! conversion factor


DATA  spmh_nam_saprc99_x(  1)     , spmh_map_saprc99_x(  1)  &
    / 'isoprene        ', 1             /
DATA  mech_nam_saprc99_x(  1)     , mech_map_saprc99_x(  1)  &
    / 'ISOP            ', 1             /
DATA  conv_fac_saprc99_x(  1) / 1.00          /

DATA  spmh_nam_saprc99_x(  2)     , spmh_map_saprc99_x(  2)  &
    / 'myrcene         ', 2             /
DATA  mech_nam_saprc99_x(  2)     , mech_map_saprc99_x(  2)  &
    / 'TERP            ', 2             /
DATA  conv_fac_saprc99_x(  2) / 1.00          /

DATA  spmh_nam_saprc99_x(  3)     , spmh_map_saprc99_x(  3)  &
    / 'sabinene        ', 3             /
DATA  mech_nam_saprc99_x(  3)     , mech_map_saprc99_x(  3)  &
    / 'TERP            ', 2             /
DATA  conv_fac_saprc99_x(  3) / 1.00          /

DATA  spmh_nam_saprc99_x(  4)     , spmh_map_saprc99_x(  4)  &
    / 'limonene        ', 4             /
DATA  mech_nam_saprc99_x(  4)     , mech_map_saprc99_x(  4)  &
    / 'TERP            ', 2             /
DATA  conv_fac_saprc99_x(  4) / 1.00          /

DATA  spmh_nam_saprc99_x(  5)     , spmh_map_saprc99_x(  5)  &
    / 'carene_3        ', 5             /
DATA  mech_nam_saprc99_x(  5)     , mech_map_saprc99_x(  5)  &
    / 'TERP            ', 2             /
DATA  conv_fac_saprc99_x(  5) / 1.00          /

DATA  spmh_nam_saprc99_x(  6)     , spmh_map_saprc99_x(  6)  &
    / 'ocimene_t_b     ', 6             /
DATA  mech_nam_saprc99_x(  6)     , mech_map_saprc99_x(  6)  &
    / 'TERP            ', 2             /
DATA  conv_fac_saprc99_x(  6) / 1.00          /

DATA  spmh_nam_saprc99_x(  7)     , spmh_map_saprc99_x(  7)  &
    / 'pinene_b        ', 7             /
DATA  mech_nam_saprc99_x(  7)     , mech_map_saprc99_x(  7)  &
    / 'TERP            ', 2             /
DATA  conv_fac_saprc99_x(  7) / 1.00          /

DATA  spmh_nam_saprc99_x(  8)     , spmh_map_saprc99_x(  8)  &
    / 'pinene_a        ', 8             /
DATA  mech_nam_saprc99_x(  8)     , mech_map_saprc99_x(  8)  &
    / 'TERP            ', 2             /
DATA  conv_fac_saprc99_x(  8) / 1.00          /

DATA  spmh_nam_saprc99_x(  9)     , spmh_map_saprc99_x(  9)  &
    / '2met_styrene    ', 9             /
DATA  mech_nam_saprc99_x(  9)     , mech_map_saprc99_x(  9)  &
    / 'OLE2            ', 3             /
DATA  conv_fac_saprc99_x(  9) / 1.00          /

DATA  spmh_nam_saprc99_x( 10)     , spmh_map_saprc99_x( 10)  &
    / 'cymene_p        ', 10            /
DATA  mech_nam_saprc99_x( 10)     , mech_map_saprc99_x( 10)  &
    / 'ARO2            ', 4             /
DATA  conv_fac_saprc99_x( 10) / 1.00          /

DATA  spmh_nam_saprc99_x( 11)     , spmh_map_saprc99_x( 11)  &
    / 'cymene_o        ', 11            /
DATA  mech_nam_saprc99_x( 11)     , mech_map_saprc99_x( 11)  &
    / 'ARO2            ', 4             /
DATA  conv_fac_saprc99_x( 11) / 1.00          /

DATA  spmh_nam_saprc99_x( 12)     , spmh_map_saprc99_x( 12)  &
    / 'phellandrene_a  ', 12            /
DATA  mech_nam_saprc99_x( 12)     , mech_map_saprc99_x( 12)  &
    / 'TERP            ', 2             /
DATA  conv_fac_saprc99_x( 12) / 1.00          /

DATA  spmh_nam_saprc99_x( 13)     , spmh_map_saprc99_x( 13)  &
    / 'thujene_a       ', 13            /
DATA  mech_nam_saprc99_x( 13)     , mech_map_saprc99_x( 13)  &
    / 'TERP            ', 2             /
DATA  conv_fac_saprc99_x( 13) / 1.00          /

DATA  spmh_nam_saprc99_x( 14)     , spmh_map_saprc99_x( 14)  &
    / 'terpinene_a     ', 14            /
DATA  mech_nam_saprc99_x( 14)     , mech_map_saprc99_x( 14)  &
    / 'TERP            ', 2             /
DATA  conv_fac_saprc99_x( 14) / 1.00          /

DATA  spmh_nam_saprc99_x( 15)     , spmh_map_saprc99_x( 15)  &
    / 'terpinene_g     ', 15            /
DATA  mech_nam_saprc99_x( 15)     , mech_map_saprc99_x( 15)  &
    / 'TERP            ', 2             /
DATA  conv_fac_saprc99_x( 15) / 1.00          /

DATA  spmh_nam_saprc99_x( 16)     , spmh_map_saprc99_x( 16)  &
    / 'terpinolene     ', 16            /
DATA  mech_nam_saprc99_x( 16)     , mech_map_saprc99_x( 16)  &
    / 'TERP            ', 2             /
DATA  conv_fac_saprc99_x( 16) / 1.00          /

DATA  spmh_nam_saprc99_x( 17)     , spmh_map_saprc99_x( 17)  &
    / 'phellandrene_b  ', 17            /
DATA  mech_nam_saprc99_x( 17)     , mech_map_saprc99_x( 17)  &
    / 'TERP            ', 2             /
DATA  conv_fac_saprc99_x( 17) / 1.00          /

DATA  spmh_nam_saprc99_x( 18)     , spmh_map_saprc99_x( 18)  &
    / 'camphene        ', 18            /
DATA  mech_nam_saprc99_x( 18)     , mech_map_saprc99_x( 18)  &
    / 'TERP            ', 2             /
DATA  conv_fac_saprc99_x( 18) / 1.00          /

DATA  spmh_nam_saprc99_x( 19)     , spmh_map_saprc99_x( 19)  &
    / 'bornene         ', 19            /
DATA  mech_nam_saprc99_x( 19)     , mech_map_saprc99_x( 19)  &
    / 'TERP            ', 2             /
DATA  conv_fac_saprc99_x( 19) / 1.00          /

DATA  spmh_nam_saprc99_x( 20)     , spmh_map_saprc99_x( 20)  &
    / 'fenchene_a      ', 20            /
DATA  mech_nam_saprc99_x( 20)     , mech_map_saprc99_x( 20)  &
    / 'TERP            ', 2             /
DATA  conv_fac_saprc99_x( 20) / 1.00          /

DATA  spmh_nam_saprc99_x( 21)     , spmh_map_saprc99_x( 21)  &
    / 'ocimene_al      ', 21            /
DATA  mech_nam_saprc99_x( 21)     , mech_map_saprc99_x( 21)  &
    / 'TERP            ', 2             /
DATA  conv_fac_saprc99_x( 21) / 1.00          /

DATA  spmh_nam_saprc99_x( 22)     , spmh_map_saprc99_x( 22)  &
    / 'ocimene_c_b     ', 22            /
DATA  mech_nam_saprc99_x( 22)     , mech_map_saprc99_x( 22)  &
    / 'TERP            ', 2             /
DATA  conv_fac_saprc99_x( 22) / 1.00          /

DATA  spmh_nam_saprc99_x( 23)     , spmh_map_saprc99_x( 23)  &
    / 'tricyclene      ', 23            /
DATA  mech_nam_saprc99_x( 23)     , mech_map_saprc99_x( 23)  &
    / 'ALK5            ', 5             /
DATA  conv_fac_saprc99_x( 23) / 1.00          /

DATA  spmh_nam_saprc99_x( 24)     , spmh_map_saprc99_x( 24)  &
    / 'estragole       ', 24            /
DATA  mech_nam_saprc99_x( 24)     , mech_map_saprc99_x( 24)  &
    / 'TERP            ', 2             /
DATA  conv_fac_saprc99_x( 24) / 1.00          /

DATA  spmh_nam_saprc99_x( 25)     , spmh_map_saprc99_x( 25)  &
    / 'camphor         ', 25            /
DATA  mech_nam_saprc99_x( 25)     , mech_map_saprc99_x( 25)  &
    / 'TERP            ', 2             /
DATA  conv_fac_saprc99_x( 25) / 1.00          /

DATA  spmh_nam_saprc99_x( 26)     , spmh_map_saprc99_x( 26)  &
    / 'fenchone        ', 26            /
DATA  mech_nam_saprc99_x( 26)     , mech_map_saprc99_x( 26)  &
    / 'ALK5            ', 5             /
DATA  conv_fac_saprc99_x( 26) / 1.00          /

DATA  spmh_nam_saprc99_x( 27)     , spmh_map_saprc99_x( 27)  &
    / 'piperitone      ', 27            /
DATA  mech_nam_saprc99_x( 27)     , mech_map_saprc99_x( 27)  &
    / 'TERP            ', 2             /
DATA  conv_fac_saprc99_x( 27) / 1.00          /

DATA  spmh_nam_saprc99_x( 28)     , spmh_map_saprc99_x( 28)  &
    / 'thujone_a       ', 28            /
DATA  mech_nam_saprc99_x( 28)     , mech_map_saprc99_x( 28)  &
    / 'ALK5            ', 5             /
DATA  conv_fac_saprc99_x( 28) / 1.00          /

DATA  spmh_nam_saprc99_x( 29)     , spmh_map_saprc99_x( 29)  &
    / 'thujone_b       ', 29            /
DATA  mech_nam_saprc99_x( 29)     , mech_map_saprc99_x( 29)  &
    / 'ALK5            ', 5             /
DATA  conv_fac_saprc99_x( 29) / 1.00          /

DATA  spmh_nam_saprc99_x( 30)     , spmh_map_saprc99_x( 30)  &
    / 'cineole_1_8     ', 30            /
DATA  mech_nam_saprc99_x( 30)     , mech_map_saprc99_x( 30)  &
    / 'ALK5            ', 5             /
DATA  conv_fac_saprc99_x( 30) / 1.00          /

DATA  spmh_nam_saprc99_x( 31)     , spmh_map_saprc99_x( 31)  &
    / 'borneol         ', 31            /
DATA  mech_nam_saprc99_x( 31)     , mech_map_saprc99_x( 31)  &
    / 'ALK5            ', 5             /
DATA  conv_fac_saprc99_x( 31) / 1.00          /

DATA  spmh_nam_saprc99_x( 32)     , spmh_map_saprc99_x( 32)  &
    / 'linalool        ', 32            /
DATA  mech_nam_saprc99_x( 32)     , mech_map_saprc99_x( 32)  &
    / 'TERP            ', 2             /
DATA  conv_fac_saprc99_x( 32) / 1.00          /

DATA  spmh_nam_saprc99_x( 33)     , spmh_map_saprc99_x( 33)  &
    / 'terpineol_4     ', 33            /
DATA  mech_nam_saprc99_x( 33)     , mech_map_saprc99_x( 33)  &
    / 'TERP            ', 2             /
DATA  conv_fac_saprc99_x( 33) / 1.00          /

DATA  spmh_nam_saprc99_x( 34)     , spmh_map_saprc99_x( 34)  &
    / 'terpineol_a     ', 34            /
DATA  mech_nam_saprc99_x( 34)     , mech_map_saprc99_x( 34)  &
    / 'TERP            ', 2             /
DATA  conv_fac_saprc99_x( 34) / 1.00          /

DATA  spmh_nam_saprc99_x( 35)     , spmh_map_saprc99_x( 35)  &
    / 'linalool_OXD_c  ', 35            /
DATA  mech_nam_saprc99_x( 35)     , mech_map_saprc99_x( 35)  &
    / 'TERP            ', 2             /
DATA  conv_fac_saprc99_x( 35) / 1.00          /

DATA  spmh_nam_saprc99_x( 36)     , spmh_map_saprc99_x( 36)  &
    / 'linalool_OXD_t  ', 36            /
DATA  mech_nam_saprc99_x( 36)     , mech_map_saprc99_x( 36)  &
    / 'TERP            ', 2             /
DATA  conv_fac_saprc99_x( 36) / 1.00          /

DATA  spmh_nam_saprc99_x( 37)     , spmh_map_saprc99_x( 37)  &
    / 'ionone_b        ', 37            /
DATA  mech_nam_saprc99_x( 37)     , mech_map_saprc99_x( 37)  &
    / 'TERP            ', 2             /
DATA  conv_fac_saprc99_x( 37) / 1.00          /

DATA  spmh_nam_saprc99_x( 38)     , spmh_map_saprc99_x( 38)  &
    / 'bornyl_ACT      ', 38            /
DATA  mech_nam_saprc99_x( 38)     , mech_map_saprc99_x( 38)  &
    / 'ALK5            ', 5             /
DATA  conv_fac_saprc99_x( 38) / 1.00          /

DATA  spmh_nam_saprc99_x( 39)     , spmh_map_saprc99_x( 39)  &
    / 'farnescene_a    ', 39            /
DATA  mech_nam_saprc99_x( 39)     , mech_map_saprc99_x( 39)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_x( 39) / 15.00         /

DATA  spmh_nam_saprc99_x( 40)     , spmh_map_saprc99_x( 40)  &
    / 'caryophyllene_b ', 40            /
DATA  mech_nam_saprc99_x( 40)     , mech_map_saprc99_x( 40)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_x( 40) / 15.00         /

DATA  spmh_nam_saprc99_x( 41)     , spmh_map_saprc99_x( 41)  &
    / 'acoradiene      ', 41            /
DATA  mech_nam_saprc99_x( 41)     , mech_map_saprc99_x( 41)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_x( 41) / 15.00         /

DATA  spmh_nam_saprc99_x( 42)     , spmh_map_saprc99_x( 42)  &
    / 'aromadendrene   ', 42            /
DATA  mech_nam_saprc99_x( 42)     , mech_map_saprc99_x( 42)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_x( 42) / 15.00         /

DATA  spmh_nam_saprc99_x( 43)     , spmh_map_saprc99_x( 43)  &
    / 'bergamotene_a   ', 43            /
DATA  mech_nam_saprc99_x( 43)     , mech_map_saprc99_x( 43)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_x( 43) / 15.00         /

DATA  spmh_nam_saprc99_x( 44)     , spmh_map_saprc99_x( 44)  &
    / 'bergamotene_b   ', 44            /
DATA  mech_nam_saprc99_x( 44)     , mech_map_saprc99_x( 44)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_x( 44) / 15.00         /

DATA  spmh_nam_saprc99_x( 45)     , spmh_map_saprc99_x( 45)  &
    / 'bisabolene_a    ', 45            /
DATA  mech_nam_saprc99_x( 45)     , mech_map_saprc99_x( 45)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_x( 45) / 15.00         /

DATA  spmh_nam_saprc99_x( 46)     , spmh_map_saprc99_x( 46)  &
    / 'bisabolene_b    ', 46            /
DATA  mech_nam_saprc99_x( 46)     , mech_map_saprc99_x( 46)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_x( 46) / 15.00         /

DATA  spmh_nam_saprc99_x( 47)     , spmh_map_saprc99_x( 47)  &
    / 'bourbonene_b    ', 47            /
DATA  mech_nam_saprc99_x( 47)     , mech_map_saprc99_x( 47)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_x( 47) / 15.00         /

DATA  spmh_nam_saprc99_x( 48)     , spmh_map_saprc99_x( 48)  &
    / 'cadinene_d      ', 48            /
DATA  mech_nam_saprc99_x( 48)     , mech_map_saprc99_x( 48)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_x( 48) / 15.00         /

DATA  spmh_nam_saprc99_x( 49)     , spmh_map_saprc99_x( 49)  &
    / 'cadinene_g      ', 49            /
DATA  mech_nam_saprc99_x( 49)     , mech_map_saprc99_x( 49)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_x( 49) / 15.00         /

DATA  spmh_nam_saprc99_x( 50)     , spmh_map_saprc99_x( 50)  &
    / 'cedrene_a       ', 50            /
DATA  mech_nam_saprc99_x( 50)     , mech_map_saprc99_x( 50)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_x( 50) / 15.00         /

DATA  spmh_nam_saprc99_x( 51)     , spmh_map_saprc99_x( 51)  &
    / 'copaene_a       ', 51            /
DATA  mech_nam_saprc99_x( 51)     , mech_map_saprc99_x( 51)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_x( 51) / 15.00         /

DATA  spmh_nam_saprc99_x( 52)     , spmh_map_saprc99_x( 52)  &
    / 'cubebene_a      ', 52            /
DATA  mech_nam_saprc99_x( 52)     , mech_map_saprc99_x( 52)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_x( 52) / 15.00         /

DATA  spmh_nam_saprc99_x( 53)     , spmh_map_saprc99_x( 53)  &
    / 'cubebene_b      ', 53            /
DATA  mech_nam_saprc99_x( 53)     , mech_map_saprc99_x( 53)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_x( 53) / 15.00         /

DATA  spmh_nam_saprc99_x( 54)     , spmh_map_saprc99_x( 54)  &
    / 'elemene_b       ', 54            /
DATA  mech_nam_saprc99_x( 54)     , mech_map_saprc99_x( 54)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_x( 54) / 15.00         /

DATA  spmh_nam_saprc99_x( 55)     , spmh_map_saprc99_x( 55)  &
    / 'farnescene_b    ', 55            /
DATA  mech_nam_saprc99_x( 55)     , mech_map_saprc99_x( 55)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_x( 55) / 15.00         /

DATA  spmh_nam_saprc99_x( 56)     , spmh_map_saprc99_x( 56)  &
    / 'germacrene_B    ', 56            /
DATA  mech_nam_saprc99_x( 56)     , mech_map_saprc99_x( 56)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_x( 56) / 15.00         /

DATA  spmh_nam_saprc99_x( 57)     , spmh_map_saprc99_x( 57)  &
    / 'germacrene_D    ', 57            /
DATA  mech_nam_saprc99_x( 57)     , mech_map_saprc99_x( 57)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_x( 57) / 15.00         /

DATA  spmh_nam_saprc99_x( 58)     , spmh_map_saprc99_x( 58)  &
    / 'gurjunene_b     ', 58            /
DATA  mech_nam_saprc99_x( 58)     , mech_map_saprc99_x( 58)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_x( 58) / 15.00         /

DATA  spmh_nam_saprc99_x( 59)     , spmh_map_saprc99_x( 59)  &
    / 'humulene_a      ', 59            /
DATA  mech_nam_saprc99_x( 59)     , mech_map_saprc99_x( 59)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_x( 59) / 15.00         /

DATA  spmh_nam_saprc99_x( 60)     , spmh_map_saprc99_x( 60)  &
    / 'humulene_g      ', 60            /
DATA  mech_nam_saprc99_x( 60)     , mech_map_saprc99_x( 60)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_x( 60) / 15.00         /

DATA  spmh_nam_saprc99_x( 61)     , spmh_map_saprc99_x( 61)  &
    / 'isolongifolene  ', 61            /
DATA  mech_nam_saprc99_x( 61)     , mech_map_saprc99_x( 61)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_x( 61) / 15.00         /

DATA  spmh_nam_saprc99_x( 62)     , spmh_map_saprc99_x( 62)  &
    / 'longifolene     ', 62            /
DATA  mech_nam_saprc99_x( 62)     , mech_map_saprc99_x( 62)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_x( 62) / 15.00         /

DATA  spmh_nam_saprc99_x( 63)     , spmh_map_saprc99_x( 63)  &
    / 'longipinene     ', 63            /
DATA  mech_nam_saprc99_x( 63)     , mech_map_saprc99_x( 63)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_x( 63) / 15.00         /

DATA  spmh_nam_saprc99_x( 64)     , spmh_map_saprc99_x( 64)  &
    / 'muurolene_a     ', 64            /
DATA  mech_nam_saprc99_x( 64)     , mech_map_saprc99_x( 64)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_x( 64) / 15.00         /

DATA  spmh_nam_saprc99_x( 65)     , spmh_map_saprc99_x( 65)  &
    / 'muurolene_g     ', 65            /
DATA  mech_nam_saprc99_x( 65)     , mech_map_saprc99_x( 65)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_x( 65) / 15.00         /

DATA  spmh_nam_saprc99_x( 66)     , spmh_map_saprc99_x( 66)  &
    / 'selinene_b      ', 66            /
DATA  mech_nam_saprc99_x( 66)     , mech_map_saprc99_x( 66)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_x( 66) / 15.00         /

DATA  spmh_nam_saprc99_x( 67)     , spmh_map_saprc99_x( 67)  &
    / 'selinene_d      ', 67            /
DATA  mech_nam_saprc99_x( 67)     , mech_map_saprc99_x( 67)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_x( 67) / 15.00         /

DATA  spmh_nam_saprc99_x( 68)     , spmh_map_saprc99_x( 68)  &
    / 'nerolidol_c     ', 68            /
DATA  mech_nam_saprc99_x( 68)     , mech_map_saprc99_x( 68)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_x( 68) / 15.00         /

DATA  spmh_nam_saprc99_x( 69)     , spmh_map_saprc99_x( 69)  &
    / 'nerolidol_t     ', 69            /
DATA  mech_nam_saprc99_x( 69)     , mech_map_saprc99_x( 69)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_x( 69) / 15.00         /

DATA  spmh_nam_saprc99_x( 70)     , spmh_map_saprc99_x( 70)  &
    / 'cedrol          ', 70            /
DATA  mech_nam_saprc99_x( 70)     , mech_map_saprc99_x( 70)  &
    / 'ALK5            ', 5             /
DATA  conv_fac_saprc99_x( 70) / 1.00          /

DATA  spmh_nam_saprc99_x( 71)     , spmh_map_saprc99_x( 71)  &
    / 'MBO_2m3e2ol     ', 71            /
DATA  mech_nam_saprc99_x( 71)     , mech_map_saprc99_x( 71)  &
    / 'OLE1            ', 7             /
DATA  conv_fac_saprc99_x( 71) / 1.00          /

DATA  spmh_nam_saprc99_x( 72)     , spmh_map_saprc99_x( 72)  &
    / 'methanol        ', 72            /
DATA  mech_nam_saprc99_x( 72)     , mech_map_saprc99_x( 72)  &
    / 'MEOH            ', 8             /
DATA  conv_fac_saprc99_x( 72) / 1.00          /

DATA  spmh_nam_saprc99_x( 73)     , spmh_map_saprc99_x( 73)  &
    / 'acetone         ', 73            /
DATA  mech_nam_saprc99_x( 73)     , mech_map_saprc99_x( 73)  &
    / 'ACET            ', 9             /
DATA  conv_fac_saprc99_x( 73) / 1.00          /

DATA  spmh_nam_saprc99_x( 74)     , spmh_map_saprc99_x( 74)  &
    / 'methane         ', 74            /
DATA  mech_nam_saprc99_x( 74)     , mech_map_saprc99_x( 74)  &
    / 'CH4             ', 10            /
DATA  conv_fac_saprc99_x( 74) / 1.00          /

DATA  spmh_nam_saprc99_x( 75)     , spmh_map_saprc99_x( 75)  &
    / 'ammonia         ', 75            /
DATA  mech_nam_saprc99_x( 75)     , mech_map_saprc99_x( 75)  &
    / 'NH3             ', 11            /
DATA  conv_fac_saprc99_x( 75) / 1.00          /

DATA  spmh_nam_saprc99_x( 76)     , spmh_map_saprc99_x( 76)  &
    / 'nitric_OXD      ', 77            /
DATA  mech_nam_saprc99_x( 76)     , mech_map_saprc99_x( 76)  &
    / 'NO              ', 12            /
DATA  conv_fac_saprc99_x( 76) / 1.00          /

DATA  spmh_nam_saprc99_x( 77)     , spmh_map_saprc99_x( 77)  &
    / 'acetaldehyde    ', 78            /
DATA  mech_nam_saprc99_x( 77)     , mech_map_saprc99_x( 77)  &
    / 'CCHO            ', 13            /
DATA  conv_fac_saprc99_x( 77) / 1.00          /

DATA  spmh_nam_saprc99_x( 78)     , spmh_map_saprc99_x( 78)  &
    / 'ethanol         ', 79            /
DATA  mech_nam_saprc99_x( 78)     , mech_map_saprc99_x( 78)  &
    / 'ALK3            ', 14            /
DATA  conv_fac_saprc99_x( 78) / 1.00          /

DATA  spmh_nam_saprc99_x( 79)     , spmh_map_saprc99_x( 79)  &
    / 'formic_acid     ', 80            /
DATA  mech_nam_saprc99_x( 79)     , mech_map_saprc99_x( 79)  &
    / 'HC2H            ', 15            /
DATA  conv_fac_saprc99_x( 79) / 1.00          /

DATA  spmh_nam_saprc99_x( 80)     , spmh_map_saprc99_x( 80)  &
    / 'formaldehyde    ', 81            /
DATA  mech_nam_saprc99_x( 80)     , mech_map_saprc99_x( 80)  &
    / 'HCHO            ', 16            /
DATA  conv_fac_saprc99_x( 80) / 1.00          /

DATA  spmh_nam_saprc99_x( 81)     , spmh_map_saprc99_x( 81)  &
    / 'acetic_acid     ', 82            /
DATA  mech_nam_saprc99_x( 81)     , mech_map_saprc99_x( 81)  &
    / 'CO2H            ', 17            /
DATA  conv_fac_saprc99_x( 81) / 1.00          /

DATA  spmh_nam_saprc99_x( 82)     , spmh_map_saprc99_x( 82)  &
    / 'MBO_3m2e1ol     ', 83            /
DATA  mech_nam_saprc99_x( 82)     , mech_map_saprc99_x( 82)  &
    / 'OLE2            ', 3             /
DATA  conv_fac_saprc99_x( 82) / 1.00          /

DATA  spmh_nam_saprc99_x( 83)     , spmh_map_saprc99_x( 83)  &
    / 'MBO_3m3e1ol     ', 84            /
DATA  mech_nam_saprc99_x( 83)     , mech_map_saprc99_x( 83)  &
    / 'OLE1            ', 7             /
DATA  conv_fac_saprc99_x( 83) / 1.00          /

DATA  spmh_nam_saprc99_x( 84)     , spmh_map_saprc99_x( 84)  &
    / 'benzaldehyde    ', 85            /
DATA  mech_nam_saprc99_x( 84)     , mech_map_saprc99_x( 84)  &
    / 'BALD            ', 18            /
DATA  conv_fac_saprc99_x( 84) / 1.00          /

DATA  spmh_nam_saprc99_x( 85)     , spmh_map_saprc99_x( 85)  &
    / 'butanone_2      ', 86            /
DATA  mech_nam_saprc99_x( 85)     , mech_map_saprc99_x( 85)  &
    / 'MEK             ', 19            /
DATA  conv_fac_saprc99_x( 85) / 1.00          /

DATA  spmh_nam_saprc99_x( 86)     , spmh_map_saprc99_x( 86)  &
    / 'decanal         ', 87            /
DATA  mech_nam_saprc99_x( 86)     , mech_map_saprc99_x( 86)  &
    / 'RCHO            ', 20            /
DATA  conv_fac_saprc99_x( 86) / 1.00          /

DATA  spmh_nam_saprc99_x( 87)     , spmh_map_saprc99_x( 87)  &
    / 'dodecene_1      ', 88            /
DATA  mech_nam_saprc99_x( 87)     , mech_map_saprc99_x( 87)  &
    / 'OLE1            ', 7             /
DATA  conv_fac_saprc99_x( 87) / 1.00          /

DATA  spmh_nam_saprc99_x( 88)     , spmh_map_saprc99_x( 88)  &
    / 'geranyl_acetone ', 89            /
DATA  mech_nam_saprc99_x( 88)     , mech_map_saprc99_x( 88)  &
    / 'TERP            ', 2             /
DATA  conv_fac_saprc99_x( 88) / 1.00          /

DATA  spmh_nam_saprc99_x( 89)     , spmh_map_saprc99_x( 89)  &
    / 'heptanal        ', 90            /
DATA  mech_nam_saprc99_x( 89)     , mech_map_saprc99_x( 89)  &
    / 'RCHO            ', 20            /
DATA  conv_fac_saprc99_x( 89) / 1.00          /

DATA  spmh_nam_saprc99_x( 90)     , spmh_map_saprc99_x( 90)  &
    / 'heptane         ', 91            /
DATA  mech_nam_saprc99_x( 90)     , mech_map_saprc99_x( 90)  &
    / 'ALK5            ', 5             /
DATA  conv_fac_saprc99_x( 90) / 1.00          /

DATA  spmh_nam_saprc99_x( 91)     , spmh_map_saprc99_x( 91)  &
    / 'hexane          ', 92            /
DATA  mech_nam_saprc99_x( 91)     , mech_map_saprc99_x( 91)  &
    / 'ALK4            ', 21            /
DATA  conv_fac_saprc99_x( 91) / 1.00          /

DATA  spmh_nam_saprc99_x( 92)     , spmh_map_saprc99_x( 92)  &
    / 'met_benzoate    ', 93            /
DATA  mech_nam_saprc99_x( 92)     , mech_map_saprc99_x( 92)  &
    / 'ARO1            ', 22            /
DATA  conv_fac_saprc99_x( 92) / 1.00          /

DATA  spmh_nam_saprc99_x( 93)     , spmh_map_saprc99_x( 93)  &
    / 'met_heptenone   ', 94            /
DATA  mech_nam_saprc99_x( 93)     , mech_map_saprc99_x( 93)  &
    / 'OLE2            ', 3             /
DATA  conv_fac_saprc99_x( 93) / 1.00          /

DATA  spmh_nam_saprc99_x( 94)     , spmh_map_saprc99_x( 94)  &
    / 'neryl_acetone   ', 95            /
DATA  mech_nam_saprc99_x( 94)     , mech_map_saprc99_x( 94)  &
    / 'OLE2            ', 3             /
DATA  conv_fac_saprc99_x( 94) / 1.00          /

DATA  spmh_nam_saprc99_x( 95)     , spmh_map_saprc99_x( 95)  &
    / 'nonanal         ', 96            /
DATA  mech_nam_saprc99_x( 95)     , mech_map_saprc99_x( 95)  &
    / 'RCHO            ', 20            /
DATA  conv_fac_saprc99_x( 95) / 1.00          /

DATA  spmh_nam_saprc99_x( 96)     , spmh_map_saprc99_x( 96)  &
    / 'nonenal         ', 97            /
DATA  mech_nam_saprc99_x( 96)     , mech_map_saprc99_x( 96)  &
    / 'OLE1            ', 7             /
DATA  conv_fac_saprc99_x( 96) / 1.00          /

DATA  spmh_nam_saprc99_x( 97)     , spmh_map_saprc99_x( 97)  &
    / 'octanal         ', 98            /
DATA  mech_nam_saprc99_x( 97)     , mech_map_saprc99_x( 97)  &
    / 'RCHO            ', 20            /
DATA  conv_fac_saprc99_x( 97) / 1.00          /

DATA  spmh_nam_saprc99_x( 98)     , spmh_map_saprc99_x( 98)  &
    / 'octanol         ', 99            /
DATA  mech_nam_saprc99_x( 98)     , mech_map_saprc99_x( 98)  &
    / 'ALK5            ', 5             /
DATA  conv_fac_saprc99_x( 98) / 1.00          /

DATA  spmh_nam_saprc99_x( 99)     , spmh_map_saprc99_x( 99)  &
    / 'octenol_1e3ol   ', 100           /
DATA  mech_nam_saprc99_x( 99)     , mech_map_saprc99_x( 99)  &
    / 'OLE1            ', 7             /
DATA  conv_fac_saprc99_x( 99) / 1.00          /

DATA  spmh_nam_saprc99_x(100)     , spmh_map_saprc99_x(100)  &
    / 'oxopentanal     ', 101           /
DATA  mech_nam_saprc99_x(100)     , mech_map_saprc99_x(100)  &
    / 'RCHO            ', 20            /
DATA  conv_fac_saprc99_x(100) / 1.00          /

DATA  spmh_nam_saprc99_x(101)     , spmh_map_saprc99_x(101)  &
    / 'pentane         ', 102           /
DATA  mech_nam_saprc99_x(101)     , mech_map_saprc99_x(101)  &
    / 'ALK4            ', 21            /
DATA  conv_fac_saprc99_x(101) / 1.00          /

DATA  spmh_nam_saprc99_x(102)     , spmh_map_saprc99_x(102)  &
    / 'phenyl_CCO      ', 103           /
DATA  mech_nam_saprc99_x(102)     , mech_map_saprc99_x(102)  &
    / 'ARO1            ', 22            /
DATA  conv_fac_saprc99_x(102) / 1.00          /

DATA  spmh_nam_saprc99_x(103)     , spmh_map_saprc99_x(103)  &
    / 'pyruvic_acid    ', 104           /
DATA  mech_nam_saprc99_x(103)     , mech_map_saprc99_x(103)  &
    / 'BACL            ', 23            /
DATA  conv_fac_saprc99_x(103) / 1.00          /

DATA  spmh_nam_saprc99_x(104)     , spmh_map_saprc99_x(104)  &
    / 'terpinyl_ACT_a  ', 105           /
DATA  mech_nam_saprc99_x(104)     , mech_map_saprc99_x(104)  &
    / 'TERP            ', 2             /
DATA  conv_fac_saprc99_x(104) / 1.00          /

DATA  spmh_nam_saprc99_x(105)     , spmh_map_saprc99_x(105)  &
    / 'tetradecene_1   ', 106           /
DATA  mech_nam_saprc99_x(105)     , mech_map_saprc99_x(105)  &
    / 'OLE1            ', 7             /
DATA  conv_fac_saprc99_x(105) / 1.00          /

DATA  spmh_nam_saprc99_x(106)     , spmh_map_saprc99_x(106)  &
    / 'toluene         ', 107           /
DATA  mech_nam_saprc99_x(106)     , mech_map_saprc99_x(106)  &
    / 'ARO1            ', 22            /
DATA  conv_fac_saprc99_x(106) / 1.00          /

DATA  spmh_nam_saprc99_x(107)     , spmh_map_saprc99_x(107)  &
    / 'carbon_monoxide ', 108           /
DATA  mech_nam_saprc99_x(107)     , mech_map_saprc99_x(107)  &
    / 'CO              ', 24            /
DATA  conv_fac_saprc99_x(107) / 1.00          /

DATA  spmh_nam_saprc99_x(108)     , spmh_map_saprc99_x(108)  &
    / 'butene          ', 109           /
DATA  mech_nam_saprc99_x(108)     , mech_map_saprc99_x(108)  &
    / 'OLE1            ', 7             /
DATA  conv_fac_saprc99_x(108) / 1.00          /

DATA  spmh_nam_saprc99_x(109)     , spmh_map_saprc99_x(109)  &
    / 'ethane          ', 110           /
DATA  mech_nam_saprc99_x(109)     , mech_map_saprc99_x(109)  &
    / 'ALK1            ', 25            /
DATA  conv_fac_saprc99_x(109) / 1.00          /

DATA  spmh_nam_saprc99_x(110)     , spmh_map_saprc99_x(110)  &
    / 'ethene          ', 111           /
DATA  mech_nam_saprc99_x(110)     , mech_map_saprc99_x(110)  &
    / 'ETHE            ', 26            /
DATA  conv_fac_saprc99_x(110) / 1.00          /

DATA  spmh_nam_saprc99_x(111)     , spmh_map_saprc99_x(111)  &
    / 'propane         ', 113           /
DATA  mech_nam_saprc99_x(111)     , mech_map_saprc99_x(111)  &
    / 'ALK2            ', 27            /
DATA  conv_fac_saprc99_x(111) / 1.00          /

DATA  spmh_nam_saprc99_x(112)     , spmh_map_saprc99_x(112)  &
    / 'propene         ', 114           /
DATA  mech_nam_saprc99_x(112)     , mech_map_saprc99_x(112)  &
    / 'OLE1            ', 7             /
DATA  conv_fac_saprc99_x(112) / 1.00          /

DATA  spmh_nam_saprc99_x(113)     , spmh_map_saprc99_x(113)  &
    / 'diallyl_2s      ', 117           /
DATA  mech_nam_saprc99_x(113)     , mech_map_saprc99_x(113)  &
    / 'OLE1            ', 7             /
DATA  conv_fac_saprc99_x(113) / 1.00          /

DATA  spmh_nam_saprc99_x(114)     , spmh_map_saprc99_x(114)  &
    / '2met_2s         ', 118           /
DATA  mech_nam_saprc99_x(114)     , mech_map_saprc99_x(114)  &
    / 'ALK3            ', 14            /
DATA  conv_fac_saprc99_x(114) / 1.00          /

DATA  spmh_nam_saprc99_x(115)     , spmh_map_saprc99_x(115)  &
    / '2met_s          ', 119           /
DATA  mech_nam_saprc99_x(115)     , mech_map_saprc99_x(115)  &
    / 'ALK4            ', 21            /
DATA  conv_fac_saprc99_x(115) / 1.00          /

DATA  spmh_nam_saprc99_x(116)     , spmh_map_saprc99_x(116)  &
    / 'met_chloride    ', 120           /
DATA  mech_nam_saprc99_x(116)     , mech_map_saprc99_x(116)  &
    / 'ALK2            ', 27            /
DATA  conv_fac_saprc99_x(116) / 1.00          /

DATA  spmh_nam_saprc99_x(117)     , spmh_map_saprc99_x(117)  &
    / 'met_bromide     ', 121           /
DATA  mech_nam_saprc99_x(117)     , mech_map_saprc99_x(117)  &
    / 'ALK2            ', 27            /
DATA  conv_fac_saprc99_x(117) / 1.00          /

DATA  spmh_nam_saprc99_x(118)     , spmh_map_saprc99_x(118)  &
    / 'met_iodide      ', 122           /
DATA  mech_nam_saprc99_x(118)     , mech_map_saprc99_x(118)  &
    / 'ALK2            ', 27            /
DATA  conv_fac_saprc99_x(118) / 1.00          /

DATA  spmh_nam_saprc99_x(119)     , spmh_map_saprc99_x(119)  &
    / 'met_mercaptan   ', 124           /
DATA  mech_nam_saprc99_x(119)     , mech_map_saprc99_x(119)  &
    / 'ALK2            ', 27            /
DATA  conv_fac_saprc99_x(119) / 1.00          /

DATA  spmh_nam_saprc99_x(120)     , spmh_map_saprc99_x(120)  &
    / 'met_propenyl_2s ', 125           /
DATA  mech_nam_saprc99_x(120)     , mech_map_saprc99_x(120)  &
    / 'OLE1            ', 7             /
DATA  conv_fac_saprc99_x(120) / 1.00          /

DATA  spmh_nam_saprc99_x(121)     , spmh_map_saprc99_x(121)  &
    / 'PPPP_2s         ', 126           /
DATA  mech_nam_saprc99_x(121)     , mech_map_saprc99_x(121)  &
    / 'OLE1            ', 7             /
DATA  conv_fac_saprc99_x(121) / 1.00          /

DATA  spmh_nam_saprc99_x(122)     , spmh_map_saprc99_x(122)  &
    / '2met_nonatriene ', 127           /
DATA  mech_nam_saprc99_x(122)     , mech_map_saprc99_x(122)  &
    / 'TERP            ', 2             /
DATA  conv_fac_saprc99_x(122) / 1.00          /

DATA  spmh_nam_saprc99_x(123)     , spmh_map_saprc99_x(123)  &
    / 'met_salicylate  ', 128           /
DATA  mech_nam_saprc99_x(123)     , mech_map_saprc99_x(123)  &
    / 'ARO1            ', 22            /
DATA  conv_fac_saprc99_x(123) / 1.00          /

DATA  spmh_nam_saprc99_x(124)     , spmh_map_saprc99_x(124)  &
    / 'indole          ', 129           /
DATA  mech_nam_saprc99_x(124)     , mech_map_saprc99_x(124)  &
    / 'ARO2            ', 4             /
DATA  conv_fac_saprc99_x(124) / 1.00          /

DATA  spmh_nam_saprc99_x(125)     , spmh_map_saprc99_x(125)  &
    / 'jasmone         ', 130           /
DATA  mech_nam_saprc99_x(125)     , mech_map_saprc99_x(125)  &
    / 'TERP            ', 2             /
DATA  conv_fac_saprc99_x(125) / 1.00          /

DATA  spmh_nam_saprc99_x(126)     , spmh_map_saprc99_x(126)  &
    / 'met_jasmonate   ', 131           /
DATA  mech_nam_saprc99_x(126)     , mech_map_saprc99_x(126)  &
    / 'TERP            ', 2             /
DATA  conv_fac_saprc99_x(126) / 1.00          /

DATA  spmh_nam_saprc99_x(127)     , spmh_map_saprc99_x(127)  &
    / '3met_3DCTT      ', 132           /
DATA  mech_nam_saprc99_x(127)     , mech_map_saprc99_x(127)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_x(127) / 16.00         /

DATA  spmh_nam_saprc99_x(128)     , spmh_map_saprc99_x(128)  &
    / 'hexanal         ', 133           /
DATA  mech_nam_saprc99_x(128)     , mech_map_saprc99_x(128)  &
    / 'RCHO            ', 20            /
DATA  conv_fac_saprc99_x(128) / 1.00          /

DATA  spmh_nam_saprc99_x(129)     , spmh_map_saprc99_x(129)  &
    / 'hexanol_1       ', 134           /
DATA  mech_nam_saprc99_x(129)     , mech_map_saprc99_x(129)  &
    / 'ALK5            ', 5             /
DATA  conv_fac_saprc99_x(129) / 1.00          /

DATA  spmh_nam_saprc99_x(130)     , spmh_map_saprc99_x(130)  &
    / 'hexenal_c3      ', 135           /
DATA  mech_nam_saprc99_x(130)     , mech_map_saprc99_x(130)  &
    / 'OLE2            ', 3             /
DATA  conv_fac_saprc99_x(130) / 1.00          /

DATA  spmh_nam_saprc99_x(131)     , spmh_map_saprc99_x(131)  &
    / 'hexenal_t2      ', 136           /
DATA  mech_nam_saprc99_x(131)     , mech_map_saprc99_x(131)  &
    / 'OLE2            ', 3             /
DATA  conv_fac_saprc99_x(131) / 1.00          /

DATA  spmh_nam_saprc99_x(132)     , spmh_map_saprc99_x(132)  &
    / 'hexenol_c3      ', 137           /
DATA  mech_nam_saprc99_x(132)     , mech_map_saprc99_x(132)  &
    / 'OLE2            ', 3             /
DATA  conv_fac_saprc99_x(132) / 1.00          /

DATA  spmh_nam_saprc99_x(133)     , spmh_map_saprc99_x(133)  &
    / 'hexenyl_ACT_c3  ', 138           /
DATA  mech_nam_saprc99_x(133)     , mech_map_saprc99_x(133)  &
    / 'OLE2            ', 3             /
DATA  conv_fac_saprc99_x(133) / 1.00          /

DATA  spmh_nam_saprc99_x(134)     , spmh_map_saprc99_x(134)  &
    / 'homosalate      ', 134            /
DATA  mech_nam_saprc99_x(134)     , mech_map_saprc99_x(134)  &
    / 'TERP            ', 2              /
DATA  conv_fac_saprc99_x(134) / 1.0            /

DATA  spmh_nam_saprc99_x(135)     , spmh_map_saprc99_x(135)  &
    / 'Ehsalate        ', 135            /
DATA  mech_nam_saprc99_x(135)     , mech_map_saprc99_x(135)  &
    / 'TERP            ', 2              /
DATA  conv_fac_saprc99_x(135) / 1.0            /

DATA  spmh_nam_saprc99_x(136)     , spmh_map_saprc99_x(136)  &
    / 'pentanal        ', 136            /
DATA  mech_nam_saprc99_x(136)     , mech_map_saprc99_x(136)  &
    / 'RCHO            ', 27             /
DATA  conv_fac_saprc99_x(136) / 1.0            /

DATA  spmh_nam_saprc99_x(137)     , spmh_map_saprc99_x(137)  &
    / 'heptanone       ', 137            /
DATA  mech_nam_saprc99_x(137)     , mech_map_saprc99_x(137)  &
    / 'OLE2            ', 26             /
DATA  conv_fac_saprc99_x(137) / 1.0            /

DATA  spmh_nam_saprc99_x(138)     , spmh_map_saprc99_x(138)  &
    / 'anisole         ',138             /
DATA  mech_nam_saprc99_x(138)     , mech_map_saprc99_x(138)  &
    / 'BALD            ', 13             /
DATA  conv_fac_saprc99_x(138) / 1.0            /

DATA  spmh_nam_saprc99_x(139)     , spmh_map_saprc99_x(139)  &
    / 'verbenene       ',139             /
DATA  mech_nam_saprc99_x(139)     , mech_map_saprc99_x(139)  &
    / 'ARO2            ', 24             /
DATA  conv_fac_saprc99_x(139) / 1.0            /

DATA  spmh_nam_saprc99_x(140)     , spmh_map_saprc99_x(140)  &
    / 'benzyl-acetate  ',140             /
DATA  mech_nam_saprc99_x(140)     , mech_map_saprc99_x(140)  &
    / 'ARO1            ', 23             /
DATA  conv_fac_saprc99_x(140) / 1.0            /

DATA  spmh_nam_saprc99_x(141)     , spmh_map_saprc99_x(141)  &
    / 'myrtenal        ',141             /
DATA  mech_nam_saprc99_x(141)     , mech_map_saprc99_x(141)  &
    / 'TERP            ', 2              /
DATA  conv_fac_saprc99_x(141) / 1.0            /

DATA  spmh_nam_saprc99_x(142)     , spmh_map_saprc99_x(142)  &
    / 'benzyl-alcohol  ',142             /
DATA  mech_nam_saprc99_x(142)     , mech_map_saprc99_x(142)  &
    / 'ARO1            ', 23             /
DATA  conv_fac_saprc99_x(142) / 1.0            /

DATA  spmh_nam_saprc99_x(143)     , spmh_map_saprc99_x(143)  &
    / 'meta-cymenene   ',143             /
DATA  mech_nam_saprc99_x(143)     , mech_map_saprc99_x(143)  &
    / 'ARO2            ', 24             /
DATA  conv_fac_saprc99_x(143) / 1.0            /

DATA  spmh_nam_saprc99_x(144)     , spmh_map_saprc99_x(144)  &
    / 'ipsenol         ',144             /
DATA  mech_nam_saprc99_x(144)     , mech_map_saprc99_x(144)  &
    / 'TERP            ', 2              /
DATA  conv_fac_saprc99_x(144) / 1.0            /

DATA  spmh_nam_saprc99_x(145)     , spmh_map_saprc99_x(145)  &
    / 'Napthalene      ', 145            /
DATA  mech_nam_saprc99_x(145)     , mech_map_saprc99_x(145)  &
    / 'ARO2            ', 24             /
DATA  conv_fac_saprc99_x(145) / 1.0            /

end module map_cv2saprc99x