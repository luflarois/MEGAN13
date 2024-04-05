module map_cv2saprc99q
!=======================================================================
!  MAP_SAPRC99_Q_CV2SAPRC99Q.EXT
!  This include file contains conversion table for 150 speciated species
!  to SAPRC99 (CMAQ) species


!  MEGAN v2.10
!  INPUT version x.x

!  History:
!  Who          When       What
!  ---------------------------------------------------------------------
!  bkoo         04/13/07 - Created
!  bkoo         05/23/07 - Modified speciation
!  Tan          07/18/11 - Updated for MEGANv2.10
!=======================================================================

CHARACTER (LEN=16) :: map_saprc99_qmech
PARAMETER     (map_saprc99_qmech = 'SAPRC99Q        ')

INTEGER :: n_saprc99_q
PARAMETER     (n_saprc99_q = 145)        ! Number of map species

CHARACTER (LEN=16) :: spmh_nam_saprc99_q( n_saprc99_q)   ! speciated species name
INTEGER :: spmh_map_saprc99_q( n_saprc99_q)   ! speciated species name
! mapped to SPC_SPCAT.EXT
CHARACTER (LEN=16) :: mech_nam_saprc99_q( n_saprc99_q)   ! mechanism species
INTEGER :: mech_map_saprc99_q( n_saprc99_q)   ! mechanism species mapped
! to SPC_CB4Q.EXT
REAL :: conv_fac_saprc99_q( n_saprc99_q )   ! conversion factor


DATA  spmh_nam_saprc99_q(  1)     , spmh_map_saprc99_q(  1)  &
    / 'isoprene        ', 1             /
DATA  mech_nam_saprc99_q(  1)     , mech_map_saprc99_q(  1)  &
    / 'ISOPRENE        ', 1             /
DATA  conv_fac_saprc99_q(  1) / 1.00          /

DATA  spmh_nam_saprc99_q(  2)     , spmh_map_saprc99_q(  2)  &
    / 'myrcene         ', 2             /
DATA  mech_nam_saprc99_q(  2)     , mech_map_saprc99_q(  2)  &
    / 'TRP1            ', 2             /
DATA  conv_fac_saprc99_q(  2) / 1.00          /

DATA  spmh_nam_saprc99_q(  3)     , spmh_map_saprc99_q(  3)  &
    / 'sabinene        ', 3             /
DATA  mech_nam_saprc99_q(  3)     , mech_map_saprc99_q(  3)  &
    / 'TRP1            ', 2             /
DATA  conv_fac_saprc99_q(  3) / 1.00          /

DATA  spmh_nam_saprc99_q(  4)     , spmh_map_saprc99_q(  4)  &
    / 'limonene        ', 4             /
DATA  mech_nam_saprc99_q(  4)     , mech_map_saprc99_q(  4)  &
    / 'TRP1            ', 2             /
DATA  conv_fac_saprc99_q(  4) / 1.00          /

DATA  spmh_nam_saprc99_q(  5)     , spmh_map_saprc99_q(  5)  &
    / 'carene_3        ', 5             /
DATA  mech_nam_saprc99_q(  5)     , mech_map_saprc99_q(  5)  &
    / 'TRP1            ', 2             /
DATA  conv_fac_saprc99_q(  5) / 1.00          /

DATA  spmh_nam_saprc99_q(  6)     , spmh_map_saprc99_q(  6)  &
    / 'ocimene_t_b     ', 6             /
DATA  mech_nam_saprc99_q(  6)     , mech_map_saprc99_q(  6)  &
    / 'TRP1            ', 2             /
DATA  conv_fac_saprc99_q(  6) / 1.00          /

DATA  spmh_nam_saprc99_q(  7)     , spmh_map_saprc99_q(  7)  &
    / 'pinene_b        ', 7             /
DATA  mech_nam_saprc99_q(  7)     , mech_map_saprc99_q(  7)  &
    / 'TRP1            ', 2             /
DATA  conv_fac_saprc99_q(  7) / 1.00          /

DATA  spmh_nam_saprc99_q(  8)     , spmh_map_saprc99_q(  8)  &
    / 'pinene_a        ', 8             /
DATA  mech_nam_saprc99_q(  8)     , mech_map_saprc99_q(  8)  &
    / 'TRP1            ', 2             /
DATA  conv_fac_saprc99_q(  8) / 1.00          /

DATA  spmh_nam_saprc99_q(  9)     , spmh_map_saprc99_q(  9)  &
    / '2met_styrene    ', 9             /
DATA  mech_nam_saprc99_q(  9)     , mech_map_saprc99_q(  9)  &
    / 'OLE2            ', 3             /
DATA  conv_fac_saprc99_q(  9) / 1.00          /

DATA  spmh_nam_saprc99_q( 10)     , spmh_map_saprc99_q( 10)  &
    / 'cymene_p        ', 10            /
DATA  mech_nam_saprc99_q( 10)     , mech_map_saprc99_q( 10)  &
    / 'ARO2            ', 4             /
DATA  conv_fac_saprc99_q( 10) / 1.00          /

DATA  spmh_nam_saprc99_q( 11)     , spmh_map_saprc99_q( 11)  &
    / 'cymene_o        ', 11            /
DATA  mech_nam_saprc99_q( 11)     , mech_map_saprc99_q( 11)  &
    / 'ARO2            ', 4             /
DATA  conv_fac_saprc99_q( 11) / 1.00          /

DATA  spmh_nam_saprc99_q( 12)     , spmh_map_saprc99_q( 12)  &
    / 'phellandrene_a  ', 12            /
DATA  mech_nam_saprc99_q( 12)     , mech_map_saprc99_q( 12)  &
    / 'TRP1            ', 2             /
DATA  conv_fac_saprc99_q( 12) / 1.00          /

DATA  spmh_nam_saprc99_q( 13)     , spmh_map_saprc99_q( 13)  &
    / 'thujene_a       ', 13            /
DATA  mech_nam_saprc99_q( 13)     , mech_map_saprc99_q( 13)  &
    / 'TRP1            ', 2             /
DATA  conv_fac_saprc99_q( 13) / 1.00          /

DATA  spmh_nam_saprc99_q( 14)     , spmh_map_saprc99_q( 14)  &
    / 'terpinene_a     ', 14            /
DATA  mech_nam_saprc99_q( 14)     , mech_map_saprc99_q( 14)  &
    / 'TRP1            ', 2             /
DATA  conv_fac_saprc99_q( 14) / 1.00          /

DATA  spmh_nam_saprc99_q( 15)     , spmh_map_saprc99_q( 15)  &
    / 'terpinene_g     ', 15            /
DATA  mech_nam_saprc99_q( 15)     , mech_map_saprc99_q( 15)  &
    / 'TRP1            ', 2             /
DATA  conv_fac_saprc99_q( 15) / 1.00          /

DATA  spmh_nam_saprc99_q( 16)     , spmh_map_saprc99_q( 16)  &
    / 'terpinolene     ', 16            /
DATA  mech_nam_saprc99_q( 16)     , mech_map_saprc99_q( 16)  &
    / 'TRP1            ', 2             /
DATA  conv_fac_saprc99_q( 16) / 1.00          /

DATA  spmh_nam_saprc99_q( 17)     , spmh_map_saprc99_q( 17)  &
    / 'phellandrene_b  ', 17            /
DATA  mech_nam_saprc99_q( 17)     , mech_map_saprc99_q( 17)  &
    / 'TRP1            ', 2             /
DATA  conv_fac_saprc99_q( 17) / 1.00          /

DATA  spmh_nam_saprc99_q( 18)     , spmh_map_saprc99_q( 18)  &
    / 'camphene        ', 18            /
DATA  mech_nam_saprc99_q( 18)     , mech_map_saprc99_q( 18)  &
    / 'TRP1            ', 2             /
DATA  conv_fac_saprc99_q( 18) / 1.00          /

DATA  spmh_nam_saprc99_q( 19)     , spmh_map_saprc99_q( 19)  &
    / 'bornene         ', 19            /
DATA  mech_nam_saprc99_q( 19)     , mech_map_saprc99_q( 19)  &
    / 'TRP1            ', 2             /
DATA  conv_fac_saprc99_q( 19) / 1.00          /

DATA  spmh_nam_saprc99_q( 20)     , spmh_map_saprc99_q( 20)  &
    / 'fenchene_a      ', 20            /
DATA  mech_nam_saprc99_q( 20)     , mech_map_saprc99_q( 20)  &
    / 'TRP1            ', 2             /
DATA  conv_fac_saprc99_q( 20) / 1.00          /

DATA  spmh_nam_saprc99_q( 21)     , spmh_map_saprc99_q( 21)  &
    / 'ocimene_al      ', 21            /
DATA  mech_nam_saprc99_q( 21)     , mech_map_saprc99_q( 21)  &
    / 'TRP1            ', 2             /
DATA  conv_fac_saprc99_q( 21) / 1.00          /

DATA  spmh_nam_saprc99_q( 22)     , spmh_map_saprc99_q( 22)  &
    / 'ocimene_c_b     ', 22            /
DATA  mech_nam_saprc99_q( 22)     , mech_map_saprc99_q( 22)  &
    / 'TRP1            ', 2             /
DATA  conv_fac_saprc99_q( 22) / 1.00          /

DATA  spmh_nam_saprc99_q( 23)     , spmh_map_saprc99_q( 23)  &
    / 'tricyclene      ', 23            /
DATA  mech_nam_saprc99_q( 23)     , mech_map_saprc99_q( 23)  &
    / 'ALK5            ', 5             /
DATA  conv_fac_saprc99_q( 23) / 1.00          /

DATA  spmh_nam_saprc99_q( 24)     , spmh_map_saprc99_q( 24)  &
    / 'estragole       ', 24            /
DATA  mech_nam_saprc99_q( 24)     , mech_map_saprc99_q( 24)  &
    / 'TRP1            ', 2             /
DATA  conv_fac_saprc99_q( 24) / 1.00          /

DATA  spmh_nam_saprc99_q( 25)     , spmh_map_saprc99_q( 25)  &
    / 'camphor         ', 25            /
DATA  mech_nam_saprc99_q( 25)     , mech_map_saprc99_q( 25)  &
    / 'TRP1            ', 2             /
DATA  conv_fac_saprc99_q( 25) / 1.00          /

DATA  spmh_nam_saprc99_q( 26)     , spmh_map_saprc99_q( 26)  &
    / 'fenchone        ', 26            /
DATA  mech_nam_saprc99_q( 26)     , mech_map_saprc99_q( 26)  &
    / 'ALK5            ', 5             /
DATA  conv_fac_saprc99_q( 26) / 1.00          /

DATA  spmh_nam_saprc99_q( 27)     , spmh_map_saprc99_q( 27)  &
    / 'piperitone      ', 27            /
DATA  mech_nam_saprc99_q( 27)     , mech_map_saprc99_q( 27)  &
    / 'TRP1            ', 2             /
DATA  conv_fac_saprc99_q( 27) / 1.00          /

DATA  spmh_nam_saprc99_q( 28)     , spmh_map_saprc99_q( 28)  &
    / 'thujone_a       ', 28            /
DATA  mech_nam_saprc99_q( 28)     , mech_map_saprc99_q( 28)  &
    / 'ALK5            ', 5             /
DATA  conv_fac_saprc99_q( 28) / 1.00          /

DATA  spmh_nam_saprc99_q( 29)     , spmh_map_saprc99_q( 29)  &
    / 'thujone_b       ', 29            /
DATA  mech_nam_saprc99_q( 29)     , mech_map_saprc99_q( 29)  &
    / 'ALK5            ', 5             /
DATA  conv_fac_saprc99_q( 29) / 1.00          /

DATA  spmh_nam_saprc99_q( 30)     , spmh_map_saprc99_q( 30)  &
    / 'cineole_1_8     ', 30            /
DATA  mech_nam_saprc99_q( 30)     , mech_map_saprc99_q( 30)  &
    / 'ALK5            ', 5             /
DATA  conv_fac_saprc99_q( 30) / 1.00          /

DATA  spmh_nam_saprc99_q( 31)     , spmh_map_saprc99_q( 31)  &
    / 'borneol         ', 31            /
DATA  mech_nam_saprc99_q( 31)     , mech_map_saprc99_q( 31)  &
    / 'ALK5            ', 5             /
DATA  conv_fac_saprc99_q( 31) / 1.00          /

DATA  spmh_nam_saprc99_q( 32)     , spmh_map_saprc99_q( 32)  &
    / 'linalool        ', 32            /
DATA  mech_nam_saprc99_q( 32)     , mech_map_saprc99_q( 32)  &
    / 'TRP1            ', 2             /
DATA  conv_fac_saprc99_q( 32) / 1.00          /

DATA  spmh_nam_saprc99_q( 33)     , spmh_map_saprc99_q( 33)  &
    / 'terpineol_4     ', 33            /
DATA  mech_nam_saprc99_q( 33)     , mech_map_saprc99_q( 33)  &
    / 'TRP1            ', 2             /
DATA  conv_fac_saprc99_q( 33) / 1.00          /

DATA  spmh_nam_saprc99_q( 34)     , spmh_map_saprc99_q( 34)  &
    / 'terpineol_a     ', 34            /
DATA  mech_nam_saprc99_q( 34)     , mech_map_saprc99_q( 34)  &
    / 'TRP1            ', 2             /
DATA  conv_fac_saprc99_q( 34) / 1.00          /

DATA  spmh_nam_saprc99_q( 35)     , spmh_map_saprc99_q( 35)  &
    / 'linalool_OXD_c  ', 35            /
DATA  mech_nam_saprc99_q( 35)     , mech_map_saprc99_q( 35)  &
    / 'TRP1            ', 2             /
DATA  conv_fac_saprc99_q( 35) / 1.00          /

DATA  spmh_nam_saprc99_q( 36)     , spmh_map_saprc99_q( 36)  &
    / 'linalool_OXD_t  ', 36            /
DATA  mech_nam_saprc99_q( 36)     , mech_map_saprc99_q( 36)  &
    / 'TRP1            ', 2             /
DATA  conv_fac_saprc99_q( 36) / 1.00          /

DATA  spmh_nam_saprc99_q( 37)     , spmh_map_saprc99_q( 37)  &
    / 'ionone_b        ', 37            /
DATA  mech_nam_saprc99_q( 37)     , mech_map_saprc99_q( 37)  &
    / 'TRP1            ', 2             /
DATA  conv_fac_saprc99_q( 37) / 1.00          /

DATA  spmh_nam_saprc99_q( 38)     , spmh_map_saprc99_q( 38)  &
    / 'bornyl_ACT      ', 38            /
DATA  mech_nam_saprc99_q( 38)     , mech_map_saprc99_q( 38)  &
    / 'ALK5            ', 5             /
DATA  conv_fac_saprc99_q( 38) / 1.00          /

DATA  spmh_nam_saprc99_q( 39)     , spmh_map_saprc99_q( 39)  &
    / 'farnescene_a    ', 39            /
DATA  mech_nam_saprc99_q( 39)     , mech_map_saprc99_q( 39)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_q( 39) / 15.00         /

DATA  spmh_nam_saprc99_q( 40)     , spmh_map_saprc99_q( 40)  &
    / 'caryophyllene_b ', 40            /
DATA  mech_nam_saprc99_q( 40)     , mech_map_saprc99_q( 40)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_q( 40) / 15.00         /

DATA  spmh_nam_saprc99_q( 41)     , spmh_map_saprc99_q( 41)  &
    / 'acoradiene      ', 41            /
DATA  mech_nam_saprc99_q( 41)     , mech_map_saprc99_q( 41)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_q( 41) / 15.00         /

DATA  spmh_nam_saprc99_q( 42)     , spmh_map_saprc99_q( 42)  &
    / 'aromadendrene   ', 42            /
DATA  mech_nam_saprc99_q( 42)     , mech_map_saprc99_q( 42)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_q( 42) / 15.00         /

DATA  spmh_nam_saprc99_q( 43)     , spmh_map_saprc99_q( 43)  &
    / 'bergamotene_a   ', 43            /
DATA  mech_nam_saprc99_q( 43)     , mech_map_saprc99_q( 43)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_q( 43) / 15.00         /

DATA  spmh_nam_saprc99_q( 44)     , spmh_map_saprc99_q( 44)  &
    / 'bergamotene_b   ', 44            /
DATA  mech_nam_saprc99_q( 44)     , mech_map_saprc99_q( 44)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_q( 44) / 15.00         /

DATA  spmh_nam_saprc99_q( 45)     , spmh_map_saprc99_q( 45)  &
    / 'bisabolene_a    ', 45            /
DATA  mech_nam_saprc99_q( 45)     , mech_map_saprc99_q( 45)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_q( 45) / 15.00         /

DATA  spmh_nam_saprc99_q( 46)     , spmh_map_saprc99_q( 46)  &
    / 'bisabolene_b    ', 46            /
DATA  mech_nam_saprc99_q( 46)     , mech_map_saprc99_q( 46)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_q( 46) / 15.00         /

DATA  spmh_nam_saprc99_q( 47)     , spmh_map_saprc99_q( 47)  &
    / 'bourbonene_b    ', 47            /
DATA  mech_nam_saprc99_q( 47)     , mech_map_saprc99_q( 47)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_q( 47) / 15.00         /

DATA  spmh_nam_saprc99_q( 48)     , spmh_map_saprc99_q( 48)  &
    / 'cadinene_d      ', 48            /
DATA  mech_nam_saprc99_q( 48)     , mech_map_saprc99_q( 48)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_q( 48) / 15.00         /

DATA  spmh_nam_saprc99_q( 49)     , spmh_map_saprc99_q( 49)  &
    / 'cadinene_g      ', 49            /
DATA  mech_nam_saprc99_q( 49)     , mech_map_saprc99_q( 49)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_q( 49) / 15.00         /

DATA  spmh_nam_saprc99_q( 50)     , spmh_map_saprc99_q( 50)  &
    / 'cedrene_a       ', 50            /
DATA  mech_nam_saprc99_q( 50)     , mech_map_saprc99_q( 50)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_q( 50) / 15.00         /

DATA  spmh_nam_saprc99_q( 51)     , spmh_map_saprc99_q( 51)  &
    / 'copaene_a       ', 51            /
DATA  mech_nam_saprc99_q( 51)     , mech_map_saprc99_q( 51)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_q( 51) / 15.00         /

DATA  spmh_nam_saprc99_q( 52)     , spmh_map_saprc99_q( 52)  &
    / 'cubebene_a      ', 52            /
DATA  mech_nam_saprc99_q( 52)     , mech_map_saprc99_q( 52)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_q( 52) / 15.00         /

DATA  spmh_nam_saprc99_q( 53)     , spmh_map_saprc99_q( 53)  &
    / 'cubebene_b      ', 53            /
DATA  mech_nam_saprc99_q( 53)     , mech_map_saprc99_q( 53)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_q( 53) / 15.00         /

DATA  spmh_nam_saprc99_q( 54)     , spmh_map_saprc99_q( 54)  &
    / 'elemene_b       ', 54            /
DATA  mech_nam_saprc99_q( 54)     , mech_map_saprc99_q( 54)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_q( 54) / 15.00         /

DATA  spmh_nam_saprc99_q( 55)     , spmh_map_saprc99_q( 55)  &
    / 'farnescene_b    ', 55            /
DATA  mech_nam_saprc99_q( 55)     , mech_map_saprc99_q( 55)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_q( 55) / 15.00         /

DATA  spmh_nam_saprc99_q( 56)     , spmh_map_saprc99_q( 56)  &
    / 'germacrene_B    ', 56            /
DATA  mech_nam_saprc99_q( 56)     , mech_map_saprc99_q( 56)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_q( 56) / 15.00         /

DATA  spmh_nam_saprc99_q( 57)     , spmh_map_saprc99_q( 57)  &
    / 'germacrene_D    ', 57            /
DATA  mech_nam_saprc99_q( 57)     , mech_map_saprc99_q( 57)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_q( 57) / 15.00         /

DATA  spmh_nam_saprc99_q( 58)     , spmh_map_saprc99_q( 58)  &
    / 'gurjunene_b     ', 58            /
DATA  mech_nam_saprc99_q( 58)     , mech_map_saprc99_q( 58)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_q( 58) / 15.00         /

DATA  spmh_nam_saprc99_q( 59)     , spmh_map_saprc99_q( 59)  &
    / 'humulene_a      ', 59            /
DATA  mech_nam_saprc99_q( 59)     , mech_map_saprc99_q( 59)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_q( 59) / 15.00         /

DATA  spmh_nam_saprc99_q( 60)     , spmh_map_saprc99_q( 60)  &
    / 'humulene_g      ', 60            /
DATA  mech_nam_saprc99_q( 60)     , mech_map_saprc99_q( 60)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_q( 60) / 15.00         /

DATA  spmh_nam_saprc99_q( 61)     , spmh_map_saprc99_q( 61)  &
    / 'isolongifolene  ', 61            /
DATA  mech_nam_saprc99_q( 61)     , mech_map_saprc99_q( 61)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_q( 61) / 15.00         /

DATA  spmh_nam_saprc99_q( 62)     , spmh_map_saprc99_q( 62)  &
    / 'longifolene     ', 62            /
DATA  mech_nam_saprc99_q( 62)     , mech_map_saprc99_q( 62)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_q( 62) / 15.00         /

DATA  spmh_nam_saprc99_q( 63)     , spmh_map_saprc99_q( 63)  &
    / 'longipinene     ', 63            /
DATA  mech_nam_saprc99_q( 63)     , mech_map_saprc99_q( 63)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_q( 63) / 15.00         /

DATA  spmh_nam_saprc99_q( 64)     , spmh_map_saprc99_q( 64)  &
    / 'muurolene_a     ', 64            /
DATA  mech_nam_saprc99_q( 64)     , mech_map_saprc99_q( 64)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_q( 64) / 15.00         /

DATA  spmh_nam_saprc99_q( 65)     , spmh_map_saprc99_q( 65)  &
    / 'muurolene_g     ', 65            /
DATA  mech_nam_saprc99_q( 65)     , mech_map_saprc99_q( 65)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_q( 65) / 15.00         /

DATA  spmh_nam_saprc99_q( 66)     , spmh_map_saprc99_q( 66)  &
    / 'selinene_b      ', 66            /
DATA  mech_nam_saprc99_q( 66)     , mech_map_saprc99_q( 66)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_q( 66) / 15.00         /

DATA  spmh_nam_saprc99_q( 67)     , spmh_map_saprc99_q( 67)  &
    / 'selinene_d      ', 67            /
DATA  mech_nam_saprc99_q( 67)     , mech_map_saprc99_q( 67)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_q( 67) / 15.00         /

DATA  spmh_nam_saprc99_q( 68)     , spmh_map_saprc99_q( 68)  &
    / 'nerolidol_c     ', 68            /
DATA  mech_nam_saprc99_q( 68)     , mech_map_saprc99_q( 68)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_q( 68) / 15.00         /

DATA  spmh_nam_saprc99_q( 69)     , spmh_map_saprc99_q( 69)  &
    / 'nerolidol_t     ', 69            /
DATA  mech_nam_saprc99_q( 69)     , mech_map_saprc99_q( 69)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_q( 69) / 15.00         /

DATA  spmh_nam_saprc99_q( 70)     , spmh_map_saprc99_q( 70)  &
    / 'cedrol          ', 70            /
DATA  mech_nam_saprc99_q( 70)     , mech_map_saprc99_q( 70)  &
    / 'ALK5            ', 5             /
DATA  conv_fac_saprc99_q( 70) / 1.00          /

DATA  spmh_nam_saprc99_q( 71)     , spmh_map_saprc99_q( 71)  &
    / 'MBO_2m3e2ol     ', 71            /
DATA  mech_nam_saprc99_q( 71)     , mech_map_saprc99_q( 71)  &
    / 'OLE1            ', 7             /
DATA  conv_fac_saprc99_q( 71) / 1.00          /

DATA  spmh_nam_saprc99_q( 72)     , spmh_map_saprc99_q( 72)  &
    / 'methanol        ', 72            /
DATA  mech_nam_saprc99_q( 72)     , mech_map_saprc99_q( 72)  &
    / 'MEOH            ', 8             /
DATA  conv_fac_saprc99_q( 72) / 1.00          /

DATA  spmh_nam_saprc99_q( 73)     , spmh_map_saprc99_q( 73)  &
    / 'acetone         ', 73            /
DATA  mech_nam_saprc99_q( 73)     , mech_map_saprc99_q( 73)  &
    / 'ACET            ', 9             /
DATA  conv_fac_saprc99_q( 73) / 1.00          /

DATA  spmh_nam_saprc99_q( 74)     , spmh_map_saprc99_q( 74)  &
    / 'methane         ', 74            /
DATA  mech_nam_saprc99_q( 74)     , mech_map_saprc99_q( 74)  &
    / 'CH4             ', 10            /
DATA  conv_fac_saprc99_q( 74) / 1.00          /

DATA  spmh_nam_saprc99_q( 75)     , spmh_map_saprc99_q( 75)  &
    / 'ammonia         ', 75            /
DATA  mech_nam_saprc99_q( 75)     , mech_map_saprc99_q( 75)  &
    / 'NH3             ', 11            /
DATA  conv_fac_saprc99_q( 75) / 1.00          /

DATA  spmh_nam_saprc99_q( 76)     , spmh_map_saprc99_q( 76)  &
    / 'nitric_OXD      ', 77            /
DATA  mech_nam_saprc99_q( 76)     , mech_map_saprc99_q( 76)  &
    / 'NO              ', 12            /
DATA  conv_fac_saprc99_q( 76) / 1.00          /

DATA  spmh_nam_saprc99_q( 77)     , spmh_map_saprc99_q( 77)  &
    / 'acetaldehyde    ', 78            /
DATA  mech_nam_saprc99_q( 77)     , mech_map_saprc99_q( 77)  &
    / 'CCHO            ', 13            /
DATA  conv_fac_saprc99_q( 77) / 1.00          /

DATA  spmh_nam_saprc99_q( 78)     , spmh_map_saprc99_q( 78)  &
    / 'ethanol         ', 79            /
DATA  mech_nam_saprc99_q( 78)     , mech_map_saprc99_q( 78)  &
    / 'ALK3            ', 14            /
DATA  conv_fac_saprc99_q( 78) / 1.00          /

DATA  spmh_nam_saprc99_q( 79)     , spmh_map_saprc99_q( 79)  &
    / 'formic_acid     ', 80            /
DATA  mech_nam_saprc99_q( 79)     , mech_map_saprc99_q( 79)  &
    / 'HCOOH           ', 15            /
DATA  conv_fac_saprc99_q( 79) / 1.00          /

DATA  spmh_nam_saprc99_q( 80)     , spmh_map_saprc99_q( 80)  &
    / 'formaldehyde    ', 81            /
DATA  mech_nam_saprc99_q( 80)     , mech_map_saprc99_q( 80)  &
    / 'HCHO            ', 16            /
DATA  conv_fac_saprc99_q( 80) / 1.00          /

DATA  spmh_nam_saprc99_q( 81)     , spmh_map_saprc99_q( 81)  &
    / 'acetic_acid     ', 82            /
DATA  mech_nam_saprc99_q( 81)     , mech_map_saprc99_q( 81)  &
    / 'CCO_OH          ', 17            /
DATA  conv_fac_saprc99_q( 81) / 1.00          /

DATA  spmh_nam_saprc99_q( 82)     , spmh_map_saprc99_q( 82)  &
    / 'MBO_3m2e1ol     ', 83            /
DATA  mech_nam_saprc99_q( 82)     , mech_map_saprc99_q( 82)  &
    / 'OLE2            ', 3             /
DATA  conv_fac_saprc99_q( 82) / 1.00          /

DATA  spmh_nam_saprc99_q( 83)     , spmh_map_saprc99_q( 83)  &
    / 'MBO_3m3e1ol     ', 84            /
DATA  mech_nam_saprc99_q( 83)     , mech_map_saprc99_q( 83)  &
    / 'OLE1            ', 7             /
DATA  conv_fac_saprc99_q( 83) / 1.00          /

DATA  spmh_nam_saprc99_q( 84)     , spmh_map_saprc99_q( 84)  &
    / 'benzaldehyde    ', 85            /
DATA  mech_nam_saprc99_q( 84)     , mech_map_saprc99_q( 84)  &
    / 'BALD            ', 18            /
DATA  conv_fac_saprc99_q( 84) / 1.00          /

DATA  spmh_nam_saprc99_q( 85)     , spmh_map_saprc99_q( 85)  &
    / 'butanone_2      ', 86            /
DATA  mech_nam_saprc99_q( 85)     , mech_map_saprc99_q( 85)  &
    / 'MEK             ', 19            /
DATA  conv_fac_saprc99_q( 85) / 1.00          /

DATA  spmh_nam_saprc99_q( 86)     , spmh_map_saprc99_q( 86)  &
    / 'decanal         ', 87            /
DATA  mech_nam_saprc99_q( 86)     , mech_map_saprc99_q( 86)  &
    / 'RCHO            ', 20            /
DATA  conv_fac_saprc99_q( 86) / 1.00          /

DATA  spmh_nam_saprc99_q( 87)     , spmh_map_saprc99_q( 87)  &
    / 'dodecene_1      ', 88            /
DATA  mech_nam_saprc99_q( 87)     , mech_map_saprc99_q( 87)  &
    / 'OLE1            ', 7             /
DATA  conv_fac_saprc99_q( 87) / 1.00          /

DATA  spmh_nam_saprc99_q( 88)     , spmh_map_saprc99_q( 88)  &
    / 'geranyl_acetone ', 89            /
DATA  mech_nam_saprc99_q( 88)     , mech_map_saprc99_q( 88)  &
    / 'TRP1            ', 2             /
DATA  conv_fac_saprc99_q( 88) / 1.00          /

DATA  spmh_nam_saprc99_q( 89)     , spmh_map_saprc99_q( 89)  &
    / 'heptanal        ', 90            /
DATA  mech_nam_saprc99_q( 89)     , mech_map_saprc99_q( 89)  &
    / 'RCHO            ', 20            /
DATA  conv_fac_saprc99_q( 89) / 1.00          /

DATA  spmh_nam_saprc99_q( 90)     , spmh_map_saprc99_q( 90)  &
    / 'heptane         ', 91            /
DATA  mech_nam_saprc99_q( 90)     , mech_map_saprc99_q( 90)  &
    / 'ALK5            ', 5             /
DATA  conv_fac_saprc99_q( 90) / 1.00          /

DATA  spmh_nam_saprc99_q( 91)     , spmh_map_saprc99_q( 91)  &
    / 'hexane          ', 92            /
DATA  mech_nam_saprc99_q( 91)     , mech_map_saprc99_q( 91)  &
    / 'ALK4            ', 21            /
DATA  conv_fac_saprc99_q( 91) / 1.00          /

DATA  spmh_nam_saprc99_q( 92)     , spmh_map_saprc99_q( 92)  &
    / 'met_benzoate    ', 93            /
DATA  mech_nam_saprc99_q( 92)     , mech_map_saprc99_q( 92)  &
    / 'ARO1            ', 22            /
DATA  conv_fac_saprc99_q( 92) / 1.00          /

DATA  spmh_nam_saprc99_q( 93)     , spmh_map_saprc99_q( 93)  &
    / 'met_heptenone   ', 94            /
DATA  mech_nam_saprc99_q( 93)     , mech_map_saprc99_q( 93)  &
    / 'OLE2            ', 3             /
DATA  conv_fac_saprc99_q( 93) / 1.00          /

DATA  spmh_nam_saprc99_q( 94)     , spmh_map_saprc99_q( 94)  &
    / 'neryl_acetone   ', 95            /
DATA  mech_nam_saprc99_q( 94)     , mech_map_saprc99_q( 94)  &
    / 'OLE2            ', 3             /
DATA  conv_fac_saprc99_q( 94) / 1.00          /

DATA  spmh_nam_saprc99_q( 95)     , spmh_map_saprc99_q( 95)  &
    / 'nonanal         ', 96            /
DATA  mech_nam_saprc99_q( 95)     , mech_map_saprc99_q( 95)  &
    / 'RCHO            ', 20            /
DATA  conv_fac_saprc99_q( 95) / 1.00          /

DATA  spmh_nam_saprc99_q( 96)     , spmh_map_saprc99_q( 96)  &
    / 'nonenal         ', 97            /
DATA  mech_nam_saprc99_q( 96)     , mech_map_saprc99_q( 96)  &
    / 'OLE1            ', 7             /
DATA  conv_fac_saprc99_q( 96) / 1.00          /

DATA  spmh_nam_saprc99_q( 97)     , spmh_map_saprc99_q( 97)  &
    / 'octanal         ', 98            /
DATA  mech_nam_saprc99_q( 97)     , mech_map_saprc99_q( 97)  &
    / 'RCHO            ', 20            /
DATA  conv_fac_saprc99_q( 97) / 1.00          /

DATA  spmh_nam_saprc99_q( 98)     , spmh_map_saprc99_q( 98)  &
    / 'octanol         ', 99            /
DATA  mech_nam_saprc99_q( 98)     , mech_map_saprc99_q( 98)  &
    / 'ALK5            ', 5             /
DATA  conv_fac_saprc99_q( 98) / 1.00          /

DATA  spmh_nam_saprc99_q( 99)     , spmh_map_saprc99_q( 99)  &
    / 'octenol_1e3ol   ', 100           /
DATA  mech_nam_saprc99_q( 99)     , mech_map_saprc99_q( 99)  &
    / 'OLE1            ', 7             /
DATA  conv_fac_saprc99_q( 99) / 1.00          /

DATA  spmh_nam_saprc99_q(100)     , spmh_map_saprc99_q(100)  &
    / 'oxopentanal     ', 101           /
DATA  mech_nam_saprc99_q(100)     , mech_map_saprc99_q(100)  &
    / 'RCHO            ', 20            /
DATA  conv_fac_saprc99_q(100) / 1.00          /

DATA  spmh_nam_saprc99_q(101)     , spmh_map_saprc99_q(101)  &
    / 'pentane         ', 102           /
DATA  mech_nam_saprc99_q(101)     , mech_map_saprc99_q(101)  &
    / 'ALK4            ', 21            /
DATA  conv_fac_saprc99_q(101) / 1.00          /

DATA  spmh_nam_saprc99_q(102)     , spmh_map_saprc99_q(102)  &
    / 'phenyl_CCO      ', 103           /
DATA  mech_nam_saprc99_q(102)     , mech_map_saprc99_q(102)  &
    / 'ARO1            ', 22            /
DATA  conv_fac_saprc99_q(102) / 1.00          /

DATA  spmh_nam_saprc99_q(103)     , spmh_map_saprc99_q(103)  &
    / 'pyruvic_acid    ', 104           /
DATA  mech_nam_saprc99_q(103)     , mech_map_saprc99_q(103)  &
    / 'BACL            ', 23            /
DATA  conv_fac_saprc99_q(103) / 1.00          /

DATA  spmh_nam_saprc99_q(104)     , spmh_map_saprc99_q(104)  &
    / 'terpinyl_ACT_a  ', 105           /
DATA  mech_nam_saprc99_q(104)     , mech_map_saprc99_q(104)  &
    / 'TRP1            ', 2             /
DATA  conv_fac_saprc99_q(104) / 1.00          /

DATA  spmh_nam_saprc99_q(105)     , spmh_map_saprc99_q(105)  &
    / 'tetradecene_1   ', 106           /
DATA  mech_nam_saprc99_q(105)     , mech_map_saprc99_q(105)  &
    / 'OLE1            ', 7             /
DATA  conv_fac_saprc99_q(105) / 1.00          /

DATA  spmh_nam_saprc99_q(106)     , spmh_map_saprc99_q(106)  &
    / 'toluene         ', 107           /
DATA  mech_nam_saprc99_q(106)     , mech_map_saprc99_q(106)  &
    / 'ARO1            ', 22            /
DATA  conv_fac_saprc99_q(106) / 1.00          /

DATA  spmh_nam_saprc99_q(107)     , spmh_map_saprc99_q(107)  &
    / 'carbon_monoxide ', 108           /
DATA  mech_nam_saprc99_q(107)     , mech_map_saprc99_q(107)  &
    / 'CO              ', 24            /
DATA  conv_fac_saprc99_q(107) / 1.00          /

DATA  spmh_nam_saprc99_q(108)     , spmh_map_saprc99_q(108)  &
    / 'butene          ', 109           /
DATA  mech_nam_saprc99_q(108)     , mech_map_saprc99_q(108)  &
    / 'OLE1            ', 7             /
DATA  conv_fac_saprc99_q(108) / 1.00          /

DATA  spmh_nam_saprc99_q(109)     , spmh_map_saprc99_q(109)  &
    / 'ethane          ', 110           /
DATA  mech_nam_saprc99_q(109)     , mech_map_saprc99_q(109)  &
    / 'ALK1            ', 25            /
DATA  conv_fac_saprc99_q(109) / 1.00          /

DATA  spmh_nam_saprc99_q(110)     , spmh_map_saprc99_q(110)  &
    / 'ethene          ', 111           /
DATA  mech_nam_saprc99_q(110)     , mech_map_saprc99_q(110)  &
    / 'ETHENE          ', 26            /
DATA  conv_fac_saprc99_q(110) / 1.00          /

DATA  spmh_nam_saprc99_q(111)     , spmh_map_saprc99_q(111)  &
    / 'propane         ', 113           /
DATA  mech_nam_saprc99_q(111)     , mech_map_saprc99_q(111)  &
    / 'ALK2            ', 27            /
DATA  conv_fac_saprc99_q(111) / 1.00          /

DATA  spmh_nam_saprc99_q(112)     , spmh_map_saprc99_q(112)  &
    / 'propene         ', 114           /
DATA  mech_nam_saprc99_q(112)     , mech_map_saprc99_q(112)  &
    / 'OLE1            ', 7             /
DATA  conv_fac_saprc99_q(112) / 1.00          /

DATA  spmh_nam_saprc99_q(113)     , spmh_map_saprc99_q(113)  &
    / 'diallyl_2s      ', 117           /
DATA  mech_nam_saprc99_q(113)     , mech_map_saprc99_q(113)  &
    / 'OLE1            ', 7             /
DATA  conv_fac_saprc99_q(113) / 1.00          /

DATA  spmh_nam_saprc99_q(114)     , spmh_map_saprc99_q(114)  &
    / '2met_2s         ', 118           /
DATA  mech_nam_saprc99_q(114)     , mech_map_saprc99_q(114)  &
    / 'ALK3            ', 14            /
DATA  conv_fac_saprc99_q(114) / 1.00          /

DATA  spmh_nam_saprc99_q(115)     , spmh_map_saprc99_q(115)  &
    / '2met_s          ', 119           /
DATA  mech_nam_saprc99_q(115)     , mech_map_saprc99_q(115)  &
    / 'ALK4            ', 21            /
DATA  conv_fac_saprc99_q(115) / 1.00          /

DATA  spmh_nam_saprc99_q(116)     , spmh_map_saprc99_q(116)  &
    / 'met_chloride    ', 120           /
DATA  mech_nam_saprc99_q(116)     , mech_map_saprc99_q(116)  &
    / 'ALK2            ', 27            /
DATA  conv_fac_saprc99_q(116) / 1.00          /

DATA  spmh_nam_saprc99_q(117)     , spmh_map_saprc99_q(117)  &
    / 'met_bromide     ', 121           /
DATA  mech_nam_saprc99_q(117)     , mech_map_saprc99_q(117)  &
    / 'ALK2            ', 27            /
DATA  conv_fac_saprc99_q(117) / 1.00          /

DATA  spmh_nam_saprc99_q(118)     , spmh_map_saprc99_q(118)  &
    / 'met_iodide      ', 122           /
DATA  mech_nam_saprc99_q(118)     , mech_map_saprc99_q(118)  &
    / 'ALK2            ', 27            /
DATA  conv_fac_saprc99_q(118) / 1.00          /

DATA  spmh_nam_saprc99_q(119)     , spmh_map_saprc99_q(119)  &
    / 'met_mercaptan   ', 124           /
DATA  mech_nam_saprc99_q(119)     , mech_map_saprc99_q(119)  &
    / 'ALK2            ', 27            /
DATA  conv_fac_saprc99_q(119) / 1.00          /

DATA  spmh_nam_saprc99_q(120)     , spmh_map_saprc99_q(120)  &
    / 'met_propenyl_2s ', 125           /
DATA  mech_nam_saprc99_q(120)     , mech_map_saprc99_q(120)  &
    / 'OLE1            ', 7             /
DATA  conv_fac_saprc99_q(120) / 1.00          /

DATA  spmh_nam_saprc99_q(121)     , spmh_map_saprc99_q(121)  &
    / 'PPPP_2s         ', 126           /
DATA  mech_nam_saprc99_q(121)     , mech_map_saprc99_q(121)  &
    / 'OLE1            ', 7             /
DATA  conv_fac_saprc99_q(121) / 1.00          /

DATA  spmh_nam_saprc99_q(122)     , spmh_map_saprc99_q(122)  &
    / '2met_nonatriene ', 127           /
DATA  mech_nam_saprc99_q(122)     , mech_map_saprc99_q(122)  &
    / 'TRP1            ', 2             /
DATA  conv_fac_saprc99_q(122) / 1.00          /

DATA  spmh_nam_saprc99_q(123)     , spmh_map_saprc99_q(123)  &
    / 'met_salicylate  ', 128           /
DATA  mech_nam_saprc99_q(123)     , mech_map_saprc99_q(123)  &
    / 'ARO1            ', 22            /
DATA  conv_fac_saprc99_q(123) / 1.00          /

DATA  spmh_nam_saprc99_q(124)     , spmh_map_saprc99_q(124)  &
    / 'indole          ', 129           /
DATA  mech_nam_saprc99_q(124)     , mech_map_saprc99_q(124)  &
    / 'ARO2            ', 4             /
DATA  conv_fac_saprc99_q(124) / 1.00          /

DATA  spmh_nam_saprc99_q(125)     , spmh_map_saprc99_q(125)  &
    / 'jasmone         ', 130           /
DATA  mech_nam_saprc99_q(125)     , mech_map_saprc99_q(125)  &
    / 'TRP1            ', 2             /
DATA  conv_fac_saprc99_q(125) / 1.00          /

DATA  spmh_nam_saprc99_q(126)     , spmh_map_saprc99_q(126)  &
    / 'met_jasmonate   ', 131           /
DATA  mech_nam_saprc99_q(126)     , mech_map_saprc99_q(126)  &
    / 'TRP1            ', 2             /
DATA  conv_fac_saprc99_q(126) / 1.00          /

DATA  spmh_nam_saprc99_q(127)     , spmh_map_saprc99_q(127)  &
    / '3met_3DCTT      ', 132           /
DATA  mech_nam_saprc99_q(127)     , mech_map_saprc99_q(127)  &
    / 'XC              ', 6             /
DATA  conv_fac_saprc99_q(127) / 16.00         /

DATA  spmh_nam_saprc99_q(128)     , spmh_map_saprc99_q(128)  &
    / 'hexanal         ', 133           /
DATA  mech_nam_saprc99_q(128)     , mech_map_saprc99_q(128)  &
    / 'RCHO            ', 20            /
DATA  conv_fac_saprc99_q(128) / 1.00          /

DATA  spmh_nam_saprc99_q(129)     , spmh_map_saprc99_q(129)  &
    / 'hexanol_1       ', 134           /
DATA  mech_nam_saprc99_q(129)     , mech_map_saprc99_q(129)  &
    / 'ALK5            ', 5             /
DATA  conv_fac_saprc99_q(129) / 1.00          /

DATA  spmh_nam_saprc99_q(130)     , spmh_map_saprc99_q(130)  &
    / 'hexenal_c3      ', 135           /
DATA  mech_nam_saprc99_q(130)     , mech_map_saprc99_q(130)  &
    / 'OLE2            ', 3             /
DATA  conv_fac_saprc99_q(130) / 1.00          /

DATA  spmh_nam_saprc99_q(131)     , spmh_map_saprc99_q(131)  &
    / 'hexenal_t2      ', 136           /
DATA  mech_nam_saprc99_q(131)     , mech_map_saprc99_q(131)  &
    / 'OLE2            ', 3             /
DATA  conv_fac_saprc99_q(131) / 1.00          /

DATA  spmh_nam_saprc99_q(132)     , spmh_map_saprc99_q(132)  &
    / 'hexenol_c3      ', 137           /
DATA  mech_nam_saprc99_q(132)     , mech_map_saprc99_q(132)  &
    / 'OLE2            ', 3             /
DATA  conv_fac_saprc99_q(132) / 1.00          /

DATA  spmh_nam_saprc99_q(133)     , spmh_map_saprc99_q(133)  &
    / 'hexenyl_ACT_c3  ', 138           /
DATA  mech_nam_saprc99_q(133)     , mech_map_saprc99_q(133)  &
    / 'OLE2            ', 3             /
DATA  conv_fac_saprc99_q(133) / 1.00          /

DATA  spmh_nam_saprc99_q(134)     , spmh_map_saprc99_q(134)  &
    / 'homosalate      ', 134            /
DATA  mech_nam_saprc99_q(134)     , mech_map_saprc99_q(134)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99_q(134) / 1.0            /

DATA  spmh_nam_saprc99_q(135)     , spmh_map_saprc99_q(135)  &
    / 'Ehsalate        ', 135            /
DATA  mech_nam_saprc99_q(135)     , mech_map_saprc99_q(135)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99_q(135) / 1.0            /

DATA  spmh_nam_saprc99_q(136)     , spmh_map_saprc99_q(136)  &
    / 'pentanal        ', 136            /
DATA  mech_nam_saprc99_q(136)     , mech_map_saprc99_q(136)  &
    / 'RCHO            ', 27             /
DATA  conv_fac_saprc99_q(136) / 1.0            /

DATA  spmh_nam_saprc99_q(137)     , spmh_map_saprc99_q(137)  &
    / 'heptanone       ', 137            /
DATA  mech_nam_saprc99_q(137)     , mech_map_saprc99_q(137)  &
    / 'OLE2            ', 26             /
DATA  conv_fac_saprc99_q(137) / 1.0            /

DATA  spmh_nam_saprc99_q(138)     , spmh_map_saprc99_q(138)  &
    / 'anisole         ',138             /
DATA  mech_nam_saprc99_q(138)     , mech_map_saprc99_q(138)  &
    / 'BALD            ', 13             /
DATA  conv_fac_saprc99_q(138) / 1.0            /

DATA  spmh_nam_saprc99_q(139)     , spmh_map_saprc99_q(139)  &
    / 'verbenene       ',139             /
DATA  mech_nam_saprc99_q(139)     , mech_map_saprc99_q(139)  &
    / 'ARO2            ', 24             /
DATA  conv_fac_saprc99_q(139) / 1.0            /

DATA  spmh_nam_saprc99_q(140)     , spmh_map_saprc99_q(140)  &
    / 'benzyl-acetate  ',140             /
DATA  mech_nam_saprc99_q(140)     , mech_map_saprc99_q(140)  &
    / 'ARO1            ', 23             /
DATA  conv_fac_saprc99_q(140) / 1.0            /

DATA  spmh_nam_saprc99_q(141)     , spmh_map_saprc99_q(141)  &
    / 'myrtenal        ',141             /
DATA  mech_nam_saprc99_q(141)     , mech_map_saprc99_q(141)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99_q(141) / 1.0            /

DATA  spmh_nam_saprc99_q(142)     , spmh_map_saprc99_q(142)  &
    / 'benzyl-alcohol  ',142             /
DATA  mech_nam_saprc99_q(142)     , mech_map_saprc99_q(142)  &
    / 'ARO1            ', 23             /
DATA  conv_fac_saprc99_q(142) / 1.0            /

DATA  spmh_nam_saprc99_q(143)     , spmh_map_saprc99_q(143)  &
    / 'meta-cymenene   ',143             /
DATA  mech_nam_saprc99_q(143)     , mech_map_saprc99_q(143)  &
    / 'ARO2            ', 24             /
DATA  conv_fac_saprc99_q(143) / 1.0            /

DATA  spmh_nam_saprc99_q(144)     , spmh_map_saprc99_q(144)  &
    / 'ipsenol         ',144             /
DATA  mech_nam_saprc99_q(144)     , mech_map_saprc99_q(144)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99_q(144) / 1.0            /

DATA  spmh_nam_saprc99_q(145)     , spmh_map_saprc99_q(145)  &
    / 'Napthalene      ', 145            /
DATA  mech_nam_saprc99_q(145)     , mech_map_saprc99_q(145)  &
    / 'ARO2            ', 24             /
DATA  conv_fac_saprc99_q(145) / 1.0            /

end module map_cv2saprc99q