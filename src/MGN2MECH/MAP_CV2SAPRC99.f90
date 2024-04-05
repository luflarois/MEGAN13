module map_cv2saprc99
!=======================================================================

!  MAP_SAPRC99_CV2SAPRC99.EXT
!  This include file contains conversion table for 150 speciated species
!  to SAPRC99 species


!  MEGAN v2.10
!  INPUT version x.x

!  History:
!  Who          When       What
!  ---------------------------------------------------------------------
!  Tan          12/02/06 - Creates this file
!  bkoo         04/13/07 - Modified for new MECHANISM scheme
!  Tan          07/18/11 - Updated for MEGANv2.10
!=======================================================================

CHARACTER (LEN=16) :: map_saprc99mech
PARAMETER     (map_saprc99mech = 'SAPRC99         ')

INTEGER :: n_saprc99
PARAMETER     (n_saprc99 = 150)        ! Number of map species

CHARACTER (LEN=16) :: spmh_nam_saprc99( n_saprc99 )   ! speciated species name
INTEGER :: spmh_map_saprc99( n_saprc99 )   ! speciated species name
! mapped to SPC_SPCAT.EXT
CHARACTER (LEN=16) :: mech_nam_saprc99( n_saprc99 )   ! mechanism species
INTEGER :: mech_map_saprc99( n_saprc99 )   ! mechanism species mapped
! to SPC_SAPRC99.EXT
REAL :: conv_fac_saprc99( n_saprc99 )   ! conversion factor


DATA  spmh_nam_saprc99(  1)     , spmh_map_saprc99(  1)  &
    / 'isoprene        ', 1              /
DATA  mech_nam_saprc99(  1)     , mech_map_saprc99(  1)  &
    / 'ISOPRENE        ', 1              /
DATA  conv_fac_saprc99(  1) / 1.0            /
DATA  spmh_nam_saprc99(  2)     , spmh_map_saprc99(  2)  &
    / 'myrcene         ', 2              /
DATA  mech_nam_saprc99(  2)     , mech_map_saprc99(  2)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99(  2) / 1.0            /
DATA  spmh_nam_saprc99(  3)     , spmh_map_saprc99(  3)  &
    / 'sabinene        ', 3              /
DATA  mech_nam_saprc99(  3)     , mech_map_saprc99(  3)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99(  3) / 1.0            /
DATA  spmh_nam_saprc99(  4)     , spmh_map_saprc99(  4)  &
    / 'limonene        ', 4              /
DATA  mech_nam_saprc99(  4)     , mech_map_saprc99(  4)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99(  4) / 1.0            /
DATA  spmh_nam_saprc99(  5)     , spmh_map_saprc99(  5)  &
    / 'carene_3        ', 5              /
DATA  mech_nam_saprc99(  5)     , mech_map_saprc99(  5)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99(  5) / 1.0            /
DATA  spmh_nam_saprc99(  6)     , spmh_map_saprc99(  6)  &
    / 'ocimene_t_b     ', 6              /
DATA  mech_nam_saprc99(  6)     , mech_map_saprc99(  6)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99(  6) / 1.0            /
DATA  spmh_nam_saprc99(  7)     , spmh_map_saprc99(  7)  &
    / 'pinene_b        ', 7              /
DATA  mech_nam_saprc99(  7)     , mech_map_saprc99(  7)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99(  7) / 1.0            /
DATA  spmh_nam_saprc99(  8)     , spmh_map_saprc99(  8)  &
    / 'pinene_a        ', 8              /
DATA  mech_nam_saprc99(  8)     , mech_map_saprc99(  8)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99(  8) / 1.0            /
DATA  spmh_nam_saprc99(  9)     , spmh_map_saprc99(  9)  &
    / '2met_styrene    ', 9              /
DATA  mech_nam_saprc99(  9)     , mech_map_saprc99(  9)  &
    / 'OLE2            ', 26             /
DATA  conv_fac_saprc99(  9) / 1.0            /
DATA  spmh_nam_saprc99( 10)     , spmh_map_saprc99( 10)  &
    / 'cymene_p        ', 10             /
DATA  mech_nam_saprc99( 10)     , mech_map_saprc99( 10)  &
    / 'ARO2            ', 24             /
DATA  conv_fac_saprc99( 10) / 1.0            /
DATA  spmh_nam_saprc99( 11)     , spmh_map_saprc99( 11)  &
    / 'cymene_o        ', 11             /
DATA  mech_nam_saprc99( 11)     , mech_map_saprc99( 11)  &
    / 'ARO2            ', 24             /
DATA  conv_fac_saprc99( 11) / 1.0            /
DATA  spmh_nam_saprc99( 12)     , spmh_map_saprc99( 12)  &
    / 'phellandrene_a  ', 12             /
DATA  mech_nam_saprc99( 12)     , mech_map_saprc99( 12)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 12) / 1.0            /
DATA  spmh_nam_saprc99( 13)     , spmh_map_saprc99( 13)  &
    / 'thujene_a       ', 13             /
DATA  mech_nam_saprc99( 13)     , mech_map_saprc99( 13)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 13) / 1.0            /
DATA  spmh_nam_saprc99( 14)     , spmh_map_saprc99( 14)  &
    / 'terpinene_a     ', 14             /
DATA  mech_nam_saprc99( 14)     , mech_map_saprc99( 14)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 14) / 1.0            /
DATA  spmh_nam_saprc99( 15)     , spmh_map_saprc99( 15)  &
    / 'terpinene_g     ', 15             /
DATA  mech_nam_saprc99( 15)     , mech_map_saprc99( 15)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 15) / 1.0            /
DATA  spmh_nam_saprc99( 16)     , spmh_map_saprc99( 16)  &
    / 'terpinolene     ', 16             /
DATA  mech_nam_saprc99( 16)     , mech_map_saprc99( 16)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 16) / 1.0            /
DATA  spmh_nam_saprc99( 17)     , spmh_map_saprc99( 17)  &
    / 'phellandrene_b  ', 17             /
DATA  mech_nam_saprc99( 17)     , mech_map_saprc99( 17)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 17) / 1.0            /
DATA  spmh_nam_saprc99( 18)     , spmh_map_saprc99( 18)  &
    / 'camphene        ', 18             /
DATA  mech_nam_saprc99( 18)     , mech_map_saprc99( 18)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 18) / 1.0            /
DATA  spmh_nam_saprc99( 19)     , spmh_map_saprc99( 19)  &
    / 'bornene         ', 19             /
DATA  mech_nam_saprc99( 19)     , mech_map_saprc99( 19)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 19) / 1.0            /
DATA  spmh_nam_saprc99( 20)     , spmh_map_saprc99( 20)  &
    / 'fenchene_a      ', 20             /
DATA  mech_nam_saprc99( 20)     , mech_map_saprc99( 20)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 20) / 1.0            /
DATA  spmh_nam_saprc99( 21)     , spmh_map_saprc99( 21)  &
    / 'ocimene_al      ', 21             /
DATA  mech_nam_saprc99( 21)     , mech_map_saprc99( 21)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 21) / 1.0            /
DATA  spmh_nam_saprc99( 22)     , spmh_map_saprc99( 22)  &
    / 'ocimene_c_b     ', 22             /
DATA  mech_nam_saprc99( 22)     , mech_map_saprc99( 22)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 22) / 1.0            /
DATA  spmh_nam_saprc99( 23)     , spmh_map_saprc99( 23)  &
    / 'tricyclene      ', 23             /
DATA  mech_nam_saprc99( 23)     , mech_map_saprc99( 23)  &
    / 'ALK5            ', 22             /
DATA  conv_fac_saprc99( 23) / 1.0            /
DATA  spmh_nam_saprc99( 24)     , spmh_map_saprc99( 24)  &
    / 'estragole       ', 24             /
DATA  mech_nam_saprc99( 24)     , mech_map_saprc99( 24)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 24) / 1.0            /
DATA  spmh_nam_saprc99( 25)     , spmh_map_saprc99( 25)  &
    / 'camphor         ', 25             /
DATA  mech_nam_saprc99( 25)     , mech_map_saprc99( 25)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 25) / 1.0            /
DATA  spmh_nam_saprc99( 26)     , spmh_map_saprc99( 26)  &
    / 'fenchone        ', 26             /
DATA  mech_nam_saprc99( 26)     , mech_map_saprc99( 26)  &
    / 'ALK5            ', 22             /
DATA  conv_fac_saprc99( 26) / 1.0            /
DATA  spmh_nam_saprc99( 27)     , spmh_map_saprc99( 27)  &
    / 'piperitone      ', 27             /
DATA  mech_nam_saprc99( 27)     , mech_map_saprc99( 27)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 27) / 1.0            /
DATA  spmh_nam_saprc99( 28)     , spmh_map_saprc99( 28)  &
    / 'thujone_a       ', 28             /
DATA  mech_nam_saprc99( 28)     , mech_map_saprc99( 28)  &
    / 'ALK5            ', 22             /
DATA  conv_fac_saprc99( 28) / 1.0            /
DATA  spmh_nam_saprc99( 29)     , spmh_map_saprc99( 29)  &
    / 'thujone_b       ', 29             /
DATA  mech_nam_saprc99( 29)     , mech_map_saprc99( 29)  &
    / 'ALK5            ', 22             /
DATA  conv_fac_saprc99( 29) / 1.0            /
DATA  spmh_nam_saprc99( 30)     , spmh_map_saprc99( 30)  &
    / 'cineole_1_8     ', 30             /
DATA  mech_nam_saprc99( 30)     , mech_map_saprc99( 30)  &
    / 'ALK5            ', 22             /
DATA  conv_fac_saprc99( 30) / 1.0            /
DATA  spmh_nam_saprc99( 31)     , spmh_map_saprc99( 31)  &
    / 'borneol         ', 31             /
DATA  mech_nam_saprc99( 31)     , mech_map_saprc99( 31)  &
    / 'ALK5            ', 22             /
DATA  conv_fac_saprc99( 31) / 1.0            /
DATA  spmh_nam_saprc99( 32)     , spmh_map_saprc99( 32)  &
    / 'linalool        ', 32             /
DATA  mech_nam_saprc99( 32)     , mech_map_saprc99( 32)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 32) / 1.0            /
DATA  spmh_nam_saprc99( 33)     , spmh_map_saprc99( 33)  &
    / 'terpineol_4     ', 33             /
DATA  mech_nam_saprc99( 33)     , mech_map_saprc99( 33)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 33) / 1.0            /
DATA  spmh_nam_saprc99( 34)     , spmh_map_saprc99( 34)  &
    / 'terpineol_a     ', 34             /
DATA  mech_nam_saprc99( 34)     , mech_map_saprc99( 34)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 34) / 1.0            /
DATA  spmh_nam_saprc99( 35)     , spmh_map_saprc99( 35)  &
    / 'linalool_OXD_c  ', 35             /
DATA  mech_nam_saprc99( 35)     , mech_map_saprc99( 35)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 35) / 1.0            /
DATA  spmh_nam_saprc99( 36)     , spmh_map_saprc99( 36)  &
    / 'linalool_OXD_t  ', 36             /
DATA  mech_nam_saprc99( 36)     , mech_map_saprc99( 36)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 36) / 1.0            /
DATA  spmh_nam_saprc99( 37)     , spmh_map_saprc99( 37)  &
    / 'ionone_b        ', 37             /
DATA  mech_nam_saprc99( 37)     , mech_map_saprc99( 37)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 37) / 1.0            /
DATA  spmh_nam_saprc99( 38)     , spmh_map_saprc99( 38)  &
    / 'bornyl_ACT      ', 38             /
DATA  mech_nam_saprc99( 38)     , mech_map_saprc99( 38)  &
    / 'ALK5            ', 22             /
DATA  conv_fac_saprc99( 38) / 1.0            /
DATA  spmh_nam_saprc99( 39)     , spmh_map_saprc99( 39)  &
    / 'farnescene_a    ', 39             /
DATA  mech_nam_saprc99( 39)     , mech_map_saprc99( 39)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 39) / 1.0            /
DATA  spmh_nam_saprc99( 40)     , spmh_map_saprc99( 40)  &
    / 'caryophyllene_b ', 40             /
DATA  mech_nam_saprc99( 40)     , mech_map_saprc99( 40)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 40) / 1.0            /
DATA  spmh_nam_saprc99( 41)     , spmh_map_saprc99( 41)  &
    / 'acoradiene      ', 41             /
DATA  mech_nam_saprc99( 41)     , mech_map_saprc99( 41)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 41) / 1.0            /
DATA  spmh_nam_saprc99( 42)     , spmh_map_saprc99( 42)  &
    / 'aromadendrene   ', 42             /
DATA  mech_nam_saprc99( 42)     , mech_map_saprc99( 42)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 42) / 1.0            /
DATA  spmh_nam_saprc99( 43)     , spmh_map_saprc99( 43)  &
    / 'bergamotene_a   ', 43             /
DATA  mech_nam_saprc99( 43)     , mech_map_saprc99( 43)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 43) / 1.0            /
DATA  spmh_nam_saprc99( 44)     , spmh_map_saprc99( 44)  &
    / 'bergamotene_b   ', 44             /
DATA  mech_nam_saprc99( 44)     , mech_map_saprc99( 44)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 44) / 1.0            /
DATA  spmh_nam_saprc99( 45)     , spmh_map_saprc99( 45)  &
    / 'bisabolene_a    ', 45             /
DATA  mech_nam_saprc99( 45)     , mech_map_saprc99( 45)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 45) / 1.0            /
DATA  spmh_nam_saprc99( 46)     , spmh_map_saprc99( 46)  &
    / 'bisabolene_b    ', 46             /
DATA  mech_nam_saprc99( 46)     , mech_map_saprc99( 46)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 46) / 1.0            /
DATA  spmh_nam_saprc99( 47)     , spmh_map_saprc99( 47)  &
    / 'bourbonene_b    ', 47             /
DATA  mech_nam_saprc99( 47)     , mech_map_saprc99( 47)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 47) / 1.0            /
DATA  spmh_nam_saprc99( 48)     , spmh_map_saprc99( 48)  &
    / 'cadinene_d      ', 48             /
DATA  mech_nam_saprc99( 48)     , mech_map_saprc99( 48)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 48) / 1.0            /
DATA  spmh_nam_saprc99( 49)     , spmh_map_saprc99( 49)  &
    / 'cadinene_g      ', 49             /
DATA  mech_nam_saprc99( 49)     , mech_map_saprc99( 49)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 49) / 1.0            /
DATA  spmh_nam_saprc99( 50)     , spmh_map_saprc99( 50)  &
    / 'cedrene_a       ', 50             /
DATA  mech_nam_saprc99( 50)     , mech_map_saprc99( 50)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 50) / 1.0            /
DATA  spmh_nam_saprc99( 51)     , spmh_map_saprc99( 51)  &
    / 'copaene_a       ', 51             /
DATA  mech_nam_saprc99( 51)     , mech_map_saprc99( 51)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 51) / 1.0            /
DATA  spmh_nam_saprc99( 52)     , spmh_map_saprc99( 52)  &
    / 'cubebene_a      ', 52             /
DATA  mech_nam_saprc99( 52)     , mech_map_saprc99( 52)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 52) / 1.0            /
DATA  spmh_nam_saprc99( 53)     , spmh_map_saprc99( 53)  &
    / 'cubebene_b      ', 53             /
DATA  mech_nam_saprc99( 53)     , mech_map_saprc99( 53)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 53) / 1.0            /
DATA  spmh_nam_saprc99( 54)     , spmh_map_saprc99( 54)  &
    / 'elemene_b       ', 54             /
DATA  mech_nam_saprc99( 54)     , mech_map_saprc99( 54)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 54) / 1.0            /
DATA  spmh_nam_saprc99( 55)     , spmh_map_saprc99( 55)  &
    / 'farnescene_b    ', 55             /
DATA  mech_nam_saprc99( 55)     , mech_map_saprc99( 55)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 55) / 1.0            /
DATA  spmh_nam_saprc99( 56)     , spmh_map_saprc99( 56)  &
    / 'germacrene_B    ', 56             /
DATA  mech_nam_saprc99( 56)     , mech_map_saprc99( 56)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 56) / 1.0            /
DATA  spmh_nam_saprc99( 57)     , spmh_map_saprc99( 57)  &
    / 'germacrene_D    ', 57             /
DATA  mech_nam_saprc99( 57)     , mech_map_saprc99( 57)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 57) / 1.0            /
DATA  spmh_nam_saprc99( 58)     , spmh_map_saprc99( 58)  &
    / 'gurjunene_b     ', 58             /
DATA  mech_nam_saprc99( 58)     , mech_map_saprc99( 58)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 58) / 1.0            /
DATA  spmh_nam_saprc99( 59)     , spmh_map_saprc99( 59)  &
    / 'humulene_a      ', 59             /
DATA  mech_nam_saprc99( 59)     , mech_map_saprc99( 59)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 59) / 1.0            /
DATA  spmh_nam_saprc99( 60)     , spmh_map_saprc99( 60)  &
    / 'humulene_g      ', 60             /
DATA  mech_nam_saprc99( 60)     , mech_map_saprc99( 60)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 60) / 1.0            /
DATA  spmh_nam_saprc99( 61)     , spmh_map_saprc99( 61)  &
    / 'isolongifolene  ', 61             /
DATA  mech_nam_saprc99( 61)     , mech_map_saprc99( 61)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 61) / 1.0            /
DATA  spmh_nam_saprc99( 62)     , spmh_map_saprc99( 62)  &
    / 'longifolene     ', 62             /
DATA  mech_nam_saprc99( 62)     , mech_map_saprc99( 62)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 62) / 1.0            /
DATA  spmh_nam_saprc99( 63)     , spmh_map_saprc99( 63)  &
    / 'longipinene     ', 63             /
DATA  mech_nam_saprc99( 63)     , mech_map_saprc99( 63)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 63) / 1.0            /
DATA  spmh_nam_saprc99( 64)     , spmh_map_saprc99( 64)  &
    / 'muurolene_a     ', 64             /
DATA  mech_nam_saprc99( 64)     , mech_map_saprc99( 64)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 64) / 1.0            /
DATA  spmh_nam_saprc99( 65)     , spmh_map_saprc99( 65)  &
    / 'muurolene_g     ', 65             /
DATA  mech_nam_saprc99( 65)     , mech_map_saprc99( 65)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 65) / 1.0            /
DATA  spmh_nam_saprc99( 66)     , spmh_map_saprc99( 66)  &
    / 'selinene_b      ', 66             /
DATA  mech_nam_saprc99( 66)     , mech_map_saprc99( 66)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 66) / 1.0            /
DATA  spmh_nam_saprc99( 67)     , spmh_map_saprc99( 67)  &
    / 'selinene_d      ', 67             /
DATA  mech_nam_saprc99( 67)     , mech_map_saprc99( 67)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 67) / 1.0            /
DATA  spmh_nam_saprc99( 68)     , spmh_map_saprc99( 68)  &
    / 'nerolidol_c     ', 68             /
DATA  mech_nam_saprc99( 68)     , mech_map_saprc99( 68)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 68) / 1.0            /
DATA  spmh_nam_saprc99( 69)     , spmh_map_saprc99( 69)  &
    / 'nerolidol_t     ', 69             /
DATA  mech_nam_saprc99( 69)     , mech_map_saprc99( 69)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 69) / 1.0            /
DATA  spmh_nam_saprc99( 70)     , spmh_map_saprc99( 70)  &
    / 'cedrol          ', 70             /
DATA  mech_nam_saprc99( 70)     , mech_map_saprc99( 70)  &
    / 'ALK5            ', 22             /
DATA  conv_fac_saprc99( 70) / 1.0            /
DATA  spmh_nam_saprc99( 71)     , spmh_map_saprc99( 71)  &
    / 'MBO_2m3e2ol     ', 71             /
DATA  mech_nam_saprc99( 71)     , mech_map_saprc99( 71)  &
    / 'ISOPRENE        ', 1              /
DATA  conv_fac_saprc99( 71) / 1.0            /
DATA  spmh_nam_saprc99( 72)     , spmh_map_saprc99( 72)  &
    / 'methanol        ', 72             /
DATA  mech_nam_saprc99( 72)     , mech_map_saprc99( 72)  &
    / 'MEOH            ', 3              /
DATA  conv_fac_saprc99( 72) / 1.0            /
DATA  spmh_nam_saprc99( 73)     , spmh_map_saprc99( 73)  &
    / 'acetone         ', 73             /
DATA  mech_nam_saprc99( 73)     , mech_map_saprc99( 73)  &
    / 'ACET            ', 4              /
DATA  conv_fac_saprc99( 73) / 1.0            /
DATA  spmh_nam_saprc99( 74)     , spmh_map_saprc99( 74)  &
    / 'methane         ', 74             /
DATA  mech_nam_saprc99( 74)     , mech_map_saprc99( 74)  &
    / 'CH4             ', 5              /
DATA  conv_fac_saprc99( 74) / 1.0            /
DATA  spmh_nam_saprc99( 75)     , spmh_map_saprc99( 75)  &
    / 'ammonia         ', 75             /
DATA  mech_nam_saprc99( 75)     , mech_map_saprc99( 75)  &
    / 'NH3             ', 8              /
DATA  conv_fac_saprc99( 75) / 1.0            /
DATA  spmh_nam_saprc99( 76)     , spmh_map_saprc99( 76)  &
    / 'nitrous_OXD     ', 76             /
DATA  mech_nam_saprc99( 76)     , mech_map_saprc99( 76)  &
    / 'NONR            ', 28             /
DATA  conv_fac_saprc99( 76) / 1.0            /
DATA  spmh_nam_saprc99( 77)     , spmh_map_saprc99( 77)  &
    / 'nitric_OXD      ', 77             /
DATA  mech_nam_saprc99( 77)     , mech_map_saprc99( 77)  &
    / 'NO              ', 6              /
DATA  conv_fac_saprc99( 77) / 1.0            /
DATA  spmh_nam_saprc99( 78)     , spmh_map_saprc99( 78)  &
    / 'acetaldehyde    ', 78             /
DATA  mech_nam_saprc99( 78)     , mech_map_saprc99( 78)  &
    / 'CCHO            ', 9              /
DATA  conv_fac_saprc99( 78) / 1.0            /
DATA  spmh_nam_saprc99( 79)     , spmh_map_saprc99( 79)  &
    / 'ethanol         ', 79             /
DATA  mech_nam_saprc99( 79)     , mech_map_saprc99( 79)  &
    / 'ALK3            ', 20             /
DATA  conv_fac_saprc99( 79) / 1.0            /
DATA  spmh_nam_saprc99( 80)     , spmh_map_saprc99( 80)  &
    / 'formic_acid     ', 80             /
DATA  mech_nam_saprc99( 80)     , mech_map_saprc99( 80)  &
    / 'HCOOH           ', 10             /
DATA  conv_fac_saprc99( 80) / 1.0            /
DATA  spmh_nam_saprc99( 81)     , spmh_map_saprc99( 81)  &
    / 'formaldehyde    ', 81             /
DATA  mech_nam_saprc99( 81)     , mech_map_saprc99( 81)  &
    / 'HCHO            ', 11             /
DATA  conv_fac_saprc99( 81) / 1.0            /
DATA  spmh_nam_saprc99( 82)     , spmh_map_saprc99( 82)  &
    / 'acetic_acid     ', 82             /
DATA  mech_nam_saprc99( 82)     , mech_map_saprc99( 82)  &
    / 'CCO_OH          ', 12             /
DATA  conv_fac_saprc99( 82) / 1.0            /
DATA  spmh_nam_saprc99( 83)     , spmh_map_saprc99( 83)  &
    / 'MBO_3m2e1ol     ', 83             /
DATA  mech_nam_saprc99( 83)     , mech_map_saprc99( 83)  &
    / 'ISOPRENE        ', 1              /
DATA  conv_fac_saprc99( 83) / 1.0            /
DATA  spmh_nam_saprc99( 84)     , spmh_map_saprc99( 84)  &
    / 'MBO_3m3e1ol     ', 84             /
DATA  mech_nam_saprc99( 84)     , mech_map_saprc99( 84)  &
    / 'ISOPRENE        ', 1              /
DATA  conv_fac_saprc99( 84) / 1.0            /
DATA  spmh_nam_saprc99( 85)     , spmh_map_saprc99( 85)  &
    / 'benzaldehyde    ', 85             /
DATA  mech_nam_saprc99( 85)     , mech_map_saprc99( 85)  &
    / 'BALD            ', 13             /
DATA  conv_fac_saprc99( 85) / 1.0            /
DATA  spmh_nam_saprc99( 86)     , spmh_map_saprc99( 86)  &
    / 'butanone_2      ', 86             /
DATA  mech_nam_saprc99( 86)     , mech_map_saprc99( 86)  &
    / 'MEK             ', 14             /
DATA  conv_fac_saprc99( 86) / 1.0            /
DATA  spmh_nam_saprc99( 87)     , spmh_map_saprc99( 87)  &
    / 'decanal         ', 87             /
DATA  mech_nam_saprc99( 87)     , mech_map_saprc99( 87)  &
    / 'RCHO            ', 27             /
DATA  conv_fac_saprc99( 87) / 1.0            /
DATA  spmh_nam_saprc99( 88)     , spmh_map_saprc99( 88)  &
    / 'dodecene_1      ', 88             /
DATA  mech_nam_saprc99( 88)     , mech_map_saprc99( 88)  &
    / 'OLE1            ', 25             /
DATA  conv_fac_saprc99( 88) / 1.0            /
DATA  spmh_nam_saprc99( 89)     , spmh_map_saprc99( 89)  &
    / 'geranyl_acetone ', 89             /
DATA  mech_nam_saprc99( 89)     , mech_map_saprc99( 89)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99( 89) / 1.0            /
DATA  spmh_nam_saprc99( 90)     , spmh_map_saprc99( 90)  &
    / 'heptanal        ', 90             /
DATA  mech_nam_saprc99( 90)     , mech_map_saprc99( 90)  &
    / 'RCHO            ', 27             /
DATA  conv_fac_saprc99( 90) / 1.0            /
DATA  spmh_nam_saprc99( 91)     , spmh_map_saprc99( 91)  &
    / 'heptane         ', 91             /
DATA  mech_nam_saprc99( 91)     , mech_map_saprc99( 91)  &
    / 'ALK5            ', 22             /
DATA  conv_fac_saprc99( 91) / 1.0            /
DATA  spmh_nam_saprc99( 92)     , spmh_map_saprc99( 92)  &
    / 'hexane          ', 92             /
DATA  mech_nam_saprc99( 92)     , mech_map_saprc99( 92)  &
    / 'ALK4            ', 21             /
DATA  conv_fac_saprc99( 92) / 1.0            /
DATA  spmh_nam_saprc99( 93)     , spmh_map_saprc99( 93)  &
    / 'met_benzoate    ', 93             /
DATA  mech_nam_saprc99( 93)     , mech_map_saprc99( 93)  &
    / 'ARO1            ', 23             /
DATA  conv_fac_saprc99( 93) / 1.0            /
DATA  spmh_nam_saprc99( 94)     , spmh_map_saprc99( 94)  &
    / 'met_heptenone   ', 94             /
DATA  mech_nam_saprc99( 94)     , mech_map_saprc99( 94)  &
    / 'OLE2            ', 26             /
DATA  conv_fac_saprc99( 94) / 1.0            /
DATA  spmh_nam_saprc99( 95)     , spmh_map_saprc99( 95)  &
    / 'neryl_acetone   ', 95             /
DATA  mech_nam_saprc99( 95)     , mech_map_saprc99( 95)  &
    / 'OLE2            ', 26             /
DATA  conv_fac_saprc99( 95) / 1.0            /
DATA  spmh_nam_saprc99( 96)     , spmh_map_saprc99( 96)  &
    / 'nonanal         ', 96             /
DATA  mech_nam_saprc99( 96)     , mech_map_saprc99( 96)  &
    / 'RCHO            ', 27             /
DATA  conv_fac_saprc99( 96) / 1.0            /
DATA  spmh_nam_saprc99( 97)     , spmh_map_saprc99( 97)  &
    / 'nonenal         ', 97             /
DATA  mech_nam_saprc99( 97)     , mech_map_saprc99( 97)  &
    / 'OLE1            ', 25             /
DATA  conv_fac_saprc99( 97) / 1.0            /
DATA  spmh_nam_saprc99( 98)     , spmh_map_saprc99( 98)  &
    / 'octanal         ', 98             /
DATA  mech_nam_saprc99( 98)     , mech_map_saprc99( 98)  &
    / 'RCHO            ', 27             /
DATA  conv_fac_saprc99( 98) / 1.0            /
DATA  spmh_nam_saprc99( 99)     , spmh_map_saprc99( 99)  &
    / 'octanol         ', 99             /
DATA  mech_nam_saprc99( 99)     , mech_map_saprc99( 99)  &
    / 'ALK5            ', 22             /
DATA  conv_fac_saprc99( 99) / 1.0            /
DATA  spmh_nam_saprc99(100)     , spmh_map_saprc99(100)  &
    / 'octenol_1e3ol   ', 100            /
DATA  mech_nam_saprc99(100)     , mech_map_saprc99(100)  &
    / 'OLE1            ', 25             /
DATA  conv_fac_saprc99(100) / 1.0            /
DATA  spmh_nam_saprc99(101)     , spmh_map_saprc99(101)  &
    / 'oxopentanal     ', 101            /
DATA  mech_nam_saprc99(101)     , mech_map_saprc99(101)  &
    / 'RCHO            ', 27             /
DATA  conv_fac_saprc99(101) / 1.0            /
DATA  spmh_nam_saprc99(102)     , spmh_map_saprc99(102)  &
    / 'pentane         ', 102            /
DATA  mech_nam_saprc99(102)     , mech_map_saprc99(102)  &
    / 'ALK4            ', 21             /
DATA  conv_fac_saprc99(102) / 1.0            /
DATA  spmh_nam_saprc99(103)     , spmh_map_saprc99(103)  &
    / 'phenyl_CCO      ', 103            /
DATA  mech_nam_saprc99(103)     , mech_map_saprc99(103)  &
    / 'ARO1            ', 23             /
DATA  conv_fac_saprc99(103) / 1.0            /
DATA  spmh_nam_saprc99(104)     , spmh_map_saprc99(104)  &
    / 'pyruvic_acid    ', 104            /
DATA  mech_nam_saprc99(104)     , mech_map_saprc99(104)  &
    / 'RCO_OH          ', 15             /
DATA  conv_fac_saprc99(104) / 1.0            /
DATA  spmh_nam_saprc99(105)     , spmh_map_saprc99(105)  &
    / 'terpinyl_ACT_a  ', 105            /
DATA  mech_nam_saprc99(105)     , mech_map_saprc99(105)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99(105) / 1.0            /
DATA  spmh_nam_saprc99(106)     , spmh_map_saprc99(106)  &
    / 'tetradecene_1   ', 106            /
DATA  mech_nam_saprc99(106)     , mech_map_saprc99(106)  &
    / 'OLE1            ', 25             /
DATA  conv_fac_saprc99(106) / 1.0            /
DATA  spmh_nam_saprc99(107)     , spmh_map_saprc99(107)  &
    / 'toluene         ', 107            /
DATA  mech_nam_saprc99(107)     , mech_map_saprc99(107)  &
    / 'ARO1            ', 23             /
DATA  conv_fac_saprc99(107) / 1.0            /
DATA  spmh_nam_saprc99(108)     , spmh_map_saprc99(108)  &
    / 'carbon_monoxide ', 108            /
DATA  mech_nam_saprc99(108)     , mech_map_saprc99(108)  &
    / 'CO              ', 16             /
DATA  conv_fac_saprc99(108) / 1.0            /
DATA  spmh_nam_saprc99(109)     , spmh_map_saprc99(109)  &
    / 'butene          ', 109            /
DATA  mech_nam_saprc99(109)     , mech_map_saprc99(109)  &
    / 'OLE1            ', 25             /
DATA  conv_fac_saprc99(109) / 1.0            /
DATA  spmh_nam_saprc99(110)     , spmh_map_saprc99(110)  &
    / 'ethane          ', 110            /
DATA  mech_nam_saprc99(110)     , mech_map_saprc99(110)  &
    / 'ALK1            ', 18             /
DATA  conv_fac_saprc99(110) / 1.0            /
DATA  spmh_nam_saprc99(111)     , spmh_map_saprc99(111)  &
    / 'ethene          ', 111            /
DATA  mech_nam_saprc99(111)     , mech_map_saprc99(111)  &
    / 'ETHENE          ', 17             /
DATA  conv_fac_saprc99(111) / 1.0            /
DATA  spmh_nam_saprc99(112)     , spmh_map_saprc99(112)  &
    / 'hydrogen_cyanide', 112            /
DATA  mech_nam_saprc99(112)     , mech_map_saprc99(112)  &
    / 'NONR            ', 28             /
DATA  conv_fac_saprc99(112) / 1.0            /
DATA  spmh_nam_saprc99(113)     , spmh_map_saprc99(113)  &
    / 'propane         ', 113            /
DATA  mech_nam_saprc99(113)     , mech_map_saprc99(113)  &
    / 'ALK2            ', 19             /
DATA  conv_fac_saprc99(113) / 1.0            /
DATA  spmh_nam_saprc99(114)     , spmh_map_saprc99(114)  &
    / 'propene         ', 114            /
DATA  mech_nam_saprc99(114)     , mech_map_saprc99(114)  &
    / 'OLE1            ', 25             /
DATA  conv_fac_saprc99(114) / 1.0            /
DATA  spmh_nam_saprc99(115)     , spmh_map_saprc99(115)  &
    / 'carbon_2s       ', 115            /
DATA  mech_nam_saprc99(115)     , mech_map_saprc99(115)  &
    / 'NONR            ', 28             /
DATA  conv_fac_saprc99(115) / 1.0            /
DATA  spmh_nam_saprc99(116)     , spmh_map_saprc99(116)  &
    / 'carbonyl_s      ', 116            /
DATA  mech_nam_saprc99(116)     , mech_map_saprc99(116)  &
    / 'NONR            ', 28             /
DATA  conv_fac_saprc99(116) / 1.0            /
DATA  spmh_nam_saprc99(117)     , spmh_map_saprc99(117)  &
    / 'diallyl_2s      ', 117            /
DATA  mech_nam_saprc99(117)     , mech_map_saprc99(117)  &
    / 'OLE1            ', 25             /
DATA  conv_fac_saprc99(117) / 1.0            /
DATA  spmh_nam_saprc99(118)     , spmh_map_saprc99(118)  &
    / '2met_2s         ', 118            /
DATA  mech_nam_saprc99(118)     , mech_map_saprc99(118)  &
    / 'ALK5            ', 22             /
DATA  conv_fac_saprc99(118) / 1.0            /
DATA  spmh_nam_saprc99(119)     , spmh_map_saprc99(119)  &
    / '2met_s          ', 119            /
DATA  mech_nam_saprc99(119)     , mech_map_saprc99(119)  &
    / 'ALK4            ', 21             /
DATA  conv_fac_saprc99(119) / 1.0            /
DATA  spmh_nam_saprc99(120)     , spmh_map_saprc99(120)  &
    / 'met_chloride    ', 120            /
DATA  mech_nam_saprc99(120)     , mech_map_saprc99(120)  &
    / 'NONR            ', 28             /
DATA  conv_fac_saprc99(120) / 1.0            /
DATA  spmh_nam_saprc99(121)     , spmh_map_saprc99(121)  &
    / 'met_bromide     ', 121            /
DATA  mech_nam_saprc99(121)     , mech_map_saprc99(121)  &
    / 'NONR            ', 28             /
DATA  conv_fac_saprc99(121) / 1.0            /
DATA  spmh_nam_saprc99(122)     , spmh_map_saprc99(122)  &
    / 'met_iodide      ', 122            /
DATA  mech_nam_saprc99(122)     , mech_map_saprc99(122)  &
    / 'NONR            ', 28             /
DATA  conv_fac_saprc99(122) / 1.0            /
DATA  spmh_nam_saprc99(123)     , spmh_map_saprc99(123)  &
    / 'hydrogen_s      ', 123            /
DATA  mech_nam_saprc99(123)     , mech_map_saprc99(123)  &
    / 'NONR            ', 28             /
DATA  conv_fac_saprc99(123) / 1.0            /
DATA  spmh_nam_saprc99(124)     , spmh_map_saprc99(124)  &
    / 'met_mercaptan   ', 124            /
DATA  mech_nam_saprc99(124)     , mech_map_saprc99(124)  &
    / 'ALK5            ', 22             /
DATA  conv_fac_saprc99(124) / 1.0            /
DATA  spmh_nam_saprc99(125)     , spmh_map_saprc99(125)  &
    / 'met_propenyl_2s ', 125            /
DATA  mech_nam_saprc99(125)     , mech_map_saprc99(125)  &
    / 'OLE1            ', 25             /
DATA  conv_fac_saprc99(125) / 1.0            /
DATA  spmh_nam_saprc99(126)     , spmh_map_saprc99(126)  &
    / 'PPPP_2s         ', 126            /
DATA  mech_nam_saprc99(126)     , mech_map_saprc99(126)  &
    / 'OLE1            ', 25             /
DATA  conv_fac_saprc99(126) / 1.0            /
DATA  spmh_nam_saprc99(127)     , spmh_map_saprc99(127)  &
    / '2met_nonatriene ', 127            /
DATA  mech_nam_saprc99(127)     , mech_map_saprc99(127)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99(127) / 1.0            /
DATA  spmh_nam_saprc99(128)     , spmh_map_saprc99(128)  &
    / 'met_salicylate  ', 128            /
DATA  mech_nam_saprc99(128)     , mech_map_saprc99(128)  &
    / 'ARO1            ', 23             /
DATA  conv_fac_saprc99(128) / 1.0            /
DATA  spmh_nam_saprc99(129)     , spmh_map_saprc99(129)  &
    / 'indole          ', 129            /
DATA  mech_nam_saprc99(129)     , mech_map_saprc99(129)  &
    / 'ARO2            ', 24             /
DATA  conv_fac_saprc99(129) / 1.0            /
DATA  spmh_nam_saprc99(130)     , spmh_map_saprc99(130)  &
    / 'jasmone         ', 130            /
DATA  mech_nam_saprc99(130)     , mech_map_saprc99(130)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99(130) / 1.0            /
DATA  spmh_nam_saprc99(131)     , spmh_map_saprc99(131)  &
    / 'met_jasmonate   ', 131            /
DATA  mech_nam_saprc99(131)     , mech_map_saprc99(131)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99(131) / 1.0            /
DATA  spmh_nam_saprc99(132)     , spmh_map_saprc99(132)  &
    / '3met_3DCTT      ', 132            /
DATA  mech_nam_saprc99(132)     , mech_map_saprc99(132)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99(132) / 1.0            /
DATA  spmh_nam_saprc99(133)     , spmh_map_saprc99(133)  &
    / 'hexanal         ', 133            /
DATA  mech_nam_saprc99(133)     , mech_map_saprc99(133)  &
    / 'RCHO            ', 27             /
DATA  conv_fac_saprc99(133) / 1.0            /
DATA  spmh_nam_saprc99(134)     , spmh_map_saprc99(134)  &
    / 'hexanol_1       ', 134            /
DATA  mech_nam_saprc99(134)     , mech_map_saprc99(134)  &
    / 'ALK5            ', 22             /
DATA  conv_fac_saprc99(134) / 1.0            /
DATA  spmh_nam_saprc99(135)     , spmh_map_saprc99(135)  &
    / 'hexenal_c3      ', 135            /
DATA  mech_nam_saprc99(135)     , mech_map_saprc99(135)  &
    / 'OLE2            ', 26             /
DATA  conv_fac_saprc99(135) / 1.0            /
DATA  spmh_nam_saprc99(136)     , spmh_map_saprc99(136)  &
    / 'hexenal_t2      ', 136            /
DATA  mech_nam_saprc99(136)     , mech_map_saprc99(136)  &
    / 'OLE2            ', 26             /
DATA  conv_fac_saprc99(136) / 1.0            /
DATA  spmh_nam_saprc99(137)     , spmh_map_saprc99(137)  &
    / 'hexenol_c3      ', 137            /
DATA  mech_nam_saprc99(137)     , mech_map_saprc99(137)  &
    / 'OLE2            ', 26             /
DATA  conv_fac_saprc99(137) / 1.0            /
DATA  spmh_nam_saprc99(138)     , spmh_map_saprc99(138)  &
    / 'hexenyl_ACT_c3  ', 138            /
DATA  mech_nam_saprc99(138)     , mech_map_saprc99(138)  &
    / 'OLE2            ', 26             /
DATA  conv_fac_saprc99(138) / 1.0            /
DATA  spmh_nam_saprc99(139)     , spmh_map_saprc99(139)  &
    / 'homosalate      ', 139            /
DATA  mech_nam_saprc99(139)     , mech_map_saprc99(139)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99(139) / 1.0            /
DATA  spmh_nam_saprc99(140)     , spmh_map_saprc99(140)  &
    / 'Ehsalate        ', 140            /
DATA  mech_nam_saprc99(140)     , mech_map_saprc99(140)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99(140) / 1.0            /
DATA  spmh_nam_saprc99(141)     , spmh_map_saprc99(141)  &
    / 'pentanal        ', 141            /
DATA  mech_nam_saprc99(141)     , mech_map_saprc99(141)  &
    / 'RCHO            ', 27             /
DATA  conv_fac_saprc99(141) / 1.0            /
DATA  spmh_nam_saprc99(142)     , spmh_map_saprc99(142)  &
    / 'heptanone       ', 142            /
DATA  mech_nam_saprc99(142)     , mech_map_saprc99(142)  &
    / 'OLE2            ', 26             /
DATA  conv_fac_saprc99(142) / 1.0            /
DATA  spmh_nam_saprc99(143)     , spmh_map_saprc99(143)  &
    / 'anisole         ',143             /
DATA  mech_nam_saprc99(143)     , mech_map_saprc99(143)  &
    / 'BALD            ', 13             /
DATA  conv_fac_saprc99(143) / 1.0            /
DATA  spmh_nam_saprc99(144)     , spmh_map_saprc99(144)  &
    / 'verbenene       ',144             /
DATA  mech_nam_saprc99(144)     , mech_map_saprc99(144)  &
    / 'ARO2            ', 24             /
DATA  conv_fac_saprc99(144) / 1.0            /
DATA  spmh_nam_saprc99(145)     , spmh_map_saprc99(145)  &
    / 'benzyl-acetate  ',145             /
DATA  mech_nam_saprc99(145)     , mech_map_saprc99(145)  &
    / 'ARO1            ', 23             /
DATA  conv_fac_saprc99(145) / 1.0            /
DATA  spmh_nam_saprc99(146)     , spmh_map_saprc99(146)  &
    / 'myrtenal        ',146             /
DATA  mech_nam_saprc99(146)     , mech_map_saprc99(146)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99(146) / 1.0            /
DATA  spmh_nam_saprc99(147)     , spmh_map_saprc99(147)  &
    / 'benzyl-alcohol  ',147             /
DATA  mech_nam_saprc99(147)     , mech_map_saprc99(147)  &
    / 'ARO1            ', 23             /
DATA  conv_fac_saprc99(147) / 1.0            /
DATA  spmh_nam_saprc99(148)     , spmh_map_saprc99(148)  &
    / 'meta-cymenene   ',148             /
DATA  mech_nam_saprc99(148)     , mech_map_saprc99(148)  &
    / 'ARO2            ', 24             /
DATA  conv_fac_saprc99(148) / 1.0            /
DATA  spmh_nam_saprc99(149)     , spmh_map_saprc99(149)  &
    / 'ipsenol         ',149             /
DATA  mech_nam_saprc99(149)     , mech_map_saprc99(149)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprc99(149) / 1.0            /
DATA  spmh_nam_saprc99(150)     , spmh_map_saprc99(150)  &
    / 'Napthalene      ', 150            /
DATA  mech_nam_saprc99(150)     , mech_map_saprc99(150)  &
    / 'ARO2            ', 24             /
DATA  conv_fac_saprc99(150) / 1.0            /

end module map_cv2saprc99