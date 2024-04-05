module map_cv2cb6
!=======================================================================
!  MAP_CB6_CV2CB6.EXT
!  This include file contains conversion table for 150 speciated species
!  to CB6 (CMAQ/CAMx) species


!  MEGAN v2.10
!  INPUT version x.x

!  History:
!  Who          When       What
!  ---------------------------------------------------------------------
!  Tan          07/18/11 - Created for MEGANv2.10
!=======================================================================

CHARACTER (LEN=16) :: map_cb6mech
PARAMETER     (map_cb6mech = 'CB6             ')

INTEGER :: n_cb6
PARAMETER     (n_cb6 = (210))        ! Number of map species

CHARACTER (LEN=16) :: spmh_nam_cb6( n_cb6 )   ! speciated species name
INTEGER :: spmh_map_cb6( n_cb6 )   ! speciated species name
! mapped to SPC_SPCAT.EXT
CHARACTER (LEN=16) :: mech_nam_cb6( n_cb6 )   ! mechanism species
INTEGER :: mech_map_cb6( n_cb6 )   ! mechanism species mapped
! to SPC_CB4Q.EXT
REAL :: conv_fac_cb6( n_cb6 )   ! conversion factor


DATA  spmh_nam_cb6(  1)     , spmh_map_cb6(  1)  &
    / 'isoprene        ', 1             /
DATA  mech_nam_cb6(  1)     , mech_map_cb6(  1)  &
    / 'ISOP            ', 1             /
DATA  conv_fac_cb6(  1) / 1.00          /

DATA  spmh_nam_cb6(  2)     , spmh_map_cb6(  2)  &
    / 'myrcene         ', 2             /
DATA  mech_nam_cb6(  2)     , mech_map_cb6(  2)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6(  2) / 1.00          /

DATA  spmh_nam_cb6(  3)     , spmh_map_cb6(  3)  &
    / 'sabinene        ', 3             /
DATA  mech_nam_cb6(  3)     , mech_map_cb6(  3)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6(  3) / 1.00          /

DATA  spmh_nam_cb6(  4)     , spmh_map_cb6(  4)  &
    / 'limonene        ', 4             /
DATA  mech_nam_cb6(  4)     , mech_map_cb6(  4)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6(  4) / 1.00          /

DATA  spmh_nam_cb6(  5)     , spmh_map_cb6(  5)  &
    / 'carene_3        ', 5             /
DATA  mech_nam_cb6(  5)     , mech_map_cb6(  5)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6(  5) / 1.00          /

DATA  spmh_nam_cb6(  6)     , spmh_map_cb6(  6)  &
    / 'ocimene_t_b     ', 6             /
DATA  mech_nam_cb6(  6)     , mech_map_cb6(  6)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6(  6) / 1.00          /

DATA  spmh_nam_cb6(  7)     , spmh_map_cb6(  7)  &
    / 'pinene_b        ', 7             /
DATA  mech_nam_cb6(  7)     , mech_map_cb6(  7)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6(  7) / 1.00          /

DATA  spmh_nam_cb6(  8)     , spmh_map_cb6(  8)  &
    / 'pinene_a        ', 8             /
DATA  mech_nam_cb6(  8)     , mech_map_cb6(  8)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6(  8) / 1.00          /

DATA  spmh_nam_cb6(  9)     , spmh_map_cb6(  9)  &
    / '2met_styrene    ', 9             /
DATA  mech_nam_cb6(  9)     , mech_map_cb6(  9)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6(  9) / 1.00          /

DATA  spmh_nam_cb6( 10)     , spmh_map_cb6( 10)  &
    / '2met_styrene    ', 9             /
DATA  mech_nam_cb6( 10)     , mech_map_cb6( 10)  &
    / 'XYL             ', 4             /
DATA  conv_fac_cb6( 10) / 1.00          /

DATA  spmh_nam_cb6( 11)     , spmh_map_cb6( 11)  &
    / '2met_styrene    ', 9             /
DATA  mech_nam_cb6( 11)     , mech_map_cb6( 11)  &
    / 'OLE             ', 5             /
DATA  conv_fac_cb6( 11) / 0.50          /

DATA  spmh_nam_cb6( 12)     , spmh_map_cb6( 12)  &
    / 'cymene_p        ', 10            /
DATA  mech_nam_cb6( 12)     , mech_map_cb6( 12)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6( 12) / 2.00          /

DATA  spmh_nam_cb6( 13)     , spmh_map_cb6( 13)  &
    / 'cymene_p        ', 10            /
DATA  mech_nam_cb6( 13)     , mech_map_cb6( 13)  &
    / 'XYL             ', 4             /
DATA  conv_fac_cb6( 13) / 1.00          /

DATA  spmh_nam_cb6( 14)     , spmh_map_cb6( 14)  &
    / 'cymene_o        ', 11            /
DATA  mech_nam_cb6( 14)     , mech_map_cb6( 14)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6( 14) / 2.00          /

DATA  spmh_nam_cb6( 15)     , spmh_map_cb6( 15)  &
    / 'cymene_o        ', 11            /
DATA  mech_nam_cb6( 15)     , mech_map_cb6( 15)  &
    / 'XYL             ', 4             /
DATA  conv_fac_cb6( 15) / 1.00          /

DATA  spmh_nam_cb6( 16)     , spmh_map_cb6( 16)  &
    / 'phellandrene_a  ', 12            /
DATA  mech_nam_cb6( 16)     , mech_map_cb6( 16)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6( 16) / 1.00          /

DATA  spmh_nam_cb6( 17)     , spmh_map_cb6( 17)  &
    / 'thujene_a       ', 13            /
DATA  mech_nam_cb6( 17)     , mech_map_cb6( 17)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6( 17) / 1.00          /

DATA  spmh_nam_cb6( 18)     , spmh_map_cb6( 18)  &
    / 'terpinene_a     ', 14            /
DATA  mech_nam_cb6( 18)     , mech_map_cb6( 18)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6( 18) / 1.00          /

DATA  spmh_nam_cb6( 19)     , spmh_map_cb6( 19)  &
    / 'terpinene_g     ', 15            /
DATA  mech_nam_cb6( 19)     , mech_map_cb6( 19)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6( 19) / 1.00          /

DATA  spmh_nam_cb6( 20)     , spmh_map_cb6( 20)  &
    / 'terpinolene     ', 16            /
DATA  mech_nam_cb6( 20)     , mech_map_cb6( 20)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6( 20) / 1.00          /

DATA  spmh_nam_cb6( 21)     , spmh_map_cb6( 21)  &
    / 'phellandrene_b  ', 17            /
DATA  mech_nam_cb6( 21)     , mech_map_cb6( 21)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6( 21) / 1.00          /

DATA  spmh_nam_cb6( 22)     , spmh_map_cb6( 22)  &
    / 'camphene        ', 18            /
DATA  mech_nam_cb6( 22)     , mech_map_cb6( 22)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6( 22) / 1.00          /

DATA  spmh_nam_cb6( 23)     , spmh_map_cb6( 23)  &
    / 'bornene         ', 19            /
DATA  mech_nam_cb6( 23)     , mech_map_cb6( 23)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6( 23) / 1.00          /

DATA  spmh_nam_cb6( 24)     , spmh_map_cb6( 24)  &
    / 'fenchene_a      ', 20            /
DATA  mech_nam_cb6( 24)     , mech_map_cb6( 24)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6( 24) / 1.00          /

DATA  spmh_nam_cb6( 25)     , spmh_map_cb6( 25)  &
    / 'ocimene_al      ', 21            /
DATA  mech_nam_cb6( 25)     , mech_map_cb6( 25)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6( 25) / 1.00          /

DATA  spmh_nam_cb6( 26)     , spmh_map_cb6( 26)  &
    / 'ocimene_c_b     ', 22            /
DATA  mech_nam_cb6( 26)     , mech_map_cb6( 26)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6( 26) / 1.00          /

DATA  spmh_nam_cb6( 27)     , spmh_map_cb6( 27)  &
    / 'tricyclene      ', 23            /
DATA  mech_nam_cb6( 27)     , mech_map_cb6( 27)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6( 27) / 9.00          /

DATA  spmh_nam_cb6( 28)     , spmh_map_cb6( 28)  &
    / 'tricyclene      ', 23            /
DATA  mech_nam_cb6( 28)     , mech_map_cb6( 28)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 28) / 1.00          /

DATA  spmh_nam_cb6( 29)     , spmh_map_cb6( 29)  &
    / 'estragole       ', 24            /
DATA  mech_nam_cb6( 29)     , mech_map_cb6( 29)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6( 29) / 1.00          /

DATA  spmh_nam_cb6( 30)     , spmh_map_cb6( 30)  &
    / 'camphor         ', 25            /
DATA  mech_nam_cb6( 30)     , mech_map_cb6( 30)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6( 30) / 8.00          /

DATA  spmh_nam_cb6( 31)     , spmh_map_cb6( 31)  &
    / 'camphor         ', 25            /
DATA  mech_nam_cb6( 31)     , mech_map_cb6( 31)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 31) / 2.00          /

DATA  spmh_nam_cb6( 32)     , spmh_map_cb6( 32)  &
    / 'fenchone        ', 26            /
DATA  mech_nam_cb6( 32)     , mech_map_cb6( 32)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6( 32) / 7.00          /

DATA  spmh_nam_cb6( 33)     , spmh_map_cb6( 33)  &
    / 'fenchone        ', 26            /
DATA  mech_nam_cb6( 33)     , mech_map_cb6( 33)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 33) / 2.00          /

DATA  spmh_nam_cb6( 34)     , spmh_map_cb6( 34)  &
    / 'piperitone      ', 27            /
DATA  mech_nam_cb6( 34)     , mech_map_cb6( 34)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6( 34) / 1.00          /

DATA  spmh_nam_cb6( 35)     , spmh_map_cb6( 35)  &
    / 'thujone_a       ', 28            /
DATA  mech_nam_cb6( 35)     , mech_map_cb6( 35)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6( 35) / 8.00          /

DATA  spmh_nam_cb6( 36)     , spmh_map_cb6( 36)  &
    / 'thujone_a       ', 28            /
DATA  mech_nam_cb6( 36)     , mech_map_cb6( 36)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 36) / 1.00          /

DATA  spmh_nam_cb6( 37)     , spmh_map_cb6( 37)  &
    / 'thujone_b       ', 29            /
DATA  mech_nam_cb6( 37)     , mech_map_cb6( 37)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6( 37) / 8.00          /

DATA  spmh_nam_cb6( 38)     , spmh_map_cb6( 38)  &
    / 'thujone_b       ', 29            /
DATA  mech_nam_cb6( 38)     , mech_map_cb6( 38)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 38) / 1.00          /

DATA  spmh_nam_cb6( 39)     , spmh_map_cb6( 39)  &
    / 'cineole_1_8     ', 30            /
DATA  mech_nam_cb6( 39)     , mech_map_cb6( 39)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6( 39) / 9.00          /

DATA  spmh_nam_cb6( 40)     , spmh_map_cb6( 40)  &
    / 'cineole_1_8     ', 30            /
DATA  mech_nam_cb6( 40)     , mech_map_cb6( 40)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 40) / 1.00          /

DATA  spmh_nam_cb6( 41)     , spmh_map_cb6( 41)  &
    / 'borneol         ', 31            /
DATA  mech_nam_cb6( 41)     , mech_map_cb6( 41)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6( 41) / 8.00          /

DATA  spmh_nam_cb6( 42)     , spmh_map_cb6( 42)  &
    / 'borneol         ', 31            /
DATA  mech_nam_cb6( 42)     , mech_map_cb6( 42)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 42) / 2.00          /

DATA  spmh_nam_cb6( 43)     , spmh_map_cb6( 43)  &
    / 'linalool        ', 32            /
DATA  mech_nam_cb6( 43)     , mech_map_cb6( 43)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6( 43) / 1.00          /

DATA  spmh_nam_cb6( 44)     , spmh_map_cb6( 44)  &
    / 'terpineol_4     ', 33            /
DATA  mech_nam_cb6( 44)     , mech_map_cb6( 44)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6( 44) / 1.00          /

DATA  spmh_nam_cb6( 45)     , spmh_map_cb6( 45)  &
    / 'terpineol_a     ', 34            /
DATA  mech_nam_cb6( 45)     , mech_map_cb6( 45)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6( 45) / 1.00          /

DATA  spmh_nam_cb6( 46)     , spmh_map_cb6( 46)  &
    / 'linalool_OXD_c  ', 35            /
DATA  mech_nam_cb6( 46)     , mech_map_cb6( 46)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6( 46) / 1.00          /

DATA  spmh_nam_cb6( 47)     , spmh_map_cb6( 47)  &
    / 'linalool_OXD_t  ', 36            /
DATA  mech_nam_cb6( 47)     , mech_map_cb6( 47)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6( 47) / 1.00          /

DATA  spmh_nam_cb6( 48)     , spmh_map_cb6( 48)  &
    / 'ionone_b        ', 37            /
DATA  mech_nam_cb6( 48)     , mech_map_cb6( 48)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6( 48) / 1.00          /

DATA  spmh_nam_cb6( 49)     , spmh_map_cb6( 49)  &
    / 'ionone_b        ', 37            /
DATA  mech_nam_cb6( 49)     , mech_map_cb6( 49)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 49) / 3.00          /

DATA  spmh_nam_cb6( 50)     , spmh_map_cb6( 50)  &
    / 'bornyl_ACT      ', 38            /
DATA  mech_nam_cb6( 50)     , mech_map_cb6( 50)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6( 50) / 6.00          /

DATA  spmh_nam_cb6( 51)     , spmh_map_cb6( 51)  &
    / 'bornyl_ACT      ', 38            /
DATA  mech_nam_cb6( 51)     , mech_map_cb6( 51)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 51) / 6.00          /

DATA  spmh_nam_cb6( 52)     , spmh_map_cb6( 52)  &
    / 'farnescene_a    ', 39            /
DATA  mech_nam_cb6( 52)     , mech_map_cb6( 52)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 52) / 15.00         /

DATA  spmh_nam_cb6( 53)     , spmh_map_cb6( 53)  &
    / 'caryophyllene_b ', 40            /
DATA  mech_nam_cb6( 53)     , mech_map_cb6( 53)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 53) / 15.00         /

DATA  spmh_nam_cb6( 54)     , spmh_map_cb6( 54)  &
    / 'acoradiene      ', 41            /
DATA  mech_nam_cb6( 54)     , mech_map_cb6( 54)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 54) / 15.00         /

DATA  spmh_nam_cb6( 55)     , spmh_map_cb6( 55)  &
    / 'aromadendrene   ', 42            /
DATA  mech_nam_cb6( 55)     , mech_map_cb6( 55)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 55) / 15.00         /

DATA  spmh_nam_cb6( 56)     , spmh_map_cb6( 56)  &
    / 'bergamotene_a   ', 43            /
DATA  mech_nam_cb6( 56)     , mech_map_cb6( 56)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 56) / 15.00         /

DATA  spmh_nam_cb6( 57)     , spmh_map_cb6( 57)  &
    / 'bergamotene_b   ', 44            /
DATA  mech_nam_cb6( 57)     , mech_map_cb6( 57)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 57) / 15.00         /

DATA  spmh_nam_cb6( 58)     , spmh_map_cb6( 58)  &
    / 'bisabolene_a    ', 45            /
DATA  mech_nam_cb6( 58)     , mech_map_cb6( 58)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 58) / 15.00         /

DATA  spmh_nam_cb6( 59)     , spmh_map_cb6( 59)  &
    / 'bisabolene_b    ', 46            /
DATA  mech_nam_cb6( 59)     , mech_map_cb6( 59)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 59) / 15.00         /

DATA  spmh_nam_cb6( 60)     , spmh_map_cb6( 60)  &
    / 'bourbonene_b    ', 47            /
DATA  mech_nam_cb6( 60)     , mech_map_cb6( 60)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 60) / 15.00         /

DATA  spmh_nam_cb6( 61)     , spmh_map_cb6( 61)  &
    / 'cadinene_d      ', 48            /
DATA  mech_nam_cb6( 61)     , mech_map_cb6( 61)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 61) / 15.00         /

DATA  spmh_nam_cb6( 62)     , spmh_map_cb6( 62)  &
    / 'cadinene_g      ', 49            /
DATA  mech_nam_cb6( 62)     , mech_map_cb6( 62)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 62) / 15.00         /

DATA  spmh_nam_cb6( 63)     , spmh_map_cb6( 63)  &
    / 'cedrene_a       ', 50            /
DATA  mech_nam_cb6( 63)     , mech_map_cb6( 63)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 63) / 15.00         /

DATA  spmh_nam_cb6( 64)     , spmh_map_cb6( 64)  &
    / 'copaene_a       ', 51            /
DATA  mech_nam_cb6( 64)     , mech_map_cb6( 64)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 64) / 15.00         /

DATA  spmh_nam_cb6( 65)     , spmh_map_cb6( 65)  &
    / 'cubebene_a      ', 52            /
DATA  mech_nam_cb6( 65)     , mech_map_cb6( 65)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 65) / 15.00         /

DATA  spmh_nam_cb6( 66)     , spmh_map_cb6( 66)  &
    / 'cubebene_b      ', 53            /
DATA  mech_nam_cb6( 66)     , mech_map_cb6( 66)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 66) / 15.00         /

DATA  spmh_nam_cb6( 67)     , spmh_map_cb6( 67)  &
    / 'elemene_b       ', 54            /
DATA  mech_nam_cb6( 67)     , mech_map_cb6( 67)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 67) / 15.00         /

DATA  spmh_nam_cb6( 68)     , spmh_map_cb6( 68)  &
    / 'farnescene_b    ', 55            /
DATA  mech_nam_cb6( 68)     , mech_map_cb6( 68)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 68) / 15.00         /

DATA  spmh_nam_cb6( 69)     , spmh_map_cb6( 69)  &
    / 'germacrene_B    ', 56            /
DATA  mech_nam_cb6( 69)     , mech_map_cb6( 69)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 69) / 15.00         /

DATA  spmh_nam_cb6( 70)     , spmh_map_cb6( 70)  &
    / 'germacrene_D    ', 57            /
DATA  mech_nam_cb6( 70)     , mech_map_cb6( 70)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 70) / 15.00         /

DATA  spmh_nam_cb6( 71)     , spmh_map_cb6( 71)  &
    / 'gurjunene_b     ', 58            /
DATA  mech_nam_cb6( 71)     , mech_map_cb6( 71)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 71) / 15.00         /

DATA  spmh_nam_cb6( 72)     , spmh_map_cb6( 72)  &
    / 'humulene_a      ', 59            /
DATA  mech_nam_cb6( 72)     , mech_map_cb6( 72)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 72) / 15.00         /

DATA  spmh_nam_cb6( 73)     , spmh_map_cb6( 73)  &
    / 'humulene_g      ', 60            /
DATA  mech_nam_cb6( 73)     , mech_map_cb6( 73)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 73) / 15.00         /

DATA  spmh_nam_cb6( 74)     , spmh_map_cb6( 74)  &
    / 'isolongifolene  ', 61            /
DATA  mech_nam_cb6( 74)     , mech_map_cb6( 74)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 74) / 15.00         /

DATA  spmh_nam_cb6( 75)     , spmh_map_cb6( 75)  &
    / 'longifolene     ', 62            /
DATA  mech_nam_cb6( 75)     , mech_map_cb6( 75)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 75) / 15.00         /

DATA  spmh_nam_cb6( 76)     , spmh_map_cb6( 76)  &
    / 'longipinene     ', 63            /
DATA  mech_nam_cb6( 76)     , mech_map_cb6( 76)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 76) / 15.00         /

DATA  spmh_nam_cb6( 77)     , spmh_map_cb6( 77)  &
    / 'muurolene_a     ', 64            /
DATA  mech_nam_cb6( 77)     , mech_map_cb6( 77)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 77) / 15.00         /

DATA  spmh_nam_cb6( 78)     , spmh_map_cb6( 78)  &
    / 'muurolene_g     ', 65            /
DATA  mech_nam_cb6( 78)     , mech_map_cb6( 78)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 78) / 15.00         /

DATA  spmh_nam_cb6( 79)     , spmh_map_cb6( 79)  &
    / 'selinene_b      ', 66            /
DATA  mech_nam_cb6( 79)     , mech_map_cb6( 79)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 79) / 15.00         /

DATA  spmh_nam_cb6( 80)     , spmh_map_cb6( 80)  &
    / 'selinene_d      ', 67            /
DATA  mech_nam_cb6( 80)     , mech_map_cb6( 80)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 80) / 15.00         /

DATA  spmh_nam_cb6( 81)     , spmh_map_cb6( 81)  &
    / 'nerolidol_c     ', 68            /
DATA  mech_nam_cb6( 81)     , mech_map_cb6( 81)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 81) / 15.00         /

DATA  spmh_nam_cb6( 82)     , spmh_map_cb6( 82)  &
    / 'nerolidol_t     ', 69            /
DATA  mech_nam_cb6( 82)     , mech_map_cb6( 82)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 82) / 15.00         /

DATA  spmh_nam_cb6( 83)     , spmh_map_cb6( 83)  &
    / 'cedrol          ', 70            /
DATA  mech_nam_cb6( 83)     , mech_map_cb6( 83)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6( 83) / 13.00         /

DATA  spmh_nam_cb6( 84)     , spmh_map_cb6( 84)  &
    / 'cedrol          ', 70            /
DATA  mech_nam_cb6( 84)     , mech_map_cb6( 84)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 84) / 2.00          /

DATA  spmh_nam_cb6( 85)     , spmh_map_cb6( 85)  &
    / 'MBO_2m3e2ol     ', 71            /
DATA  mech_nam_cb6( 85)     , mech_map_cb6( 85)  &
    / 'OLE             ', 5             /
DATA  conv_fac_cb6( 85) / 1.00          /

DATA  spmh_nam_cb6( 86)     , spmh_map_cb6( 86)  &
    / 'MBO_2m3e2ol     ', 71            /
DATA  mech_nam_cb6( 86)     , mech_map_cb6( 86)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6( 86) / 3.00          /

DATA  spmh_nam_cb6( 87)     , spmh_map_cb6( 87)  &
    / 'methanol        ', 72            /
DATA  mech_nam_cb6( 87)     , mech_map_cb6( 87)  &
    / 'MEOH            ', 7             /
DATA  conv_fac_cb6( 87) / 1.00          /

DATA  spmh_nam_cb6( 88)     , spmh_map_cb6( 88)  &
    / 'acetone         ', 73            /
DATA  mech_nam_cb6( 88)     , mech_map_cb6( 88)  &
    / 'ACET            ', 23            /
DATA  conv_fac_cb6( 88) / 1.00          /

DATA  spmh_nam_cb6( 89)     , spmh_map_cb6( 89)  &
    / 'methane         ', 74            /
DATA  mech_nam_cb6( 89)     , mech_map_cb6( 89)  &
    / 'CH4             ', 8             /
DATA  conv_fac_cb6( 89) / 1.00          /

DATA  spmh_nam_cb6( 90)     , spmh_map_cb6( 90)  &
    / 'ammonia         ', 75            /
DATA  mech_nam_cb6( 90)     , mech_map_cb6( 90)  &
    / 'NH3             ', 9             /
DATA  conv_fac_cb6( 90) / 1.00          /

DATA  spmh_nam_cb6( 91)     , spmh_map_cb6( 91)  &
    / 'nitric_OXD      ', 77            /
DATA  mech_nam_cb6( 91)     , mech_map_cb6( 91)  &
    / 'NO              ', 10            /
DATA  conv_fac_cb6( 91) / 1.00          /

DATA  spmh_nam_cb6( 92)     , spmh_map_cb6( 92)  &
    / 'acetaldehyde    ', 78            /
DATA  mech_nam_cb6( 92)     , mech_map_cb6( 92)  &
    / 'ALD2            ', 11            /
DATA  conv_fac_cb6( 92) / 1.00          /

DATA  spmh_nam_cb6( 93)     , spmh_map_cb6( 93)  &
    / 'ethanol         ', 79            /
DATA  mech_nam_cb6( 93)     , mech_map_cb6( 93)  &
    / 'ETOH            ', 12            /
DATA  conv_fac_cb6( 93) / 1.00          /

DATA  spmh_nam_cb6( 94)     , spmh_map_cb6( 94)  &
    / 'formic_acid     ', 80            /
DATA  mech_nam_cb6( 94)     , mech_map_cb6( 94)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 94) / 1.00          /

DATA  spmh_nam_cb6( 95)     , spmh_map_cb6( 95)  &
    / 'formaldehyde    ', 81            /
DATA  mech_nam_cb6( 95)     , mech_map_cb6( 95)  &
    / 'FORM            ', 13            /
DATA  conv_fac_cb6( 95) / 1.00          /

DATA  spmh_nam_cb6( 96)     , spmh_map_cb6( 96)  &
    / 'acetic_acid     ', 82            /
DATA  mech_nam_cb6( 96)     , mech_map_cb6( 96)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6( 96) / 1.00          /

DATA  spmh_nam_cb6( 97)     , spmh_map_cb6( 97)  &
    / 'acetic_acid     ', 82            /
DATA  mech_nam_cb6( 97)     , mech_map_cb6( 97)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6( 97) / 1.00          /

DATA  spmh_nam_cb6( 98)     , spmh_map_cb6( 98)  &
    / 'MBO_3m2e1ol     ', 83            /
DATA  mech_nam_cb6( 98)     , mech_map_cb6( 98)  &
    / 'ALDX            ', 14            /
DATA  conv_fac_cb6( 98) / 1.00          /

DATA  spmh_nam_cb6( 99)     , spmh_map_cb6( 99)  &
    / 'MBO_3m2e1ol     ', 83            /
DATA  mech_nam_cb6( 99)     , mech_map_cb6( 99)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6( 99) / 3.00          /

DATA  spmh_nam_cb6(100)     , spmh_map_cb6(100)  &
    / 'MBO_3m3e1ol     ', 84            /
DATA  mech_nam_cb6(100)     , mech_map_cb6(100)  &
    / 'FORM            ', 13            /
DATA  conv_fac_cb6(100) / 1.00          /

DATA  spmh_nam_cb6(101)     , spmh_map_cb6(101)  &
    / 'MBO_3m3e1ol     ', 84            /
DATA  mech_nam_cb6(101)     , mech_map_cb6(101)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6(101) / 4.00          /

DATA  spmh_nam_cb6(102)     , spmh_map_cb6(102)  &
    / 'benzaldehyde    ', 85            /
DATA  mech_nam_cb6(102)     , mech_map_cb6(102)  &
    / 'TOL             ', 15            /
DATA  conv_fac_cb6(102) / 1.00          /

DATA  spmh_nam_cb6(103)     , spmh_map_cb6(103)  &
    / 'butanone_2      ', 86            /
DATA  mech_nam_cb6(103)     , mech_map_cb6(103)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6(103) / 3.00          /

DATA  spmh_nam_cb6(104)     , spmh_map_cb6(104)  &
    / 'decanal         ', 87            /
DATA  mech_nam_cb6(104)     , mech_map_cb6(104)  &
    / 'ALDX            ', 14            /
DATA  conv_fac_cb6(104) / 1.00          /

DATA  spmh_nam_cb6(105)     , spmh_map_cb6(105)  &
    / 'decanal         ', 87            /
DATA  mech_nam_cb6(105)     , mech_map_cb6(105)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6(105) / 8.00          /

DATA  spmh_nam_cb6(106)     , spmh_map_cb6(106)  &
    / 'dodecene_1      ', 88            /
DATA  mech_nam_cb6(106)     , mech_map_cb6(106)  &
    / 'OLE             ', 5             /
DATA  conv_fac_cb6(106) / 1.00          /

DATA  spmh_nam_cb6(107)     , spmh_map_cb6(107)  &
    / 'dodecene_1      ', 88            /
DATA  mech_nam_cb6(107)     , mech_map_cb6(107)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6(107) / 10.00         /

DATA  spmh_nam_cb6(108)     , spmh_map_cb6(108)  &
    / 'geranyl_acetone ', 89            /
DATA  mech_nam_cb6(108)     , mech_map_cb6(108)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6(108) / 1.00          /

DATA  spmh_nam_cb6(109)     , spmh_map_cb6(109)  &
    / 'geranyl_acetone ', 89            /
DATA  mech_nam_cb6(109)     , mech_map_cb6(109)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6(109) / 3.00          /

DATA  spmh_nam_cb6(110)     , spmh_map_cb6(110)  &
    / 'heptanal        ', 90            /
DATA  mech_nam_cb6(110)     , mech_map_cb6(110)  &
    / 'ALDX            ', 14            /
DATA  conv_fac_cb6(110) / 1.00          /

DATA  spmh_nam_cb6(111)     , spmh_map_cb6(111)  &
    / 'heptanal        ', 90            /
DATA  mech_nam_cb6(111)     , mech_map_cb6(111)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6(111) / 5.00          /

DATA  spmh_nam_cb6(112)     , spmh_map_cb6(112)  &
    / 'heptane         ', 91            /
DATA  mech_nam_cb6(112)     , mech_map_cb6(112)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6(112) / 7.00          /

DATA  spmh_nam_cb6(113)     , spmh_map_cb6(113)  &
    / 'hexane          ', 92            /
DATA  mech_nam_cb6(113)     , mech_map_cb6(113)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6(113) / 6.00          /

DATA  spmh_nam_cb6(114)     , spmh_map_cb6(114)  &
    / 'met_benzoate    ', 93            /
DATA  mech_nam_cb6(114)     , mech_map_cb6(114)  &
    / 'TOL             ', 15            /
DATA  conv_fac_cb6(114) / 1.00          /

DATA  spmh_nam_cb6(115)     , spmh_map_cb6(115)  &
    / 'met_benzoate    ', 93            /
DATA  mech_nam_cb6(115)     , mech_map_cb6(115)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6(115) / 1.00          /

DATA  spmh_nam_cb6(116)     , spmh_map_cb6(116)  &
    / 'met_heptenone   ', 94            /
DATA  mech_nam_cb6(116)     , mech_map_cb6(116)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6(116) / 5.00          /

DATA  spmh_nam_cb6(117)     , spmh_map_cb6(117)  &
    / 'met_heptenone   ', 94            /
DATA  mech_nam_cb6(117)     , mech_map_cb6(117)  &
    / 'ALDX            ', 14            /
DATA  conv_fac_cb6(117) / 1.00          /

DATA  spmh_nam_cb6(118)     , spmh_map_cb6(118)  &
    / 'neryl_acetone   ', 95            /
DATA  mech_nam_cb6(118)     , mech_map_cb6(118)  &
    / 'IOLE            ', 16            /
DATA  conv_fac_cb6(118) / 1.00          /

DATA  spmh_nam_cb6(119)     , spmh_map_cb6(119)  &
    / 'neryl_acetone   ', 95            /
DATA  mech_nam_cb6(119)     , mech_map_cb6(119)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6(119) / 7.00          /

DATA  spmh_nam_cb6(120)     , spmh_map_cb6(120)  &
    / 'neryl_acetone   ', 95            /
DATA  mech_nam_cb6(120)     , mech_map_cb6(120)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6(120) / 1.00          /

DATA  spmh_nam_cb6(121)     , spmh_map_cb6(121)  &
    / 'nonanal         ', 96            /
DATA  mech_nam_cb6(121)     , mech_map_cb6(121)  &
    / 'ALDX            ', 14            /
DATA  conv_fac_cb6(121) / 1.00          /

DATA  spmh_nam_cb6(122)     , spmh_map_cb6(122)  &
    / 'nonanal         ', 96            /
DATA  mech_nam_cb6(122)     , mech_map_cb6(122)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6(122) / 7.00          /

DATA  spmh_nam_cb6(123)     , spmh_map_cb6(123)  &
    / 'nonenal         ', 97            /
DATA  mech_nam_cb6(123)     , mech_map_cb6(123)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6(123) / 3.00          /

DATA  spmh_nam_cb6(124)     , spmh_map_cb6(124)  &
    / 'nonenal         ', 97            /
DATA  mech_nam_cb6(124)     , mech_map_cb6(124)  &
    / 'IOLE            ', 16            /
DATA  conv_fac_cb6(124) / 1.00          /

DATA  spmh_nam_cb6(125)     , spmh_map_cb6(125)  &
    / 'nonenal         ', 97            /
DATA  mech_nam_cb6(125)     , mech_map_cb6(125)  &
    / 'ALDX            ', 14            /
DATA  conv_fac_cb6(125) / 1.00          /

DATA  spmh_nam_cb6(126)     , spmh_map_cb6(126)  &
    / 'octanal         ', 98            /
DATA  mech_nam_cb6(126)     , mech_map_cb6(126)  &
    / 'ALDX            ', 14            /
DATA  conv_fac_cb6(126) / 1.00          /

DATA  spmh_nam_cb6(127)     , spmh_map_cb6(127)  &
    / 'octanal         ', 98            /
DATA  mech_nam_cb6(127)     , mech_map_cb6(127)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6(127) / 6.00          /

DATA  spmh_nam_cb6(128)     , spmh_map_cb6(128)  &
    / 'octanol         ', 99            /
DATA  mech_nam_cb6(128)     , mech_map_cb6(128)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6(128) / 8.00          /

DATA  spmh_nam_cb6(129)     , spmh_map_cb6(129)  &
    / 'octenol_1e3ol   ', 100           /
DATA  mech_nam_cb6(129)     , mech_map_cb6(129)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6(129) / 6.00          /

DATA  spmh_nam_cb6(130)     , spmh_map_cb6(130)  &
    / 'octenol_1e3ol   ', 100           /
DATA  mech_nam_cb6(130)     , mech_map_cb6(130)  &
    / 'OLE             ', 5             /
DATA  conv_fac_cb6(130) / 1.00          /

DATA  spmh_nam_cb6(131)     , spmh_map_cb6(131)  &
    / 'oxopentanal     ', 101           /
DATA  mech_nam_cb6(131)     , mech_map_cb6(131)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6(131) / 3.00          /

DATA  spmh_nam_cb6(132)     , spmh_map_cb6(132)  &
    / 'oxopentanal     ', 101           /
DATA  mech_nam_cb6(132)     , mech_map_cb6(132)  &
    / 'ALDX            ', 14            /
DATA  conv_fac_cb6(132) / 1.00          /

DATA  spmh_nam_cb6(133)     , spmh_map_cb6(133)  &
    / 'pentane         ', 102           /
DATA  mech_nam_cb6(133)     , mech_map_cb6(133)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6(133) / 5.00          /

DATA  spmh_nam_cb6(134)     , spmh_map_cb6(134)  &
    / 'phenyl_CCO      ', 103           /
DATA  mech_nam_cb6(134)     , mech_map_cb6(134)  &
    / 'TOL             ', 15            /
DATA  conv_fac_cb6(134) / 1.00          /

DATA  spmh_nam_cb6(135)     , spmh_map_cb6(135)  &
    / 'phenyl_CCO      ', 103           /
DATA  mech_nam_cb6(135)     , mech_map_cb6(135)  &
    / 'ALDX            ', 14            /
DATA  conv_fac_cb6(135) / 0.50          /

DATA  spmh_nam_cb6(136)     , spmh_map_cb6(136)  &
    / 'pyruvic_acid    ', 104           /
DATA  mech_nam_cb6(136)     , mech_map_cb6(136)  &
    / 'FORM            ', 13            /
DATA  conv_fac_cb6(136) / 1.00          /

DATA  spmh_nam_cb6(137)     , spmh_map_cb6(137)  &
    / 'pyruvic_acid    ', 104           /
DATA  mech_nam_cb6(137)     , mech_map_cb6(137)  &
    / 'ALDX            ', 14            /
DATA  conv_fac_cb6(137) / 1.00          /

DATA  spmh_nam_cb6(138)     , spmh_map_cb6(138)  &
    / 'terpinyl_ACT_a  ', 105           /
DATA  mech_nam_cb6(138)     , mech_map_cb6(138)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6(138) / 1.00          /

DATA  spmh_nam_cb6(139)     , spmh_map_cb6(139)  &
    / 'terpinyl_ACT_a  ', 105           /
DATA  mech_nam_cb6(139)     , mech_map_cb6(139)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6(139) / 2.00          /

DATA  spmh_nam_cb6(140)     , spmh_map_cb6(140)  &
    / 'tetradecene_1   ', 106           /
DATA  mech_nam_cb6(140)     , mech_map_cb6(140)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6(140) / 12.00         /

DATA  spmh_nam_cb6(141)     , spmh_map_cb6(141)  &
    / 'tetradecene_1   ', 106           /
DATA  mech_nam_cb6(141)     , mech_map_cb6(141)  &
    / 'OLE             ', 5             /
DATA  conv_fac_cb6(141) / 1.00          /

DATA  spmh_nam_cb6(142)     , spmh_map_cb6(142)  &
    / 'toluene         ', 107           /
DATA  mech_nam_cb6(142)     , mech_map_cb6(142)  &
    / 'TOL             ', 15            /
DATA  conv_fac_cb6(142) / 1.00          /

DATA  spmh_nam_cb6(143)     , spmh_map_cb6(143)  &
    / 'carbon_monoxide ', 108           /
DATA  mech_nam_cb6(143)     , mech_map_cb6(143)  &
    / 'CO              ', 17            /
DATA  conv_fac_cb6(143) / 1.00          /

DATA  spmh_nam_cb6(144)     , spmh_map_cb6(144)  &
    / 'butene          ', 109           /
DATA  mech_nam_cb6(144)     , mech_map_cb6(144)  &
    / 'OLE             ', 5             /
DATA  conv_fac_cb6(144) / 1.00          /

DATA  spmh_nam_cb6(145)     , spmh_map_cb6(145)  &
    / 'butene          ', 109           /
DATA  mech_nam_cb6(145)     , mech_map_cb6(145)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6(145) / 2.00          /

DATA  spmh_nam_cb6(146)     , spmh_map_cb6(146)  &
    / 'ethane          ', 110           /
DATA  mech_nam_cb6(146)     , mech_map_cb6(146)  &
    / 'ETHA            ', 18            /
DATA  conv_fac_cb6(146) / 1.00          /

DATA  spmh_nam_cb6(147)     , spmh_map_cb6(147)  &
    / 'ethene          ', 111           /
DATA  mech_nam_cb6(147)     , mech_map_cb6(147)  &
    / 'ETH             ', 19            /
DATA  conv_fac_cb6(147) / 1.00          /

DATA  spmh_nam_cb6(148)     , spmh_map_cb6(148)  &
    / 'propane         ', 113           /
DATA  mech_nam_cb6(148)     , mech_map_cb6(148)  &
    / 'PRPA            ', 21            /
DATA  conv_fac_cb6(148) / 1.00          /

DATA  spmh_nam_cb6(149)     , spmh_map_cb6(149)  &
    / 'propene         ', 114           /
DATA  mech_nam_cb6(149)     , mech_map_cb6(149)  &
    / 'OLE             ', 5             /
DATA  conv_fac_cb6(149) / 1.00          /

DATA  spmh_nam_cb6(150)     , spmh_map_cb6(150)  &
    / 'propene         ', 114           /
DATA  mech_nam_cb6(150)     , mech_map_cb6(150)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6(150) / 1.00          /

DATA  spmh_nam_cb6((151))     , spmh_map_cb6((151))  &
    / 'diallyl_2s      ', 117           /
DATA  mech_nam_cb6((151))     , mech_map_cb6((151))  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6((151)) / 2.00          /

DATA  spmh_nam_cb6((152))     , spmh_map_cb6((152))  &
    / 'diallyl_2s      ', 117           /
DATA  mech_nam_cb6((152))     , mech_map_cb6((152))  &
    / 'OLE             ', 5             /
DATA  conv_fac_cb6((152)) / 2.00          /

DATA  spmh_nam_cb6((153))     , spmh_map_cb6((153))  &
    / '2met_2s         ', 118           /
DATA  mech_nam_cb6((153))     , mech_map_cb6((153))  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6((153)) / 2.00          /

DATA  spmh_nam_cb6((154))     , spmh_map_cb6((154))  &
    / '2met_s          ', 119           /
DATA  mech_nam_cb6((154))     , mech_map_cb6((154))  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6((154)) / 2.00          /

DATA  spmh_nam_cb6((155))     , spmh_map_cb6((155))  &
    / 'met_chloride    ', 120           /
DATA  mech_nam_cb6((155))     , mech_map_cb6((155))  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6((155)) / 1.00          /

DATA  spmh_nam_cb6((156))     , spmh_map_cb6((156))  &
    / 'met_bromide     ', 121           /
DATA  mech_nam_cb6((156))     , mech_map_cb6((156))  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6((156)) / 1.00          /

DATA  spmh_nam_cb6((157))     , spmh_map_cb6((157))  &
    / 'met_iodide      ', 122           /
DATA  mech_nam_cb6((157))     , mech_map_cb6((157))  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6((157)) / 1.00          /

DATA  spmh_nam_cb6((158))     , spmh_map_cb6((158))  &
    / 'met_mercaptan   ', 124           /
DATA  mech_nam_cb6((158))     , mech_map_cb6((158))  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6((158)) / 1.00          /

DATA  spmh_nam_cb6((159))     , spmh_map_cb6((159))  &
    / 'met_propenyl_2s ', 125           /
DATA  mech_nam_cb6((159))     , mech_map_cb6((159))  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6((159)) / 2.00          /

DATA  spmh_nam_cb6((160))     , spmh_map_cb6((160))  &
    / 'met_propenyl_2s ', 125           /
DATA  mech_nam_cb6((160))     , mech_map_cb6((160))  &
    / 'OLE             ', 5             /
DATA  conv_fac_cb6((160)) / 1.00          /

DATA  spmh_nam_cb6((161))     , spmh_map_cb6((161))  &
    / 'PPPP_2s         ', 126           /
DATA  mech_nam_cb6((161))     , mech_map_cb6((161))  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6((161)) / 4.00          /

DATA  spmh_nam_cb6((162))     , spmh_map_cb6((162))  &
    / 'PPPP_2s         ', 126           /
DATA  mech_nam_cb6((162))     , mech_map_cb6((162))  &
    / 'OLE             ', 5             /
DATA  conv_fac_cb6((162)) / 1.00          /

DATA  spmh_nam_cb6((163))     , spmh_map_cb6((163))  &
    / '2met_nonatriene ', 127           /
DATA  mech_nam_cb6((163))     , mech_map_cb6((163))  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6((163)) / 1.00          /

DATA  spmh_nam_cb6((164))     , spmh_map_cb6((164))  &
    / 'met_salicylate  ', 128           /
DATA  mech_nam_cb6((164))     , mech_map_cb6((164))  &
    / 'TOL             ', 15            /
DATA  conv_fac_cb6((164)) / 1.00          /

DATA  spmh_nam_cb6((165))     , spmh_map_cb6((165))  &
    / 'met_salicylate  ', 128           /
DATA  mech_nam_cb6((165))     , mech_map_cb6((165))  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6((165)) / 1.00          /

DATA  spmh_nam_cb6((166))     , spmh_map_cb6((166))  &
    / 'indole          ', 129           /
DATA  mech_nam_cb6((166))     , mech_map_cb6((166))  &
    / 'TOL             ', 15            /
DATA  conv_fac_cb6((166)) / 1.00          /

DATA  spmh_nam_cb6((167))     , spmh_map_cb6((167))  &
    / 'indole          ', 129           /
DATA  mech_nam_cb6((167))     , mech_map_cb6((167))  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6((167)) / 1.00          /

DATA  spmh_nam_cb6((168))     , spmh_map_cb6((168))  &
    / 'jasmone         ', 130           /
DATA  mech_nam_cb6((168))     , mech_map_cb6((168))  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6((168)) / 1.00          /

DATA  spmh_nam_cb6((169))     , spmh_map_cb6((169))  &
    / 'jasmone         ', 130           /
DATA  mech_nam_cb6((169))     , mech_map_cb6((169))  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6((169)) / 1.00          /

DATA  spmh_nam_cb6((170))     , spmh_map_cb6((170))  &
    / 'met_jasmonate   ', 131           /
DATA  mech_nam_cb6((170))     , mech_map_cb6((170))  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6((170)) / 1.00          /

DATA  spmh_nam_cb6((171))     , spmh_map_cb6((171))  &
    / 'met_jasmonate   ', 131           /
DATA  mech_nam_cb6((171))     , mech_map_cb6((171))  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6((171)) / 3.00          /

DATA  spmh_nam_cb6((172))     , spmh_map_cb6((172))  &
    / '3met_3DCTT      ', 132           /
DATA  mech_nam_cb6((172))     , mech_map_cb6((172))  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6((172)) / 16.00         /

DATA  spmh_nam_cb6((173))     , spmh_map_cb6((173))  &
    / 'hexanal         ', 133           /
DATA  mech_nam_cb6((173))     , mech_map_cb6((173))  &
    / 'ALDX            ', 14            /
DATA  conv_fac_cb6((173)) / 1.00          /

DATA  spmh_nam_cb6((174))     , spmh_map_cb6((174))  &
    / 'hexanal         ', 133           /
DATA  mech_nam_cb6((174))     , mech_map_cb6((174))  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6((174)) / 4.00          /

DATA  spmh_nam_cb6((175))     , spmh_map_cb6((175))  &
    / 'hexanol_1       ', 134           /
DATA  mech_nam_cb6((175))     , mech_map_cb6((175))  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6((175)) / 6.00          /

DATA  spmh_nam_cb6((176))     , spmh_map_cb6((176))  &
    / 'hexenal_c3      ', 135           /
DATA  mech_nam_cb6((176))     , mech_map_cb6((176))  &
    / 'IOLE            ', 16            /
DATA  conv_fac_cb6((176)) / 1.00          /

DATA  spmh_nam_cb6((177))     , spmh_map_cb6((177))  &
    / 'hexenal_c3      ', 135           /
DATA  mech_nam_cb6((177))     , mech_map_cb6((177))  &
    / 'ALDX            ', 14            /
DATA  conv_fac_cb6((177)) / 1.00          /

DATA  spmh_nam_cb6((178))     , spmh_map_cb6((178))  &
    / 'hexenal_t2      ', 136           /
DATA  mech_nam_cb6((178))     , mech_map_cb6((178))  &
    / 'IOLE            ', 16            /
DATA  conv_fac_cb6((178)) / 1.00          /

DATA  spmh_nam_cb6((179))     , spmh_map_cb6((179))  &
    / 'hexenal_t2      ', 136           /
DATA  mech_nam_cb6((179))     , mech_map_cb6((179))  &
    / 'ALDX            ', 14            /
DATA  conv_fac_cb6((179)) / 1.00          /

DATA  spmh_nam_cb6((180))     , spmh_map_cb6((180))  &
    / 'hexenol_c3      ', 137           /
DATA  mech_nam_cb6((180))     , mech_map_cb6((180))  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6((180)) / 2.00          /

DATA  spmh_nam_cb6((181))     , spmh_map_cb6((181))  &
    / 'hexenol_c3      ', 137           /
DATA  mech_nam_cb6((181))     , mech_map_cb6((181))  &
    / 'IOLE            ', 16            /
DATA  conv_fac_cb6((181)) / 1.00          /

DATA  spmh_nam_cb6((182))     , spmh_map_cb6((182))  &
    / 'hexenyl_ACT_c3  ', 138           /
DATA  mech_nam_cb6((182))     , mech_map_cb6((182))  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6((182)) / 3.00          /

DATA  spmh_nam_cb6((183))     , spmh_map_cb6((183))  &
    / 'hexenyl_ACT_c3  ', 138           /
DATA  mech_nam_cb6((183))     , mech_map_cb6((183))  &
    / 'IOLE            ', 16            /
DATA  conv_fac_cb6((183)) / 1.00          /

DATA  spmh_nam_cb6((184))     , spmh_map_cb6((184))  &
    / 'hexenyl_ACT_c3  ', 138           /
DATA  mech_nam_cb6((184))     , mech_map_cb6((184))  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6((184)) / 1.00          /

DATA  spmh_nam_cb6((185))     , spmh_map_cb6((185))  &
    / 'homosalate      ', 139           /
DATA  mech_nam_cb6((185))     , mech_map_cb6((185))  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6((185)) / 1.00          /

DATA  spmh_nam_cb6((186))     , spmh_map_cb6((186))  &
    / 'homosalate      ', 139           /
DATA  mech_nam_cb6((186))     , mech_map_cb6((186))  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6((186)) / 3.00          /

DATA  spmh_nam_cb6((187))     , spmh_map_cb6((187))  &
    / 'Ehsalate        ', 140           /
DATA  mech_nam_cb6((187))     , mech_map_cb6((187))  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6((187)) / 1.00          /

DATA  spmh_nam_cb6((188))     , spmh_map_cb6((188))  &
    / 'Ehsalate        ', 140           /
DATA  mech_nam_cb6((188))     , mech_map_cb6((188))  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6((188)) / 3.00          /

DATA  spmh_nam_cb6((189))     , spmh_map_cb6((189))  &
    / 'pentanal        ', 141           /
DATA  mech_nam_cb6((189))     , mech_map_cb6((189))  &
    / 'ALDX            ', 14            /
DATA  conv_fac_cb6((189)) / 1.00          /

DATA  spmh_nam_cb6((190))     , spmh_map_cb6((190))  &
    / 'pentanal        ', 141           /
DATA  mech_nam_cb6((190))     , mech_map_cb6((190))  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6((190)) / 3.00          /

DATA  spmh_nam_cb6((191))     , spmh_map_cb6((191))  &
    / 'heptanone       ', 142           /
DATA  mech_nam_cb6((191))     , mech_map_cb6((191))  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6((191)) / 6.00          /

DATA  spmh_nam_cb6((192))     , spmh_map_cb6((192))  &
    / 'anisole         ', 143           /
DATA  mech_nam_cb6((192))     , mech_map_cb6((192))  &
    / 'TOL             ', 15            /
DATA  conv_fac_cb6((192)) / 1.00          /

DATA  spmh_nam_cb6((193))     , spmh_map_cb6((193))  &
    / 'verbenene       ', 144           /
DATA  mech_nam_cb6((193))     , mech_map_cb6((193))  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6((193)) / 1.00          /

DATA  spmh_nam_cb6((194))     , spmh_map_cb6((194))  &
    / 'benzyl-acetate  ', 145           /
DATA  mech_nam_cb6((194))     , mech_map_cb6((194))  &
    / 'TOL             ', 15            /
DATA  conv_fac_cb6((194)) / 1.00          /

DATA  spmh_nam_cb6((195))     , spmh_map_cb6((195))  &
    / 'benzyl-acetate  ', 145           /
DATA  mech_nam_cb6((195))     , mech_map_cb6((195))  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6((195)) / 1.00          /

DATA  spmh_nam_cb6((196))     , spmh_map_cb6((196))  &
    / 'benzyl-acetate  ', 145           /
DATA  mech_nam_cb6((196))     , mech_map_cb6((196))  &
    / 'NR              ', 6             /
DATA  conv_fac_cb6((196)) / 1.00          /

DATA  spmh_nam_cb6((197))     , spmh_map_cb6((197))  &
    / 'myrtenal        ', 146           /
DATA  mech_nam_cb6((197))     , mech_map_cb6((197))  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6((197)) / 1.00          /

DATA  spmh_nam_cb6((198))     , spmh_map_cb6((198))  &
    / 'benzyl-alcohol  ', 147           /
DATA  mech_nam_cb6((198))     , mech_map_cb6((198))  &
    / 'TOL             ', 15            /
DATA  conv_fac_cb6((198)) / 1.00          /

DATA  spmh_nam_cb6((199))     , spmh_map_cb6((199))  &
    / 'meta-cymenene   ', 148           /
DATA  mech_nam_cb6((199))     , mech_map_cb6((199))  &
    / 'XYL             ', 4             /
DATA  conv_fac_cb6((199)) / 1.00          /

DATA  spmh_nam_cb6((200))     , spmh_map_cb6((200))  &
    / 'meta-cymenene   ', 148           /
DATA  mech_nam_cb6((200))     , mech_map_cb6((200))  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6((200)) / 2.00          /

DATA  spmh_nam_cb6((201))     , spmh_map_cb6((201))  &
    / 'ipsenol         ', 149           /
DATA  mech_nam_cb6((201))     , mech_map_cb6((201))  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb6((201)) / 1.00          /

DATA  spmh_nam_cb6((202))     , spmh_map_cb6((202))  &
    / 'Napthalene      ', 150           /
DATA  mech_nam_cb6((202))     , mech_map_cb6((202))  &
    / 'XYL             ', 4             /
DATA  conv_fac_cb6((202)) / 1.00          /

DATA  spmh_nam_cb6((203))     , spmh_map_cb6((203))  &
    / 'Napthalene      ', 150           /
DATA  mech_nam_cb6((203))     , mech_map_cb6((203))  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb6((203)) / 2.00          /

!===================================================
DATA  spmh_nam_cb6((204))     , spmh_map_cb6((204))  &
    / 'fenchone        ', 26            /
DATA  mech_nam_cb6((204))     , mech_map_cb6((204))  &
    / 'KET             ', 24            /
DATA  conv_fac_cb6((204)) / 1.00          /

DATA  spmh_nam_cb6((205))     , spmh_map_cb6((205))  &
    / 'thujone_a       ', 28            /
DATA  mech_nam_cb6((205))     , mech_map_cb6((205))  &
    / 'KET             ', 24            /
DATA  conv_fac_cb6((205)) / 1.00          /

DATA  spmh_nam_cb6((206))     , spmh_map_cb6((206))  &
    / 'thujone_b       ', 29            /
DATA  mech_nam_cb6((206))     , mech_map_cb6((206))  &
    / 'KET             ', 24            /
DATA  conv_fac_cb6((206)) / 1.00          /

DATA  spmh_nam_cb6((207))     , spmh_map_cb6((207))  &
    / 'butanone_2      ', 86            /
DATA  mech_nam_cb6((207))     , mech_map_cb6((207))  &
    / 'KET             ', 24            /
DATA  conv_fac_cb6((207)) / 1.00          /

DATA  spmh_nam_cb6((208))     , spmh_map_cb6((208))  &
    / 'met_heptenone   ', 94            /
DATA  mech_nam_cb6((208))     , mech_map_cb6((208))  &
    / 'KET             ', 24            /
DATA  conv_fac_cb6((208)) / 1.00          /

DATA  spmh_nam_cb6((209))     , spmh_map_cb6((209))  &
    / 'neryl_acetone   ', 95            /
DATA  mech_nam_cb6((209))     , mech_map_cb6((209))  &
    / 'KET             ', 24            /
DATA  conv_fac_cb6((209)) / 1.00          /

DATA  spmh_nam_cb6((210))     , spmh_map_cb6((210))  &
    / 'heptanone       ', 142           /
DATA  mech_nam_cb6((210))     , mech_map_cb6((210))  &
    / 'KET             ', 24            /
DATA  conv_fac_cb6((210)) / 1.00          /
!===================================================

END module map_cv2cb6