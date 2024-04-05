module map_cv2cb05
!=======================================================================
!  MAP_CB05_CV2CB05.EXT
!  This include file contains conversion table for 150 speciated species
!  to CB05 (CMAQ/CAMx) species


!  MEGAN v2.10
!  INPUT version x.x

!  History:
!  Who          When       What
!  ---------------------------------------------------------------------
!  bkoo         04/13/07 - Created
!  Tan          07/18/11 - Updated for MEGANv2.10
!=======================================================================

CHARACTER (LEN=16) :: map_cb05mech
PARAMETER     (map_cb05mech = 'CB05            ')

INTEGER :: n_cb05
PARAMETER     (n_cb05 = 204)        ! Number of map species

CHARACTER (LEN=16) :: spmh_nam_cb05( n_cb05 )   ! speciated species name
INTEGER :: spmh_map_cb05( n_cb05 )   ! speciated species name
! mapped to SPC_SPCAT.EXT
CHARACTER (LEN=16) :: mech_nam_cb05( n_cb05 )   ! mechanism species
INTEGER :: mech_map_cb05( n_cb05 )   ! mechanism species mapped
! to SPC_CB4Q.EXT
REAL :: conv_fac_cb05( n_cb05 )   ! conversion factor


DATA  spmh_nam_cb05(  1)     , spmh_map_cb05(  1)  &
    / 'isoprene        ', 1             /
DATA  mech_nam_cb05(  1)     , mech_map_cb05(  1)  &
    / 'ISOP            ', 1             /
DATA  conv_fac_cb05(  1) / 1.00          /

DATA  spmh_nam_cb05(  2)     , spmh_map_cb05(  2)  &
    / 'myrcene         ', 2             /
DATA  mech_nam_cb05(  2)     , mech_map_cb05(  2)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05(  2) / 1.00          /

DATA  spmh_nam_cb05(  3)     , spmh_map_cb05(  3)  &
    / 'sabinene        ', 3             /
DATA  mech_nam_cb05(  3)     , mech_map_cb05(  3)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05(  3) / 1.00          /

DATA  spmh_nam_cb05(  4)     , spmh_map_cb05(  4)  &
    / 'limonene        ', 4             /
DATA  mech_nam_cb05(  4)     , mech_map_cb05(  4)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05(  4) / 1.00          /

DATA  spmh_nam_cb05(  5)     , spmh_map_cb05(  5)  &
    / 'carene_3        ', 5             /
DATA  mech_nam_cb05(  5)     , mech_map_cb05(  5)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05(  5) / 1.00          /

DATA  spmh_nam_cb05(  6)     , spmh_map_cb05(  6)  &
    / 'ocimene_t_b     ', 6             /
DATA  mech_nam_cb05(  6)     , mech_map_cb05(  6)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05(  6) / 1.00          /

DATA  spmh_nam_cb05(  7)     , spmh_map_cb05(  7)  &
    / 'pinene_b        ', 7             /
DATA  mech_nam_cb05(  7)     , mech_map_cb05(  7)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05(  7) / 1.00          /

DATA  spmh_nam_cb05(  8)     , spmh_map_cb05(  8)  &
    / 'pinene_a        ', 8             /
DATA  mech_nam_cb05(  8)     , mech_map_cb05(  8)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05(  8) / 1.00          /

DATA  spmh_nam_cb05(  9)     , spmh_map_cb05(  9)  &
    / '2met_styrene    ', 9             /
DATA  mech_nam_cb05(  9)     , mech_map_cb05(  9)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(  9) / 1.00          /

DATA  spmh_nam_cb05( 10)     , spmh_map_cb05( 10)  &
    / '2met_styrene    ', 9             /
DATA  mech_nam_cb05( 10)     , mech_map_cb05( 10)  &
    / 'XYL             ', 4             /
DATA  conv_fac_cb05( 10) / 1.00          /

DATA  spmh_nam_cb05( 11)     , spmh_map_cb05( 11)  &
    / '2met_styrene    ', 9             /
DATA  mech_nam_cb05( 11)     , mech_map_cb05( 11)  &
    / 'OLE             ', 5             /
DATA  conv_fac_cb05( 11) / 0.50          /

DATA  spmh_nam_cb05( 12)     , spmh_map_cb05( 12)  &
    / 'cymene_p        ', 10            /
DATA  mech_nam_cb05( 12)     , mech_map_cb05( 12)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05( 12) / 2.00          /

DATA  spmh_nam_cb05( 13)     , spmh_map_cb05( 13)  &
    / 'cymene_p        ', 10            /
DATA  mech_nam_cb05( 13)     , mech_map_cb05( 13)  &
    / 'XYL             ', 4             /
DATA  conv_fac_cb05( 13) / 1.00          /

DATA  spmh_nam_cb05( 14)     , spmh_map_cb05( 14)  &
    / 'cymene_o        ', 11            /
DATA  mech_nam_cb05( 14)     , mech_map_cb05( 14)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05( 14) / 2.00          /

DATA  spmh_nam_cb05( 15)     , spmh_map_cb05( 15)  &
    / 'cymene_o        ', 11            /
DATA  mech_nam_cb05( 15)     , mech_map_cb05( 15)  &
    / 'XYL             ', 4             /
DATA  conv_fac_cb05( 15) / 1.00          /

DATA  spmh_nam_cb05( 16)     , spmh_map_cb05( 16)  &
    / 'phellandrene_a  ', 12            /
DATA  mech_nam_cb05( 16)     , mech_map_cb05( 16)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05( 16) / 1.00          /

DATA  spmh_nam_cb05( 17)     , spmh_map_cb05( 17)  &
    / 'thujene_a       ', 13            /
DATA  mech_nam_cb05( 17)     , mech_map_cb05( 17)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05( 17) / 1.00          /

DATA  spmh_nam_cb05( 18)     , spmh_map_cb05( 18)  &
    / 'terpinene_a     ', 14            /
DATA  mech_nam_cb05( 18)     , mech_map_cb05( 18)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05( 18) / 1.00          /

DATA  spmh_nam_cb05( 19)     , spmh_map_cb05( 19)  &
    / 'terpinene_g     ', 15            /
DATA  mech_nam_cb05( 19)     , mech_map_cb05( 19)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05( 19) / 1.00          /

DATA  spmh_nam_cb05( 20)     , spmh_map_cb05( 20)  &
    / 'terpinolene     ', 16            /
DATA  mech_nam_cb05( 20)     , mech_map_cb05( 20)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05( 20) / 1.00          /

DATA  spmh_nam_cb05( 21)     , spmh_map_cb05( 21)  &
    / 'phellandrene_b  ', 17            /
DATA  mech_nam_cb05( 21)     , mech_map_cb05( 21)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05( 21) / 1.00          /

DATA  spmh_nam_cb05( 22)     , spmh_map_cb05( 22)  &
    / 'camphene        ', 18            /
DATA  mech_nam_cb05( 22)     , mech_map_cb05( 22)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05( 22) / 1.00          /

DATA  spmh_nam_cb05( 23)     , spmh_map_cb05( 23)  &
    / 'bornene         ', 19            /
DATA  mech_nam_cb05( 23)     , mech_map_cb05( 23)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05( 23) / 1.00          /

DATA  spmh_nam_cb05( 24)     , spmh_map_cb05( 24)  &
    / 'fenchene_a      ', 20            /
DATA  mech_nam_cb05( 24)     , mech_map_cb05( 24)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05( 24) / 1.00          /

DATA  spmh_nam_cb05( 25)     , spmh_map_cb05( 25)  &
    / 'ocimene_al      ', 21            /
DATA  mech_nam_cb05( 25)     , mech_map_cb05( 25)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05( 25) / 1.00          /

DATA  spmh_nam_cb05( 26)     , spmh_map_cb05( 26)  &
    / 'ocimene_c_b     ', 22            /
DATA  mech_nam_cb05( 26)     , mech_map_cb05( 26)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05( 26) / 1.00          /

DATA  spmh_nam_cb05( 27)     , spmh_map_cb05( 27)  &
    / 'tricyclene      ', 23            /
DATA  mech_nam_cb05( 27)     , mech_map_cb05( 27)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05( 27) / 9.00          /

DATA  spmh_nam_cb05( 28)     , spmh_map_cb05( 28)  &
    / 'tricyclene      ', 23            /
DATA  mech_nam_cb05( 28)     , mech_map_cb05( 28)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 28) / 1.00          /

DATA  spmh_nam_cb05( 29)     , spmh_map_cb05( 29)  &
    / 'estragole       ', 24            /
DATA  mech_nam_cb05( 29)     , mech_map_cb05( 29)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05( 29) / 1.00          /

DATA  spmh_nam_cb05( 30)     , spmh_map_cb05( 30)  &
    / 'camphor         ', 25            /
DATA  mech_nam_cb05( 30)     , mech_map_cb05( 30)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05( 30) / 8.00          /

DATA  spmh_nam_cb05( 31)     , spmh_map_cb05( 31)  &
    / 'camphor         ', 25            /
DATA  mech_nam_cb05( 31)     , mech_map_cb05( 31)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 31) / 2.00          /

DATA  spmh_nam_cb05( 32)     , spmh_map_cb05( 32)  &
    / 'fenchone        ', 26            /
DATA  mech_nam_cb05( 32)     , mech_map_cb05( 32)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05( 32) / 8.00          /

DATA  spmh_nam_cb05( 33)     , spmh_map_cb05( 33)  &
    / 'fenchone        ', 26            /
DATA  mech_nam_cb05( 33)     , mech_map_cb05( 33)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 33) / 2.00          /

DATA  spmh_nam_cb05( 34)     , spmh_map_cb05( 34)  &
    / 'piperitone      ', 27            /
DATA  mech_nam_cb05( 34)     , mech_map_cb05( 34)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05( 34) / 1.00          /

DATA  spmh_nam_cb05( 35)     , spmh_map_cb05( 35)  &
    / 'thujone_a       ', 28            /
DATA  mech_nam_cb05( 35)     , mech_map_cb05( 35)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05( 35) / 9.00          /

DATA  spmh_nam_cb05( 36)     , spmh_map_cb05( 36)  &
    / 'thujone_a       ', 28            /
DATA  mech_nam_cb05( 36)     , mech_map_cb05( 36)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 36) / 1.00          /

DATA  spmh_nam_cb05( 37)     , spmh_map_cb05( 37)  &
    / 'thujone_b       ', 29            /
DATA  mech_nam_cb05( 37)     , mech_map_cb05( 37)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05( 37) / 9.00          /

DATA  spmh_nam_cb05( 38)     , spmh_map_cb05( 38)  &
    / 'thujone_b       ', 29            /
DATA  mech_nam_cb05( 38)     , mech_map_cb05( 38)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 38) / 1.00          /

DATA  spmh_nam_cb05( 39)     , spmh_map_cb05( 39)  &
    / 'cineole_1_8     ', 30            /
DATA  mech_nam_cb05( 39)     , mech_map_cb05( 39)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05( 39) / 9.00          /

DATA  spmh_nam_cb05( 40)     , spmh_map_cb05( 40)  &
    / 'cineole_1_8     ', 30            /
DATA  mech_nam_cb05( 40)     , mech_map_cb05( 40)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 40) / 1.00          /

DATA  spmh_nam_cb05( 41)     , spmh_map_cb05( 41)  &
    / 'borneol         ', 31            /
DATA  mech_nam_cb05( 41)     , mech_map_cb05( 41)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05( 41) / 8.00          /

DATA  spmh_nam_cb05( 42)     , spmh_map_cb05( 42)  &
    / 'borneol         ', 31            /
DATA  mech_nam_cb05( 42)     , mech_map_cb05( 42)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 42) / 2.00          /

DATA  spmh_nam_cb05( 43)     , spmh_map_cb05( 43)  &
    / 'linalool        ', 32            /
DATA  mech_nam_cb05( 43)     , mech_map_cb05( 43)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05( 43) / 1.00          /

DATA  spmh_nam_cb05( 44)     , spmh_map_cb05( 44)  &
    / 'terpineol_4     ', 33            /
DATA  mech_nam_cb05( 44)     , mech_map_cb05( 44)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05( 44) / 1.00          /

DATA  spmh_nam_cb05( 45)     , spmh_map_cb05( 45)  &
    / 'terpineol_a     ', 34            /
DATA  mech_nam_cb05( 45)     , mech_map_cb05( 45)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05( 45) / 1.00          /

DATA  spmh_nam_cb05( 46)     , spmh_map_cb05( 46)  &
    / 'linalool_OXD_c  ', 35            /
DATA  mech_nam_cb05( 46)     , mech_map_cb05( 46)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05( 46) / 1.00          /

DATA  spmh_nam_cb05( 47)     , spmh_map_cb05( 47)  &
    / 'linalool_OXD_t  ', 36            /
DATA  mech_nam_cb05( 47)     , mech_map_cb05( 47)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05( 47) / 1.00          /

DATA  spmh_nam_cb05( 48)     , spmh_map_cb05( 48)  &
    / 'ionone_b        ', 37            /
DATA  mech_nam_cb05( 48)     , mech_map_cb05( 48)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05( 48) / 1.00          /

DATA  spmh_nam_cb05( 49)     , spmh_map_cb05( 49)  &
    / 'ionone_b        ', 37            /
DATA  mech_nam_cb05( 49)     , mech_map_cb05( 49)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 49) / 3.00          /

DATA  spmh_nam_cb05( 50)     , spmh_map_cb05( 50)  &
    / 'bornyl_ACT      ', 38            /
DATA  mech_nam_cb05( 50)     , mech_map_cb05( 50)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05( 50) / 6.00          /

DATA  spmh_nam_cb05( 51)     , spmh_map_cb05( 51)  &
    / 'bornyl_ACT      ', 38            /
DATA  mech_nam_cb05( 51)     , mech_map_cb05( 51)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 51) / 6.00          /

DATA  spmh_nam_cb05( 52)     , spmh_map_cb05( 52)  &
    / 'farnescene_a    ', 39            /
DATA  mech_nam_cb05( 52)     , mech_map_cb05( 52)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 52) / 15.00         /

DATA  spmh_nam_cb05( 53)     , spmh_map_cb05( 53)  &
    / 'caryophyllene_b ', 40            /
DATA  mech_nam_cb05( 53)     , mech_map_cb05( 53)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 53) / 15.00         /

DATA  spmh_nam_cb05( 54)     , spmh_map_cb05( 54)  &
    / 'acoradiene      ', 41            /
DATA  mech_nam_cb05( 54)     , mech_map_cb05( 54)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 54) / 15.00         /

DATA  spmh_nam_cb05( 55)     , spmh_map_cb05( 55)  &
    / 'aromadendrene   ', 42            /
DATA  mech_nam_cb05( 55)     , mech_map_cb05( 55)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 55) / 15.00         /

DATA  spmh_nam_cb05( 56)     , spmh_map_cb05( 56)  &
    / 'bergamotene_a   ', 43            /
DATA  mech_nam_cb05( 56)     , mech_map_cb05( 56)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 56) / 15.00         /

DATA  spmh_nam_cb05( 57)     , spmh_map_cb05( 57)  &
    / 'bergamotene_b   ', 44            /
DATA  mech_nam_cb05( 57)     , mech_map_cb05( 57)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 57) / 15.00         /

DATA  spmh_nam_cb05( 58)     , spmh_map_cb05( 58)  &
    / 'bisabolene_a    ', 45            /
DATA  mech_nam_cb05( 58)     , mech_map_cb05( 58)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 58) / 15.00         /

DATA  spmh_nam_cb05( 59)     , spmh_map_cb05( 59)  &
    / 'bisabolene_b    ', 46            /
DATA  mech_nam_cb05( 59)     , mech_map_cb05( 59)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 59) / 15.00         /

DATA  spmh_nam_cb05( 60)     , spmh_map_cb05( 60)  &
    / 'bourbonene_b    ', 47            /
DATA  mech_nam_cb05( 60)     , mech_map_cb05( 60)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 60) / 15.00         /

DATA  spmh_nam_cb05( 61)     , spmh_map_cb05( 61)  &
    / 'cadinene_d      ', 48            /
DATA  mech_nam_cb05( 61)     , mech_map_cb05( 61)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 61) / 15.00         /

DATA  spmh_nam_cb05( 62)     , spmh_map_cb05( 62)  &
    / 'cadinene_g      ', 49            /
DATA  mech_nam_cb05( 62)     , mech_map_cb05( 62)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 62) / 15.00         /

DATA  spmh_nam_cb05( 63)     , spmh_map_cb05( 63)  &
    / 'cedrene_a       ', 50            /
DATA  mech_nam_cb05( 63)     , mech_map_cb05( 63)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 63) / 15.00         /

DATA  spmh_nam_cb05( 64)     , spmh_map_cb05( 64)  &
    / 'copaene_a       ', 51            /
DATA  mech_nam_cb05( 64)     , mech_map_cb05( 64)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 64) / 15.00         /

DATA  spmh_nam_cb05( 65)     , spmh_map_cb05( 65)  &
    / 'cubebene_a      ', 52            /
DATA  mech_nam_cb05( 65)     , mech_map_cb05( 65)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 65) / 15.00         /

DATA  spmh_nam_cb05( 66)     , spmh_map_cb05( 66)  &
    / 'cubebene_b      ', 53            /
DATA  mech_nam_cb05( 66)     , mech_map_cb05( 66)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 66) / 15.00         /

DATA  spmh_nam_cb05( 67)     , spmh_map_cb05( 67)  &
    / 'elemene_b       ', 54            /
DATA  mech_nam_cb05( 67)     , mech_map_cb05( 67)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 67) / 15.00         /

DATA  spmh_nam_cb05( 68)     , spmh_map_cb05( 68)  &
    / 'farnescene_b    ', 55            /
DATA  mech_nam_cb05( 68)     , mech_map_cb05( 68)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 68) / 15.00         /

DATA  spmh_nam_cb05( 69)     , spmh_map_cb05( 69)  &
    / 'germacrene_B    ', 56            /
DATA  mech_nam_cb05( 69)     , mech_map_cb05( 69)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 69) / 15.00         /

DATA  spmh_nam_cb05( 70)     , spmh_map_cb05( 70)  &
    / 'germacrene_D    ', 57            /
DATA  mech_nam_cb05( 70)     , mech_map_cb05( 70)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 70) / 15.00         /

DATA  spmh_nam_cb05( 71)     , spmh_map_cb05( 71)  &
    / 'gurjunene_b     ', 58            /
DATA  mech_nam_cb05( 71)     , mech_map_cb05( 71)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 71) / 15.00         /

DATA  spmh_nam_cb05( 72)     , spmh_map_cb05( 72)  &
    / 'humulene_a      ', 59            /
DATA  mech_nam_cb05( 72)     , mech_map_cb05( 72)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 72) / 15.00         /

DATA  spmh_nam_cb05( 73)     , spmh_map_cb05( 73)  &
    / 'humulene_g      ', 60            /
DATA  mech_nam_cb05( 73)     , mech_map_cb05( 73)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 73) / 15.00         /

DATA  spmh_nam_cb05( 74)     , spmh_map_cb05( 74)  &
    / 'isolongifolene  ', 61            /
DATA  mech_nam_cb05( 74)     , mech_map_cb05( 74)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 74) / 15.00         /

DATA  spmh_nam_cb05( 75)     , spmh_map_cb05( 75)  &
    / 'longifolene     ', 62            /
DATA  mech_nam_cb05( 75)     , mech_map_cb05( 75)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 75) / 15.00         /

DATA  spmh_nam_cb05( 76)     , spmh_map_cb05( 76)  &
    / 'longipinene     ', 63            /
DATA  mech_nam_cb05( 76)     , mech_map_cb05( 76)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 76) / 15.00         /

DATA  spmh_nam_cb05( 77)     , spmh_map_cb05( 77)  &
    / 'muurolene_a     ', 64            /
DATA  mech_nam_cb05( 77)     , mech_map_cb05( 77)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 77) / 15.00         /

DATA  spmh_nam_cb05( 78)     , spmh_map_cb05( 78)  &
    / 'muurolene_g     ', 65            /
DATA  mech_nam_cb05( 78)     , mech_map_cb05( 78)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 78) / 15.00         /

DATA  spmh_nam_cb05( 79)     , spmh_map_cb05( 79)  &
    / 'selinene_b      ', 66            /
DATA  mech_nam_cb05( 79)     , mech_map_cb05( 79)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 79) / 15.00         /

DATA  spmh_nam_cb05( 80)     , spmh_map_cb05( 80)  &
    / 'selinene_d      ', 67            /
DATA  mech_nam_cb05( 80)     , mech_map_cb05( 80)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 80) / 15.00         /

DATA  spmh_nam_cb05( 81)     , spmh_map_cb05( 81)  &
    / 'nerolidol_c     ', 68            /
DATA  mech_nam_cb05( 81)     , mech_map_cb05( 81)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 81) / 15.00         /

DATA  spmh_nam_cb05( 82)     , spmh_map_cb05( 82)  &
    / 'nerolidol_t     ', 69            /
DATA  mech_nam_cb05( 82)     , mech_map_cb05( 82)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 82) / 15.00         /

DATA  spmh_nam_cb05( 83)     , spmh_map_cb05( 83)  &
    / 'cedrol          ', 70            /
DATA  mech_nam_cb05( 83)     , mech_map_cb05( 83)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05( 83) / 13.00         /

DATA  spmh_nam_cb05( 84)     , spmh_map_cb05( 84)  &
    / 'cedrol          ', 70            /
DATA  mech_nam_cb05( 84)     , mech_map_cb05( 84)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 84) / 2.00          /

DATA  spmh_nam_cb05( 85)     , spmh_map_cb05( 85)  &
    / 'MBO_2m3e2ol     ', 71            /
DATA  mech_nam_cb05( 85)     , mech_map_cb05( 85)  &
    / 'OLE             ', 5             /
DATA  conv_fac_cb05( 85) / 1.00          /

DATA  spmh_nam_cb05( 86)     , spmh_map_cb05( 86)  &
    / 'MBO_2m3e2ol     ', 71            /
DATA  mech_nam_cb05( 86)     , mech_map_cb05( 86)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05( 86) / 3.00          /

DATA  spmh_nam_cb05( 87)     , spmh_map_cb05( 87)  &
    / 'methanol        ', 72            /
DATA  mech_nam_cb05( 87)     , mech_map_cb05( 87)  &
    / 'MEOH            ', 7             /
DATA  conv_fac_cb05( 87) / 1.00          /

DATA  spmh_nam_cb05( 88)     , spmh_map_cb05( 88)  &
    / 'acetone         ', 73            /
DATA  mech_nam_cb05( 88)     , mech_map_cb05( 88)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05( 88) / 3.00          /

DATA  spmh_nam_cb05( 89)     , spmh_map_cb05( 89)  &
    / 'methane         ', 74            /
DATA  mech_nam_cb05( 89)     , mech_map_cb05( 89)  &
    / 'CH4             ', 8             /
DATA  conv_fac_cb05( 89) / 1.00          /

DATA  spmh_nam_cb05( 90)     , spmh_map_cb05( 90)  &
    / 'ammonia         ', 75            /
DATA  mech_nam_cb05( 90)     , mech_map_cb05( 90)  &
    / 'NH3             ', 9             /
DATA  conv_fac_cb05( 90) / 1.00          /

DATA  spmh_nam_cb05( 91)     , spmh_map_cb05( 91)  &
    / 'nitric_OXD      ', 77            /
DATA  mech_nam_cb05( 91)     , mech_map_cb05( 91)  &
    / 'NO              ', 10            /
DATA  conv_fac_cb05( 91) / 1.00          /

DATA  spmh_nam_cb05( 92)     , spmh_map_cb05( 92)  &
    / 'acetaldehyde    ', 78            /
DATA  mech_nam_cb05( 92)     , mech_map_cb05( 92)  &
    / 'ALD2            ', 11            /
DATA  conv_fac_cb05( 92) / 1.00          /

DATA  spmh_nam_cb05( 93)     , spmh_map_cb05( 93)  &
    / 'ethanol         ', 79            /
DATA  mech_nam_cb05( 93)     , mech_map_cb05( 93)  &
    / 'ETOH            ', 12            /
DATA  conv_fac_cb05( 93) / 1.00          /

DATA  spmh_nam_cb05( 94)     , spmh_map_cb05( 94)  &
    / 'formic_acid     ', 80            /
DATA  mech_nam_cb05( 94)     , mech_map_cb05( 94)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 94) / 1.00          /

DATA  spmh_nam_cb05( 95)     , spmh_map_cb05( 95)  &
    / 'formaldehyde    ', 81            /
DATA  mech_nam_cb05( 95)     , mech_map_cb05( 95)  &
    / 'FORM            ', 13            /
DATA  conv_fac_cb05( 95) / 1.00          /

DATA  spmh_nam_cb05( 96)     , spmh_map_cb05( 96)  &
    / 'acetic_acid     ', 82            /
DATA  mech_nam_cb05( 96)     , mech_map_cb05( 96)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05( 96) / 1.00          /

DATA  spmh_nam_cb05( 97)     , spmh_map_cb05( 97)  &
    / 'acetic_acid     ', 82            /
DATA  mech_nam_cb05( 97)     , mech_map_cb05( 97)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05( 97) / 1.00          /

DATA  spmh_nam_cb05( 98)     , spmh_map_cb05( 98)  &
    / 'MBO_3m2e1ol     ', 83            /
DATA  mech_nam_cb05( 98)     , mech_map_cb05( 98)  &
    / 'ALDX            ', 14            /
DATA  conv_fac_cb05( 98) / 1.00          /

DATA  spmh_nam_cb05( 99)     , spmh_map_cb05( 99)  &
    / 'MBO_3m2e1ol     ', 83            /
DATA  mech_nam_cb05( 99)     , mech_map_cb05( 99)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05( 99) / 3.00          /

DATA  spmh_nam_cb05(100)     , spmh_map_cb05(100)  &
    / 'MBO_3m3e1ol     ', 84            /
DATA  mech_nam_cb05(100)     , mech_map_cb05(100)  &
    / 'FORM            ', 13            /
DATA  conv_fac_cb05(100) / 1.00          /

DATA  spmh_nam_cb05(101)     , spmh_map_cb05(101)  &
    / 'MBO_3m3e1ol     ', 84            /
DATA  mech_nam_cb05(101)     , mech_map_cb05(101)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(101) / 4.00          /

DATA  spmh_nam_cb05(102)     , spmh_map_cb05(102)  &
    / 'benzaldehyde    ', 85            /
DATA  mech_nam_cb05(102)     , mech_map_cb05(102)  &
    / 'TOL             ', 15            /
DATA  conv_fac_cb05(102) / 1.00          /

DATA  spmh_nam_cb05(103)     , spmh_map_cb05(103)  &
    / 'butanone_2      ', 86            /
DATA  mech_nam_cb05(103)     , mech_map_cb05(103)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(103) / 4.00          /

DATA  spmh_nam_cb05(104)     , spmh_map_cb05(104)  &
    / 'decanal         ', 87            /
DATA  mech_nam_cb05(104)     , mech_map_cb05(104)  &
    / 'ALDX            ', 14            /
DATA  conv_fac_cb05(104) / 1.00          /

DATA  spmh_nam_cb05(105)     , spmh_map_cb05(105)  &
    / 'decanal         ', 87            /
DATA  mech_nam_cb05(105)     , mech_map_cb05(105)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(105) / 8.00          /

DATA  spmh_nam_cb05(106)     , spmh_map_cb05(106)  &
    / 'dodecene_1      ', 88            /
DATA  mech_nam_cb05(106)     , mech_map_cb05(106)  &
    / 'OLE             ', 5             /
DATA  conv_fac_cb05(106) / 1.00          /

DATA  spmh_nam_cb05(107)     , spmh_map_cb05(107)  &
    / 'dodecene_1      ', 88            /
DATA  mech_nam_cb05(107)     , mech_map_cb05(107)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(107) / 10.00         /

DATA  spmh_nam_cb05(108)     , spmh_map_cb05(108)  &
    / 'geranyl_acetone ', 89            /
DATA  mech_nam_cb05(108)     , mech_map_cb05(108)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05(108) / 1.00          /

DATA  spmh_nam_cb05(109)     , spmh_map_cb05(109)  &
    / 'geranyl_acetone ', 89            /
DATA  mech_nam_cb05(109)     , mech_map_cb05(109)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05(109) / 3.00          /

DATA  spmh_nam_cb05(110)     , spmh_map_cb05(110)  &
    / 'heptanal        ', 90            /
DATA  mech_nam_cb05(110)     , mech_map_cb05(110)  &
    / 'ALDX            ', 14            /
DATA  conv_fac_cb05(110) / 1.00          /

DATA  spmh_nam_cb05(111)     , spmh_map_cb05(111)  &
    / 'heptanal        ', 90            /
DATA  mech_nam_cb05(111)     , mech_map_cb05(111)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(111) / 5.00          /

DATA  spmh_nam_cb05(112)     , spmh_map_cb05(112)  &
    / 'heptane         ', 91            /
DATA  mech_nam_cb05(112)     , mech_map_cb05(112)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(112) / 7.00          /

DATA  spmh_nam_cb05(113)     , spmh_map_cb05(113)  &
    / 'hexane          ', 92            /
DATA  mech_nam_cb05(113)     , mech_map_cb05(113)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(113) / 6.00          /

DATA  spmh_nam_cb05(114)     , spmh_map_cb05(114)  &
    / 'met_benzoate    ', 93            /
DATA  mech_nam_cb05(114)     , mech_map_cb05(114)  &
    / 'TOL             ', 15            /
DATA  conv_fac_cb05(114) / 1.00          /

DATA  spmh_nam_cb05(115)     , spmh_map_cb05(115)  &
    / 'met_benzoate    ', 93            /
DATA  mech_nam_cb05(115)     , mech_map_cb05(115)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05(115) / 1.00          /

DATA  spmh_nam_cb05(116)     , spmh_map_cb05(116)  &
    / 'met_heptenone   ', 94            /
DATA  mech_nam_cb05(116)     , mech_map_cb05(116)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(116) / 6.00          /

DATA  spmh_nam_cb05(117)     , spmh_map_cb05(117)  &
    / 'met_heptenone   ', 94            /
DATA  mech_nam_cb05(117)     , mech_map_cb05(117)  &
    / 'ALDX            ', 14            /
DATA  conv_fac_cb05(117) / 1.00          /

DATA  spmh_nam_cb05(118)     , spmh_map_cb05(118)  &
    / 'neryl_acetone   ', 95            /
DATA  mech_nam_cb05(118)     , mech_map_cb05(118)  &
    / 'IOLE            ', 16            /
DATA  conv_fac_cb05(118) / 1.00          /

DATA  spmh_nam_cb05(119)     , spmh_map_cb05(119)  &
    / 'neryl_acetone   ', 95            /
DATA  mech_nam_cb05(119)     , mech_map_cb05(119)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(119) / 8.00          /

DATA  spmh_nam_cb05(120)     , spmh_map_cb05(120)  &
    / 'neryl_acetone   ', 95            /
DATA  mech_nam_cb05(120)     , mech_map_cb05(120)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05(120) / 1.00          /

DATA  spmh_nam_cb05(121)     , spmh_map_cb05(121)  &
    / 'nonanal         ', 96            /
DATA  mech_nam_cb05(121)     , mech_map_cb05(121)  &
    / 'ALDX            ', 14            /
DATA  conv_fac_cb05(121) / 1.00          /

DATA  spmh_nam_cb05(122)     , spmh_map_cb05(122)  &
    / 'nonanal         ', 96            /
DATA  mech_nam_cb05(122)     , mech_map_cb05(122)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(122) / 7.00          /

DATA  spmh_nam_cb05(123)     , spmh_map_cb05(123)  &
    / 'nonenal         ', 97            /
DATA  mech_nam_cb05(123)     , mech_map_cb05(123)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(123) / 3.00          /

DATA  spmh_nam_cb05(124)     , spmh_map_cb05(124)  &
    / 'nonenal         ', 97            /
DATA  mech_nam_cb05(124)     , mech_map_cb05(124)  &
    / 'IOLE            ', 16            /
DATA  conv_fac_cb05(124) / 1.00          /

DATA  spmh_nam_cb05(125)     , spmh_map_cb05(125)  &
    / 'nonenal         ', 97            /
DATA  mech_nam_cb05(125)     , mech_map_cb05(125)  &
    / 'ALDX            ', 14            /
DATA  conv_fac_cb05(125) / 1.00          /

DATA  spmh_nam_cb05(126)     , spmh_map_cb05(126)  &
    / 'octanal         ', 98            /
DATA  mech_nam_cb05(126)     , mech_map_cb05(126)  &
    / 'ALDX            ', 14            /
DATA  conv_fac_cb05(126) / 1.00          /

DATA  spmh_nam_cb05(127)     , spmh_map_cb05(127)  &
    / 'octanal         ', 98            /
DATA  mech_nam_cb05(127)     , mech_map_cb05(127)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(127) / 6.00          /

DATA  spmh_nam_cb05(128)     , spmh_map_cb05(128)  &
    / 'octanol         ', 99            /
DATA  mech_nam_cb05(128)     , mech_map_cb05(128)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(128) / 8.00          /

DATA  spmh_nam_cb05(129)     , spmh_map_cb05(129)  &
    / 'octenol_1e3ol   ', 100           /
DATA  mech_nam_cb05(129)     , mech_map_cb05(129)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(129) / 6.00          /

DATA  spmh_nam_cb05(130)     , spmh_map_cb05(130)  &
    / 'octenol_1e3ol   ', 100           /
DATA  mech_nam_cb05(130)     , mech_map_cb05(130)  &
    / 'OLE             ', 5             /
DATA  conv_fac_cb05(130) / 1.00          /

DATA  spmh_nam_cb05(131)     , spmh_map_cb05(131)  &
    / 'oxopentanal     ', 101           /
DATA  mech_nam_cb05(131)     , mech_map_cb05(131)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(131) / 3.00          /

DATA  spmh_nam_cb05(132)     , spmh_map_cb05(132)  &
    / 'oxopentanal     ', 101           /
DATA  mech_nam_cb05(132)     , mech_map_cb05(132)  &
    / 'ALDX            ', 14            /
DATA  conv_fac_cb05(132) / 1.00          /

DATA  spmh_nam_cb05(133)     , spmh_map_cb05(133)  &
    / 'pentane         ', 102           /
DATA  mech_nam_cb05(133)     , mech_map_cb05(133)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(133) / 5.00          /

DATA  spmh_nam_cb05(134)     , spmh_map_cb05(134)  &
    / 'phenyl_CCO      ', 103           /
DATA  mech_nam_cb05(134)     , mech_map_cb05(134)  &
    / 'TOL             ', 15            /
DATA  conv_fac_cb05(134) / 1.00          /

DATA  spmh_nam_cb05(135)     , spmh_map_cb05(135)  &
    / 'phenyl_CCO      ', 103           /
DATA  mech_nam_cb05(135)     , mech_map_cb05(135)  &
    / 'ALDX            ', 14            /
DATA  conv_fac_cb05(135) / 0.50          /

DATA  spmh_nam_cb05(136)     , spmh_map_cb05(136)  &
    / 'pyruvic_acid    ', 104           /
DATA  mech_nam_cb05(136)     , mech_map_cb05(136)  &
    / 'FORM            ', 13            /
DATA  conv_fac_cb05(136) / 1.00          /

DATA  spmh_nam_cb05(137)     , spmh_map_cb05(137)  &
    / 'pyruvic_acid    ', 104           /
DATA  mech_nam_cb05(137)     , mech_map_cb05(137)  &
    / 'ALDX            ', 14            /
DATA  conv_fac_cb05(137) / 1.00          /

DATA  spmh_nam_cb05(138)     , spmh_map_cb05(138)  &
    / 'terpinyl_ACT_a  ', 105           /
DATA  mech_nam_cb05(138)     , mech_map_cb05(138)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05(138) / 1.00          /

DATA  spmh_nam_cb05(139)     , spmh_map_cb05(139)  &
    / 'terpinyl_ACT_a  ', 105           /
DATA  mech_nam_cb05(139)     , mech_map_cb05(139)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05(139) / 2.00          /

DATA  spmh_nam_cb05(140)     , spmh_map_cb05(140)  &
    / 'tetradecene_1   ', 106           /
DATA  mech_nam_cb05(140)     , mech_map_cb05(140)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(140) / 12.00         /

DATA  spmh_nam_cb05(141)     , spmh_map_cb05(141)  &
    / 'tetradecene_1   ', 106           /
DATA  mech_nam_cb05(141)     , mech_map_cb05(141)  &
    / 'OLE             ', 5             /
DATA  conv_fac_cb05(141) / 1.00          /

DATA  spmh_nam_cb05(142)     , spmh_map_cb05(142)  &
    / 'toluene         ', 107           /
DATA  mech_nam_cb05(142)     , mech_map_cb05(142)  &
    / 'TOL             ', 15            /
DATA  conv_fac_cb05(142) / 1.00          /

DATA  spmh_nam_cb05(143)     , spmh_map_cb05(143)  &
    / 'carbon_monoxide ', 108           /
DATA  mech_nam_cb05(143)     , mech_map_cb05(143)  &
    / 'CO              ', 17            /
DATA  conv_fac_cb05(143) / 1.00          /

DATA  spmh_nam_cb05(144)     , spmh_map_cb05(144)  &
    / 'butene          ', 109           /
DATA  mech_nam_cb05(144)     , mech_map_cb05(144)  &
    / 'OLE             ', 5             /
DATA  conv_fac_cb05(144) / 1.00          /

DATA  spmh_nam_cb05(145)     , spmh_map_cb05(145)  &
    / 'butene          ', 109           /
DATA  mech_nam_cb05(145)     , mech_map_cb05(145)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(145) / 2.00          /

DATA  spmh_nam_cb05(146)     , spmh_map_cb05(146)  &
    / 'ethane          ', 110           /
DATA  mech_nam_cb05(146)     , mech_map_cb05(146)  &
    / 'ETHA            ', 18            /
DATA  conv_fac_cb05(146) / 1.00          /

DATA  spmh_nam_cb05(147)     , spmh_map_cb05(147)  &
    / 'ethene          ', 111           /
DATA  mech_nam_cb05(147)     , mech_map_cb05(147)  &
    / 'ETH             ', 19            /
DATA  conv_fac_cb05(147) / 1.00          /

DATA  spmh_nam_cb05(148)     , spmh_map_cb05(148)  &
    / 'propane         ', 113           /
DATA  mech_nam_cb05(148)     , mech_map_cb05(148)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(148) / 1.50          /

DATA  spmh_nam_cb05(149)     , spmh_map_cb05(149)  &
    / 'propane         ', 113           /
DATA  mech_nam_cb05(149)     , mech_map_cb05(149)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05(149) / 1.50          /

DATA  spmh_nam_cb05(150)     , spmh_map_cb05(150)  &
    / 'propene         ', 114           /
DATA  mech_nam_cb05(150)     , mech_map_cb05(150)  &
    / 'OLE             ', 5             /
DATA  conv_fac_cb05(150) / 1.00          /

DATA  spmh_nam_cb05(151)     , spmh_map_cb05(151)  &
    / 'propene         ', 114           /
DATA  mech_nam_cb05(151)     , mech_map_cb05(151)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(151) / 1.00          /

DATA  spmh_nam_cb05(152)     , spmh_map_cb05(152)  &
    / 'diallyl_2s      ', 117           /
DATA  mech_nam_cb05(152)     , mech_map_cb05(152)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(152) / 2.00          /

DATA  spmh_nam_cb05(153)     , spmh_map_cb05(153)  &
    / 'diallyl_2s      ', 117           /
DATA  mech_nam_cb05(153)     , mech_map_cb05(153)  &
    / 'OLE             ', 5             /
DATA  conv_fac_cb05(153) / 2.00          /

DATA  spmh_nam_cb05(154)     , spmh_map_cb05(154)  &
    / '2met_2s         ', 118           /
DATA  mech_nam_cb05(154)     , mech_map_cb05(154)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(154) / 2.00          /

DATA  spmh_nam_cb05(155)     , spmh_map_cb05(155)  &
    / '2met_s          ', 119           /
DATA  mech_nam_cb05(155)     , mech_map_cb05(155)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(155) / 2.00          /

DATA  spmh_nam_cb05(156)     , spmh_map_cb05(156)  &
    / 'met_chloride    ', 120           /
DATA  mech_nam_cb05(156)     , mech_map_cb05(156)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05(156) / 1.00          /

DATA  spmh_nam_cb05(157)     , spmh_map_cb05(157)  &
    / 'met_bromide     ', 121           /
DATA  mech_nam_cb05(157)     , mech_map_cb05(157)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05(157) / 1.00          /

DATA  spmh_nam_cb05(158)     , spmh_map_cb05(158)  &
    / 'met_iodide      ', 122           /
DATA  mech_nam_cb05(158)     , mech_map_cb05(158)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05(158) / 1.00          /

DATA  spmh_nam_cb05(159)     , spmh_map_cb05(159)  &
    / 'met_mercaptan   ', 124           /
DATA  mech_nam_cb05(159)     , mech_map_cb05(159)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(159) / 1.00          /

DATA  spmh_nam_cb05(160)     , spmh_map_cb05(160)  &
    / 'met_propenyl_2s ', 125           /
DATA  mech_nam_cb05(160)     , mech_map_cb05(160)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(160) / 2.00          /

DATA  spmh_nam_cb05(161)     , spmh_map_cb05(161)  &
    / 'met_propenyl_2s ', 125           /
DATA  mech_nam_cb05(161)     , mech_map_cb05(161)  &
    / 'OLE             ', 5             /
DATA  conv_fac_cb05(161) / 1.00          /

DATA  spmh_nam_cb05(162)     , spmh_map_cb05(162)  &
    / 'PPPP_2s         ', 126           /
DATA  mech_nam_cb05(162)     , mech_map_cb05(162)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(162) / 4.00          /

DATA  spmh_nam_cb05(163)     , spmh_map_cb05(163)  &
    / 'PPPP_2s         ', 126           /
DATA  mech_nam_cb05(163)     , mech_map_cb05(163)  &
    / 'OLE             ', 5             /
DATA  conv_fac_cb05(163) / 1.00          /

DATA  spmh_nam_cb05(164)     , spmh_map_cb05(164)  &
    / '2met_nonatriene ', 127           /
DATA  mech_nam_cb05(164)     , mech_map_cb05(164)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05(164) / 1.00          /

DATA  spmh_nam_cb05(165)     , spmh_map_cb05(165)  &
    / 'met_salicylate  ', 128           /
DATA  mech_nam_cb05(165)     , mech_map_cb05(165)  &
    / 'TOL             ', 15            /
DATA  conv_fac_cb05(165) / 1.00          /

DATA  spmh_nam_cb05(166)     , spmh_map_cb05(166)  &
    / 'met_salicylate  ', 128           /
DATA  mech_nam_cb05(166)     , mech_map_cb05(166)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05(166) / 1.00          /

DATA  spmh_nam_cb05(167)     , spmh_map_cb05(167)  &
    / 'indole          ', 129           /
DATA  mech_nam_cb05(167)     , mech_map_cb05(167)  &
    / 'TOL             ', 15            /
DATA  conv_fac_cb05(167) / 1.00          /

DATA  spmh_nam_cb05(168)     , spmh_map_cb05(168)  &
    / 'indole          ', 129           /
DATA  mech_nam_cb05(168)     , mech_map_cb05(168)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05(168) / 1.00          /

DATA  spmh_nam_cb05(169)     , spmh_map_cb05(169)  &
    / 'jasmone         ', 130           /
DATA  mech_nam_cb05(169)     , mech_map_cb05(169)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05(169) / 1.00          /

DATA  spmh_nam_cb05(170)     , spmh_map_cb05(170)  &
    / 'jasmone         ', 130           /
DATA  mech_nam_cb05(170)     , mech_map_cb05(170)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05(170) / 1.00          /

DATA  spmh_nam_cb05(171)     , spmh_map_cb05(171)  &
    / 'met_jasmonate   ', 131           /
DATA  mech_nam_cb05(171)     , mech_map_cb05(171)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05(171) / 1.00          /

DATA  spmh_nam_cb05(172)     , spmh_map_cb05(172)  &
    / 'met_jasmonate   ', 131           /
DATA  mech_nam_cb05(172)     , mech_map_cb05(172)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05(172) / 3.00          /

DATA  spmh_nam_cb05(173)     , spmh_map_cb05(173)  &
    / '3met_3DCTT      ', 132           /
DATA  mech_nam_cb05(173)     , mech_map_cb05(173)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05(173) / 16.00         /

DATA  spmh_nam_cb05(174)     , spmh_map_cb05(174)  &
    / 'hexanal         ', 133           /
DATA  mech_nam_cb05(174)     , mech_map_cb05(174)  &
    / 'ALDX            ', 14            /
DATA  conv_fac_cb05(174) / 1.00          /

DATA  spmh_nam_cb05(175)     , spmh_map_cb05(175)  &
    / 'hexanal         ', 133           /
DATA  mech_nam_cb05(175)     , mech_map_cb05(175)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(175) / 4.00          /

DATA  spmh_nam_cb05(176)     , spmh_map_cb05(176)  &
    / 'hexanol_1       ', 134           /
DATA  mech_nam_cb05(176)     , mech_map_cb05(176)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(176) / 6.00          /

DATA  spmh_nam_cb05(177)     , spmh_map_cb05(177)  &
    / 'hexenal_c3      ', 135           /
DATA  mech_nam_cb05(177)     , mech_map_cb05(177)  &
    / 'IOLE            ', 16            /
DATA  conv_fac_cb05(177) / 1.00          /

DATA  spmh_nam_cb05(178)     , spmh_map_cb05(178)  &
    / 'hexenal_c3      ', 135           /
DATA  mech_nam_cb05(178)     , mech_map_cb05(178)  &
    / 'ALDX            ', 14            /
DATA  conv_fac_cb05(178) / 1.00          /

DATA  spmh_nam_cb05(179)     , spmh_map_cb05(179)  &
    / 'hexenal_t2      ', 136           /
DATA  mech_nam_cb05(179)     , mech_map_cb05(179)  &
    / 'IOLE            ', 16            /
DATA  conv_fac_cb05(179) / 1.00          /

DATA  spmh_nam_cb05(180)     , spmh_map_cb05(180)  &
    / 'hexenal_t2      ', 136           /
DATA  mech_nam_cb05(180)     , mech_map_cb05(180)  &
    / 'ALDX            ', 14            /
DATA  conv_fac_cb05(180) / 1.00          /

DATA  spmh_nam_cb05(181)     , spmh_map_cb05(181)  &
    / 'hexenol_c3      ', 137           /
DATA  mech_nam_cb05(181)     , mech_map_cb05(181)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(181) / 2.00          /

DATA  spmh_nam_cb05(182)     , spmh_map_cb05(182)  &
    / 'hexenol_c3      ', 137           /
DATA  mech_nam_cb05(182)     , mech_map_cb05(182)  &
    / 'IOLE            ', 16            /
DATA  conv_fac_cb05(182) / 1.00          /

DATA  spmh_nam_cb05(183)     , spmh_map_cb05(183)  &
    / 'hexenyl_ACT_c3  ', 138           /
DATA  mech_nam_cb05(183)     , mech_map_cb05(183)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(183) / 3.00          /

DATA  spmh_nam_cb05(184)     , spmh_map_cb05(184)  &
    / 'hexenyl_ACT_c3  ', 138           /
DATA  mech_nam_cb05(184)     , mech_map_cb05(184)  &
    / 'IOLE            ', 16            /
DATA  conv_fac_cb05(184) / 1.00          /

DATA  spmh_nam_cb05(185)     , spmh_map_cb05(185)  &
    / 'hexenyl_ACT_c3  ', 138           /
DATA  mech_nam_cb05(185)     , mech_map_cb05(185)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05(185) / 1.00          /

DATA  spmh_nam_cb05(186)     , spmh_map_cb05(186)  &
    / 'homosalate      ', 139           /
DATA  mech_nam_cb05(186)     , mech_map_cb05(186)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05(186) / 1.00          /

DATA  spmh_nam_cb05(187)     , spmh_map_cb05(187)  &
    / 'homosalate      ', 139           /
DATA  mech_nam_cb05(187)     , mech_map_cb05(187)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05(187) / 3.00          /

DATA  spmh_nam_cb05(188)     , spmh_map_cb05(188)  &
    / 'Ehsalate        ', 140           /
DATA  mech_nam_cb05(188)     , mech_map_cb05(188)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05(188) / 1.00          /

DATA  spmh_nam_cb05(189)     , spmh_map_cb05(189)  &
    / 'Ehsalate        ', 140           /
DATA  mech_nam_cb05(189)     , mech_map_cb05(189)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05(189) / 3.00          /

DATA  spmh_nam_cb05(190)     , spmh_map_cb05(190)  &
    / 'pentanal        ', 141           /
DATA  mech_nam_cb05(190)     , mech_map_cb05(190)  &
    / 'ALDX            ', 14            /
DATA  conv_fac_cb05(190) / 1.00          /

DATA  spmh_nam_cb05(191)     , spmh_map_cb05(191)  &
    / 'pentanal        ', 141           /
DATA  mech_nam_cb05(191)     , mech_map_cb05(191)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(191) / 3.00          /

DATA  spmh_nam_cb05(192)     , spmh_map_cb05(192)  &
    / 'heptanone       ', 142           /
DATA  mech_nam_cb05(192)     , mech_map_cb05(192)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(192) / 7.00          /

DATA  spmh_nam_cb05(193)     , spmh_map_cb05(193)  &
    / 'anisole         ', 143           /
DATA  mech_nam_cb05(193)     , mech_map_cb05(193)  &
    / 'TOL             ', 15            /
DATA  conv_fac_cb05(193) / 1.00          /

DATA  spmh_nam_cb05(194)     , spmh_map_cb05(194)  &
    / 'verbenene       ', 144           /
DATA  mech_nam_cb05(194)     , mech_map_cb05(194)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05(194) / 1.00          /

DATA  spmh_nam_cb05(195)     , spmh_map_cb05(195)  &
    / 'benzyl-acetate  ', 145           /
DATA  mech_nam_cb05(195)     , mech_map_cb05(195)  &
    / 'TOL             ', 15            /
DATA  conv_fac_cb05(195) / 1.00          /

DATA  spmh_nam_cb05(196)     , spmh_map_cb05(196)  &
    / 'benzyl-acetate  ', 145           /
DATA  mech_nam_cb05(196)     , mech_map_cb05(196)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(196) / 1.00          /

DATA  spmh_nam_cb05(197)     , spmh_map_cb05(197)  &
    / 'benzyl-acetate  ', 145           /
DATA  mech_nam_cb05(197)     , mech_map_cb05(197)  &
    / 'NR              ', 6             /
DATA  conv_fac_cb05(197) / 1.00          /

DATA  spmh_nam_cb05(198)     , spmh_map_cb05(198)  &
    / 'myrtenal        ', 146           /
DATA  mech_nam_cb05(198)     , mech_map_cb05(198)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05(198) / 1.00          /

DATA  spmh_nam_cb05(199)     , spmh_map_cb05(199)  &
    / 'benzyl-alcohol  ', 147           /
DATA  mech_nam_cb05(199)     , mech_map_cb05(199)  &
    / 'TOL             ', 15            /
DATA  conv_fac_cb05(199) / 1.00          /

DATA  spmh_nam_cb05(200)     , spmh_map_cb05(200)  &
    / 'meta-cymenene   ', 148           /
DATA  mech_nam_cb05(200)     , mech_map_cb05(200)  &
    / 'XYL             ', 4             /
DATA  conv_fac_cb05(200) / 1.00          /

DATA  spmh_nam_cb05(201)     , spmh_map_cb05(201)  &
    / 'meta-cymenene   ', 148           /
DATA  mech_nam_cb05(201)     , mech_map_cb05(201)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(201) / 2.00          /

DATA  spmh_nam_cb05(202)     , spmh_map_cb05(202)  &
    / 'ipsenol         ', 149           /
DATA  mech_nam_cb05(202)     , mech_map_cb05(202)  &
    / 'TERP            ', 2             /
DATA  conv_fac_cb05(202) / 1.00          /

DATA  spmh_nam_cb05(203)     , spmh_map_cb05(203)  &
    / 'Napthalene      ', 150           /
DATA  mech_nam_cb05(203)     , mech_map_cb05(203)  &
    / 'XYL             ', 4             /
DATA  conv_fac_cb05(203) / 1.00          /

DATA  spmh_nam_cb05(204)     , spmh_map_cb05(204)  &
    / 'Napthalene      ', 150           /
DATA  mech_nam_cb05(204)     , mech_map_cb05(204)  &
    / 'PAR             ', 3             /
DATA  conv_fac_cb05(204) / 2.00          /

end module map_cv2cb05