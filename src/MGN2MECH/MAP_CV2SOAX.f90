module map_cv2soax
!=======================================================================
 
!  MAP_SOAX_CV2SOAX.EXT
!  This include file contains conversion table for 138 speciated species
!  to SOA (CAMx) species


!  MEGAN v2.10
!  INPUT version x.x

!  History:
!  Who          When       What
!  ---------------------------------------------------------------------
!  bkoo         04/13/07 - Created
!  bkoo         07/25/07 - Modified speciation
!  Tan          07/18/11 - Updated for MEGANv2.10
!=======================================================================

CHARACTER (LEN=16) :: map_soaxmech
PARAMETER     (map_soaxmech = 'SOAX            ')

INTEGER :: n_soax
PARAMETER     (n_soax =  110)       ! Number of map species

CHARACTER (LEN=16) :: spmh_nam_soax( n_soax )   ! speciated species name
INTEGER :: spmh_map_soax( n_soax )   ! speciated species name
! mapped to SPC_SPCAT.EXT
CHARACTER (LEN=16) :: mech_nam_soax( n_soax )   ! mechanism species
INTEGER :: mech_map_soax( n_soax )   ! mechanism species mapped
! to SPC_CB4Q.EXT
REAL :: conv_fac_soax( n_soax )   ! conversion factor


DATA  spmh_nam_soax(  1)     , spmh_map_soax(  1)  &
    / 'isoprene        ', 1             /
DATA  mech_nam_soax(  1)     , mech_map_soax(  1)  &
    / 'ISP             ', 1             /
DATA  conv_fac_soax(  1) / 1.00          /

DATA  spmh_nam_soax(  2)     , spmh_map_soax(  2)  &
    / 'myrcene         ', 2             /
DATA  mech_nam_soax(  2)     , mech_map_soax(  2)  &
    / 'TRP             ', 2             /
DATA  conv_fac_soax(  2) / 1.00          /

DATA  spmh_nam_soax(  3)     , spmh_map_soax(  3)  &
    / 'sabinene        ', 3             /
DATA  mech_nam_soax(  3)     , mech_map_soax(  3)  &
    / 'TRP             ', 2             /
DATA  conv_fac_soax(  3) / 1.00          /

DATA  spmh_nam_soax(  4)     , spmh_map_soax(  4)  &
    / 'limonene        ', 4             /
DATA  mech_nam_soax(  4)     , mech_map_soax(  4)  &
    / 'TRP             ', 2             /
DATA  conv_fac_soax(  4) / 1.00          /

DATA  spmh_nam_soax(  5)     , spmh_map_soax(  5)  &
    / 'carene_3        ', 5             /
DATA  mech_nam_soax(  5)     , mech_map_soax(  5)  &
    / 'TRP             ', 2             /
DATA  conv_fac_soax(  5) / 1.00          /

DATA  spmh_nam_soax(  6)     , spmh_map_soax(  6)  &
    / 'ocimene_t_b     ', 6             /
DATA  mech_nam_soax(  6)     , mech_map_soax(  6)  &
    / 'TRP             ', 2             /
DATA  conv_fac_soax(  6) / 1.00          /

DATA  spmh_nam_soax(  7)     , spmh_map_soax(  7)  &
    / 'pinene_b        ', 7             /
DATA  mech_nam_soax(  7)     , mech_map_soax(  7)  &
    / 'TRP             ', 2             /
DATA  conv_fac_soax(  7) / 1.00          /

DATA  spmh_nam_soax(  8)     , spmh_map_soax(  8)  &
    / 'pinene_a        ', 8             /
DATA  mech_nam_soax(  8)     , mech_map_soax(  8)  &
    / 'TRP             ', 2             /
DATA  conv_fac_soax(  8) / 1.00          /

DATA  spmh_nam_soax(  9)     , spmh_map_soax(  9)  &
    / '2met_styrene    ', 9             /
DATA  mech_nam_soax(  9)     , mech_map_soax(  9)  &
    / 'XYLA            ', 3             /
DATA  conv_fac_soax(  9) / 1.00          /

DATA  spmh_nam_soax( 10)     , spmh_map_soax( 10)  &
    / 'cymene_p        ', 10            /
DATA  mech_nam_soax( 10)     , mech_map_soax( 10)  &
    / 'XYLA            ', 3             /
DATA  conv_fac_soax( 10) / 1.00          /

DATA  spmh_nam_soax( 11)     , spmh_map_soax( 11)  &
    / 'cymene_o        ', 11            /
DATA  mech_nam_soax( 11)     , mech_map_soax( 11)  &
    / 'XYLA            ', 3             /
DATA  conv_fac_soax( 11) / 1.00          /

DATA  spmh_nam_soax( 12)     , spmh_map_soax( 12)  &
    / 'phellandrene_a  ', 12            /
DATA  mech_nam_soax( 12)     , mech_map_soax( 12)  &
    / 'TRP             ', 2             /
DATA  conv_fac_soax( 12) / 1.00          /

DATA  spmh_nam_soax( 13)     , spmh_map_soax( 13)  &
    / 'thujene_a       ', 13            /
DATA  mech_nam_soax( 13)     , mech_map_soax( 13)  &
    / 'TRP             ', 2             /
DATA  conv_fac_soax( 13) / 1.00          /

DATA  spmh_nam_soax( 14)     , spmh_map_soax( 14)  &
    / 'terpinene_a     ', 14            /
DATA  mech_nam_soax( 14)     , mech_map_soax( 14)  &
    / 'TRP             ', 2             /
DATA  conv_fac_soax( 14) / 1.00          /

DATA  spmh_nam_soax( 15)     , spmh_map_soax( 15)  &
    / 'terpinene_g     ', 15            /
DATA  mech_nam_soax( 15)     , mech_map_soax( 15)  &
    / 'TRP             ', 2             /
DATA  conv_fac_soax( 15) / 1.00          /

DATA  spmh_nam_soax( 16)     , spmh_map_soax( 16)  &
    / 'terpinolene     ', 16            /
DATA  mech_nam_soax( 16)     , mech_map_soax( 16)  &
    / 'TRP             ', 2             /
DATA  conv_fac_soax( 16) / 1.00          /

DATA  spmh_nam_soax( 17)     , spmh_map_soax( 17)  &
    / 'phellandrene_b  ', 17            /
DATA  mech_nam_soax( 17)     , mech_map_soax( 17)  &
    / 'TRP             ', 2             /
DATA  conv_fac_soax( 17) / 1.00          /

DATA  spmh_nam_soax( 18)     , spmh_map_soax( 18)  &
    / 'camphene        ', 18            /
DATA  mech_nam_soax( 18)     , mech_map_soax( 18)  &
    / 'TRP             ', 2             /
DATA  conv_fac_soax( 18) / 1.00          /

DATA  spmh_nam_soax( 19)     , spmh_map_soax( 19)  &
    / 'bornene         ', 19            /
DATA  mech_nam_soax( 19)     , mech_map_soax( 19)  &
    / 'TRP             ', 2             /
DATA  conv_fac_soax( 19) / 1.00          /

DATA  spmh_nam_soax( 20)     , spmh_map_soax( 20)  &
    / 'fenchene_a      ', 20            /
DATA  mech_nam_soax( 20)     , mech_map_soax( 20)  &
    / 'TRP             ', 2             /
DATA  conv_fac_soax( 20) / 1.00          /

DATA  spmh_nam_soax( 21)     , spmh_map_soax( 21)  &
    / 'ocimene_al      ', 21            /
DATA  mech_nam_soax( 21)     , mech_map_soax( 21)  &
    / 'TRP             ', 2             /
DATA  conv_fac_soax( 21) / 1.00          /

DATA  spmh_nam_soax( 22)     , spmh_map_soax( 22)  &
    / 'ocimene_c_b     ', 22            /
DATA  mech_nam_soax( 22)     , mech_map_soax( 22)  &
    / 'TRP             ', 2             /
DATA  conv_fac_soax( 22) / 1.00          /

DATA  spmh_nam_soax( 23)     , spmh_map_soax( 23)  &
    / 'estragole       ', 24            /
DATA  mech_nam_soax( 23)     , mech_map_soax( 23)  &
    / 'TRP             ', 2             /
DATA  conv_fac_soax( 23) / 1.00          /

DATA  spmh_nam_soax( 24)     , spmh_map_soax( 24)  &
    / 'camphor         ', 25            /
DATA  mech_nam_soax( 24)     , mech_map_soax( 24)  &
    / 'CG5             ', 4             /
DATA  conv_fac_soax( 24) / 0.85          /

DATA  spmh_nam_soax( 25)     , spmh_map_soax( 25)  &
    / 'fenchone        ', 26            /
DATA  mech_nam_soax( 25)     , mech_map_soax( 25)  &
    / 'CG5             ', 4             /
DATA  conv_fac_soax( 25) / 0.85          /

DATA  spmh_nam_soax( 26)     , spmh_map_soax( 26)  &
    / 'piperitone      ', 27            /
DATA  mech_nam_soax( 26)     , mech_map_soax( 26)  &
    / 'TRP             ', 2             /
DATA  conv_fac_soax( 26) / 1.00          /

DATA  spmh_nam_soax( 27)     , spmh_map_soax( 27)  &
    / 'thujone_a       ', 28            /
DATA  mech_nam_soax( 27)     , mech_map_soax( 27)  &
    / 'CG5             ', 4             /
DATA  conv_fac_soax( 27) / 0.85          /

DATA  spmh_nam_soax( 28)     , spmh_map_soax( 28)  &
    / 'thujone_b       ', 29            /
DATA  mech_nam_soax( 28)     , mech_map_soax( 28)  &
    / 'CG5             ', 4             /
DATA  conv_fac_soax( 28) / 0.85          /

DATA  spmh_nam_soax( 29)     , spmh_map_soax( 29)  &
    / 'cineole_1_8     ', 30            /
DATA  mech_nam_soax( 29)     , mech_map_soax( 29)  &
    / 'CG5             ', 4             /
DATA  conv_fac_soax( 29) / 0.86          /

DATA  spmh_nam_soax( 30)     , spmh_map_soax( 30)  &
    / 'borneol         ', 31            /
DATA  mech_nam_soax( 30)     , mech_map_soax( 30)  &
    / 'CG5             ', 4             /
DATA  conv_fac_soax( 30) / 0.86          /

DATA  spmh_nam_soax( 31)     , spmh_map_soax( 31)  &
    / 'linalool        ', 32            /
DATA  mech_nam_soax( 31)     , mech_map_soax( 31)  &
    / 'TRP             ', 2             /
DATA  conv_fac_soax( 31) / 1.00          /

DATA  spmh_nam_soax( 32)     , spmh_map_soax( 32)  &
    / 'terpineol_4     ', 33            /
DATA  mech_nam_soax( 32)     , mech_map_soax( 32)  &
    / 'TRP             ', 2             /
DATA  conv_fac_soax( 32) / 1.00          /

DATA  spmh_nam_soax( 33)     , spmh_map_soax( 33)  &
    / 'terpineol_a     ', 34            /
DATA  mech_nam_soax( 33)     , mech_map_soax( 33)  &
    / 'TRP             ', 2             /
DATA  conv_fac_soax( 33) / 1.00          /

DATA  spmh_nam_soax( 34)     , spmh_map_soax( 34)  &
    / 'linalool_OXD_c  ', 35            /
DATA  mech_nam_soax( 34)     , mech_map_soax( 34)  &
    / 'TRP             ', 2             /
DATA  conv_fac_soax( 34) / 1.00          /

DATA  spmh_nam_soax( 35)     , spmh_map_soax( 35)  &
    / 'linalool_OXD_t  ', 36            /
DATA  mech_nam_soax( 35)     , mech_map_soax( 35)  &
    / 'TRP             ', 2             /
DATA  conv_fac_soax( 35) / 1.00          /

DATA  spmh_nam_soax( 36)     , spmh_map_soax( 36)  &
    / 'ionone_b        ', 37            /
DATA  mech_nam_soax( 36)     , mech_map_soax( 36)  &
    / 'TRP             ', 2             /
DATA  conv_fac_soax( 36) / 1.00          /

DATA  spmh_nam_soax( 37)     , spmh_map_soax( 37)  &
    / 'bornyl_ACT      ', 38            /
DATA  mech_nam_soax( 37)     , mech_map_soax( 37)  &
    / 'CG5             ', 4             /
DATA  conv_fac_soax( 37) / 1.09          /

DATA  spmh_nam_soax( 38)     , spmh_map_soax( 38)  &
    / 'farnescene_a    ', 39            /
DATA  mech_nam_soax( 38)     , mech_map_soax( 38)  &
    / 'SQT             ', 5             /
DATA  conv_fac_soax( 38) / 1.00          /

DATA  spmh_nam_soax( 39)     , spmh_map_soax( 39)  &
    / 'caryophyllene_b ', 40            /
DATA  mech_nam_soax( 39)     , mech_map_soax( 39)  &
    / 'SQT             ', 5             /
DATA  conv_fac_soax( 39) / 1.00          /

DATA  spmh_nam_soax( 40)     , spmh_map_soax( 40)  &
    / 'acoradiene      ', 41            /
DATA  mech_nam_soax( 40)     , mech_map_soax( 40)  &
    / 'SQT             ', 5             /
DATA  conv_fac_soax( 40) / 1.00          /

DATA  spmh_nam_soax( 41)     , spmh_map_soax( 41)  &
    / 'aromadendrene   ', 42            /
DATA  mech_nam_soax( 41)     , mech_map_soax( 41)  &
    / 'SQT             ', 5             /
DATA  conv_fac_soax( 41) / 1.00          /

DATA  spmh_nam_soax( 42)     , spmh_map_soax( 42)  &
    / 'bergamotene_a   ', 43            /
DATA  mech_nam_soax( 42)     , mech_map_soax( 42)  &
    / 'SQT             ', 5             /
DATA  conv_fac_soax( 42) / 1.00          /

DATA  spmh_nam_soax( 43)     , spmh_map_soax( 43)  &
    / 'bergamotene_b   ', 44            /
DATA  mech_nam_soax( 43)     , mech_map_soax( 43)  &
    / 'SQT             ', 5             /
DATA  conv_fac_soax( 43) / 1.00          /

DATA  spmh_nam_soax( 44)     , spmh_map_soax( 44)  &
    / 'bisabolene_a    ', 45            /
DATA  mech_nam_soax( 44)     , mech_map_soax( 44)  &
    / 'SQT             ', 5             /
DATA  conv_fac_soax( 44) / 1.00          /

DATA  spmh_nam_soax( 45)     , spmh_map_soax( 45)  &
    / 'bisabolene_b    ', 46            /
DATA  mech_nam_soax( 45)     , mech_map_soax( 45)  &
    / 'SQT             ', 5             /
DATA  conv_fac_soax( 45) / 1.00          /

DATA  spmh_nam_soax( 46)     , spmh_map_soax( 46)  &
    / 'bourbonene_b    ', 47            /
DATA  mech_nam_soax( 46)     , mech_map_soax( 46)  &
    / 'SQT             ', 5             /
DATA  conv_fac_soax( 46) / 1.00          /

DATA  spmh_nam_soax( 47)     , spmh_map_soax( 47)  &
    / 'cadinene_d      ', 48            /
DATA  mech_nam_soax( 47)     , mech_map_soax( 47)  &
    / 'SQT             ', 5             /
DATA  conv_fac_soax( 47) / 1.00          /

DATA  spmh_nam_soax( 48)     , spmh_map_soax( 48)  &
    / 'cadinene_g      ', 49            /
DATA  mech_nam_soax( 48)     , mech_map_soax( 48)  &
    / 'SQT             ', 5             /
DATA  conv_fac_soax( 48) / 1.00          /

DATA  spmh_nam_soax( 49)     , spmh_map_soax( 49)  &
    / 'cedrene_a       ', 50            /
DATA  mech_nam_soax( 49)     , mech_map_soax( 49)  &
    / 'SQT             ', 5             /
DATA  conv_fac_soax( 49) / 1.00          /

DATA  spmh_nam_soax( 50)     , spmh_map_soax( 50)  &
    / 'copaene_a       ', 51            /
DATA  mech_nam_soax( 50)     , mech_map_soax( 50)  &
    / 'SQT             ', 5             /
DATA  conv_fac_soax( 50) / 1.00          /

DATA  spmh_nam_soax( 51)     , spmh_map_soax( 51)  &
    / 'cubebene_a      ', 52            /
DATA  mech_nam_soax( 51)     , mech_map_soax( 51)  &
    / 'SQT             ', 5             /
DATA  conv_fac_soax( 51) / 1.00          /

DATA  spmh_nam_soax( 52)     , spmh_map_soax( 52)  &
    / 'cubebene_b      ', 53            /
DATA  mech_nam_soax( 52)     , mech_map_soax( 52)  &
    / 'SQT             ', 5             /
DATA  conv_fac_soax( 52) / 1.00          /

DATA  spmh_nam_soax( 53)     , spmh_map_soax( 53)  &
    / 'elemene_b       ', 54            /
DATA  mech_nam_soax( 53)     , mech_map_soax( 53)  &
    / 'SQT             ', 5             /
DATA  conv_fac_soax( 53) / 1.00          /

DATA  spmh_nam_soax( 54)     , spmh_map_soax( 54)  &
    / 'farnescene_b    ', 55            /
DATA  mech_nam_soax( 54)     , mech_map_soax( 54)  &
    / 'SQT             ', 5             /
DATA  conv_fac_soax( 54) / 1.00          /

DATA  spmh_nam_soax( 55)     , spmh_map_soax( 55)  &
    / 'germacrene_B    ', 56            /
DATA  mech_nam_soax( 55)     , mech_map_soax( 55)  &
    / 'SQT             ', 5             /
DATA  conv_fac_soax( 55) / 1.00          /

DATA  spmh_nam_soax( 56)     , spmh_map_soax( 56)  &
    / 'germacrene_D    ', 57            /
DATA  mech_nam_soax( 56)     , mech_map_soax( 56)  &
    / 'SQT             ', 5             /
DATA  conv_fac_soax( 56) / 1.00          /

DATA  spmh_nam_soax( 57)     , spmh_map_soax( 57)  &
    / 'gurjunene_b     ', 58            /
DATA  mech_nam_soax( 57)     , mech_map_soax( 57)  &
    / 'SQT             ', 5             /
DATA  conv_fac_soax( 57) / 1.00          /

DATA  spmh_nam_soax( 58)     , spmh_map_soax( 58)  &
    / 'humulene_a      ', 59            /
DATA  mech_nam_soax( 58)     , mech_map_soax( 58)  &
    / 'SQT             ', 5             /
DATA  conv_fac_soax( 58) / 1.00          /

DATA  spmh_nam_soax( 59)     , spmh_map_soax( 59)  &
    / 'humulene_g      ', 60            /
DATA  mech_nam_soax( 59)     , mech_map_soax( 59)  &
    / 'SQT             ', 5             /
DATA  conv_fac_soax( 59) / 1.00          /

DATA  spmh_nam_soax( 60)     , spmh_map_soax( 60)  &
    / 'isolongifolene  ', 61            /
DATA  mech_nam_soax( 60)     , mech_map_soax( 60)  &
    / 'SQT             ', 5             /
DATA  conv_fac_soax( 60) / 1.00          /

DATA  spmh_nam_soax( 61)     , spmh_map_soax( 61)  &
    / 'longifolene     ', 62            /
DATA  mech_nam_soax( 61)     , mech_map_soax( 61)  &
    / 'SQT             ', 5             /
DATA  conv_fac_soax( 61) / 1.00          /

DATA  spmh_nam_soax( 62)     , spmh_map_soax( 62)  &
    / 'longipinene     ', 63            /
DATA  mech_nam_soax( 62)     , mech_map_soax( 62)  &
    / 'SQT             ', 5             /
DATA  conv_fac_soax( 62) / 1.00          /

DATA  spmh_nam_soax( 63)     , spmh_map_soax( 63)  &
    / 'muurolene_a     ', 64            /
DATA  mech_nam_soax( 63)     , mech_map_soax( 63)  &
    / 'SQT             ', 5             /
DATA  conv_fac_soax( 63) / 1.00          /

DATA  spmh_nam_soax( 64)     , spmh_map_soax( 64)  &
    / 'muurolene_g     ', 65            /
DATA  mech_nam_soax( 64)     , mech_map_soax( 64)  &
    / 'SQT             ', 5             /
DATA  conv_fac_soax( 64) / 1.00          /

DATA  spmh_nam_soax( 65)     , spmh_map_soax( 65)  &
    / 'selinene_b      ', 66            /
DATA  mech_nam_soax( 65)     , mech_map_soax( 65)  &
    / 'SQT             ', 5             /
DATA  conv_fac_soax( 65) / 1.00          /

DATA  spmh_nam_soax( 66)     , spmh_map_soax( 66)  &
    / 'selinene_d      ', 67            /
DATA  mech_nam_soax( 66)     , mech_map_soax( 66)  &
    / 'SQT             ', 5             /
DATA  conv_fac_soax( 66) / 1.00          /

DATA  spmh_nam_soax( 67)     , spmh_map_soax( 67)  &
    / 'nerolidol_c     ', 68            /
DATA  mech_nam_soax( 67)     , mech_map_soax( 67)  &
    / 'SQT             ', 5             /
DATA  conv_fac_soax( 67) / 1.00          /

DATA  spmh_nam_soax( 68)     , spmh_map_soax( 68)  &
    / 'nerolidol_t     ', 69            /
DATA  mech_nam_soax( 68)     , mech_map_soax( 68)  &
    / 'SQT             ', 5             /
DATA  conv_fac_soax( 68) / 1.00          /

DATA  spmh_nam_soax( 69)     , spmh_map_soax( 69)  &
    / 'cedrol          ', 70            /
DATA  mech_nam_soax( 69)     , mech_map_soax( 69)  &
    / 'CG5             ', 4             /
DATA  conv_fac_soax( 69) / 1.24          /

DATA  spmh_nam_soax( 70)     , spmh_map_soax( 70)  &
    / 'benzaldehyde    ', 85            /
DATA  mech_nam_soax( 70)     , mech_map_soax( 70)  &
    / 'TOLA            ', 6             /
DATA  conv_fac_soax( 70) / 1.00          /

DATA  spmh_nam_soax( 71)     , spmh_map_soax( 71)  &
    / 'decanal         ', 87            /
DATA  mech_nam_soax( 71)     , mech_map_soax( 71)  &
    / 'CG5             ', 4             /
DATA  conv_fac_soax( 71) / 0.87          /

DATA  spmh_nam_soax( 72)     , spmh_map_soax( 72)  &
    / 'geranyl_acetone ', 89            /
DATA  mech_nam_soax( 72)     , mech_map_soax( 72)  &
    / 'TRP             ', 2             /
DATA  conv_fac_soax( 72) / 1.00          /

DATA  spmh_nam_soax( 73)     , spmh_map_soax( 73)  &
    / 'heptanal        ', 90            /
DATA  mech_nam_soax( 73)     , mech_map_soax( 73)  &
    / 'CG6             ', 7             /
DATA  conv_fac_soax( 73) / 0.63          /

DATA  spmh_nam_soax( 74)     , spmh_map_soax( 74)  &
    / 'met_benzoate    ', 93            /
DATA  mech_nam_soax( 74)     , mech_map_soax( 74)  &
    / 'TOLA            ', 6             /
DATA  conv_fac_soax( 74) / 1.00          /

DATA  spmh_nam_soax( 75)     , spmh_map_soax( 75)  &
    / 'met_heptenone   ', 94            /
DATA  mech_nam_soax( 75)     , mech_map_soax( 75)  &
    / 'CG6             ', 7             /
DATA  conv_fac_soax( 75) / 0.70          /

DATA  spmh_nam_soax( 76)     , spmh_map_soax( 76)  &
    / 'neryl_acetone   ', 95            /
DATA  mech_nam_soax( 76)     , mech_map_soax( 76)  &
    / 'CG5             ', 4             /
DATA  conv_fac_soax( 76) / 1.08          /

DATA  spmh_nam_soax( 77)     , spmh_map_soax( 77)  &
    / 'nonanal         ', 96            /
DATA  mech_nam_soax( 77)     , mech_map_soax( 77)  &
    / 'CG6             ', 7             /
DATA  conv_fac_soax( 77) / 0.79          /

DATA  spmh_nam_soax( 78)     , spmh_map_soax( 78)  &
    / 'nonenal         ', 97            /
DATA  mech_nam_soax( 78)     , mech_map_soax( 78)  &
    / 'CG6             ', 7             /
DATA  conv_fac_soax( 78) / 0.78          /

DATA  spmh_nam_soax( 79)     , spmh_map_soax( 79)  &
    / 'octanal         ', 98            /
DATA  mech_nam_soax( 79)     , mech_map_soax( 79)  &
    / 'CG6             ', 7             /
DATA  conv_fac_soax( 79) / 0.71          /

DATA  spmh_nam_soax( 80)     , spmh_map_soax( 80)  &
    / 'octanol         ', 99            /
DATA  mech_nam_soax( 80)     , mech_map_soax( 80)  &
    / 'CG6             ', 7             /
DATA  conv_fac_soax( 80) / 0.72          /

DATA  spmh_nam_soax( 81)     , spmh_map_soax( 81)  &
    / 'octenol_1e3ol   ', 100           /
DATA  mech_nam_soax( 81)     , mech_map_soax( 81)  &
    / 'CG6             ', 7             /
DATA  conv_fac_soax( 81) / 0.71          /

DATA  spmh_nam_soax( 82)     , spmh_map_soax( 82)  &
    / 'oxopentanal     ', 101           /
DATA  mech_nam_soax( 82)     , mech_map_soax( 82)  &
    / 'CG4             ', 8             /
DATA  conv_fac_soax( 82) / 0.77          /

DATA  spmh_nam_soax( 83)     , spmh_map_soax( 83)  &
    / 'phenyl_CCO      ', 103           /
DATA  mech_nam_soax( 83)     , mech_map_soax( 83)  &
    / 'TOLA            ', 6             /
DATA  conv_fac_soax( 83) / 1.00          /

DATA  spmh_nam_soax( 84)     , spmh_map_soax( 84)  &
    / 'pyruvic_acid    ', 104           /
DATA  mech_nam_soax( 84)     , mech_map_soax( 84)  &
    / 'CG4             ', 8             /
DATA  conv_fac_soax( 84) / 0.68          /

DATA  spmh_nam_soax( 85)     , spmh_map_soax( 85)  &
    / 'terpinyl_ACT_a  ', 105           /
DATA  mech_nam_soax( 85)     , mech_map_soax( 85)  &
    / 'TRP             ', 2             /
DATA  conv_fac_soax( 85) / 1.00          /

DATA  spmh_nam_soax( 86)     , spmh_map_soax( 86)  &
    / 'toluene         ', 107           /
DATA  mech_nam_soax( 86)     , mech_map_soax( 86)  &
    / 'TOLA            ', 6             /
DATA  conv_fac_soax( 86) / 1.00          /

DATA  spmh_nam_soax( 87)     , spmh_map_soax( 87)  &
    / '2met_nonatriene ', 127           /
DATA  mech_nam_soax( 87)     , mech_map_soax( 87)  &
    / 'TRP             ', 2             /
DATA  conv_fac_soax( 87) / 1.00          /

DATA  spmh_nam_soax( 88)     , spmh_map_soax( 88)  &
    / 'met_salicylate  ', 128           /
DATA  mech_nam_soax( 88)     , mech_map_soax( 88)  &
    / 'TOLA            ', 6             /
DATA  conv_fac_soax( 88) / 1.00          /

DATA  spmh_nam_soax( 89)     , spmh_map_soax( 89)  &
    / 'indole          ', 129           /
DATA  mech_nam_soax( 89)     , mech_map_soax( 89)  &
    / 'TOLA            ', 6             /
DATA  conv_fac_soax( 89) / 1.00          /

DATA  spmh_nam_soax( 90)     , spmh_map_soax( 90)  &
    / 'jasmone         ', 130           /
DATA  mech_nam_soax( 90)     , mech_map_soax( 90)  &
    / 'TRP             ', 2             /
DATA  conv_fac_soax( 90) / 1.00          /

DATA  spmh_nam_soax( 91)     , spmh_map_soax( 91)  &
    / 'met_jasmonate   ', 131           /
DATA  mech_nam_soax( 91)     , mech_map_soax( 91)  &
    / 'TRP             ', 2             /
DATA  conv_fac_soax( 91) / 1.00          /

DATA  spmh_nam_soax( 92)     , spmh_map_soax( 92)  &
    / '3met_3DCTT      ', 132           /
DATA  mech_nam_soax( 92)     , mech_map_soax( 92)  &
    / 'SQT             ', 5             /
DATA  conv_fac_soax( 92) / 1.00          /

DATA  spmh_nam_soax( 93)     , spmh_map_soax( 93)  &
    / 'hexanal         ', 133           /
DATA  mech_nam_soax( 93)     , mech_map_soax( 93)  &
    / 'CG4             ', 8             /
DATA  conv_fac_soax( 93) / 0.77          /

DATA  spmh_nam_soax( 94)     , spmh_map_soax( 94)  &
    / 'hexanol_1       ', 134           /
DATA  mech_nam_soax( 94)     , mech_map_soax( 94)  &
    / 'CG4             ', 8             /
DATA  conv_fac_soax( 94) / 0.79          /

DATA  spmh_nam_soax( 95)     , spmh_map_soax( 95)  &
    / 'hexenal_c3      ', 135           /
DATA  mech_nam_soax( 95)     , mech_map_soax( 95)  &
    / 'CG4             ', 8             /
DATA  conv_fac_soax( 95) / 0.76          /

DATA  spmh_nam_soax( 96)     , spmh_map_soax( 96)  &
    / 'hexenal_t2      ', 136           /
DATA  mech_nam_soax( 96)     , mech_map_soax( 96)  &
    / 'CG4             ', 8             /
DATA  conv_fac_soax( 96) / 0.76          /

DATA  spmh_nam_soax( 97)     , spmh_map_soax( 97)  &
    / 'hexenol_c3      ', 137           /
DATA  mech_nam_soax( 97)     , mech_map_soax( 97)  &
    / 'CG4             ', 8             /
DATA  conv_fac_soax( 97) / 0.77          /

DATA  spmh_nam_soax( 98)     , spmh_map_soax( 98)  &
    / 'hexenyl_ACT_c3  ', 138           /
DATA  mech_nam_soax( 98)     , mech_map_soax( 98)  &
    / 'CG6             ', 7             /
DATA  conv_fac_soax( 98) / 0.79          /

DATA  spmh_nam_soax( 99)     , spmh_map_soax( 99)  &
    / 'homosalate      ', 139           /
DATA  mech_nam_soax( 99)     , mech_map_soax( 99)  &
    / 'SQT             ', 5             /
DATA  conv_fac_soax( 99) / 1.00          /

DATA  spmh_nam_soax(100)     , spmh_map_soax(100)  &
    / 'Ehsalate        ', 140           /
DATA  mech_nam_soax(100)     , mech_map_soax(100)  &
    / 'SQT             ', 5             /
DATA  conv_fac_soax(100) / 1.00          /

DATA  spmh_nam_soax(101)     , spmh_map_soax(101)  &
    / 'pentanal        ', 141           /
DATA  mech_nam_soax(101)     , mech_map_soax(101)  &
    / 'CG4             ', 8             /
DATA  conv_fac_soax(101) / 0.77          /

DATA  spmh_nam_soax(102)     , spmh_map_soax(102)  &
    / 'heptanone       ', 142           /
DATA  mech_nam_soax(102)     , mech_map_soax(102)  &
    / 'CG6             ', 7             /
DATA  conv_fac_soax(102) / 0.70          /

DATA  spmh_nam_soax(103)     , spmh_map_soax(103)  &
    / 'anisole         ', 143           /
DATA  mech_nam_soax(103)     , mech_map_soax(103)  &
    / 'TOLA            ', 6             /
DATA  conv_fac_soax(103) / 1.00          /

DATA  spmh_nam_soax(104)     , spmh_map_soax(104)  &
    / 'verbenene       ', 144           /
DATA  mech_nam_soax(104)     , mech_map_soax(104)  &
    / 'TRP             ', 2             /
DATA  conv_fac_soax(104) / 1.00          /

DATA  spmh_nam_soax(105)     , spmh_map_soax(105)  &
    / 'benzyl-acetate  ', 145           /
DATA  mech_nam_soax(105)     , mech_map_soax(105)  &
    / 'TOLA            ', 6             /
DATA  conv_fac_soax(105) / 1.00          /

DATA  spmh_nam_soax(106)     , spmh_map_soax(106)  &
    / 'myrtenal        ', 146           /
DATA  mech_nam_soax(106)     , mech_map_soax(106)  &
    / 'TRP             ', 2             /
DATA  conv_fac_soax(106) / 1.00          /

DATA  spmh_nam_soax(107)     , spmh_map_soax(107)  &
    / 'benzyl-alcohol  ', 147           /
DATA  mech_nam_soax(107)     , mech_map_soax(107)  &
    / 'TOLA            ', 6             /
DATA  conv_fac_soax(107) / 1.00          /

DATA  spmh_nam_soax(108)     , spmh_map_soax(108)  &
    / 'meta-cymenene   ', 148           /
DATA  mech_nam_soax(108)     , mech_map_soax(108)  &
    / 'XYLA            ', 3             /
DATA  conv_fac_soax(108) / 1.00          /

DATA  spmh_nam_soax(109)     , spmh_map_soax(109)  &
    / 'ipsenol         ', 149           /
DATA  mech_nam_soax(109)     , mech_map_soax(109)  &
    / 'TRP             ', 2             /
DATA  conv_fac_soax(109) / 1.00          /

DATA  spmh_nam_soax(110)     , spmh_map_soax(110)  &
    / 'Napthalene      ', 150           /
DATA  mech_nam_soax(110)     , mech_map_soax(110)  &
    / 'XYLA            ', 3             /
DATA  conv_fac_soax(110) / 1.00          /

END module map_cv2soax