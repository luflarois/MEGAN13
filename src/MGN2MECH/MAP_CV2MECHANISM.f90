module map_cv2mechanism
!=======================================================================

!  ALLMECH.EXT
!  This include file contains all number of mechanism species.
!
!
!  Mechanism Name: SAPRCII, SAPRC99, RADM2, RACM, and CBMZ
!  MEGAN v2.1.0
!
!  Created by Tan 12/02/06
!=======================================================================

INTEGER,PARAMETER :: n_saprcii_spc = 31  ! Number of mechanism species
CHARACTER (LEN=26) :: mech_spc_saprcii( n_saprcii_spc )  ! Mechanism species name
REAL :: mech_mwt_saprcii( n_saprcii_spc )  ! Mechanism species mol. wt.

INTEGER,PARAMETER :: n_radm2_spc = 21    ! Number of mechanism species
CHARACTER (LEN=26) :: mech_spc_radm2( n_radm2_spc )  ! Mechanism species name
REAL :: mech_mwt_radm2( n_radm2_spc )  ! Mechanism species mol. wt.

INTEGER,PARAMETER :: n_racm_spc = 23     ! Number of mechanism species
CHARACTER (LEN=26) :: mech_spc_racm( n_racm_spc )  ! Mechanism species name
REAL :: mech_mwt_racm( n_racm_spc )  ! Mechanism species mol. wt.

INTEGER,PARAMETER :: n_cbmz_spc = 23     ! Number of mechanism species
CHARACTER (LEN=26) :: mech_spc_cbmz( n_cbmz_spc )  ! Mechanism species name
REAL :: mech_mwt_cbmz( n_cbmz_spc )  ! Mechanism species mol. wt.

INTEGER,PARAMETER :: n_saprcii = 150        ! Number of map species
CHARACTER (LEN=26) :: spmh_nam_saprcii( n_saprcii )   ! speciated species name
INTEGER :: spmh_map_saprcii( n_saprcii )   ! speciated species name
! mapped to SPC_SPCAT.EXT
CHARACTER (LEN=26) :: mech_nam_saprcii( n_saprcii )   ! mechanism species
INTEGER :: mech_map_saprcii( n_saprcii )   ! mechanism species mapped
! to SPC_SAPRCII.EXT
REAL :: conv_fac_saprcii( n_saprcii )   ! conversion factor


INTEGER,PARAMETER :: n_radm2 = 177        ! Number of map species
CHARACTER (LEN=26) :: spmh_nam_radm2( n_radm2 )   ! speciated species name
INTEGER :: spmh_map_radm2( n_radm2 )   ! speciated species name
! mapped to SPC_SPCAT.EXT
CHARACTER (LEN=26) :: mech_nam_radm2( n_radm2 )   ! mechanism species
INTEGER :: mech_map_radm2( n_radm2 )   ! mechanism species mapped
! to SPC_RADM2.EXT
REAL :: conv_fac_radm2( n_radm2 )   ! conversion factor

INTEGER,PARAMETER :: n_racm = 159        ! Number of map species
CHARACTER (LEN=26) :: spmh_nam_racm( n_racm )   ! speciated species name
INTEGER :: spmh_map_racm( n_racm )   ! speciated species name
! mapped to SPC_SPCAT.EXT
CHARACTER (LEN=26) :: mech_nam_racm( n_racm )   ! mechanism species
INTEGER :: mech_map_racm( n_racm )   ! mechanism species mapped
! to SPC_RACM.EXT
REAL :: conv_fac_racm( n_racm )   ! conversion factor

INTEGER,PARAMETER :: n_cbmz = 185        ! Number of map species
CHARACTER (LEN=26) :: spmh_nam_cbmz( n_cbmz )   ! speciated species name
INTEGER :: spmh_map_cbmz( n_cbmz )   ! speciated species name
! mapped to SPC_SPCAT.EXT
CHARACTER (LEN=26) :: mech_nam_cbmz( n_cbmz )   ! mechanism species
INTEGER :: mech_map_cbmz( n_cbmz )   ! mechanism species mapped
! to SPC_CBMZ.EXT
REAL :: conv_fac_cbmz( n_cbmz )   ! conversion factor

!=======================================================================
!  MAP_CV2SAPRCII.EXT
!  This include file contains conversion table for 134 speciated species
!  to SAPRCII species
!
!
!  MEGAN v2.1.0
!  INPUT version 200
!
!  History:
!  Who          When       What
!  ---------------------------------------------------------------------
!  Tan          12/02/06 - Creates this file
!  Tan          08/14/07 - Move from MEGAN v2.0 to MEGAN v2.1.0 with no update.
!=======================================================================

DATA  spmh_nam_saprcii(  1)   , spmh_map_saprcii(  1)  &
    / 'isoprene        ', 1        /
DATA  mech_nam_saprcii(  1)   , mech_map_saprcii(  1)  &
    / 'ISOPRENE        ', 1              /
DATA  conv_fac_saprcii(  1) / 1.0            /
DATA  spmh_nam_saprcii(  2)     , spmh_map_saprcii(  2)  &
    / 'myrcene         ', 2              /
DATA  mech_nam_saprcii(  2)     , mech_map_saprcii(  2)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprcii(  2) / 1.0            /
DATA  spmh_nam_saprcii(  3)     , spmh_map_saprcii(  3)  &
    / 'sabinene        ', 3              /
DATA  mech_nam_saprcii(  3)     , mech_map_saprcii(  3)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprcii(  3) / 1.0            /
DATA  spmh_nam_saprcii(  4)     , spmh_map_saprcii(  4)  &
    / 'limonene        ', 4              /
DATA  mech_nam_saprcii(  4)     , mech_map_saprcii(  4)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprcii(  4) / 1.0            /
DATA  spmh_nam_saprcii(  5)     , spmh_map_saprcii(  5)  &
    / 'carene_3        ', 5              /
DATA  mech_nam_saprcii(  5)     , mech_map_saprcii(  5)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprcii(  5) / 1.0            /

DATA  spmh_nam_saprcii(  6)     , spmh_map_saprcii(  6)  &
    / 'ocimene_t_b     ', 6              /
DATA  mech_nam_saprcii(  6)     , mech_map_saprcii(  6)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprcii(  6) / 1.0            /
DATA  spmh_nam_saprcii(  7)     , spmh_map_saprcii(  7)  &
    / 'pinene_b        ', 7              /
DATA  mech_nam_saprcii(  7)     , mech_map_saprcii(  7)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprcii(  7) / 1.0            /
DATA  spmh_nam_saprcii(  8)     , spmh_map_saprcii(  8)  &
    / 'pinene_a        ', 8              /
DATA  mech_nam_saprcii(  8)     , mech_map_saprcii(  8)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprcii(  8) / 1.0            /
DATA  spmh_nam_saprcii(  9)     , spmh_map_saprcii(  9)  &
    / 'A_2met_styrene    ', 9              /
DATA  mech_nam_saprcii(  9)     , mech_map_saprcii(  9)  &
    / 'OLE2            ', 29             /
DATA  conv_fac_saprcii(  9) / 1.0            /
DATA  spmh_nam_saprcii( 10)     , spmh_map_saprcii( 10)  &
    / 'cymene_p        ', 10             /
DATA  mech_nam_saprcii( 10)     , mech_map_saprcii( 10)  &
    / 'ARO2            ', 27             /
DATA  conv_fac_saprcii( 10) / 1.0            /
DATA  spmh_nam_saprcii( 11)     , spmh_map_saprcii( 11)  &
    / 'cymene_o        ', 11             /
DATA  mech_nam_saprcii( 11)     , mech_map_saprcii( 11)  &
    / 'ARO2            ', 27             /
DATA  conv_fac_saprcii( 11) / 1.0            /
DATA  spmh_nam_saprcii( 12)     , spmh_map_saprcii( 12)  &
    / 'phellandrene_a  ', 12             /
DATA  mech_nam_saprcii( 12)     , mech_map_saprcii( 12)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprcii( 12) / 1.0            /
DATA  spmh_nam_saprcii( 13)     , spmh_map_saprcii( 13)  &
    / 'thujene_a       ', 13             /
DATA  mech_nam_saprcii( 13)     , mech_map_saprcii( 13)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprcii( 13) / 1.0            /
DATA  spmh_nam_saprcii( 14)     , spmh_map_saprcii( 14)  &
    / 'terpinene_a     ', 14             /
DATA  mech_nam_saprcii( 14)     , mech_map_saprcii( 14)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprcii( 14) / 1.0            /
DATA  spmh_nam_saprcii( 15)     , spmh_map_saprcii( 15)  &
    / 'terpinene_g     ', 15             /
DATA  mech_nam_saprcii( 15)     , mech_map_saprcii( 15)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprcii( 15) / 1.0            /
DATA  spmh_nam_saprcii( 16)     , spmh_map_saprcii( 16)  &
    / 'terpinolene     ', 16             /
DATA  mech_nam_saprcii( 16)     , mech_map_saprcii( 16)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprcii( 16) / 1.0            /
DATA  spmh_nam_saprcii( 17)     , spmh_map_saprcii( 17)  &
    / 'phellandrene_b  ', 17             /
DATA  mech_nam_saprcii( 17)     , mech_map_saprcii( 17)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprcii( 17) / 1.0            /
DATA  spmh_nam_saprcii( 18)     , spmh_map_saprcii( 18)  &
    / 'camphene        ', 18             /
DATA  mech_nam_saprcii( 18)     , mech_map_saprcii( 18)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprcii( 18) / 1.0            /
DATA  spmh_nam_saprcii( 19)     , spmh_map_saprcii( 19)  &
    / 'bornene         ', 19             /
DATA  mech_nam_saprcii( 19)     , mech_map_saprcii( 19)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprcii( 19) / 1.0            /
DATA  spmh_nam_saprcii( 20)     , spmh_map_saprcii( 20)  &
    / 'fenchene_a      ', 20             /
DATA  mech_nam_saprcii( 20)     , mech_map_saprcii( 20)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprcii( 20) / 1.0            /
DATA  spmh_nam_saprcii( 21)     , spmh_map_saprcii( 21)  &
    / 'ocimene_al      ', 21             /
DATA  mech_nam_saprcii( 21)     , mech_map_saprcii( 21)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprcii( 21) / 1.0            /
DATA  spmh_nam_saprcii( 22)     , spmh_map_saprcii( 22)  &
    / 'ocimene_c_b     ', 22             /
DATA  mech_nam_saprcii( 22)     , mech_map_saprcii( 22)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprcii( 22) / 1.0            /
DATA  spmh_nam_saprcii( 23)     , spmh_map_saprcii( 23)  &
    / 'tricyclene      ', 23             /
DATA  mech_nam_saprcii( 23)     , mech_map_saprcii( 23)  &
    / 'ALK5            ', 25             /
DATA  conv_fac_saprcii( 23) / 1.0            /
DATA  spmh_nam_saprcii( 24)     , spmh_map_saprcii( 24)  &
    / 'estragole       ', 24             /
DATA  mech_nam_saprcii( 24)     , mech_map_saprcii( 24)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprcii( 24) / 1.0            /
DATA  spmh_nam_saprcii( 25)     , spmh_map_saprcii( 25)  &
    / 'camphor         ', 25             /
DATA  mech_nam_saprcii( 25)     , mech_map_saprcii( 25)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprcii( 25) / 1.0            /
DATA  spmh_nam_saprcii( 26)     , spmh_map_saprcii( 26)  &
    / 'fenchone        ', 26             /
DATA  mech_nam_saprcii( 26)     , mech_map_saprcii( 26)  &
    / 'ALK5            ', 25             /
DATA  conv_fac_saprcii( 26) / 1.0            /
DATA  spmh_nam_saprcii( 27)     , spmh_map_saprcii( 27)  &
    / 'piperitone      ', 27             /
DATA  mech_nam_saprcii( 27)     , mech_map_saprcii( 27)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprcii( 27) / 1.0            /
DATA  spmh_nam_saprcii( 28)     , spmh_map_saprcii( 28)  &
    / 'thujone_a       ', 28             /
DATA  mech_nam_saprcii( 28)     , mech_map_saprcii( 28)  &
    / 'ALK5            ', 25             /
DATA  conv_fac_saprcii( 28) / 1.0            /
DATA  spmh_nam_saprcii( 29)     , spmh_map_saprcii( 29)  &
    / 'thujone_b       ', 29             /
DATA  mech_nam_saprcii( 29)     , mech_map_saprcii( 29)  &
    / 'ALK5            ', 25             /
DATA  conv_fac_saprcii( 29) / 1.0            /
DATA  spmh_nam_saprcii( 30)     , spmh_map_saprcii( 30)  &
    / 'cineole_1_8     ', 30             /
DATA  mech_nam_saprcii( 30)     , mech_map_saprcii( 30)  &
    / 'ALK5            ', 25             /
DATA  conv_fac_saprcii( 30) / 1.0            /
DATA  spmh_nam_saprcii( 31)     , spmh_map_saprcii( 31)  &
    / 'borneol         ', 31             /
DATA  mech_nam_saprcii( 31)     , mech_map_saprcii( 31)  &
    / 'ALK5            ', 25             /
DATA  conv_fac_saprcii( 31) / 1.0            /
DATA  spmh_nam_saprcii( 32)     , spmh_map_saprcii( 32)  &
    / 'linalool        ', 32             /
DATA  mech_nam_saprcii( 32)     , mech_map_saprcii( 32)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprcii( 32) / 1.0            /
DATA  spmh_nam_saprcii( 33)     , spmh_map_saprcii( 33)  &
    / 'terpineol_4     ', 33             /
DATA  mech_nam_saprcii( 33)     , mech_map_saprcii( 33)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprcii( 33) / 1.0            /
DATA  spmh_nam_saprcii( 34)     , spmh_map_saprcii( 34)  &
    / 'terpineol_a     ', 34             /
DATA  mech_nam_saprcii( 34)     , mech_map_saprcii( 34)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprcii( 34) / 1.0            /
DATA  spmh_nam_saprcii( 35)     , spmh_map_saprcii( 35)  &
    / 'linalool_OXD_c  ', 35             /
DATA  mech_nam_saprcii( 35)     , mech_map_saprcii( 35)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprcii( 35) / 1.0            /
DATA  spmh_nam_saprcii( 36)     , spmh_map_saprcii( 36)  &
    / 'linalool_OXD_t  ', 36             /
DATA  mech_nam_saprcii( 36)     , mech_map_saprcii( 36)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprcii( 36) / 1.0            /
DATA  spmh_nam_saprcii( 37)     , spmh_map_saprcii( 37)  &
    / 'ionone_b        ', 37             /
DATA  mech_nam_saprcii( 37)     , mech_map_saprcii( 37)  &
    / 'SSQT            ', 5              /
DATA  conv_fac_saprcii( 37) / 1.0            /
DATA  spmh_nam_saprcii( 38)     , spmh_map_saprcii( 38)  &
    / 'bornyl_ACT      ', 38             /
DATA  mech_nam_saprcii( 38)     , mech_map_saprcii( 38)  &
    / 'ALK5            ', 25             /
DATA  conv_fac_saprcii( 38) / 1.0            /
DATA  spmh_nam_saprcii( 39)     , spmh_map_saprcii( 39)  &
    / 'farnescene_a    ', 39             /
DATA  mech_nam_saprcii( 39)     , mech_map_saprcii( 39)  &
    / 'AHUMUL          ', 4              /
DATA  conv_fac_saprcii( 39) / 1.0            /
DATA  spmh_nam_saprcii( 40)     , spmh_map_saprcii( 40)  &
    / 'caryophyllene_b ', 40             /
DATA  mech_nam_saprcii( 40)     , mech_map_saprcii( 40)  &
    / 'BCARL           ', 3              /
DATA  conv_fac_saprcii( 40) / 1.0            /
DATA  spmh_nam_saprcii( 41)     , spmh_map_saprcii( 41)  &
    / 'acoradiene      ', 41             /
DATA  mech_nam_saprcii( 41)     , mech_map_saprcii( 41)  &
    / 'SSQT            ', 5              /
DATA  conv_fac_saprcii( 41) / 1.0            /
DATA  spmh_nam_saprcii( 42)     , spmh_map_saprcii( 42)  &
    / 'aromadendrene   ', 42             /
DATA  mech_nam_saprcii( 42)     , mech_map_saprcii( 42)  &
    / 'SSQT            ', 5              /
DATA  conv_fac_saprcii( 42) / 1.0            /
DATA  spmh_nam_saprcii( 43)     , spmh_map_saprcii( 43)  &
    / 'bergamotene_a   ', 43             /
DATA  mech_nam_saprcii( 43)     , mech_map_saprcii( 43)  &
    / 'SSQT            ', 5              /
DATA  conv_fac_saprcii( 43) / 1.0            /
DATA  spmh_nam_saprcii( 44)     , spmh_map_saprcii( 44)  &
    / 'bergamotene_b   ', 44             /
DATA  mech_nam_saprcii( 44)     , mech_map_saprcii( 44)  &
    / 'SSQT            ', 5              /
DATA  conv_fac_saprcii( 44) / 1.0            /
DATA  spmh_nam_saprcii( 45)     , spmh_map_saprcii( 45)  &
    / 'bisabolene_a    ', 45             /
DATA  mech_nam_saprcii( 45)     , mech_map_saprcii( 45)  &
    / 'AHUMUL          ', 4              /
DATA  conv_fac_saprcii( 45) / 1.0            /
DATA  spmh_nam_saprcii( 46)     , spmh_map_saprcii( 46)  &
    / 'bisabolene_b    ', 46             /
DATA  mech_nam_saprcii( 46)     , mech_map_saprcii( 46)  &
    / 'SSQT            ', 5              /
DATA  conv_fac_saprcii( 46) / 1.0            /
DATA  spmh_nam_saprcii( 47)     , spmh_map_saprcii( 47)  &
    / 'bourbonene_b    ', 47             /
DATA  mech_nam_saprcii( 47)     , mech_map_saprcii( 47)  &
    / 'SSQT            ', 5              /
DATA  conv_fac_saprcii( 47) / 1.0            /
DATA  spmh_nam_saprcii( 48)     , spmh_map_saprcii( 48)  &
    / 'cadinene_d      ', 48             /
DATA  mech_nam_saprcii( 48)     , mech_map_saprcii( 48)  &
    / 'SSQT            ', 5              /
DATA  conv_fac_saprcii( 48) / 1.0            /
DATA  spmh_nam_saprcii( 49)     , spmh_map_saprcii( 49)  &
    / 'cadinene_g      ', 49             /
DATA  mech_nam_saprcii( 49)     , mech_map_saprcii( 49)  &
    / 'SSQT            ', 5              /
DATA  conv_fac_saprcii( 49) / 1.0            /
DATA  spmh_nam_saprcii( 50)     , spmh_map_saprcii( 50)  &
    / 'cedrene_a       ', 50             /
DATA  mech_nam_saprcii( 50)     , mech_map_saprcii( 50)  &
    / 'SSQT            ', 5              /
DATA  conv_fac_saprcii( 50) / 1.0            /
DATA  spmh_nam_saprcii( 51)     , spmh_map_saprcii( 51)  &
    / 'copaene_a       ', 51             /
DATA  mech_nam_saprcii( 51)     , mech_map_saprcii( 51)  &
    / 'SSQT            ', 5              /
DATA  conv_fac_saprcii( 51) / 1.0            /
DATA  spmh_nam_saprcii( 52)     , spmh_map_saprcii( 52)  &
    / 'cubebene_a      ', 52             /
DATA  mech_nam_saprcii( 52)     , mech_map_saprcii( 52)  &
    / 'SSQT            ', 5              /
DATA  conv_fac_saprcii( 52) / 1.0            /
DATA  spmh_nam_saprcii( 53)     , spmh_map_saprcii( 53)  &
    / 'cubebene_b      ', 53             /
DATA  mech_nam_saprcii( 53)     , mech_map_saprcii( 53)  &
    / 'SSQT            ', 5              /
DATA  conv_fac_saprcii( 53) / 1.0            /
DATA  spmh_nam_saprcii( 54)     , spmh_map_saprcii( 54)  &
    / 'elemene_b       ', 54             /
DATA  mech_nam_saprcii( 54)     , mech_map_saprcii( 54)  &
    / 'SSQT            ', 5              /
DATA  conv_fac_saprcii( 54) / 1.0            /
DATA  spmh_nam_saprcii( 55)     , spmh_map_saprcii( 55)  &
    / 'farnescene_b    ', 55             /
DATA  mech_nam_saprcii( 55)     , mech_map_saprcii( 55)  &
    / 'AHUMUL          ', 4              /
DATA  conv_fac_saprcii( 55) / 1.0            /
DATA  spmh_nam_saprcii( 56)     , spmh_map_saprcii( 56)  &
    / 'germacrene_B    ', 56             /
DATA  mech_nam_saprcii( 56)     , mech_map_saprcii( 56)  &
    / 'AHUMUL          ', 4              /
DATA  conv_fac_saprcii( 56) / 1.0            /
DATA  spmh_nam_saprcii( 57)     , spmh_map_saprcii( 57)  &
    / 'germacrene_D    ', 57             /
DATA  mech_nam_saprcii( 57)     , mech_map_saprcii( 57)  &
    / 'SSQT            ', 5              /
DATA  conv_fac_saprcii( 57) / 1.0            /
DATA  spmh_nam_saprcii( 58)     , spmh_map_saprcii( 58)  &
    / 'gurjunene_b     ', 58             /
DATA  mech_nam_saprcii( 58)     , mech_map_saprcii( 58)  &
    / 'SSQT            ', 5              /
DATA  conv_fac_saprcii( 58) / 1.0            /
DATA  spmh_nam_saprcii( 59)     , spmh_map_saprcii( 59)  &
    / 'humulene_a      ', 59             /
DATA  mech_nam_saprcii( 59)     , mech_map_saprcii( 59)  &
    / 'AHUMUL          ', 4              /
DATA  conv_fac_saprcii( 59) / 1.0            /
DATA  spmh_nam_saprcii( 60)     , spmh_map_saprcii( 60)  &
    / 'humulene_g      ', 60             /
DATA  mech_nam_saprcii( 60)     , mech_map_saprcii( 60)  &
    / 'AHUMUL          ', 4              /
DATA  conv_fac_saprcii( 60) / 1.0            /
DATA  spmh_nam_saprcii( 61)     , spmh_map_saprcii( 61)  &
    / 'isolongifolene  ', 61             /
DATA  mech_nam_saprcii( 61)     , mech_map_saprcii( 61)  &
    / 'SSQT            ', 5              /
DATA  conv_fac_saprcii( 61) / 1.0            /
DATA  spmh_nam_saprcii( 62)     , spmh_map_saprcii( 62)  &
    / 'longifolene     ', 62             /
DATA  mech_nam_saprcii( 62)     , mech_map_saprcii( 62)  &
    / 'SSQT            ', 5              /
DATA  conv_fac_saprcii( 62) / 1.0            /
DATA  spmh_nam_saprcii( 63)     , spmh_map_saprcii( 63)  &
    / 'longipinene     ', 63             /
DATA  mech_nam_saprcii( 63)     , mech_map_saprcii( 63)  &
    / 'SSQT            ', 5              /
DATA  conv_fac_saprcii( 63) / 1.0            /
DATA  spmh_nam_saprcii( 64)     , spmh_map_saprcii( 64)  &
    / 'muurolene_a     ', 64             /
DATA  mech_nam_saprcii( 64)     , mech_map_saprcii( 64)  &
    / 'SSQT            ', 5              /
DATA  conv_fac_saprcii( 64) / 1.0            /
DATA  spmh_nam_saprcii( 65)     , spmh_map_saprcii( 65)  &
    / 'muurolene_g     ', 65             /
DATA  mech_nam_saprcii( 65)     , mech_map_saprcii( 65)  &
    / 'SSQT            ', 5              /
DATA  conv_fac_saprcii( 65) / 1.0            /
DATA  spmh_nam_saprcii( 66)     , spmh_map_saprcii( 66)  &
    / 'selinene_b      ', 66             /
DATA  mech_nam_saprcii( 66)     , mech_map_saprcii( 66)  &
    / 'SSQT            ', 5              /
DATA  conv_fac_saprcii( 66) / 1.0            /
DATA  spmh_nam_saprcii( 67)     , spmh_map_saprcii( 67)  &
    / 'selinene_d      ', 67             /
DATA  mech_nam_saprcii( 67)     , mech_map_saprcii( 67)  &
    / 'SSQT            ', 5              /
DATA  conv_fac_saprcii( 67) / 1.0            /
DATA  spmh_nam_saprcii( 68)     , spmh_map_saprcii( 68)  &
    / 'nerolidol_c     ', 68             /
DATA  mech_nam_saprcii( 68)     , mech_map_saprcii( 68)  &
    / 'SSQT            ', 5              /
DATA  conv_fac_saprcii( 68) / 1.0            /
DATA  spmh_nam_saprcii( 69)     , spmh_map_saprcii( 69)  &
    / 'nerolidol_t     ', 69             /
DATA  mech_nam_saprcii( 69)     , mech_map_saprcii( 69)  &
    / 'SSQT            ', 5              /
DATA  conv_fac_saprcii( 69) / 1.0            /
DATA  spmh_nam_saprcii( 70)     , spmh_map_saprcii( 70)  &
    / 'cedrol          ', 70             /
DATA  mech_nam_saprcii( 70)     , mech_map_saprcii( 70)  &
    / 'ALK5            ', 25             /
DATA  conv_fac_saprcii( 70) / 1.0            /
DATA  spmh_nam_saprcii( 71)     , spmh_map_saprcii( 71)  &
    / 'MBO_2m3e2ol     ', 71             /
DATA  mech_nam_saprcii( 71)     , mech_map_saprcii( 71)  &
    / 'ISOPRENE        ', 1              /
DATA  conv_fac_saprcii( 71) / 1.0            /
DATA  spmh_nam_saprcii( 72)     , spmh_map_saprcii( 72)  &
    / 'methanol        ', 72             /
DATA  mech_nam_saprcii( 72)     , mech_map_saprcii( 72)  &
    / 'MEOH            ', 6              /
DATA  conv_fac_saprcii( 72) / 1.0            /
DATA  spmh_nam_saprcii( 73)     , spmh_map_saprcii( 73)  &
    / 'acetone         ', 73             /
DATA  mech_nam_saprcii( 73)     , mech_map_saprcii( 73)  &
    / 'ACET            ', 7              /
DATA  conv_fac_saprcii( 73) / 1.0            /
DATA  spmh_nam_saprcii( 74)     , spmh_map_saprcii( 74)  &
    / 'methane         ', 74             /
DATA  mech_nam_saprcii( 74)     , mech_map_saprcii( 74)  &
    / 'CH4             ', 8              /
DATA  conv_fac_saprcii( 74) / 1.0            /
DATA  spmh_nam_saprcii( 75)     , spmh_map_saprcii( 75)  &
    / 'ammonia         ', 75             /
DATA  mech_nam_saprcii( 75)     , mech_map_saprcii( 75)  &
    / 'NH3             ', 11             /
DATA  conv_fac_saprcii( 75) / 1.0            /
DATA  spmh_nam_saprcii( 76)     , spmh_map_saprcii( 76)  &
    / 'nitrous_OXD     ', 76             /
DATA  mech_nam_saprcii( 76)     , mech_map_saprcii( 76)  &
    / 'NONR            ', 31             /
DATA  conv_fac_saprcii( 76) / 1.0            /
DATA  spmh_nam_saprcii( 77)     , spmh_map_saprcii( 77)  &
    / 'nitric_OXD      ', 77             /
DATA  mech_nam_saprcii( 77)     , mech_map_saprcii( 77)  &
    / 'NO              ', 9              /
DATA  conv_fac_saprcii( 77) / 1.0            /
DATA  spmh_nam_saprcii( 78)     , spmh_map_saprcii( 78)  &
    / 'acetaldehyde    ', 78             /
DATA  mech_nam_saprcii( 78)     , mech_map_saprcii( 78)  &
    / 'CCHO            ', 12             /
DATA  conv_fac_saprcii( 78) / 1.0            /
DATA  spmh_nam_saprcii( 79)     , spmh_map_saprcii( 79)  &
    / 'ethanol         ', 79             /
DATA  mech_nam_saprcii( 79)     , mech_map_saprcii( 79)  &
    / 'ALK3            ', 23             /
DATA  conv_fac_saprcii( 79) / 1.0            /
DATA  spmh_nam_saprcii( 80)     , spmh_map_saprcii( 80)  &
    / 'formic_acid     ', 80             /
DATA  mech_nam_saprcii( 80)     , mech_map_saprcii( 80)  &
    / 'HCOOH           ', 13             /
DATA  conv_fac_saprcii( 80) / 1.0            /
DATA  spmh_nam_saprcii( 81)     , spmh_map_saprcii( 81)  &
    / 'formaldehyde    ', 81             /
DATA  mech_nam_saprcii( 81)     , mech_map_saprcii( 81)  &
    / 'HCHO            ', 14             /
DATA  conv_fac_saprcii( 81) / 1.0            /
DATA  spmh_nam_saprcii( 82)     , spmh_map_saprcii( 82)  &
    / 'acetic_acid     ', 82             /
DATA  mech_nam_saprcii( 82)     , mech_map_saprcii( 82)  &
    / 'CCO_OH          ', 15             /
DATA  conv_fac_saprcii( 82) / 1.0            /
DATA  spmh_nam_saprcii( 83)     , spmh_map_saprcii( 83)  &
    / 'MBO_3m2e1ol     ', 83             /
DATA  mech_nam_saprcii( 83)     , mech_map_saprcii( 83)  &
    / 'ISOPRENE        ', 1              /
DATA  conv_fac_saprcii( 83) / 1.0            /
DATA  spmh_nam_saprcii( 84)     , spmh_map_saprcii( 84)  &
    / 'MBO_3m3e1ol     ', 84             /
DATA  mech_nam_saprcii( 84)     , mech_map_saprcii( 84)  &
    / 'ISOPRENE        ', 1              /
DATA  conv_fac_saprcii( 84) / 1.0            /
DATA  spmh_nam_saprcii( 85)     , spmh_map_saprcii( 85)  &
    / 'benzaldehyde    ', 85             /
DATA  mech_nam_saprcii( 85)     , mech_map_saprcii( 85)  &
    / 'BALD            ', 16             /
DATA  conv_fac_saprcii( 85) / 1.0            /
DATA  spmh_nam_saprcii( 86)     , spmh_map_saprcii( 86)  &
    / 'butanone_2      ', 86             /
DATA  mech_nam_saprcii( 86)     , mech_map_saprcii( 86)  &
    / 'MEK             ', 17             /
DATA  conv_fac_saprcii( 86) / 1.0            /
DATA  spmh_nam_saprcii( 87)     , spmh_map_saprcii( 87)  &
    / 'decanal         ', 87             /
DATA  mech_nam_saprcii( 87)     , mech_map_saprcii( 87)  &
    / 'RCHO            ', 30             /
DATA  conv_fac_saprcii( 87) / 1.0            /
DATA  spmh_nam_saprcii( 88)     , spmh_map_saprcii( 88)  &
    / 'dodecene_1      ', 88             /
DATA  mech_nam_saprcii( 88)     , mech_map_saprcii( 88)  &
    / 'OLE1            ', 28             /
DATA  conv_fac_saprcii( 88) / 1.0            /
DATA  spmh_nam_saprcii( 89)     , spmh_map_saprcii( 89)  &
    / 'geranyl_acetone ', 89             /
DATA  mech_nam_saprcii( 89)     , mech_map_saprcii( 89)  &
    / 'SSQT            ', 5              /
DATA  conv_fac_saprcii( 89) / 1.0            /
DATA  spmh_nam_saprcii( 90)     , spmh_map_saprcii( 90)  &
    / 'heptanal        ', 90             /
DATA  mech_nam_saprcii( 90)     , mech_map_saprcii( 90)  &
    / 'RCHO            ', 30             /
DATA  conv_fac_saprcii( 90) / 1.0            /
DATA  spmh_nam_saprcii( 91)     , spmh_map_saprcii( 91)  &
    / 'heptane         ', 91             /
DATA  mech_nam_saprcii( 91)     , mech_map_saprcii( 91)  &
    / 'ALK5            ', 25             /
DATA  conv_fac_saprcii( 91) / 1.0            /
DATA  spmh_nam_saprcii( 92)     , spmh_map_saprcii( 92)  &
    / 'hexane          ', 92             /
DATA  mech_nam_saprcii( 92)     , mech_map_saprcii( 92)  &
    / 'ALK4            ', 24             /
DATA  conv_fac_saprcii( 92) / 1.0            /
DATA  spmh_nam_saprcii( 93)     , spmh_map_saprcii( 93)  &
    / 'met_benzoate    ', 93             /
DATA  mech_nam_saprcii( 93)     , mech_map_saprcii( 93)  &
    / 'ARO1            ', 26             /
DATA  conv_fac_saprcii( 93) / 1.0            /
DATA  spmh_nam_saprcii( 94)     , spmh_map_saprcii( 94)  &
    / 'met_heptenone   ', 94             /
DATA  mech_nam_saprcii( 94)     , mech_map_saprcii( 94)  &
    / 'OLE2            ', 29             /
DATA  conv_fac_saprcii( 94) / 1.0            /
DATA  spmh_nam_saprcii( 95)     , spmh_map_saprcii( 95)  &
    / 'neryl_acetone   ', 95             /
DATA  mech_nam_saprcii( 95)     , mech_map_saprcii( 95)  &
    / 'OLE2            ', 29             /
DATA  conv_fac_saprcii( 95) / 1.0            /
DATA  spmh_nam_saprcii( 96)     , spmh_map_saprcii( 96)  &
    / 'nonanal         ', 96             /
DATA  mech_nam_saprcii( 96)     , mech_map_saprcii( 96)  &
    / 'RCHO            ', 30             /
DATA  conv_fac_saprcii( 96) / 1.0            /
DATA  spmh_nam_saprcii( 97)     , spmh_map_saprcii( 97)  &
    / 'nonenal         ', 97             /
DATA  mech_nam_saprcii( 97)     , mech_map_saprcii( 97)  &
    / 'OLE1            ', 28             /
DATA  conv_fac_saprcii( 97) / 1.0            /
DATA  spmh_nam_saprcii( 98)     , spmh_map_saprcii( 98)  &
    / 'octanal         ', 98             /
DATA  mech_nam_saprcii( 98)     , mech_map_saprcii( 98)  &
    / 'RCHO            ', 30             /
DATA  conv_fac_saprcii( 98) / 1.0            /
DATA  spmh_nam_saprcii( 99)     , spmh_map_saprcii( 99)  &
    / 'octanol         ', 99             /
DATA  mech_nam_saprcii( 99)     , mech_map_saprcii( 99)  &
    / 'ALK5            ', 25             /
DATA  conv_fac_saprcii( 99) / 1.0            /
DATA  spmh_nam_saprcii(100)     , spmh_map_saprcii(100)  &
    / 'octenol_1e3ol   ', 100            /
DATA  mech_nam_saprcii(100)     , mech_map_saprcii(100)  &
    / 'OLE1            ', 28             /
DATA  conv_fac_saprcii(100) / 1.0            /
DATA  spmh_nam_saprcii(101)     , spmh_map_saprcii(101)  &
    / 'oxopentanal     ', 101            /
DATA  mech_nam_saprcii(101)     , mech_map_saprcii(101)  &
    / 'RCHO            ', 30             /
DATA  conv_fac_saprcii(101) / 1.0            /
DATA  spmh_nam_saprcii(102)     , spmh_map_saprcii(102)  &
    / 'pentane         ', 102            /
DATA  mech_nam_saprcii(102)     , mech_map_saprcii(102)  &
    / 'ALK4            ', 24             /
DATA  conv_fac_saprcii(102) / 1.0            /
DATA  spmh_nam_saprcii(103)     , spmh_map_saprcii(103)  &
    / 'phenyl_CCO      ', 103            /
DATA  mech_nam_saprcii(103)     , mech_map_saprcii(103)  &
    / 'ARO1            ', 26             /
DATA  conv_fac_saprcii(103) / 1.0            /
DATA  spmh_nam_saprcii(104)     , spmh_map_saprcii(104)  &
    / 'pyruvic_acid    ', 104            /
DATA  mech_nam_saprcii(104)     , mech_map_saprcii(104)  &
    / 'RCO_OH          ', 18             /
DATA  conv_fac_saprcii(104) / 1.0            /
DATA  spmh_nam_saprcii(105)     , spmh_map_saprcii(105)  &
    / 'terpinyl_ACT_a  ', 105            /
DATA  mech_nam_saprcii(105)     , mech_map_saprcii(105)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprcii(105) / 1.0            /
DATA  spmh_nam_saprcii(106)     , spmh_map_saprcii(106)  &
    / 'tetradecene_1   ', 106            /
DATA  mech_nam_saprcii(106)     , mech_map_saprcii(106)  &
    / 'OLE1            ', 28             /
DATA  conv_fac_saprcii(106) / 1.0            /
DATA  spmh_nam_saprcii(107)     , spmh_map_saprcii(107)  &
    / 'toluene         ', 107            /
DATA  mech_nam_saprcii(107)     , mech_map_saprcii(107)  &
    / 'ARO1            ', 26             /
DATA  conv_fac_saprcii(107) / 1.0            /
DATA  spmh_nam_saprcii(108)     , spmh_map_saprcii(108)  &
    / 'carbon_monoxide ', 108            /
DATA  mech_nam_saprcii(108)     , mech_map_saprcii(108)  &
    / 'CO              ', 19             /
DATA  conv_fac_saprcii(108) / 1.0            /
DATA  spmh_nam_saprcii(109)     , spmh_map_saprcii(109)  &
    / 'butene          ', 109            /
DATA  mech_nam_saprcii(109)     , mech_map_saprcii(109)  &
    / 'OLE1            ', 28             /
DATA  conv_fac_saprcii(109) / 1.0            /
DATA  spmh_nam_saprcii(110)     , spmh_map_saprcii(110)  &
    / 'ethane          ', 110            /
DATA  mech_nam_saprcii(110)     , mech_map_saprcii(110)  &
    / 'ALK1            ', 21             /
DATA  conv_fac_saprcii(110) / 1.0            /
DATA  spmh_nam_saprcii(111)     , spmh_map_saprcii(111)  &
    / 'ethene          ', 111            /
DATA  mech_nam_saprcii(111)     , mech_map_saprcii(111)  &
    / 'ETHENE          ', 20             /
DATA  conv_fac_saprcii(111) / 1.0            /
DATA  spmh_nam_saprcii(112)     , spmh_map_saprcii(112)  &
    / 'hydrogen_cyanide', 112            /
DATA  mech_nam_saprcii(112)     , mech_map_saprcii(112)  &
    / 'NONR            ', 31             /
DATA  conv_fac_saprcii(112) / 1.0            /
DATA  spmh_nam_saprcii(113)     , spmh_map_saprcii(113)  &
    / 'propane         ', 113            /
DATA  mech_nam_saprcii(113)     , mech_map_saprcii(113)  &
    / 'ALK2            ', 22             /
DATA  conv_fac_saprcii(113) / 1.0            /
DATA  spmh_nam_saprcii(114)     , spmh_map_saprcii(114)  &
    / 'propene         ', 114            /
DATA  mech_nam_saprcii(114)     , mech_map_saprcii(114)  &
    / 'OLE1            ', 28             /
DATA  conv_fac_saprcii(114) / 1.0            /
DATA  spmh_nam_saprcii(115)     , spmh_map_saprcii(115)  &
    / 'carbon_2s       ', 115            /
DATA  mech_nam_saprcii(115)     , mech_map_saprcii(115)  &
    / 'NONR            ', 31             /
DATA  conv_fac_saprcii(115) / 1.0            /
DATA  spmh_nam_saprcii(116)     , spmh_map_saprcii(116)  &
    / 'carbonyl_s      ', 116            /
DATA  mech_nam_saprcii(116)     , mech_map_saprcii(116)  &
    / 'NONR            ', 31             /
DATA  conv_fac_saprcii(116) / 1.0            /
DATA  spmh_nam_saprcii(117)     , spmh_map_saprcii(117)  &
    / 'diallyl_2s      ', 117            /
DATA  mech_nam_saprcii(117)     , mech_map_saprcii(117)  &
    / 'OLE1            ', 28             /
DATA  conv_fac_saprcii(117) / 1.0            /
DATA  spmh_nam_saprcii(118)     , spmh_map_saprcii(118)  &
    / 'A_2met_2s         ', 118            /
DATA  mech_nam_saprcii(118)     , mech_map_saprcii(118)  &
    / 'ALK5            ', 25             /
DATA  conv_fac_saprcii(118) / 1.0            /
DATA  spmh_nam_saprcii(119)     , spmh_map_saprcii(119)  &
    / 'A_2met_s          ', 119            /
DATA  mech_nam_saprcii(119)     , mech_map_saprcii(119)  &
    / 'ALK4            ', 24             /
DATA  conv_fac_saprcii(119) / 1.0            /
DATA  spmh_nam_saprcii(120)     , spmh_map_saprcii(120)  &
    / 'met_chloride    ', 120            /
DATA  mech_nam_saprcii(120)     , mech_map_saprcii(120)  &
    / 'NONR            ', 31             /
DATA  conv_fac_saprcii(120) / 1.0            /
DATA  spmh_nam_saprcii(121)     , spmh_map_saprcii(121)  &
    / 'met_bromide     ', 121            /
DATA  mech_nam_saprcii(121)     , mech_map_saprcii(121)  &
    / 'NONR            ', 31             /
DATA  conv_fac_saprcii(121) / 1.0            /
DATA  spmh_nam_saprcii(122)     , spmh_map_saprcii(122)  &
    / 'met_iodide      ', 122            /
DATA  mech_nam_saprcii(122)     , mech_map_saprcii(122)  &
    / 'NONR            ', 31             /
DATA  conv_fac_saprcii(122) / 1.0            /
DATA  spmh_nam_saprcii(123)     , spmh_map_saprcii(123)  &
    / 'hydrogen_s      ', 123            /
DATA  mech_nam_saprcii(123)     , mech_map_saprcii(123)  &
    / 'NONR            ', 31             /
DATA  conv_fac_saprcii(123) / 1.0            /
DATA  spmh_nam_saprcii(124)     , spmh_map_saprcii(124)  &
    / 'met_mercaptan   ', 124            /
DATA  mech_nam_saprcii(124)     , mech_map_saprcii(124)  &
    / 'ALK5            ', 25             /
DATA  conv_fac_saprcii(124) / 1.0            /
DATA  spmh_nam_saprcii(125)     , spmh_map_saprcii(125)  &
    / 'met_propenyl_2s ', 125            /
DATA  mech_nam_saprcii(125)     , mech_map_saprcii(125)  &
    / 'OLE1            ', 28             /
DATA  conv_fac_saprcii(125) / 1.0            /
DATA  spmh_nam_saprcii(126)     , spmh_map_saprcii(126)  &
    / 'PPPP_2s         ', 126            /
DATA  mech_nam_saprcii(126)     , mech_map_saprcii(126)  &
    / 'OLE1            ', 28             /
DATA  conv_fac_saprcii(126) / 1.0            /
DATA  spmh_nam_saprcii(127)     , spmh_map_saprcii(127)  &
    / 'A_2met_nonatriene ', 127            /
DATA  mech_nam_saprcii(127)     , mech_map_saprcii(127)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprcii(127) / 1.0            /
DATA  spmh_nam_saprcii(128)     , spmh_map_saprcii(128)  &
    / 'met_salicylate  ', 128            /
DATA  mech_nam_saprcii(128)     , mech_map_saprcii(128)  &
    / 'ARO1            ', 26             /
DATA  conv_fac_saprcii(128) / 1.0            /
DATA  spmh_nam_saprcii(129)     , spmh_map_saprcii(129)  &
    / 'indole          ', 129            /
DATA  mech_nam_saprcii(129)     , mech_map_saprcii(129)  &
    / 'ARO2            ', 27             /
DATA  conv_fac_saprcii(129) / 1.0            /
DATA  spmh_nam_saprcii(130)     , spmh_map_saprcii(130)  &
    / 'jasmone         ', 130            /
DATA  mech_nam_saprcii(130)     , mech_map_saprcii(130)  &
    / 'TRP1            ', 2              /
DATA  conv_fac_saprcii(130) / 1.0            /
DATA  spmh_nam_saprcii(131)     , spmh_map_saprcii(131)  &
    / 'met_jasmonate   ', 131            /
DATA  mech_nam_saprcii(131)     , mech_map_saprcii(131)  &
    / 'SSQT            ', 5              /
DATA  conv_fac_saprcii(131) / 1.0            /
DATA  spmh_nam_saprcii(132)     , spmh_map_saprcii(132)  &
    / 'A_3met_3DCTT      ', 132            /
DATA  mech_nam_saprcii(132)     , mech_map_saprcii(132)  &
    / 'AHUMUL          ', 4              /
DATA  conv_fac_saprcii(132) / 1.0            /
DATA  spmh_nam_saprcii(133)     , spmh_map_saprcii(133)  &
    / 'hexanal         ', 133            /
DATA  mech_nam_saprcii(133)     , mech_map_saprcii(133)  &
    / 'RCHO            ', 30             /
DATA  conv_fac_saprcii(133) / 1.0            /
DATA  spmh_nam_saprcii(134)     , spmh_map_saprcii(134)  &
    / 'hexanol_1       ', 134            /
DATA  mech_nam_saprcii(134)     , mech_map_saprcii(134)  &
    / 'ALK5            ', 25             /
DATA  conv_fac_saprcii(134) / 1.0            /
DATA  spmh_nam_saprcii(135)     , spmh_map_saprcii(135)  &
    / 'hexenal_c3      ', 135            /
DATA  mech_nam_saprcii(135)     , mech_map_saprcii(135)  &
    / 'OLE2            ', 29             /
DATA  conv_fac_saprcii(135) / 1.0            /
DATA  spmh_nam_saprcii(136)     , spmh_map_saprcii(136)  &
    / 'hexenal_t2      ', 136            /
DATA  mech_nam_saprcii(136)     , mech_map_saprcii(136)  &
    / 'OLE2            ', 29             /
DATA  conv_fac_saprcii(136) / 1.0            /
DATA  spmh_nam_saprcii(137)     , spmh_map_saprcii(137)  &
    / 'hexenol_c3      ', 137            /
DATA  mech_nam_saprcii(137)     , mech_map_saprcii(137)  &
    / 'OLE2            ', 29             /
DATA  conv_fac_saprcii(137) / 1.0            /
DATA  spmh_nam_saprcii(138)     , spmh_map_saprcii(138)  &
    / 'hexenyl_ACT_c3  ', 138            /
DATA  mech_nam_saprcii(138)     , mech_map_saprcii(138)  &
    / 'OLE2            ', 29             /
DATA  conv_fac_saprcii(138) / 1.0            /
DATA  spmh_nam_saprcii(139)     , spmh_map_saprcii(139)  &
    / 'homosalate      ', 139            /
DATA  mech_nam_saprcii(139)     , mech_map_saprcii(139)  &
    / 'SSQT            ', 5             /
DATA  conv_fac_saprcii(139) / 1.0            /
DATA  spmh_nam_saprcii(140)     , spmh_map_saprcii(140)  &
    / 'Ehsalate   ', 140            /
DATA  mech_nam_saprcii(140)     , mech_map_saprcii(140)  &
    / 'SSQT            ', 5             /
DATA  conv_fac_saprcii(140) / 1.0            /
DATA  spmh_nam_saprcii(141)     , spmh_map_saprcii(141)  &
    / 'pentanal     ', 141            /
DATA  mech_nam_saprcii(141)     , mech_map_saprcii(141)  &
    / 'RCHO            ', 30             /
DATA  conv_fac_saprcii(141) / 1.0            /
DATA  spmh_nam_saprcii(142)     , spmh_map_saprcii(142)  &
    / 'heptanone      ', 142            /
DATA  mech_nam_saprcii(142)     , mech_map_saprcii(142)  &
    / 'OLE2           ', 29             /
DATA  conv_fac_saprcii(142) / 1.0            /
DATA  spmh_nam_saprcii(143)     , spmh_map_saprcii(143)  &
    / 'anisole      ', 143            /
DATA  mech_nam_saprcii(143)     , mech_map_saprcii(143)  &
    / 'BALD         ', 16             /
DATA  conv_fac_saprcii(143) / 1.0            /
DATA  spmh_nam_saprcii(144)     , spmh_map_saprcii(144)  &
    / 'verbenene   ', 144            /
DATA  mech_nam_saprcii(144)     , mech_map_saprcii(144)  &
    / 'ARO2            ', 27             /
DATA  conv_fac_saprcii(144) / 1.0            /
DATA  spmh_nam_saprcii(145)     , spmh_map_saprcii(145)  &
    / 'benzyl-acetate ', 145            /
DATA  mech_nam_saprcii(145)     , mech_map_saprcii(145)  &
    / 'BALD            ', 16             /
DATA  conv_fac_saprcii(145) / 1.0            /
DATA  spmh_nam_saprcii(146)     , spmh_map_saprcii(146)  &
    / 'myrtenal         ', 146            /
DATA  mech_nam_saprcii(146)     , mech_map_saprcii(146)  &
    / 'TRP1            ', 2             /
DATA  conv_fac_saprcii(146) / 1.0            /
DATA  spmh_nam_saprcii(147)     , spmh_map_saprcii(147)  &
    / 'benzyl-alcohol ', 147            /
DATA  mech_nam_saprcii(147)     , mech_map_saprcii(147)  &
    / 'BALD            ', 16              /
DATA  conv_fac_saprcii(147) / 1.0            /
DATA  spmh_nam_saprcii(148)     , spmh_map_saprcii(148)  &
    / 'meta-cymenene  ', 148            /
DATA  mech_nam_saprcii(148)     , mech_map_saprcii(148)  &
    / 'ARO2           ', 27              /
DATA  conv_fac_saprcii(148) / 1.0            /
DATA  spmh_nam_saprcii(149)     , spmh_map_saprcii(149)  &
    / 'ipsenol        ', 149            /
DATA  mech_nam_saprcii(149)     , mech_map_saprcii(149)  &
    / 'TRP1           ', 2              /
DATA  conv_fac_saprcii(149) / 1.0            /
DATA  spmh_nam_saprcii(150)     , spmh_map_saprcii(150)  &
    / 'Napthalene     ', 150            /
DATA  mech_nam_saprcii(150)     , mech_map_saprcii(150)  &
    / 'ARO2           ', 27              /
DATA  conv_fac_saprcii(150) / 1.0            /

!=======================================================================
!  SPC_SAPRCII.EXT
!  This include file contains SAPRCII species and their MW.


!  Mechanism Name: SAPRCII
!  MEGAN v2.1.0
!  INPUT version 200

!  History:
!  Who          When       What
!  ---------------------------------------------------------------------
!  Tan          12/02/06 - Creates this file
!  Tan          08/14/07 - Move from MEGAN v2.0 to MEGAN v2.1.0 with no update.
!=======================================================================
! Note conversion between 134 species and SAPRCII is done by 1:1 mole

DATA  mech_spc_saprcii(  1), mech_mwt_saprcii(  1) / 'ISOPRENE', 68.00   /
DATA  mech_spc_saprcii(  2), mech_mwt_saprcii(  2) / 'TRP1    ', 136.00  /
DATA  mech_spc_saprcii(  3), mech_mwt_saprcii(  3) / 'BCARL   ', 204.00  /
DATA  mech_spc_saprcii(  4), mech_mwt_saprcii(  4) / 'AHUMUL  ', 204.00  /
DATA  mech_spc_saprcii(  5), mech_mwt_saprcii(  5) / 'SSQT   ', 204.00  /
DATA  mech_spc_saprcii(  6), mech_mwt_saprcii(  6) / 'MEOH   ', 32.00   /
DATA  mech_spc_saprcii(  7), mech_mwt_saprcii(  7) / 'ACET    ', 58.00   /
DATA  mech_spc_saprcii(  8), mech_mwt_saprcii(  8) / 'CH4    ', 16.00   /
DATA  mech_spc_saprcii(  9), mech_mwt_saprcii(  9) / 'NO     ', 30.00   /
DATA  mech_spc_saprcii( 10), mech_mwt_saprcii( 10) / 'NO2    ', 44.01   /
DATA  mech_spc_saprcii( 11), mech_mwt_saprcii( 11) / 'NH3    ', 17.00   /
DATA  mech_spc_saprcii( 12), mech_mwt_saprcii( 12) / 'CCHO   ', 44.00   /
DATA  mech_spc_saprcii( 13), mech_mwt_saprcii( 13) / 'HCOOH  ', 46.00   /
DATA  mech_spc_saprcii( 14), mech_mwt_saprcii( 14) / 'HCHO   ', 30.00   /
DATA  mech_spc_saprcii( 15), mech_mwt_saprcii( 15) / 'CCO_OH ', 60.00   /
DATA  mech_spc_saprcii( 16), mech_mwt_saprcii( 16) / 'BALD   ', 106.00  /
DATA  mech_spc_saprcii( 17), mech_mwt_saprcii( 17) / 'MEK    ', 72.00   /
DATA  mech_spc_saprcii( 18), mech_mwt_saprcii( 18) / 'RCO_OH ', 74.00   /
DATA  mech_spc_saprcii( 19), mech_mwt_saprcii( 19) / 'CO     ', 28.00   /
DATA  mech_spc_saprcii( 20), mech_mwt_saprcii( 20) / 'ETHENE ', 28.00   /
DATA  mech_spc_saprcii( 21), mech_mwt_saprcii( 21) / 'ALK1   ', 30.10   /
DATA  mech_spc_saprcii( 22), mech_mwt_saprcii( 22) / 'ALK2   ', 36.70   /
DATA  mech_spc_saprcii( 23), mech_mwt_saprcii( 23) / 'ALK3   ', 58.60   /
DATA  mech_spc_saprcii( 24), mech_mwt_saprcii( 24) / 'ALK4   ', 77.60   /
DATA  mech_spc_saprcii( 25), mech_mwt_saprcii( 25) / 'ALK5   ', 118.90  /
DATA  mech_spc_saprcii( 26), mech_mwt_saprcii( 26) / 'ARO1   ', 98.60   /
DATA  mech_spc_saprcii( 27), mech_mwt_saprcii( 27) / 'ARO2   ', 118.70  /
DATA  mech_spc_saprcii( 28), mech_mwt_saprcii( 28) / 'OLE1   ', 72.30   /
DATA  mech_spc_saprcii( 29), mech_mwt_saprcii( 29) / 'OLE2   ', 75.80   /
DATA  mech_spc_saprcii( 30), mech_mwt_saprcii( 30) / 'RCHO  ', 58.00   /
DATA  mech_spc_saprcii( 31), mech_mwt_saprcii( 31) / 'NONR  ', 1.00    /

!=======================================================================
!  MAP_CV2RADM2.EXT
!  This include file contains conversion table for 134 speciated species
!  to RADM2 species


!  MEGAN v2.1.0
!  INPUT version 200

!  History:
!  Who          When       What
!  ---------------------------------------------------------------------
!  Tan          12/02/06 - Creates this file
!  Tan          08/14/07 - Move from MEGAN v2.0 to MEGAN v2.1.0 with no update.
!=======================================================================

DATA  spmh_nam_radm2(  1)     , spmh_map_radm2(  1)  &
    / 'isoprene        ', 1             /
DATA  mech_nam_radm2(  1)     , mech_map_radm2(  1)  &
    / 'ISO             ', 1             /
DATA  conv_fac_radm2(  1) / 1.000         /

DATA  spmh_nam_radm2(  2)     , spmh_map_radm2(  2)  &
    / 'myrcene         ', 2             /
DATA  mech_nam_radm2(  2)     , mech_map_radm2(  2)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2(  2) / 0.5           /

DATA  spmh_nam_radm2(  3)     , spmh_map_radm2(  3)  &
    / 'A_myrcene         ', 2             /
DATA  mech_nam_radm2(  3)     , mech_map_radm2(  3)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2(  3) / 0.5           /

DATA  spmh_nam_radm2(  4)     , spmh_map_radm2(  4)  &
    / 'sabinene        ', 3             /
DATA  mech_nam_radm2(  4)     , mech_map_radm2(  4)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2(  4) / 1.000         /

DATA  spmh_nam_radm2(  5)     , spmh_map_radm2(  5)  &
    / 'limonene        ', 4             /
DATA  mech_nam_radm2(  5)     , mech_map_radm2(  5)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2(  5) / 0.5           /

DATA  spmh_nam_radm2(  6)     , spmh_map_radm2(  6)  &
    / 'A_limonene        ', 4             /
DATA  mech_nam_radm2(  6)     , mech_map_radm2(  6)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2(  6) / 0.5           /

DATA  spmh_nam_radm2(  7)     , spmh_map_radm2(  7)  &
    / 'carene_3        ', 5             /
DATA  mech_nam_radm2(  7)     , mech_map_radm2(  7)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2(  7) / 1.000         /

DATA  spmh_nam_radm2(  8)     , spmh_map_radm2(  8)  &
    / 'ocimene_t_b     ', 6             /
DATA  mech_nam_radm2(  8)     , mech_map_radm2(  8)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2(  8) / 0.5           /

DATA  spmh_nam_radm2(  9)     , spmh_map_radm2(  9)  &
    / 'ocimene_t_b     ', 6             /
DATA  mech_nam_radm2(  9)     , mech_map_radm2(  9)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2(  9) / 0.5           /

DATA  spmh_nam_radm2( 10)     , spmh_map_radm2( 10)  &
    / 'pinene_b        ', 7             /
DATA  mech_nam_radm2( 10)     , mech_map_radm2( 10)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2( 10) / 1.000         /

DATA  spmh_nam_radm2( 11)     , spmh_map_radm2( 11)  &
    / 'pinene_a        ', 8             /
DATA  mech_nam_radm2( 11)     , mech_map_radm2( 11)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 11) / 1.000         /

DATA  spmh_nam_radm2( 12)     , spmh_map_radm2( 12)  &
    / 'A_2met_styrene    ', 9             /
DATA  mech_nam_radm2( 12)     , mech_map_radm2( 12)  &
    / 'TOL             ', 12            /
DATA  conv_fac_radm2( 12) / 1.000         /

DATA  spmh_nam_radm2( 13)     , spmh_map_radm2( 13)  &
    / 'cymene_p        ', 10            /
DATA  mech_nam_radm2( 13)     , mech_map_radm2( 13)  &
    / 'TOL             ', 12            /
DATA  conv_fac_radm2( 13) / 1.000         /

DATA  spmh_nam_radm2( 14)     , spmh_map_radm2( 14)  &
    / 'cymene_o        ', 11            /
DATA  mech_nam_radm2( 14)     , mech_map_radm2( 14)  &
    / 'TOL             ', 12            /
DATA  conv_fac_radm2( 14) / 1.000         /

DATA  spmh_nam_radm2( 15)     , spmh_map_radm2( 15)  &
    / 'phellandrene_a  ', 12            /
DATA  mech_nam_radm2( 15)     , mech_map_radm2( 15)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 15) / 1.000         /

DATA  spmh_nam_radm2( 16)     , spmh_map_radm2( 16)  &
    / 'thujene_a       ', 13            /
DATA  mech_nam_radm2( 16)     , mech_map_radm2( 16)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 16) / 1.000         /

DATA  spmh_nam_radm2( 17)     , spmh_map_radm2( 17)  &
    / 'terpinene_a     ', 14            /
DATA  mech_nam_radm2( 17)     , mech_map_radm2( 17)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 17) / 1.000         /

DATA  spmh_nam_radm2( 18)     , spmh_map_radm2( 18)  &
    / 'terpinene_g     ', 15            /
DATA  mech_nam_radm2( 18)     , mech_map_radm2( 18)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 18) / 1.000         /

DATA  spmh_nam_radm2( 19)     , spmh_map_radm2( 19)  &
    / 'terpinolene     ', 16            /
DATA  mech_nam_radm2( 19)     , mech_map_radm2( 19)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 19) / 1.000         /

DATA  spmh_nam_radm2( 20)     , spmh_map_radm2( 20)  &
    / 'phellandrene_b  ', 17            /
DATA  mech_nam_radm2( 20)     , mech_map_radm2( 20)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2( 20) / 0.5           /

DATA  spmh_nam_radm2( 21)     , spmh_map_radm2( 21)  &
    / 'phellandrene_b  ', 17            /
DATA  mech_nam_radm2( 21)     , mech_map_radm2( 21)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 21) / 0.5           /

DATA  spmh_nam_radm2( 22)     , spmh_map_radm2( 22)  &
    / 'camphene        ', 18            /
DATA  mech_nam_radm2( 22)     , mech_map_radm2( 22)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2( 22) / 1.000         /

DATA  spmh_nam_radm2( 23)     , spmh_map_radm2( 23)  &
    / 'bornene         ', 19            /
DATA  mech_nam_radm2( 23)     , mech_map_radm2( 23)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 23) / 1.000         /

DATA  spmh_nam_radm2( 24)     , spmh_map_radm2( 24)  &
    / 'fenchene_a      ', 20            /
DATA  mech_nam_radm2( 24)     , mech_map_radm2( 24)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2( 24) / 1.000         /

DATA  spmh_nam_radm2( 25)     , spmh_map_radm2( 25)  &
    / 'ocimene_al      ', 21            /
DATA  mech_nam_radm2( 25)     , mech_map_radm2( 25)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 25) / 1.000         /

DATA  spmh_nam_radm2( 26)     , spmh_map_radm2( 26)  &
    / 'ocimene_c_b     ', 22            /
DATA  mech_nam_radm2( 26)     , mech_map_radm2( 26)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2( 26) / 0.5           /

DATA  spmh_nam_radm2( 27)     , spmh_map_radm2( 27)  &
    / 'ocimene_c_b     ', 22            /
DATA  mech_nam_radm2( 27)     , mech_map_radm2( 27)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 27) / 0.5           /

DATA  spmh_nam_radm2( 28)     , spmh_map_radm2( 28)  &
    / 'tricyclene      ', 23            /
DATA  mech_nam_radm2( 28)     , mech_map_radm2( 28)  &
    / 'NR              ', 21            /
DATA  conv_fac_radm2( 28) / 1.000         /

DATA  spmh_nam_radm2( 29)     , spmh_map_radm2( 29)  &
    / 'estragole       ', 24            /
DATA  mech_nam_radm2( 29)     , mech_map_radm2( 29)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2( 29) / 1.000         /

DATA  spmh_nam_radm2( 30)     , spmh_map_radm2( 30)  &
    / 'camphor         ', 25            /
DATA  mech_nam_radm2( 30)     , mech_map_radm2( 30)  &
    / 'HC8             ', 6             /
DATA  conv_fac_radm2( 30) / 0.388         /

DATA  spmh_nam_radm2( 31)     , spmh_map_radm2( 31)  &
    / 'fenchone        ', 26            /
DATA  mech_nam_radm2( 31)     , mech_map_radm2( 31)  &
    / 'NR              ', 21            /
DATA  conv_fac_radm2( 31) / 1.000         /

DATA  spmh_nam_radm2( 32)     , spmh_map_radm2( 32)  &
    / 'piperitone      ', 27            /
DATA  mech_nam_radm2( 32)     , mech_map_radm2( 32)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2( 32) / 1.000         /

DATA  spmh_nam_radm2( 33)     , spmh_map_radm2( 33)  &
    / 'thujone_a       ', 28            /
DATA  mech_nam_radm2( 33)     , mech_map_radm2( 33)  &
    / 'NR              ', 21            /
DATA  conv_fac_radm2( 33) / 1.000         /

DATA  spmh_nam_radm2( 34)     , spmh_map_radm2( 34)  &
    / 'thujone_b       ', 29            /
DATA  mech_nam_radm2( 34)     , mech_map_radm2( 34)  &
    / 'NR              ', 21            /
DATA  conv_fac_radm2( 34) / 1.000         /

DATA  spmh_nam_radm2( 35)     , spmh_map_radm2( 35)  &
    / 'cineole_1_8     ', 30            /
DATA  mech_nam_radm2( 35)     , mech_map_radm2( 35)  &
    / 'HC8             ', 6             /
DATA  conv_fac_radm2( 35) / 0.755         /

DATA  spmh_nam_radm2( 36)     , spmh_map_radm2( 36)  &
    / 'borneol         ', 31            /
DATA  mech_nam_radm2( 36)     , mech_map_radm2( 36)  &
    / 'NR              ', 21            /
DATA  conv_fac_radm2( 36) / 1.000         /

DATA  spmh_nam_radm2( 37)     , spmh_map_radm2( 37)  &
    / 'linalool        ', 32            /
DATA  mech_nam_radm2( 37)     , mech_map_radm2( 37)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2( 37) / 0.5           /

DATA  spmh_nam_radm2( 38)     , spmh_map_radm2( 38)  &
    / 'linalool        ', 32            /
DATA  mech_nam_radm2( 38)     , mech_map_radm2( 38)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 38) / 0.5           /

DATA  spmh_nam_radm2( 39)     , spmh_map_radm2( 39)  &
    / 'terpineol_4     ', 33            /
DATA  mech_nam_radm2( 39)     , mech_map_radm2( 39)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 39) / 1.000         /

DATA  spmh_nam_radm2( 40)     , spmh_map_radm2( 40)  &
    / 'terpineol_a     ', 34            /
DATA  mech_nam_radm2( 40)     , mech_map_radm2( 40)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 40) / 1.000         /

DATA  spmh_nam_radm2( 41)     , spmh_map_radm2( 41)  &
    / 'linalool_OXD_c  ', 35            /
DATA  mech_nam_radm2( 41)     , mech_map_radm2( 41)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2( 41) / 1.000         /

DATA  spmh_nam_radm2( 42)     , spmh_map_radm2( 42)  &
    / 'linalool_OXD_t  ', 36            /
DATA  mech_nam_radm2( 42)     , mech_map_radm2( 42)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2( 42) / 1.000         /

DATA  spmh_nam_radm2( 43)     , spmh_map_radm2( 43)  &
    / 'ionone_b        ', 37            /
DATA  mech_nam_radm2( 43)     , mech_map_radm2( 43)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 43) / 1.000         /

DATA  spmh_nam_radm2( 44)     , spmh_map_radm2( 44)  &
    / 'bornyl_ACT      ', 38            /
DATA  mech_nam_radm2( 44)     , mech_map_radm2( 44)  &
    / 'NR              ', 21            /
DATA  conv_fac_radm2( 44) / 1.000         /

DATA  spmh_nam_radm2( 45)     , spmh_map_radm2( 45)  &
    / 'farnescene_a    ', 39            /
DATA  mech_nam_radm2( 45)     , mech_map_radm2( 45)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2( 45) / 0.5           /

DATA  spmh_nam_radm2( 46)     , spmh_map_radm2( 46)  &
    / 'farnescene_a    ', 39            /
DATA  mech_nam_radm2( 46)     , mech_map_radm2( 46)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 46) / 0.5           /

DATA  spmh_nam_radm2( 47)     , spmh_map_radm2( 47)  &
    / 'caryophyllene_b ', 40            /
DATA  mech_nam_radm2( 47)     , mech_map_radm2( 47)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2( 47) / 0.5           /

DATA  spmh_nam_radm2( 48)     , spmh_map_radm2( 48)  &
    / 'caryophyllene_b ', 40            /
DATA  mech_nam_radm2( 48)     , mech_map_radm2( 48)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 48) / 0.5           /

DATA  spmh_nam_radm2( 49)     , spmh_map_radm2( 49)  &
    / 'acoradiene      ', 41            /
DATA  mech_nam_radm2( 49)     , mech_map_radm2( 49)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2( 49) / 0.5           /

DATA  spmh_nam_radm2( 50)     , spmh_map_radm2( 50)  &
    / 'acoradiene      ', 41            /
DATA  mech_nam_radm2( 50)     , mech_map_radm2( 50)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 50) / 0.5           /

DATA  spmh_nam_radm2( 51)     , spmh_map_radm2( 51)  &
    / 'aromadendrene   ', 42            /
DATA  mech_nam_radm2( 51)     , mech_map_radm2( 51)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2( 51) / 1.000         /

DATA  spmh_nam_radm2( 52)     , spmh_map_radm2( 52)  &
    / 'bergamotene_a   ', 43            /
DATA  mech_nam_radm2( 52)     , mech_map_radm2( 52)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 52) / 1.000         /

DATA  spmh_nam_radm2( 53)     , spmh_map_radm2( 53)  &
    / 'bergamotene_b   ', 44            /
DATA  mech_nam_radm2( 53)     , mech_map_radm2( 53)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2( 53) / 0.5           /

DATA  spmh_nam_radm2( 54)     , spmh_map_radm2( 54)  &
    / 'bergamotene_b   ', 44            /
DATA  mech_nam_radm2( 54)     , mech_map_radm2( 54)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 54) / 0.5           /

DATA  spmh_nam_radm2( 55)     , spmh_map_radm2( 55)  &
    / 'bisabolene_a    ', 45            /
DATA  mech_nam_radm2( 55)     , mech_map_radm2( 55)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 55) / 1.000         /

DATA  spmh_nam_radm2( 56)     , spmh_map_radm2( 56)  &
    / 'bisabolene_b    ', 46            /
DATA  mech_nam_radm2( 56)     , mech_map_radm2( 56)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2( 56) / 0.5           /

DATA  spmh_nam_radm2( 57)     , spmh_map_radm2( 57)  &
    / 'bisabolene_b    ', 46            /
DATA  mech_nam_radm2( 57)     , mech_map_radm2( 57)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 57) / 0.5           /

DATA  spmh_nam_radm2( 58)     , spmh_map_radm2( 58)  &
    / 'bourbonene_b    ', 47            /
DATA  mech_nam_radm2( 58)     , mech_map_radm2( 58)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2( 58) / 1.000         /

DATA  spmh_nam_radm2( 59)     , spmh_map_radm2( 59)  &
    / 'cadinene_d      ', 48            /
DATA  mech_nam_radm2( 59)     , mech_map_radm2( 59)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 59) / 1.000         /

DATA  spmh_nam_radm2( 60)     , spmh_map_radm2( 60)  &
    / 'cadinene_g      ', 49            /
DATA  mech_nam_radm2( 60)     , mech_map_radm2( 60)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2( 60) / 0.5           /

DATA  spmh_nam_radm2( 61)     , spmh_map_radm2( 61)  &
    / 'cadinene_g      ', 49            /
DATA  mech_nam_radm2( 61)     , mech_map_radm2( 61)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 61) / 0.5           /

DATA  spmh_nam_radm2( 62)     , spmh_map_radm2( 62)  &
    / 'cedrene_a       ', 50            /
DATA  mech_nam_radm2( 62)     , mech_map_radm2( 62)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 62) / 1.000         /

DATA  spmh_nam_radm2( 63)     , spmh_map_radm2( 63)  &
    / 'copaene_a       ', 51            /
DATA  mech_nam_radm2( 63)     , mech_map_radm2( 63)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 63) / 1.000         /

DATA  spmh_nam_radm2( 64)     , spmh_map_radm2( 64)  &
    / 'cubebene_a      ', 52            /
DATA  mech_nam_radm2( 64)     , mech_map_radm2( 64)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 64) / 1.000         /

DATA  spmh_nam_radm2( 65)     , spmh_map_radm2( 65)  &
    / 'cubebene_b      ', 53            /
DATA  mech_nam_radm2( 65)     , mech_map_radm2( 65)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2( 65) / 1.000         /

DATA  spmh_nam_radm2( 66)     , spmh_map_radm2( 66)  &
    / 'elemene_b       ', 54            /
DATA  mech_nam_radm2( 66)     , mech_map_radm2( 66)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2( 66) / 1.000         /

DATA  spmh_nam_radm2( 67)     , spmh_map_radm2( 67)  &
    / 'farnescene_b    ', 55            /
DATA  mech_nam_radm2( 67)     , mech_map_radm2( 67)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2( 67) / 0.5           /

DATA  spmh_nam_radm2( 68)     , spmh_map_radm2( 68)  &
    / 'farnescene_b    ', 55            /
DATA  mech_nam_radm2( 68)     , mech_map_radm2( 68)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 68) / 0.5           /

DATA  spmh_nam_radm2( 69)     , spmh_map_radm2( 69)  &
    / 'germacrene_B    ', 56            /
DATA  mech_nam_radm2( 69)     , mech_map_radm2( 69)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 69) / 1.000         /

DATA  spmh_nam_radm2( 70)     , spmh_map_radm2( 70)  &
    / 'germacrene_D    ', 57            /
DATA  mech_nam_radm2( 70)     , mech_map_radm2( 70)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2( 70) / 0.5           /

DATA  spmh_nam_radm2( 71)     , spmh_map_radm2( 71)  &
    / 'germacrene_D    ', 57            /
DATA  mech_nam_radm2( 71)     , mech_map_radm2( 71)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 71) / 0.5           /

DATA  spmh_nam_radm2( 72)     , spmh_map_radm2( 72)  &
    / 'gurjunene_b     ', 58            /
DATA  mech_nam_radm2( 72)     , mech_map_radm2( 72)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 72) / 1.000         /

DATA  spmh_nam_radm2( 73)     , spmh_map_radm2( 73)  &
    / 'humulene_a      ', 59            /
DATA  mech_nam_radm2( 73)     , mech_map_radm2( 73)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 73) / 1.000         /

DATA  spmh_nam_radm2( 74)     , spmh_map_radm2( 74)  &
    / 'humulene_g      ', 60            /
DATA  mech_nam_radm2( 74)     , mech_map_radm2( 74)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 74) / 1.000         /

DATA  spmh_nam_radm2( 75)     , spmh_map_radm2( 75)  &
    / 'isolongifolene  ', 61            /
DATA  mech_nam_radm2( 75)     , mech_map_radm2( 75)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 75) / 1.000         /

DATA  spmh_nam_radm2( 76)     , spmh_map_radm2( 76)  &
    / 'longifolene     ', 62            /
DATA  mech_nam_radm2( 76)     , mech_map_radm2( 76)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2( 76) / 1.000         /

DATA  spmh_nam_radm2( 77)     , spmh_map_radm2( 77)  &
    / 'longipinene     ', 63            /
DATA  mech_nam_radm2( 77)     , mech_map_radm2( 77)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 77) / 1.000         /

DATA  spmh_nam_radm2( 78)     , spmh_map_radm2( 78)  &
    / 'muurolene_a     ', 64            /
DATA  mech_nam_radm2( 78)     , mech_map_radm2( 78)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 78) / 1.000         /

DATA  spmh_nam_radm2( 79)     , spmh_map_radm2( 79)  &
    / 'muurolene_g     ', 65            /
DATA  mech_nam_radm2( 79)     , mech_map_radm2( 79)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2( 79) / 0.5           /

DATA  spmh_nam_radm2( 80)     , spmh_map_radm2( 80)  &
    / 'muurolene_g     ', 65            /
DATA  mech_nam_radm2( 80)     , mech_map_radm2( 80)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 80) / 0.5           /

DATA  spmh_nam_radm2( 81)     , spmh_map_radm2( 81)  &
    / 'selinene_b      ', 66            /
DATA  mech_nam_radm2( 81)     , mech_map_radm2( 81)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2( 81) / 1.000         /

DATA  spmh_nam_radm2( 82)     , spmh_map_radm2( 82)  &
    / 'selinene_d      ', 67            /
DATA  mech_nam_radm2( 82)     , mech_map_radm2( 82)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 82) / 1.000         /

DATA  spmh_nam_radm2( 83)     , spmh_map_radm2( 83)  &
    / 'nerolidol_c     ', 68            /
DATA  mech_nam_radm2( 83)     , mech_map_radm2( 83)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2( 83) / 0.5           /

DATA  spmh_nam_radm2( 84)     , spmh_map_radm2( 84)  &
    / 'nerolidol_c     ', 68            /
DATA  mech_nam_radm2( 84)     , mech_map_radm2( 84)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 84) / 0.5           /

DATA  spmh_nam_radm2( 85)     , spmh_map_radm2( 85)  &
    / 'nerolidol_t     ', 69            /
DATA  mech_nam_radm2( 85)     , mech_map_radm2( 85)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2( 85) / 0.5           /

DATA  spmh_nam_radm2( 86)     , spmh_map_radm2( 86)  &
    / 'nerolidol_t     ', 69            /
DATA  mech_nam_radm2( 86)     , mech_map_radm2( 86)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2( 86) / 0.5           /

DATA  spmh_nam_radm2( 87)     , spmh_map_radm2( 87)  &
    / 'cedrol          ', 70            /
DATA  mech_nam_radm2( 87)     , mech_map_radm2( 87)  &
    / 'NR              ', 21            /
DATA  conv_fac_radm2( 87) / 1.000         /

DATA  spmh_nam_radm2( 88)     , spmh_map_radm2( 88)  &
    / 'MBO_2m3e2ol     ', 71            /
DATA  mech_nam_radm2( 88)     , mech_map_radm2( 88)  &
    / 'ISO             ', 1             /
DATA  conv_fac_radm2( 88) / 1.000         /

DATA  spmh_nam_radm2( 89)     , spmh_map_radm2( 89)  &
    / 'methanol        ', 72            /
DATA  mech_nam_radm2( 89)     , mech_map_radm2( 89)  &
    / 'HC3             ', 4             /
DATA  conv_fac_radm2( 89) / 0.402         /

DATA  spmh_nam_radm2( 90)     , spmh_map_radm2( 90)  &
    / 'acetone         ', 73            /
DATA  mech_nam_radm2( 90)     , mech_map_radm2( 90)  &
    / 'KET             ', 11            /
DATA  conv_fac_radm2( 90) / 0.253         /

DATA  spmh_nam_radm2( 91)     , spmh_map_radm2( 91)  &
    / 'methane         ', 74            /
DATA  mech_nam_radm2( 91)     , mech_map_radm2( 91)  &
    / 'CH4             ', 2             /
DATA  conv_fac_radm2( 91) / 1.000         /

DATA  spmh_nam_radm2( 92)     , spmh_map_radm2( 92)  &
    / 'ammonia         ', 75            /
DATA  mech_nam_radm2( 92)     , mech_map_radm2( 92)  &
    / 'NR              ', 21            /
DATA  conv_fac_radm2( 92) / 1.000         /

DATA  spmh_nam_radm2( 93)     , spmh_map_radm2( 93)  &
    / 'nitrous_OXD     ', 76            /
DATA  mech_nam_radm2( 93)     , mech_map_radm2( 93)  &
    / 'NO2             ', 20            /
DATA  conv_fac_radm2( 93) / 1.000         /

DATA  spmh_nam_radm2( 94)     , spmh_map_radm2( 94)  &
    / 'nitric_OXD      ', 77            /
DATA  mech_nam_radm2( 94)     , mech_map_radm2( 94)  &
    / 'NO              ', 18            /
DATA  conv_fac_radm2( 94) / 1.000         /

DATA  spmh_nam_radm2( 95)     , spmh_map_radm2( 95)  &
    / 'acetaldehyde    ', 78            /
DATA  mech_nam_radm2( 95)     , mech_map_radm2( 95)  &
    / 'ALD             ', 10            /
DATA  conv_fac_radm2( 95) / 1.000         /

DATA  spmh_nam_radm2( 96)     , spmh_map_radm2( 96)  &
    / 'ethanol         ', 79            /
DATA  mech_nam_radm2( 96)     , mech_map_radm2( 96)  &
    / 'HC3             ', 4             /
DATA  conv_fac_radm2( 96) / 1.198         /

DATA  spmh_nam_radm2( 97)     , spmh_map_radm2( 97)  &
    / 'formic_acid     ', 80            /
DATA  mech_nam_radm2( 97)     , mech_map_radm2( 97)  &
    / 'ORA1            ', 14            /
DATA  conv_fac_radm2( 97) / 1.000         /

DATA  spmh_nam_radm2( 98)     , spmh_map_radm2( 98)  &
    / 'formaldehyde    ', 81            /
DATA  mech_nam_radm2( 98)     , mech_map_radm2( 98)  &
    / 'HCHO            ', 13            /
DATA  conv_fac_radm2( 98) / 1.000         /

DATA  spmh_nam_radm2( 99)     , spmh_map_radm2( 99)  &
    / 'acetic_acid     ', 82            /
DATA  mech_nam_radm2( 99)     , mech_map_radm2( 99)  &
    / 'ORA2            ', 15            /
DATA  conv_fac_radm2( 99) / 1.000         /

DATA  spmh_nam_radm2(100)     , spmh_map_radm2(100)  &
    / 'MBO_3m2e1ol     ', 83            /
DATA  mech_nam_radm2(100)     , mech_map_radm2(100)  &
    / 'ISO             ', 1             /
DATA  conv_fac_radm2(100) / 1.000         /

DATA  spmh_nam_radm2(101)     , spmh_map_radm2(101)  &
    / 'MBO_3m3e1ol     ', 84            /
DATA  mech_nam_radm2(101)     , mech_map_radm2(101)  &
    / 'ISO             ', 1             /
DATA  conv_fac_radm2(101) / 1.000         /

DATA  spmh_nam_radm2(102)     , spmh_map_radm2(102)  &
    / 'benzaldehyde    ', 85            /
DATA  mech_nam_radm2(102)     , mech_map_radm2(102)  &
    / 'NR              ', 21            /
DATA  conv_fac_radm2(102) / 1.000         /

DATA  spmh_nam_radm2(103)     , spmh_map_radm2(103)  &
    / 'butanone_2      ', 86            /
DATA  mech_nam_radm2(103)     , mech_map_radm2(103)  &
    / 'KET             ', 11            /
DATA  conv_fac_radm2(103) / 1.000         /

DATA  spmh_nam_radm2(104)     , spmh_map_radm2(104)  &
    / 'decanal         ', 87            /
DATA  mech_nam_radm2(104)     , mech_map_radm2(104)  &
    / 'ALD             ', 10            /
DATA  conv_fac_radm2(104) / 1.000         /

DATA  spmh_nam_radm2(105)     , spmh_map_radm2(105)  &
    / 'dodecene_1      ', 88            /
DATA  mech_nam_radm2(105)     , mech_map_radm2(105)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2(105) / 1.000         /

DATA  spmh_nam_radm2(106)     , spmh_map_radm2(106)  &
    / 'geranyl_acetone ', 89            /
DATA  mech_nam_radm2(106)     , mech_map_radm2(106)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2(106) / 1.000         /

DATA  spmh_nam_radm2(107)     , spmh_map_radm2(107)  &
    / 'heptanal        ', 90            /
DATA  mech_nam_radm2(107)     , mech_map_radm2(107)  &
    / 'ALD             ', 10            /
DATA  conv_fac_radm2(107) / 1.000         /

DATA  spmh_nam_radm2(108)     , spmh_map_radm2(108)  &
    / 'heptane         ', 91            /
DATA  mech_nam_radm2(108)     , mech_map_radm2(108)  &
    / 'HC5             ', 5             /
DATA  conv_fac_radm2(108) / 1.226         /

DATA  spmh_nam_radm2(109)     , spmh_map_radm2(109)  &
    / 'hexane          ', 92            /
DATA  mech_nam_radm2(109)     , mech_map_radm2(109)  &
    / 'HC5             ', 5             /
DATA  conv_fac_radm2(109) / 1.049         /

DATA  spmh_nam_radm2(110)     , spmh_map_radm2(110)  &
    / 'met_benzoate    ', 93            /
DATA  mech_nam_radm2(110)     , mech_map_radm2(110)  &
    / 'HC8             ', 6             /
DATA  conv_fac_radm2(110) / 1.000         /

DATA  spmh_nam_radm2(111)     , spmh_map_radm2(111)  &
    / 'met_heptenone   ', 94            /
DATA  mech_nam_radm2(111)     , mech_map_radm2(111)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2(111) / 1.000         /

DATA  spmh_nam_radm2(112)     , spmh_map_radm2(112)  &
    / 'neryl_acetone   ', 95            /
DATA  mech_nam_radm2(112)     , mech_map_radm2(112)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2(112) / 1.000         /

DATA  spmh_nam_radm2(113)     , spmh_map_radm2(113)  &
    / 'nonanal         ', 96            /
DATA  mech_nam_radm2(113)     , mech_map_radm2(113)  &
    / 'ALD             ', 10            /
DATA  conv_fac_radm2(113) / 1.000         /

DATA  spmh_nam_radm2(114)     , spmh_map_radm2(114)  &
    / 'nonenal         ', 97            /
DATA  mech_nam_radm2(114)     , mech_map_radm2(114)  &
    / 'ALD             ', 10            /
DATA  conv_fac_radm2(114) / 1.000         /

DATA  spmh_nam_radm2(115)     , spmh_map_radm2(115)  &
    / 'nonenal         ', 97            /
DATA  mech_nam_radm2(115)     , mech_map_radm2(115)  &
    / 'HC8             ', 6             /
DATA  conv_fac_radm2(115) / 1.000         /

DATA  spmh_nam_radm2(116)     , spmh_map_radm2(116)  &
    / 'octanal         ', 98            /
DATA  mech_nam_radm2(116)     , mech_map_radm2(116)  &
    / 'ALD             ', 10            /
DATA  conv_fac_radm2(116) / 1.000         /

DATA  spmh_nam_radm2(117)     , spmh_map_radm2(117)  &
    / 'octanol         ', 99            /
DATA  mech_nam_radm2(117)     , mech_map_radm2(117)  &
    / 'HC8             ', 6             /
DATA  conv_fac_radm2(117) / 1.119         /

DATA  spmh_nam_radm2(118)     , spmh_map_radm2(118)  &
    / 'octenol_1e3ol   ', 100           /
DATA  mech_nam_radm2(118)     , mech_map_radm2(118)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2(118) / 1.000         /

DATA  spmh_nam_radm2(119)     , spmh_map_radm2(119)  &
    / 'oxopentanal     ', 101           /
DATA  mech_nam_radm2(119)     , mech_map_radm2(119)  &
    / 'ALD             ', 10            /
DATA  conv_fac_radm2(119) / 1.000         /

DATA  spmh_nam_radm2(120)     , spmh_map_radm2(120)  &
    / 'pentane         ', 102           /
DATA  mech_nam_radm2(120)     , mech_map_radm2(120)  &
    / 'HC5             ', 5             /
DATA  conv_fac_radm2(120) / 0.847         /

DATA  spmh_nam_radm2(121)     , spmh_map_radm2(121)  &
    / 'phenyl_CCO      ', 103           /
DATA  mech_nam_radm2(121)     , mech_map_radm2(121)  &
    / 'NR              ', 21            /
DATA  conv_fac_radm2(121) / 1.000         /

DATA  spmh_nam_radm2(122)     , spmh_map_radm2(122)  &
    / 'pyruvic_acid    ', 104           /
DATA  mech_nam_radm2(122)     , mech_map_radm2(122)  &
    / 'ORA2            ', 15            /
DATA  conv_fac_radm2(122) / 1.000         /

DATA  spmh_nam_radm2(123)     , spmh_map_radm2(123)  &
    / 'terpinyl_ACT_a  ', 105           /
DATA  mech_nam_radm2(123)     , mech_map_radm2(123)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2(123) / 1.000         /

DATA  spmh_nam_radm2(124)     , spmh_map_radm2(124)  &
    / 'tetradecene_1   ', 106           /
DATA  mech_nam_radm2(124)     , mech_map_radm2(124)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2(124) / 1.000         /

DATA  spmh_nam_radm2(125)     , spmh_map_radm2(125)  &
    / 'toluene         ', 107           /
DATA  mech_nam_radm2(125)     , mech_map_radm2(125)  &
    / 'TOL             ', 12            /
DATA  conv_fac_radm2(125) / 1.000         /

DATA  spmh_nam_radm2(126)     , spmh_map_radm2(126)  &
    / 'carbon_monoxide ', 108           /
DATA  mech_nam_radm2(126)     , mech_map_radm2(126)  &
    / 'CO              ', 16            /
DATA  conv_fac_radm2(126) / 1.000         /

DATA  spmh_nam_radm2(127)     , spmh_map_radm2(127)  &
    / 'butene          ', 109           /
DATA  mech_nam_radm2(127)     , mech_map_radm2(127)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2(127) / 1.000         /

DATA  spmh_nam_radm2(128)     , spmh_map_radm2(128)  &
    / 'ethane          ', 110           /
DATA  mech_nam_radm2(128)     , mech_map_radm2(128)  &
    / 'ETH             ', 3             /
DATA  conv_fac_radm2(128) / 1.000         /

DATA  spmh_nam_radm2(129)     , spmh_map_radm2(129)  &
    / 'ethene          ', 111           /
DATA  mech_nam_radm2(129)     , mech_map_radm2(129)  &
    / 'OL2             ', 7             /
DATA  conv_fac_radm2(129) / 1.000         /

DATA  spmh_nam_radm2(130)     , spmh_map_radm2(130)  &
    / 'hydrogen_cyanide', 112           /
DATA  mech_nam_radm2(130)     , mech_map_radm2(130)  &
    / 'NR              ', 21            /
DATA  conv_fac_radm2(130) / 1.000         /

DATA  spmh_nam_radm2(131)     , spmh_map_radm2(131)  &
    / 'propane         ', 113           /
DATA  mech_nam_radm2(131)     , mech_map_radm2(131)  &
    / 'HC3             ', 4             /
DATA  conv_fac_radm2(131) / 0.519         /

DATA  spmh_nam_radm2(132)     , spmh_map_radm2(132)  &
    / 'propene         ', 114           /
DATA  mech_nam_radm2(132)     , mech_map_radm2(132)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2(132) / 1.000         /

DATA  spmh_nam_radm2(133)     , spmh_map_radm2(133)  &
    / 'carbon_2s       ', 115           /
DATA  mech_nam_radm2(133)     , mech_map_radm2(133)  &
    / 'NR              ', 21            /
DATA  conv_fac_radm2(133) / 1.000         /

DATA  spmh_nam_radm2(134)     , spmh_map_radm2(134)  &
    / 'carbonyl_s      ', 116           /
DATA  mech_nam_radm2(134)     , mech_map_radm2(134)  &
    / 'NR              ', 21            /
DATA  conv_fac_radm2(134) / 1.000         /

DATA  spmh_nam_radm2(135)     , spmh_map_radm2(135)  &
    / 'diallyl_2s      ', 117           /
DATA  mech_nam_radm2(135)     , mech_map_radm2(135)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2(135) / 1.000         /

DATA  spmh_nam_radm2(136)     , spmh_map_radm2(136)  &
    / 'diallyl_2s      ', 117           /
DATA  mech_nam_radm2(136)     , mech_map_radm2(136)  &
    / 'SO2             ', 17            /
DATA  conv_fac_radm2(136) / 2             /

DATA  spmh_nam_radm2(137)     , spmh_map_radm2(137)  &
    / 'A_2met_2s         ', 118           /
DATA  mech_nam_radm2(137)     , mech_map_radm2(137)  &
    / 'ETH             ', 3             /
DATA  conv_fac_radm2(137) / 1.000         /

DATA  spmh_nam_radm2(138)     , spmh_map_radm2(138)  &
    / 'A_2met_2s         ', 118           /
DATA  mech_nam_radm2(138)     , mech_map_radm2(138)  &
    / 'SO2             ', 17            /
DATA  conv_fac_radm2(138) / 2             /

DATA  spmh_nam_radm2(139)     , spmh_map_radm2(139)  &
    / 'A_2met_s          ', 119           /
DATA  mech_nam_radm2(139)     , mech_map_radm2(139)  &
    / 'ETH             ', 3             /
DATA  conv_fac_radm2(139) / 1.000         /

DATA  spmh_nam_radm2(140)     , spmh_map_radm2(140)  &
    / 'A_2met_s          ', 119           /
DATA  mech_nam_radm2(140)     , mech_map_radm2(140)  &
    / 'SO2             ', 17            /
DATA  conv_fac_radm2(140) / 1.000         /
DATA  spmh_nam_radm2(141)     , spmh_map_radm2(141)  &
    / 'met_chloride    ', 120           /
DATA  mech_nam_radm2(141)     , mech_map_radm2(141)  &
    / 'NR              ', 21            /
DATA  conv_fac_radm2(141) / 1.000         /
DATA  spmh_nam_radm2(142)     , spmh_map_radm2(142)  &
    / 'met_bromide     ', 121           /
DATA  mech_nam_radm2(142)     , mech_map_radm2(142)  &
    / 'NR              ', 21            /
DATA  conv_fac_radm2(142) / 1.000         /
DATA  spmh_nam_radm2(143)     , spmh_map_radm2(143)  &
    / 'met_iodide      ', 122           /
DATA  mech_nam_radm2(143)     , mech_map_radm2(143)  &
    / 'NR              ', 21            /
DATA  conv_fac_radm2(143) / 1.000         /
DATA  spmh_nam_radm2(144)     , spmh_map_radm2(144)  &
    / 'hydrogen_s      ', 123           /
DATA  mech_nam_radm2(144)     , mech_map_radm2(144)  &
    / 'SO2             ', 17            /
DATA  conv_fac_radm2(144) / 1.000         /
DATA  spmh_nam_radm2(145)     , spmh_map_radm2(145)  &
    / 'met_mercaptan   ', 124           /
DATA  mech_nam_radm2(145)     , mech_map_radm2(145)  &
    / 'CH4             ', 2             /
DATA  conv_fac_radm2(145) / 1.000         /
DATA  spmh_nam_radm2(146)     , spmh_map_radm2(146)  &
    / 'met_mercaptan   ', 124           /
DATA  mech_nam_radm2(146)     , mech_map_radm2(146)  &
    / 'SO2             ', 17            /
DATA  conv_fac_radm2(146) / 1.000         /
DATA  spmh_nam_radm2(147)     , spmh_map_radm2(147)  &
    / 'met_propenyl_2s ', 125           /
DATA  mech_nam_radm2(147)     , mech_map_radm2(147)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2(147) / 1.000         /
DATA  spmh_nam_radm2(148)     , spmh_map_radm2(148)  &
    / 'met_propenyl_2s ', 125           /
DATA  mech_nam_radm2(148)     , mech_map_radm2(148)  &
    / 'SO2             ', 17            /
DATA  conv_fac_radm2(148) / 2             /
DATA  spmh_nam_radm2(149)     , spmh_map_radm2(149)  &
    / 'PPPP_2s         ', 126           /
DATA  mech_nam_radm2(149)     , mech_map_radm2(149)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2(149) / 1.000         /
DATA  spmh_nam_radm2(150)     , spmh_map_radm2(150)  &
    / 'PPPP_2s         ', 126           /
DATA  mech_nam_radm2(150)     , mech_map_radm2(150)  &
    / 'SO2             ', 17            /
DATA  conv_fac_radm2(150) / 2             /
DATA  spmh_nam_radm2(151)     , spmh_map_radm2(151)  &
    / 'A_2met_nonatriene ', 127           /
DATA  mech_nam_radm2(151)     , mech_map_radm2(151)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2(151) / 0.5           /
DATA  spmh_nam_radm2(152)     , spmh_map_radm2(152)  &
    / 'A_2met_nonatriene ', 127           /
DATA  mech_nam_radm2(152)     , mech_map_radm2(152)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2(152) / 0.5           /
DATA  spmh_nam_radm2(153)     , spmh_map_radm2(153)  &
    / 'met_salicylate  ', 128           /
DATA  mech_nam_radm2(153)     , mech_map_radm2(153)  &
    / 'HC8             ', 6             /
DATA  conv_fac_radm2(153) / 1.000         /

DATA  spmh_nam_radm2(154)     , spmh_map_radm2(154)  &
    / 'indole          ', 129           /
DATA  mech_nam_radm2(154)     , mech_map_radm2(154)  &
    / 'HC8             ', 6             /
DATA  conv_fac_radm2(154) / 1.238         /

DATA  spmh_nam_radm2(155)     , spmh_map_radm2(155)  &
    / 'indole          ', 129           /
DATA  mech_nam_radm2(155)     , mech_map_radm2(155)  &
    / 'HNO3            ', 19            /
DATA  conv_fac_radm2(155) / 1.000         /

DATA  spmh_nam_radm2(156)     , spmh_map_radm2(156)  &
    / 'jasmone         ', 130           /
DATA  mech_nam_radm2(156)     , mech_map_radm2(156)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2(156) / 1.000         /

DATA  spmh_nam_radm2(157)     , spmh_map_radm2(157)  &
    / 'met_jasmonate   ', 131           /
DATA  mech_nam_radm2(157)     , mech_map_radm2(157)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2(157) / 1.000         /

DATA  spmh_nam_radm2(158)     , spmh_map_radm2(158)  &
    / 'A_3met_3DCTT      ', 132           /
DATA  mech_nam_radm2(158)     , mech_map_radm2(158)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2(158) / 0.5           /

DATA  spmh_nam_radm2(159)     , spmh_map_radm2(159)  &
    / 'A_3met_3DCTT      ', 132           /
DATA  mech_nam_radm2(159)     , mech_map_radm2(159)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2(159) / 0.5           /

DATA  spmh_nam_radm2(160)     , spmh_map_radm2(160)  &
    / 'hexanal         ', 133           /
DATA  mech_nam_radm2(160)     , mech_map_radm2(160)  &
    / 'ALD             ', 10            /
DATA  conv_fac_radm2(160) / 1.000         /

DATA  spmh_nam_radm2(161)     , spmh_map_radm2(161)  &
    / 'hexanol_1       ', 134           /
DATA  mech_nam_radm2(161)     , mech_map_radm2(161)  &
    / 'HC5             ', 5             /
DATA  conv_fac_radm2(161) / 1.697         /

DATA  spmh_nam_radm2(162)     , spmh_map_radm2(162)  &
    / 'hexenal_c3      ', 135           /
DATA  mech_nam_radm2(162)     , mech_map_radm2(162)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2(162) / 1.000         /

DATA  spmh_nam_radm2(163)     , spmh_map_radm2(163)  &
    / 'hexenal_t2      ', 136           /
DATA  mech_nam_radm2(163)     , mech_map_radm2(163)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2(163) / 1.000         /

DATA  spmh_nam_radm2(164)     , spmh_map_radm2(164)  &
    / 'hexenol_c3      ', 137           /
DATA  mech_nam_radm2(164)     , mech_map_radm2(164)  &
    / 'OLT             ', 9             /
DATA  conv_fac_radm2(164) / 1.000         /

DATA  spmh_nam_radm2(165)     , spmh_map_radm2(165)  &
    / 'hexenyl_ACT_c3  ', 138           /
DATA  mech_nam_radm2(165)     , mech_map_radm2(165)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2(165) / 1.000         /

DATA  spmh_nam_radm2(166)     , spmh_map_radm2(166)  &
    / 'homosalate      ', 139            /
DATA  mech_nam_radm2(166)     , mech_map_radm2(166)  &
    / 'OLI             ', 8              /
DATA  conv_fac_radm2(166) / 1.0            /
DATA  spmh_nam_radm2(167)     , spmh_map_radm2(167)  &
    / 'Ehsalate        ', 140            /
DATA  mech_nam_radm2(167)     , mech_map_radm2(167)  &
    / 'OLI             ', 8              /
DATA  conv_fac_radm2(167) / 1.0            /
DATA  spmh_nam_radm2(168)     , spmh_map_radm2(168)  &
    / 'pentanal         ', 141            /
DATA  mech_nam_radm2(168)     , mech_map_radm2(168)  &
    /  'ALD             ', 10            /
DATA  conv_fac_radm2(168) / 1.0            /
DATA  spmh_nam_radm2(169)     , spmh_map_radm2(169)  &
    / 'heptanone      ', 142            /
DATA  mech_nam_radm2(169)     , mech_map_radm2(169)  &
    / 'OLI             ', 8             /
DATA  conv_fac_radm2(169) / 1.0            /
DATA  spmh_nam_radm2(170)     , spmh_map_radm2(170)  &
    / 'anisole         ', 143            /
DATA  mech_nam_radm2(170)     , mech_map_radm2(170)  &
    / 'NR              ', 21            /
DATA  conv_fac_radm2(170) / 1.0            /
DATA  spmh_nam_radm2(171)     , spmh_map_radm2(171)  &
    / 'verbenene       ', 144            /
DATA  mech_nam_radm2(171)     , mech_map_radm2(171)  &
    / 'TOL             ', 12             /
DATA  conv_fac_radm2(171) / 1.0            /
DATA  spmh_nam_radm2(172)     , spmh_map_radm2(172)  &
    / 'benzyl-acetate   ', 145            /
DATA  mech_nam_radm2(172)     , mech_map_radm2(172)  &
    / 'NR               ', 21             /
DATA  conv_fac_radm2(172) / 1.0            /
DATA  spmh_nam_radm2(173)     , spmh_map_radm2(173)  &
    / 'myrtenal         ', 146            /
DATA  mech_nam_radm2(173)     , mech_map_radm2(173)  &
    / 'OLT             ', 9            /
DATA  conv_fac_radm2(173) / 1.0            /
DATA  spmh_nam_radm2(174)     , spmh_map_radm2(174)  &
    / 'benzyl-alcohol ', 147            /
DATA  mech_nam_radm2(174)     , mech_map_radm2(174)  &
    / 'NR              ', 21              /
DATA  conv_fac_radm2(174) / 1.0            /
DATA  spmh_nam_radm2(175)     , spmh_map_radm2(175)  &
    / 'meta-cymenene  ', 148            /
DATA  mech_nam_radm2(175)     , mech_map_radm2(175)  &
    / 'TOL             ', 12              /
DATA  conv_fac_radm2(175) / 1.0            /
DATA  spmh_nam_radm2(176)     , spmh_map_radm2(176)  &
    / 'ipsenol        ', 149            /
DATA  mech_nam_radm2(176)     , mech_map_radm2(176)  &
    / 'OLT             ', 9              /
DATA  conv_fac_radm2(176) / 1.0            /
DATA  spmh_nam_radm2(177)     , spmh_map_radm2(177)  &
    / 'Napthalene     ', 150            /
DATA  mech_nam_radm2(177)     , mech_map_radm2(177)  &
    / 'HC8            ', 6               /
DATA  conv_fac_radm2(177) / 1.0            /
!=======================================================================
!  SPC_RADM2.EXT
!  This include file contains RADM2 species and their MW.


!  Mechanism Name: RADM2
!  MEGAN v2.1.0
!  INPUT version 200

!  History:
!  Who          When       What
!  ---------------------------------------------------------------------
!  Tan          12/02/06 - Creates this file
!  Tan          08/14/07 - Move from MEGAN v2.0 to MEGAN v2.1.0 with no update.
!=======================================================================

DATA  mech_spc_radm2( 1), mech_mwt_radm2( 1) /'ISO', 68.0   /
DATA  mech_spc_radm2( 2), mech_mwt_radm2( 2) /'CH4', 16.0   /
DATA  mech_spc_radm2( 3), mech_mwt_radm2( 3) /'ETH', 30.0   /
DATA  mech_spc_radm2( 4), mech_mwt_radm2( 4) /'HC3', 44.0   /
DATA  mech_spc_radm2( 5), mech_mwt_radm2( 5) /'HC5', 72.0   /
DATA  mech_spc_radm2( 6), mech_mwt_radm2( 6) /'HC8', 114.0  /
DATA  mech_spc_radm2( 7), mech_mwt_radm2( 7) /'OL2', 28.0   /
DATA  mech_spc_radm2( 8), mech_mwt_radm2( 8) /'OLI', 56.0   /
DATA  mech_spc_radm2( 9), mech_mwt_radm2( 9) /'OLT', 42.0   /
DATA  mech_spc_radm2(10), mech_mwt_radm2(10) /'ALD', 44.0   /
DATA  mech_spc_radm2(11), mech_mwt_radm2(11) /'KET', 72.0   /
DATA  mech_spc_radm2(12), mech_mwt_radm2(12) /'TOL', 92.0   /
DATA  mech_spc_radm2(13), mech_mwt_radm2(13) /'HCHO ', 30.0   /
DATA  mech_spc_radm2(14), mech_mwt_radm2(14) /'ORA1 ', 46.0   /
DATA  mech_spc_radm2(15), mech_mwt_radm2(15) /'ORA2 ', 60.0   /
DATA  mech_spc_radm2(16), mech_mwt_radm2(16) /'CO ', 28.0   /
DATA  mech_spc_radm2(17), mech_mwt_radm2(17) /'SO2', 64.0   /
DATA  mech_spc_radm2(18), mech_mwt_radm2(18) /'NO ', 30.0   /
DATA  mech_spc_radm2(19), mech_mwt_radm2(19) /'HNO3 ', 63.0   /
DATA  mech_spc_radm2(20), mech_mwt_radm2(20) /'NO2', 46.0   /
DATA  mech_spc_radm2(21), mech_mwt_radm2(21) /'NR ', 1.0    /
!=======================================================================
!  MAP_CV2RACM.EXT
!  This include file contains conversion table for 134 speciated species
!  to RACM species


!  MEGAN v2.1.0
!  INPUT version 200

!  History:
!  Who          When       What
!  ---------------------------------------------------------------------
!  Tan          12/02/06 - Creates this file
!  Tan          08/14/07 - Move from MEGAN v2.0 to MEGAN v2.1.0 with no update.
!=======================================================================

DATA  spmh_nam_racm(  1)     , spmh_map_racm(  1)  &
    / 'isoprene        ', 1             /
DATA  mech_nam_racm(  1)     , mech_map_racm(  1)  &
    / 'ISO             ', 1             /
DATA  conv_fac_racm(  1) / 1.000         /

DATA  spmh_nam_racm(  2)     , spmh_map_racm(  2)  &
    / 'myrcene         ', 2             /
DATA  mech_nam_racm(  2)     , mech_map_racm(  2)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm(  2) / 1.000         /

DATA  spmh_nam_racm(  3)     , spmh_map_racm(  3)  &
    / 'sabinene        ', 3             /
DATA  mech_nam_racm(  3)     , mech_map_racm(  3)  &
    / 'API             ', 16            /
DATA  conv_fac_racm(  3) / 1.000         /

DATA  spmh_nam_racm(  4)     , spmh_map_racm(  4)  &
    / 'limonene        ', 4             /
DATA  mech_nam_racm(  4)     , mech_map_racm(  4)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm(  4) / 1.000         /

DATA  spmh_nam_racm(  5)     , spmh_map_racm(  5)  &
    / 'carene_3        ', 5             /
DATA  mech_nam_racm(  5)     , mech_map_racm(  5)  &
    / 'API             ', 16            /
DATA  conv_fac_racm(  5) / 1.000         /

DATA  spmh_nam_racm(  6)     , spmh_map_racm(  6)  &
    / 'ocimene_t_b     ', 6             /
DATA  mech_nam_racm(  6)     , mech_map_racm(  6)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm(  6) / 1.000         /

DATA  spmh_nam_racm(  7)     , spmh_map_racm(  7)  &
    / 'pinene_b        ', 7             /
DATA  mech_nam_racm(  7)     , mech_map_racm(  7)  &
    / 'API             ', 16            /
DATA  conv_fac_racm(  7) / 1.000         /

DATA  spmh_nam_racm(  8)     , spmh_map_racm(  8)  &
    / 'pinene_a        ', 8             /
DATA  mech_nam_racm(  8)     , mech_map_racm(  8)  &
    / 'API             ', 16            /
DATA  conv_fac_racm(  8) / 1.000         /

DATA  spmh_nam_racm(  9)     , spmh_map_racm(  9)  &
    / 'A_2met_styrene    ', 9             /
DATA  mech_nam_racm(  9)     , mech_map_racm(  9)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm(  9) / 1.000         /

DATA  spmh_nam_racm( 10)     , spmh_map_racm( 10)  &
    / 'cymene_p        ', 10            /
DATA  mech_nam_racm( 10)     , mech_map_racm( 10)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 10) / 1.000         /

DATA  spmh_nam_racm( 11)     , spmh_map_racm( 11)  &
    / 'cymene_o        ', 11            /
DATA  mech_nam_racm( 11)     , mech_map_racm( 11)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 11) / 1.000         /

DATA  spmh_nam_racm( 12)     , spmh_map_racm( 12)  &
    / 'phellandrene_a  ', 12            /
DATA  mech_nam_racm( 12)     , mech_map_racm( 12)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 12) / 1.000         /

DATA  spmh_nam_racm( 13)     , spmh_map_racm( 13)  &
    / 'thujene_a       ', 13            /
DATA  mech_nam_racm( 13)     , mech_map_racm( 13)  &
    / 'API             ', 16            /
DATA  conv_fac_racm( 13) / 1.000         /

DATA  spmh_nam_racm( 14)     , spmh_map_racm( 14)  &
    / 'terpinene_a     ', 14            /
DATA  mech_nam_racm( 14)     , mech_map_racm( 14)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 14) / 1.000         /

DATA  spmh_nam_racm( 15)     , spmh_map_racm( 15)  &
    / 'terpinene_g     ', 15            /
DATA  mech_nam_racm( 15)     , mech_map_racm( 15)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 15) / 1.000         /

DATA  spmh_nam_racm( 16)     , spmh_map_racm( 16)  &
    / 'terpinolene     ', 16            /
DATA  mech_nam_racm( 16)     , mech_map_racm( 16)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 16) / 1.000         /

DATA  spmh_nam_racm( 17)     , spmh_map_racm( 17)  &
    / 'phellandrene_b  ', 17            /
DATA  mech_nam_racm( 17)     , mech_map_racm( 17)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 17) / 1.000         /

DATA  spmh_nam_racm( 18)     , spmh_map_racm( 18)  &
    / 'camphene        ', 18            /
DATA  mech_nam_racm( 18)     , mech_map_racm( 18)  &
    / 'API             ', 16            /
DATA  conv_fac_racm( 18) / 1.000         /

DATA  spmh_nam_racm( 19)     , spmh_map_racm( 19)  &
    / 'bornene         ', 19            /
DATA  mech_nam_racm( 19)     , mech_map_racm( 19)  &
    / 'API             ', 16            /
DATA  conv_fac_racm( 19) / 1.000         /

DATA  spmh_nam_racm( 20)     , spmh_map_racm( 20)  &
    / 'fenchene_a      ', 20            /
DATA  mech_nam_racm( 20)     , mech_map_racm( 20)  &
    / 'API             ', 16            /
DATA  conv_fac_racm( 20) / 1.000         /

DATA  spmh_nam_racm( 21)     , spmh_map_racm( 21)  &
    / 'ocimene_al      ', 21            /
DATA  mech_nam_racm( 21)     , mech_map_racm( 21)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 21) / 1.000         /

DATA  spmh_nam_racm( 22)     , spmh_map_racm( 22)  &
    / 'ocimene_c_b     ', 22            /
DATA  mech_nam_racm( 22)     , mech_map_racm( 22)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 22) / 1.000         /

DATA  spmh_nam_racm( 23)     , spmh_map_racm( 23)  &
    / 'tricyclene      ', 23            /
DATA  mech_nam_racm( 23)     , mech_map_racm( 23)  &
    / 'NR              ', 23            /
DATA  conv_fac_racm( 23) / 1.000         /

DATA  spmh_nam_racm( 24)     , spmh_map_racm( 24)  &
    / 'estragole       ', 24            /
DATA  mech_nam_racm( 24)     , mech_map_racm( 24)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 24) / 1.000         /

DATA  spmh_nam_racm( 25)     , spmh_map_racm( 25)  &
    / 'camphor         ', 25            /
DATA  mech_nam_racm( 25)     , mech_map_racm( 25)  &
    / 'HC8             ', 6             /
DATA  conv_fac_racm( 25) / 0.380         /

DATA  spmh_nam_racm( 26)     , spmh_map_racm( 26)  &
    / 'fenchone        ', 26            /
DATA  mech_nam_racm( 26)     , mech_map_racm( 26)  &
    / 'NR              ', 23            /
DATA  conv_fac_racm( 26) / 1.000         /

DATA  spmh_nam_racm( 27)     , spmh_map_racm( 27)  &
    / 'piperitone      ', 27            /
DATA  mech_nam_racm( 27)     , mech_map_racm( 27)  &
    / 'API             ', 16            /
DATA  conv_fac_racm( 27) / 1.000         /

DATA  spmh_nam_racm( 28)     , spmh_map_racm( 28)  &
    / 'thujone_a       ', 28            /
DATA  mech_nam_racm( 28)     , mech_map_racm( 28)  &
    / 'NR              ', 23            /
DATA  conv_fac_racm( 28) / 1.000         /

DATA  spmh_nam_racm( 29)     , spmh_map_racm( 29)  &
    / 'thujone_b       ', 29            /
DATA  mech_nam_racm( 29)     , mech_map_racm( 29)  &
    / 'NR              ', 23            /
DATA  conv_fac_racm( 29) / 1.000         /

DATA  spmh_nam_racm( 30)     , spmh_map_racm( 30)  &
    / 'cineole_1_8     ', 30            /
DATA  mech_nam_racm( 30)     , mech_map_racm( 30)  &
    / 'HC8             ', 6             /
DATA  conv_fac_racm( 30) / 0.738         /

DATA  spmh_nam_racm( 31)     , spmh_map_racm( 31)  &
    / 'borneol         ', 31            /
DATA  mech_nam_racm( 31)     , mech_map_racm( 31)  &
    / 'NR              ', 23            /
DATA  conv_fac_racm( 31) / 1.000         /

DATA  spmh_nam_racm( 32)     , spmh_map_racm( 32)  &
    / 'linalool        ', 32            /
DATA  mech_nam_racm( 32)     , mech_map_racm( 32)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 32) / 1.000         /

DATA  spmh_nam_racm( 33)     , spmh_map_racm( 33)  &
    / 'terpineol_4     ', 33            /
DATA  mech_nam_racm( 33)     , mech_map_racm( 33)  &
    / 'API             ', 16            /
DATA  conv_fac_racm( 33) / 1.000         /

DATA  spmh_nam_racm( 34)     , spmh_map_racm( 34)  &
    / 'terpineol_a     ', 34            /
DATA  mech_nam_racm( 34)     , mech_map_racm( 34)  &
    / 'API             ', 16            /
DATA  conv_fac_racm( 34) / 1.000         /

DATA  spmh_nam_racm( 35)     , spmh_map_racm( 35)  &
    / 'linalool_OXD_c  ', 35            /
DATA  mech_nam_racm( 35)     , mech_map_racm( 35)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 35) / 1.000         /

DATA  spmh_nam_racm( 36)     , spmh_map_racm( 36)  &
    / 'linalool_OXD_t  ', 36            /
DATA  mech_nam_racm( 36)     , mech_map_racm( 36)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 36) / 1.000         /

DATA  spmh_nam_racm( 37)     , spmh_map_racm( 37)  &
    / 'ionone_b        ', 37            /
DATA  mech_nam_racm( 37)     , mech_map_racm( 37)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 37) / 1.000         /

DATA  spmh_nam_racm( 38)     , spmh_map_racm( 38)  &
    / 'bornyl_ACT      ', 38            /
DATA  mech_nam_racm( 38)     , mech_map_racm( 38)  &
    / 'NR              ', 23            /
DATA  conv_fac_racm( 38) / 1.000         /

DATA  spmh_nam_racm( 39)     , spmh_map_racm( 39)  &
    / 'farnescene_a    ', 39            /
DATA  mech_nam_racm( 39)     , mech_map_racm( 39)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 39) / 1.000         /

DATA  spmh_nam_racm( 40)     , spmh_map_racm( 40)  &
    / 'caryophyllene_b ', 40            /
DATA  mech_nam_racm( 40)     , mech_map_racm( 40)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 40) / 1.000         /

DATA  spmh_nam_racm( 41)     , spmh_map_racm( 41)  &
    / 'acoradiene      ', 41            /
DATA  mech_nam_racm( 41)     , mech_map_racm( 41)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 41) / 1.000         /

DATA  spmh_nam_racm( 42)     , spmh_map_racm( 42)  &
    / 'aromadendrene   ', 42            /
DATA  mech_nam_racm( 42)     , mech_map_racm( 42)  &
    / 'API             ', 16            /
DATA  conv_fac_racm( 42) / 1.000         /

DATA  spmh_nam_racm( 43)     , spmh_map_racm( 43)  &
    / 'bergamotene_a   ', 43            /
DATA  mech_nam_racm( 43)     , mech_map_racm( 43)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 43) / 1.000         /

DATA  spmh_nam_racm( 44)     , spmh_map_racm( 44)  &
    / 'bergamotene_b   ', 44            /
DATA  mech_nam_racm( 44)     , mech_map_racm( 44)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 44) / 1.000         /

DATA  spmh_nam_racm( 45)     , spmh_map_racm( 45)  &
    / 'bisabolene_a    ', 45            /
DATA  mech_nam_racm( 45)     , mech_map_racm( 45)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 45) / 1.000         /

DATA  spmh_nam_racm( 46)     , spmh_map_racm( 46)  &
    / 'bisabolene_b    ', 46            /
DATA  mech_nam_racm( 46)     , mech_map_racm( 46)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 46) / 1.000         /

DATA  spmh_nam_racm( 47)     , spmh_map_racm( 47)  &
    / 'bourbonene_b    ', 47            /
DATA  mech_nam_racm( 47)     , mech_map_racm( 47)  &
    / 'API             ', 16            /
DATA  conv_fac_racm( 47) / 1.000         /

DATA  spmh_nam_racm( 48)     , spmh_map_racm( 48)  &
    / 'cadinene_d      ', 48            /
DATA  mech_nam_racm( 48)     , mech_map_racm( 48)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 48) / 1.000         /

DATA  spmh_nam_racm( 49)     , spmh_map_racm( 49)  &
    / 'cadinene_g      ', 49            /
DATA  mech_nam_racm( 49)     , mech_map_racm( 49)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 49) / 1.000         /

DATA  spmh_nam_racm( 50)     , spmh_map_racm( 50)  &
    / 'cedrene_a       ', 50            /
DATA  mech_nam_racm( 50)     , mech_map_racm( 50)  &
    / 'API             ', 16            /
DATA  conv_fac_racm( 50) / 1.000         /

DATA  spmh_nam_racm( 51)     , spmh_map_racm( 51)  &
    / 'copaene_a       ', 51            /
DATA  mech_nam_racm( 51)     , mech_map_racm( 51)  &
    / 'API             ', 16            /
DATA  conv_fac_racm( 51) / 1.000         /

DATA  spmh_nam_racm( 52)     , spmh_map_racm( 52)  &
    / 'cubebene_a      ', 52            /
DATA  mech_nam_racm( 52)     , mech_map_racm( 52)  &
    / 'API             ', 16            /
DATA  conv_fac_racm( 52) / 1.000         /

DATA  spmh_nam_racm( 53)     , spmh_map_racm( 53)  &
    / 'cubebene_b      ', 53            /
DATA  mech_nam_racm( 53)     , mech_map_racm( 53)  &
    / 'API             ', 16            /
DATA  conv_fac_racm( 53) / 1.000         /

DATA  spmh_nam_racm( 54)     , spmh_map_racm( 54)  &
    / 'elemene_b       ', 54            /
DATA  mech_nam_racm( 54)     , mech_map_racm( 54)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 54) / 1.000         /

DATA  spmh_nam_racm( 55)     , spmh_map_racm( 55)  &
    / 'farnescene_b    ', 55            /
DATA  mech_nam_racm( 55)     , mech_map_racm( 55)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 55) / 1.000         /

DATA  spmh_nam_racm( 56)     , spmh_map_racm( 56)  &
    / 'germacrene_B    ', 56            /
DATA  mech_nam_racm( 56)     , mech_map_racm( 56)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 56) / 1.000         /

DATA  spmh_nam_racm( 57)     , spmh_map_racm( 57)  &
    / 'germacrene_D    ', 57            /
DATA  mech_nam_racm( 57)     , mech_map_racm( 57)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 57) / 1.000         /

DATA  spmh_nam_racm( 58)     , spmh_map_racm( 58)  &
    / 'gurjunene_b     ', 58            /
DATA  mech_nam_racm( 58)     , mech_map_racm( 58)  &
    / 'API             ', 16            /
DATA  conv_fac_racm( 58) / 1.000         /

DATA  spmh_nam_racm( 59)     , spmh_map_racm( 59)  &
    / 'humulene_a      ', 59            /
DATA  mech_nam_racm( 59)     , mech_map_racm( 59)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 59) / 1.000         /

DATA  spmh_nam_racm( 60)     , spmh_map_racm( 60)  &
    / 'humulene_g      ', 60            /
DATA  mech_nam_racm( 60)     , mech_map_racm( 60)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 60) / 1.000         /

DATA  spmh_nam_racm( 61)     , spmh_map_racm( 61)  &
    / 'isolongifolene  ', 61            /
DATA  mech_nam_racm( 61)     , mech_map_racm( 61)  &
    / 'API             ', 16            /
DATA  conv_fac_racm( 61) / 1.000         /

DATA  spmh_nam_racm( 62)     , spmh_map_racm( 62)  &
    / 'longifolene     ', 62            /
DATA  mech_nam_racm( 62)     , mech_map_racm( 62)  &
    / 'API             ', 16            /
DATA  conv_fac_racm( 62) / 1.000         /

DATA  spmh_nam_racm( 63)     , spmh_map_racm( 63)  &
    / 'longipinene     ', 63            /
DATA  mech_nam_racm( 63)     , mech_map_racm( 63)  &
    / 'API             ', 16            /
DATA  conv_fac_racm( 63) / 1.000         /

DATA  spmh_nam_racm( 64)     , spmh_map_racm( 64)  &
    / 'muurolene_a     ', 64            /
DATA  mech_nam_racm( 64)     , mech_map_racm( 64)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 64) / 1.000         /

DATA  spmh_nam_racm( 65)     , spmh_map_racm( 65)  &
    / 'muurolene_g     ', 65            /
DATA  mech_nam_racm( 65)     , mech_map_racm( 65)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 65) / 1.000         /

DATA  spmh_nam_racm( 66)     , spmh_map_racm( 66)  &
    / 'selinene_b      ', 66            /
DATA  mech_nam_racm( 66)     , mech_map_racm( 66)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 66) / 1.000         /

DATA  spmh_nam_racm( 67)     , spmh_map_racm( 67)  &
    / 'selinene_d      ', 67            /
DATA  mech_nam_racm( 67)     , mech_map_racm( 67)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 67) / 1.000         /

DATA  spmh_nam_racm( 68)     , spmh_map_racm( 68)  &
    / 'nerolidol_c     ', 68            /
DATA  mech_nam_racm( 68)     , mech_map_racm( 68)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 68) / 1.000         /

DATA  spmh_nam_racm( 69)     , spmh_map_racm( 69)  &
    / 'nerolidol_t     ', 69            /
DATA  mech_nam_racm( 69)     , mech_map_racm( 69)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 69) / 1.000         /

DATA  spmh_nam_racm( 70)     , spmh_map_racm( 70)  &
    / 'cedrol          ', 70            /
DATA  mech_nam_racm( 70)     , mech_map_racm( 70)  &
    / 'NR              ', 23            /
DATA  conv_fac_racm( 70) / 1.000         /

DATA  spmh_nam_racm( 71)     , spmh_map_racm( 71)  &
    / 'MBO_2m3e2ol     ', 71            /
DATA  mech_nam_racm( 71)     , mech_map_racm( 71)  &
    / 'ISO             ', 1             /
DATA  conv_fac_racm( 71) / 1.000         /

DATA  spmh_nam_racm( 72)     , spmh_map_racm( 72)  &
    / 'methanol        ', 72            /
DATA  mech_nam_racm( 72)     , mech_map_racm( 72)  &
    / 'HC3             ', 4             /
DATA  conv_fac_racm( 72) / 0.49          /

DATA  spmh_nam_racm( 73)     , spmh_map_racm( 73)  &
    / 'acetone         ', 73            /
DATA  mech_nam_racm( 73)     , mech_map_racm( 73)  &
    / 'KET             ', 11            /
DATA  conv_fac_racm( 73) / 0.33          /

DATA  spmh_nam_racm( 74)     , spmh_map_racm( 74)  &
    / 'methane         ', 74            /
DATA  mech_nam_racm( 74)     , mech_map_racm( 74)  &
    / 'CH4             ', 2             /
DATA  conv_fac_racm( 74) / 1.000         /

DATA  spmh_nam_racm( 75)     , spmh_map_racm( 75)  &
    / 'ammonia         ', 75            /
DATA  mech_nam_racm( 75)     , mech_map_racm( 75)  &
    / 'NR              ', 23            /
DATA  conv_fac_racm( 75) / 1.000         /

DATA  spmh_nam_racm( 76)     , spmh_map_racm( 76)  &
    / 'nitrous_OXD     ', 76            /
DATA  mech_nam_racm( 76)     , mech_map_racm( 76)  &
    / 'NO2             ', 22            /
DATA  conv_fac_racm( 76) / 1.000         /

DATA  spmh_nam_racm( 77)     , spmh_map_racm( 77)  &
    / 'nitric_OXD      ', 77            /
DATA  mech_nam_racm( 77)     , mech_map_racm( 77)  &
    / 'NO              ', 20            /
DATA  conv_fac_racm( 77) / 1.000         /

DATA  spmh_nam_racm( 78)     , spmh_map_racm( 78)  &
    / 'acetaldehyde    ', 78            /
DATA  mech_nam_racm( 78)     , mech_map_racm( 78)  &
    / 'ALD             ', 10            /
DATA  conv_fac_racm( 78) / 1.000         /

DATA  spmh_nam_racm( 79)     , spmh_map_racm( 79)  &
    / 'ethanol         ', 79            /
DATA  mech_nam_racm( 79)     , mech_map_racm( 79)  &
    / 'HC3             ', 4             /
DATA  conv_fac_racm( 79) / 1.37          /

DATA  spmh_nam_racm( 80)     , spmh_map_racm( 80)  &
    / 'formic_acid     ', 80            /
DATA  mech_nam_racm( 80)     , mech_map_racm( 80)  &
    / 'ORA1            ', 14            /
DATA  conv_fac_racm( 80) / 1.000         /

DATA  spmh_nam_racm( 81)     , spmh_map_racm( 81)  &
    / 'formaldehyde    ', 81            /
DATA  mech_nam_racm( 81)     , mech_map_racm( 81)  &
    / 'HCHO            ', 13            /
DATA  conv_fac_racm( 81) / 1.000         /

DATA  spmh_nam_racm( 82)     , spmh_map_racm( 82)  &
    / 'acetic_acid     ', 82            /
DATA  mech_nam_racm( 82)     , mech_map_racm( 82)  &
    / 'ORA2            ', 15            /
DATA  conv_fac_racm( 82) / 1.000         /

DATA  spmh_nam_racm( 83)     , spmh_map_racm( 83)  &
    / 'MBO_3m2e1ol     ', 83            /
DATA  mech_nam_racm( 83)     , mech_map_racm( 83)  &
    / 'ISO             ', 1             /
DATA  conv_fac_racm( 83) / 1.000         /

DATA  spmh_nam_racm( 84)     , spmh_map_racm( 84)  &
    / 'MBO_3m3e1ol     ', 84            /
DATA  mech_nam_racm( 84)     , mech_map_racm( 84)  &
    / 'ISO             ', 1             /
DATA  conv_fac_racm( 84) / 1.000         /

DATA  spmh_nam_racm( 85)     , spmh_map_racm( 85)  &
    / 'benzaldehyde    ', 85            /
DATA  mech_nam_racm( 85)     , mech_map_racm( 85)  &
    / 'NR              ', 23            /
DATA  conv_fac_racm( 85) / 1.000         /

DATA  spmh_nam_racm( 86)     , spmh_map_racm( 86)  &
    / 'butanone_2      ', 86            /
DATA  mech_nam_racm( 86)     , mech_map_racm( 86)  &
    / 'KET             ', 11            /
DATA  conv_fac_racm( 86) / 1.61          /

DATA  spmh_nam_racm( 87)     , spmh_map_racm( 87)  &
    / 'decanal         ', 87            /
DATA  mech_nam_racm( 87)     , mech_map_racm( 87)  &
    / 'ALD             ', 10            /
DATA  conv_fac_racm( 87) / 1.000         /

DATA  spmh_nam_racm( 88)     , spmh_map_racm( 88)  &
    / 'dodecene_1      ', 88            /
DATA  mech_nam_racm( 88)     , mech_map_racm( 88)  &
    / 'OLT             ', 9             /
DATA  conv_fac_racm( 88) / 1.000         /

DATA  spmh_nam_racm( 89)     , spmh_map_racm( 89)  &
    / 'geranyl_acetone ', 89            /
DATA  mech_nam_racm( 89)     , mech_map_racm( 89)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm( 89) / 1.000         /

DATA  spmh_nam_racm( 90)     , spmh_map_racm( 90)  &
    / 'heptanal        ', 90            /
DATA  mech_nam_racm( 90)     , mech_map_racm( 90)  &
    / 'ALD             ', 10            /
DATA  conv_fac_racm( 90) / 1.000         /

DATA  spmh_nam_racm( 91)     , spmh_map_racm( 91)  &
    / 'heptane         ', 91            /
DATA  mech_nam_racm( 91)     , mech_map_racm( 91)  &
    / 'HC5             ', 5             /
DATA  conv_fac_racm( 91) / 1.236         /

DATA  spmh_nam_racm( 92)     , spmh_map_racm( 92)  &
    / 'hexane          ', 92            /
DATA  mech_nam_racm( 92)     , mech_map_racm( 92)  &
    / 'HC5             ', 5             /
DATA  conv_fac_racm( 92) / 1.058         /

DATA  spmh_nam_racm( 93)     , spmh_map_racm( 93)  &
    / 'met_benzoate    ', 93            /
DATA  mech_nam_racm( 93)     , mech_map_racm( 93)  &
    / 'HC8             ', 6             /
DATA  conv_fac_racm( 93) / 1.000         /

DATA  spmh_nam_racm( 94)     , spmh_map_racm( 94)  &
    / 'met_heptenone   ', 94            /
DATA  mech_nam_racm( 94)     , mech_map_racm( 94)  &
    / 'OLI             ', 8             /
DATA  conv_fac_racm( 94) / 1.000         /

DATA  spmh_nam_racm( 95)     , spmh_map_racm( 95)  &
    / 'neryl_acetone   ', 95            /
DATA  mech_nam_racm( 95)     , mech_map_racm( 95)  &
    / 'OLI             ', 8             /
DATA  conv_fac_racm( 95) / 1.000         /

DATA  spmh_nam_racm( 96)     , spmh_map_racm( 96)  &
    / 'nonanal         ', 96            /
DATA  mech_nam_racm( 96)     , mech_map_racm( 96)  &
    / 'ALD             ', 10            /
DATA  conv_fac_racm( 96) / 1.000         /

DATA  spmh_nam_racm( 97)     , spmh_map_racm( 97)  &
    / 'nonenal         ', 97            /
DATA  mech_nam_racm( 97)     , mech_map_racm( 97)  &
    / 'HC8             ', 6             /
DATA  conv_fac_racm( 97) / 1.000         /

DATA  spmh_nam_racm( 98)     , spmh_map_racm( 98)  &
    / 'nonenal         ', 97            /
DATA  mech_nam_racm( 98)     , mech_map_racm( 98)  &
    / 'ALD             ', 10            /
DATA  conv_fac_racm( 98) / 1.000         /

DATA  spmh_nam_racm( 99)     , spmh_map_racm( 99)  &
    / 'octanal         ', 98            /
DATA  mech_nam_racm( 99)     , mech_map_racm( 99)  &
    / 'ALD             ', 10            /
DATA  conv_fac_racm( 99) / 1.000         /

DATA  spmh_nam_racm(100)     , spmh_map_racm(100)  &
    / 'octanol         ', 99            /
DATA  mech_nam_racm(100)     , mech_map_racm(100)  &
    / 'HC8             ', 6             /
DATA  conv_fac_racm(100) / 1.092         /

DATA  spmh_nam_racm(101)     , spmh_map_racm(101)  &
    / 'octenol_1e3ol   ', 100           /
DATA  mech_nam_racm(101)     , mech_map_racm(101)  &
    / 'OLT             ', 9             /
DATA  conv_fac_racm(101) / 1.000         /

DATA  spmh_nam_racm(102)     , spmh_map_racm(102)  &
    / 'oxopentanal     ', 101           /
DATA  mech_nam_racm(102)     , mech_map_racm(102)  &
    / 'ALD             ', 10            /
DATA  conv_fac_racm(102) / 1.000         /

DATA  spmh_nam_racm(103)     , spmh_map_racm(103)  &
    / 'pentane         ', 102           /
DATA  mech_nam_racm(103)     , mech_map_racm(103)  &
    / 'HC5             ', 5             /
DATA  conv_fac_racm(103) / 0.854         /

DATA  spmh_nam_racm(104)     , spmh_map_racm(104)  &
    / 'phenyl_CCO      ', 103           /
DATA  mech_nam_racm(104)     , mech_map_racm(104)  &
    / 'NR              ', 23            /
DATA  conv_fac_racm(104) / 1.000         /

DATA  spmh_nam_racm(105)     , spmh_map_racm(105)  &
    / 'pyruvic_acid    ', 104           /
DATA  mech_nam_racm(105)     , mech_map_racm(105)  &
    / 'ORA2            ', 15            /
DATA  conv_fac_racm(105) / 1.000         /

DATA  spmh_nam_racm(106)     , spmh_map_racm(106)  &
    / 'terpinyl_ACT_a  ', 105           /
DATA  mech_nam_racm(106)     , mech_map_racm(106)  &
    / 'API             ', 16            /
DATA  conv_fac_racm(106) / 1.000         /

DATA  spmh_nam_racm(107)     , spmh_map_racm(107)  &
    / 'tetradecene_1   ', 106           /
DATA  mech_nam_racm(107)     , mech_map_racm(107)  &
    / 'OLT             ', 9             /
DATA  conv_fac_racm(107) / 1.000         /

DATA  spmh_nam_racm(108)     , spmh_map_racm(108)  &
    / 'toluene         ', 107           /
DATA  mech_nam_racm(108)     , mech_map_racm(108)  &
    / 'TOL             ', 12            /
DATA  conv_fac_racm(108) / 1.000         /

DATA  spmh_nam_racm(109)     , spmh_map_racm(109)  &
    / 'carbon_monoxide ', 108           /
DATA  mech_nam_racm(109)     , mech_map_racm(109)  &
    / 'CO              ', 18            /
DATA  conv_fac_racm(109) / 1.000         /

DATA  spmh_nam_racm(110)     , spmh_map_racm(110)  &
    / 'butene          ', 109           /
DATA  mech_nam_racm(110)     , mech_map_racm(110)  &
    / 'OLT             ', 9             /
DATA  conv_fac_racm(110) / 1.000         /

DATA  spmh_nam_racm(111)     , spmh_map_racm(111)  &
    / 'ethane          ', 110           /
DATA  mech_nam_racm(111)     , mech_map_racm(111)  &
    / 'ETH             ', 3             /
DATA  conv_fac_racm(111) / 1.000         /

DATA  spmh_nam_racm(112)     , spmh_map_racm(112)  &
    / 'ethene          ', 111           /
DATA  mech_nam_racm(112)     , mech_map_racm(112)  &
    / 'ETE             ', 7             /
DATA  conv_fac_racm(112) / 1.000         /

DATA  spmh_nam_racm(113)     , spmh_map_racm(113)  &
    / 'hydrogen_cyanide', 112           /
DATA  mech_nam_racm(113)     , mech_map_racm(113)  &
    / 'NR              ', 23            /
DATA  conv_fac_racm(113) / 1.000         /

DATA  spmh_nam_racm(114)     , spmh_map_racm(114)  &
    / 'propane         ', 113           /
DATA  mech_nam_racm(114)     , mech_map_racm(114)  &
    / 'HC3             ', 4             /
DATA  conv_fac_racm(114) / 0.57          /

DATA  spmh_nam_racm(115)     , spmh_map_racm(115)  &
    / 'propene         ', 114           /
DATA  mech_nam_racm(115)     , mech_map_racm(115)  &
    / 'OLT             ', 9             /
DATA  conv_fac_racm(115) / 1.000         /

DATA  spmh_nam_racm(116)     , spmh_map_racm(116)  &
    / 'carbon_2s       ', 115           /
DATA  mech_nam_racm(116)     , mech_map_racm(116)  &
    / 'NR              ', 23            /
DATA  conv_fac_racm(116) / 1.000         /

DATA  spmh_nam_racm(117)     , spmh_map_racm(117)  &
    / 'carbonyl_s      ', 116           /
DATA  mech_nam_racm(117)     , mech_map_racm(117)  &
    / 'NR              ', 23            /
DATA  conv_fac_racm(117) / 1.000         /

DATA  spmh_nam_racm(118)     , spmh_map_racm(118)  &
    / 'diallyl_2s      ', 117           /
DATA  mech_nam_racm(118)     , mech_map_racm(118)  &
    / 'OLI             ', 8             /
DATA  conv_fac_racm(118) / 1.000         /

DATA  spmh_nam_racm(119)     , spmh_map_racm(119)  &
    / 'diallyl_2s      ', 117           /
DATA  mech_nam_racm(119)     , mech_map_racm(119)  &
    / 'SO2             ', 19            /
DATA  conv_fac_racm(119) / 2             /

DATA  spmh_nam_racm(120)     , spmh_map_racm(120)  &
    / 'A_2met_2s         ', 118           /
DATA  mech_nam_racm(120)     , mech_map_racm(120)  &
    / 'ETH             ', 3             /
DATA  conv_fac_racm(120) / 1.000         /

DATA  spmh_nam_racm(121)     , spmh_map_racm(121)  &
    / 'A_2met_2s         ', 118           /
DATA  mech_nam_racm(121)     , mech_map_racm(121)  &
    / 'SO2             ', 19            /
DATA  conv_fac_racm(121) / 2             /

DATA  spmh_nam_racm(122)     , spmh_map_racm(122)  &
    / 'A_2met_s          ', 119           /
DATA  mech_nam_racm(122)     , mech_map_racm(122)  &
    / 'ETH             ', 3             /
DATA  conv_fac_racm(122) / 1.000         /

DATA  spmh_nam_racm(123)     , spmh_map_racm(123)  &
    / 'A_2met_s          ', 119           /
DATA  mech_nam_racm(123)     , mech_map_racm(123)  &
    / 'SO2             ', 19            /
DATA  conv_fac_racm(123) / 1.000         /

DATA  spmh_nam_racm(124)     , spmh_map_racm(124)  &
    / 'met_chloride    ', 120           /
DATA  mech_nam_racm(124)     , mech_map_racm(124)  &
    / 'NR              ', 23            /
DATA  conv_fac_racm(124) / 1.000         /

DATA  spmh_nam_racm(125)     , spmh_map_racm(125)  &
    / 'met_bromide     ', 121           /
DATA  mech_nam_racm(125)     , mech_map_racm(125)  &
    / 'NR              ', 23            /
DATA  conv_fac_racm(125) / 1.000         /

DATA  spmh_nam_racm(126)     , spmh_map_racm(126)  &
    / 'met_iodide      ', 122           /
DATA  mech_nam_racm(126)     , mech_map_racm(126)  &
    / 'NR              ', 23            /
DATA  conv_fac_racm(126) / 1.000         /

DATA  spmh_nam_racm(127)     , spmh_map_racm(127)  &
    / 'hydrogen_s      ', 123           /
DATA  mech_nam_racm(127)     , mech_map_racm(127)  &
    / 'SO2             ', 19            /
DATA  conv_fac_racm(127) / 1.000         /

DATA  spmh_nam_racm(128)     , spmh_map_racm(128)  &
    / 'met_mercaptan   ', 124           /
DATA  mech_nam_racm(128)     , mech_map_racm(128)  &
    / 'CH4             ', 2             /
DATA  conv_fac_racm(128) / 1.000         /

DATA  spmh_nam_racm(129)     , spmh_map_racm(129)  &
    / 'met_mercaptan   ', 124           /
DATA  mech_nam_racm(129)     , mech_map_racm(129)  &
    / 'SO2             ', 19            /
DATA  conv_fac_racm(129) / 1.000         /

DATA  spmh_nam_racm(130)     , spmh_map_racm(130)  &
    / 'met_propenyl_2s ', 125           /
DATA  mech_nam_racm(130)     , mech_map_racm(130)  &
    / 'OLI             ', 8             /
DATA  conv_fac_racm(130) / 1.000         /

DATA  spmh_nam_racm(131)     , spmh_map_racm(131)  &
    / 'met_propenyl_2s ', 125           /
DATA  mech_nam_racm(131)     , mech_map_racm(131)  &
    / 'SO2             ', 19            /
DATA  conv_fac_racm(131) / 2             /

DATA  spmh_nam_racm(132)     , spmh_map_racm(132)  &
    / 'PPPP_2s         ', 126           /
DATA  mech_nam_racm(132)     , mech_map_racm(132)  &
    / 'OLI             ', 8             /
DATA  conv_fac_racm(132) / 1.000         /

DATA  spmh_nam_racm(133)     , spmh_map_racm(133)  &
    / 'PPPP_2s         ', 126           /
DATA  mech_nam_racm(133)     , mech_map_racm(133)  &
    / 'SO2             ', 19            /
DATA  conv_fac_racm(133) / 2             /

DATA  spmh_nam_racm(134)     , spmh_map_racm(134)  &
    / 'A_2met_nonatriene ', 127           /
DATA  mech_nam_racm(134)     , mech_map_racm(134)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm(134) / 1.000         /

DATA  spmh_nam_racm(135)     , spmh_map_racm(135)  &
    / 'met_salicylate  ', 128           /
DATA  mech_nam_racm(135)     , mech_map_racm(135)  &
    / 'HC8             ', 6             /
DATA  conv_fac_racm(135) / 1.000         /

DATA  spmh_nam_racm(136)     , spmh_map_racm(136)  &
    / 'indole          ', 129           /
DATA  mech_nam_racm(136)     , mech_map_racm(136)  &
    / 'HC8             ', 6             /
DATA  conv_fac_racm(136) / 1.201         /

DATA  spmh_nam_racm(137)     , spmh_map_racm(137)  &
    / 'indole          ', 129           /
DATA  mech_nam_racm(137)     , mech_map_racm(137)  &
    / 'HNO3            ', 21            /
DATA  conv_fac_racm(137) / 1.000         /

DATA  spmh_nam_racm(138)     , spmh_map_racm(138)  &
    / 'jasmone         ', 130           /
DATA  mech_nam_racm(138)     , mech_map_racm(138)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm(138) / 1.000         /

DATA  spmh_nam_racm(139)     , spmh_map_racm(139)  &
    / 'met_jasmonate   ', 131           /
DATA  mech_nam_racm(139)     , mech_map_racm(139)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm(139) / 1.000         /

DATA  spmh_nam_racm(140)     , spmh_map_racm(140)  &
    / 'A_3met_3DCTT      ', 132           /
DATA  mech_nam_racm(140)     , mech_map_racm(140)  &
    / 'OLI             ', 8             /
DATA  conv_fac_racm(140) / 0.5           /

DATA  spmh_nam_racm(141)     , spmh_map_racm(141)  &
    / 'A_3met_3DCTT      ', 132           /
DATA  mech_nam_racm(141)     , mech_map_racm(141)  &
    / 'OLT             ', 9             /
DATA  conv_fac_racm(141) / 0.5           /

DATA  spmh_nam_racm(142)     , spmh_map_racm(142)  &
    / 'hexanal         ', 133           /
DATA  mech_nam_racm(142)     , mech_map_racm(142)  &
    / 'ALD             ', 10            /
DATA  conv_fac_racm(142) / 1.000         /

DATA  spmh_nam_racm(143)     , spmh_map_racm(143)  &
    / 'hexanol_1       ', 134           /
DATA  mech_nam_racm(143)     , mech_map_racm(143)  &
    / 'HC5             ', 5             /
DATA  conv_fac_racm(143) / 1.710         /

DATA  spmh_nam_racm(144)     , spmh_map_racm(144)  &
    / 'hexenal_c3      ', 135           /
DATA  mech_nam_racm(144)     , mech_map_racm(144)  &
    / 'OLI             ', 8             /
DATA  conv_fac_racm(144) / 1.000         /

DATA  spmh_nam_racm(145)     , spmh_map_racm(145)  &
    / 'hexenal_t2      ', 136           /
DATA  mech_nam_racm(145)     , mech_map_racm(145)  &
    / 'OLI             ', 8             /
DATA  conv_fac_racm(145) / 1.000         /

DATA  spmh_nam_racm(146)     , spmh_map_racm(146)  &
    / 'hexenol_c3      ', 137           /
DATA  mech_nam_racm(146)     , mech_map_racm(146)  &
    / 'OLT             ', 9             /
DATA  conv_fac_racm(146) / 1.000         /

DATA  spmh_nam_racm(147)     , spmh_map_racm(147)  &
    / 'hexenyl_ACT_c3  ', 138           /
DATA  mech_nam_racm(147)     , mech_map_racm(147)  &
    / 'OLI             ', 8             /
DATA  conv_fac_racm(147) / 1.000         /

DATA  spmh_nam_racm(148)     , spmh_map_racm(148)  &
    / 'homosalate      ', 139            /
DATA  mech_nam_racm(148)     , mech_map_racm(148)  &
    / 'LIM             ', 17             /
DATA  conv_fac_racm(148) / 1.0            /

DATA  spmh_nam_racm(149)     , spmh_map_racm(149)  &
    / 'Ehsalate        ', 140            /
DATA  mech_nam_racm(149)     , mech_map_racm(149)  &
    / 'LIM             ', 17              /
DATA  conv_fac_racm(149) / 1.0            /

DATA  spmh_nam_racm(150)     , spmh_map_racm(150)  &
    / 'pentanal         ', 141            /
DATA  mech_nam_racm(150)     , mech_map_racm(150)  &
    / 'ALD             ', 10            /
DATA  conv_fac_racm(150) / 1.0            /

DATA  spmh_nam_racm(151)     , spmh_map_racm(151)  &
    / 'heptanone      ', 142            /
DATA  mech_nam_racm(151)     , mech_map_racm(151)  &
    / 'OLI             ', 8             /
DATA  conv_fac_racm(151) / 1.0            /

DATA  spmh_nam_racm(152)     , spmh_map_racm(152)  &
    / 'anisole         ', 143            /
DATA  mech_nam_racm(152)     , mech_map_racm(152)  &
    / 'NR              ', 23            /
DATA  conv_fac_racm(152) / 1.0            /

DATA  spmh_nam_racm(153)     , spmh_map_racm(153)  &
    / 'verbenene       ', 144            /
DATA  mech_nam_racm(153)     , mech_map_racm(153)  &
    / 'LIM             ', 17              /
DATA  conv_fac_racm(153) / 1.0            /

DATA  spmh_nam_racm(154)     , spmh_map_racm(154)  &
    / 'benzyl-acetate   ', 145            /
DATA  mech_nam_racm(154)     , mech_map_racm(154)  &
    / 'NR              ', 23             /
DATA  conv_fac_racm(154) / 1.0            /

DATA  spmh_nam_racm(155)     , spmh_map_racm(155)  &
    / 'myrtenal         ', 146            /
DATA  mech_nam_racm(155)     , mech_map_racm(155)  &
    / 'LIM             ', 17            /
DATA  conv_fac_racm(155) / 1.0            /

DATA  spmh_nam_racm(156)     , spmh_map_racm(156)  &
    / 'benzyl-alcohol ', 147            /
DATA  mech_nam_racm(156)     , mech_map_racm(156)  &
    / 'NR              ', 23              /
DATA  conv_fac_racm(156) / 1.0            /

DATA  spmh_nam_racm(157)     , spmh_map_racm(157)  &
    / 'meta-cymenene  ', 148            /
DATA  mech_nam_racm(157)     , mech_map_racm(157)  &
    / 'LIM             ', 17               /
DATA  conv_fac_racm(157) / 1.0            /

DATA  spmh_nam_racm(158)     , spmh_map_racm(158)  &
    / 'ipsenol        ', 149            /
DATA  mech_nam_racm(158)     , mech_map_racm(158)  &
    / 'LIM             ', 17              /
DATA  conv_fac_racm(158) / 1.0            /

DATA  spmh_nam_racm(159)     , spmh_map_racm(159)  &
    / 'Napthalene     ', 150            /
DATA  mech_nam_racm(159)     , mech_map_racm(159)  &
    / 'HC8            ', 6               /
DATA  conv_fac_racm(159) / 1.0            /


!=======================================================================
!  SPC_RACM.EXT
!  This include file contains RACM species and their MW.


!  Mechanism Name: RACM
!  MEGAN v2.1.0
!  INPUT version 200

!  History:
!  Who          When       What
!  ---------------------------------------------------------------------
!  Tan          12/02/06 - Creates this file
!  Tan          08/14/07 - Move from MEGAN v2.0 to MEGAN v2.1.0 with no update.
!=======================================================================


DATA  mech_spc_racm(  1), mech_mwt_racm(  1) /'ISO  ', 68.0   /
DATA  mech_spc_racm(  2), mech_mwt_racm(  2) /'CH4  ', 16.0   /
DATA  mech_spc_racm(  3), mech_mwt_racm(  3) /'ETH  ', 30.0   /
DATA  mech_spc_racm(  4), mech_mwt_racm(  4) /'HC3  ', 44.0   /
DATA  mech_spc_racm(  5), mech_mwt_racm(  5) /'HC5  ', 72.0   /
DATA  mech_spc_racm(  6), mech_mwt_racm(  6) /'HC8  ', 114.0  /
DATA  mech_spc_racm(  7), mech_mwt_racm(  7) /'OL2  ', 28.0   /
DATA  mech_spc_racm(  8), mech_mwt_racm(  8) /'OLI  ', 68.0   /
DATA  mech_spc_racm(  9), mech_mwt_racm(  9) /'OLT  ', 42.0   /
DATA  mech_spc_racm( 10), mech_mwt_racm( 10) /'ALD  ', 44.0   /
DATA  mech_spc_racm( 11), mech_mwt_racm( 11) /'KET  ', 72.0   /
DATA  mech_spc_racm( 12), mech_mwt_racm( 12) /'TOL  ', 92.0   /
DATA  mech_spc_racm( 13), mech_mwt_racm( 13) /'HCHO ', 30.0   /
DATA  mech_spc_racm( 14), mech_mwt_racm( 14) /'ORA1 ', 46.0   /
DATA  mech_spc_racm( 15), mech_mwt_racm( 15) /'ORA2 ', 60.0   /
DATA  mech_spc_racm( 16), mech_mwt_racm( 16) /'API  ', 136.0  /
DATA  mech_spc_racm( 17), mech_mwt_racm( 17) /'LIM  ', 136.0  /
DATA  mech_spc_racm( 18), mech_mwt_racm( 18) /'CO   ', 28.0   /
DATA  mech_spc_racm( 19), mech_mwt_racm( 19) /'SO2  ', 64.0   /
DATA  mech_spc_racm( 20), mech_mwt_racm( 20) /'NO   ', 30.0   /
DATA  mech_spc_racm( 21), mech_mwt_racm( 21) /'HNO3 ', 63.0   /
DATA  mech_spc_racm( 22), mech_mwt_racm( 22) /'NO2  ', 46.0   /
DATA  mech_spc_racm( 23), mech_mwt_racm( 23) /'NR   ', 1.0    /

!=======================================================================
!  MAP_CV2CBMZ.EXT
!  This include file contains conversion table for 138 speciated species
!  to CBMZ species


!  MEGAN v2.1.0
!  INPUT version 200

!  History:
!  Who          When       What
!  ---------------------------------------------------------------------
!  Tan          12/02/06 - Creates this file
!  Tan          08/14/07 - Move from MEGAN v2.0 to MEGAN v2.1.0 with no update.
!=======================================================================

DATA  spmh_nam_cbmz(  1)     , spmh_map_cbmz(  1)  &
    / 'isoprene        ', 1              /
DATA  mech_nam_cbmz(  1)     , mech_map_cbmz(  1)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz(  1) / 1.0            /
DATA  spmh_nam_cbmz(  2)     , spmh_map_cbmz(  2)  &
    / 'myrcene         ', 2              /
DATA  mech_nam_cbmz(  2)     , mech_map_cbmz(  2)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz(  2) / 2.0            /
DATA  spmh_nam_cbmz(  3)     , spmh_map_cbmz(  3)  &
    / 'sabinene        ', 3              /
DATA  mech_nam_cbmz(  3)     , mech_map_cbmz(  3)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz(  3) / 2.0            /
DATA  spmh_nam_cbmz(  4)     , spmh_map_cbmz(  4)  &
    / 'limonene        ', 4              /
DATA  mech_nam_cbmz(  4)     , mech_map_cbmz(  4)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz(  4) / 2.0            /
DATA  spmh_nam_cbmz(  5)     , spmh_map_cbmz(  5)  &
    / 'carene_3        ', 5              /
DATA  mech_nam_cbmz(  5)     , mech_map_cbmz(  5)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz(  5) / 2.0            /
DATA  spmh_nam_cbmz(  6)     , spmh_map_cbmz(  6)  &
    / 'ocimene_t_b     ', 6              /
DATA  mech_nam_cbmz(  6)     , mech_map_cbmz(  6)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz(  6) / 2.0            /
DATA  spmh_nam_cbmz(  7)     , spmh_map_cbmz(  7)  &
    / 'pinene_b        ', 7              /
DATA  mech_nam_cbmz(  7)     , mech_map_cbmz(  7)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz(  7) / 2.0            /
DATA  spmh_nam_cbmz(  8)     , spmh_map_cbmz(  8)  &
    / 'pinene_a        ', 8              /
DATA  mech_nam_cbmz(  8)     , mech_map_cbmz(  8)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz(  8) / 2.0            /
DATA  spmh_nam_cbmz(  9)     , spmh_map_cbmz(  9)  &
    / 'A_2met_styrene    ', 9              /
DATA  mech_nam_cbmz(  9)     , mech_map_cbmz(  9)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz(  9) / 2.0            /
DATA  spmh_nam_cbmz( 10)     , spmh_map_cbmz( 10)  &
    / 'cymene_p        ', 10             /
DATA  mech_nam_cbmz( 10)     , mech_map_cbmz( 10)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 10) / 2.0            /
DATA  spmh_nam_cbmz( 11)     , spmh_map_cbmz( 11)  &
    / 'cymene_o        ', 11             /
DATA  mech_nam_cbmz( 11)     , mech_map_cbmz( 11)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 11) / 2.0            /
DATA  spmh_nam_cbmz( 12)     , spmh_map_cbmz( 12)  &
    / 'phellandrene_a  ', 12             /
DATA  mech_nam_cbmz( 12)     , mech_map_cbmz( 12)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 12) / 2.0            /
DATA  spmh_nam_cbmz( 13)     , spmh_map_cbmz( 13)  &
    / 'thujene_a       ', 13             /
DATA  mech_nam_cbmz( 13)     , mech_map_cbmz( 13)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 13) / 2.0            /
DATA  spmh_nam_cbmz( 14)     , spmh_map_cbmz( 14)  &
    / 'terpinene_a     ', 14             /
DATA  mech_nam_cbmz( 14)     , mech_map_cbmz( 14)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 14) / 2.0            /
DATA  spmh_nam_cbmz( 15)     , spmh_map_cbmz( 15)  &
    / 'terpinene_g     ', 15             /
DATA  mech_nam_cbmz( 15)     , mech_map_cbmz( 15)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 15) / 2.0            /
DATA  spmh_nam_cbmz( 16)     , spmh_map_cbmz( 16)  &
    / 'terpinolene     ', 16             /
DATA  mech_nam_cbmz( 16)     , mech_map_cbmz( 16)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 16) / 2.0            /
DATA  spmh_nam_cbmz( 17)     , spmh_map_cbmz( 17)  &
    / 'phellandrene_b  ', 17             /
DATA  mech_nam_cbmz( 17)     , mech_map_cbmz( 17)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 17) / 2.0            /
DATA  spmh_nam_cbmz( 18)     , spmh_map_cbmz( 18)  &
    / 'camphene        ', 18             /
DATA  mech_nam_cbmz( 18)     , mech_map_cbmz( 18)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 18) / 2.0            /
DATA  spmh_nam_cbmz( 19)     , spmh_map_cbmz( 19)  &
    / 'bornene         ', 19             /
DATA  mech_nam_cbmz( 19)     , mech_map_cbmz( 19)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 19) / 2.0            /
DATA  spmh_nam_cbmz( 20)     , spmh_map_cbmz( 20)  &
    / 'fenchene_a      ', 20             /
DATA  mech_nam_cbmz( 20)     , mech_map_cbmz( 20)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 20) / 2.0            /
DATA  spmh_nam_cbmz( 21)     , spmh_map_cbmz( 21)  &
    / 'ocimene_al      ', 21             /
DATA  mech_nam_cbmz( 21)     , mech_map_cbmz( 21)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 21) / 2.0            /
DATA  spmh_nam_cbmz( 22)     , spmh_map_cbmz( 22)  &
    / 'ocimene_c_b     ', 22             /
DATA  mech_nam_cbmz( 22)     , mech_map_cbmz( 22)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 22) / 2.0            /
DATA  spmh_nam_cbmz( 23)     , spmh_map_cbmz( 23)  &
    / 'tricyclene      ', 23             /
DATA  mech_nam_cbmz( 23)     , mech_map_cbmz( 23)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 23) / 2.0            /
DATA  spmh_nam_cbmz( 24)     , spmh_map_cbmz( 24)  &
    / 'estragole       ', 24             /
DATA  mech_nam_cbmz( 24)     , mech_map_cbmz( 24)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 24) / 2.0            /
DATA  spmh_nam_cbmz( 25)     , spmh_map_cbmz( 25)  &
    / 'camphor         ', 25             /
DATA  mech_nam_cbmz( 25)     , mech_map_cbmz( 25)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 25) / 2.0            /
DATA  spmh_nam_cbmz( 26)     , spmh_map_cbmz( 26)  &
    / 'fenchone        ', 26             /
DATA  mech_nam_cbmz( 26)     , mech_map_cbmz( 26)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 26) / 2.0            /
DATA  spmh_nam_cbmz( 27)     , spmh_map_cbmz( 27)  &
    / 'piperitone      ', 27             /
DATA  mech_nam_cbmz( 27)     , mech_map_cbmz( 27)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 27) / 2.0            /
DATA  spmh_nam_cbmz( 28)     , spmh_map_cbmz( 28)  &
    / 'thujone_a       ', 28             /
DATA  mech_nam_cbmz( 28)     , mech_map_cbmz( 28)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 28) / 2.0            /
DATA  spmh_nam_cbmz( 29)     , spmh_map_cbmz( 29)  &
    / 'thujone_b       ', 29             /
DATA  mech_nam_cbmz( 29)     , mech_map_cbmz( 29)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 29) / 2.0            /
DATA  spmh_nam_cbmz( 30)     , spmh_map_cbmz( 30)  &
    / 'cineole_1_8     ', 30             /
DATA  mech_nam_cbmz( 30)     , mech_map_cbmz( 30)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 30) / 2.0            /
DATA  spmh_nam_cbmz( 31)     , spmh_map_cbmz( 31)  &
    / 'borneol         ', 31             /
DATA  mech_nam_cbmz( 31)     , mech_map_cbmz( 31)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 31) / 2.0            /
DATA  spmh_nam_cbmz( 32)     , spmh_map_cbmz( 32)  &
    / 'linalool        ', 32             /
DATA  mech_nam_cbmz( 32)     , mech_map_cbmz( 32)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 32) / 2.0            /
DATA  spmh_nam_cbmz( 33)     , spmh_map_cbmz( 33)  &
    / 'terpineol_4     ', 33             /
DATA  mech_nam_cbmz( 33)     , mech_map_cbmz( 33)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 33) / 2.0            /
DATA  spmh_nam_cbmz( 34)     , spmh_map_cbmz( 34)  &
    / 'terpineol_a     ', 34             /
DATA  mech_nam_cbmz( 34)     , mech_map_cbmz( 34)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 34) / 2.0            /
DATA  spmh_nam_cbmz( 35)     , spmh_map_cbmz( 35)  &
    / 'linalool_OXD_c  ', 35             /
DATA  mech_nam_cbmz( 35)     , mech_map_cbmz( 35)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 35) / 2.0            /
DATA  spmh_nam_cbmz( 36)     , spmh_map_cbmz( 36)  &
    / 'linalool_OXD_t  ', 36             /
DATA  mech_nam_cbmz( 36)     , mech_map_cbmz( 36)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 36) / 2.0            /
DATA  spmh_nam_cbmz( 37)     , spmh_map_cbmz( 37)  &
    / 'ionone_b        ', 37             /
DATA  mech_nam_cbmz( 37)     , mech_map_cbmz( 37)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 37) / 3.0            /
DATA  spmh_nam_cbmz( 38)     , spmh_map_cbmz( 38)  &
    / 'bornyl_ACT      ', 38             /
DATA  mech_nam_cbmz( 38)     , mech_map_cbmz( 38)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 38) / 2.0            /
DATA  spmh_nam_cbmz( 39)     , spmh_map_cbmz( 39)  &
    / 'farnescene_a    ', 39             /
DATA  mech_nam_cbmz( 39)     , mech_map_cbmz( 39)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 39) / 3.0            /
DATA  spmh_nam_cbmz( 40)     , spmh_map_cbmz( 40)  &
    / 'caryophyllene_b ', 40             /
DATA  mech_nam_cbmz( 40)     , mech_map_cbmz( 40)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 40) / 3.0            /
DATA  spmh_nam_cbmz( 41)     , spmh_map_cbmz( 41)  &
    / 'acoradiene      ', 41             /
DATA  mech_nam_cbmz( 41)     , mech_map_cbmz( 41)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 41) / 3.0            /
DATA  spmh_nam_cbmz( 42)     , spmh_map_cbmz( 42)  &
    / 'aromadendrene   ', 42             /
DATA  mech_nam_cbmz( 42)     , mech_map_cbmz( 42)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 42) / 3.0            /
DATA  spmh_nam_cbmz( 43)     , spmh_map_cbmz( 43)  &
    / 'bergamotene_a   ', 43             /
DATA  mech_nam_cbmz( 43)     , mech_map_cbmz( 43)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 43) / 3.0            /
DATA  spmh_nam_cbmz( 44)     , spmh_map_cbmz( 44)  &
    / 'bergamotene_b   ', 44             /
DATA  mech_nam_cbmz( 44)     , mech_map_cbmz( 44)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 44) / 3.0            /
DATA  spmh_nam_cbmz( 45)     , spmh_map_cbmz( 45)  &
    / 'bisabolene_a    ', 45             /
DATA  mech_nam_cbmz( 45)     , mech_map_cbmz( 45)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 45) / 3.0            /
DATA  spmh_nam_cbmz( 46)     , spmh_map_cbmz( 46)  &
    / 'bisabolene_b    ', 46             /
DATA  mech_nam_cbmz( 46)     , mech_map_cbmz( 46)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 46) / 3.0            /
DATA  spmh_nam_cbmz( 47)     , spmh_map_cbmz( 47)  &
    / 'bourbonene_b    ', 47             /
DATA  mech_nam_cbmz( 47)     , mech_map_cbmz( 47)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 47) / 3.0            /
DATA  spmh_nam_cbmz( 48)     , spmh_map_cbmz( 48)  &
    / 'cadinene_d      ', 48             /
DATA  mech_nam_cbmz( 48)     , mech_map_cbmz( 48)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 48) / 3.0            /
DATA  spmh_nam_cbmz( 49)     , spmh_map_cbmz( 49)  &
    / 'cadinene_g      ', 49             /
DATA  mech_nam_cbmz( 49)     , mech_map_cbmz( 49)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 49) / 3.0            /
DATA  spmh_nam_cbmz( 50)     , spmh_map_cbmz( 50)  &
    / 'cedrene_a       ', 50             /
DATA  mech_nam_cbmz( 50)     , mech_map_cbmz( 50)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 50) / 3.0            /
DATA  spmh_nam_cbmz( 51)     , spmh_map_cbmz( 51)  &
    / 'copaene_a       ', 51             /
DATA  mech_nam_cbmz( 51)     , mech_map_cbmz( 51)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 51) / 3.0            /
DATA  spmh_nam_cbmz( 52)     , spmh_map_cbmz( 52)  &
    / 'cubebene_a      ', 52             /
DATA  mech_nam_cbmz( 52)     , mech_map_cbmz( 52)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 52) / 3.0            /
DATA  spmh_nam_cbmz( 53)     , spmh_map_cbmz( 53)  &
    / 'cubebene_b      ', 53             /
DATA  mech_nam_cbmz( 53)     , mech_map_cbmz( 53)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 53) / 3.0            /
DATA  spmh_nam_cbmz( 54)     , spmh_map_cbmz( 54)  &
    / 'elemene_b       ', 54             /
DATA  mech_nam_cbmz( 54)     , mech_map_cbmz( 54)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 54) / 3.0            /
DATA  spmh_nam_cbmz( 55)     , spmh_map_cbmz( 55)  &
    / 'farnescene_b    ', 55             /
DATA  mech_nam_cbmz( 55)     , mech_map_cbmz( 55)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 55) / 3.0            /
DATA  spmh_nam_cbmz( 56)     , spmh_map_cbmz( 56)  &
    / 'germacrene_B    ', 56             /
DATA  mech_nam_cbmz( 56)     , mech_map_cbmz( 56)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 56) / 3.0            /
DATA  spmh_nam_cbmz( 57)     , spmh_map_cbmz( 57)  &
    / 'germacrene_D    ', 57             /
DATA  mech_nam_cbmz( 57)     , mech_map_cbmz( 57)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 57) / 3.0            /
DATA  spmh_nam_cbmz( 58)     , spmh_map_cbmz( 58)  &
    / 'gurjunene_b     ', 58             /
DATA  mech_nam_cbmz( 58)     , mech_map_cbmz( 58)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 58) / 3.0            /
DATA  spmh_nam_cbmz( 59)     , spmh_map_cbmz( 59)  &
    / 'humulene_a      ', 59             /
DATA  mech_nam_cbmz( 59)     , mech_map_cbmz( 59)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 59) / 3.0            /
DATA  spmh_nam_cbmz( 60)     , spmh_map_cbmz( 60)  &
    / 'humulene_g      ', 60             /
DATA  mech_nam_cbmz( 60)     , mech_map_cbmz( 60)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 60) / 3.0            /
DATA  spmh_nam_cbmz( 61)     , spmh_map_cbmz( 61)  &
    / 'isolongifolene  ', 61             /
DATA  mech_nam_cbmz( 61)     , mech_map_cbmz( 61)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 61) / 3.0            /
DATA  spmh_nam_cbmz( 62)     , spmh_map_cbmz( 62)  &
    / 'longifolene     ', 62             /
DATA  mech_nam_cbmz( 62)     , mech_map_cbmz( 62)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 62) / 3.0            /
DATA  spmh_nam_cbmz( 63)     , spmh_map_cbmz( 63)  &
    / 'longipinene     ', 63             /
DATA  mech_nam_cbmz( 63)     , mech_map_cbmz( 63)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 63) / 3.0            /
DATA  spmh_nam_cbmz( 64)     , spmh_map_cbmz( 64)  &
    / 'muurolene_a     ', 64             /
DATA  mech_nam_cbmz( 64)     , mech_map_cbmz( 64)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 64) / 3.0            /
DATA  spmh_nam_cbmz( 65)     , spmh_map_cbmz( 65)  &
    / 'muurolene_g     ', 65             /
DATA  mech_nam_cbmz( 65)     , mech_map_cbmz( 65)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 65) / 3.0            /
DATA  spmh_nam_cbmz( 66)     , spmh_map_cbmz( 66)  &
    / 'selinene_b      ', 66             /
DATA  mech_nam_cbmz( 66)     , mech_map_cbmz( 66)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 66) / 3.0            /
DATA  spmh_nam_cbmz( 67)     , spmh_map_cbmz( 67)  &
    / 'selinene_d      ', 67             /
DATA  mech_nam_cbmz( 67)     , mech_map_cbmz( 67)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 67) / 3.0            /
DATA  spmh_nam_cbmz( 68)     , spmh_map_cbmz( 68)  &
    / 'nerolidol_c     ', 68             /
DATA  mech_nam_cbmz( 68)     , mech_map_cbmz( 68)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 68) / 3.0            /
DATA  spmh_nam_cbmz( 69)     , spmh_map_cbmz( 69)  &
    / 'nerolidol_t     ', 69             /
DATA  mech_nam_cbmz( 69)     , mech_map_cbmz( 69)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 69) / 3.0            /
DATA  spmh_nam_cbmz( 70)     , spmh_map_cbmz( 70)  &
    / 'cedrol          ', 70             /
DATA  mech_nam_cbmz( 70)     , mech_map_cbmz( 70)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 70) / 3.0            /
DATA  spmh_nam_cbmz( 71)     , spmh_map_cbmz( 71)  &
    / 'MBO_2m3e2ol     ', 71             /
DATA  mech_nam_cbmz( 71)     , mech_map_cbmz( 71)  &
    / 'OLET            ', 15             /
DATA  conv_fac_cbmz( 71) / 1.0            /
DATA  spmh_nam_cbmz( 72)     , spmh_map_cbmz( 72)  &
    / 'MBO_2m3e2ol     ', 71             /
DATA  mech_nam_cbmz( 72)     , mech_map_cbmz( 72)  &
    / 'PAR             ', 13             /
DATA  conv_fac_cbmz( 72) / 3.0            /
DATA  spmh_nam_cbmz( 73)     , spmh_map_cbmz( 73)  &
    / 'methanol        ', 72             /
DATA  mech_nam_cbmz( 73)     , mech_map_cbmz( 73)  &
    / 'CH3OH           ', 5              /
DATA  conv_fac_cbmz( 73) / 1.0            /
DATA  spmh_nam_cbmz( 74)     , spmh_map_cbmz( 74)  &
    / 'acetone         ', 73             /
DATA  mech_nam_cbmz( 74)     , mech_map_cbmz( 74)  &
    / 'AONE            ', 18             /
DATA  conv_fac_cbmz( 74) / 1.0            /
DATA  spmh_nam_cbmz( 75)     , spmh_map_cbmz( 75)  &
    / 'methane         ', 74             /
DATA  mech_nam_cbmz( 75)     , mech_map_cbmz( 75)  &
    / 'CH4             ', 11             /
DATA  conv_fac_cbmz( 75) / 1.0            /
DATA  spmh_nam_cbmz( 76)     , spmh_map_cbmz( 76)  &
    / 'ammonia         ', 75             /
DATA  mech_nam_cbmz( 76)     , mech_map_cbmz( 76)  &
    / 'NH3             ', 22             /
DATA  conv_fac_cbmz( 76) / 1.0            /
DATA  spmh_nam_cbmz( 77)     , spmh_map_cbmz( 77)  &
    / 'nitrous_OXD     ', 76             /
DATA  mech_nam_cbmz( 77)     , mech_map_cbmz( 77)  &
    / 'NR              ', 23             /
DATA  conv_fac_cbmz( 77) / 1.0            /
DATA  spmh_nam_cbmz( 78)     , spmh_map_cbmz( 78)  &
    / 'nitric_OXD      ', 77             /
DATA  mech_nam_cbmz( 78)     , mech_map_cbmz( 78)  &
    / 'NO              ', 2              /
DATA  conv_fac_cbmz( 78) / 1.0            /
DATA  spmh_nam_cbmz( 79)     , spmh_map_cbmz( 79)  &
    / 'acetaldehyde    ', 78             /
DATA  mech_nam_cbmz( 79)     , mech_map_cbmz( 79)  &
    / 'ALD2            ', 7              /
DATA  conv_fac_cbmz( 79) / 1.0            /
DATA  spmh_nam_cbmz( 80)     , spmh_map_cbmz( 80)  &
    / 'ethanol         ', 79             /
DATA  mech_nam_cbmz( 80)     , mech_map_cbmz( 80)  &
    / 'ANOL            ', 6              /
DATA  conv_fac_cbmz( 80) / 1.0            /
DATA  spmh_nam_cbmz( 81)     , spmh_map_cbmz( 81)  &
    / 'formic_acid     ', 80             /
DATA  mech_nam_cbmz( 81)     , mech_map_cbmz( 81)  &
    / 'HCOOH           ', 9              /
DATA  conv_fac_cbmz( 81) / 1.0            /
DATA  spmh_nam_cbmz( 82)     , spmh_map_cbmz( 82)  &
    / 'formaldehyde    ', 81             /
DATA  mech_nam_cbmz( 82)     , mech_map_cbmz( 82)  &
    / 'HCHO            ', 8              /
DATA  conv_fac_cbmz( 82) / 1.0            /
DATA  spmh_nam_cbmz( 83)     , spmh_map_cbmz( 83)  &
    / 'acetic_acid     ', 82             /
DATA  mech_nam_cbmz( 83)     , mech_map_cbmz( 83)  &
    / 'RCOOH           ', 10             /
DATA  conv_fac_cbmz( 83) / 1.0            /
DATA  spmh_nam_cbmz( 84)     , spmh_map_cbmz( 84)  &
    / 'MBO_3m2e1ol     ', 83             /
DATA  mech_nam_cbmz( 84)     , mech_map_cbmz( 84)  &
    / 'ALD2            ', 7              /
DATA  conv_fac_cbmz( 84) / 1.0            /
DATA  spmh_nam_cbmz( 85)     , spmh_map_cbmz( 85)  &
    / 'MBO_3m2e1ol     ', 83             /
DATA  mech_nam_cbmz( 85)     , mech_map_cbmz( 85)  &
    / 'PAR             ', 13             /
DATA  conv_fac_cbmz( 85) / 3.0            /
DATA  spmh_nam_cbmz( 86)     , spmh_map_cbmz( 86)  &
    / 'MBO_3m3e1ol     ', 84             /
DATA  mech_nam_cbmz( 86)     , mech_map_cbmz( 86)  &
    / 'HCHO            ', 8              /
DATA  conv_fac_cbmz( 86) / 1.0            /
DATA  spmh_nam_cbmz( 87)     , spmh_map_cbmz( 87)  &
    / 'MBO_3m3e1ol     ', 84             /
DATA  mech_nam_cbmz( 87)     , mech_map_cbmz( 87)  &
    / 'PAR             ', 13             /
DATA  conv_fac_cbmz( 87) / 4.0            /
DATA  spmh_nam_cbmz( 88)     , spmh_map_cbmz( 88)  &
    / 'benzaldehyde    ', 85             /
DATA  mech_nam_cbmz( 88)     , mech_map_cbmz( 88)  &
    / 'TOL             ', 19             /
DATA  conv_fac_cbmz( 88) / 1.0            /
DATA  spmh_nam_cbmz( 89)     , spmh_map_cbmz( 89)  &
    / 'butanone_2      ', 86             /
DATA  mech_nam_cbmz( 89)     , mech_map_cbmz( 89)  &
    / 'AONE            ', 18             /
DATA  conv_fac_cbmz( 89) / 1.0            /
DATA  spmh_nam_cbmz( 90)     , spmh_map_cbmz( 90)  &
    / 'butanone_2      ', 86             /
DATA  mech_nam_cbmz( 90)     , mech_map_cbmz( 90)  &
    / 'PAR             ', 13             /
DATA  conv_fac_cbmz( 90) / 1.0            /
DATA  spmh_nam_cbmz( 91)     , spmh_map_cbmz( 91)  &
    / 'decanal         ', 87             /
DATA  mech_nam_cbmz( 91)     , mech_map_cbmz( 91)  &
    / 'ALD2            ', 7              /
DATA  conv_fac_cbmz( 91) / 1.0            /
DATA  spmh_nam_cbmz( 92)     , spmh_map_cbmz( 92)  &
    / 'decanal         ', 87             /
DATA  mech_nam_cbmz( 92)     , mech_map_cbmz( 92)  &
    / 'PAR             ', 13             /
DATA  conv_fac_cbmz( 92) / 8.0            /
DATA  spmh_nam_cbmz( 93)     , spmh_map_cbmz( 93)  &
    / 'dodecene_1      ', 88             /
DATA  mech_nam_cbmz( 93)     , mech_map_cbmz( 93)  &
    / 'OLET            ', 15             /
DATA  conv_fac_cbmz( 93) / 1.0            /
DATA  spmh_nam_cbmz( 94)     , spmh_map_cbmz( 94)  &
    / 'dodecene_1      ', 88             /
DATA  mech_nam_cbmz( 94)     , mech_map_cbmz( 94)  &
    / 'PAR             ', 13             /
DATA  conv_fac_cbmz( 94) / 10.0           /
DATA  spmh_nam_cbmz( 95)     , spmh_map_cbmz( 95)  &
    / 'geranyl_acetone ', 89             /
DATA  mech_nam_cbmz( 95)     , mech_map_cbmz( 95)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz( 95) / 3.0            /
DATA  spmh_nam_cbmz( 96)     , spmh_map_cbmz( 96)  &
    / 'heptanal        ', 90             /
DATA  mech_nam_cbmz( 96)     , mech_map_cbmz( 96)  &
    / 'ALD2            ', 7              /
DATA  conv_fac_cbmz( 96) / 1.0            /
DATA  spmh_nam_cbmz( 97)     , spmh_map_cbmz( 97)  &
    / 'heptanal        ', 90             /
DATA  mech_nam_cbmz( 97)     , mech_map_cbmz( 97)  &
    / 'PAR             ', 13             /
DATA  conv_fac_cbmz( 97) / 5.0            /
DATA  spmh_nam_cbmz( 98)     , spmh_map_cbmz( 98)  &
    / 'heptane         ', 91             /
DATA  mech_nam_cbmz( 98)     , mech_map_cbmz( 98)  &
    / 'PAR             ', 13             /
DATA  conv_fac_cbmz( 98) / 7.0            /
DATA  spmh_nam_cbmz( 99)     , spmh_map_cbmz( 99)  &
    / 'hexane          ', 92             /
DATA  mech_nam_cbmz( 99)     , mech_map_cbmz( 99)  &
    / 'PAR             ', 13             /
DATA  conv_fac_cbmz( 99) / 6.0            /
DATA  spmh_nam_cbmz(100)     , spmh_map_cbmz(100)  &
    / 'met_benzoate    ', 93             /
DATA  mech_nam_cbmz(100)     , mech_map_cbmz(100)  &
    / 'TOL             ', 19             /
DATA  conv_fac_cbmz(100) / 1.0            /
DATA  spmh_nam_cbmz(101)     , spmh_map_cbmz(101)  &
    / 'met_heptenone   ', 94             /
DATA  mech_nam_cbmz(101)     , mech_map_cbmz(101)  &
    / 'AONE            ', 18             /
DATA  conv_fac_cbmz(101) / 1.0            /
DATA  spmh_nam_cbmz(102)     , spmh_map_cbmz(102)  &
    / 'met_heptenone   ', 94             /
DATA  mech_nam_cbmz(102)     , mech_map_cbmz(102)  &
    / 'PAR             ', 13             /
DATA  conv_fac_cbmz(102) / 3.0            /
DATA  spmh_nam_cbmz(103)     , spmh_map_cbmz(103)  &
    / 'met_heptenone   ', 94             /
DATA  mech_nam_cbmz(103)     , mech_map_cbmz(103)  &
    / 'OLET            ', 15             /
DATA  conv_fac_cbmz(103) / 1.0            /
DATA  spmh_nam_cbmz(104)     , spmh_map_cbmz(104)  &
    / 'neryl_acetone   ', 95             /
DATA  mech_nam_cbmz(104)     , mech_map_cbmz(104)  &
    / 'AONE            ', 18             /
DATA  conv_fac_cbmz(104) / 1.0            /
DATA  spmh_nam_cbmz(105)     , spmh_map_cbmz(105)  &
    / 'neryl_acetone   ', 95             /
DATA  mech_nam_cbmz(105)     , mech_map_cbmz(105)  &
    / 'PAR             ', 13             /
DATA  conv_fac_cbmz(105) / 8.0            /
DATA  spmh_nam_cbmz(106)     , spmh_map_cbmz(106)  &
    / 'neryl_acetone   ', 95             /
DATA  mech_nam_cbmz(106)     , mech_map_cbmz(106)  &
    / 'OLEI            ', 16             /
DATA  conv_fac_cbmz(106) / 2.0            /
DATA  spmh_nam_cbmz(107)     , spmh_map_cbmz(107)  &
    / 'nonanal         ', 96             /
DATA  mech_nam_cbmz(107)     , mech_map_cbmz(107)  &
    / 'ALD2            ', 7              /
DATA  conv_fac_cbmz(107) / 1.0            /
DATA  spmh_nam_cbmz(108)     , spmh_map_cbmz(108)  &
    / 'nonanal         ', 96             /
DATA  mech_nam_cbmz(108)     , mech_map_cbmz(108)  &
    / 'PAR             ', 13             /
DATA  conv_fac_cbmz(108) / 7.0            /
DATA  spmh_nam_cbmz(109)     , spmh_map_cbmz(109)  &
    / 'nonenal         ', 97             /
DATA  mech_nam_cbmz(109)     , mech_map_cbmz(109)  &
    / 'ALD2            ', 7              /
DATA  conv_fac_cbmz(109) / 1.0            /
DATA  spmh_nam_cbmz(110)     , spmh_map_cbmz(110)  &
    / 'nonenal         ', 97             /
DATA  mech_nam_cbmz(110)     , mech_map_cbmz(110)  &
    / 'PAR             ', 13             /
DATA  conv_fac_cbmz(110) / 6.0            /
DATA  spmh_nam_cbmz(111)     , spmh_map_cbmz(111)  &
    / 'nonenal         ', 97             /
DATA  mech_nam_cbmz(111)     , mech_map_cbmz(111)  &
    / 'OLEI            ', 16             /
DATA  conv_fac_cbmz(111) / 1.0            /
DATA  spmh_nam_cbmz(112)     , spmh_map_cbmz(112)  &
    / 'octanal         ', 98             /
DATA  mech_nam_cbmz(112)     , mech_map_cbmz(112)  &
    / 'ALD2            ', 7              /
DATA  conv_fac_cbmz(112) / 1.0            /
DATA  spmh_nam_cbmz(113)     , spmh_map_cbmz(113)  &
    / 'octanal         ', 98             /
DATA  mech_nam_cbmz(113)     , mech_map_cbmz(113)  &
    / 'PAR             ', 13             /
DATA  conv_fac_cbmz(113) / 6.0            /
DATA  spmh_nam_cbmz(114)     , spmh_map_cbmz(114)  &
    / 'octanol         ', 99             /
DATA  mech_nam_cbmz(114)     , mech_map_cbmz(114)  &
    / 'PAR             ', 13             /
DATA  conv_fac_cbmz(114) / 8.0            /
DATA  spmh_nam_cbmz(115)     , spmh_map_cbmz(115)  &
    / 'octenol_1e3ol   ', 100            /
DATA  mech_nam_cbmz(115)     , mech_map_cbmz(115)  &
    / 'PAR             ', 13             /
DATA  conv_fac_cbmz(115) / 6.0            /
DATA  spmh_nam_cbmz(116)     , spmh_map_cbmz(116)  &
    / 'octenol_1e3ol   ', 100            /
DATA  mech_nam_cbmz(116)     , mech_map_cbmz(116)  &
    / 'OLET            ', 15             /
DATA  conv_fac_cbmz(116) / 1.0            /
DATA  spmh_nam_cbmz(117)     , spmh_map_cbmz(117)  &
    / 'oxopentanal     ', 101            /
DATA  mech_nam_cbmz(117)     , mech_map_cbmz(117)  &
    / 'PAR             ', 13             /
DATA  conv_fac_cbmz(117) / 3.0            /
DATA  spmh_nam_cbmz(118)     , spmh_map_cbmz(118)  &
    / 'oxopentanal     ', 101            /
DATA  mech_nam_cbmz(118)     , mech_map_cbmz(118)  &
    / 'ALD2            ', 7              /
DATA  conv_fac_cbmz(118) / 1.0            /
DATA  spmh_nam_cbmz(119)     , spmh_map_cbmz(119)  &
    / 'pentane         ', 102            /
DATA  mech_nam_cbmz(119)     , mech_map_cbmz(119)  &
    / 'PAR             ', 13             /
DATA  conv_fac_cbmz(119) / 5.0            /
DATA  spmh_nam_cbmz(120)     , spmh_map_cbmz(120)  &
    / 'phenyl_CCO      ', 103            /
DATA  mech_nam_cbmz(120)     , mech_map_cbmz(120)  &
    / 'ALD2            ', 7              /
DATA  conv_fac_cbmz(120) / 1.0            /
DATA  spmh_nam_cbmz(121)     , spmh_map_cbmz(121)  &
    / 'phenyl_CCO      ', 103            /
DATA  mech_nam_cbmz(121)     , mech_map_cbmz(121)  &
    / 'TOL             ', 19             /
DATA  conv_fac_cbmz(121) / 1.0            /
DATA  spmh_nam_cbmz(122)     , spmh_map_cbmz(122)  &
    / 'pyruvic_acid    ', 104            /
DATA  mech_nam_cbmz(122)     , mech_map_cbmz(122)  &
    / 'HCOOH           ', 9              /
DATA  conv_fac_cbmz(122) / 1.0            /
DATA  spmh_nam_cbmz(123)     , spmh_map_cbmz(123)  &
    / 'pyruvic_acid    ', 104            /
DATA  mech_nam_cbmz(123)     , mech_map_cbmz(123)  &
    / 'AONE            ', 18             /
DATA  conv_fac_cbmz(123) / 1.0            /
DATA  spmh_nam_cbmz(124)     , spmh_map_cbmz(124)  &
    / 'terpinyl_ACT_a  ', 105            /
DATA  mech_nam_cbmz(124)     , mech_map_cbmz(124)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz(124) / 2.0            /
DATA  spmh_nam_cbmz(125)     , spmh_map_cbmz(125)  &
    / 'tetradecene_1   ', 106            /
DATA  mech_nam_cbmz(125)     , mech_map_cbmz(125)  &
    / 'PAR             ', 13             /
DATA  conv_fac_cbmz(125) / 12.0           /
DATA  spmh_nam_cbmz(126)     , spmh_map_cbmz(126)  &
    / 'tetradecene_1   ', 106            /
DATA  mech_nam_cbmz(126)     , mech_map_cbmz(126)  &
    / 'OLET            ', 15             /
DATA  conv_fac_cbmz(126) / 1.0            /
DATA  spmh_nam_cbmz(127)     , spmh_map_cbmz(127)  &
    / 'toluene         ', 107            /
DATA  mech_nam_cbmz(127)     , mech_map_cbmz(127)  &
    / 'TOL             ', 19             /
DATA  conv_fac_cbmz(127) / 1.0            /
DATA  spmh_nam_cbmz(128)     , spmh_map_cbmz(128)  &
    / 'carbon_monoxide ', 108            /
DATA  mech_nam_cbmz(128)     , mech_map_cbmz(128)  &
    / 'CO              ', 4              /
DATA  conv_fac_cbmz(128) / 1.0            /
DATA  spmh_nam_cbmz(129)     , spmh_map_cbmz(129)  &
    / 'butene          ', 109            /
DATA  mech_nam_cbmz(129)     , mech_map_cbmz(129)  &
    / 'OLET            ', 15             /
DATA  conv_fac_cbmz(129) / 1.0            /
DATA  spmh_nam_cbmz(130)     , spmh_map_cbmz(130)  &
    / 'butene          ', 109            /
DATA  mech_nam_cbmz(130)     , mech_map_cbmz(130)  &
    / 'PAR             ', 13             /
DATA  conv_fac_cbmz(130) / 2.0            /
DATA  spmh_nam_cbmz(131)     , spmh_map_cbmz(131)  &
    / 'ethane          ', 110            /
DATA  mech_nam_cbmz(131)     , mech_map_cbmz(131)  &
    / 'C2H6            ', 12             /
DATA  conv_fac_cbmz(131) / 1.0            /
DATA  spmh_nam_cbmz(132)     , spmh_map_cbmz(132)  &
    / 'ethene          ', 111            /
DATA  mech_nam_cbmz(132)     , mech_map_cbmz(132)  &
    / 'ETH             ', 14             /
DATA  conv_fac_cbmz(132) / 1.0            /
DATA  spmh_nam_cbmz(133)     , spmh_map_cbmz(133)  &
    / 'hydrogen_cyanide', 112            /
DATA  mech_nam_cbmz(133)     , mech_map_cbmz(133)  &
    / 'NR              ', 23             /
DATA  conv_fac_cbmz(133) / 1.0            /
DATA  spmh_nam_cbmz(134)     , spmh_map_cbmz(134)  &
    / 'propane         ', 113            /
DATA  mech_nam_cbmz(134)     , mech_map_cbmz(134)  &
    / 'PAR             ', 13             /
DATA  conv_fac_cbmz(134) / 3.0            /
DATA  spmh_nam_cbmz(135)     , spmh_map_cbmz(135)  &
    / 'propene         ', 114            /
DATA  mech_nam_cbmz(135)     , mech_map_cbmz(135)  &
    / 'OLET            ', 15             /
DATA  conv_fac_cbmz(135) / 1.0            /
DATA  spmh_nam_cbmz(136)     , spmh_map_cbmz(136)  &
    / 'propene         ', 114            /
DATA  mech_nam_cbmz(136)     , mech_map_cbmz(136)  &
    / 'PAR             ', 13             /
DATA  conv_fac_cbmz(136) / 1.0            /
DATA  spmh_nam_cbmz(137)     , spmh_map_cbmz(137)  &
    / 'carbon_2s       ', 115            /
DATA  mech_nam_cbmz(137)     , mech_map_cbmz(137)  &
    / 'NR              ', 23             /
DATA  conv_fac_cbmz(137) / 1.0            /
DATA  spmh_nam_cbmz(138)     , spmh_map_cbmz(138)  &
    / 'carbonyl_s      ', 116            /
DATA  mech_nam_cbmz(138)     , mech_map_cbmz(138)  &
    / 'NR              ', 23             /
DATA  conv_fac_cbmz(138) / 1.0            /
DATA  spmh_nam_cbmz(139)     , spmh_map_cbmz(139)  &
    / 'diallyl_2s      ', 117            /
DATA  mech_nam_cbmz(139)     , mech_map_cbmz(139)  &
    / 'DMS             ', 21             /
DATA  conv_fac_cbmz(139) / 1.0            /
DATA  spmh_nam_cbmz(140)     , spmh_map_cbmz(140)  &
    / 'diallyl_2s      ', 117            /
DATA  mech_nam_cbmz(140)     , mech_map_cbmz(140)  &
    / 'PAR             ', 13             /
DATA  conv_fac_cbmz(140) / 2.0            /
DATA  spmh_nam_cbmz(141)     , spmh_map_cbmz(141)  &
    / 'diallyl_2s      ', 117            /
DATA  mech_nam_cbmz(141)     , mech_map_cbmz(141)  &
    / 'OLET            ', 15             /
DATA  conv_fac_cbmz(141) / 2.0            /
DATA  spmh_nam_cbmz(142)     , spmh_map_cbmz(142)  &
    / 'A_2met_2s         ', 118            /
DATA  mech_nam_cbmz(142)     , mech_map_cbmz(142)  &
    / 'DMS             ', 21             /
DATA  conv_fac_cbmz(142) / 1.0            /
DATA  spmh_nam_cbmz(143)     , spmh_map_cbmz(143)  &
    / 'A_2met_s          ', 119            /
DATA  mech_nam_cbmz(143)     , mech_map_cbmz(143)  &
    / 'DMS             ', 21             /
DATA  conv_fac_cbmz(143) / 1.0            /
DATA  spmh_nam_cbmz(144)     , spmh_map_cbmz(144)  &
    / 'met_chloride    ', 120            /
DATA  mech_nam_cbmz(144)     , mech_map_cbmz(144)  &
    / 'NR              ', 23             /
DATA  conv_fac_cbmz(144) / 1.0            /
DATA  spmh_nam_cbmz(145)     , spmh_map_cbmz(145)  &
    / 'met_bromide     ', 121            /
DATA  mech_nam_cbmz(145)     , mech_map_cbmz(145)  &
    / 'NR              ', 23             /
DATA  conv_fac_cbmz(145) / 1.0            /
DATA  spmh_nam_cbmz(146)     , spmh_map_cbmz(146)  &
    / 'met_iodide      ', 122            /
DATA  mech_nam_cbmz(146)     , mech_map_cbmz(146)  &
    / 'NR              ', 23             /
DATA  conv_fac_cbmz(146) / 1.0            /
DATA  spmh_nam_cbmz(147)     , spmh_map_cbmz(147)  &
    / 'hydrogen_s      ', 123            /
DATA  mech_nam_cbmz(147)     , mech_map_cbmz(147)  &
    / 'NR              ', 23             /
DATA  conv_fac_cbmz(147) / 1.0            /
DATA  spmh_nam_cbmz(148)     , spmh_map_cbmz(148)  &
    / 'met_mercaptan   ', 124            /
DATA  mech_nam_cbmz(148)     , mech_map_cbmz(148)  &
    / 'PAR             ', 13             /
DATA  conv_fac_cbmz(148) / 1.0            /
DATA  spmh_nam_cbmz(149)     , spmh_map_cbmz(149)  &
    / 'met_propenyl_2s ', 125            /
DATA  mech_nam_cbmz(149)     , mech_map_cbmz(149)  &
    / 'DMS             ', 21             /
DATA  conv_fac_cbmz(149) / 1.0            /
DATA  spmh_nam_cbmz(150)     , spmh_map_cbmz(150)  &
    / 'met_propenyl_2s ', 125            /
DATA  mech_nam_cbmz(150)     , mech_map_cbmz(150)  &
    / 'OLEI            ', 16             /
DATA  conv_fac_cbmz(150) / 1.0            /
DATA  spmh_nam_cbmz(151)     , spmh_map_cbmz(151)  &
    / 'PPPP_2s         ', 126            /
DATA  mech_nam_cbmz(151)     , mech_map_cbmz(151)  &
    / 'DMS             ', 21             /
DATA  conv_fac_cbmz(151) / 1.0            /
DATA  spmh_nam_cbmz(152)     , spmh_map_cbmz(152)  &
    / 'PPPP_2s         ', 126            /
DATA  mech_nam_cbmz(152)     , mech_map_cbmz(152)  &
    / 'PAR             ', 13             /
DATA  conv_fac_cbmz(152) / 2.0            /
DATA  spmh_nam_cbmz(153)     , spmh_map_cbmz(153)  &
    / 'PPPP_2s         ', 126            /
DATA  mech_nam_cbmz(153)     , mech_map_cbmz(153)  &
    / 'OLEI            ', 16             /
DATA  conv_fac_cbmz(153) / 1.0            /
DATA  spmh_nam_cbmz(154)     , spmh_map_cbmz(154)  &
    / 'A_2met_nonatriene ', 127            /
DATA  mech_nam_cbmz(154)     , mech_map_cbmz(154)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz(154) / 2.0            /
DATA  spmh_nam_cbmz(155)     , spmh_map_cbmz(155)  &
    / 'met_salicylate  ', 128            /
DATA  mech_nam_cbmz(155)     , mech_map_cbmz(155)  &
    / 'TOL             ', 19             /
DATA  conv_fac_cbmz(155) / 1.0            /
DATA  spmh_nam_cbmz(156)     , spmh_map_cbmz(156)  &
    / 'indole          ', 129            /
DATA  mech_nam_cbmz(156)     , mech_map_cbmz(156)  &
    / 'TOL             ', 19             /
DATA  conv_fac_cbmz(156) / 1.0            /
DATA  spmh_nam_cbmz(157)     , spmh_map_cbmz(157)  &
    / 'jasmone         ', 130            /
DATA  mech_nam_cbmz(157)     , mech_map_cbmz(157)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz(157) / 2.0            /
DATA  spmh_nam_cbmz(158)     , spmh_map_cbmz(158)  &
    / 'met_jasmonate   ', 131            /
DATA  mech_nam_cbmz(158)     , mech_map_cbmz(158)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz(158) / 3.0            /
DATA  spmh_nam_cbmz(159)     , spmh_map_cbmz(159)  &
    / 'A_3met_3DCTT      ', 132            /
DATA  mech_nam_cbmz(159)     , mech_map_cbmz(159)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz(159) / 3.0            /
DATA  spmh_nam_cbmz(160)     , spmh_map_cbmz(160)  &
    / 'hexanal         ', 133            /
DATA  mech_nam_cbmz(160)     , mech_map_cbmz(160)  &
    / 'ALD2            ', 7              /
DATA  conv_fac_cbmz(160) / 1.0            /
DATA  spmh_nam_cbmz(161)     , spmh_map_cbmz(161)  &
    / 'hexanal         ', 133            /
DATA  mech_nam_cbmz(161)     , mech_map_cbmz(161)  &
    / 'PAR             ', 13             /
DATA  conv_fac_cbmz(161) / 4.0            /
DATA  spmh_nam_cbmz(162)     , spmh_map_cbmz(162)  &
    / 'hexanol_1       ', 134            /
DATA  mech_nam_cbmz(162)     , mech_map_cbmz(162)  &
    / 'PAR             ', 13             /
DATA  conv_fac_cbmz(162) / 6.0            /
DATA  spmh_nam_cbmz(163)     , spmh_map_cbmz(163)  &
    / 'hexenal_c3      ', 135            /
DATA  mech_nam_cbmz(163)     , mech_map_cbmz(163)  &
    / 'ALD2            ', 7              /
DATA  conv_fac_cbmz(163) / 1.0            /
DATA  spmh_nam_cbmz(164)     , spmh_map_cbmz(164)  &
    / 'hexenal_c3      ', 135            /
DATA  mech_nam_cbmz(164)     , mech_map_cbmz(164)  &
    / 'PAR             ', 13             /
DATA  conv_fac_cbmz(164) / 3.0            /
DATA  spmh_nam_cbmz(165)     , spmh_map_cbmz(165)  &
    / 'hexenal_c3      ', 135            /
DATA  mech_nam_cbmz(165)     , mech_map_cbmz(165)  &
    / 'OLEI            ', 16             /
DATA  conv_fac_cbmz(165) / 1.0            /
DATA  spmh_nam_cbmz(166)     , spmh_map_cbmz(166)  &
    / 'hexenal_t2      ', 136            /
DATA  mech_nam_cbmz(166)     , mech_map_cbmz(166)  &
    / 'ALD2            ', 7              /
DATA  conv_fac_cbmz(166) / 1.0            /
DATA  spmh_nam_cbmz(167)     , spmh_map_cbmz(167)  &
    / 'hexenal_t2      ', 136            /
DATA  mech_nam_cbmz(167)     , mech_map_cbmz(167)  &
    / 'PAR             ', 13             /
DATA  conv_fac_cbmz(167) / 6.0            /
DATA  spmh_nam_cbmz(168)     , spmh_map_cbmz(168)  &
    / 'hexenal_t2      ', 136            /
DATA  mech_nam_cbmz(168)     , mech_map_cbmz(168)  &
    / 'OLEI            ', 16             /
DATA  conv_fac_cbmz(168) / 1.0            /
DATA  spmh_nam_cbmz(169)     , spmh_map_cbmz(169)  &
    / 'hexenol_c3      ', 137            /
DATA  mech_nam_cbmz(169)     , mech_map_cbmz(169)  &
    / 'PAR             ', 13             /
DATA  conv_fac_cbmz(169) / 5.0            /
DATA  spmh_nam_cbmz(170)     , spmh_map_cbmz(170)  &
    / 'hexenol_c3      ', 137            /
DATA  mech_nam_cbmz(170)     , mech_map_cbmz(170)  &
    / 'OLEI            ', 16             /
DATA  conv_fac_cbmz(170) / 1.0            /
DATA  spmh_nam_cbmz(171)     , spmh_map_cbmz(171)  &
    / 'hexenyl_ACT_c3  ', 138            /
DATA  mech_nam_cbmz(171)     , mech_map_cbmz(171)  &
    / 'AONE            ', 18             /
DATA  conv_fac_cbmz(171) / 1.0            /
DATA  spmh_nam_cbmz(172)     , spmh_map_cbmz(172)  &
    / 'hexenyl_ACT_c3  ', 138            /
DATA  mech_nam_cbmz(172)     , mech_map_cbmz(172)  &
    / 'PAR             ', 13             /
DATA  conv_fac_cbmz(172) / 3.0            /
DATA  spmh_nam_cbmz(173)     , spmh_map_cbmz(173)  &
    / 'hexenyl_ACT_c3  ', 138            /
DATA  mech_nam_cbmz(173)     , mech_map_cbmz(173)  &
    / 'OLEI            ', 16             /
DATA  conv_fac_cbmz(173) / 1.0            /


DATA  spmh_nam_cbmz(174)     , spmh_map_cbmz(174)  &
    / 'homosalate      ', 139            /
DATA  mech_nam_cbmz(174)     , mech_map_cbmz(174)  &
    / 'ISOP            ', 1             /
DATA  conv_fac_cbmz(174) / 1.0            /
DATA  spmh_nam_cbmz(175)     , spmh_map_cbmz(175)  &
    / 'Ehsalate        ', 140            /
DATA  mech_nam_cbmz(175)     , mech_map_cbmz(175)  &
    / 'ISOP            ', 1             /
DATA  conv_fac_cbmz(175) / 1.0            /
DATA  spmh_nam_cbmz(176)     , spmh_map_cbmz(176)  &
    / 'pentanal         ', 141            /
DATA  mech_nam_cbmz(176)     , mech_map_cbmz(176)  &
    / 'ALD2            ', 7            /
DATA  conv_fac_cbmz(176) / 1.0            /
DATA  spmh_nam_cbmz(177)     , spmh_map_cbmz(177)  &
    / 'heptanone      ', 142            /
DATA  mech_nam_cbmz(177)     , mech_map_cbmz(177)  &
    / 'AONE           ', 18             /
DATA  conv_fac_cbmz(177) / 1.0            /
DATA  spmh_nam_cbmz(178)     , spmh_map_cbmz(178)  &
    / 'anisole         ', 143            /
DATA  mech_nam_cbmz(178)     , mech_map_cbmz(178)  &
    / 'TOL             ', 19             /
DATA  conv_fac_cbmz(178) / 1.0            /
DATA  spmh_nam_cbmz(179)     , spmh_map_cbmz(179)  &
    / 'verbenene       ', 144            /
DATA  mech_nam_cbmz(179)     , mech_map_cbmz(179)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz(179) / 1.0            /
DATA  spmh_nam_cbmz(180)     , spmh_map_cbmz(180)  &
    / 'benzyl-acetate   ', 145            /
DATA  mech_nam_cbmz(180)     , mech_map_cbmz(180)  &
    / 'TOL             ', 19             /
DATA  conv_fac_cbmz(180) / 1.0            /
DATA  spmh_nam_cbmz(181)     , spmh_map_cbmz(181)  &
    / 'myrtenal         ', 146            /
DATA  mech_nam_cbmz(181)     , mech_map_cbmz(181)  &
    / 'ISOP            ', 1           /
DATA  conv_fac_cbmz(181) / 1.0            /
DATA  spmh_nam_cbmz(182)     , spmh_map_cbmz(182)  &
    / 'benzyl-alcohol ', 147            /
DATA  mech_nam_cbmz(182)     , mech_map_cbmz(182)  &
    / 'TOL             ', 19               /
DATA  conv_fac_cbmz(182) / 1.0            /
DATA  spmh_nam_cbmz(183)     , spmh_map_cbmz(183)  &
    / 'meta-cymenene  ', 148            /
DATA  mech_nam_cbmz(183)     , mech_map_cbmz(183)  &
    / 'ISOP            ', 1              /
DATA  conv_fac_cbmz(183) / 1.0            /
DATA  spmh_nam_cbmz(184)     , spmh_map_cbmz(184)  &
    / 'ipsenol        ', 149            /
DATA  mech_nam_cbmz(184)     , mech_map_cbmz(184)  &
    / 'ISOP           ', 1              /
DATA  conv_fac_cbmz(184) / 1.0            /
DATA  spmh_nam_cbmz(185)     , spmh_map_cbmz(185)  &
    / 'Napthalene     ', 150            /
DATA  mech_nam_cbmz(185)     , mech_map_cbmz(185)  &
    / 'TOL            ', 19               /
DATA  conv_fac_cbmz(185) / 1.0            /

!=======================================================================
!  SPC_CBMZ.EXT
!  This include file contains CBMZ species and their MW.
!
!
!  Mechanism Name: CBMZ
!  MEGAN v2.1.0
!  INPUT version 200
!
!  History:
!  Who          When       What
!  ---------------------------------------------------------------------
!  Tan          12/02/06 - Creates this file
!  Tan          08/14/07 - Move from MEGAN v2.0 to MEGAN v2.1.0 with no update.
!=======================================================================

DATA  mech_spc_cbmz( 1), mech_mwt_cbmz( 1) / 'ISOP ', 68.12   /
DATA  mech_spc_cbmz( 2), mech_mwt_cbmz( 2) / 'NO   ', 30.01   /
DATA  mech_spc_cbmz( 3), mech_mwt_cbmz( 3) / 'NO2  ', 44.01   /
DATA  mech_spc_cbmz( 4), mech_mwt_cbmz( 4) / 'CO   ', 28.01   /
DATA  mech_spc_cbmz( 5), mech_mwt_cbmz( 5) / 'CH3OH', 32.04   /
DATA  mech_spc_cbmz( 6), mech_mwt_cbmz( 6) / 'ANOL ', 46.07   /
DATA  mech_spc_cbmz( 7), mech_mwt_cbmz( 7) / 'ALD2 ', 44.05   /
DATA  mech_spc_cbmz( 8), mech_mwt_cbmz( 8) / 'HCHO ', 30.03   /
DATA  mech_spc_cbmz( 9), mech_mwt_cbmz( 9) / 'HCOOH', 46.03   /
DATA  mech_spc_cbmz(10), mech_mwt_cbmz(10) / 'RCOOH', 60.05   /
DATA  mech_spc_cbmz(11), mech_mwt_cbmz(11) / 'CH4  ', 16.04   /
DATA  mech_spc_cbmz(12), mech_mwt_cbmz(12) / 'C2H6 ', 30.07   /
DATA  mech_spc_cbmz(13), mech_mwt_cbmz(13) / 'PAR  ', 13.00   /
DATA  mech_spc_cbmz(14), mech_mwt_cbmz(14) / 'ETH  ', 28.05   /
DATA  mech_spc_cbmz(15), mech_mwt_cbmz(15) / 'OLET ', 27.00   /
DATA  mech_spc_cbmz(16), mech_mwt_cbmz(16) / 'OLEI ', 26.00   /
DATA  mech_spc_cbmz(17), mech_mwt_cbmz(17) / 'CRES ', 108.14  /
DATA  mech_spc_cbmz(18), mech_mwt_cbmz(18) / 'AONE ', 58.08   /
DATA  mech_spc_cbmz(19), mech_mwt_cbmz(19) / 'TOL  ', 92.14   /
DATA  mech_spc_cbmz(20), mech_mwt_cbmz(20) / 'XYL  ', 106.17  /
DATA  mech_spc_cbmz(21), mech_mwt_cbmz(21) / 'DMS  ', 62.14   /
DATA  mech_spc_cbmz(22), mech_mwt_cbmz(22) / 'NH3  ', 17.03   /
DATA  mech_spc_cbmz(23), mech_mwt_cbmz(23) / 'NR   ', 1.00    /

end module map_cv2mechanism