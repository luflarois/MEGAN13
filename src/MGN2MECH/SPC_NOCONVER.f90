module spc_noconver
!=======================================================================
!  SPC_MGN.EXT
!  This include file contains MEGAN species
!
!
!
!  MEGAN v2.1.0
!  INPUT version 200
!
!  History:
!  Who          When       What
!  ---------------------------------------------------------------------
!  Tan          12/02/06 - Creates this file
!  Tan          08/14/07 - Move from MEGAN v2.0 to MEGAN v2.02 with no update.
!======================================================================

INTEGER,PARAMETER :: n_mgn_spc  = 20
CHARACTER (LEN=16) :: mgn_spc(n_mgn_spc)
REAL :: mgn_mwt(n_mgn_spc)

DATA  mgn_spc(  1), mgn_mwt(  1) / 'ISOP    ', 1.0    /

DATA  mgn_spc(  2), mgn_mwt(  2) / 'MYRC    ', 1.0    /
DATA  mgn_spc(  3), mgn_mwt(  3) / 'SABI    ', 1.0    /
DATA  mgn_spc(  4), mgn_mwt(  4) / 'LIMO    ', 1.0    /
DATA  mgn_spc(  5), mgn_mwt(  5) / 'A_3CAR  ', 1.0    /
DATA  mgn_spc(  6), mgn_mwt(  6) / 'OCIM    ', 1.0    /
DATA  mgn_spc(  7), mgn_mwt(  7) / 'BPIN    ', 1.0    /
DATA  mgn_spc(  8), mgn_mwt(  8) / 'APIN    ', 1.0    /

DATA  mgn_spc(  9), mgn_mwt(  9) / 'OMTP    ', 1.0    /

DATA  mgn_spc( 10), mgn_mwt( 10) / 'FARN    ', 1.0    /
DATA  mgn_spc( 11), mgn_mwt( 11) / 'BCAR    ', 1.0    /

DATA  mgn_spc( 12), mgn_mwt( 12) / 'OSQT    ', 1.0    /

DATA  mgn_spc( 13), mgn_mwt( 13) / 'MBO     ', 1.0    /
DATA  mgn_spc( 14), mgn_mwt( 14) / 'MEOH    ', 1.0    /
DATA  mgn_spc( 15), mgn_mwt( 15) / 'ACTO    ', 1.0    /
DATA  mgn_spc( 16), mgn_mwt( 16) / 'CO      ', 1.0    /

DATA  mgn_spc( 17), mgn_mwt( 17) / 'NO      ', 1.0    /

DATA  mgn_spc( 18), mgn_mwt( 18) / 'BIDER   ', 1.0    /

DATA  mgn_spc( 19), mgn_mwt( 19) / 'STRESS  ', 1.0    /

DATA  mgn_spc( 20), mgn_mwt( 20) / 'OTHER   ', 1.0    /
!=======================================================================
!  SPC_SPCAT.EXT
!  This include file contains MEGAN speciated species and their MW.
!
!  MEGAN v2.1.0
!  INPUT version 200
!
!  History:
!  Who          When       What
!  ---------------------------------------------------------------------
!  Tan          12/02/06 - Creates this file
!  Tan          08/14/07 - Move from MEGAN v2.0 to MEGAN v2.02 with no update.
!=======================================================================

INTEGER,PARAMETER :: n_spca_spc = 150        ! Number of speciated species
CHARACTER (LEN=20) :: spca_spc( n_spca_spc )   ! speciated species name
REAL :: spca_mwt( n_spca_spc )   ! Mechanism species molecular weight

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! _a  = alpha, _b  = beta, _c  = cis, _al = allo,
! _g  = gamma, _d  = delta, _t  = trans, _m  = methyl,
! _p  = para, _o  = ortho, _e  = ene, _ol = ol ,
! met = methyl, 2met= dimethyl, MBO = methylbutenol        ,
! 2s  = disulfide, s   = sulfide, OXD = oxide, ACT = acetate,
! PPPP= propenylpropyl       , DCTT= decatetraene         ,
! COTHER= acetaldehyde
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

! Isoprene
DATA  spca_spc(  1), spca_mwt(  1) / 'isoprene', 68.12  /

! MTP
DATA  spca_spc(  2), spca_mwt(  2) / 'myrcene ', 136.23 /
DATA  spca_spc(  3), spca_mwt(  3) / 'sabinene', 136.23 /
DATA  spca_spc(  4), spca_mwt(  4) / 'limonene', 136.23 /
DATA  spca_spc(  5), spca_mwt(  5) / 'carene_3', 136.23 /
DATA  spca_spc(  6), spca_mwt(  6) / 'ocimene_t_b   ', 136.23 /
DATA  spca_spc(  7), spca_mwt(  7) / 'pinene_b', 136.23 /
DATA  spca_spc(  8), spca_mwt(  8) / 'pinene_a', 136.23 /
! Other MT
DATA  spca_spc(  9), spca_mwt(  9) / 'A_2met_styrene  ', 132.20 /
DATA  spca_spc( 10), spca_mwt( 10) / 'cymene_p', 134.22 /
DATA  spca_spc( 11), spca_mwt( 11) / 'cymene_o', 134.22 /
DATA  spca_spc( 12), spca_mwt( 12) / 'phellandrene_a', 136.23 /
DATA  spca_spc( 13), spca_mwt( 13) / 'thujene_a ', 136.23 /
DATA  spca_spc( 14), spca_mwt( 14) / 'terpinene_a   ', 136.23 /
DATA  spca_spc( 15), spca_mwt( 15) / 'terpinene_g   ', 136.23 /
DATA  spca_spc( 16), spca_mwt( 16) / 'terpinolene   ', 136.23 /
DATA  spca_spc( 17), spca_mwt( 17) / 'phellandrene_b', 136.23 /
DATA  spca_spc( 18), spca_mwt( 18) / 'camphene', 136.23 /
DATA  spca_spc( 19), spca_mwt( 19) / 'bornene ', 136.23 /
DATA  spca_spc( 20), spca_mwt( 20) / 'fenchene_a', 136.23 /
DATA  spca_spc( 21), spca_mwt( 21) / 'ocimene_al', 136.23 /
DATA  spca_spc( 22), spca_mwt( 22) / 'ocimene_c_b   ', 136.23 /
DATA  spca_spc( 23), spca_mwt( 23) / 'tricyclene', 136.23 /
DATA  spca_spc( 24), spca_mwt( 24) / 'estragole ', 148.20 /
DATA  spca_spc( 25), spca_mwt( 25) / 'camphor ', 152.23 /
DATA  spca_spc( 26), spca_mwt( 26) / 'fenchone', 152.23 /
DATA  spca_spc( 27), spca_mwt( 27) / 'piperitone', 152.23 /
DATA  spca_spc( 28), spca_mwt( 28) / 'thujone_a ', 152.23 /
DATA  spca_spc( 29), spca_mwt( 29) / 'thujone_b ', 152.23 /
DATA  spca_spc( 30), spca_mwt( 30) / 'cineole_1_8   ', 154.25 /
DATA  spca_spc( 31), spca_mwt( 31) / 'borneol ', 154.25 /
DATA  spca_spc( 32), spca_mwt( 32) / 'linalool', 154.25 /
DATA  spca_spc( 33), spca_mwt( 33) / 'terpineol_4   ', 154.25 /
DATA  spca_spc( 34), spca_mwt( 34) / 'terpineol_a   ', 154.25 /
DATA  spca_spc( 35), spca_mwt( 35) / 'linalool_OXD_c', 170.25 /
DATA  spca_spc( 36), spca_mwt( 36) / 'linalool_OXD_t', 170.25 /
DATA  spca_spc( 37), spca_mwt( 37) / 'ionone_b', 192.30 /
DATA  spca_spc( 38), spca_mwt( 38) / 'bornyl_ACT', 196.29 /

! SQT
DATA  spca_spc( 39), spca_mwt( 39) / 'farnescene_a  ', 204.35 /
DATA  spca_spc( 40), spca_mwt( 40) / 'caryophyllene_b ', 204.35 /
! Other SQT
DATA  spca_spc( 41), spca_mwt( 41) / 'acoradiene', 204.35 /
DATA  spca_spc( 42), spca_mwt( 42) / 'aromadendrene ', 204.35 /
DATA  spca_spc( 43), spca_mwt( 43) / 'bergamotene_a ', 204.35 /
DATA  spca_spc( 44), spca_mwt( 44) / 'bergamotene_b ', 204.35 /
DATA  spca_spc( 45), spca_mwt( 45) / 'bisabolene_a  ', 204.35 /
DATA  spca_spc( 46), spca_mwt( 46) / 'bisabolene_b  ', 204.35 /
DATA  spca_spc( 47), spca_mwt( 47) / 'bourbonene_b  ', 204.35 /
DATA  spca_spc( 48), spca_mwt( 48) / 'cadinene_d', 204.35 /
DATA  spca_spc( 49), spca_mwt( 49) / 'cadinene_g', 204.35 /
DATA  spca_spc( 50), spca_mwt( 50) / 'cedrene_a ', 204.35 /
DATA  spca_spc( 51), spca_mwt( 51) / 'copaene_a ', 204.35 /
DATA  spca_spc( 52), spca_mwt( 52) / 'cubebene_a', 204.35 /
DATA  spca_spc( 53), spca_mwt( 53) / 'cubebene_b', 204.35 /
DATA  spca_spc( 54), spca_mwt( 54) / 'elemene_b ', 204.35 /
DATA  spca_spc( 55), spca_mwt( 55) / 'farnescene_b  ', 204.35 /
DATA  spca_spc( 56), spca_mwt( 56) / 'germacrene_B  ', 204.35 /
DATA  spca_spc( 57), spca_mwt( 57) / 'germacrene_D  ', 204.35 /
DATA  spca_spc( 58), spca_mwt( 58) / 'gurjunene_b   ', 204.35 /
DATA  spca_spc( 59), spca_mwt( 59) / 'humulene_a', 204.35 /
DATA  spca_spc( 60), spca_mwt( 60) / 'humulene_g', 204.35 /
DATA  spca_spc( 61), spca_mwt( 61) / 'isolongifolene', 204.35 /
DATA  spca_spc( 62), spca_mwt( 62) / 'longifolene   ', 204.35 /
DATA  spca_spc( 63), spca_mwt( 63) / 'longipinene   ', 204.35 /
DATA  spca_spc( 64), spca_mwt( 64) / 'muurolene_a   ', 204.35 /
DATA  spca_spc( 65), spca_mwt( 65) / 'muurolene_g   ', 204.35 /
DATA  spca_spc( 66), spca_mwt( 66) / 'selinene_b', 204.35 /
DATA  spca_spc( 67), spca_mwt( 67) / 'selinene_d', 204.35 /
DATA  spca_spc( 68), spca_mwt( 68) / 'nerolidol_c   ', 222.37 /
DATA  spca_spc( 69), spca_mwt( 69) / 'nerolidol_t   ', 222.37 /
DATA  spca_spc( 70), spca_mwt( 70) / 'cedrol  ', 222.37 /

! VOC
DATA  spca_spc( 71), spca_mwt( 71) / 'MBO_2m3e2ol   ', 86.13  /
DATA  spca_spc( 72), spca_mwt( 72) / 'methanol', 32.04  /
DATA  spca_spc( 73), spca_mwt( 73) / 'acetone ', 58.08  /
DATA  spca_spc( 74), spca_mwt( 74) / 'methane ', 16.04  /
! Ammonia, NO2, and NO
DATA  spca_spc( 75), spca_mwt( 75) / 'ammonia ', 17.03  /
DATA  spca_spc( 76), spca_mwt( 76) / 'nitrous_OXD   ', 44.01  /
DATA  spca_spc( 77), spca_mwt( 77) / 'nitric_OXD', 30.01  /
! Acetaldehyde + ethanol
DATA  spca_spc( 78), spca_mwt( 78) / 'acetaldehyde  ', 44.05  /
DATA  spca_spc( 79), spca_mwt( 79) / 'ethanol ', 46.07  /
! Formic acid + formaldehyde + acetic acid
DATA  spca_spc( 80), spca_mwt( 80) / 'formic_acid   ', 46.03  /
DATA  spca_spc( 81), spca_mwt( 81) / 'formaldehyde  ', 30.03  /
DATA  spca_spc( 82), spca_mwt( 82) / 'acetic_acid   ', 60.05  /
! Other VC
DATA  spca_spc( 83), spca_mwt( 83) / 'MBO_3m2e1ol   ', 86.13  /
DATA  spca_spc( 84), spca_mwt( 84) / 'MBO_3m3e1ol   ', 86.13  /
DATA  spca_spc( 85), spca_mwt( 85) / 'benzaldehyde  ', 106.12 /
DATA  spca_spc( 86), spca_mwt( 86) / 'butanone_2', 72.11  /
DATA  spca_spc( 87), spca_mwt( 87) / 'decanal ', 156.27 /
DATA  spca_spc( 88), spca_mwt( 88) / 'dodecene_1', 168.32 /
DATA  spca_spc( 89), spca_mwt( 89) / 'geranyl_acetone ', 194.31 /
DATA  spca_spc( 90), spca_mwt( 90) / 'heptanal', 114.19 /
DATA  spca_spc( 91), spca_mwt( 91) / 'heptane ', 100.20 /
DATA  spca_spc( 92), spca_mwt( 92) / 'hexane  ', 86.18  /
DATA  spca_spc( 93), spca_mwt( 93) / 'met_benzoate  ', 136.15 /
DATA  spca_spc( 94), spca_mwt( 94) / 'met_heptenone ', 126.20 /
DATA  spca_spc( 95), spca_mwt( 95) / 'neryl_acetone ', 194.31 /
DATA  spca_spc( 96), spca_mwt( 96) / 'nonanal ', 142.24 /
DATA  spca_spc( 97), spca_mwt( 97) / 'nonenal ', 140.22 /
DATA  spca_spc( 98), spca_mwt( 98) / 'octanal ', 128.21 /
DATA  spca_spc( 99), spca_mwt( 99) / 'octanol ', 130.23 /
DATA  spca_spc(100), spca_mwt(100) / 'octenol_1e3ol ', 128.21 /
DATA  spca_spc(101), spca_mwt(101) / 'oxopentanal   ', 100.12 /
DATA  spca_spc(102), spca_mwt(102) / 'pentane ', 72.15  /
DATA  spca_spc(103), spca_mwt(103) / 'phenyl_CCO', 120.15 /
DATA  spca_spc(104), spca_mwt(104) / 'pyruvic_acid  ', 88.06  /
DATA  spca_spc(105), spca_mwt(105) / 'terpinyl_ACT_a', 196.29 /
DATA  spca_spc(106), spca_mwt(106) / 'tetradecene_1 ', 196.37 /
DATA  spca_spc(107), spca_mwt(107) / 'toluene ', 92.14  /
DATA  spca_spc(108), spca_mwt(108) / 'carbon_monoxide ', 28.01  /
DATA  spca_spc(109), spca_mwt(109) / 'butene  ', 56.11  /
DATA  spca_spc(110), spca_mwt(110) / 'ethane  ', 30.07  /
DATA  spca_spc(111), spca_mwt(111) / 'ethene  ', 28.05  /
DATA  spca_spc(112), spca_mwt(112) / 'hydrogen_cyanide', 27.03  /
DATA  spca_spc(113), spca_mwt(113) / 'propane ', 44.10  /
DATA  spca_spc(114), spca_mwt(114) / 'propene ', 42.08  /
DATA  spca_spc(115), spca_mwt(115) / 'carbon_2s ', 76.14  /
DATA  spca_spc(116), spca_mwt(116) / 'carbonyl_s', 60.08  /
DATA  spca_spc(117), spca_mwt(117) / 'diallyl_2s', 146.28 /
DATA  spca_spc(118), spca_mwt(118) / 'A_2met_2s ', 94.20  /
DATA  spca_spc(119), spca_mwt(119) / 'A_2met_s  ', 62.14  /
DATA  spca_spc(120), spca_mwt(120) / 'met_chloride  ', 50.49  /
DATA  spca_spc(121), spca_mwt(121) / 'met_bromide   ', 94.94  /
DATA  spca_spc(122), spca_mwt(122) / 'met_iodide', 141.94 /
DATA  spca_spc(123), spca_mwt(123) / 'hydrogen_s', 34.08  /
DATA  spca_spc(124), spca_mwt(124) / 'met_mercaptan ', 48.11  /
DATA  spca_spc(125), spca_mwt(125) / 'met_propenyl_2s ', 120.24 /
DATA  spca_spc(126), spca_mwt(126) / 'PPPP_2s ', 148.29 /
DATA  spca_spc(127), spca_mwt(127) / 'A_2met_nonatriene',150.26 /
DATA  spca_spc(128), spca_mwt(128) / 'met_salicylate', 152.15 /
DATA  spca_spc(129), spca_mwt(129) / 'indole  ', 117.15 /
DATA  spca_spc(130), spca_mwt(130) / 'jasmone ', 164.24 /
DATA  spca_spc(131), spca_mwt(131) / 'met_jasmonate ', 224.30 /
DATA  spca_spc(132), spca_mwt(132) / 'A_3met_3DCTT', 218.38 /
DATA  spca_spc(133), spca_mwt(133) / 'hexanal ', 100.16 /
DATA  spca_spc(134), spca_mwt(134) / 'hexanol_1 ', 102.17 /
DATA  spca_spc(135), spca_mwt(135) / 'hexenal_c3', 98.14  /
DATA  spca_spc(136), spca_mwt(136) / 'hexenal_t2', 98.14  /
DATA  spca_spc(137), spca_mwt(137) / 'hexenol_c3', 100.16 /
DATA  spca_spc(138), spca_mwt(138) / 'hexenyl_ACT_c3', 142.20 /
DATA  spca_spc(139), spca_mwt(139) / 'homosalate  ', 131 /
DATA  spca_spc(140), spca_mwt(140) / 'Ehsalate ', 131 /
DATA  spca_spc(141), spca_mwt(141) / 'pentanal ', 133 /
DATA  spca_spc(142), spca_mwt(142) / 'heptanone', 94 /
DATA  spca_spc(143), spca_mwt(143) / 'anisole ', 85 /
DATA  spca_spc(144), spca_mwt(144) / 'verbenene ', 10 /
DATA  spca_spc(145), spca_mwt(145) / 'benzyl-acetate', 85  /
DATA  spca_spc(146), spca_mwt(146) / 'myrtenal', 32  /
DATA  spca_spc(147), spca_mwt(147) / 'benzyl-alcohol', 85 /
DATA  spca_spc(148), spca_mwt(148) / 'meta-cymenene', 10 /
DATA  spca_spc(149), spca_mwt(149) / 'ipsenol  ', 32 /
DATA  spca_spc(150), spca_mwt(150) / 'Napthalene ', 129 /
!=======================================================================
!  MAP_MGN20T138.EXT
!  This include file contains conversion table for MEGAN species to
!  134 species
!
!
!  MEGAN v2.1.0
!  INPUT version 200
!
!  History:
!  Who          When       What
!  ---------------------------------------------------------------------
!  Tan          12/02/06 - Creates this file
!  Tan          08/14/07 - Move from MEGAN v2.0 to MEGAN v2.02 with no update.
!=======================================================================

INTEGER,PARAMETER :: n_smap_spc = 150   ! Number of map species

CHARACTER (LEN=16) :: spca_nam( n_smap_spc )   ! speciated species name
INTEGER :: spca_map( n_smap_spc )   ! speciated species name
! mapped to SPCAT_SPC.EXT
CHARACTER (LEN=16) :: mg20_nam( n_smap_spc )   ! MEGAN species
INTEGER :: mg20_map( n_smap_spc )   ! MEGAN species mapped to
! MGN_SPC.EXT

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! _a  = alpha, _b  = beta, _c  = cis, _al = allo,
! _g  = gamma, _d  = delta, _t  = trans, _m  = methyl,
! _p  = para, _o  = ortho, _e  = ene, _ol = ol ,
! met = methyl, 2met= dimethyl, MBO = methylbutenol        ,
! 2s  = disulfide, s   = sulfide, OXD = oxide, ACT = acetate,
! PPPP= propenylpropyl       , DCTT= decatetraene         ,
! COTHER= acetaldehyde
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


DATA  spca_nam(  1), spca_map(  1), mg20_nam(  1), mg20_map(  1)  &
    / 'isoprene', 1     , 'ISOP    ', 1             /
DATA  spca_nam(  2), spca_map(  2), mg20_nam(  2), mg20_map(  2)  &
    / 'myrcene ', 2     , 'MYRC    ', 2             /
DATA  spca_nam(  3), spca_map(  3), mg20_nam(  3), mg20_map(  3)  &
    / 'sabinene', 3     , 'SABI    ', 3             /
DATA  spca_nam(  4), spca_map(  4), mg20_nam(  4), mg20_map(  4)  &
    / 'limonene', 4     , 'LIMO    ', 4             /
DATA  spca_nam(  5), spca_map(  5), mg20_nam(  5), mg20_map(  5)  &
    / 'carene_3', 5     , '3CAR    ', 5             /
DATA  spca_nam(  6), spca_map(  6), mg20_nam(  6), mg20_map(  6)  &
    / 'ocimene_t_b   ', 6     , 'OCIM    ', 6             /
DATA  spca_nam(  7), spca_map(  7), mg20_nam(  7), mg20_map(  7)  &
    / 'pinene_b', 7     , 'BPIN    ', 7             /
DATA  spca_nam(  8), spca_map(  8), mg20_nam(  8), mg20_map(  8)  &
    / 'pinene_a', 8     , 'APIN    ', 8             /
DATA  spca_nam(  9), spca_map(  9), mg20_nam(  9), mg20_map(  9)  &
    / 'A_2met_styrene  ', 9     , 'OMTP    ', 9             /
DATA  spca_nam( 10), spca_map( 10), mg20_nam( 10), mg20_map( 10)  &
    / 'cymene_p', 10    , 'OMTP    ', 9             /
DATA  spca_nam( 11), spca_map( 11), mg20_nam( 11), mg20_map( 11)  &
    / 'cymene_o', 11    , 'OMTP    ', 9             /
DATA  spca_nam( 12), spca_map( 12), mg20_nam( 12), mg20_map( 12)  &
    / 'phellandrene_a', 12    , 'OMTP    ', 9             /
DATA  spca_nam( 13), spca_map( 13), mg20_nam( 13), mg20_map( 13)  &
    / 'thujene_a ', 13    , 'OMTP    ', 9             /
DATA  spca_nam( 14), spca_map( 14), mg20_nam( 14), mg20_map( 14)  &
    / 'terpinene_a   ', 14    , 'OMTP    ', 9             /
DATA  spca_nam( 15), spca_map( 15), mg20_nam( 15), mg20_map( 15)  &
    / 'terpinene_g   ', 15    , 'OMTP    ', 9             /
DATA  spca_nam( 16), spca_map( 16), mg20_nam( 16), mg20_map( 16)  &
    / 'terpinolene   ', 16    , 'OMTP    ', 9             /
DATA  spca_nam( 17), spca_map( 17), mg20_nam( 17), mg20_map( 17)  &
    / 'phellandrene_b', 17    , 'OMTP    ', 9             /
DATA  spca_nam( 18), spca_map( 18), mg20_nam( 18), mg20_map( 18)  &
    / 'camphene', 18    , 'OMTP    ', 9             /
DATA  spca_nam( 19), spca_map( 19), mg20_nam( 19), mg20_map( 19)  &
    / 'bornene ', 19    , 'OMTP    ', 9             /
DATA  spca_nam( 20), spca_map( 20), mg20_nam( 20), mg20_map( 20)  &
    / 'fenchene_a', 20    , 'OMTP    ', 9             /
DATA  spca_nam( 21), spca_map( 21), mg20_nam( 21), mg20_map( 21)  &
    / 'ocimene_al', 21    , 'OMTP    ', 9             /
DATA  spca_nam( 22), spca_map( 22), mg20_nam( 22), mg20_map( 22)  &
    / 'ocimene_c_b   ', 22    , 'OMTP    ', 9             /
DATA  spca_nam( 23), spca_map( 23), mg20_nam( 23), mg20_map( 23)  &
    / 'tricyclene', 23    , 'OMTP    ', 9             /
DATA  spca_nam( 24), spca_map( 24), mg20_nam( 24), mg20_map( 24)  &
    / 'estragole ', 24    , 'OMTP    ', 9             /
DATA  spca_nam( 25), spca_map( 25), mg20_nam( 25), mg20_map( 25)  &
    / 'camphor ', 25    , 'OMTP    ', 9             /
DATA  spca_nam( 26), spca_map( 26), mg20_nam( 26), mg20_map( 26)  &
    / 'fenchone', 26    , 'OMTP    ', 9             /
DATA  spca_nam( 27), spca_map( 27), mg20_nam( 27), mg20_map( 27)  &
    / 'piperitone', 27    , 'OMTP    ', 9             /
DATA  spca_nam( 28), spca_map( 28), mg20_nam( 28), mg20_map( 28)  &
    / 'thujone_a ', 28    , 'OMTP    ', 9             /
DATA  spca_nam( 29), spca_map( 29), mg20_nam( 29), mg20_map( 29)  &
    / 'thujone_b ', 29    , 'OMTP    ', 9             /
DATA  spca_nam( 30), spca_map( 30), mg20_nam( 30), mg20_map( 30)  &
    / 'cineole_1_8   ', 30    , 'OMTP    ', 9             /
DATA  spca_nam( 31), spca_map( 31), mg20_nam( 31), mg20_map( 31)  &
    / 'borneol ', 31    , 'OMTP    ', 9             /
DATA  spca_nam( 32), spca_map( 32), mg20_nam( 32), mg20_map( 32)  &
    / 'linalool', 32    , 'OMTP    ', 9             /
DATA  spca_nam( 33), spca_map( 33), mg20_nam( 33), mg20_map( 33)  &
    / 'terpineol_4   ', 33    , 'OMTP    ', 9             /
DATA  spca_nam( 34), spca_map( 34), mg20_nam( 34), mg20_map( 34)  &
    / 'terpineol_a   ', 34    , 'OMTP    ', 9             /
DATA  spca_nam( 35), spca_map( 35), mg20_nam( 35), mg20_map( 35)  &
    / 'linalool_OXD_c', 35    , 'OMTP    ', 9             /
DATA  spca_nam( 36), spca_map( 36), mg20_nam( 36), mg20_map( 36)  &
    / 'linalool_OXD_t', 36    , 'OMTP    ', 9             /
DATA  spca_nam( 37), spca_map( 37), mg20_nam( 37), mg20_map( 37)  &
    / 'ionone_b', 37    , 'OMTP    ', 9             /
DATA  spca_nam( 38), spca_map( 38), mg20_nam( 38), mg20_map( 38)  &
    / 'bornyl_ACT', 38    , 'OMTP    ', 9             /
DATA  spca_nam( 39), spca_map( 39), mg20_nam( 39), mg20_map( 39)  &
    / 'farnescene_a  ', 39    , 'FARN    ', 10            /
DATA  spca_nam( 40), spca_map( 40), mg20_nam( 40), mg20_map( 40)  &
    / 'caryophyllene_b ', 40    , 'BCAR    ', 11            /
DATA  spca_nam( 41), spca_map( 41), mg20_nam( 41), mg20_map( 41)  &
    / 'acoradiene', 41    , 'OSQT    ', 12            /
DATA  spca_nam( 42), spca_map( 42), mg20_nam( 42), mg20_map( 42)  &
    / 'aromadendrene ', 42    , 'OSQT    ', 12            /
DATA  spca_nam( 43), spca_map( 43), mg20_nam( 43), mg20_map( 43)  &
    / 'bergamotene_a ', 43    , 'OSQT    ', 12            /
DATA  spca_nam( 44), spca_map( 44), mg20_nam( 44), mg20_map( 44)  &
    / 'bergamotene_b ', 44    , 'OSQT    ', 12            /
DATA  spca_nam( 45), spca_map( 45), mg20_nam( 45), mg20_map( 45)  &
    / 'bisabolene_a  ', 45    , 'OSQT    ', 12            /
DATA  spca_nam( 46), spca_map( 46), mg20_nam( 46), mg20_map( 46)  &
    / 'bisabolene_b  ', 46    , 'OSQT    ', 12            /
DATA  spca_nam( 47), spca_map( 47), mg20_nam( 47), mg20_map( 47)  &
    / 'bourbonene_b  ', 47    , 'OSQT    ', 12            /
DATA  spca_nam( 48), spca_map( 48), mg20_nam( 48), mg20_map( 48)  &
    / 'cadinene_d', 48    , 'OSQT    ', 12            /
DATA  spca_nam( 49), spca_map( 49), mg20_nam( 49), mg20_map( 49)  &
    / 'cadinene_g', 49    , 'OSQT    ', 12            /
DATA  spca_nam( 50), spca_map( 50), mg20_nam( 50), mg20_map( 50)  &
    / 'cedrene_a ', 50    , 'OSQT    ', 12            /
DATA  spca_nam( 51), spca_map( 51), mg20_nam( 51), mg20_map( 51)  &
    / 'copaene_a ', 51    , 'OSQT    ', 12            /
DATA  spca_nam( 52), spca_map( 52), mg20_nam( 52), mg20_map( 52)  &
    / 'cubebene_a', 52    , 'OSQT    ', 12            /
DATA  spca_nam( 53), spca_map( 53), mg20_nam( 53), mg20_map( 53)  &
    / 'cubebene_b', 53    , 'OSQT    ', 12            /
DATA  spca_nam( 54), spca_map( 54), mg20_nam( 54), mg20_map( 54)  &
    / 'elemene_b ', 54    , 'OSQT    ', 12            /
DATA  spca_nam( 55), spca_map( 55), mg20_nam( 55), mg20_map( 55)  &
    / 'farnescene_b  ', 55    , 'OSQT    ', 12            /
DATA  spca_nam( 56), spca_map( 56), mg20_nam( 56), mg20_map( 56)  &
    / 'germacrene_B  ', 56    , 'OSQT    ', 12            /
DATA  spca_nam( 57), spca_map( 57), mg20_nam( 57), mg20_map( 57)  &
    / 'germacrene_D  ', 57    , 'OSQT    ', 12            /
DATA  spca_nam( 58), spca_map( 58), mg20_nam( 58), mg20_map( 58)  &
    / 'gurjunene_b   ', 58    , 'OSQT    ', 12            /
DATA  spca_nam( 59), spca_map( 59), mg20_nam( 59), mg20_map( 59)  &
    / 'humulene_a', 59    , 'OSQT    ', 12            /
DATA  spca_nam( 60), spca_map( 60), mg20_nam( 60), mg20_map( 60)  &
    / 'humulene_g', 60    , 'OSQT    ', 12            /
DATA  spca_nam( 61), spca_map( 61), mg20_nam( 61), mg20_map( 61)  &
    / 'isolongifolene', 61    , 'OSQT    ', 12            /
DATA  spca_nam( 62), spca_map( 62), mg20_nam( 62), mg20_map( 62)  &
    / 'longifolene   ', 62    , 'OSQT    ', 12            /
DATA  spca_nam( 63), spca_map( 63), mg20_nam( 63), mg20_map( 63)  &
    / 'longipinene   ', 63    , 'OSQT    ', 12            /
DATA  spca_nam( 64), spca_map( 64), mg20_nam( 64), mg20_map( 64)  &
    / 'muurolene_a   ', 64    , 'OSQT    ', 12            /
DATA  spca_nam( 65), spca_map( 65), mg20_nam( 65), mg20_map( 65)  &
    / 'muurolene_g   ', 65    , 'OSQT    ', 12            /
DATA  spca_nam( 66), spca_map( 66), mg20_nam( 66), mg20_map( 66)  &
    / 'selinene_b', 66    , 'OSQT    ', 12            /
DATA  spca_nam( 67), spca_map( 67), mg20_nam( 67), mg20_map( 67)  &
    / 'selinene_d', 67    , 'OSQT    ', 12            /
DATA  spca_nam( 68), spca_map( 68), mg20_nam( 68), mg20_map( 68)  &
    / 'nerolidol_c   ', 68    , 'OSQT    ', 12            /
DATA  spca_nam( 69), spca_map( 69), mg20_nam( 69), mg20_map( 69)  &
    / 'nerolidol_t   ', 69    , 'OSQT    ', 12            /
DATA  spca_nam( 70), spca_map( 70), mg20_nam( 70), mg20_map( 70)  &
    / 'cedrol  ', 70    , 'OSQT    ', 12            /
DATA  spca_nam( 71), spca_map( 71), mg20_nam( 71), mg20_map( 71)  &
    / 'MBO_2m3e2ol   ', 71    , 'MBO     ', 13            /
DATA  spca_nam( 72), spca_map( 72), mg20_nam( 72), mg20_map( 72)  &
    / 'methanol', 72    , 'MEOH    ', 14            /
DATA  spca_nam( 73), spca_map( 73), mg20_nam( 73), mg20_map( 73)  &
    / 'acetone ', 73    , 'ACTO    ', 15            /
DATA  spca_nam( 74), spca_map( 74), mg20_nam( 74), mg20_map( 74)  &
    / 'methane ', 74    , 'OTHER     ', 20            /
DATA  spca_nam( 75), spca_map( 75), mg20_nam( 75), mg20_map( 75)  &
    / 'ammonia ', 75    , 'NO      ', 17            /
DATA  spca_nam( 76), spca_map( 76), mg20_nam( 76), mg20_map( 76)  &
    / 'nitrous_OXD   ', 76    , 'NO      ', 17            /
DATA  spca_nam( 77), spca_map( 77), mg20_nam( 77), mg20_map( 77)  &
    / 'nitric_OXD', 77    , 'NO      ', 17            /
DATA  spca_nam( 78), spca_map( 78), mg20_nam( 78), mg20_map( 78)  &
    / 'acetaldehyde  ', 78    , 'BIDIR   ', 18            /
DATA  spca_nam( 79), spca_map( 79), mg20_nam( 79), mg20_map( 79)  &
    / 'ethanol ', 79    , 'BIDIR   ', 18            /
DATA  spca_nam( 80), spca_map( 80), mg20_nam( 80), mg20_map( 80)  &
    / 'formic_acid   ', 80    , 'BIDIR   ', 18           /
DATA  spca_nam( 81), spca_map( 81), mg20_nam( 81), mg20_map( 81)  &
    / 'formaldehyde  ', 81    , 'BIDIR   ', 18            /
DATA  spca_nam( 82), spca_map( 82), mg20_nam( 82), mg20_map( 82)  &
    / 'acetic_acid   ', 82    , 'BIDIR   ', 18            /
DATA  spca_nam( 83), spca_map( 83), mg20_nam( 83), mg20_map( 83)  &
    / 'MBO_3m2e1ol   ', 83    , 'OTHER    ', 20            /
DATA  spca_nam( 84), spca_map( 84), mg20_nam( 84), mg20_map( 84)  &
    / 'MBO_3m3e1ol   ', 84    , 'OTHER    ', 20            /
DATA  spca_nam( 85), spca_map( 85), mg20_nam( 85), mg20_map( 85)  &
    / 'benzaldehyde  ', 85    , 'OTHER    ', 20            /
DATA  spca_nam( 86), spca_map( 86), mg20_nam( 86), mg20_map( 86)  &
    / 'butanone_2', 86    , 'OTHER    ', 20            /
DATA  spca_nam( 87), spca_map( 87), mg20_nam( 87), mg20_map( 87)  &
    / 'decanal ', 87    , 'OTHER    ', 20            /
DATA  spca_nam( 88), spca_map( 88), mg20_nam( 88), mg20_map( 88)  &
    / 'dodecene_1', 88    , 'OTHER    ', 20            /
DATA  spca_nam( 89), spca_map( 89), mg20_nam( 89), mg20_map( 89)  &
    / 'geranyl_acetone ', 89    , 'OTHER  ', 20            /
DATA  spca_nam( 90), spca_map( 90), mg20_nam( 90), mg20_map( 90)  &
    / 'heptanal', 90    , 'OTHER     ', 20            /
DATA  spca_nam( 91), spca_map( 91), mg20_nam( 91), mg20_map( 91)  &
    / 'heptane ', 91    , 'OTHER     ', 20            /
DATA  spca_nam( 92), spca_map( 92), mg20_nam( 92), mg20_map( 92)  &
    / 'hexane  ', 92    , 'OTHER      ', 20            /
DATA  spca_nam( 93), spca_map( 93), mg20_nam( 93), mg20_map( 93)  &
    / 'met_benzoate  ', 93    , 'OTHER     ', 20            /
DATA  spca_nam( 94), spca_map( 94), mg20_nam( 94), mg20_map( 94)  &
    / 'met_heptenone ', 94    , 'OTHER     ', 20            /
DATA  spca_nam( 95), spca_map( 95), mg20_nam( 95), mg20_map( 95)  &
    / 'neryl_acetone ', 95    , 'OTHER     ', 20            /
DATA  spca_nam( 96), spca_map( 96), mg20_nam( 96), mg20_map( 96)  &
    / 'nonanal ', 96    , 'OTHER     ', 20            /
DATA  spca_nam( 97), spca_map( 97), mg20_nam( 97), mg20_map( 97)  &
    / 'nonenal ', 97    , 'OTHER     ', 20            /
DATA  spca_nam( 98), spca_map( 98), mg20_nam( 98), mg20_map( 98)  &
    / 'octanal ', 98    , 'OTHER     ', 20            /
DATA  spca_nam( 99), spca_map( 99), mg20_nam( 99), mg20_map( 99)  &
    / 'octanol ', 99    , 'OTHER     ', 20            /
DATA  spca_nam(100), spca_map(100), mg20_nam(100), mg20_map(100)  &
    / 'octenol_1e3ol ', 100   , 'OTHER     ', 20            /
DATA  spca_nam(101), spca_map(101), mg20_nam(101), mg20_map(101)  &
    / 'oxopentanal   ', 101   , 'OTHER     ', 20            /
DATA  spca_nam(102), spca_map(102), mg20_nam(102), mg20_map(102)  &
    / 'pentane ', 102   , 'OTHER     ', 20            /
DATA  spca_nam(103), spca_map(103), mg20_nam(103), mg20_map(103)  &
    / 'phenyl_CCO', 103   , 'OTHER     ', 20            /
DATA  spca_nam(104), spca_map(104), mg20_nam(104), mg20_map(104)  &
    / 'pyruvic_acid  ', 104   , 'OTHER     ', 20            /
DATA  spca_nam(105), spca_map(105), mg20_nam(105), mg20_map(105)  &
    / 'terpinyl_ACT_a', 105   , 'OTHER     ', 20            /
DATA  spca_nam(106), spca_map(106), mg20_nam(106), mg20_map(106)  &
    / 'tetradecene_1 ', 106   , 'OTHER     ', 20            /
DATA  spca_nam(107), spca_map(107), mg20_nam(107), mg20_map(107)  &
    / 'toluene ', 107   , 'STRESS     ', 19            /
DATA  spca_nam(108), spca_map(108), mg20_nam(108), mg20_map(108)  &
    / 'carbon_monoxide ', 108   , 'CO     ', 16            /
DATA  spca_nam(109), spca_map(109), mg20_nam(109), mg20_map(109)  &
    / 'butene  ', 109   , 'OTHER     ', 20            /
DATA  spca_nam(110), spca_map(110), mg20_nam(110), mg20_map(110)  &
    / 'ethane  ', 110   , 'OTHER     ', 20            /
DATA  spca_nam(111), spca_map(111), mg20_nam(111), mg20_map(111)  &
    / 'ethene  ', 111   , 'STRESS     ', 20            /
DATA  spca_nam(112), spca_map(112), mg20_nam(112), mg20_map(112)  &
    / 'hydrogen_cyanide', 112   , 'STRESS     ', 20            /
DATA  spca_nam(113), spca_map(113), mg20_nam(113), mg20_map(113)  &
    / 'propane ', 113   , 'OTHER     ', 20            /
DATA  spca_nam(114), spca_map(114), mg20_nam(114), mg20_map(114)  &
    / 'propene ', 114   , 'OTHER     ', 20            /
DATA  spca_nam(115), spca_map(115), mg20_nam(115), mg20_map(115)  &
    / 'carbon_2s ', 115   , 'OTHER     ', 20            /
DATA  spca_nam(116), spca_map(116), mg20_nam(116), mg20_map(116)  &
    / 'carbonyl_s', 116   , 'OTHER     ', 20            /
DATA  spca_nam(117), spca_map(117), mg20_nam(117), mg20_map(117)  &
    / 'diallyl_2s', 117   , 'OTHER     ', 20            /
DATA  spca_nam(118), spca_map(118), mg20_nam(118), mg20_map(118)  &
    / 'A_2met_2s ', 118   , 'OTHER     ', 20            /
DATA  spca_nam(119), spca_map(119), mg20_nam(119), mg20_map(119)  &
    / 'A_2met_s  ', 119   , 'OTHER     ', 20            /
DATA  spca_nam(120), spca_map(120), mg20_nam(120), mg20_map(120)  &
    / 'met_chloride  ', 120   , 'OTHER     ', 20            /
DATA  spca_nam(121), spca_map(121), mg20_nam(121), mg20_map(121)  &
    / 'met_bromide   ', 121   , 'OTHER     ', 20            /
DATA  spca_nam(122), spca_map(122), mg20_nam(122), mg20_map(122)  &
    / 'met_iodide', 122   , 'OTHER     ', 20            /
DATA  spca_nam(123), spca_map(123), mg20_nam(123), mg20_map(123)  &
    / 'hydrogen_s', 123   , 'OTHER     ', 20            /
DATA  spca_nam(124), spca_map(124), mg20_nam(124), mg20_map(124)  &
    / 'met_mercaptan ', 124   , 'OTHER     ', 20            /
DATA  spca_nam(125), spca_map(125), mg20_nam(125), mg20_map(125)  &
    / 'met_propenyl_2s ', 125   , 'OTHER     ', 20            /
DATA  spca_nam(126), spca_map(126), mg20_nam(126), mg20_map(126)  &
    / 'PPPP_2s ', 126   , 'OTHER     ', 20            /
DATA  spca_nam(127), spca_map(127), mg20_nam(127), mg20_map(127)  &
    / 'A_2met_nonatriene ', 127   , 'STRESS     ', 20            /
DATA  spca_nam(128), spca_map(128), mg20_nam(128), mg20_map(128)  &
    / 'met_salicylate', 128   , 'STRESS     ', 20            /
DATA  spca_nam(129), spca_map(129), mg20_nam(129), mg20_map(129)  &
    / 'indole  ', 129   , 'STRESS     ', 19            /
DATA  spca_nam(130), spca_map(130), mg20_nam(130), mg20_map(130)  &
    / 'jasmone ', 130   , 'STRESS     ', 19            /
DATA  spca_nam(131), spca_map(131), mg20_nam(131), mg20_map(131)  &
    / 'met_jasmonate ', 131   , 'STRESS     ', 19            /
DATA  spca_nam(132), spca_map(132), mg20_nam(132), mg20_map(132)  &
    / '3met_3DCTT', 132   , 'STRESS     ', 19            /
DATA  spca_nam(133), spca_map(133), mg20_nam(133), mg20_map(133)  &
    / 'hexanal ', 133   , 'STRESS    ', 19            /
DATA  spca_nam(134), spca_map(134), mg20_nam(134), mg20_map(134)  &
    / 'hexanol_1 ', 134   , 'STRESS     ', 19            /
DATA  spca_nam(135), spca_map(135), mg20_nam(135), mg20_map(135)  &
    / 'hexenal_c3', 135   , 'STRESS     ', 19            /
DATA  spca_nam(136), spca_map(136), mg20_nam(136), mg20_map(136)  &
    / 'hexenal_t2', 136   , 'STRESS    ', 19            /
DATA  spca_nam(137), spca_map(137), mg20_nam(137), mg20_map(137)  &
    / 'hexenol_c3', 137   , 'STRESS    ', 19            /
DATA  spca_nam(138), spca_map(138), mg20_nam(138), mg20_map(138)  &
    / 'hexenyl_ACT_c3', 138   , 'STRESS     ', 19       /
DATA  spca_nam(139), spca_map(139), mg20_nam(139), mg20_map(139)  &
    / 'homosalate  ', 139   , 'OTHER     ', 20            /
DATA  spca_nam(140), spca_map(140), mg20_nam(140), mg20_map(140)  &
    / 'Ehsalate ', 140   , 'OTHER     ', 20            /
DATA  spca_nam(141), spca_map(141), mg20_nam(141), mg20_map(141)  &
    / 'pentanal ', 141   , 'OTHER     ', 20            /
DATA  spca_nam(142), spca_map(142), mg20_nam(142), mg20_map(142)  &
    / 'heptanone', 142   , 'OTHER     ', 20            /
DATA  spca_nam(143), spca_map(143), mg20_nam(143), mg20_map(143)  &
    / 'anisole ', 143   , 'OTHER    ', 20            /
DATA  spca_nam(144), spca_map(144), mg20_nam(144), mg20_map(144)  &
    / 'verbenene ', 144   , 'OMTP     ', 9            /
DATA  spca_nam(145), spca_map(145), mg20_nam(145), mg20_map(145)  &
    / 'benzyl-acetate', 145   , 'OTHER     ', 20            /
DATA  spca_nam(146), spca_map(146), mg20_nam(146), mg20_map(146)  &
    / 'myrtenal', 146   , 'OMTP    ',  9            /
DATA  spca_nam(147), spca_map(147), mg20_nam(147), mg20_map(147)  &
    / 'benzyl-alcohol', 147   , 'OTHER    ', 20            /
DATA  spca_nam(148), spca_map(148), mg20_nam(148), mg20_map(148)  &
    / 'meta-cymenene', 148   , 'OMTP    ',  9        /
DATA  spca_nam(149), spca_map(149), mg20_nam(149), mg20_map(149)  &
    / 'ipsenol', 149   , 'OMTP    ',  9             /
DATA  spca_nam(150), spca_map(150), mg20_nam(150), mg20_map(150)  &
    / 'Napthalene', 150   , 'OTHER     ', 20            /
end module spc_noconver
