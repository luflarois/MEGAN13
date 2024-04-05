MODULE eaco
!=======================================================================
 
!  LD_FCT.EXT
!  This include file contains "light dependent" factors.
!  MEGAN v2.02
!  INPUT version 210
!  History:
!  Who          When       What
!  ---------------------------------------------------------------------
!  Tan          12/02/06 - Creates this file
!  Guenther A.  08/11/07 - Move from MEGAN v2.0 to MEGAN v2.02 with updates.
!                          See the update document.
!  Jiang X.     05/07/12 - Update LDFs
!=======================================================================

INTEGER,PARAMETER :: n_ldf_spc  = 20
CHARACTER (LEN=16) :: ldf_spc(n_ldf_spc)
REAL :: ldf_fct(n_ldf_spc)
INTEGER :: ldf_map(n_ldf_spc)

DATA     ldf_spc(  1)      , ldf_fct(  1), ldf_map(  1)  &
    / 'ISOP            ', 0.999      , 1            /

DATA     ldf_spc(  2)      , ldf_fct(  2), ldf_map(  2)  &
    / 'MYRC            ', 0.6        , 2            /

DATA     ldf_spc(  3)      , ldf_fct(  3), ldf_map(  3)  &
    / 'SABI            ', 0.6         , 3            /

DATA     ldf_spc(  4)      , ldf_fct(  4), ldf_map(  4)  &
    / 'LIMO            ', 0.4        , 4            /

DATA     ldf_spc(  5)      , ldf_fct(  5), ldf_map(  5)  &
    / 'A_3CAR            ', 0.4        , 5            /

DATA     ldf_spc(  6)      , ldf_fct(  6), ldf_map(  6)  &
    / 'OCIM            ', 0.4         , 6            /

DATA     ldf_spc(  7)      , ldf_fct(  7), ldf_map(  7)  &
    / 'BPIN            ', 0.4         , 7            /

DATA     ldf_spc(  8)      , ldf_fct(  8), ldf_map(  8)  &
    / 'APIN            ', 0.6         , 8            /

DATA     ldf_spc(  9)      , ldf_fct(  9), ldf_map(  9)  &
    / 'OMTP            ', 0.4         , 9            /

DATA     ldf_spc( 10)      , ldf_fct( 10), ldf_map( 10)  &
    / 'FARN            ', 0.5         , 10           /

DATA     ldf_spc( 11)      , ldf_fct( 11), ldf_map( 11)  &
    / 'BCAR            ', 0.5         , 11           /

DATA     ldf_spc( 12)      , ldf_fct( 12), ldf_map( 12)  &
    / 'OSQT            ', 0.5         , 12           /

DATA     ldf_spc( 13)      , ldf_fct( 13), ldf_map( 13)  &
    / 'MBO             ', 0.999      , 13           /

DATA     ldf_spc( 14)      , ldf_fct( 14), ldf_map( 14)  &
    / 'MEOH            ', 0.8        , 14           /

DATA     ldf_spc( 15)      , ldf_fct( 15), ldf_map( 15)  &
    / 'ACTO            ', 0.2        , 15           /

DATA     ldf_spc( 16)      , ldf_fct( 16), ldf_map( 16)  &
    / 'CO              ', 0.999      , 16           /

DATA     ldf_spc( 17)      , ldf_fct( 17), ldf_map( 17)  &
    / 'NO              ', 0.0         , 17           /

DATA     ldf_spc( 18)      , ldf_fct( 18), ldf_map( 18)  &
    / 'BIDER           ', 0.8         , 18           /

DATA     ldf_spc( 19)      , ldf_fct( 19), ldf_map( 19)  &
    / 'STRESS          ', 0.8         , 19           /

DATA     ldf_spc( 20)      , ldf_fct( 20), ldf_map( 20)  &
    / 'OTHER           ', 0.2         , 20           /

!=======================================================================
!  TEMPD_PRM.EXT
!  This include file contains "temperature dependent" parameter for
!  light-independent emissions.


!  MEGAN v2.02
!  INPUT version 210

!  History:
!  Who          When       What
!  ---------------------------------------------------------------------
!  Tan          12/02/06 - Creates this file
!  Guenther A.  08/11/07 - Move from MEGAN v2.0 to MEGAN v2.02 with updates.
!                          See the update document.
!=======================================================================

INTEGER,PARAMETER :: n_tdf_spc  = 20
CHARACTER (LEN=16) :: tdf_spc(n_tdf_spc)
REAL :: tdf_prm(n_tdf_spc)
INTEGER :: tdf_map(n_tdf_spc)

DATA     tdf_spc(  1)      , tdf_prm(  1), tdf_map(  1)  &
    / 'ISOP            ', 0.13        , 1           /

DATA     tdf_spc(  2)      , tdf_prm(  2), tdf_map(  2)  &
    / 'MYRC            ', 0.1         , 2           /

DATA     tdf_spc(  3)      , tdf_prm(  3), tdf_map(  3)  &
    / 'SABI            ', 0.1         , 3           /

DATA     tdf_spc(  4)      , tdf_prm(  4), tdf_map(  4)  &
    / 'LIMO            ', 0.1         , 4           /

DATA     tdf_spc(  5)      , tdf_prm(  5), tdf_map(  5)  &
    / 'A_3CAR            ', 0.1         , 5           /

DATA     tdf_spc(  6)      , tdf_prm(  6), tdf_map(  6)  &
    / 'OCIM            ', 0.1         , 6           /

DATA     tdf_spc(  7)      , tdf_prm(  7), tdf_map(  7)  &
    / 'BPIN            ', 0.1         , 7           /
DATA     tdf_spc(  8)      , tdf_prm(  8), tdf_map(  8)  &
    / 'APIN            ', 0.1         , 8           /

DATA     tdf_spc(  9)      , tdf_prm(  9), tdf_map(  9)  &
    / 'OMTP            ', 0.1         , 9           /

DATA     tdf_spc( 10)      , tdf_prm( 10), tdf_map( 10)  &
    / 'FARN            ', 0.17        , 10          /

DATA     tdf_spc( 11)      , tdf_prm( 11), tdf_map( 11)  &
    / 'BCAR            ', 0.17        , 11          /

DATA     tdf_spc( 12)      , tdf_prm( 12), tdf_map( 12)  &
    / 'OSQT            ', 0.17        , 12          /

DATA     tdf_spc( 13)      , tdf_prm( 13), tdf_map( 13)  &
    / 'MBO             ', 0.13        , 13          /

DATA     tdf_spc( 14)      , tdf_prm( 14), tdf_map( 14)  &
    / 'MEOH            ', 0.08        , 14          /
DATA     tdf_spc( 15)      , tdf_prm( 15), tdf_map( 15)  &
    / 'ACTO            ', 0.10        , 15          /

DATA     tdf_spc( 16)      , tdf_prm( 16), tdf_map( 16)  &
    / 'CO              ', 0.08        , 16          /

DATA     tdf_spc( 17)      , tdf_prm( 17), tdf_map( 17)  &
    / 'NO              ', 0.10        , 17          /

DATA     tdf_spc( 18)      , tdf_prm( 18), tdf_map( 18)  &
    / 'BIDER           ', 0.13        , 18          /

DATA     tdf_spc( 19)      , tdf_prm( 19), tdf_map( 19)  &
    / 'STRESS          ', 0.1        , 19          /

DATA     tdf_spc( 20)      , tdf_prm( 20), tdf_map( 20)  &
    / 'OTHER           ', 0.1        , 20          /
!------------------------------------------------------------------------
!  relative emission activity index
!------------------------------------------------------------------------
INTEGER,PARAMETER :: n_rea_spc  = 20
CHARACTER (LEN=16) :: rea_spc(n_rea_spc)
INTEGER :: rea_index(n_rea_spc)


DATA     rea_spc(  1)      , rea_index(  1)  &
    / 'ISOP            ', 5                  /

DATA     rea_spc(  2)      , rea_index(  2)  &
    / 'MYRC            ', 2                  /

DATA     rea_spc(  3)      , rea_index(  3)  &
    / 'SABI            ', 2                  /

DATA     rea_spc(  4)      , rea_index(  4)  &
    / 'LIMO            ', 2                  /

DATA     rea_spc(  5)      , rea_index(  5)  &
    / 'A_3CAR            ', 2                  /

DATA     rea_spc(  6)      , rea_index(  6)  &
    / 'OCIM            ', 2                   /

DATA     rea_spc(  7)      , rea_index(  7)  &
    / 'BPIN            ', 2                   /

DATA     rea_spc(  8)      , rea_index(  8)  &
    / 'APIN            ', 2                   /

DATA     rea_spc(  9)      , rea_index(  9)  &
    / 'OMTP            ', 2                   /

DATA     rea_spc( 10)      , rea_index( 10)  &
    / 'FARN            ', 3                   /

DATA     rea_spc( 11)      , rea_index( 11)  &
    / 'BCAR            ', 3                   /

DATA     rea_spc( 12)      , rea_index( 12)  &
    / 'OSQT            ', 3                    /

DATA     rea_spc( 13)      , rea_index( 13)  &
    / 'MBO             ', 5                    /

DATA     rea_spc( 14)      , rea_index( 14)  &
    / 'MEOH            ', 4                     /

DATA     rea_spc( 15)      , rea_index( 15)  &
    / 'ACTO            ', 1                     /

DATA     rea_spc( 16)      , rea_index( 16)  &
    / 'CO             ', 1                     /

DATA     rea_spc( 17)      , rea_index( 17)  &
    / 'NO              ', 1                     /

DATA     rea_spc( 18)      , rea_index( 18)  &
    / 'BIDER           ', 1                     /

DATA     rea_spc( 19)      , rea_index( 19)  &
    / 'STRESS          ', 1                     /

DATA     rea_spc( 20)      , rea_index( 20)  &
    / 'OTHER           ', 1                     /

!=======================================================================
!  REL_EM_ACT.EXT
!  This include file contains "produciton and loss within canopy"
!  factors.


!  MEGAN v2.02
!  INPUT version 200

!  History:
!  Who          When       What
!  ---------------------------------------------------------------------
!  Tan          12/02/06 - Creates this file
!  Tan          08/14/07 - Move from MEGAN v2.0 to MEGAN v2.02 with no update.
!=======================================================================

INTEGER,PARAMETER :: n_cat  = 5
REAL :: anew(n_cat)
REAL :: agro(n_cat)
REAL :: amat(n_cat)
REAL :: aold(n_cat)

DATA    anew(  1),  agro(  1),  amat(  1),  aold(  1)  &
    /  1.0      ,  1.0      ,  1.0      ,  1.0       /

DATA    anew(  2),  agro(  2),  amat(  2),  aold(  2)  &
    /  2.0      ,  1.8      ,  1.0     ,  1.05       /

DATA    anew(  3),  agro(  3),  amat(  3),  aold(  3)  &
    /  0.4      ,  0.6      ,  1.0    ,  0.95       /

DATA    anew(  4),  agro(  4),  amat(  4),  aold(  4)  &
    /  3.5      ,  3.0      ,  1.0     ,  1.2       /

DATA    anew(  5),  agro(  5),  amat(  5),  aold(  5)  &
    /  0.05     ,  0.6      ,  1.0    ,  0.9       /

!=======================================================================
!  SPC_MGN.EXT
!  This include file contains MEGAN species



!  MEGAN v2.02
!  INPUT version 200

!  History:
!  Who          When       What
!  ---------------------------------------------------------------------
!  Tan          12/02/06 - Creates this file
!  Tan          08/14/07 - Move from MEGAN v2.0 to MEGAN v2.02 with no update.
!=======================================================================

INTEGER,PARAMETER :: n_mgn_spc  = 20
CHARACTER (LEN=16) :: mgn_spc(n_mgn_spc)
REAL :: mgn_mwt(n_mgn_spc)

DATA     mgn_spc(  1), mgn_mwt(  1) / 'ISOP            ', 1.0    /

DATA     mgn_spc(  2), mgn_mwt(  2) / 'MYRC            ', 1.0    /
DATA     mgn_spc(  3), mgn_mwt(  3) / 'SABI            ', 1.0    /
DATA     mgn_spc(  4), mgn_mwt(  4) / 'LIMO            ', 1.0    /
DATA     mgn_spc(  5), mgn_mwt(  5) / 'A_3CAR          ', 1.0    /
DATA     mgn_spc(  6), mgn_mwt(  6) / 'OCIM            ', 1.0    /
DATA     mgn_spc(  7), mgn_mwt(  7) / 'BPIN            ', 1.0    /
DATA     mgn_spc(  8), mgn_mwt(  8) / 'APIN            ', 1.0    /
DATA     mgn_spc(  9), mgn_mwt(  9) / 'OMTP            ', 1.0    /

DATA     mgn_spc( 10), mgn_mwt( 10) / 'FARN            ', 1.0    /
DATA     mgn_spc( 11), mgn_mwt( 11) / 'BCAR            ', 1.0    /

DATA     mgn_spc( 12), mgn_mwt( 12) / 'OSQT            ', 1.0    /

DATA     mgn_spc( 13), mgn_mwt( 13) / 'MBO             ', 1.0    /
DATA     mgn_spc( 14), mgn_mwt( 14) / 'MEOH            ', 1.0    /
DATA     mgn_spc( 15), mgn_mwt( 15) / 'ACTO            ', 1.0    /
DATA     mgn_spc( 16), mgn_mwt( 16) / 'CO              ', 1.0    /

DATA     mgn_spc( 17), mgn_mwt( 17) / 'NO              ', 1.0    /

DATA     mgn_spc( 18), mgn_mwt( 18) / 'BIDER           ', 1.0    /

DATA     mgn_spc( 19), mgn_mwt( 19) / 'STRESS          ', 1.0    /

DATA     mgn_spc( 20), mgn_mwt( 20) / 'OTHER           ', 1.0    /


!=======================================================================
!  PDT_LOS_CP.EXT
!  This include file contains "produciton and loss within canopy"
!  factors.


!  MEGAN v2.02
!  INPUT version 200

!  History:
!  Who          When       What
!  ---------------------------------------------------------------------
!  Tan          12/02/06 - Creates this file
!  Tan          08/14/07 - Move from MEGAN v2.0 to MEGAN v2.02 with no update.
!=======================================================================

INTEGER,PARAMETER :: n_rho_spc  = 20
CHARACTER (LEN=16) :: rho_spc(n_rho_spc)
REAL :: rho_fct(n_rho_spc)
INTEGER :: rho_map(n_rho_spc)

DATA     rho_spc(  1)      , rho_fct(  1), rho_map(  1)  &
    / 'ISOP            ',   1.0       , 1           /

DATA     rho_spc(  2)      , rho_fct(  2), rho_map(  2)  &
    / 'MYRC            ',   1.0       , 2           /

DATA     rho_spc(  3)      , rho_fct(  3), rho_map(  3)  &
    / 'SABI            ',   1.0       , 3           /
DATA     rho_spc(  4)      , rho_fct(  4), rho_map(  4)  &
    / 'LIMO            ',   1.0       , 4           /

DATA     rho_spc(  5)      , rho_fct(  5), rho_map(  5)  &
    / 'A_3CAR            ',   1.0       , 5           /

DATA     rho_spc(  6)      , rho_fct(  6), rho_map(  6)  &
    / 'OCIM            ',   1.0       , 6           /

DATA     rho_spc(  7)      , rho_fct(  7), rho_map(  7)  &
    / 'BPIN            ',   1.0       , 7           /

DATA     rho_spc(  8)      , rho_fct(  8), rho_map(  8)  &
    / 'APIN            ',   1.0       , 8           /

DATA     rho_spc(  9)      , rho_fct(  9), rho_map(  9)  &
    / 'OMTP            ',   1.0       , 9           /

DATA     rho_spc( 10)      , rho_fct( 10), rho_map( 10)  &
    / 'FARN            ',   1.0       , 10          /

DATA     rho_spc( 11)      , rho_fct( 11), rho_map( 11)  &
    / 'BCAR            ',   1.0       , 11          /

DATA     rho_spc( 12)      , rho_fct( 12), rho_map( 12)  &
    / 'OSQT            ',   1.0       , 12          /

DATA     rho_spc( 13)      , rho_fct( 13), rho_map( 13)  &
    / 'MBO             ',   1.0       , 13          /
DATA     rho_spc( 14)      , rho_fct( 14), rho_map( 14)  &
    / 'MEOH            ',   1.0       , 14          /

DATA     rho_spc( 15)      , rho_fct( 15), rho_map( 15)  &
    / 'ACTO            ',   1.0       , 15          /

DATA     rho_spc( 16)      , rho_fct( 16), rho_map( 16)  &
    / 'CO             ',   1.0       , 16          /

DATA     rho_spc( 17)      , rho_fct( 17), rho_map( 17)  &
    / 'NO              ',   1.0       , 17          /

DATA     rho_spc( 18)      , rho_fct( 18), rho_map( 18)  &
    / 'BIDER           ',   1.0       , 18          /

DATA     rho_spc( 19)      , rho_fct( 19), rho_map( 19)  &
    / 'STRESS          ',   1.0       , 19          /

DATA     rho_spc( 20)      , rho_fct( 20), rho_map( 20)  &
    / 'OTHER           ',   1.0       , 20          /

!  TEMPD_PRM
!  This include file contains MEGAN species



!  MEGAN v2.1

!  History:
!  Who          When       What
!  ---------------------------------------------------------------------
!  Xuemei      06/15/2009 - Creates this file
!=======================================================================

INTEGER,PARAMETER :: n_prm_spc  = 20
REAL :: ct1( n_prm_spc)
REAL :: cceo(n_prm_spc)

DATA    ct1(1),cceo (1)    /  95.0, 2.0  /
DATA    ct1(2),cceo (2)    /  80.0, 1.83  /
DATA    ct1(3),cceo (3)    /  80.0, 1.83  /
DATA    ct1(4),cceo (4)    /  80.0, 1.83  /
DATA    ct1(5),cceo (5)    /  80.0, 1.83  /
DATA    ct1(6),cceo (6)    /  80.0, 1.83  /
DATA    ct1(7),cceo (7)    /  80.0, 1.83  /
DATA    ct1(8),cceo (8)    /  80.0, 1.83  /
DATA    ct1(9),cceo (9)    /  80.0, 1.83  /
DATA    ct1(10),cceo (10)    / 130.0, 2.37  /
DATA    ct1(11),cceo (11)    / 130.0, 2.37  /
DATA    ct1(12),cceo (12)    / 130.0, 2.37  /
DATA    ct1(13),cceo (13)    /  95.0, 2.0  /
DATA    ct1(14),cceo (14)    /  60.0, 1.6  /
DATA    ct1(15),cceo (15)    /  80.0, 1.83  /
DATA    ct1(16),cceo (16)    /  60.0, 1.6  /
DATA    ct1(17),cceo (17)    /  80.0, 1.83  /
DATA    ct1(18),cceo (18)    /  95.0, 2.0  /
DATA    ct1(19),cceo (19)    /  80.0, 1.83  /
DATA    ct1(20),cceo (20)    /  80.0, 1.83  /

!=======================================================================
!  PFT_MGN.EXT
!  This include file contains MEGAN species
!
!
!
!  MEGAN v2.10
!  INPUT version XXX
!
!  History:
!  Who          When       What
!  ---------------------------------------------------------------------
!  Tan          07/07/11 - Creates this file for MEGANv2.10
!=======================================================================

INTEGER,PARAMETER :: n_mgn_pft = 16
CHARACTER (LEN=16) :: mgn_pft(n_mgn_pft)
CHARACTER (LEN=16) :: mgn_nam(n_mgn_pft)
REAL :: mgn_dum(n_mgn_pft)

DATA     mgn_pft(  1), mgn_dum(  1) / 'NT_EG_TEMP      ', 1.0    /
DATA     mgn_pft(  2), mgn_dum(  2) / 'NT_DC_BORL      ', 1.0    /
DATA     mgn_pft(  3), mgn_dum(  3) / 'NT_EG_BORL      ', 1.0    /
DATA     mgn_pft(  4), mgn_dum(  4) / 'BT_EG_TROP      ', 1.0    /
DATA     mgn_pft(  5), mgn_dum(  5) / 'BT_EG_TEMP      ', 1.0    /
DATA     mgn_pft(  6), mgn_dum(  6) / 'BT_DC_TROP      ', 1.0    /
DATA     mgn_pft(  7), mgn_dum(  7) / 'BT_DC_TEMP      ', 1.0    /
DATA     mgn_pft(  8), mgn_dum(  8) / 'BT_DC_BORL      ', 1.0    /
DATA     mgn_pft(  9), mgn_dum(  9) / 'SB_EG_TEMP      ', 1.0    /
DATA     mgn_pft( 10), mgn_dum( 10) / 'SB_DC_TEMP      ', 1.0    /
DATA     mgn_pft( 11), mgn_dum( 11) / 'SB_DC_BORL      ', 1.0    /
DATA     mgn_pft( 12), mgn_dum( 12) / 'GS_C3_COLD      ', 1.0    /
DATA     mgn_pft( 13), mgn_dum( 13) / 'GS_C3_COOL      ', 1.0    /
DATA     mgn_pft( 14), mgn_dum( 14) / 'GS_C3_WARM      ', 1.0    /
DATA     mgn_pft( 15), mgn_dum( 15) / 'CORN            ', 1.0    /
DATA     mgn_pft( 16), mgn_dum( 16) / 'CROP            ', 1.0    /

DATA     mgn_nam(  1) /'Needleaf evergreen temperate tree   '/
DATA     mgn_nam(  2) /'Needleaf deciduous boreal tree      '/
DATA     mgn_nam(  3) /'Needleaf evergreen boreal tree      '/
DATA     mgn_nam(  4) /'Broadleaf evergreen tropical tree   '/
DATA     mgn_nam(  5) /'Broadleaf evergreen temperate tree  '/
DATA     mgn_nam(  6) /'Broadleaf deciduous tropical tree   '/
DATA     mgn_nam(  7) /'Broadleaf deciduous temperate tree  '/
DATA     mgn_nam(  8) /'Broadleaf deciduous boreal tree     '/
DATA     mgn_nam(  9) /'Broadleaf evergreen temperate shrub '/
DATA     mgn_nam( 10) /'Broadleaf deciduous temperate shrub '/
DATA     mgn_nam( 11) /'Broadleaf deciduous boreal shrub    '/
DATA     mgn_nam( 12) /'Cold C3 grass                       '/
DATA     mgn_nam( 13) /'Cool C3 grass                       '/
DATA     mgn_nam( 14) /'Warm C3 grass                       '/
DATA     mgn_nam( 15) /'Corn                                '/
DATA     mgn_nam( 16) /'Other crops                         '/

END MODULE eaco
