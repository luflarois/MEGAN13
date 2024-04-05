module pft_mgn
!=======================================================================

!  PFT_MGN.EXT
!  This include file contains MEGAN species
!
!
!
!  MEGAN v2.10
!  INPUT version 2011a
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
DATA     mgn_pft( 15), mgn_dum( 15) / 'CROP            ', 1.0    /
DATA     mgn_pft( 16), mgn_dum( 16) / 'CORN            ', 1.0    /

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
DATA     mgn_nam( 15) /'Other crops                         '/
DATA     mgn_nam( 16) /'Corn                                '/

end module pft_mgn