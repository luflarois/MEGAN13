module spc_mgn
!=======================================================================

!  SPC_MGN.EXT
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
!  Tan          12/02/06 - Creates this file
!  Tan          08/14/07 - Move from MEGAN v2.0 to MEGAN v2.02 with no update.
!  Tan          07/07/11 - Move to MEGANv2.10
!=======================================================================

INTEGER,PARAMETER :: n_mgn_spc = 20
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

end module spc_mgn
