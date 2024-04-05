module spc_cb6
!=======================================================================
!  SPC_CB6_CB6.EXT
!  This include file contains CB6 (CMAQ/CAMx) species and their MW.


!  Mechanism Name: CB6 (CMAQ/CAMx)
!  MEGAN v2.10
!  INPUT version x.x

!  History:
!  Who          When       What
!  ---------------------------------------------------------------------
!  bkoo         04/13/07 - Created
!  Tan          07/18/11 - Updated for MEGANv2.10
!=======================================================================

CHARACTER (LEN=16) :: spc_cb6mech
PARAMETER     (spc_cb6mech = 'CB6             ')

INTEGER :: n_cb6_spc
PARAMETER     (n_cb6_spc = 24)

CHARACTER (LEN=16) :: mech_spc_cb6( n_cb6_spc )  ! Mechanism species name
REAL :: mech_mwt_cb6( n_cb6_spc )  ! Mechanism species molecular weight


DATA  mech_spc_cb6(  1), mech_mwt_cb6(  1) / 'ISOP            ',  80.00  /
DATA  mech_spc_cb6(  2), mech_mwt_cb6(  2) / 'TERP            ', 160.00  /
DATA  mech_spc_cb6(  3), mech_mwt_cb6(  3) / 'PAR             ',  16.00  /
DATA  mech_spc_cb6(  4), mech_mwt_cb6(  4) / 'XYL             ', 128.00  /
DATA  mech_spc_cb6(  5), mech_mwt_cb6(  5) / 'OLE             ',  32.00  /
DATA  mech_spc_cb6(  6), mech_mwt_cb6(  6) / 'NR              ',  16.00  /
DATA  mech_spc_cb6(  7), mech_mwt_cb6(  7) / 'MEOH            ',  16.00  /
DATA  mech_spc_cb6(  8), mech_mwt_cb6(  8) / 'CH4             ',  16.00  /
DATA  mech_spc_cb6(  9), mech_mwt_cb6(  9) / 'NH3             ',  17.00  /
DATA  mech_spc_cb6( 10), mech_mwt_cb6( 10) / 'NO              ',  46.00  /
DATA  mech_spc_cb6( 11), mech_mwt_cb6( 11) / 'ALD2            ',  32.00  /
DATA  mech_spc_cb6( 12), mech_mwt_cb6( 12) / 'ETOH            ',  32.00  /
DATA  mech_spc_cb6( 13), mech_mwt_cb6( 13) / 'FORM            ',  16.00  /
DATA  mech_spc_cb6( 14), mech_mwt_cb6( 14) / 'ALDX            ',  32.00  /
DATA  mech_spc_cb6( 15), mech_mwt_cb6( 15) / 'TOL             ', 112.00  /
DATA  mech_spc_cb6( 16), mech_mwt_cb6( 16) / 'IOLE            ',  64.00  /
DATA  mech_spc_cb6( 17), mech_mwt_cb6( 17) / 'CO              ',  28.00  /
DATA  mech_spc_cb6( 18), mech_mwt_cb6( 18) / 'ETHA            ',  32.00  /
DATA  mech_spc_cb6( 19), mech_mwt_cb6( 19) / 'ETH             ',  28.00  /
DATA  mech_spc_cb6( 20), mech_mwt_cb6( 20) / 'ETHY            ',  26.00  /
DATA  mech_spc_cb6( 21), mech_mwt_cb6( 21) / 'PRPA            ',  44.00  /
DATA  mech_spc_cb6( 22), mech_mwt_cb6( 22) / 'BENZ            ',  78.00  /
DATA  mech_spc_cb6( 23), mech_mwt_cb6( 23) / 'ACET            ',  58.00  /
DATA  mech_spc_cb6( 24), mech_mwt_cb6( 24) / 'KET             ',  58.00  /

end module spc_cb6