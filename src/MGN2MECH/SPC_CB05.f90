module spc_cb05
!=======================================================================

!  SPC_CB05_CB05.EXT
!  This include file contains CB05 (CMAQ/CAMx) species and their MW.


!  Mechanism Name: CB05 (CMAQ/CAMx)
!  MEGAN v2.10
!  INPUT version x.x

!  History:
!  Who          When       What
!  ---------------------------------------------------------------------
!  bkoo         04/13/07 - Created
!=======================================================================

CHARACTER (LEN=16) :: spc_cb05mech
PARAMETER     (spc_cb05mech = 'CB05            ')

INTEGER :: n_cb05_spc
PARAMETER     (n_cb05_spc = 19)

CHARACTER (LEN=16) :: mech_spc_cb05( n_cb05_spc )  ! Mechanism species name
REAL :: mech_mwt_cb05( n_cb05_spc )  ! Mechanism species molecular weight


DATA  mech_spc_cb05(  1), mech_mwt_cb05(  1) / 'ISOP            ',  80.00  /
DATA  mech_spc_cb05(  2), mech_mwt_cb05(  2) / 'TERP            ', 160.00  /
DATA  mech_spc_cb05(  3), mech_mwt_cb05(  3) / 'PAR             ',  16.00  /
DATA  mech_spc_cb05(  4), mech_mwt_cb05(  4) / 'XYL             ', 128.00  /
DATA  mech_spc_cb05(  5), mech_mwt_cb05(  5) / 'OLE             ',  32.00  /
DATA  mech_spc_cb05(  6), mech_mwt_cb05(  6) / 'NR              ',  16.00  /
DATA  mech_spc_cb05(  7), mech_mwt_cb05(  7) / 'MEOH            ',  16.00  /
DATA  mech_spc_cb05(  8), mech_mwt_cb05(  8) / 'CH4             ',  16.00  /
DATA  mech_spc_cb05(  9), mech_mwt_cb05(  9) / 'NH3             ',  17.00  /
DATA  mech_spc_cb05( 10), mech_mwt_cb05( 10) / 'NO              ',  46.00  /
DATA  mech_spc_cb05( 11), mech_mwt_cb05( 11) / 'ALD2            ',  32.00  /
DATA  mech_spc_cb05( 12), mech_mwt_cb05( 12) / 'ETOH            ',  32.00  /
DATA  mech_spc_cb05( 13), mech_mwt_cb05( 13) / 'FORM            ',  16.00  /
DATA  mech_spc_cb05( 14), mech_mwt_cb05( 14) / 'ALDX            ',  32.00  /
DATA  mech_spc_cb05( 15), mech_mwt_cb05( 15) / 'TOL             ', 112.00  /
DATA  mech_spc_cb05( 16), mech_mwt_cb05( 16) / 'IOLE            ',  64.00  /
DATA  mech_spc_cb05( 17), mech_mwt_cb05( 17) / 'CO              ',  28.00  /
DATA  mech_spc_cb05( 18), mech_mwt_cb05( 18) / 'ETHA            ',  32.00  /
DATA  mech_spc_cb05( 19), mech_mwt_cb05( 19) / 'ETH             ',  28.00  /

end module spc_cb05