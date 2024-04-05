module spc_saprc99q
!=======================================================================

!  SPC_SAPRC99_Q_SAPRC99Q.EXT
!  This include file contains SAPRC99 (CMAQ) species and their MW.


!  Mechanism Name: SAPRC99 (CMAQ)
!  MEGAN v2.10
!  INPUT version x.x

!  History:
!  Who          When       What
!  ---------------------------------------------------------------------
!  bkoo         04/13/07 - Created
!  bkoo         05/23/07 - Modified speciation
!=======================================================================

CHARACTER (LEN=16) :: spc_saprc99_qmech
PARAMETER     (spc_saprc99_qmech = 'SAPRC99Q        ')

INTEGER :: n_saprc99_q_spc
PARAMETER     (n_saprc99_q_spc= 27)

CHARACTER (LEN=16) :: mech_spc_saprc99_q( n_saprc99_q_spc )  ! Mechanism species name
REAL :: mech_mwt_saprc99_q( n_saprc99_q_spc )  ! Mechanism species molecular weight


DATA  mech_spc_saprc99_q(  1), mech_mwt_saprc99_q(  1) / 'ISOPRENE        ',  68.00  /
DATA  mech_spc_saprc99_q(  2), mech_mwt_saprc99_q(  2) / 'TRP1            ', 136.00  /
DATA  mech_spc_saprc99_q(  3), mech_mwt_saprc99_q(  3) / 'OLE2            ',  75.80  /
DATA  mech_spc_saprc99_q(  4), mech_mwt_saprc99_q(  4) / 'ARO2            ', 118.70  /
DATA  mech_spc_saprc99_q(  5), mech_mwt_saprc99_q(  5) / 'ALK5            ', 118.90  /
DATA  mech_spc_saprc99_q(  6), mech_mwt_saprc99_q(  6) / 'XC              ',  12.00  /
DATA  mech_spc_saprc99_q(  7), mech_mwt_saprc99_q(  7) / 'OLE1            ',  72.30  /
DATA  mech_spc_saprc99_q(  8), mech_mwt_saprc99_q(  8) / 'MEOH            ',  32.00  /
DATA  mech_spc_saprc99_q(  9), mech_mwt_saprc99_q(  9) / 'ACET            ',  58.00  /
DATA  mech_spc_saprc99_q( 10), mech_mwt_saprc99_q( 10) / 'CH4             ',  16.00  /
DATA  mech_spc_saprc99_q( 11), mech_mwt_saprc99_q( 11) / 'NH3             ',  17.00  /
DATA  mech_spc_saprc99_q( 12), mech_mwt_saprc99_q( 12) / 'NO              ',  30.00  /
DATA  mech_spc_saprc99_q( 13), mech_mwt_saprc99_q( 13) / 'CCHO            ',  44.00  /
DATA  mech_spc_saprc99_q( 14), mech_mwt_saprc99_q( 14) / 'ALK3            ',  58.60  /
DATA  mech_spc_saprc99_q( 15), mech_mwt_saprc99_q( 15) / 'HCOOH           ',  46.00  /
DATA  mech_spc_saprc99_q( 16), mech_mwt_saprc99_q( 16) / 'HCHO            ',  30.00  /
DATA  mech_spc_saprc99_q( 17), mech_mwt_saprc99_q( 17) / 'CCO_OH          ',  75.00  /
DATA  mech_spc_saprc99_q( 18), mech_mwt_saprc99_q( 18) / 'BALD            ', 106.00  /
DATA  mech_spc_saprc99_q( 19), mech_mwt_saprc99_q( 19) / 'MEK             ',  72.00  /
DATA  mech_spc_saprc99_q( 20), mech_mwt_saprc99_q( 20) / 'RCHO            ',  58.00  /
DATA  mech_spc_saprc99_q( 21), mech_mwt_saprc99_q( 21) / 'ALK4            ',  77.60  /
DATA  mech_spc_saprc99_q( 22), mech_mwt_saprc99_q( 22) / 'ARO1            ',  98.60  /
DATA  mech_spc_saprc99_q( 23), mech_mwt_saprc99_q( 23) / 'BACL            ',  86.00  /
DATA  mech_spc_saprc99_q( 24), mech_mwt_saprc99_q( 24) / 'CO              ',  28.00  /
DATA  mech_spc_saprc99_q( 25), mech_mwt_saprc99_q( 25) / 'ALK1            ',  30.10  /
DATA  mech_spc_saprc99_q( 26), mech_mwt_saprc99_q( 26) / 'ETHENE          ',  92.00  /
DATA  mech_spc_saprc99_q( 27), mech_mwt_saprc99_q( 27) / 'ALK2            ',  36.70  /

end module spc_saprc99q