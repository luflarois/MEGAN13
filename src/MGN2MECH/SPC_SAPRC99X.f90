module spc_saprc99x
!=======================================================================

!  SPC_SAPRC99_X_SAPRC99X.EXT
!  This include file contains SAPRC99 (CAMx) species and their MW.


!  Mechanism Name: SAPRC99 (CAMx)
!  MEGAN v2.10
!  INPUT version x.x

!  History:
!  Who          When       What
!  ---------------------------------------------------------------------
!  bkoo         04/13/07 - Created
!  bkoo         05/23/07 - Modified speciation
!=======================================================================

CHARACTER (LEN=16) :: spc_saprc99_xmech
PARAMETER     (spc_saprc99_xmech = 'SAPRC99X        ')

INTEGER :: n_saprc99_x_spc
PARAMETER     (n_saprc99_x_spc = 27)

CHARACTER (LEN=16) :: mech_spc_saprc99_x( n_saprc99_x_spc )  ! Mechanism species name
REAL :: mech_mwt_saprc99_x( n_saprc99_x_spc )  ! Mechanism species molecular weight


DATA  mech_spc_saprc99_x(  1), mech_mwt_saprc99_x(  1) / 'ISOP            ',  68.00  /
DATA  mech_spc_saprc99_x(  2), mech_mwt_saprc99_x(  2) / 'TERP            ', 136.00  /
DATA  mech_spc_saprc99_x(  3), mech_mwt_saprc99_x(  3) / 'OLE2            ',  75.80  /
DATA  mech_spc_saprc99_x(  4), mech_mwt_saprc99_x(  4) / 'ARO2            ', 118.70  /
DATA  mech_spc_saprc99_x(  5), mech_mwt_saprc99_x(  5) / 'ALK5            ', 118.90  /
DATA  mech_spc_saprc99_x(  6), mech_mwt_saprc99_x(  6) / 'XC              ',  12.00  /
DATA  mech_spc_saprc99_x(  7), mech_mwt_saprc99_x(  7) / 'OLE1            ',  72.30  /
DATA  mech_spc_saprc99_x(  8), mech_mwt_saprc99_x(  8) / 'MEOH            ',  32.00  /
DATA  mech_spc_saprc99_x(  9), mech_mwt_saprc99_x(  9) / 'ACET            ',  58.00  /
DATA  mech_spc_saprc99_x( 10), mech_mwt_saprc99_x( 10) / 'CH4             ',  16.00  /
DATA  mech_spc_saprc99_x( 11), mech_mwt_saprc99_x( 11) / 'NH3             ',  17.00  /
DATA  mech_spc_saprc99_x( 12), mech_mwt_saprc99_x( 12) / 'NO              ',  30.00  /
DATA  mech_spc_saprc99_x( 13), mech_mwt_saprc99_x( 13) / 'CCHO            ',  44.00  /
DATA  mech_spc_saprc99_x( 14), mech_mwt_saprc99_x( 14) / 'ALK3            ',  58.60  /
DATA  mech_spc_saprc99_x( 15), mech_mwt_saprc99_x( 15) / 'HC2H            ',  46.00  /
DATA  mech_spc_saprc99_x( 16), mech_mwt_saprc99_x( 16) / 'HCHO            ',  30.00  /
DATA  mech_spc_saprc99_x( 17), mech_mwt_saprc99_x( 17) / 'CO2H            ',  75.00  /
DATA  mech_spc_saprc99_x( 18), mech_mwt_saprc99_x( 18) / 'BALD            ', 106.00  /
DATA  mech_spc_saprc99_x( 19), mech_mwt_saprc99_x( 19) / 'MEK             ',  72.00  /
DATA  mech_spc_saprc99_x( 20), mech_mwt_saprc99_x( 20) / 'RCHO            ',  58.00  /
DATA  mech_spc_saprc99_x( 21), mech_mwt_saprc99_x( 21) / 'ALK4            ',  77.60  /
DATA  mech_spc_saprc99_x( 22), mech_mwt_saprc99_x( 22) / 'ARO1            ',  98.60  /
DATA  mech_spc_saprc99_x( 23), mech_mwt_saprc99_x( 23) / 'BACL            ',  86.00  /
DATA  mech_spc_saprc99_x( 24), mech_mwt_saprc99_x( 24) / 'CO              ',  28.00  /
DATA  mech_spc_saprc99_x( 25), mech_mwt_saprc99_x( 25) / 'ALK1            ',  30.10  /
DATA  mech_spc_saprc99_x( 26), mech_mwt_saprc99_x( 26) / 'ETHE            ',  92.00  /
DATA  mech_spc_saprc99_x( 27), mech_mwt_saprc99_x( 27) / 'ALK2            ',  36.70  /

end module spc_saprc99x