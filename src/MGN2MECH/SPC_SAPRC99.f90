module spc_saprc99
!=======================================================================

!  SPC_SAPRC99_SAPRC99.EXT
!  This include file contains SAPRC99 species and their MW.


!  Mechanism Name: SAPRC99
!  MEGAN v2.10
!  INPUT version x.x

!  History:
!  Who          When       What
!  ---------------------------------------------------------------------
!  Tan          12/02/06 - Creates this file
!  bkoo         04/13/07 - Modified for new MECHANISM scheme
!=======================================================================

CHARACTER (LEN=16) :: spc_saprc99mech
PARAMETER     (spc_saprc99mech = 'SAPRC99         ')

INTEGER :: n_saprc99_spc
PARAMETER     (n_saprc99_spc = 28)

CHARACTER (LEN=16) :: mech_spc_saprc99( n_saprc99_spc )  ! Mechanism species name
REAL :: mech_mwt_saprc99( n_saprc99_spc )  ! Mechanism species molecular weight

! Note conversion between 134 species and SAPRC99 is done by 1:1 mole


DATA  mech_spc_saprc99(  1), mech_mwt_saprc99(  1) / 'ISOPRENE        ', 68.00   /
DATA  mech_spc_saprc99(  2), mech_mwt_saprc99(  2) / 'TRP1            ', 136.00  /
DATA  mech_spc_saprc99(  3), mech_mwt_saprc99(  3) / 'MEOH            ', 32.00   /
DATA  mech_spc_saprc99(  4), mech_mwt_saprc99(  4) / 'ACET            ', 58.00   /
DATA  mech_spc_saprc99(  5), mech_mwt_saprc99(  5) / 'CH4             ', 16.00   /
DATA  mech_spc_saprc99(  6), mech_mwt_saprc99(  6) / 'NO              ', 30.00   /
DATA  mech_spc_saprc99(  7), mech_mwt_saprc99(  7) / 'NO2             ', 44.01   /
DATA  mech_spc_saprc99(  8), mech_mwt_saprc99(  8) / 'NH3             ', 17.00   /
DATA  mech_spc_saprc99(  9), mech_mwt_saprc99(  9) / 'CCHO            ', 44.00   /
DATA  mech_spc_saprc99( 10), mech_mwt_saprc99( 10) / 'HCOOH           ', 46.00   /
DATA  mech_spc_saprc99( 11), mech_mwt_saprc99( 11) / 'HCHO            ', 30.00   /
DATA  mech_spc_saprc99( 12), mech_mwt_saprc99( 12) / 'CCO_OH          ', 60.00   /
DATA  mech_spc_saprc99( 13), mech_mwt_saprc99( 13) / 'BALD            ', 106.00  /
DATA  mech_spc_saprc99( 14), mech_mwt_saprc99( 14) / 'MEK             ', 72.00   /
DATA  mech_spc_saprc99( 15), mech_mwt_saprc99( 15) / 'RCO_OH          ', 74.00   /
DATA  mech_spc_saprc99( 16), mech_mwt_saprc99( 16) / 'CO              ', 28.00   /
DATA  mech_spc_saprc99( 17), mech_mwt_saprc99( 17) / 'ETHENE          ', 28.00   /
DATA  mech_spc_saprc99( 18), mech_mwt_saprc99( 18) / 'ALK1            ', 30.10   /
DATA  mech_spc_saprc99( 19), mech_mwt_saprc99( 19) / 'ALK2            ', 36.70   /
DATA  mech_spc_saprc99( 20), mech_mwt_saprc99( 20) / 'ALK3            ', 58.60   /
DATA  mech_spc_saprc99( 21), mech_mwt_saprc99( 21) / 'ALK4            ', 77.60   /
DATA  mech_spc_saprc99( 22), mech_mwt_saprc99( 22) / 'ALK5            ', 118.90  /
DATA  mech_spc_saprc99( 23), mech_mwt_saprc99( 23) / 'ARO1            ', 98.60   /
DATA  mech_spc_saprc99( 24), mech_mwt_saprc99( 24) / 'ARO2            ', 118.70  /
DATA  mech_spc_saprc99( 25), mech_mwt_saprc99( 25) / 'OLE1            ', 72.30   /
DATA  mech_spc_saprc99( 26), mech_mwt_saprc99( 26) / 'OLE2            ', 75.80   /
DATA  mech_spc_saprc99( 27), mech_mwt_saprc99( 27) / 'RCHO            ', 58.00   /
DATA  mech_spc_saprc99( 28), mech_mwt_saprc99( 28) / 'NONR            ', 1.00    /

end module spc_saprc99