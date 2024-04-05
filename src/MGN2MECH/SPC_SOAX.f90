module spc_soax
!=======================================================================

!  SPC_SOAX_SOAX.EXT
!  This include file contains SOA (CAMx) species and their MW.


!  Mechanism Name: SOA (CAMx)
!  MEGAN v2.10
!  INPUT version x.x

!  History:
!  Who          When       What
!  ---------------------------------------------------------------------
!  bkoo         04/13/07 - Created
!  bkoo         07/25/07 - Modified speciation
!=======================================================================

CHARACTER (LEN=16) :: spc_soaxmech
PARAMETER     (spc_soaxmech = 'SOAX            ')

INTEGER :: n_soax_spc
PARAMETER     (n_soax_spc = 8)

CHARACTER (LEN=16) :: mech_spc_soax( n_soax_spc )  ! Mechanism species name
REAL :: mech_mwt_soax( n_soax_spc )  ! Mechanism species molecular weight


DATA  mech_spc_soax(  1), mech_mwt_soax(  1) / 'ISP             ',  68.00  /
DATA  mech_spc_soax(  2), mech_mwt_soax(  2) / 'TRP             ', 136.00  /
DATA  mech_spc_soax(  3), mech_mwt_soax(  3) / 'XYLA            ', 106.00  /
DATA  mech_spc_soax(  4), mech_mwt_soax(  4) / 'CG5             ', 180.00  /
DATA  mech_spc_soax(  5), mech_mwt_soax(  5) / 'SQT             ', 204.00  /
DATA  mech_spc_soax(  6), mech_mwt_soax(  6) / 'TOLA            ',  92.00  /
DATA  mech_spc_soax(  7), mech_mwt_soax(  7) / 'CG6             ', 180.00  /
DATA  mech_spc_soax(  8), mech_mwt_soax(  8) / 'CG4             ', 130.00  /

end module spc_soax