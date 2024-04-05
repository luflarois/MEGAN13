MODULE const_canopy

INTEGER,PARAMETER :: layers =5

REAL,PARAMETER ::  &
    convertwm2toumolm2s = 4.766,       &! Convert radiation from [W/m2] to [umol/m2/s1]  
    solarconstant       = 1367,        &! Solar constant [W/m2]  
    waterairratio       = 18.016/28.97  ! Ratio between water and air molecules

REAL,PARAMETER :: pstd_sun =200.0, pstd_shade =50.0
REAL,PARAMETER :: cce =0.56

!=======================================================================
!  CANOPY.EXT
!  This include file contains MEGAN species

!  Who                   When       What
!  ---------------------------------------------------------------------
!  Xuemei Wang          06/16/2009 - Creates this file
!=======================================================================

INTEGER,PARAMETER :: n_mgn_spc  = 20
CHARACTER (LEN=16) :: mgn_spc(n_mgn_spc)
REAL :: cleo ( n_mgn_spc)
REAL :: ctm1 ( n_mgn_spc)

DATA     mgn_spc(  1) , cleo(1) ,   ctm1(1) / 'ISOP ' , 2.0, 95.0   /
DATA     mgn_spc(  2), cleo(2) ,   ctm1(2) / 'MYRC ', 1.83, 80.0   /
DATA     mgn_spc(  3), cleo(3) ,   ctm1(3) / 'SABI ', 1.83, 80.0   /
DATA     mgn_spc(  4), cleo(4) ,   ctm1(4) / 'LIMO  ',  1.83, 80.0  /
DATA     mgn_spc(  5), cleo(5) ,   ctm1(5) / 'A_3CAR ',  1.83, 80.0    /
DATA     mgn_spc(  6), cleo(6) ,   ctm1(6) / 'OCIM    ', 1.83, 80.0    /
DATA     mgn_spc(  7), cleo(7) ,   ctm1(7) / 'BPIN    ',  1.83, 80.0    /
DATA     mgn_spc(  8), cleo(8) ,   ctm1(8) / 'APIN   ',  1.83, 80.0    /
DATA     mgn_spc(  9), cleo(9) ,   ctm1(9) / 'OMTP   ',   1.83, 80.0   /
DATA     mgn_spc( 10), cleo(10) ,   ctm1(10) / 'FARN   ',  2.37,130.0    /
DATA     mgn_spc( 11), cleo(11),   ctm1(11) / 'BCAR   ',  2.37,130.0    /
DATA     mgn_spc( 12), cleo(12) ,   ctm1(12) / 'OSQT   ',  2.37,130.0    /
DATA     mgn_spc( 13), cleo(13) ,   ctm1(13) / 'MBO    ',  2.0, 95.0    /
DATA     mgn_spc( 14), cleo(14) ,   ctm1(14) / 'MEOH   ',  1.6, 60.0    /
DATA     mgn_spc( 15), cleo(15) ,   ctm1(15) / 'ACTO   ',  1.83, 80.0    /
DATA     mgn_spc( 16), cleo(16) ,   ctm1(16) / 'CO     ',  1.6, 60.0    /
DATA     mgn_spc( 17), cleo(17) ,   ctm1(17) / 'NO     ',  1.83, 80.0    /
DATA     mgn_spc( 18), cleo(18) ,   ctm1(18) / 'BIDER  ',  2.0, 95.0    /
DATA     mgn_spc( 19), cleo(19) ,   ctm1(19) / 'STRESS ', 1.83, 80.0    /
DATA     mgn_spc( 20), cleo(20) ,   ctm1(20) / 'OTHER  ', 1.83, 80.0     /


END MODULE const_canopy