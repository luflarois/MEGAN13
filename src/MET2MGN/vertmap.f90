SUBROUTINE vertmap(nx,ny,nz,kz1,kz2,sigma,xin,xout)

use param

 IMPLICIT NONE

!-----VERTMAP vertically aggregates MM5 data on the met model grid
!     to the CAMx grid

!     NOTE: the CAMx physical height grid is a coarser set of the
!           MM5 sigma-p coordinate system


INTEGER, INTENT(IN)                      :: nx
INTEGER, INTENT(IN)                      :: ny
INTEGER, INTENT(IN)                      :: nz
INTEGER, INTENT(IN)                      :: kz1(mnzc)
INTEGER, INTENT(IN)                      :: kz2(mnzc)
REAL, INTENT(IN)                         :: sigma(0:mnz)
REAL, INTENT(IN)                         :: xin(mnxc,mnyc,mnz)
REAL, INTENT(OUT)                        :: xout(mnxc,mnyc,mnzc)

INTEGER :: i,j,k,kk
REAL :: sum,dsigma

DO j = 1,ny
  DO i = 1,nx
    DO k = 1,nz
      sum = 0.
      DO kk = kz1(k),kz2(k)
        dsigma = sigma(kk-1) - sigma(kk)
        sum = sum + xin(i,j,kk)*dsigma
      END DO
      dsigma = sigma(kz1(k)-1) - sigma(kz2(k))
      xout(i,j,k) = sum/dsigma
    END DO
  END DO
END DO

RETURN
END SUBROUTINE vertmap
