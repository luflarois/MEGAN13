SUBROUTINE xtod(arrin,arrout,nx,ny)
 
use param
IMPLICIT NONE
!-----Interpolates a 2-D array from cross (x) points to dot (d) points


REAL, INTENT(IN)                         :: arrin(mnx,mny)
REAL, INTENT(OUT)                        :: arrout(mnx,mny)
INTEGER, INTENT(IN)                      :: nx
INTEGER, INTENT(IN)                      :: ny



INTEGER :: i,j


arrout(1,1) = arrin(1,1)
arrout(nx,ny) = arrin(nx-1,ny-1)
arrout(1,ny) = arrin(1,ny-1)
arrout(nx,1) = arrin(nx-1,1)

DO j = 2,ny-1
  DO i = 2,nx-1
    arrout(i,j) = 0.25*(arrin(i-1,j-1) + arrin(i,j-1) +  &
        arrin(i,j) + arrin(i-1,j))
  END DO
END DO

DO i = 2,nx-1
  arrout(i,1) = 0.5*(arrin(i-1,1) + arrin(i,1))
  arrout(i,ny) = 0.5*(arrin(i-1,ny-1) + arrin(i,ny-1))
END DO

DO j = 2,ny-1
  arrout(1,j) = 0.5*(arrin(1,j-1) + arrin(1,j))
  arrout(nx,j) = 0.5*(arrin(nx-1,j-1) + arrin(nx-1,j))
END DO

RETURN
END SUBROUTINE xtod
