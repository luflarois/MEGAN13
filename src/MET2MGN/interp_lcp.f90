SUBROUTINE interp_lcp(nx,ny,nz,nxc,nyc,nzc,kz1,kz2,ioffset,  &
        joffset,deltax,dxcamx,kvmeth)
 
USE param
USE fields
IMPLICIT NONE


!-----INTERP horizontally interpolates, then vertically aggregates,
!     MM5 data on the met model grid to the CAMx grid

!     NOTE: CAMx grid is simply a windowed area of the MM5 Lambert Conformal
!           grid, and the CAMx physical height grid is a coarser set of the
!           MM5 sigma-p coordinate system. This version allows simple
!           interpolation from coarse to fine resolution, assuming that the
!           fine CAMx grid exactly aligns with the coarser MM5 grid
!           (i.e., the CAMx grid is effectively a nest of the MM5 grid).


INTEGER, INTENT(IN)                      :: nx
INTEGER, INTENT(IN)                      :: ny
INTEGER, INTENT(IN)                      :: nz
INTEGER, INTENT(IN)                      :: nxc
INTEGER, INTENT(IN)                      :: nyc
INTEGER, INTENT(IN)                      :: nzc
INTEGER, INTENT(IN OUT)                  :: kz1(mnzc)
INTEGER, INTENT(IN)                      :: kz2(mnzc)
INTEGER, INTENT(IN)                      :: ioffset
INTEGER, INTENT(IN)                      :: joffset
REAL, INTENT(IN OUT)                     :: deltax
REAL, INTENT(IN OUT)                     :: dxcamx
CHARACTER (LEN=10), INTENT(IN)           :: kvmeth

INTEGER :: i,j,k,n,istd,jstd,istx,jstx,iid,jjd,iix,jjx,ic,jc,ii,jj
REAL :: mshfac,du1,du2,u1,u2,du,dv1,dv2,v1,v2,dv,pstarx,pstary

!-----Couple MM5 variables to P*

DO k = 1,nz
  DO j = 1,ny
    DO i = 1,nx
      ua(i,j,k) = ua(i,j,k)*psad(i,j)
      va(i,j,k) = va(i,j,k)*psad(i,j)
    END DO
  END DO
  DO j = 1,ny-1
    DO i = 1,nx-1
      ta(i,j,k) = ta(i,j,k)*psax(i,j)
      pa(i,j,k) = pa(i,j,k)*psax(i,j)
      qa(i,j,k) = qa(i,j,k)*psax(i,j)
    END DO
  END DO
END DO

!-----Compute the starting indices of MM5 grid points to use for
!     the CAMx grid

istd = ioffset + 2
jstd = joffset + 2
istx = istd - 1
jstx = jstd - 1
mshfac = FLOAT(nint(deltax/dxcamx))

!-----Interpolate wind components from Arakawa B grid (MM5) to
!     Arakawa C grid (CAMx)

DO k = 1,nz
  IF (mshfac == 1.) THEN
    
!-----Simple case of windowing the MM5 grid with 1:1 meshing
    
    DO j = 1,nyc
      DO i = 1,nxc
        iid = istd + i - 1
        jjd = jstd + j - 1
        utmp(i,j,k) = (ua(iid,jjd,k) + ua(iid,jjd-1,k))/2.
        vtmp(i,j,k) = (va(iid,jjd,k) + va(iid-1,jjd,k))/2.
        iix = istx + i - 1
        jjx = jstx + j - 1
        ttmp(i,j,k)  = ta(iix,jjx,k)
        ptmp(i,j,k)  = pa(iix,jjx,k)
        qtmp(i,j,k)  = qa(iix,jjx,k)
        cwtmp(i,j,k) = qc(iix,jjx,k)
        prtmp(i,j,k) = qpr(iix,jjx,k)
        pstmp(i,j,k) = qps(iix,jjx,k)
        pgtmp(i,j,k) = qpg(iix,jjx,k)
        odtmp(i,j,k) = tau(iix,jjx,k)
        IF (kvmeth == 'TKE') tktmp(i,j,k) = tke(iix,jjx,k)
        ztmp(i,j,k) = zh(iix,jjx,k)
        IF (k == 1) THEN
          pstar(i,j) = psax(iix,jjx)
          tsfc(i,j)  = tsrf(iix,jjx)
          topcx(i,j) = topo(iix,jjx)
          rground(i,j) = rgrnd(iix,jjx)
          
          DO n = 1,11
            lucx(i,j,n)  = clu(iix,jjx,n)
          END DO
          IF (kvmeth == 'OB70' .OR. kvmeth == 'CMAQ') THEN
            pblc(i,j) = pbl(iix,jjx)
            z0c(i,j)  = z0(iix,jjx)
          END IF
        END IF
      END DO
    END DO
  ELSE
    
!-----Complex case of windowing and interpolating to finer resolution
!     Winds first
    
    jc = 0
    DO j = jstd,jstd+nyc/INT(mshfac)-1
      ic = 0
      DO i = istd,istd+nxc/INT(mshfac)-1
        
        du1 = ua(i,j-1,k) - ua(i-1,j-1,k)
        du2 = ua(i,j,k) - ua(i-1,j,k)
        DO ii = 1,INT(mshfac)
          u1 = ua(i-1,j-1,k) + du1*ii/mshfac
          u2 = ua(i-1,j,k) + du2*ii/mshfac
          du = u2 - u1
          DO jj = 1,INT(mshfac)
            utmp(ic+ii,jc+jj,k) = u1 + du* (1. + 2.*(jj - 1))/(2.*mshfac)
          END DO
        END DO
        
        dv1 = va(i-1,j,k) - va(i-1,j-1,k)
        dv2 = va(i,j,k) - va(i,j-1,k)
        DO jj = 1,INT(mshfac)
          v1 = va(i-1,j-1,k) + dv1*jj/mshfac
          v2 = va(i,j-1,k) + dv2*jj/mshfac
          dv = v2 - v1
          DO ii = 1,INT(mshfac)
            vtmp(ic+ii,jc+jj,k) = v1 + dv* (1. + 2.*(ii - 1))/(2.*mshfac)
          END DO
        END DO
        
        ic = ic + INT(mshfac)
      END DO
      jc = jc + INT(mshfac)
    END DO
    
!-----Now cell-centered variables
    
    CALL finelcp(istx,jstx,nx,ny,nxc,nyc,mshfac, zh(1,1,k),ztmp(1,1,k))
    CALL finelcp(istx,jstx,nx,ny,nxc,nyc,mshfac, ta(1,1,k),ttmp(1,1,k))
    CALL finelcp(istx,jstx,nx,ny,nxc,nyc,mshfac, pa(1,1,k),ptmp(1,1,k))
    CALL finelcp(istx,jstx,nx,ny,nxc,nyc,mshfac, qa(1,1,k),qtmp(1,1,k))
    CALL finelcp(istx,jstx,nx,ny,nxc,nyc,mshfac, qc(1,1,k),cwtmp(1,1,k))
    CALL finelcp(istx,jstx,nx,ny,nxc,nyc,mshfac, qpr(1,1,k),prtmp(1,1,k))
    CALL finelcp(istx,jstx,nx,ny,nxc,nyc,mshfac, qps(1,1,k),pstmp(1,1,k))
    CALL finelcp(istx,jstx,nx,ny,nxc,nyc,mshfac, qpg(1,1,k),pgtmp(1,1,k))
    CALL finelcp(istx,jstx,nx,ny,nxc,nyc,mshfac, tau(1,1,k),odtmp(1,1,k))
    IF (kvmeth == 'TKE') THEN
      CALL finelcp(istx,jstx,nx,ny,nxc,nyc,mshfac, tke(1,1,k),tktmp(1,1,k))
    ELSE IF ((kvmeth == 'OB70' .OR. kvmeth == 'CMAQ')  &
          .AND. k == 1) THEN
      CALL finelcp(istx,jstx,nx,ny,nxc,nyc,mshfac, pbl,pblc)
      CALL finelcp(istx,jstx,nx,ny,nxc,nyc,mshfac, z0,z0c)
    END IF
    IF (k == 1) THEN
      CALL finelcp(istx,jstx,nx,ny,nxc,nyc,mshfac, psax,pstar)
      CALL finelcp(istx,jstx,nx,ny,nxc,nyc,mshfac, tsrf,tsfc)
      CALL finelcp(istx,jstx,nx,ny,nxc,nyc,mshfac, topo,topcx)
      CALL finelcp(istx,jstx,nx,ny,nxc,nyc,mshfac, rgrnd,rground)
      DO n = 1,11
        CALL finelcp(istx,jstx,nx,ny,nxc,nyc,mshfac, clu(1,1,n),lucx(1,1,n))
      END DO
    END IF
    
  END IF
END DO

!-----Map momentum and thermodynamic variables onto the CAMx vertical
!     grid structure

CALL vertmap(nxc,nyc,nzc,kz1,kz2,sigma,utmp,uac)
CALL vertmap(nxc,nyc,nzc,kz1,kz2,sigma,vtmp,vac)
CALL vertmap(nxc,nyc,nzc,kz1,kz2,sigma,ttmp,tac)
CALL vertmap(nxc,nyc,nzc,kz1,kz2,sigma,ptmp,pac)
CALL vertmap(nxc,nyc,nzc,kz1,kz2,sigma,qtmp,qac)

!-----Decouple vertically interpolated variables from P*

DO k = 1,nzc
  DO j = 1,nyc
    DO i = 1,nxc
      IF (i == nxc) THEN
        uac(i,j,k) = uac(i,j,k)/pstar(i,j)
      ELSE
        pstarx = (pstar(i,j) + pstar(i+1,j))/2.
        uac(i,j,k) = uac(i,j,k)/pstarx
      END IF
      IF (j == nyc) THEN
        vac(i,j,k) = vac(i,j,k)/pstar(i,j)
      ELSE
        pstary = (pstar(i,j) + pstar(i,j+1))/2.
        vac(i,j,k) = vac(i,j,k)/pstary
      END IF
    END DO
  END DO
  DO j = 1,nyc
    DO i = 1,nxc
      tac(i,j,k) = tac(i,j,k)/pstar(i,j)
      pac(i,j,k) = pac(i,j,k)/pstar(i,j)
      qac(i,j,k) = qac(i,j,k)/pstar(i,j)
    END DO
  END DO
END DO

!-----Map layer interface heights and TKE to CAMx vertical grid

DO j = 1,nyc
  DO i = 1,nxc
    DO k = 1,nzc
      zhc(i,j,k) = ztmp(i,j,kz2(k))
      IF (kvmeth == 'TKE') tkc(i,j,k) = tktmp(i,j,kz2(k))
    END DO
  END DO
END DO

RETURN
END SUBROUTINE interp_lcp

!-----------------------------------------------------------------------

SUBROUTINE finelcp(istx,jstx,nx,ny,nxc,nyc,mshfac,cc,ff)

!-----FINELCP horizontally interpolates an input field on LCP cross points
!     to a finer LCP grid
USE param
IMPLICIT NONE


INTEGER, INTENT(IN)                      :: istx
INTEGER, INTENT(IN)                      :: jstx
INTEGER, INTENT(IN OUT)                  :: nx
INTEGER, INTENT(IN OUT)                  :: ny
INTEGER, INTENT(IN)                      :: nxc
INTEGER, INTENT(IN)                      :: nyc
REAL, INTENT(IN)                         :: mshfac
REAL, INTENT(IN)                         :: cc(mnx,mny)
REAL, INTENT(OUT)                        :: ff(mnxc,mnyc)


INTEGER :: i,j,ic,jc,ii,jj
REAL :: dc1,dc2,c1,c2,dc

jc = -1
DO j = jstx,jstx+nyc/INT(mshfac)
  ic = -1
  DO i = istx,istx+nxc/INT(mshfac)
    
    dc1 = cc(i,j-1) - cc(i-1,j-1)
    dc2 = cc(i,j) - cc(i-1,j)
    DO ii = 1,INT(mshfac)
      c1 = cc(i-1,j-1) + dc1*ii/mshfac
      c2 = cc(i-1,j) + dc2*ii/mshfac
      dc = c2 - c1
      DO  jj = 1,INT(mshfac)
        IF (ic+ii < 1 .OR. jc+jj < 1 .OR.  &
            ic+ii > nxc .OR. jc+jj > nyc) GO TO 100
        ff(ic+ii,jc+jj) = c1 + dc*jj/mshfac
100   enddo
    END DO
    
    ic = ic + INT(mshfac)
  END DO
  jc = jc + INT(mshfac)
END DO

RETURN
END SUBROUTINE finelcp
