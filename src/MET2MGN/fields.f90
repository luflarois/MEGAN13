module fields
   use param
!-----MM5 fields

REAL :: ua(mnx,mny,mnz),va(mnx,mny,mnz),ta(mnx,mny,mnz),  &
    qa(mnx,mny,mnz),tke(mnx,mny,mnz),pa(mnx,mny,mnz),  &
    zh(mnx,mny,mnz),ql(mnx,mny,mnz),qi(mnx,mny,mnz),  &
    qr(mnx,mny,mnz),qs(mnx,mny,mnz),qg(mnx,mny,mnz),  &
    qc(mnx,mny,mnz),qcold(mnx,mny,mnz),qpr(mnx,mny,mnz),  &
    qps(mnx,mny,mnz),qpg(mnx,mny,mnz),tau(mnx,mny,mnz)
REAL :: psax(mnx,mny),psad(mnx,mny),trn(mnx,mny),pbl(mnx,mny),  &
    tsrf(mnx,mny),rainr(mnx,mny),rainc(mnx,mny),rainro(mnx,mny),  &
    rainco(mnx,mny),z0(mnx,mny),topo(mnx,mny),clu(mnx,mny,11), rgrnd(mnx,mny)
REAL :: sigma(0:mnz)
REAL :: ptop
REAL :: xdot(mnx),ydot(mny),xcrs(mnx),ycrs(mny),xdprj(mnx,mny), ydprj(mnx,mny)
COMMON /mm5fld/ ua,va,ta,qa,tke,pa,zh,ql,qi,qr,qs,qg,  &
    qc,qcold,qpr,qps,qpg,tau,psax,psad,trn,pbl,  &
    tsrf,rainr,rainc,rainro,rainco,z0,sigma,ptop, topo,rgrnd,clu
COMMON /mm5grd/ xdot,ydot,xcrs,ycrs,xdprj,ydprj

!-----Intermediate fields (horizontally interpolated)

REAL :: utmp(mnxc,mnyc,mnz),vtmp(mnxc,mnyc,mnz),ttmp(mnxc,mnyc,mnz),  &
    qtmp(mnxc,mnyc,mnz),ptmp(mnxc,mnyc,mnz),tktmp(mnxc,mnyc,mnz),  &
    ztmp(mnxc,mnyc,mnz),cwtmp(mnxc,mnyc,mnz),  &
    prtmp(mnxc,mnyc,mnz),pstmp(mnxc,mnyc,mnz),  &
    pgtmp(mnxc,mnyc,mnz),odtmp(mnxc,mnyc,mnz)
COMMON /tmpfld/ utmp,vtmp,ttmp,qtmp,ptmp,tktmp,ztmp,cwtmp,prtmp,  &
    pstmp,pgtmp,odtmp

!-----Final CAMx fields (vertically aggregated)

REAL :: uac(mnxc,mnyc,mnzc),vac(mnxc,mnyc,mnzc),tac(mnxc,mnyc,mnzc),  &
    qac(mnxc,mnyc,mnzc),pac(mnxc,mnyc,mnzc),zhc(mnxc,mnyc,mnzc),  &
    rkv(mnxc,mnyc,mnzc),tkc(mnxc,mnyc,mnzc)
REAL :: cwc(mnxc,mnyc,mnzc),pwr(mnxc,mnyc,mnzc),pws(mnxc,mnyc,mnzc),  &
    pwg(mnxc,mnyc,mnzc),cod(mnxc,mnyc,mnzc),pwtr(mnxc,mnyc,mnzc)
REAL :: rain(mnxc,mnyc),pstar(mnxc,mnyc),tsfc(mnxc,mnyc),  &
    pblc(mnxc,mnyc),z0c(mnxc,mnyc),topcx(mnxc,mnyc),rground(mnxc,mnyc),  &
    lucx(mnxc,mnyc,11)
REAL :: xc(mnxc),yc(mnyc),xclcp(mnxc,mnyc),yclcp(mnxc,mnyc)
REAL :: kvmin
INTEGER :: idot(mnxc,mnyc),jdot(mnxc,mnyc),icrs(mnxc,mnyc), jcrs(mnxc,mnyc)
COMMON /camxfld/ uac,vac,tac,qac,pac,zhc,rkv,tkc,cwc,pwr,pws,pwg,  &
    cod,rain,pstar,tsfc,rground,pblc,z0c,topcx,lucx
COMMON /camxgrd/ xc,yc,xclcp,yclcp,idot,jdot,icrs,jcrs,kvmin
END module fields