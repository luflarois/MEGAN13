module param
!-----Note: mnx is the MM5 west to east coordinate
!           mny is the MM5 south to north coordinate
!           mnz is the MM5 vertical coortinate

!           mnxc is the CAMx west to east coordinate
!           mnyc is the CAMx south to north coordinate
!           mnzc is the CAMx vertical coortinate

INTEGER :: mnx,mny,mnz,mnxc,mnyc,mnzc,numvals,numprogs

PARAMETER(mnx=200,mny=200,mnz=50)
PARAMETER(mnxc=200,mnyc=200,mnzc=40)

PARAMETER (numvals=1000,numprogs=20)

end module param
