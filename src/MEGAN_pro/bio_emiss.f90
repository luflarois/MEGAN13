!--------------------------------------------------------------------
! This program that converts the following emissions data into 
!    into WRF input data data files. The potential fields are:
!       1) biogenic emissions data
!--------------------------------------------------------------------

program map_megan2_emissions

   use area_mapper, only : xlong => lon, xlat => lat
   use bio_types

   implicit none

   INTEGER, parameter :: PS = 2                             ! Polar Stereographic map projection
   INTEGER, parameter :: lower = 0
   INTEGER, parameter :: upper = 1
   INTEGER :: icnt
   INTEGER :: ids, ide, jds, jde
   INTEGER :: i, j, n
   INTEGER :: ii, jj
   integer :: ncid
   integer :: cell
   integer :: map_proj
   integer :: ierr, astat
   integer :: dimid, varid
   integer :: nlon_megan, nlat_megan
   integer :: domain, domains
   integer :: mnth, mnth_s, mnth_e, megan_month
   integer :: start_lai_mnth = 1
   integer :: end_lai_mnth   = 12
   integer :: xndx_megan(2)
   integer :: yndx_megan(2)
   integer, allocatable :: ix(:,:,:)                        ! index used by interpolation
   integer, allocatable :: jy(:,:,:)                        ! index used by interpolation

   real    :: missing_value
   real    :: scale_factor
   real    :: wrk_sum
   real    :: ds1, ds2
   real    :: xl, xu
   real    :: yl, yu, dy
   real    :: wrf_lon_min
   real    :: wrf_lon_max
   real    :: wrf_lat_min
   real    :: wrf_lat_max
   real    :: cen_lon
   real    :: cen_lat
   real    :: stand_lon
   real    :: truelat1
   real    :: truelat2
   real    :: dx
   real(8), allocatable :: xedge_megan(:)
   real(8), allocatable :: yedge_megan(:)
   real, allocatable :: megan_lons(:)
   real, allocatable :: megan_lats(:)
   real, allocatable :: msebio_isop(:,:)
   real, allocatable :: msebio_myrc(:,:)
   real, allocatable :: msebio_sabi(:,:)
   real, allocatable :: msebio_limo(:,:)
   real, allocatable :: msebio_a_3car(:,:)
   real, allocatable :: msebio_ocim(:,:)
   real, allocatable :: msebio_bpin(:,:)
   real, allocatable :: msebio_apin(:,:)
   real, allocatable :: msebio_mbo(:,:)
   real, allocatable :: msebio_meoh(:,:)
   real, allocatable :: msebio_no(:,:)
   real, allocatable :: msebio_omtp(:,:)
   real, allocatable :: msebio_farn(:,:)
   real, allocatable :: msebio_bcar(:,:)
   real, allocatable :: msebio_osqt(:,:)
   real, allocatable :: msebio_acto(:,:)
   real, allocatable :: msebio_co(:,:)
   real, allocatable :: msebio_bider(:,:)
   real, allocatable :: msebio_stress(:,:)
   real, allocatable :: msebio_other(:,:)


   real, allocatable :: pftp_01(:,:)
   real, allocatable :: pftp_02(:,:)
   real, allocatable :: pftp_03(:,:)
   real, allocatable :: pftp_04(:,:)
   real, allocatable :: pftp_05(:,:)
   real, allocatable :: pftp_06(:,:)
   real, allocatable :: pftp_07(:,:)
   real, allocatable :: pftp_08(:,:)
   real, allocatable :: pftp_09(:,:)
   real, allocatable :: pftp_10(:,:)
   real, allocatable :: pftp_11(:,:)
   real, allocatable :: pftp_12(:,:)
   real, allocatable :: pftp_13(:,:)
   real, allocatable :: pftp_14(:,:)
   real, allocatable :: pftp_15(:,:)
   real, allocatable :: pftp_16(:,:)
 
  real, allocatable :: mlai(:,:,:)
   real, allocatable :: mtsa(:,:,:)
   real, allocatable :: mswdown(:,:,:)
   real, allocatable :: tmp3(:,:,:)
   real, allocatable :: ax(:,:,:)                        ! weight coef. all domain
   real, allocatable :: by(:,:,:)                        ! weight coef. all domain

   CHARACTER (LEN=132) :: varname
   CHARACTER (LEN=132) :: filespec
   CHARACTER (LEN=80)  :: message
   CHARACTER (LEN=80)  :: attribute
   CHARACTER (LEN=80)  :: units_attribute
   CHARACTER (LEN=80)  :: description_attribute
   CHARACTER (LEN=80)  :: stagger_attribute
   CHARACTER (LEN=80)  :: coor_attribute
   CHARACTER (LEN=80)  :: memord_attribute
   CHARACTER (LEN=80)  :: inpname
   CHARACTER (LEN=80)  :: outpname
   CHARACTER (LEN=80)  :: wrf_dir
   CHARACTER (LEN=80)  :: megan_dir
   CHARACTER (LEN=19)  :: Times(1)
   CHARACTER (LEN=4)   :: num  
   CHARACTER (LEN=3)   :: char_mnth(12)

   logical :: has_area_map
   logical :: new_grid

   namelist /control/ domains, start_lai_mnth, end_lai_mnth, &
                      wrf_dir, megan_dir

!---------------------------------------------------------------------
!	... include files
!---------------------------------------------------------------------
   include 'netcdf.inc'

   char_mnth(:) = (/ 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', &
                     'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' /)
!   wrf_dir   = '.'
!   megan_dir = '.'

!-----------------------------------------------------------------
!     read control variables
!-----------------------------------------------------------------
   read(*,nml=control,iostat=ierr)
   write(*,*) domains, start_lai_mnth, end_lai_mnth, wrf_dir, megan_dir
   if( ierr /= 0 ) then
     write(*,*) 'convert_emissions: failed to read namelist; error = ',ierr
     stop 'bio_emiss abort'
   endif
!-----------------------------------------------------------------
!     check namelist inputs
!-----------------------------------------------------------------
   if( domains < 1 ) then
     write(*,*) 'convert_emissions: domains must be >= 1'
     stop 'bio_emiss abort'
   endif
   if( start_lai_mnth < 1 .or. start_lai_mnth > 12 ) then
     write(*,*) 'convert_emissions: start month must be in set [1,12]'
     stop 'bio_emiss abort'
   endif
   if( end_lai_mnth < 1 .or. end_lai_mnth > 12 ) then
     write(*,*) 'convert_emissions: end month must be in set [1,12]'
     stop 'bio_emiss abort'
   endif
   if( end_lai_mnth < start_lai_mnth ) then
     write(*,*) 'convert_emissions: end month must >= start_month'
      stop 'bio_emiss abort'
   endif

!-----------------------------------------------------------------
!     loop over domains
!-----------------------------------------------------------------
domain_loop : &
   do domain = 1,domains

      write(*,*) ' '
      write(*,*) '========================================='
      write(*,*) 'Domain = ',domain
      write(*,*) '========================================='
      write(*,*) ' '

!-----------------------------------------------------------
!     ... read wrfinput file
!-----------------------------------------------------------
      call wrf_file

!-----------------------------------------------------------
!     ... read and interpolate megan datasets
!-----------------------------------------------------------

!------read MSWDOWN, and MTSA -------------------------------
      mnth_s        = 1
      mnth_e        = 12
      scale_factor  = 1.
      inpname       = 'DSW.nc'
      varname       = 'DSW_AVE'
      CALL  megan2_bioemiss
      inpname       = 'TAS.nc'
      varname       = 'TAS_AVE'
      missing_value = -32768.
      CALL  megan2_bioemiss

!-----read LAI----------------------------------------------
      mnth_e        = 1
      scale_factor  = 1.e-1
      missing_value = 255.
      do megan_month = 1, 46  !there are 46 8-day files 
        write(num,'(i4)') 1000+megan_month*8-7
         write(*,*)'num=',num
         inpname       = 'laina2008' // num(2:4) // '.nc'
         varname       = 'LAIv'
         CALL  megan2_bioemiss
      end do

!-----read emission factors----------------------------------
!-----Isoprene (ug/m2/hr)
     missing_value = -32768.
     scale_factor = 1.0 
     inpname       = 'isoall200021_30sec.nc'
     varname       = 'isoprene'
      CALL  megan2_bioemiss
!-----Myrcene (ug/m2/hr)
     scale_factor = 1.0
      inpname       = 'myrall200021_30sec.nc'
      varname       = 'myrcene'
      CALL  megan2_bioemiss
!-----Sabinene (ug/m2/hr)
     scale_factor = 1.0
      inpname       = 'saball200021_30sec.nc'
      varname       = 'sabinene'
      CALL  megan2_bioemiss
!-----Limonene (ug/m2/hr)
     scale_factor = 1.0
      inpname       = 'limall200021_30sec.nc'
      varname       = 'limonene'
      CALL  megan2_bioemiss
!-----Carene (ug/m2/hr)
     scale_factor = 1.0
      inpname       = 'careall200021_30sec.nc'
      varname       = 'carene'
      CALL  megan2_bioemiss
!-----Ocimene (ug/m2/hr)
     scale_factor = 1.0
      inpname       = 'ociall200021_30sec.nc'
      varname       = 'ocimene'
      CALL  megan2_bioemiss
!-----Beta_pinene (ug/m2/hr)
     scale_factor = 1.0
      inpname       = 'pinball200021_30sec.nc'
      varname       = 'beta_pinene'
      CALL  megan2_bioemiss
!-----Alpha_pinene (ug/m2/hr)
     scale_factor = 1.0
      inpname       = 'pinaall200021_30sec.nc'
      varname       = 'alpha_pinene'
      CALL  megan2_bioemiss
!-----Methylbutenol (ug/m2/hr)
     scale_factor = 1.0
      inpname       = 'mboall200021_30sec.nc'
      varname       = 'methylbutenol'
      CALL  megan2_bioemiss
!-----Methanol (ug/m2/hr)
     scale_factor = 1.0
      inpname       = 'meohall200021_30sec.nc'
      varname       = 'methanol'
      CALL  megan2_bioemiss
!-----NO (ug/m2/hr)
     scale_factor = 1.0
      inpname       = 'noall200021_30sec.nc'
      varname       = 'nitric_oxide'
      CALL  megan2_bioemiss


!-----read PFTs----------------------------------------------
      scale_factor  = 1.
      inpname       = 'pft01net08.nc'
      varname       = 'pft01net08'
      missing_value = 255.
      CALL  megan2_bioemiss
      inpname       = 'pft02ndt08.nc'
      varname       = 'pft02ndt08'
      CALL  megan2_bioemiss
      inpname       = 'pft05bet08.nc'
      varname       = 'pft05bet08'
      CALL  megan2_bioemiss
      inpname       = 'pft07bdt08.nc'
      varname       = 'pft07bdt08'
      CALL  megan2_bioemiss
      inpname       = 'pft10ds08.nc'
      varname       = 'pft10ds08'
      CALL  megan2_bioemiss
      inpname       = 'pft12agr08.nc'
      varname       = 'pft12agr08'
      CALL  megan2_bioemiss
      inpname       = 'pft13cgr08.nc'
      varname       = 'pft13cgr08'
      CALL  megan2_bioemiss
      inpname       = 'pft14wgr08.nc'
      varname       = 'pft14wgr08'
      CALL  megan2_bioemiss
      inpname       = 'pft15ocr08.nc'
      varname       = 'pft15ocr08'
      CALL  megan2_bioemiss
      inpname       = 'pft16crn08.nc'
      varname       = 'pft16crn08'
      CALL  megan2_bioemiss




!     endif

!-----------------------------------------------------------
!     ... write wrfbiochemi_d<nn> file
!-----------------------------------------------------------
      write(*,*) 'map_megan2_emissions: Before write_bioemiss'
      CALL write_ef210
      CALL write_lai210
      CALL write_pft210

      CALL  write_bioemiss
      write(*,*) 'map_megan2_emissions: After write_bioemiss'
      write(*,*) ' '
      write(*,'(''map_megan2_emissions: '',i3,'' cached grid(s)'')') grid_cnt

!-----------------------------------------------------------
!     cleanup domain variables
!-----------------------------------------------------------
      deallocate( mlai, msebio_isop, &
                        msebio_myrc, & 
                        msebio_sabi, &
                        msebio_limo, &
                        msebio_a_3car, &
                        msebio_ocim, &
                        msebio_bpin, &
                        msebio_apin, &
                        msebio_mbo, &
                        msebio_meoh, &
                        msebio_no, &
                        msebio_omtp,&
                        msebio_farn,&
                        msebio_bcar,&
                        msebio_osqt,&
                        msebio_acto,&
                        msebio_co,&
                        msebio_bider,&
                        msebio_stress,&
                        msebio_other,&
                  pftp_01, pftp_02, pftp_03, pftp_04, pftp_05, &
                  pftp_06, pftp_07, pftp_08, pftp_09, pftp_10, &
                  pftp_11, pftp_12, pftp_13, pftp_14, pftp_15, &
                  pftp_16, &
                  mtsa, mswdown )
      if( allocated( xlong ) ) then
        deallocate( xlong )
      endif
      if( allocated( xlat ) ) then
        deallocate( xlat )
      endif
      do n = 1,grid_cnt
        if( associated( grid_specs(n)%lon ) ) then
          deallocate( grid_specs(n)%lon )
        endif
        if( associated( grid_specs(n)%lat ) ) then
          deallocate( grid_specs(n)%lat )
        endif
        if( associated( grid_specs(n)%model_area_type ) ) then
          do j = 1,jde
            do i = 1,ide
              if( associated( grid_specs(n)%model_area_type(i,j)%dcell_lon_ndx ) ) then
                deallocate( grid_specs(n)%model_area_type(i,j)%dcell_lon_ndx )
              endif
              if( associated( grid_specs(n)%model_area_type(i,j)%dcell_lat_ndx ) ) then
                deallocate( grid_specs(n)%model_area_type(i,j)%dcell_lat_ndx )
              endif
              if( associated( grid_specs(n)%model_area_type(i,j)%wght ) ) then
                deallocate( grid_specs(n)%model_area_type(i,j)%wght )
              endif
            end do
          end do
          deallocate( grid_specs(n)%model_area_type )
        endif
      end do
!++sw
      grid_cnt = 0
!--sw
   end do domain_loop

   CONTAINS

   subroutine wrf_file
!---------------------------------------------------------------------
!   read wrf file
!---------------------------------------------------------------------

   use area_mapper, only : proj_init
print *, '1'
   write(num,'(i3)') 100+domain
   inpname = 'wrfinput_d' // num(2:3)
   filespec = trim( wrf_dir ) // '/' // trim( inpname )
print *, '2'
!---------------------------------------------------------------------
!   open wrf input file
!---------------------------------------------------------------------
   message = 'wrf_file: Failed to open ' // trim(inpname)
   call handle_ncerr( nf_open( trim(filespec), nf_noclobber, ncid ), message )       
print *, '3'
!---------------------------------------------------------------------
!   get wrf dimesions
!---------------------------------------------------------------------
   message = 'Failed to get lon dimension id'
   call handle_ncerr( nf_inq_dimid( ncid, 'west_east', dimid ), message )
   message = 'Failed to get lon dimension'
   call handle_ncerr( nf_inq_dimlen( ncid, dimid, ide ), message )
   message = 'Failed to get lat dimension id'
   call handle_ncerr( nf_inq_dimid( ncid, 'south_north', dimid ), message )
   message = 'Failed to get lat dimension'
   call handle_ncerr( nf_inq_dimlen( ncid, dimid, jde ), message )
!---------------------------------------------------------------------
!   get wrf map projection variables
!---------------------------------------------------------------------
   message = 'Failed to get map_proj'
   call handle_ncerr( nf_get_att_int( ncid, nf_global, 'MAP_PROJ', map_proj ), message )
   if( map_proj /= PS ) then
      write(*,*) 'wrf_file: MAP_PROJ is not polar stereographic'
   else
      write(*,*) 'wrf_file: MAP_PROJ is polar stereographic'
   endif
   message = 'wrf_file: Failed to get cen_lon'
   call handle_ncerr( nf_get_att_real( ncid, nf_global, 'CEN_LON', cen_lon ), message )
   write(*,*) 'wrf_file: CEN_LON = ',cen_lon
   message = 'wrf_file: Failed to get cen_lat'
   call handle_ncerr( nf_get_att_real( ncid, nf_global, 'CEN_LAT', cen_lat ), message )
   write(*,*) 'wrf_file: CEN_LAT = ',cen_lat
   message = 'wrf_file: Failed to get stand_lon'
   call handle_ncerr( nf_get_att_real( ncid, nf_global, 'STAND_LON', stand_lon ), message )
   write(*,*) 'wrf_file: STAND_LON = ',stand_lon
   message = 'wrf_file: Failed to get truelat1'
   call handle_ncerr( nf_get_att_real( ncid, nf_global, 'TRUELAT1', truelat1 ), message )
   write(*,*) 'wrf_file: TRUELAT1 = ',truelat1
   message = 'wrf_file: Failed to get truelat2'
   call handle_ncerr( nf_get_att_real( ncid, nf_global, 'TRUELAT2', truelat2 ), message )
   write(*,*) 'wrf_file: TRUELAT2 = ',truelat2
   message = 'wrf_file: Failed to get dx'
   call handle_ncerr( nf_get_att_real( ncid, nf_global, 'DX', dx ), message )
   write(*,*) 'wrf_file: DX = ',dx

!---------------------------------------------------------------------
!   initialize map projection
!---------------------------------------------------------------------
   call proj_init( map_proj, cen_lon, cen_lat, truelat1, truelat2, &
                   stand_lon, dx, ide, jde )

   ids = 1
   jds = 1

   message = 'Failed to get Times id'
   call handle_ncerr( nf_inq_varid( ncid, 'Times', varid ), message )
   message = 'Failed to read Times'
   call handle_ncerr( nf_get_var_text( ncid, varid, Times ), message )

   write(*,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
   write(*,*) 'wrf_file: time = ',trim(Times(1))
   write(*,*) 'wrf_file: grid dimensions'
   write(*,*) 'wrf_file: ids,ide,jds,jde'
   write(*,'(4i6)') ids,ide,jds,jde
   write(*,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
!---------------------------------------------------------------------
!   close wrf file
!---------------------------------------------------------------------
   message = 'wrf_file: Failed to close ' // trim(inpname)
   call handle_ncerr( nf_close( ncid ), message )       
!---------------------------------------------------------------------
!   allocate final bioemission variables
!---------------------------------------------------------------------
   allocate( mlai(ide,jde,46),stat=astat ) 
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate mlai; error = ',astat
     stop 'allocate failed'
   endif
   allocate( msebio_isop(ide,jde),stat=astat ) 
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate msebio_isop; error = ',astat
     stop 'allocate failed'
   endif
   allocate( msebio_myrc(ide,jde),stat=astat )
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate msebio_myrc; error = ',astat
     stop 'allocate failed'
   endif
   allocate( msebio_sabi(ide,jde),stat=astat )
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate msebio_sabi; error = ',astat
     stop 'allocate failed'
   endif
   allocate( msebio_limo(ide,jde),stat=astat )
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate msebio_limo; error = ',astat
     stop 'allocate failed'
   endif
   allocate( msebio_a_3car(ide,jde),stat=astat )
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate msebio_a_3car; error = ',astat
     stop 'allocate failed'
   endif
   allocate( msebio_ocim(ide,jde),stat=astat )
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate msebio_ocim; error = ',astat
     stop 'allocate failed'
   endif
   allocate( msebio_bpin(ide,jde),stat=astat )
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate msebio_bpin; error = ',astat
     stop 'allocate failed'
   endif
   allocate( msebio_apin(ide,jde),stat=astat )
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate msebio_apin; error = ',astat
     stop 'allocate failed'
   endif
   allocate( msebio_mbo(ide,jde),stat=astat )
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate msebio_mbo; error = ',astat
     stop 'allocate failed'
   endif
   allocate( msebio_meoh(ide,jde),stat=astat )
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate msebio_meoh; error = ',astat
     stop 'allocate failed'
   endif
   allocate( msebio_no(ide,jde),stat=astat )
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate msebio_no; error = ',astat
     stop 'allocate failed'
   endif
   allocate( msebio_omtp(ide,jde),stat=astat )
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate msebio_omtp; error = ',astat
     stop 'allocate failed'
   endif
   allocate( msebio_farn(ide,jde),stat=astat )
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate msebio_farn; error = ',astat
     stop 'allocate failed'
   endif
   allocate( msebio_bcar(ide,jde),stat=astat )
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate msebio_bcar; error = ',astat
     stop 'allocate failed'
   endif
   allocate( msebio_osqt(ide,jde),stat=astat )
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate msebio_osqt; error = ',astat
     stop 'allocate failed'
   endif
   allocate( msebio_acto(ide,jde),stat=astat )
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate msebio_acto; error = ',astat
     stop 'allocate failed'
   endif
   allocate( msebio_co(ide,jde),stat=astat )
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate msebio_co; error = ',astat
     stop 'allocate failed'
   endif
   allocate( msebio_bider(ide,jde),stat=astat )
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate msebio_bider; error = ',astat
     stop 'allocate failed'
   endif
   allocate( msebio_stress(ide,jde),stat=astat )
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate msebio_stress; error = ',astat
     stop 'allocate failed'
   endif
   allocate( msebio_other(ide,jde),stat=astat )
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate msebio_other; error = ',astat
     stop 'allocate failed'
   endif


   allocate( pftp_01(ide,jde),stat=astat ) 
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate pftp_01; error = ',astat
     stop 'allocate failed'
   endif
   allocate( pftp_02(ide,jde),stat=astat ) 
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate pftp_02; error = ',astat
     stop 'allocate failed'
   endif
   allocate( pftp_03(ide,jde),stat=astat ) 
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate pftp_03; error = ',astat
     stop 'allocate failed'
   endif
   allocate( pftp_04(ide,jde),stat=astat ) 
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate pftp_04; error = ',astat
     stop 'allocate failed'
   endif
   allocate( pftp_05(ide,jde),stat=astat )
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate pftp_05; error = ',astat
     stop 'allocate failed'
   endif
   allocate( pftp_06(ide,jde),stat=astat )
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate pftp_06; error = ',astat
     stop 'allocate failed'
   endif
   allocate( pftp_07(ide,jde),stat=astat )
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate pftp_07; error = ',astat
     stop 'allocate failed'
   endif
   allocate( pftp_08(ide,jde),stat=astat )
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate pftp_08; error = ',astat
     stop 'allocate failed'
   endif
   allocate( pftp_09(ide,jde),stat=astat )
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate pftp_09; error = ',astat
     stop 'allocate failed'
   endif
   allocate( pftp_10(ide,jde),stat=astat )
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate pftp_10; error = ',astat
     stop 'allocate failed'
   endif
   allocate( pftp_11(ide,jde),stat=astat )
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate pftp_11; error = ',astat
     stop 'allocate failed'
   endif
   allocate( pftp_12(ide,jde),stat=astat )
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate pftp_12; error = ',astat
     stop 'allocate failed'
   endif
   allocate( pftp_13(ide,jde),stat=astat )
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate pftp_13; error = ',astat
     stop 'allocate failed'
   endif
   allocate( pftp_14(ide,jde),stat=astat )
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate pftp_14; error = ',astat
     stop 'allocate failed'
   endif
   allocate( pftp_15(ide,jde),stat=astat )
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate pftp_15; error = ',astat
     stop 'allocate failed'
   endif
  allocate( pftp_16(ide,jde),stat=astat )
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate pftp_16; error = ',astat
     stop 'allocate failed'
   endif



   allocate( mtsa(ide,jde,12),stat=astat ) 
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate mtsa; error = ',astat
     stop 'allocate failed'
   endif
   allocate( mswdown(ide,jde,12),stat=astat ) 
   if( astat /= 0 ) then
     write(*,*) 'wrf_file: failed to allocate mswdown; error = ',astat
     stop 'allocate failed'
   endif

   end subroutine wrf_file

   subroutine megan2_bioemiss
!---------------------------------------------------------------------
!   map megan dataset to wrf grid
!---------------------------------------------------------------------

   use area_mapper, only : area_interp
   use constants_module, only : rad_per_deg, earth_radius_m
   use bio_types

!---------------------------------------------------------------------
!   local variables
!---------------------------------------------------------------------
    integer :: il, iu, jl, ju
    integer :: n
    integer :: status
    real    :: wrf_lon, wrf_lat
    real    :: data_dx
    real, allocatable :: wrk_data(:,:)
    logical :: debug = .false.

    write(*,*) ' '
    write(*,*) 'Reading megan2 bio emiss file ' // trim(inpname)
!---------------------------------------------------------------------
!   open megan dataset file
!---------------------------------------------------------------------
    message = 'megan2_bioemiss: Failed to open ' // trim(inpname)
    filespec = trim( megan_dir ) // '/' // trim(inpname)
    call handle_ncerr( nf_open( trim(filespec), nf_noclobber, ncid ), message )       
!---------------------------------------------------------------------
!   get megan dataset dimesions
!---------------------------------------------------------------------
    message = 'megan2_bioemiss: Failed to get lon dimension id'
    call handle_ncerr( nf_inq_dimid( ncid, 'lon', dimid ), message )
    message = 'megan2_bioemiss: Failed to get lon dimension'
    call handle_ncerr( nf_inq_dimlen( ncid, dimid, nlon_megan ), message )
    message = 'megan2_bioemiss: Failed to get lat dimension id'
    call handle_ncerr( nf_inq_dimid( ncid, 'lat', dimid ), message )
    message = 'megan2_bioemiss: Failed to get lat dimension'
    call handle_ncerr( nf_inq_dimlen( ncid, dimid, nlat_megan ), message )
    write(*,*) 'megan2_bioemiss:  nlon_megan, nlat_megan = ',nlon_megan,nlat_megan

!---------------------------------------------------------------------
!   allocate working variable
!---------------------------------------------------------------------
    if( allocated( megan_lons ) ) then
      deallocate( megan_lons )
    endif
    allocate( megan_lons(nlon_megan),stat=astat )
    if( astat /= 0 ) then
      write(*,*) 'megan2_bioemiss: Failed to allocate megan_lons; error = ',astat
      stop 'allocate failed'
    endif
!---------------------------------------------------------------------
!   read megan longitude variable
!---------------------------------------------------------------------
    message = 'megan2_bioemiss: Failed to get lon variable id'
    call handle_ncerr( nf_inq_varid( ncid, 'lon', varid ), message )
    message = 'megan2_bioemiss: Failed to read lon variable'
    call handle_ncerr( nf_get_var_real( ncid, varid, megan_lons ), message )

    if( allocated( megan_lats ) ) then
      deallocate( megan_lats )
    endif
    allocate( megan_lats(nlat_megan),stat=ierr )
    if( ierr /= 0 ) then
      write(*,*) 'megan2_bioemiss: Failed to allocate megan_lats; error = ',ierr
      stop 'allocate failed'
    endif
!---------------------------------------------------------------------
!   read megan latitude variable
!---------------------------------------------------------------------
    message = 'megan2_bioemiss: Failed to get lat variable id'
    call handle_ncerr( nf_inq_varid( ncid, 'lat', varid ), message )
    message = 'megan2_bioemiss: Failed to read lat variable'
    call handle_ncerr( nf_get_var_real( ncid, varid, megan_lats ), message )

!---------------------------------------------------------------------
!   determine interpolation type; bilinear or area conserving
!---------------------------------------------------------------------
    data_dx = earth_radius_m * (megan_lats(2) - megan_lats(1)) * rad_per_deg
    has_area_map = data_dx < dx
    write(*,*) 'megan2_bioemiss: data_dx,dx,has_area_map = ',data_dx,dx,has_area_map

!-------------------------------------------------------------
!   check for match against prior datasets
!-------------------------------------------------------------
   if( grid_cnt >= grid_max ) then
     write(*,*) 'megan2_bioemiss: reached grid cache max: ',grid_max
     stop
   endif
   grid_ndx = 0
   new_grid = .true.
   do n = 1,grid_cnt
     if( grid_specs(n)%nlons /= nlon_megan .or. grid_specs(n)%nlats /= nlat_megan ) then
       cycle
     endif
     if( any( grid_specs(n)%lon(:) /= megan_lons(:) ) ) then
       cycle
     endif
     if( any( grid_specs(n)%lat(:) /= megan_lats(:) ) ) then
       cycle
     endif
     grid_ndx = n
     new_grid = .false.
     exit
   end do
!-------------------------------------------------------------
!   new data grid to cache
!-------------------------------------------------------------
   if( new_grid ) then
     grid_cnt = grid_cnt + 1
     grid_specs(grid_cnt)%nlons = nlon_megan
     grid_specs(grid_cnt)%nlats = nlat_megan
     grid_specs(grid_cnt)%has_area_map = has_area_map
     allocate( grid_specs(grid_cnt)%lon(nlon_megan),stat=ierr )
     if( ierr /= 0 ) then
       write(*,*) 'megan2_bioemiss: Failed to allocate megan_lats; error = ',ierr
       stop 'allocate failed'
     endif
     allocate( grid_specs(grid_cnt)%lat(nlat_megan),stat=ierr )
     if( ierr /= 0 ) then
       write(*,*) 'megan2_bioemiss: Failed to allocate megan_lats; error = ',ierr
       stop 'allocate failed'
     endif
     grid_specs(grid_cnt)%lon(:) = megan_lons(:)
     grid_specs(grid_cnt)%lat(:) = megan_lats(:)
     if( has_area_map ) then
       allocate( grid_specs(grid_cnt)%model_area_type(ide,jde),stat=astat )
       if( astat /= 0 ) then
         write(*,*) 'proj_init; failed to allocate model_area_type: error = ',astat
         stop
       endif
       grid_specs(grid_cnt)%model_area_type(:,:)%has_data = .false.
       grid_specs(grid_cnt)%model_area_type(:,:)%active_dcell_cnt = 0
       grid_specs(grid_cnt)%model_area_type(:,:)%total_dcell_cnt  = 0
       grid_specs(grid_cnt)%model_area_type(:,:)%interior_dcell_cnt = 0
       grid_specs(grid_cnt)%model_area_type(:,:)%partial_dcell_cnt  = 0
     endif
     grid_ndx = grid_cnt
     write(*,*) 'megan2_bioemiss: file ' // trim(inpname),' has a new grid'
   endif

is_area_map : &
    if( has_area_map ) then
!---------------------------------------------------------------------
!   form megan longitude edges
!---------------------------------------------------------------------
      allocate( xedge_megan(nlon_megan+1),stat=astat )
      if( astat /= 0 ) then
        write(*,*) 'megan2_bioemiss: Failed to allocate xedge_megan; error = ',astat
        stop 'allocate error'
      endif
      xedge_megan(2:nlon_megan) = .5_8*(megan_lons(1:nlon_megan-1) + megan_lons(2:nlon_megan))
      xedge_megan(1)            = megan_lons(1) - .5_8*(megan_lons(2) - megan_lons(1))
      xedge_megan(nlon_megan+1) = megan_lons(nlon_megan) + .5_8*(megan_lons(nlon_megan) - megan_lons(nlon_megan-1))
      write(*,'(''megan2_bioemiss: xcen_megan(1,2)  = '',1p,2g22.15)') megan_lons(1:2)
      write(*,'(''megan2_bioemiss: xedge_megan(1,2) = '',1p,2g22.15)') xedge_megan(1:2)
      write(*,'(''megan2_bioemiss: dx = '',1pg22.15)') int( 1./(megan_lons(2) - megan_lons(1)) )
!---------------------------------------------------------------------
!   form megan latitude edges
!---------------------------------------------------------------------
      allocate( yedge_megan(nlat_megan+1),stat=ierr )
      if( ierr /= 0 ) then
        write(*,*) 'megan2_bioemiss: Failed to allocate yedge_megan; error = ',ierr
        stop 'allocate error'
      endif

      yedge_megan(2:nlat_megan) = .5_8*(megan_lats(1:nlat_megan-1) + megan_lats(2:nlat_megan))
      yedge_megan(1)            = megan_lats(1) - .5_8*(megan_lats(2) - megan_lats(1))
      yedge_megan(nlat_megan+1) = megan_lats(nlat_megan) + .5_8*(megan_lats(nlat_megan) - megan_lats(nlat_megan-1))

      write(*,'(''megan2_bioemiss: nlon_megan,nlat_megan = '',i6,1x,i6)') nlon_megan,nlat_megan
      write(*,'(''megan2_bioemiss: ycen_megan  = '',1p,2g22.15)') megan_lats(nlat_megan-1:nlat_megan)
      write(*,'(''megan2_bioemiss: yedge_megan = '',1p,2g22.15)') yedge_megan(nlat_megan:nlat_megan+1)

      if( allocated( wrk_data ) ) then
        deallocate( wrk_data )
      endif
      allocate( wrk_data(ide,jde),stat=status )
      if( status /= 0 ) then
        write(*,*) 'megan2_bioemiss: allocate for wrk_data failed; error = ',ierr
        stop 'allocation error'
      endif
!---------------------------------------------------------------------
!   area conserving interpolation
!---------------------------------------------------------------------
      call area_interp( xedge_megan, yedge_megan, nlon_megan, nlat_megan, int(missing_value,2), &
                        wrk_data, ncid, varname, grid_ndx, new_grid )
      if( varname(:3) == 'LAI' ) then
      write(*,*)'megan_month=',megan_month
        mlai(:,:,megan_month) = scale_factor * wrk_data(:,:)
      elseif( trim(varname) == 'isoprene' ) then
        msebio_isop(:,:) = scale_factor * wrk_data(:,:)
      elseif( trim(varname) == 'myrcene' ) then
        msebio_myrc(:,:) = scale_factor * wrk_data(:,:)
      elseif( trim(varname) == 'sabinene' ) then
        msebio_sabi(:,:) = scale_factor * wrk_data(:,:)
     elseif( trim(varname) == 'limonene' ) then
        msebio_limo(:,:) = scale_factor * wrk_data(:,:)
      elseif( trim(varname) == 'carene' ) then
        msebio_a_3car(:,:) = scale_factor * wrk_data(:,:)
      elseif( trim(varname) == 'ocimene' ) then
        msebio_ocim(:,:) = scale_factor * wrk_data(:,:)
     elseif( trim(varname) == 'beta_pinene' ) then
        msebio_bpin(:,:) = scale_factor * wrk_data(:,:)
      elseif( trim(varname) == 'alpha_pinene' ) then
        msebio_apin(:,:) = scale_factor * wrk_data(:,:)
      elseif( trim(varname) == 'methylbutenol' ) then
        msebio_mbo(:,:) = scale_factor * wrk_data(:,:)
     elseif( trim(varname) == 'methanol' ) then
        msebio_meoh(:,:) = scale_factor * wrk_data(:,:)
      elseif( trim(varname) == 'nitric_oxide' ) then
        msebio_no(:,:) = scale_factor * wrk_data(:,:)

      elseif( varname(:5) == 'pft01' ) then
        pftp_01(:,:) = scale_factor * wrk_data(:,:)
     elseif( varname(:5) == 'pft02' ) then
        pftp_02(:,:) = scale_factor * wrk_data(:,:)
     elseif( varname(:5) == 'pft03' ) then
        pftp_03(:,:) = scale_factor * wrk_data(:,:)
     elseif( varname(:5) == 'pft04' ) then
        pftp_04(:,:) = scale_factor * wrk_data(:,:)
     elseif( varname(:5) == 'pft05' ) then
        pftp_05(:,:) = scale_factor * wrk_data(:,:)
     elseif( varname(:5) == 'pft06' ) then
        pftp_06(:,:) = scale_factor * wrk_data(:,:)
     elseif( varname(:5) == 'pft07' ) then
        pftp_07(:,:) = scale_factor * wrk_data(:,:)
     elseif( varname(:5) == 'pft08' ) then
        pftp_08(:,:) = scale_factor * wrk_data(:,:)
     elseif( varname(:5) == 'pft09' ) then
        pftp_09(:,:) = scale_factor * wrk_data(:,:)
     elseif( varname(:5) == 'pft10' ) then
        pftp_10(:,:) = scale_factor * wrk_data(:,:)
     elseif( varname(:5) == 'pft11' ) then
        pftp_11(:,:) = scale_factor * wrk_data(:,:)
     elseif( varname(:5) == 'pft12' ) then
        pftp_12(:,:) = scale_factor * wrk_data(:,:)
     elseif( varname(:5) == 'pft13' ) then
        pftp_13(:,:) = scale_factor * wrk_data(:,:)
     elseif( varname(:5) == 'pft14' ) then
        pftp_14(:,:) = scale_factor * wrk_data(:,:)
     elseif( varname(:5) == 'pft15' ) then
        pftp_15(:,:) = scale_factor * wrk_data(:,:)
     elseif( varname(:5) == 'pft16' ) then
        pftp_16(:,:) = scale_factor * wrk_data(:,:)

      endif

        pftp_03(:,:) = 0.0
        pftp_04(:,:) = 0.0
        pftp_06(:,:) = 0.0
        pftp_08(:,:) = 0.0
        pftp_09(:,:) = 0.0
        pftp_11(:,:) = 0.0
        msebio_omtp(:,:) = 1.0
        msebio_farn(:,:) = 1.0
        msebio_bcar(:,:) = 1.0
        msebio_osqt(:,:) = 1.0
        msebio_acto(:,:) = 1.0
        msebio_co(:,:) = 1.0
        msebio_bider(:,:) = 1.0
        msebio_stress(:,:) = 1.0
        msebio_other(:,:) = 1.0

      deallocate( wrk_data )
    else is_area_map
!---------------------------------------------------------------------
!   form megan coordinate limits
!---------------------------------------------------------------------
      wrf_lon_min = minval( xlong(ids:ide,jds:jde) )
      wrf_lon_max = maxval( xlong(ids:ide,jds:jde) )
      wrf_lat_min = minval( xlat(ids:ide,jds:jde) )
      wrf_lat_max = maxval( xlat(ids:ide,jds:jde) )
      write(*,*) ' '
      write(*,'('' megan2_bioemiss: model lon limits = '',1p,2g25.16)') wrf_lon_min,wrf_lon_max
      write(*,'('' megan2_bioemiss: model lat limits = '',1p,2g25.16)') wrf_lat_min,wrf_lat_max
      write(*,*) ' '
      write(*,'('' megan2_bioemiss: megan lon limits = '',1p,2g25.16)') megan_lons(1),megan_lons(nlon_megan)
      write(*,'('' megan2_bioemiss: megan lat limits = '',1p,2g25.16)') megan_lats(1),megan_lats(nlat_megan)
      write(*,*) ' '
!---------------------------------------------------------------
!     allocate memory space to store interpolation coef.
!---------------------------------------------------------------
      ierr = 0
      allocate( ax(ids:ide,jds:jde,0:1), &
                by(ids:ide,jds:jde,0:1), stat=status )
      ierr = ierr + status
      allocate( ix(ids:ide,jds:jde,0:1), &
                jy(ids:ide,jds:jde,0:1), stat=status )
      ierr = ierr + status
      if( ierr /= 0 ) then
        write(*,*) 'allocate for ax ... jy failed; error = ',ierr
      endif
!---------------------------------------------------------------------
!   set bilinear interp variables
!---------------------------------------------------------------------
lat_loop : &
      do j = jds,jde
        do i = ids,ide
!---------------------------------------------------------------------
!   longitudes
!---------------------------------------------------------------------
          wrf_lon = xlong(i,j)
          if( wrf_lon >= megan_lons(nlon_megan) .or. &
              wrf_lon < megan_lons(1) ) then
            ix(i,j,lower) = nlon_megan
          else
            do n = 2,nlon_megan
              if( wrf_lon < megan_lons(n) ) then
                ix(i,j,lower) = min( nlon_megan-1, max(n-1,1) )
                exit
              endif
            end do
          endif
          ix(i,j,upper) = mod( ix(i,j,lower),nlon_megan ) + 1
          data_dx = megan_lons(ix(i,j,upper)) - megan_lons(ix(i,j,lower))
          if( data_dx < 0. ) then
            data_dx = 360. + data_dx
          endif
          ds1 = wrf_lon - megan_lons(ix(i,j,lower))
          if( ds1 < 0. ) then
            ds1 = 360. + ds1
          endif
          ax(i,j,lower) = ds1/data_dx
          ax(i,j,upper) = 1.0 - ax(i,j,lower)
!---------------------------------------------------------------------
!   latitudes
!---------------------------------------------------------------------
          wrf_lat = xlat(i,j)
          if( wrf_lat < megan_lats(1) ) then
            jy(i,j,0:1) = -1
            by(i,j,0:1) = 0.
          elseif( wrf_lat > megan_lats(nlat_megan) ) then
            jy(i,j,0:1) = -2
            by(i,j,0:1) = 0.
          else
            do n = 1,nlat_megan
              if( wrf_lat < megan_lats(n) ) then
                exit
              endif
            end do
            jy(i,j,lower) = min( nlat_megan-1, max(n-1,1) )
            jy(i,j,upper) = jy(i,j,lower) + 1
            by(i,j,lower) = (wrf_lat - megan_lats(jy(i,j,lower))) &
                             /(megan_lats(jy(i,j,upper)) - megan_lats(jy(i,j,lower)))
            by(i,j,upper) = 1.0 - by(i,j,lower)
          endif
        end do
      end do lat_loop
!---------------------------------------------------------------------
!   form dataset index limits
!---------------------------------------------------------------------
      write(*,*) 'megan2_bioemiss: count of points <,> data min,max lat = ',count(ix(:,:,0) == nlon_megan )
      xndx_megan(1) = minval( ix(:,:,lower) )
      xndx_megan(2) = maxval( ix(:,:,upper) )
      write(*,*) 'xndx_megan = ',xndx_megan(:)
      write(*,*) 'megan2_bioemiss: count of points < data min lat = ',count(jy(:,:,0) == -1)
      write(*,*) 'megan2_bioemiss: count of points > data max lat = ',count(jy(:,:,0) == -2)
      yndx_megan(1) = minval( jy(:,:,lower),mask=jy(:,:,lower)>0 )
      yndx_megan(2) = maxval( jy(:,:,upper),mask=jy(:,:,upper)>0 )
      write(*,*) 'yndx_megan = ',yndx_megan(:)

      if( debug ) then
      write(*,*) ' '
      write(*,*) 'megan2_bioemiss: bilinear interp diagnostics'
      write(*,*) 'megan2_bioemiss: ix'
      write(*,*) ix(ids,jds,:)
      write(*,*) 'megan2_bioemiss: ax'
      write(*,*) ax(ids,jds,:)
      write(*,*) 'megan2_bioemiss: megan lons'
      write(*,*) megan_lons(ix(ids,jds,0)),megan_lons(ix(ids,jds,1))
      write(*,*) 'megan2_bioemiss: wrf lon = ',xlong(ids,jds)
      write(*,*) 'megan2_bioemiss: jy'
      write(*,*) jy(ids,jds,:)
      write(*,*) 'megan2_bioemiss: by'
      write(*,*) by(ids,jds,:)
      write(*,*) 'megan2_bioemiss: megan lats'
      write(*,*) megan_lats(jy(ids,jds,0)),megan_lats(jy(ids,jds,1))
      write(*,*) 'megan2_bioemiss: wrf lat = ',xlat(ids,jds)
      write(*,*) ' '
      do j = jds,jde
        do i = ids,ide
          if( ix(i,j,lower) == nlon_megan ) then
      write(*,*) 'megan2_bioemiss: bilinear interp diagnostics'
      write(*,*) 'megan2_bioemiss: ix'
      write(*,*) ix(i,j,:)
      write(*,*) 'megan2_bioemiss: ax'
      write(*,*) ax(i,j,:)
      write(*,*) 'megan2_bioemiss: megan lons'
      write(*,*) megan_lons(ix(i,j,0)),megan_lons(ix(i,j,1))
      write(*,*) 'megan2_bioemiss: wrf lon = ',xlong(i,j)
      write(*,*) 'megan2_bioemiss: jy'
      write(*,*) jy(i,j,:)
      write(*,*) 'megan2_bioemiss: by'
      write(*,*) by(i,j,:)
      write(*,*) 'megan2_bioemiss: megan lats'
      write(*,*) megan_lats(jy(i,j,0)),megan_lats(jy(i,j,1))
      write(*,*) 'megan2_bioemiss: wrf lat = ',xlat(i,j)
            stop 'diagnostics'
          endif
        end do
      end do
      stop 'diagnostics'
      endif

!---------------------------------------------------------------------
!   allocate and read dataset variable
!---------------------------------------------------------------------
       if( allocated( tmp3 ) ) then
          deallocate( tmp3 )
       endif
       allocate( tmp3(xndx_megan(1):xndx_megan(2),yndx_megan(1):yndx_megan(2),mnth_s:mnth_e),stat=ierr )
                 
       if( ierr /= 0 ) then
         write(message,*) 'Failed to allocate tmp3 for lai; error = ',ierr
         stop 'bio_emiss abort'
       endif
       message = 'Failed to get variable id'
       call handle_ncerr( nf_inq_varid( ncid, trim(varname), varid ), message )
       message = 'Failed to read variable'
       if( mnth_s == mnth_e ) then
          call handle_ncerr( nf_get_vara_real( ncid, varid, &
                                               (/ xndx_megan(1),yndx_megan(1) /), &
                                               (/ xndx_megan(2)-xndx_megan(1)+1,yndx_megan(2)-yndx_megan(1)+1 /), &
                                               tmp3 ), message )
       else
          call handle_ncerr( nf_get_vara_real( ncid, varid, &
                                               (/ xndx_megan(1),yndx_megan(1),mnth_s /), &
                                               (/ xndx_megan(2)-xndx_megan(1)+1, &
                                                  yndx_megan(2)-yndx_megan(1)+1,mnth_e-mnth_s+1 /), &
                                               tmp3 ), message )
       endif

       write(*,*)  'dataset size = ',size(tmp3)
       write(*,*)  'dataset min,max values = ',minval(tmp3(:,:,:)),maxval(tmp3(:,:,:))
       write(*,*)  'dataset missing value count = ',count(tmp3(:,:,:) == missing_value )
       write(*,*)  '% valid data = ',100.* real( count(tmp3(:,:,:) /= missing_value ) ) /real(size(tmp3))

!---------------------------------------------------------------------
!   replace missing values with zero
!---------------------------------------------------------------------
       where( tmp3(:,:,:) == missing_value )
          tmp3(:,:,:) = 0.
       endwhere
!---------------------------------------------------------------------
!   set wrf bioemission variable
!---------------------------------------------------------------------
mnth_loop : &
       do mnth = mnth_s,mnth_e
         do j = jds,jde
           do i = ids,ide
             jl = jy(i,j,0)
             if( jl > 0 ) then
               il = ix(i,j,0)
               iu = ix(i,j,1)
               ju = jy(i,j,1)
               wrk_sum  = tmp3(il,jl,mnth)*ax(i,j,upper)*by(i,j,upper) &
                        + tmp3(il,ju,mnth)*ax(i,j,upper)*by(i,j,lower) &
                        + tmp3(iu,jl,mnth)*ax(i,j,lower)*by(i,j,upper) &
                        + tmp3(iu,ju,mnth)*ax(i,j,lower)*by(i,j,lower)
             else
               wrk_sum  = 0.
             endif
             if( varname(:3) == 'LAI' ) then
               mlai(i,j,megan_month) = scale_factor * wrk_sum
             elseif( trim(varname) == 'isoprene' ) then
               msebio_isop(i,j) = scale_factor * wrk_sum
             elseif( trim(varname) == 'myrcene' ) then
               msebio_myrc(i,j) = scale_factor * wrk_sum
             elseif( trim(varname) == 'sabinene' ) then
               msebio_sabi(i,j) = scale_factor * wrk_sum
             elseif( trim(varname) == 'limonene' ) then
               msebio_limo(i,j) = scale_factor * wrk_sum
             elseif( trim(varname) == 'carene' ) then
               msebio_a_3car(i,j) = scale_factor * wrk_sum
             elseif( trim(varname) == 'ocimene' ) then
               msebio_ocim(i,j) = scale_factor * wrk_sum
             elseif( trim(varname) == 'beta_pinene' ) then
               msebio_bpin(i,j) = scale_factor * wrk_sum
             elseif( trim(varname) == 'alpha_pinene' ) then
               msebio_apin(i,j) = scale_factor * wrk_sum
             elseif( trim(varname) == 'methylbutenol' ) then
               msebio_mbo(i,j) = scale_factor * wrk_sum
             elseif( trim(varname) == 'methanol' ) then
               msebio_meoh(i,j) = scale_factor * wrk_sum
             elseif( trim(varname) == 'nitric_oxide' ) then
               msebio_no(i,j) = scale_factor * wrk_sum

             elseif( varname(:5) == 'pft01' ) then
               pftp_01(i,j) = scale_factor * wrk_sum
             elseif( varname(:5) == 'pft02' ) then
               pftp_02(i,j) = scale_factor * wrk_sum
             elseif( varname(:5) == 'pft03' ) then
               pftp_03(i,j) = scale_factor * wrk_sum
             elseif( varname(:5) == 'pft04' ) then
               pftp_04(i,j) = scale_factor * wrk_sum
             elseif( varname(:5) == 'pft05' ) then
               pftp_05(i,j) = scale_factor * wrk_sum
            elseif( varname(:5) == 'pft06' ) then
               pftp_06(i,j) = scale_factor * wrk_sum
             elseif( varname(:5) == 'pft07' ) then
               pftp_07(i,j) = scale_factor * wrk_sum
             elseif( varname(:5) == 'pft08' ) then
               pftp_08(i,j) = scale_factor * wrk_sum
             elseif( varname(:5) == 'pft09' ) then
               pftp_09(i,j) = scale_factor * wrk_sum
             elseif( varname(:5) == 'pft10' ) then
               pftp_10(i,j) = scale_factor * wrk_sum
            elseif( varname(:5) == 'pft11' ) then
               pftp_11(i,j) = scale_factor * wrk_sum
             elseif( varname(:5) == 'pft12' ) then
               pftp_12(i,j) = scale_factor * wrk_sum
             elseif( varname(:5) == 'pft13' ) then
               pftp_13(i,j) = scale_factor * wrk_sum
             elseif( varname(:5) == 'pft14' ) then
               pftp_14(i,j) = scale_factor * wrk_sum
             elseif( varname(:5) == 'pft15' ) then
               pftp_15(i,j) = scale_factor * wrk_sum
            elseif( varname(:5) == 'pft16' ) then
               pftp_16(i,j) = scale_factor * wrk_sum


             elseif( varname(:7) == 'TAS_AVE' ) then
               mtsa(i,j,mnth) = scale_factor * wrk_sum
             elseif( varname(:7) == 'DSW_AVE' ) then
               mswdown(i,j,mnth) = scale_factor * wrk_sum
             endif
        msebio_omtp(:,:) = 1.0
        msebio_farn(:,:) = 1.0
        msebio_bcar(:,:) = 1.0
        msebio_osqt(:,:) = 1.0
        msebio_acto(:,:) = 1.0
        msebio_co(:,:) = 1.0
        msebio_bider(:,:) = 1.0
        msebio_stress(:,:) = 1.0
        msebio_other(:,:) = 1.0


           end do
         end do
       end do mnth_loop
    endif is_area_map

!---------------------------------------------------------------------
!   exit
!---------------------------------------------------------------------
    if( has_area_map ) then
      deallocate( xedge_megan, yedge_megan )
    else
       deallocate( ix, jy, ax, by, tmp3 )
    endif

    write(*,*) ' Finished megan2 bio emiss dataset ',trim(inpname)
!---------------------------------------------------------------------
!   close megan dataset file
!---------------------------------------------------------------------
    message = 'Failed to close ' // trim(inpname)
    call handle_ncerr( nf_close( ncid ), message )       

    end subroutine megan2_bioemiss

   subroutine write_bioemiss
!---------------------------------------------------------------------
!	... write the netcdf bio emission file
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!	... local variables
!---------------------------------------------------------------------
      integer :: lon_id
      integer :: lat_id
      integer :: time_id
      integer :: months_id
      integer :: string_id
      integer :: dims(4)
      integer :: start_ndx(4)
      integer :: length(4)

      write(num,'(i3)') 100+domain
      outpname = 'wrfbiochemi_d' // num(2:3)
!-----------------------------------------------------------------------
!     	... create netcdf bio emission file and enter define mode
!-----------------------------------------------------------------------
      message = 'write_bioemiss: Failed to create ' // trim( outpname )
      call handle_ncerr( nf_create( trim( outpname ), nf_clobber, ncid ), message )

!-----------------------------------------------------------------------
!     	... define the dimensions
!-----------------------------------------------------------------------
      call handle_ncerr( nf_def_dim( ncid, 'west_east', ide, lon_id ), &
                         'write_bioemiss: Failed to define longitude dimension' )
      call handle_ncerr( nf_def_dim( ncid, 'south_north', jde, lat_id ), &
                         'write_bioemiss: Failed to define latitude dimension' )
      call handle_ncerr( nf_def_dim( ncid, 'months_per_year_stag', 46, months_id ), &
                         'write_bioemiss: Failed to define months_per_year_stag dimension' )
      call handle_ncerr( nf_def_dim( ncid, 'DateStrLen', 19, string_id ), &
                         'write_bioemiss: Failed to define DateStrLen dimension' )
      call handle_ncerr( nf_def_dim( ncid, 'Time', nf_unlimited, time_id ), &
                         'write_bioemiss: Failed to create Time dimension' )
!-----------------------------------------------------------------------
!     	... define the variables
!-----------------------------------------------------------------------
      dims(1:2) = (/ string_id, time_id /)
      call handle_ncerr( nf_def_var( ncid, 'Times', nf_char, 2, dims(1:2), varid ), &
                         'write_bioemiss: Failed to define Times variable' )
      dims(1:3) = (/ lon_id, lat_id, time_id /)
      call handle_ncerr( nf_def_var( ncid, 'XLONG', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define XLONG variable' )
      call handle_ncerr( nf_def_var( ncid, 'XLAT', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define XLAT variable' )
      call handle_ncerr( nf_def_var( ncid, 'MSEBIO_ISOP', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define MSEBIO_ISOP variable' )

      call handle_ncerr( nf_def_var( ncid, 'MSEBIO_MYRC', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define MSEBIO_MYRC variable' )
      call handle_ncerr( nf_def_var( ncid, 'MSEBIO_SABI', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define MSEBIO_SABI variable' )
      call handle_ncerr( nf_def_var( ncid, 'MSEBIO_LIMO', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define MSEBIO_LIMO variable' )
      call handle_ncerr( nf_def_var( ncid, 'MSEBIO_A_3CAR', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define MSEBIO_A_3CAR variable' )
      call handle_ncerr( nf_def_var( ncid, 'MSEBIO_OCIM', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define MSEBIO_OCIM variable' )
      call handle_ncerr( nf_def_var( ncid, 'MSEBIO_BPIN', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define MSEBIO_BPIN variable' )
      call handle_ncerr( nf_def_var( ncid, 'MSEBIO_APIN', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define MSEBIO_APIN variable' )
      call handle_ncerr( nf_def_var( ncid, 'MSEBIO_MBO', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define MSEBIO_MBO variable' )
      call handle_ncerr( nf_def_var( ncid, 'MSEBIO_MEOH', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define MSEBIO_MEOH variable' )
      call handle_ncerr( nf_def_var( ncid, 'MSEBIO_NO', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define MSEBIO_NO variable' )

      call handle_ncerr( nf_def_var( ncid, 'MSEBIO_OMTP', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define MSEBIO_OMTP variable' )
      call handle_ncerr( nf_def_var( ncid, 'MSEBIO_FARN', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define MSEBIO_FARN variable' )
      call handle_ncerr( nf_def_var( ncid, 'MSEBIO_BCAR', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define MSEBIO_BCAR variable' )
      call handle_ncerr( nf_def_var( ncid, 'MSEBIO_OSQT', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define MSEBIO_OSQT variable' )
      call handle_ncerr( nf_def_var( ncid, 'MSEBIO_ACTO', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define MSEBIO_ACTO variable' )
      call handle_ncerr( nf_def_var( ncid, 'MSEBIO_CO', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define MSEBIO_CO variable' )
      call handle_ncerr( nf_def_var( ncid, 'MSEBIO_BIDER', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define MSEBIO_BIDER variable' )
      call handle_ncerr( nf_def_var( ncid, 'MSEBIO_STRESS', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define MSEBIO_STRESS variable' )
      call handle_ncerr( nf_def_var( ncid, 'MSEBIO_OTHER', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define MSEBIO_OTHER variable' )


 write(*,*)'Jiang testing 1'
      call handle_ncerr( nf_def_var( ncid, 'PFTP_01', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define PFTP_01 variable' )
      call handle_ncerr( nf_def_var( ncid, 'PFTP_02', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define PFTP_02 variable' )
      call handle_ncerr( nf_def_var( ncid, 'PFTP_03', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define PFTP_03 variable' )
      call handle_ncerr( nf_def_var( ncid, 'PFTP_04', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define PFTP_04 variable' )
      call handle_ncerr( nf_def_var( ncid, 'PFTP_05', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define PFTP_05 variable' )
      call handle_ncerr( nf_def_var( ncid, 'PFTP_06', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define PFTP_06 variable' )
      call handle_ncerr( nf_def_var( ncid, 'PFTP_07', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define PFTP_07 variable' )
      call handle_ncerr( nf_def_var( ncid, 'PFTP_08', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define PFTP_08 variable' )
      call handle_ncerr( nf_def_var( ncid, 'PFTP_09', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define PFTP_09 variable' )
      call handle_ncerr( nf_def_var( ncid, 'PFTP_10', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define PFTP_10 variable' )
      call handle_ncerr( nf_def_var( ncid, 'PFTP_11', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define PFTP_11 variable' )
      call handle_ncerr( nf_def_var( ncid, 'PFTP_12', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define PFTP_12 variable' )
      call handle_ncerr( nf_def_var( ncid, 'PFTP_13', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define PFTP_13 variable' )
      call handle_ncerr( nf_def_var( ncid, 'PFTP_14', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define PFTP_14 variable' )
      call handle_ncerr( nf_def_var( ncid, 'PFTP_15', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define PFTP_15 variable' )
      call handle_ncerr( nf_def_var( ncid, 'PFTP_16', nf_float, 3, dims(1:3), varid ), &
                         'write_bioemiss: Failed to define PFTP_16 variable' )

      dims(:) = (/ lon_id, lat_id, months_id, time_id /)
      call handle_ncerr( nf_def_var( ncid, 'MLAI', nf_float, 4, dims, varid ), &
                         'write_bioemiss: Failed to define MLAI variable' )
      call handle_ncerr( nf_def_var( ncid, 'MTSA', nf_float, 4, dims, varid ), &
                         'write_bioemiss: Failed to define MTSA variable' )
      call handle_ncerr( nf_def_var( ncid, 'MSWDOWN', nf_float, 4, dims, varid ), &
                         'write_bioemiss: Failed to define MSWDOWN variable' )
 write(*,*)'Jiang testing 2'
!-----------------------------------------------------------------------
!     	... define attributes
!-----------------------------------------------------------------------
      varname = 'XLONG'
      units_attribute       = 'degree east'
      description_attribute = 'LONGITUDE, WEST IS NEGATIVE'
      stagger_attribute     = ''
      memord_attribute      = 'XY '
      coor_attribute        = ' '
      CALL write_bioemiss_attributes

      varname = 'XLAT'
      units_attribute       = 'degree north'
      description_attribute = 'LATITUDE, SOUTH IS NEGATIVE'
      stagger_attribute     = ''
      memord_attribute      = 'XY '
      coor_attribute        = ' '
      CALL write_bioemiss_attributes

      varname = 'MSEBIO_ISOP'
      units_attribute       = 'ug m^-2 hr^-1'
      description_attribute = 'isoprene emission factor'
      stagger_attribute     = ''
      memord_attribute      = 'XY '
      coor_attribute        = 'XLONG XLAT'
      CALL write_bioemiss_attributes
      varname = 'MSEBIO_MYRC'
      units_attribute       = 'ug m^-2 hr^-1'
      description_attribute = 'myrcene emission factor'
      stagger_attribute     = ''
      memord_attribute      = 'XY '
      coor_attribute        = 'XLONG XLAT'
      CALL write_bioemiss_attributes
      varname = 'MSEBIO_SABI'
      units_attribute       = 'ug m^-2 hr^-1'
      description_attribute = 'sabinene emission factor'
      stagger_attribute     = ''
      memord_attribute      = 'XY '
      coor_attribute        = 'XLONG XLAT'
      CALL write_bioemiss_attributes
      varname = 'MSEBIO_LIMO'
      units_attribute       = 'ug m^-2 hr^-1'
      description_attribute = 'limonene emission factor'
      stagger_attribute     = ''
      memord_attribute      = 'XY '
      coor_attribute        = 'XLONG XLAT'
      CALL write_bioemiss_attributes
      varname = 'MSEBIO_A_3CAR'
      units_attribute       = 'ug m^-2 hr^-1'
      description_attribute = 'carene emission factor'
      stagger_attribute     = ''
      memord_attribute      = 'XY '
      coor_attribute        = 'XLONG XLAT'
      CALL write_bioemiss_attributes
      varname = 'MSEBIO_OCIM'
      units_attribute       = 'ug m^-2 hr^-1'
      description_attribute = 'ocimene emission factor'
      stagger_attribute     = ''
      memord_attribute      = 'XY '
      coor_attribute        = 'XLONG XLAT'
      CALL write_bioemiss_attributes
      varname = 'MSEBIO_BPIN'
      units_attribute       = 'ug m^-2 hr^-1'
      description_attribute = 'beta_pinene emission factor'
      stagger_attribute     = ''
      memord_attribute      = 'XY '
      coor_attribute        = 'XLONG XLAT'
      CALL write_bioemiss_attributes
      varname = 'MSEBIO_APIN'
      units_attribute       = 'ug m^-2 hr^-1'
      description_attribute = 'alpha_pinene emission factor'
      stagger_attribute     = ''
      memord_attribute      = 'XY '
      coor_attribute        = 'XLONG XLAT'
      CALL write_bioemiss_attributes
      varname = 'MSEBIO_MBO'
      units_attribute       = 'ug m^-2 hr^-1'
      description_attribute = 'methylbutenol emission factor'
      stagger_attribute     = ''
      memord_attribute      = 'XY '
      coor_attribute        = 'XLONG XLAT'
      CALL write_bioemiss_attributes
      varname = 'MSEBIO_MEOH'
      units_attribute       = 'ug m^-2 hr^-1'
      description_attribute = 'methanol emission factor'
      stagger_attribute     = ''
      memord_attribute      = 'XY '
      coor_attribute        = 'XLONG XLAT'
      CALL write_bioemiss_attributes
      varname = 'MSEBIO_NO'
      units_attribute       = 'ug m^-2 hr^-1'
      description_attribute = 'nitric_oxide emission factor'
      stagger_attribute     = ''
      memord_attribute      = 'XY '
      coor_attribute        = 'XLONG XLAT'
      CALL write_bioemiss_attributes

 write(*,*)'Jiang testing 3'
      varname = 'MSEBIO_OMTP'
      units_attribute       = 'ug m^-2 hr^-1'
      description_attribute = 'omtp emission factor'
      stagger_attribute     = ''
      memord_attribute      = 'XY '
      coor_attribute        = 'XLONG XLAT'
      CALL write_bioemiss_attributes
      varname = 'MSEBIO_FARN'
      units_attribute       = 'ug m^-2 hr^-1'
      description_attribute = 'farn emission factor'
      stagger_attribute     = ''
      memord_attribute      = 'XY '
      coor_attribute        = 'XLONG XLAT'
      CALL write_bioemiss_attributes
      varname = 'MSEBIO_BCAR'
      units_attribute       = 'ug m^-2 hr^-1'
      description_attribute = 'bcar emission factor'
      stagger_attribute     = ''
      memord_attribute      = 'XY '
      coor_attribute        = 'XLONG XLAT'
      CALL write_bioemiss_attributes
      varname = 'MSEBIO_OSQT'
      units_attribute       = 'ug m^-2 hr^-1'
      description_attribute = 'osqt factor'
      stagger_attribute     = ''
      memord_attribute      = 'XY '
      coor_attribute        = 'XLONG XLAT'
      CALL write_bioemiss_attributes
      varname = 'MSEBIO_ACTO'
      units_attribute       = 'ug m^-2 hr^-1'
      description_attribute = 'acto emission factor'
      stagger_attribute     = ''
      memord_attribute      = 'XY '
      coor_attribute        = 'XLONG XLAT'
      CALL write_bioemiss_attributes
      varname = 'MSEBIO_CO'
      units_attribute       = 'ug m^-2 hr^-1'
      description_attribute = 'co emission factor'
      stagger_attribute     = ''
      memord_attribute      = 'XY '
      coor_attribute        = 'XLONG XLAT'
      CALL write_bioemiss_attributes
      varname = 'MSEBIO_BIDER'
      units_attribute       = 'ug m^-2 hr^-1'
      description_attribute = 'bider emission factor'
      stagger_attribute     = ''
      memord_attribute      = 'XY '
      coor_attribute        = 'XLONG XLAT'
      CALL write_bioemiss_attributes
      varname = 'MSEBIO_STRESS'
      units_attribute       = 'ug m^-2 hr^-1'
      description_attribute = 'stress emission factor'
      stagger_attribute     = ''
      memord_attribute      = 'XY '
      coor_attribute        = 'XLONG XLAT'
      CALL write_bioemiss_attributes
      varname = 'MSEBIO_OTHER'
      units_attribute       = 'ug m^-2 hr^-1'
      description_attribute = 'other emission factor'
      stagger_attribute     = ''
      memord_attribute      = 'XY '
      coor_attribute        = 'XLONG XLAT'
      CALL write_bioemiss_attributes

 write(*,*)'Jiang testing 4'

      varname = 'PFTP_01'
      description_attribute = 'MEGAN2 NET temperate PFT % coverage'
      units_attribute       = '%'
      CALL write_bioemiss_attributes

      varname = 'PFTP_02'
      description_attribute = 'MEGAN2 NET boreal PFT % coverage'
      CALL write_bioemiss_attributes

      varname = 'PFTP_03'
      description_attribute = 'MEGAN2 NDT boreal PFT % coverage'
      CALL write_bioemiss_attributes

      varname = 'PFTP_04'
      description_attribute = 'MEGAN2 BET tropical PFT % coverage'
      CALL write_bioemiss_attributes

      varname = 'PFTP_05'
      description_attribute = 'MEGAN2 BET temperate PFT % coverage'
      CALL write_bioemiss_attributes

      varname = 'PFTP_06'
      description_attribute = 'MEGAN2 BDT tropical PFT % coverage'
      units_attribute       = '%'
      CALL write_bioemiss_attributes

      varname = 'PFTP_07'
      description_attribute = 'MEGAN2 BDT temperate PFT % coverage'
      CALL write_bioemiss_attributes

      varname = 'PFTP_08'
      description_attribute = 'MEGAN2 BDT boreal PFT % coverage'
      CALL write_bioemiss_attributes

      varname = 'PFTP_09'
      description_attribute = 'MEGAN2  BES temperate PFT % coverage'
      CALL write_bioemiss_attributes

      varname = 'PFTP_10'
      description_attribute = 'MEGAN2 BDS temperate PFT % coverage'
      CALL write_bioemiss_attributes

      varname = 'PFTP_11'
      description_attribute = 'MEGAN2 BDS boreal PFT % coverage'
      units_attribute       = '%'
      CALL write_bioemiss_attributes

      varname = 'PFTP_12'
      description_attribute = 'MEGAN2 C3 grass arctic PFT % coverage'
      CALL write_bioemiss_attributes

      varname = 'PFTP_13'
      description_attribute = 'MEGAN2 C3 grass PFT % coverage'
      CALL write_bioemiss_attributes

      varname = 'PFTP_14'
      description_attribute = 'MEGAN2 C4 grass PFT % coverage'
      CALL write_bioemiss_attributes

      varname = 'PFTP_15'
      description_attribute = 'MEGAN2 Crop1 PFT % coverage'
      CALL write_bioemiss_attributes

      varname = 'PFTP_16'
      description_attribute = 'MEGAN2 Crop2 PFT % coverage'
      CALL write_bioemiss_attributes

      varname = 'MLAI'
      units_attribute   = ''
      stagger_attribute = 'Z'
      memord_attribute  = 'XYZ'
      description_attribute = 'Monthly Leaf area index for MEGAN2'
      CALL write_bioemiss_attributes

      varname = 'MTSA'
      units_attribute   = 'K'
      description_attribute = 'Monthly surface air temperature'
      CALL write_bioemiss_attributes

      varname = 'MSWDOWN'
      units_attribute   = 'W/m2'
      description_attribute = 'Monthly SWdown'
      CALL write_bioemiss_attributes

 write(*,*)'Jiang testing 5'
!-----------------------------------------------------------------------
!     	... leave define mode
!-----------------------------------------------------------------------
      call handle_ncerr( nf_enddef( ncid ), 'write_bioemiss: Failed to leave define mode' )

!-----------------------------------------------------------------------
!     	... write the variables
!-----------------------------------------------------------------------
      start_ndx(1:2) = (/ 1,1 /)
      length(1:2)    = (/ 19, 1 /)
      call handle_ncerr( nf_inq_varid( ncid, 'Times', varid ), &
                         'write_bioemiss: Failed to get Times variable id' )
      call handle_ncerr( nf_put_vara_text( ncid, varid, start_ndx(:2), length(:2), Times ), &
                         'write_bioemiss: Failed to write Times variable' )
      start_ndx(1:3) = (/ 1,1,1 /)
      length(1:3)    = (/ ide, jde, 1 /)
      call handle_ncerr( nf_inq_varid( ncid, 'XLONG', varid ), &
                         'write_bioemiss: Failed to get xlong variable id' )
      call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), xlong ), &
                         'write_bioemiss: Failed to write xlong variable' )
      call handle_ncerr( nf_inq_varid( ncid, 'XLAT', varid ), &
                         'write_bioemiss: Failed to get xlat variable id' )
      call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), xlat ), &
                         'write_bioemiss: Failed to write xlat variable' )
      call handle_ncerr( nf_inq_varid( ncid, 'MSEBIO_ISOP', varid ), &
                         'write_bioemiss: Failed to get msebio_isop variable id' )
      call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), msebio_isop ), &
                         'write_bioemiss: Failed to write msebio_isop variable' )

write(*,*)'Jiang testing 6'


     call handle_ncerr( nf_inq_varid( ncid, 'MSEBIO_MYRC', varid ), &
                         'write_bioemiss: Failed to get msebio_myrc variable id' )
     call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), msebio_myrc ), &
                         'write_bioemiss: Failed to write msebio_myrc variable' )
 
     call handle_ncerr( nf_inq_varid( ncid, 'MSEBIO_SABI', varid ), &
                         'write_bioemiss: Failed to get msebio_sabi variable id' )
     call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), msebio_sabi ), &
                         'write_bioemiss: Failed to write msebio_sabi variable' )

     call handle_ncerr( nf_inq_varid( ncid, 'MSEBIO_LIMO', varid ), &
                         'write_bioemiss: Failed to get msebio_limo variable id' )
     call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), msebio_limo ), &
                         'write_bioemiss: Failed to write msebio_limo variable' )

     call handle_ncerr( nf_inq_varid( ncid, 'MSEBIO_A_3CAR', varid ), &
                         'write_bioemiss: Failed to get msebio_a_3car variable id' )
     call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), msebio_a_3car ), &
                         'write_bioemiss: Failed to write msebio_a_3car variable' )

     call handle_ncerr( nf_inq_varid( ncid, 'MSEBIO_OCIM', varid ), &
                         'write_bioemiss: Failed to get msebio_ocim variable id' )
     call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), msebio_ocim ), &
                         'write_bioemiss: Failed to write msebio_ocim variable' )

     call handle_ncerr( nf_inq_varid( ncid, 'MSEBIO_BPIN', varid ), &
                         'write_bioemiss: Failed to get msebio_bpin variable id' )
     call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), msebio_bpin ), &
                         'write_bioemiss: Failed to write msebio_bpin variable' )

     call handle_ncerr( nf_inq_varid( ncid, 'MSEBIO_APIN', varid ), &
                         'write_bioemiss: Failed to get msebio_apin variable id' )
     call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), msebio_apin ), &
                         'write_bioemiss: Failed to write msebio_apin variable' )

     call handle_ncerr( nf_inq_varid( ncid, 'MSEBIO_MBO', varid ), &
                         'write_bioemiss: Failed to get msebio_mbo variable id' )
     call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), msebio_mbo ), &
                         'write_bioemiss: Failed to write msebio_mbo variable' )

     call handle_ncerr( nf_inq_varid( ncid, 'MSEBIO_MEOH', varid ), &
                         'write_bioemiss: Failed to get msebio_meoh variable id' )
     call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), msebio_meoh ), &
                         'write_bioemiss: Failed to write msebio_meoh variable' )

     call handle_ncerr( nf_inq_varid( ncid, 'MSEBIO_NO', varid ), &
                         'write_bioemiss: Failed to get msebio_no variable id' )
      call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), msebio_no ), &
                         'write_bioemiss: Failed to write msebio_no variable' )
 write(*,*)'Jiang testing 7'

     call handle_ncerr( nf_inq_varid( ncid, 'MSEBIO_OMTP', varid ), &
                         'write_bioemiss: Failed to get msebio_omtp variable id' )
      call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), msebio_omtp ), &
                         'write_bioemiss: Failed to write msebio_omtp variable' )
 
    call handle_ncerr( nf_inq_varid( ncid, 'MSEBIO_FARN', varid ), &
                         'write_bioemiss: Failed to get msebio_farn variable id' )
      call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), msebio_farn ), &
                         'write_bioemiss: Failed to write msebio_farn variable' )

     call handle_ncerr( nf_inq_varid( ncid, 'MSEBIO_BCAR', varid ), &
                         'write_bioemiss: Failed to get msebio_bcar variable id' )
      call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), msebio_bcar ), &
                         'write_bioemiss: Failed to write msebio_bcar variable' )

     call handle_ncerr( nf_inq_varid( ncid, 'MSEBIO_OSQT', varid ), &
                         'write_bioemiss: Failed to get msebio_osqt variable id' )
      call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), msebio_osqt ), &
                         'write_bioemiss: Failed to write msebio_osqt variable' )

     call handle_ncerr( nf_inq_varid( ncid, 'MSEBIO_ACTO', varid ), &
                         'write_bioemiss: Failed to get msebio_acto variable id' )
      call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), msebio_acto ), &
                         'write_bioemiss: Failed to write msebio_acto variable' )

     call handle_ncerr( nf_inq_varid( ncid, 'MSEBIO_CO', varid ), &
                         'write_bioemiss: Failed to get msebio_co variable id' )
      call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), msebio_co ), &
                         'write_bioemiss: Failed to write msebio_co variable' )

     call handle_ncerr( nf_inq_varid( ncid, 'MSEBIO_BIDER', varid ), &
                         'write_bioemiss: Failed to get msebio_bider variable id' )
      call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), msebio_bider ), &
                         'write_bioemiss: Failed to write msebio_bider variable' )

     call handle_ncerr( nf_inq_varid( ncid, 'MSEBIO_STRESS', varid ), &
                         'write_bioemiss: Failed to get msebio_stress variable id' )
      call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), msebio_stress ), &
                         'write_bioemiss: Failed to write msebio_stress variable' )

     call handle_ncerr( nf_inq_varid( ncid, 'MSEBIO_OTHER', varid ), &
                         'write_bioemiss: Failed to get msebio_other variable id' )
      call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), msebio_other ), &
                         'write_bioemiss: Failed to write msebio_other variable' )


      call handle_ncerr( nf_inq_varid( ncid, 'PFTP_01', varid ), &
                         'write_bioemiss: Failed to get pftp_01 variable id' )
      call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), pftp_01 ), &
                         'write_bioemiss: Failed to write pftp_01 variable' )
      call handle_ncerr( nf_inq_varid( ncid, 'PFTP_02', varid ), &
                         'write_bioemiss: Failed to get pftp_02 variable id' )
      call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), pftp_02 ), &
                         'write_bioemiss: Failed to write pftp_02 variable' )
      call handle_ncerr( nf_inq_varid( ncid, 'PFTP_03', varid ), &
                         'write_bioemiss: Failed to get pftp_03 variable id' )
      call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), pftp_03 ), &
                         'write_bioemiss: Failed to write pftp_03 variable' )
      call handle_ncerr( nf_inq_varid( ncid, 'PFTP_04', varid ), &
                         'write_bioemiss: Failed to get pftp_04 variable id' )
      call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), pftp_04 ), &
                         'write_bioemiss: Failed to write pftp_04 variable' )
      call handle_ncerr( nf_inq_varid( ncid, 'PFTP_05', varid ), &
                         'write_bioemiss: Failed to get pftp_05 variable id' )
      call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), pftp_05 ), &
                         'write_bioemiss: Failed to write pftp_05 variable' )
     call handle_ncerr( nf_inq_varid( ncid, 'PFTP_06', varid ), &
                         'write_bioemiss: Failed to get pftp_06 variable id' )
      call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), pftp_06 ), &
                         'write_bioemiss: Failed to write pftp_06 variable' )
      call handle_ncerr( nf_inq_varid( ncid, 'PFTP_07', varid ), &
                         'write_bioemiss: Failed to get pftp_07 variable id' )
      call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), pftp_07 ), &
                         'write_bioemiss: Failed to write pftp_07 variable' )
      call handle_ncerr( nf_inq_varid( ncid, 'PFTP_08', varid ), &
                         'write_bioemiss: Failed to get pftp_08 variable id' )
      call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), pftp_08 ), &
                         'write_bioemiss: Failed to write pftp_08 variable' )
      call handle_ncerr( nf_inq_varid( ncid, 'PFTP_09', varid ), &
                         'write_bioemiss: Failed to get pftp_09 variable id' )
      call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), pftp_08 ), &
                         'write_bioemiss: Failed to write pftp_09 variable' )
      call handle_ncerr( nf_inq_varid( ncid, 'PFTP_10', varid ), &
                         'write_bioemiss: Failed to get pftp_10 variable id' )
      call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), pftp_10 ), &
                         'write_bioemiss: Failed to write pftp_10 variable' )
     call handle_ncerr( nf_inq_varid( ncid, 'PFTP_11', varid ), &
                         'write_bioemiss: Failed to get pftp_11 variable id' )
      call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), pftp_11 ), &
                         'write_bioemiss: Failed to write pftp_11 variable' )
      call handle_ncerr( nf_inq_varid( ncid, 'PFTP_12', varid ), &
                         'write_bioemiss: Failed to get pftp_12 variable id' )
      call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), pftp_12 ), &
                         'write_bioemiss: Failed to write pftp_12 variable' )
      call handle_ncerr( nf_inq_varid( ncid, 'PFTP_13', varid ), &
                         'write_bioemiss: Failed to get pftp_13 variable id' )
      call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), pftp_13 ), &
                         'write_bioemiss: Failed to write pftp_13 variable' )
      call handle_ncerr( nf_inq_varid( ncid, 'PFTP_14', varid ), &
                         'write_bioemiss: Failed to get pftp_14 variable id' )
      call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), pftp_14 ), &
                         'write_bioemiss: Failed to write pftp_14 variable' )
      call handle_ncerr( nf_inq_varid( ncid, 'PFTP_15', varid ), &
                         'write_bioemiss: Failed to get pftp_15 variable id' )
      call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), pftp_15 ), &
                         'write_bioemiss: Failed to write pftp_15 variable' )
     call handle_ncerr( nf_inq_varid( ncid, 'PFTP_16', varid ), &
                         'write_bioemiss: Failed to get pftp_16 variable id' )
     call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx(:3), length(:3), pftp_16 ), &
                         'write_bioemiss: Failed to write pftp_16 variable' )


      start_ndx(:) = (/ 1,1,1,1 /)
      length(:)    = (/ ide, jde, 1, 1 /)

write(*,*)'Jiang testing 8'
      do mnth = 1,46
         start_ndx(3) = mnth
         call handle_ncerr( nf_inq_varid( ncid, 'MLAI', varid ), &
                            'write_bioemiss: Failed to get mlai variable id' )
         call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx, length, mlai(:,:,mnth) ), &
                            'write_bioemiss: Failed to write mlai variable' )
       end do

write(*,*)'Jiang testing 9'
       do mnth = 1,12 
         call handle_ncerr( nf_inq_varid( ncid, 'MTSA', varid ), &
                            'write_bioemiss: Failed to get mtsa variable id' )
         call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx, length, mtsa(:,:,mnth) ), &
                            'write_bioemiss: Failed to write mtsa variable' )
         call handle_ncerr( nf_inq_varid( ncid, 'MSWDOWN', varid ), &
                            'write_bioemiss: Failed to get mswdown variable id' )
         call handle_ncerr( nf_put_vara_real( ncid, varid, start_ndx, length, mswdown(:,:,mnth) ), &
                            'write_bioemiss: Failed to write mswdown variable' )
      end do
!---------------------------------------------------------------------
!   close wrf file
!---------------------------------------------------------------------
       message = 'Failed to close ' // trim(outpname)
       call handle_ncerr( nf_close( ncid ), message )       

   end subroutine write_bioemiss

   subroutine write_bioemiss_attributes
!---------------------------------------------------------------------
!   write common variable attributes
!---------------------------------------------------------------------

      message = 'write_bioemiss: Failed to get ' // trim(varname) // ' variable id'
      call handle_ncerr( nf_inq_varid( ncid, trim(varname), varid ), message )
      message = 'write_bioemiss: Failed to create ' // trim(varname) // ' attribute'
      call handle_ncerr( nf_put_att_text( ncid, varid, 'MemoryOrder', 3, memord_attribute ), message )
      call handle_ncerr( nf_put_att_text( ncid, varid, 'description', &
                                          len_trim(description_attribute), trim(description_attribute) ), message )
      call handle_ncerr( nf_put_att_text( ncid, varid, 'units', &
                                          len_trim(units_attribute), trim(units_attribute) ), message )
      call handle_ncerr( nf_put_att_text( ncid, varid, 'stagger', &
                                          len_trim(stagger_attribute), trim(stagger_attribute) ), message )
      if( coor_attribute /= ' ' ) then
         call handle_ncerr( nf_put_att_text( ncid, varid, 'coordinates', &
                                             len_trim(coor_attribute), trim(coor_attribute) ), message )
      endif
      ii = 104
      call handle_ncerr( nf_put_att_int( ncid, varid, 'FieldType', nf_int, 1, ii ), message )

   end subroutine write_bioemiss_attributes

   subroutine handle_ncerr( ret, mes )
!---------------------------------------------------------------------
!	... netcdf error handling routine
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!	... dummy arguments
!---------------------------------------------------------------------
   integer, intent(in) :: ret
   character(len=*), intent(in) :: mes

   if( ret /= nf_noerr ) then
      write(*,*) nf_strerror( ret )
      stop 'netcdf error'
   endif

   end subroutine handle_ncerr

   subroutine write_ef210
!---------------------------------------------------------------------
!       ... write the csv emission factor file
!---------------------------------------------------------------------
!       ... local variables
!---------------------------------------------------------------------
      integer :: cell_id
      integer :: ilat,ilon
      integer :: dsrad,dtemp
      dsrad = 250
      dtemp=300

      outpname = 'EF210.csv'
      open(unit=99,file=outpname,status='unknown')
      write(99,'(a)')"CELL_ID,X,Y,LAT,LONG,DSRAD,DTEMP,ISOP,MYRC,SABI,LIMO,A_3CAR,OCIM,BPIN,APIN,OMTP,FARN,BCAR,OSQT,MBO,MEOH,ACTO,CO,NO,BIDER,STRESS,OTHER"
      cell_id=0
      do ilat=2,jde-jds
       do ilon=2,ide-ids
      cell_id=cell_id+1
      write (99, '(3(I5,","), 2(F10.4,","),2(I,","),19(F10.4,","),1(F10.4))')cell_id,ilon-1,ilat-1,XLAT(ilon,ilat),XLONG(ilon,ilat), &
                                        dsrad,dtemp,&
                                        MSEBIO_ISOP(ilon,ilat),MSEBIO_MYRC(ilon,ilat),MSEBIO_SABI(ilon,ilat), &
                                        MSEBIO_LIMO(ilon,ilat),MSEBIO_A_3CAR(ilon,ilat),MSEBIO_OCIM(ilon,ilat), &
                                        MSEBIO_BPIN(ilon,ilat),MSEBIO_APIN(ilon,ilat),MSEBIO_OMTP(ilon,ilat), &
                                        MSEBIO_FARN(ilon,ilat),MSEBIO_BCAR(ilon,ilat),MSEBIO_OSQT(ilon,ilat), &
                                        MSEBIO_MBO(ilon,ilat),MSEBIO_MEOH(ilon,ilat),MSEBIO_ACTO(ilon,ilat), &
                                        MSEBIO_CO(ilon,ilat),MSEBIO_NO(ilon,ilat),MSEBIO_BIDER(ilon,ilat), &
                                        MSEBIO_STRESS(ilon,ilat),MSEBIO_OTHER(ilon,ilat)                            
       end do
      end do
      close(99)
      end subroutine write_ef210

      subroutine write_lai210
!---------------------------------------------------------------------
!       ... write the lai  file
!---------------------------------------------------------------------
!       ... local variables
!---------------------------------------------------------------------
      integer :: cell_id,istep,it,im
      integer :: ilat,ilon
      integer :: dsrad,dtemp
      real :: xlai(ide,jde,46)

      outpname = 'LAI210.csv'
      open(unit=99,file=outpname,status='unknown')
      write(99,'(a)')"CELL_ID,X,Y,LAI01,LAI02,LAI03,LAI04,LAI05,LAI06,LAI07,LAI08,LAI09,LAI10, &
                      LAI11,LAI12,LAI13,LAI14,LAI15,LAI16,LAI17,LAI18,LAI19,LAI20, &
                      LAI21,LAI22,LAI23,LAI24,LAI25,LAI26,LAI27,LAI28,LAI29,LAI30, &
                      LAI31,LAI32,LAI33,LAI34,LAI35,LAI36,LAI37,LAI38,LAI39,LAI40, &
                      LAI41,LAI42,LAI43,LAI44,LAI45,LAI46"
      

     xlai(:,:,:)=mlai(:,:,:)

      do ilat=1,jde-jds+1
        do ilon=1,ide-ids+1
        do im=1,46 
      if(xlai(ilon,ilat,im).gt.20.or.xlai(ilon,ilat,im).lt.0.0)xlai(ilon,ilat,im)=0.0
        end do
      end do
      end do
!-----write new xlai to output
     cell_id=0
      do ilat=2,jde-jds
       do ilon=2,ide-ids
      cell_id=cell_id+1
      write (99, '(3(I,","), 46(F10.4,","))')cell_id,ilon-1,ilat-1,xlai(ilon,ilat,:)
       end do
      end do
     close(99)
        
      end subroutine write_lai210

      subroutine write_pft210
!---------------------------------------------------------------------
!       ... write the lai  file
!---------------------------------------------------------------------
!       ... local variables
!---------------------------------------------------------------------
      integer :: cell_id,istep,it,im
      integer :: ilat,ilon
      integer :: dsrad,dtemp
      real :: xlai(ide,jde,46)
      real :: NT_EG_TEMP(ide,jde),  NT_DC_BORL(ide,jde),  NT_EG_BORL(ide,jde)
      real :: BT_EG_TROP(ide,jde),  BT_EG_TEMP(ide,jde),  BT_DC_TROP(ide,jde),  BT_DC_TEMP(ide,jde),  BT_DC_BORL(ide,jde)
      real :: SB_EG_TEMP(ide,jde),  SB_DC_TEMP(ide,jde),  SB_DC_BORL(ide,jde)
      real :: GS_C3_COLD(ide,jde),  GS_C3_COOL(ide,jde),  GS_C3_WARM(ide,jde)
      real :: CROP(ide,jde),CORN(ide,jde)

!-----convert six pfts to sixteen pfts 
!nt   =fet, fdt, fet ==>NT_EG_TEMP,  NT_DC_BORL,  NT_EG_BORL
!bt   =btr==>BT_EG_TROP,  BT_EG_TEMP,  BT_DC_TROP,  BT_DC_TEMP,  BT_DC_BORL
!sb   =shr==>SB_EG_TEMP,  SB_DC_TEMP,  SB_DC_BORL
!hb   =grs==>GS_C3_COLD,  GS_C3_COOL,  GS_C3_WARM
!cr   =crp==>CROP,CORN

NT_EG_TEMP(:,:) = pftp_01(:,:)  
NT_DC_BORL(:,:) = pftp_02(:,:) 
NT_EG_BORL(:,:) = pftp_03(:,:)
BT_EG_TROP(:,:) = pftp_04(:,:) 
BT_EG_TEMP(:,:) = pftp_05(:,:) 
BT_DC_TROP(:,:) = pftp_06(:,:) 
BT_DC_TEMP(:,:) = pftp_07(:,:) 
BT_DC_BORL(:,:) = pftp_08(:,:)
SB_EG_TEMP(:,:) = pftp_09(:,:) 
SB_DC_TEMP(:,:) = pftp_10(:,:) 
SB_DC_BORL(:,:) = pftp_11(:,:)
GS_C3_COLD(:,:) = pftp_12(:,:) 
GS_C3_COOL(:,:) = pftp_13(:,:) 
GS_C3_WARM(:,:) = pftp_14(:,:)
CROP(:,:) = pftp_15(:,:)
CORN(:,:) = pftp_16(:,:)
 
     do ilat=1,jde-jds+1
        do ilon=1,ide-ids+1
      if(NT_EG_TEMP(ilon,ilat).gt.100.or.NT_EG_TEMP(ilon,ilat).lt.0.0)NT_EG_TEMP(ilon,ilat)=0.0
      if(NT_DC_BORL(ilon,ilat).gt.100.or.NT_DC_BORL(ilon,ilat).lt.0.0)NT_DC_BORL(ilon,ilat)=0.0
      if(NT_EG_BORL(ilon,ilat).gt.100.or.NT_EG_BORL(ilon,ilat).lt.0.0)NT_EG_BORL(ilon,ilat)=0.0
      if(BT_EG_TROP(ilon,ilat).gt.100.or.BT_EG_TROP(ilon,ilat).lt.0.0)BT_EG_TROP(ilon,ilat)=0.0
      if(BT_EG_TEMP(ilon,ilat).gt.100.or.BT_EG_TEMP(ilon,ilat).lt.0.0)BT_EG_TEMP(ilon,ilat)=0.0
      if(BT_DC_TROP(ilon,ilat).gt.100.or.BT_DC_TROP(ilon,ilat).lt.0.0)BT_DC_TROP(ilon,ilat)=0.0
      if(BT_DC_TEMP(ilon,ilat).gt.100.or.BT_DC_TEMP(ilon,ilat).lt.0.0)BT_DC_TEMP(ilon,ilat)=0.0
      if(BT_DC_BORL(ilon,ilat).gt.100.or.BT_DC_BORL(ilon,ilat).lt.0.0)BT_DC_BORL(ilon,ilat)=0.0
      if(SB_EG_TEMP(ilon,ilat).gt.100.or.SB_EG_TEMP(ilon,ilat).lt.0.0)SB_EG_TEMP(ilon,ilat)=0.0
      if(SB_DC_TEMP(ilon,ilat).gt.100.or.SB_DC_TEMP(ilon,ilat).lt.0.0)SB_DC_TEMP(ilon,ilat)=0.0
      if(SB_DC_BORL(ilon,ilat).gt.100.or.SB_DC_BORL(ilon,ilat).lt.0.0)SB_DC_BORL(ilon,ilat)=0.0
      if(GS_C3_COLD(ilon,ilat).gt.100.or.GS_C3_COLD(ilon,ilat).lt.0.0)GS_C3_COLD(ilon,ilat)=0.0
      if(GS_C3_COOL(ilon,ilat).gt.100.or.GS_C3_COOL(ilon,ilat).lt.0.0)GS_C3_COOL(ilon,ilat)=0.0
      if(GS_C3_WARM(ilon,ilat).gt.100.or.GS_C3_WARM(ilon,ilat).lt.0.0)GS_C3_WARM(ilon,ilat)=0.0
      if(CROP(ilon,ilat).gt.100.or.CROP(ilon,ilat).lt.0.0)CROP(ilon,ilat)=0.0
      if(CORN(ilon,ilat).gt.100.or.CORN(ilon,ilat).lt.0.0)CORN(ilon,ilat)=0.0
      end do
      end do


      outpname = 'PFT210.csv'
      open(unit=99,file=outpname,status='unknown')
      write(99,'(a)')"CID, ICELL, JCELL,  NT_EG_TEMP,  NT_DC_BORL,  NT_EG_BORL, &
                BT_EG_TROP,  BT_EG_TEMP,  BT_DC_TROP,  BT_DC_TEMP,  BT_DC_BORL, &
                SB_EG_TEMP,  SB_DC_TEMP,  SB_DC_BORL,  &
                GS_C3_COLD,  GS_C3_COOL,  GS_C3_WARM,  &
                CROP,CORN"
!-----write new pft to output
     cell_id=0
      do ilat=2,jde-jds
       do ilon=2,ide-ids
      cell_id=cell_id+1
      write (99, '(3(I,","), 16(F10.4,","))')cell_id,ilon-1,ilat-1, &
                NT_EG_TEMP(ilon,ilat),  NT_DC_BORL(ilon,ilat),  NT_EG_BORL(ilon,ilat), &
                BT_EG_TROP(ilon,ilat),  BT_EG_TEMP(ilon,ilat),  BT_DC_TROP(ilon,ilat), &
                BT_DC_TEMP(ilon,ilat),  BT_DC_BORL(ilon,ilat), &
                SB_EG_TEMP(ilon,ilat),  SB_DC_TEMP(ilon,ilat),  SB_DC_BORL(ilon,ilat),  &
                GS_C3_COLD(ilon,ilat),  GS_C3_COOL(ilon,ilat),  GS_C3_WARM(ilon,ilat),  &
                CROP(ilon,ilat),CORN(ilon,ilat)

       end do
      end do
     close(99)



      end subroutine write_pft210
end program map_megan2_emissions
