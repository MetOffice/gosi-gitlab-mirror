MODULE declaration
!!-----------------------------------------------------------
!!
!!
!!-----------------------------------------------------------
  USE netcdf
  !
! STRONG TYPING IMPOSED
! =====================
  IMPLICIT NONE
  PUBLIC
!
!
! SPECIFICATIONS FOR VARIABLES
! ============================
!
!
    character(len=nf90_max_name) :: sn_msk_fname_s, sn_msk_fname_t, smsk_fname
    character(len=1)  :: pivot, pivot_t
    character(len=12) :: msk_name(5) = (/ 'tmask', 'umask', 'vmask', 'fmask', 'tmask' /)
    character(len=1)  :: gtype(5) = (/ 'T', 'U', 'V', 'F', 'W' /)
    character(len=3)  :: e1_name_t(5) = (/ 'e1t', 'e1u', 'e1v', 'e1f', 'e1t' /)
    character(len=3)  :: e2_name_t(5) = (/ 'e2t', 'e2u', 'e2v', 'e2f', 'e2t' /)
    character(len=5)  :: rmp_fname(5) = (/ 'rmp_t', 'rmp_u', 'rmp_v', 'rmp_f', 'rmp_w' /)

    integer*4, parameter :: wp = 8
    ! Halo size -- to be parametrised
    integer*4, parameter :: hls = 1

    integer*4 :: nn_factor, nn_lli, nn_llj, nn_lvlcut
    integer*4 :: illid(2)
    integer*4 :: crsf
    integer*4 :: xt, yt, xs, ys, zd
!
    integer*4 :: iost, i_err
    integer*4 :: numnam_ref = 10
    integer*4 :: kout = 6
    integer*4 :: nb_link_2D, nb_link, nb_link_ptc

    logical :: l_2d = .FALSE.
    logical           :: ln_transport, ln_zoom

    integer(kind = 1), allocatable :: bmask_s(:,:,:,:), bmask_t(:,:,:,:)
    integer(kind = 1), allocatable :: check_s(:,:,:,:)

    real(wp)         , allocatable :: hsf_r(:,:,:), hsf_t(:,:)
    real(wp)         , allocatable :: addr_t(:,:)

    TYPE GRID_INFO
      INTEGER ::  ndim(3)=0   ! dimension size
      LOGICAL ::  iperio=.FALSE.   ! longitude periodicity
      LOGICAL ::  jperio=.FALSE.   ! latitude periodicity
      CHARACTER ::  pivot  ! pivot of North Pole Folding
    END TYPE GRID_INFO

    TYPE(GRID_INFO),DIMENSION(2) :: st_grid 
    TYPE(GRID_INFO)              :: gsource              
    TYPE(GRID_INFO)              :: gtarget              


END MODULE declaration
