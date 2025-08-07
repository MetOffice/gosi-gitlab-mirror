MODULE dyndta
   !!======================================================================
   !!                       ***  MODULE  dyndta  ***
   !! Off-line : interpolation of the physical fields
   !!======================================================================
   !! History :   !!        4.2.2 ! 2025-03 (D. Storkey) original code based on OFF/dtadyn.F90
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dta_dyn      : Read in after SSH, U and V fields and calculate
   !!                  after scale factors
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers variables
   USE dom_oce         ! ocean domain: variables
#if defined key_qco 
   USE domqco          ! variable volume
#elif ! defined key_linssh
   USE domvvl
#endif
   USE sbc_oce         ! surface module: variables
   USE ldftra, only:  aeiu, aeiv
   USE ldfslp, only:  uslp, vslp, wslpi, wslpj
   USE zdf_oce, only: avt, avs 
   USE lbclnk          ! ocean lateral boundary conditions (or mpp link)
   USE in_out_manager  ! I/O manager
   USE iom             ! I/O library
   USE lib_mpp         ! distributed memory computing library
   USE prtctl          ! print control
   USE fldread         ! read input fields 
   USE timing          ! Timing

   IMPLICIT NONE
   PRIVATE

   PUBLIC   dyn_dta            ! called by stp
   PUBLIC   dyn_dta_init       ! called by nemo_init

   CHARACTER(len=100) ::   cn_dir          !: Root directory for location of ssr files

   INTEGER  , PARAMETER ::   jpfld = 11     ! maximum number of fields to read
   INTEGER  , SAVE      ::   jf_ssh         ! index of ssh
   INTEGER  , SAVE      ::   jf_uu          ! index of u velocity
   INTEGER  , SAVE      ::   jf_vv          ! index of v velocity
   INTEGER  , SAVE      ::   jf_uslp        ! index of U isopycnal slope
   INTEGER  , SAVE      ::   jf_vslp        ! index of V isopycnal slope
   INTEGER  , SAVE      ::   jf_wslpi       ! index of W isopycnal slope (i-direction)
   INTEGER  , SAVE      ::   jf_wslpj       ! index of W isopycnal slope (j-direction)
   INTEGER  , SAVE      ::   jf_avt         ! vertical diffusivity (temperature)
   INTEGER  , SAVE      ::   jf_avs         ! vertical diffusivity (salinity)
   INTEGER  , SAVE      ::   jf_aeiu        ! eddy-induced diffusivity (i-direction)
   INTEGER  , SAVE      ::   jf_aeiv        ! eddy-induced diffusivity (j-direction)

   TYPE(FLD), ALLOCATABLE, SAVE, DIMENSION(:) :: sf_dyn  ! structure of input fields (file informations, fields read)
   !                                               ! 
   INTEGER, SAVE  :: nprevrec, nsecdyn

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   
   !!----------------------------------------------------------------------
   !! NEMO/OFF 4.0 , NEMO Consortium (2018)
   !! $Id: dyndta.F90 15090 2021-07-06 14:25:18Z cetlod $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dyn_dta( kt, Kbb, Kmm, Kaa )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dyn_dta  ***
      !!
      !! ** Purpose :  Prepares dynamics and physics fields from a NEMO run
      !!               for an off-line simulation of passive tracers
      !!
      !! ** Method : calculates the position of data 
      !!             - computes slopes if needed
      !!             - interpolates data if needed
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt             ! ocean time-step index
      INTEGER, INTENT(in) ::   Kbb, Kmm, Kaa  ! ocean time level indices
      !
      INTEGER             ::   ji, jj, jk
      REAL(wp), ALLOCATABLE, DIMENSION(:,:)   ::   zemp
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) ::   zhdivtr
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start( 'dyn_dta')
      !
      nsecdyn = nsec_year + nsec1jan000   ! number of seconds between Jan. 1st 00h of nit000 year and the middle of time step
      !
      IF(lwp) WRITE(numout,*) "Calling fld_read from dyn_dta"; FLUSH(numout)
      CALL fld_read( kt, 1, sf_dyn )      !=  read data  ==!
      IF(lwp) WRITE(numout,*) "Back from calling fld_read from dyn_dta"; FLUSH(numout)
      !
      ssh(:,:, Kaa)        = sf_dyn(jf_ssh)%fnow(:,:,1) * tmask(:,:,1)   ! SSH
      uu(:,:,:,Kmm)        = sf_dyn(jf_uu)%fnow(:,:,:) * umask(:,:,:)    ! effective u-transport
      vv(:,:,:,Kmm)        = sf_dyn(jf_vv)%fnow(:,:,:) * vmask(:,:,:)    ! effective v-transport
      uslp(:,:,:)          = sf_dyn(jf_uslp)%fnow(:,:,:) * umask(:,:,:)   ! U isopycnal slope
      vslp(:,:,:)          = sf_dyn(jf_vslp)%fnow(:,:,:) * vmask(:,:,:)   ! V isopycnal slope
      wslpi(:,:,:)         = sf_dyn(jf_wslpi)%fnow(:,:,:) * wmask(:,:,:)   ! W isopycnal slope (i-direction)
      wslpj(:,:,:)         = sf_dyn(jf_wslpj)%fnow(:,:,:) * wmask(:,:,:)   ! W isopycnal slope (j-direction)
      avt(:,:,:)           = sf_dyn(jf_avt)%fnow(:,:,:) * wmask(:,:,:)   ! vertical diffusivity (temperature)
      avs(:,:,:)           = sf_dyn(jf_avs)%fnow(:,:,:) * wmask(:,:,:)   ! vertical diffusivity (salinity)
      DO jk = 1, jpkm1                             ! deeper value = surface value + mask for all levels
         aeiu(:,:,jk)        = sf_dyn(jf_aeiu)%fnow(:,:,1) * umask(:,:,jk)   ! eddy-induced diffusivity (i-direction)
         aeiv(:,:,jk)        = sf_dyn(jf_aeiv)%fnow(:,:,1) * vmask(:,:,jk)   ! eddy-induced diffusivity (j-direction)
      END DO
      !
      IF(sn_cfctl%l_prtctl) THEN                 ! print control
         CALL prt_ctl(tab3d_1=uu(:,:,:,Kmm)               , clinfo1=' uu(:,:,:,Kmm)      - : ', mask1=umask )
         CALL prt_ctl(tab3d_1=vv(:,:,:,Kmm)               , clinfo1=' vv(:,:,:,Kmm)      - : ', mask1=vmask )
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop( 'dyn_dta')
      !
   END SUBROUTINE dyn_dta

   SUBROUTINE dyn_dta_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE dyn_dta_init  ***
      !!
      !! ** Purpose :   Initialisation of the dynamical data     
      !! ** Method  : - read the data namdyn_dta namelist
      !!----------------------------------------------------------------------
      INTEGER  :: ierr, ierr0, ierr1                 ! return error code
      INTEGER  :: ifpr                               ! dummy loop indice
      INTEGER  :: idv, idimv                         ! local integer
      INTEGER  :: ios                                ! Local integer output status for namelist read
      !!
      CHARACTER(len=100)            ::  cn_dir        !   Root directory for location of core files
      TYPE(FLD_N), DIMENSION(jpfld) ::  slf_d         ! array of namelist informations on the fields to read
      TYPE(FLD_N) :: sn_ssh, sn_uu, sn_vv                  ! informations about the fields to be read
      TYPE(FLD_N) :: sn_uslp, sn_vslp, sn_wslpi, sn_wslpj  ! informations about the fields to be read
      TYPE(FLD_N) :: sn_avt, sn_avs, sn_aeiu, sn_aeiv      ! informations about the fields to be read
      !!
      NAMELIST/namdyn_dta/cn_dir,  &
           &                sn_ssh, sn_uu, sn_vv, &
           &                sn_uslp, sn_vslp, sn_wslpi, sn_wslpj, &
           &                sn_avt, sn_avs, sn_aeiu, sn_aeiv
      !!----------------------------------------------------------------------
      !
      READ  ( numnam_ref, namdyn_dta, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namdyn_dta in reference namelist' )
      READ  ( numnam_cfg, namdyn_dta, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'namdyn_dta in configuration namelist' )
      IF(lwm) WRITE ( numond, namdyn_dta )
      !                                         ! store namelist information in an array
      ! 
      jf_ssh  = 1      ;   jf_uu    = 2      ;   jf_vv    = 3  
      jf_uslp = 4      ;   jf_vslp  = 5      ;   jf_wslpi = 6      ;   jf_wslpj = 7  
      jf_avt  = 8      ;   jf_avs   = 9      ;   jf_aeiu  = 10     ;   jf_aeiv = 11   
      !
      slf_d(jf_ssh)   = sn_ssh     ;   slf_d(jf_uu)   = sn_uu     ;   slf_d(jf_vv)    = sn_vv  
      slf_d(jf_uslp)  = sn_uslp    ;   slf_d(jf_vslp) = sn_vslp   ;   slf_d(jf_wslpi) = sn_wslpi  ;   slf_d(jf_wslpj) = sn_wslpj  
      slf_d(jf_avt)   = sn_avt     ;   slf_d(jf_avs)  = sn_avs    ;   slf_d(jf_aeiu)= sn_aeiu     ;   slf_d(jf_aeiv)  = sn_aeiv
      !

      ALLOCATE( sf_dyn(jpfld), STAT=ierr )         ! set sf structure
      IF( ierr > 0 )  THEN
         CALL ctl_stop( 'dyn_dta: unable to allocate sf structure' )   ;   RETURN
      ENDIF
      !                                         ! fill sf with slf_i and control print
      CALL fld_fill( sf_dyn, slf_d, cn_dir, 'dyn_dta_init', 'Data in file', 'namdyn_dta' )
      sf_dyn(jf_uu)%cltype = 'U'   ;   sf_dyn(jf_uu)%zsgn = -1._wp  
      sf_dyn(jf_vv)%cltype = 'V'   ;   sf_dyn(jf_vv)%zsgn = -1._wp  
      !
      ! Open file for each variable to get his number of dimension
      DO ifpr = 1, jpfld
         CALL fld_def( sf_dyn(ifpr) )
         CALL iom_open( sf_dyn(ifpr)%clname, sf_dyn(ifpr)%num )
         idv   = iom_varid( sf_dyn(ifpr)%num , slf_d(ifpr)%clvar )        ! id of the variable sdjf%clvar
         idimv = iom_file ( sf_dyn(ifpr)%num )%ndims(idv)                 ! number of dimension for variable sdjf%clvar
         CALL iom_close( sf_dyn(ifpr)%num )                               ! close file if already open
         ierr1=0
         IF( idimv == 3 ) THEN    ! 2D variable
            IF(lwp) WRITE(numout,*) "Allocating 2D space for ",TRIM(sf_dyn(ifpr)%clvar)
                                      ALLOCATE( sf_dyn(ifpr)%fnow(jpi,jpj,1)    , STAT=ierr0 )
            IF( slf_d(ifpr)%ln_tint ) ALLOCATE( sf_dyn(ifpr)%fdta(jpi,jpj,1,2)  , STAT=ierr1 )
         ELSE                     ! 3D variable
            IF(lwp) WRITE(numout,*) "Allocating 3D space for ",TRIM(sf_dyn(ifpr)%clvar)
                                      ALLOCATE( sf_dyn(ifpr)%fnow(jpi,jpj,jpk)  , STAT=ierr0 )
            IF( slf_d(ifpr)%ln_tint ) ALLOCATE( sf_dyn(ifpr)%fdta(jpi,jpj,jpk,2), STAT=ierr1 )
         ENDIF
         IF( ierr0 + ierr1 > 0 ) THEN
            CALL ctl_stop( 'dyn_dta_init : unable to allocate sf_dyn array structure' )   ;   RETURN
         ENDIF
      END DO
      !
   END SUBROUTINE dyn_dta_init

   !!======================================================================
END MODULE dyndta
