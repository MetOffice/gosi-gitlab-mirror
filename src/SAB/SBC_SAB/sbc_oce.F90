MODULE sbc_oce
   !!======================================================================
   !!                       ***  MODULE  sbc_oce  ***
   !! Surface module :   variables defined in core memory
   !!======================================================================
   !! History :  3.0  ! 2006-06  (G. Madec)  Original code
   !!             -   ! 2008-08  (G. Madec)  namsbc moved from sbcmod
   !!            3.3  ! 2010-04  (M. Leclair, G. Madec)  Forcing averaged over 2 time steps
   !!             -   ! 2010-11  (G. Madec) ice-ocean stress always computed at each ocean time-step
   !!            3.3  ! 2010-10  (J. Chanut, C. Bricaud)  add the surface pressure forcing
   !!            4.0  ! 2012-05  (C. Rousset) add attenuation coef for use in ice model
   !!            4.0  ! 2016-06  (L. Brodeau) new unified bulk routine (based on AeroBulk)
   !!            4.0  ! 2019-03  (F. Lemarié, G. Samson) add compatibility with ABL mode
   !!            4.2  ! 2020-12  (G. Madec, E. Clementi) modified wave parameters in namelist
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   sbc_oce_alloc : allocation of sbc arrays
   !!----------------------------------------------------------------------
   USE par_oce        ! ocean parameters
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   sbc_oce_alloc   ! routine called in sbcmod.F90
   PUBLIC   sbc_oce_dealloc ! routine called in nemogcm.F90

   !!----------------------------------------------------------------------
   !!           Namelist for the Ocean Surface Boundary Condition
   !!----------------------------------------------------------------------
   !                                   !!* namsbc namelist *
   LOGICAL , PUBLIC ::   ln_usr         !: user defined formulation
   LOGICAL , PUBLIC ::   ln_flx         !: flux      formulation
   LOGICAL , PUBLIC ::   ln_blk         !: bulk formulation
   LOGICAL , PUBLIC ::   ln_abl         !: Atmospheric boundary layer model
   LOGICAL , PUBLIC ::   ln_wave        !: wave in the system (forced or coupled)
   LOGICAL , PUBLIC ::   ln_cpl         !: ocean-atmosphere coupled formulation
   LOGICAL , PUBLIC ::   ln_mixcpl      !: ocean-atmosphere forced-coupled mixed formulation
   LOGICAL , PUBLIC ::   ln_dm2dc       !: Daily mean to Diurnal Cycle short wave (qsr)
   LOGICAL , PUBLIC ::   ln_rnf         !: runoffs / runoff mouths
   LOGICAL , PUBLIC ::   ln_ssr         !: Sea Surface restoring on SST and/or SSS
   LOGICAL , PUBLIC ::   ln_apr_dyn     !: Atmospheric pressure forcing used on dynamics (ocean & ice)
   INTEGER , PUBLIC ::   nn_ice         !: flag for ice in the surface boundary condition (=0/1/2/3)
   LOGICAL , PUBLIC ::   ln_ice_embd    !: flag for levitating/embedding sea-ice in the ocean
   !                                             !: =F levitating ice (no presure effect) with mass and salt exchanges
   !                                             !: =T embedded sea-ice (pressure effect + mass and salt exchanges)
   INTEGER , PUBLIC ::   nn_fsbc = 1    !  nn_fsbc is transparent in SAB, not a parameter, it exists just for icb_stp not to crash.    
   INTEGER , PUBLIC ::   nn_components = 3  !: flag for sbc module (including sea-ice) coupling mode (see component definition below)
   !!----------------------------------------------------------------------
   !! time level indices
   !!----------------------------------------------------------------------
   INTEGER, PUBLIC :: Nbb, Nnn, Naa, Nrhs    !! used by sab_init, AND sab_cpl !! 

   INTEGER , PUBLIC ::   nn_fwb         !: FreshWater Budget:
   !                                             !:  = 0 unchecked
   !                                             !:  = 1 global mean of e-p-r set to zero at each nn_fsbc time step
   !                                             !:  = 2 annual global mean of e-p-r set to zero
   LOGICAL , PUBLIC ::   ln_icebergs    !: Icebergs
   LOGICAL , PUBLIC ::   ln_berg_cpl    !: use coupling with SAB to compute icebergs outside of NEMO.
   LOGICAL , PUBLIC ::   ln_cpl_asynchrone  !: In case of coupling with SAB, use synchrone reception of berg fluxes or not
                                                  !: .true.  : for reproducibility check, comparisons with reference NEMO 5
                                                  !: .false. : for performance runs, reception of berg fluxes is desynchronized.
   LOGICAL , PUBLIC ::   ln_cpl_nlvlcut  !: In case of coupling with SAB, with ln_M2016 = .true. , reduces the number of vertical levels to be sent though OASIS 
   INTEGER , PUBLIC ::   nn_lvlcut_cpl  !: if ln_cpl_nlvlcut : enables user to set a given number of vertical levels to be sent
   INTEGER , PUBLIC ::   nlvlsab_cpl   ! number of vertical levels effectively used in NEMO-SAB coupling, after processing parameters in icbini

   !
   INTEGER , PUBLIC ::   nn_lsm         !: Number of iteration if seaoverland is applied
   !
   !                                   !!* namsbc_cpl namelist *
   INTEGER , PUBLIC ::   nn_cats_cpl    !: Number of sea ice categories over which the coupling is carried out
   !
   !                                   !!* namsbc_wave namelist *
   LOGICAL , PUBLIC ::   ln_sdw         !: =T 3d stokes drift from wave model
   LOGICAL , PUBLIC ::   ln_stcor       !: =T if Stokes-Coriolis and tracer advection terms are used
   LOGICAL , PUBLIC ::   ln_cdgw        !: =T neutral drag coefficient from wave model
   LOGICAL , PUBLIC ::   ln_tauoc       !: =T if normalized stress from wave is used
   LOGICAL , PUBLIC ::   ln_wave_test   !: =T wave test case (constant Stokes drift)
   LOGICAL , PUBLIC ::   ln_charn       !: =T Chranock coefficient from wave model
   LOGICAL , PUBLIC ::   ln_taw         !: =T wind stress corrected by wave intake
   LOGICAL , PUBLIC ::   ln_phioc       !: =T TKE surface BC from wave model
   LOGICAL , PUBLIC ::   ln_bern_srfc   !: Bernoulli head, waves' inuced pressure
   LOGICAL , PUBLIC ::   ln_breivikFV_2016 !: Breivik 2016 profile
   LOGICAL , PUBLIC ::   ln_vortex_force !: vortex force activation
   LOGICAL , PUBLIC ::   ln_stshear     !: Stoked Drift shear contribution in zdftke
   !
   !!----------------------------------------------------------------------
   !!           switch definition (improve readability)
   !!----------------------------------------------------------------------
   INTEGER , PUBLIC, PARAMETER ::   jp_usr     = 1        !: user defined                  formulation
   INTEGER , PUBLIC, PARAMETER ::   jp_flx     = 2        !: flux                          formulation
   INTEGER , PUBLIC, PARAMETER ::   jp_blk     = 3        !: bulk                          formulation
   INTEGER , PUBLIC, PARAMETER ::   jp_abl     = 4        !: Atmospheric boundary layer    formulation
   INTEGER , PUBLIC, PARAMETER ::   jp_purecpl = 5        !: Pure ocean-atmosphere Coupled formulation
   INTEGER , PUBLIC, PARAMETER ::   jp_none    = 6        !: for OCE when doing coupling via SAS module
   !
   !!----------------------------------------------------------------------
   !!           component definition
   !!----------------------------------------------------------------------
   INTEGER , PUBLIC, PARAMETER ::   jp_iam_nemo = 0      !: Initial single executable configuration
   !  (no internal OASIS coupling)
   INTEGER , PUBLIC, PARAMETER ::   jp_iam_oce  = 1      !: Multi executable configuration - OCE component
   !  (internal OASIS coupling)
   INTEGER , PUBLIC, PARAMETER ::   jp_iam_sas  = 2      !: Multi executable configuration - SAS component
   !  (internal OASIS coupling)
   INTEGER , PUBLIC, PARAMETER ::   jp_iam_icb  = 3      !: Multi executable configuration - SAS component

   !!----------------------------------------------------------------------
   !!              Ocean Surface Boundary Condition fields
   !!----------------------------------------------------------------------
   INTEGER , PUBLIC ::  ncpl_qsr_freq = 0        !: qsr coupling frequency per days from atmosphere (used by top)
   !
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   utau_icb, vtau_icb !: sea surface (i,j)-stress used by icebergs     [N/m2]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   emp    , qns     !: freshwater budget: volume flux                [Kg/m2/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   fr_i               !: ice fraction = 1 - lead fraction       (between 0 to 1)
   !
   !!---------------------------------------------------------------------
   !! ABL Vertical Domain size
   !!---------------------------------------------------------------------
   INTEGER , PUBLIC            ::   jpka   = 2     !: ABL number of vertical levels (default definition)
   INTEGER , PUBLIC            ::   jpkam1 = 1     !: jpka-1
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)   ::   ght_abl, ghw_abl          !: ABL geopotential height (needed for iom)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)   ::   e3t_abl, e3w_abl          !: ABL vertical scale factors (needed for iom)

   !!----------------------------------------------------------------------
   !!                     Sea Surface Mean fields
   !!----------------------------------------------------------------------
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   ssu_m     !: mean (nn_fsbc time-step) surface sea i-current (U-point) [m/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   ssv_m     !: mean (nn_fsbc time-step) surface sea j-current (V-point) [m/s]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   sst_m     !: mean (nn_fsbc time-step) surface sea temperature     [Celsius]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   sss_m     !: mean (nn_fsbc time-step) surface sea salinity            [psu]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   ssh_m     !: mean (nn_fsbc time-step) sea surface height                [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   e3t_m     !: mean (nn_fsbc time-step) sea surface layer thickness       [m]

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION sbc_oce_alloc()
      !!---------------------------------------------------------------------
      !!                  ***  FUNCTION sbc_oce_alloc  ***
      !!---------------------------------------------------------------------
      INTEGER :: ierr(2)
      !!---------------------------------------------------------------------
      ierr(:) = 0
      !
      ! ----------------- !
      ! == FULL ARRAYS == !
      ! ----------------- !
      !
      ALLOCATE( emp(jpi,jpj) , qns(jpi,jpj) ,  &
         &      STAT=ierr(1) )
      !
      ALLOCATE( fr_i(jpi,jpj) ,     &
         &      ssu_m  (jpi,jpj) , sst_m(jpi,jpj) ,      &
         &      ssv_m  (jpi,jpj) , sss_m(jpi,jpj) , ssh_m(jpi,jpj) , e3t_m(jpi,jpj) , STAT=ierr(2) )
      !
      sbc_oce_alloc = MAXVAL( ierr )
      CALL mpp_sum ( 'sbc_oce', sbc_oce_alloc )
      IF( sbc_oce_alloc > 0 )   CALL ctl_warn('sbc_oce_alloc: allocation of arrays failed')
      !
   END FUNCTION sbc_oce_alloc


   SUBROUTINE sbc_oce_dealloc()
      IF( ALLOCATED(emp) ) THEN
         DEALLOCATE( emp  , qns)
         DEALLOCATE( fr_i , ssu_m , sst_m , ssv_m , sss_m, ssh_m, e3t_m )
      ENDIF
   END SUBROUTINE sbc_oce_dealloc
   
END MODULE sbc_oce
