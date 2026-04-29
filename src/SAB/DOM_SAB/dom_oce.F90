MODULE dom_oce
   !!======================================================================
   !!                       ***  MODULE dom_oce  ***
   !! ** Purpose :   Define in memory all the ocean space domain variables
   !!======================================================================
   !! History :  1.0  ! 2005-10  (A. Beckmann, G. Madec)  reactivate s-coordinate
   !!            3.3  ! 2010-11  (G. Madec) add mbk. arrays associated to the deepest ocean level
   !!            3.4  ! 2011-01  (A. R. Porter, STFC Daresbury) dynamical allocation
   !!            3.5  ! 2012     (S. Mocavero, I. Epicoco) Add arrays associated
   !!                             to the optimization of BDY communications
   !!            3.7  ! 2015-11  (G. Madec) introduce surface and scale factor ratio
   !!             -   ! 2015-11  (G. Madec, A. Coward)  time varying zgr by default
   !!            4.1  ! 2019-08  (A. Coward, D. Storkey) rename prognostic variables in preparation for new time scheme.
   !!            4.x  ! 2020-02  (G. Madec, S. Techene) introduce ssh to h0 ratio
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   Agrif_Root    : dummy function used when lk_agrif=F
   !!   Agrif_Fixed   : dummy function used when lk_agrif=F
   !!   Agrif_CFixed  : dummy function used when lk_agrif=F
   !!   dom_oce_alloc : dynamical allocation of dom_oce arrays
   !!----------------------------------------------------------------------
   USE par_oce        ! ocean parameters

   IMPLICIT NONE
   PUBLIC             ! allows the acces to par_oce when dom_oce is used (exception to coding rules)

   PUBLIC dom_oce_alloc  ! Called by sabgcm.F90

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! time & space domain namelist
   !! ----------------------------
   !                                   !!* Namelist namdom : time & space domain *
   LOGICAL , PUBLIC ::   ln_linssh      !: =T  linear free surface ==>> model level are fixed in time
   LOGICAL , PUBLIC ::   ln_meshmask    !: =T  create a mesh-mask file (mesh_mask.nc)
   REAL(wp), PUBLIC ::   rn_Dt          !: time step for the dynamics and tracer
   REAL(wp), PUBLIC ::   rn_atfp        !: asselin time filter parameter
   LOGICAL , PUBLIC ::   ln_crs         !: Apply grid coarsening to dynamical model output or online passive tracers
   LOGICAL , PUBLIC ::   ln_c1d         !: =T  single column domain (1x1 pt)

   !! Free surface parameters
   !! =======================
   LOGICAL , PUBLIC :: ln_dynspg_exp    !: Explicit free surface flag
   LOGICAL , PUBLIC :: ln_dynspg_ts     !: Split-Explicit free surface flag

   !! Time splitting parameters
   !! =========================
   LOGICAL,  PUBLIC :: ln_bt_fw         !: Forward integration of barotropic sub-stepping
   LOGICAL,  PUBLIC :: ln_bt_av         !: Time averaging of barotropic variables
   LOGICAL,  PUBLIC :: ln_bt_auto       !: Set number of barotropic iterations automatically
   INTEGER,  PUBLIC :: nn_bt_flt        !: Filter choice
   INTEGER,  PUBLIC :: nn_e          !: Number of barotropic iterations during one baroclinic step (rn_Dt)
   REAL(wp), PUBLIC :: rn_bt_cmax       !: Maximum allowed courant number (used if ln_bt_auto=T)
   REAL(wp), PUBLIC :: rn_bt_alpha      !: Time stepping diffusion parameter


   !                                   !!! associated variables
   REAL(wp), PUBLIC ::   rDt, r1_Dt     !: Current model timestep and reciprocal

   !!----------------------------------------------------------------------
   !! space domain parameters
   !!----------------------------------------------------------------------
   LOGICAL         , PUBLIC ::   l_Iperio, l_Jperio   ! i- j-periodicity
   LOGICAL         , PUBLIC ::   l_NFold              ! North Pole folding
   CHARACTER(len=1), PUBLIC ::   c_NFtype             ! type of North pole Folding: T or F point

   ! Tiling namelist
   LOGICAL, PUBLIC ::   ln_tile
   INTEGER         ::   nn_ltile_i, nn_ltile_j

   ! Domain tiling
   INTEGER, PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   ntsi_a       !: start of internal part of tile domain
   INTEGER, PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   ntsj_a       !
   INTEGER, PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   ntei_a       !: end of internal part of tile domain
   INTEGER, PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   ntej_a       !
   LOGICAL, PUBLIC                                  ::   l_istiled    ! whether tiling is currently active or not

   !                             !: domain MPP decomposition parameters
   INTEGER              , PUBLIC ::   nimpp, njmpp     !: i- & j-indexes for mpp-subdomain left bottom
   INTEGER              , PUBLIC ::   narea            !: number for local area (starting at 1) = MPI rank + 1
   INTEGER              , PUBLIC ::   nidom      !: IOIPSL things...
   INTEGER              , PUBLIC ::   nimpi, njmpi     !: i and j position in the MPI domain decomposition

   INTEGER, PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   mig        !: local ==> global domain, including halos (jpiglo), i-index
   INTEGER, PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   mjg        !: local ==> global domain, including halos (jpjglo), j-index
   INTEGER, PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   mig0       !: local ==> global domain, excluding halos (Ni0glo), i-index
   INTEGER, PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   mjg0       !: local ==> global domain, excluding halos (Nj0glo), j-index
   INTEGER, PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   mi0, mi1   !: global, including halos (jpiglo) ==> local domain i-index
   !                                                                !:    (mi0=1 and mi1=0 if global index not in local domain)
   INTEGER, PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   mj0, mj1   !: global, including halos (jpjglo) ==> local domain j-index
   !                                                                !:    (mj0=1 and mj1=0 if global index not in local domain)
   INTEGER, PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   nfimpp, nfproc, nfjpi, nfni_0


      !                    !==  1D reference coordinate  ==!   used in zco and zps cases (z- or z-partial cell coord.)
   !
   REAL(wp), PUBLIC, ALLOCATABLE, DIMENSION(:)     ::   e3t_1d, e3w_1d   !: reference depth & scale factor of W-level points [m]
   REAL(wp), PUBLIC, ALLOCATABLE, DIMENSION(:)     ::   gdepw_1d, gdept_1d  !: reference depth of T and W-level points [m]


   !!----------------------------------------------------------------------
   !! horizontal curvilinear coordinate and scale factors
   !! ---------------------------------------------------------------------
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE        , DIMENSION(:,:) ::   glamt , gphit    !: longitude and latitude (resp) at t-points [degree]
   
   REAL(wp), PUBLIC, ALLOCATABLE,         DIMENSION(:,:) ::   glamu, glamv , glamf    !: longitude at t, u, v, f-points [degree]
   REAL(wp), PUBLIC, ALLOCATABLE,         DIMENSION(:,:) ::   gphiu, gphiv , gphif    !: latitude  at t, u, v, f-points [degree]
   
   REAL(dp), PUBLIC, ALLOCATABLE, SAVE, TARGET, DIMENSION(:,:)  :: e1t, e2t!: t-point horizontal scale factors    [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, TARGET, DIMENSION(:,:)  :: e2u !: horizontal scale factors at u-point [m]
   REAL(dp), PUBLIC, ALLOCATABLE, SAVE, TARGET, DIMENSION(:,:)  :: e1u !: horizontal scale factors at u-point [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, TARGET, DIMENSION(:,:)  :: e1v !: horizontal scale factors at v-point [m]
   REAL(dp), PUBLIC, ALLOCATABLE, SAVE, TARGET, DIMENSION(:,:)  :: e2v !: horizontal scale factors at v-point [m]
   REAL(dp), PUBLIC, ALLOCATABLE, SAVE, TARGET, DIMENSION(:,:)  :: e1f, e2f!: horizontal scale factors at f-point [m]
   !
   REAL(dp), PUBLIC, ALLOCATABLE, SAVE        , DIMENSION(:,:) ::   e1e2t , r1_e1e2t                !: associated metrics at t-point
   REAL(dp), PUBLIC, ALLOCATABLE, SAVE        , DIMENSION(:,:) ::   e1e2u, e1e2v, e1e2f             !: associated metrics at u,v,f-point, needed only for iom_init not to fail

   REAL(wp), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:)   ::   r3t
   !
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   ff_f, ff_t         !: Coriolis factor at f- & t-points  [1/s]
   !!----------------------------------------------------------------------
   !! vertical curvilinear coordinate and scale factors
   !! ---------------------------------------------------------------------
   !
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE        , DIMENSION(:,:,:) ::   e3t_3d              !: associated metrics at T-point
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE        , DIMENSION(:,:,:,:) ::   e3t              !: associated metrics at T-point

   !!----------------------------------------------------------------------
   !! vertical coordinate and scale factors
   !! ---------------------------------------------------------------------
   LOGICAL, PUBLIC ::   l_zco       !: z-coordinate - full step
   LOGICAL, PUBLIC ::   l_zps       !: z-coordinate - partial step
   LOGICAL, PUBLIC ::   l_sco       !: s-coordinate or hybrid z-s coordinate
   LOGICAL, PUBLIC ::   ln_isfcav    !: presence of ISF
   !                                                        ! time-dependent heights of ocean water column   (domvvl)
   INTEGER, PUBLIC ::   nla10              !: deepest    W level Above  ~10m (nlb10 - 1)
   INTEGER, PUBLIC ::   nlb10              !: shallowest W level Bellow ~10m (nla10 + 1)

   !!----------------------------------------------------------------------
   !! masks, top and bottom ocean point position
   !! ---------------------------------------------------------------------
   REAL(wp), PUBLIC, ALLOCATABLE, DIMENSION(:,:)           ::   smask0                              !: surface mask at T-pts on inner domain
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   tmask_i, smask0_i                  !: interior (excluding halos+duplicated points) domain T-point mask
   REAL(wp), PUBLIC, ALLOCATABLE, DIMENSION(:,:)           ::   ssmask                         !: surface mask at T-pt
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:), TARGET ::   tmask, umask, vmask,fmask    !: land/ocean mask at T-, U-, V-, W- and F-pts
   INTEGER, PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   mbkt                  !: interior (excluding halos+duplicated points) domain T-point mask

   !!----------------------------------------------------------------------
   !! calendar variables
   !! ---------------------------------------------------------------------
   INTEGER , PUBLIC ::   nyear         !: current year
   INTEGER , PUBLIC ::   nmonth        !: current month
   INTEGER , PUBLIC ::   nday          !: current day of the month
   INTEGER , PUBLIC ::   nhour         !: current hour
   INTEGER , PUBLIC ::   nhour0        !: time of day at the start of the run: hour
   INTEGER , PUBLIC ::   nminute0      !:    and minute
   INTEGER , PUBLIC ::   nminute       !: current minute
   INTEGER , PUBLIC ::   ndastp        !: time step date in yyyymmdd format
   INTEGER , PUBLIC ::   nday_year     !: current day counted from jan 1st of the current year
   INTEGER , PUBLIC ::   nsec_year     !: seconds between 00h jan 1st of the current  year and half of the current time step
   INTEGER , PUBLIC ::   nsec_month    !: seconds between 00h 1st day of the current month and half of the current time step
   INTEGER , PUBLIC ::   nsec_monday   !: seconds between 00h         of the last Monday   and half of the current time step
   INTEGER , PUBLIC ::   nsec_day      !: seconds between 00h         of the current   day and half of the current time step
   REAL(dp), PUBLIC ::   fjulday = 0.      !: current julian day
   REAL(dp), PUBLIC ::   fjulstartyear !: first day of the current year in julian days
   REAL(wp), PUBLIC ::   adatrj        !: number of elapsed days since the begining of the whole simulation
   !                                   !: (cumulative duration of previous runs that may have used different time-step size)
   INTEGER , PUBLIC, DIMENSION(  0: 2) ::   nyear_len     !: length in days of the previous/current/next year
   INTEGER , PUBLIC, DIMENSION(-11:25) ::   nmonth_len    !: length in days of the months of the current year
   INTEGER , PUBLIC, DIMENSION(-11:25) ::   nmonth_beg    !: second since Jan 1st 0h of the current year and the half of the months
   INTEGER , PUBLIC                  ::   nsec1jan000     !: second since Jan 1st 0h of nit000 year and Jan 1st 0h the current year
   INTEGER , PUBLIC                  ::   nsec000_1jan000   !: second since Jan 1st 0h of nit000 year and nit000
   INTEGER , PUBLIC                  ::   nsecend_1jan000   !: second since Jan 1st 0h of nit000 year and nitend

   !!----------------------------------------------------------------------
   !! variable defined here to avoid circular dependencies...
   !! ---------------------------------------------------------------------
   INTEGER, PUBLIC ::   nbasin         ! number of basin to be considered in diaprt (glo, atl, pac, ind, ipc)

   !!----------------------------------------------------------------------
   !! agrif domain
   !!----------------------------------------------------------------------
   LOGICAL, PUBLIC, PARAMETER ::   lk_agrif = .FALSE.   !: agrif flag

   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   LOGICAL FUNCTION Agrif_Root()
      Agrif_Root = .TRUE.
   END FUNCTION Agrif_Root

   INTEGER FUNCTION Agrif_Fixed()
      Agrif_Fixed = 0
   END FUNCTION Agrif_Fixed

   CHARACTER(len=3) FUNCTION Agrif_CFixed()
      Agrif_CFixed = '0'
   END FUNCTION Agrif_CFixed

   INTEGER FUNCTION dom_oce_alloc()
      !!----------------------------------------------------------------------
      INTEGER                ::   ii
      INTEGER, DIMENSION(30) :: ierr
      !!----------------------------------------------------------------------
      ii = 0   ;   ierr(:) = 0
      !
      ii = ii+1
      ALLOCATE( gdept_1d(jpk), gdepw_1d(jpk), e3t_1d(jpk) ,e3w_1d(jpk), STAT=ierr(ii) ) 
      
      ii = ii +1
      ALLOCATE(glamu(jpi,jpj) ,  glamv(jpi,jpj) ,  glamf(jpi,jpj) ,     &
         &     gphiu(jpi,jpj) ,  gphiv(jpi,jpj) ,  gphif(jpi,jpj)     , STAT=ierr(ii) )

      ii = ii +1            
      ALLOCATE( glamt(jpi,jpj) , gphit(jpi,jpj)    ,     &
         &       e1t (jpi,jpj) ,     e2t (jpi,jpj) ,     &
         &       e1u (jpi,jpj) ,     e2u (jpi,jpj) ,     &
         &       e1v (jpi,jpj) ,     e2v (jpi,jpj) ,     &
         &       e1f (jpi,jpj) ,     e2f (jpi,jpj) ,     &
         &      e1e2t(jpi,jpj) , r1_e1e2t(jpi,jpj) , e1e2u(jpi,jpj), e1e2v(jpi,jpj), e1e2f(jpi,jpj),    &
         &      ff_f (jpi,jpj) , ff_t (jpi,jpj),     mbkt(jpi,jpj)                     , STAT=ierr(ii) )
      !
      ii = ii+1 
      ALLOCATE( r3t(jpi,jpj,jpt), STAT=ierr(ii) ) 
      !
      ii = ii+1
      ALLOCATE( tmask(jpi,jpj,jpk) , umask(jpi,jpj,jpk), vmask(jpi,jpj,jpk) , &
         &      fmask(jpi,jpj,jpk) , STAT=ierr(ii) )
      ii = ii+1
      !
      ALLOCATE( tmask_i(jpi,jpj), smask0(A2D(0)), smask0_i(A2D(0)), ssmask(jpi,jpj), STAT=ierr(ii))
      ii = ii+1
      !
      ALLOCATE( e3t_3d(jpi,jpj,jpk), e3t(jpi,jpj,jpk,jpt), STAT=ierr(ii) ) ! futher optimisation can be done here : if ln_M2016 = .false., no need to allocate e3t_3d and e3t 
      !
      dom_oce_alloc = MAXVAL(ierr)
      !
   END FUNCTION dom_oce_alloc

   !!======================================================================
END MODULE dom_oce
