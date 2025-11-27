MODULE icethd_zdf_BL99
   !!======================================================================
   !!                       ***  MODULE icethd_zdf_BL99 ***
   !!   sea-ice: vertical heat diffusion in sea ice (computation of temperatures)
   !!======================================================================
   !! History :       !  2003-02  (M. Vancoppenolle) original 1D code
   !!                 !  2005-06  (M. Vancoppenolle) 3d version
   !!            4.0  !  2018     (many people)      SI3 [aka Sea Ice cube]
   !!----------------------------------------------------------------------
#if defined key_si3
   !!----------------------------------------------------------------------
   !!   'key_si3'                                       SI3 sea-ice model
   !!----------------------------------------------------------------------
   !!  ice_thd_zdf_BL99 : vertical diffusion computation
   !!----------------------------------------------------------------------
   USE par_oce
   USE par_ice        ! SI3 parameters
   USE par_kind, ONLY : wp
   USE phycst  
   USE ice
   USE sbc_ice , ONLY : qns_ice, dqns_ice, qcn_ice, qtr_ice_top, qsr_ice
   USE icevar  , ONLY : ice_var_snwfra, ice_var_enthalpy

   IMPLICIT NONE
   PRIVATE

   PUBLIC   ice_thd_zdf_BL99   ! called by icethd_zdf

   !! * Substitutions
#  include "do_loop_substitute.h90"

   !!----------------------------------------------------------------------
   !! NEMO/ICE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE ice_thd_zdf_BL99( jl_cat, k_cnd )
      !!-------------------------------------------------------------------
      !!                ***  ROUTINE ice_thd_zdf_BL99  ***
      !!
      !! ** Purpose : computes the time evolution of snow and sea-ice temperature
      !!              profiles, using the original Bitz and Lipscomb (1999) algorithm
      !!
      !! ** Method  : solves the heat equation diffusion with a Neumann boundary
      !!              condition at the surface and a Dirichlet one at the bottom.
      !!              Solar radiation is partially absorbed into the ice.
      !!              The specific heat and thermal conductivities depend on ice
      !!              salinity and temperature to take into account brine pocket
      !!              melting. The numerical scheme is an iterative Crank-Nicolson
      !!              on a non-uniform multilayer grid in the ice and snow system.
      !!
      !!           The successive steps of this routine are
      !!           1.  initialization of ice-snow layers thicknesses
      !!           2.  Internal absorbed and transmitted radiation
      !!           Then iterative procedure begins
      !!           3.  Thermal conductivity
      !!           4.  Kappa factors
      !!           5.  specific heat in the ice
      !!           6.  eta factors
      !!           7.  surface flux computation
      !!           8.  tridiagonal system terms
      !!           9.  solving the tridiagonal system with Gauss elimination
      !!           Iterative procedure ends according to a criterion on evolution
      !!           of temperature
      !!           10. Fluxes at the interfaces
      !!
      !! ** Inputs / Ouputs : (global commons)
      !!           surface temperature              : t_su
      !!           ice/snow temperatures            : t_i, t_s
      !!           ice salinities                   : sz_i
      !!           number of layers in the ice/snow : nlay_i, nlay_s
      !!           total ice/snow thickness         : h_i, h_s
      !!-------------------------------------------------------------------
      INTEGER, INTENT(in) ::   k_cnd     ! conduction flux (off, on, emulated)
      INTEGER, INTENT(in) ::   jl_cat
      !
      INTEGER ::   ji, jj, jk     ! spatial loop index
      INTEGER ::   jm             ! current reference number of equation
      INTEGER ::   iconv          ! number of iterations in iterative procedure
      INTEGER ::   iconv_max = 50 ! max number of iterations in iterative procedure
      !
      LOGICAL, DIMENSION(A2D(0)) ::   l_T_converged   ! true when T converges (per grid point)
      !
      REAL(wp) ::   zg1s      =  2._wp        ! for the tridiagonal system
      REAL(wp) ::   zg1       =  2._wp        !
      REAL(wp) ::   zgamma    =  18009._wp    ! for specific heat
      REAL(wp) ::   zbeta     =  0.117_wp     ! for thermal conductivity (could be 0.13)
      REAL(wp) ::   zkimin    =  0.10_wp      ! minimum ice thermal conductivity
      REAL(wp) ::   ztsu_err  =  1.e-5_wp     ! range around which t_su is considered at 0C
      REAL(wp) ::   zdti_bnd  =  1.e-4_wp     ! maximal authorized error on temperature
      REAL(wp) ::   zhs_ssl   =  0.03_wp      ! surface scattering layer in the snow
      REAL(wp) ::   zhi_ssl   =  0.10_wp      ! surface scattering layer in the ice
      REAL(wp) ::   zh_min    =  1.e-3_wp     ! minimum ice/snow thickness for conduction
      REAL(wp) ::   ztmelts                   ! ice melting temperature
      REAL(wp) ::   zdti_max                  ! current maximal error on temperature
      REAL(wp) ::   zcpi                      ! Ice specific heat
      REAL(wp) ::   zhfx_err, zdq             ! diag errors on heat
      REAL(wp) ::   zfac
      !
      REAL(wp), DIMENSION(A2D(0)) ::   zraext_s     ! extinction coefficient of radiation in the snow
      REAL(wp), DIMENSION(A2D(0)) ::   ztsub        ! surface temperature at previous iteration
      REAL(wp), DIMENSION(A2D(0)) ::   zh_i, z1_h_i ! ice layer thickness
      REAL(wp), DIMENSION(A2D(0)) ::   zh_s, z1_h_s ! snow layer thickness
      REAL(wp), DIMENSION(A2D(0)) ::   zqns_ice_b   ! solar radiation absorbed at the surface
      REAL(wp), DIMENSION(A2D(0)) ::   zdqns_ice_b  ! derivative of the surface flux function
      REAL(wp), DIMENSION(A2D(0),0:nlay_s) ::   zradtr_s    ! Radiation transmited through the snow
      REAL(wp), DIMENSION(A2D(0),0:nlay_s) ::   zradab_s    ! Radiation absorbed in the snow
      REAL(wp), DIMENSION(A2D(0),0:nlay_i) ::   zradtr_i    ! Radiation transmitted through the ice
      REAL(wp), DIMENSION(A2D(0),0:nlay_i) ::   zradab_i    ! Radiation absorbed in the ice
      REAL(wp), DIMENSION(A2D(0),0:nlay_i) ::   ztcond_i    ! Ice thermal conductivity
      REAL(wp), DIMENSION(A2D(0),0:nlay_i) ::   ztcond_i_cp ! copy
      REAL(wp), DIMENSION(A2D(0),nlay_i)   ::   ztiold      ! Old temperature in the ice
      REAL(wp), DIMENSION(A2D(0),nlay_s)   ::   ztsold      ! Old temperature in the snow
      REAL(wp), DIMENSION(A2D(0),0:nlay_i) ::   zkappa_i    ! Kappa factor in the ice
      REAL(wp), DIMENSION(A2D(0),0:nlay_i) ::   zeta_i      ! Eta factor in the ice
      REAL(wp), DIMENSION(A2D(0),0:nlay_s) ::   zkappa_s    ! Kappa factor in the snow
      REAL(wp), DIMENSION(A2D(0),0:nlay_s) ::   zeta_s      ! Eta factor in the snow
      REAL(wp), DIMENSION(A2D(0))          ::   zkappa_comb ! Combined snow and ice surface conductivity
      REAL(wp), DIMENSION(A2D(0))          ::   zq_ini      ! diag errors on heat
      REAL(wp), DIMENSION(A2D(0))          ::   zghe        ! G(he), th. conduct enhancement factor, mono-cat
      REAL(wp), DIMENSION(A2D(0))          ::   za_s_fra    ! ice fraction covered by snow
      REAL(wp), DIMENSION(A2D(0))          ::   isnow       ! snow presence (1) or not (0)
      REAL(wp), DIMENSION(A2D(0))          ::   isnow_comb  ! snow presence for met-office
      !
      INTEGER  ::   jm_min    ! reference number of top equation
      INTEGER  ::   jm_max    ! reference number of bottom equation
      REAL(wp) ::   zfnet     ! surface flux function
      REAL(wp), DIMENSION(nlay_i)   ::   ztib        ! Temporary temperature in the ice to check the convergence
      REAL(wp), DIMENSION(nlay_s)   ::   ztsb        ! Temporary temperature in the snow to check the convergence
      REAL(wp), DIMENSION(nlay_i+nlay_s+1)   ::   zindterm    ! 'Ind'ependent term
      REAL(wp), DIMENSION(nlay_i+nlay_s+1)   ::   zindtbis    ! Temporary 'ind'ependent term
      REAL(wp), DIMENSION(nlay_i+nlay_s+1)   ::   zdiagbis    ! Temporary 'dia'gonal term
      REAL(wp), DIMENSION(nlay_i+nlay_s+1,3) ::   ztrid       ! Tridiagonal system terms
      !
      ! Mono-category
      REAL(wp) ::   zepsilon   ! determines thres. above which computation of G(h) is done
      REAL(wp) ::   zhe        ! dummy factor
      REAL(wp) ::   zcnd_i     ! mean sea ice thermal conductivity
      !!------------------------------------------------------------------
      ! --- diag error on heat diffusion - PART 1 --- !
      DO_2D( 0, 0, 0, 0 )
         IF( l_ice_present(ji,jj) ) THEN
            zq_ini(ji,jj) = ( SUM( e_i(ji,jj,1:nlay_i,jl_cat) ) * h_i(ji,jj,jl_cat) * r1_nlay_i +  &
               &              SUM( e_s(ji,jj,1:nlay_s,jl_cat) ) * h_s(ji,jj,jl_cat) * r1_nlay_s )
         ENDIF
      END_2D

      ! calculate ice fraction covered by snow for radiation
      CALL ice_var_snwfra( h_s(A2D(0),jl_cat), za_s_fra(:,:) ) ! May need to apply mask in ice_var_snwfra, check...

      !------------------
      ! 1) Initialization
      !------------------
      !
      ! extinction radiation in the snow
      IF    ( nn_qtrice == 0 ) THEN   ! constant
         zraext_s(:,:) = rn_kappa_s
      ELSEIF( nn_qtrice == 1 ) THEN   ! depends on melting/freezing conditions
         WHERE( t_su(:,:,jl_cat) < rt0 )   ;   zraext_s(:,:) = rn_kappa_sdry   ! no surface melting
         ELSEWHERE                         ;   zraext_s(:,:) = rn_kappa_smlt   !    surface melting
         END WHERE
      ENDIF
      !
      ! thicknesses
      DO_2D( 0, 0, 0, 0 )
         IF( l_ice_present(ji,jj) ) THEN
            ! ice thickness
            IF( h_i(ji,jj,jl_cat) > 0._wp ) THEN
               zh_i  (ji,jj) = MAX( zh_min , h_i(ji,jj,jl_cat) ) * r1_nlay_i ! set a minimum thickness for conduction
               z1_h_i(ji,jj) = 1._wp / zh_i(ji,jj)                           !       it must be very small
            ELSE
               zh_i  (ji,jj) = 0._wp
               z1_h_i(ji,jj) = 0._wp
            ENDIF
            ! snow thickness
            IF( h_s(ji,jj,jl_cat) > 0._wp ) THEN
               zh_s  (ji,jj) = MAX( zh_min , h_s(ji,jj,jl_cat) ) * r1_nlay_s ! set a minimum thickness for conduction
               z1_h_s(ji,jj) = 1._wp / zh_s(ji,jj)                           !       it must be very small
               isnow (ji,jj) = 1._wp
            ELSE
               zh_s  (ji,jj) = 0._wp
               z1_h_s(ji,jj) = 0._wp
               isnow (ji,jj) = 0._wp
            ENDIF
            ! for Met-Office
            IF( h_s(ji,jj,jl_cat) < zh_min ) THEN
               isnow_comb(ji,jj) = h_s(ji,jj,jl_cat) / zh_min
            ELSE
               isnow_comb(ji,jj) = 1._wp
            ENDIF
         ENDIF
      END_2D
      ! clem: we should apply correction on snow thickness to take into account snow fraction
      !       it must be a distribution, so it is a bit complicated
      !
      ! Store initial temperatures and non solar heat fluxes
      IF( k_cnd == np_cnd_OFF .OR. k_cnd == np_cnd_EMU ) THEN
         DO_2D( 0, 0, 0, 0 )
            IF( l_ice_present(ji,jj) ) THEN
               ztsub(ji,jj)       =      t_su(ji,jj,jl_cat)                     ! surface temperature at iteration n-1
               t_su(ji,jj,jl_cat) = MIN( t_su(ji,jj,jl_cat), rt0 - ztsu_err )   ! required to leave the choice between melting or not
               zdqns_ice_b(ji,jj) = dqns_ice (ji,jj,jl_cat)                     ! derivative of incoming nonsolar flux
               zqns_ice_b (ji,jj) = qns_ice  (ji,jj,jl_cat)                     ! store previous qns_ice value
            ENDIF
         END_2D
         !
      ENDIF
      !
      DO_2D( 0, 0, 0, 0 )
         IF( l_ice_present(ji,jj) ) THEN
            ztsold (ji,jj,:) = t_s(ji,jj,:,jl_cat)   ! Old snow temperature
            ztiold (ji,jj,:) = t_i(ji,jj,:,jl_cat)   ! Old ice temperature
         ENDIF
      END_2D

      !-------------
      ! 2) Radiation
      !-------------
      ! --- Transmission/absorption of solar radiation in the ice --- !
      DO_2D( 0, 0, 0, 0 )
         IF( l_ice_present(ji,jj) ) THEN
            zradtr_s(ji,jj,0) = qtr_ice_top(ji,jj,jl_cat)
         ENDIF
      END_2D
      DO jk = 1, nlay_s
         DO_2D( 0, 0, 0, 0 )
            IF( l_ice_present(ji,jj) ) THEN
               !                             ! radiation transmitted below the layer-th snow layer
               zradtr_s(ji,jj,jk) = zradtr_s(ji,jj,0) * EXP( - zraext_s(ji,jj) * MAX( 0._wp, zh_s(ji,jj) * REAL(jk) - zhs_ssl ) )
               !                             ! radiation absorbed by the layer-th snow layer
               zradab_s(ji,jj,jk) = zradtr_s(ji,jj,jk-1) - zradtr_s(ji,jj,jk)
            ENDIF
         END_2D
      END DO
      !
      DO_2D( 0, 0, 0, 0 )
         IF( l_ice_present(ji,jj) ) THEN
            zradtr_i(ji,jj,0) = zradtr_s(ji,jj,nlay_s) * za_s_fra(ji,jj) + qtr_ice_top(ji,jj,jl_cat) * ( 1._wp - za_s_fra(ji,jj) )
         ENDIF
      END_2D
      DO jk = 1, nlay_i
         DO_2D( 0, 0, 0, 0 )
            IF( l_ice_present(ji,jj) ) THEN
               !                             ! radiation transmitted below the layer-th ice layer
               zradtr_i(ji,jj,jk) =   za_s_fra(ji,jj)   * zradtr_s(ji,jj,nlay_s)                       &   ! part covered by snow
                  &                                     * EXP( - rn_kappa_i * MAX( 0._wp, zh_i(ji,jj) * REAL(jk) - zh_min  ) ) &
                  &       + ( 1._wp - za_s_fra(ji,jj) ) * qtr_ice_top(ji,jj,jl_cat)                    &   ! part snow free
                  &                                     * EXP( - rn_kappa_i * MAX( 0._wp, zh_i(ji,jj) * REAL(jk) - zhi_ssl ) )
               !                             ! radiation absorbed by the layer-th ice layer
               zradab_i(ji,jj,jk) = zradtr_i(ji,jj,jk-1) - zradtr_i(ji,jj,jk)
            ENDIF
         END_2D
      END DO
         !
      DO_2D( 0, 0, 0, 0 )
         IF( l_ice_present(ji,jj) ) THEN
            qtr_ice_bot(ji,jj,jl_cat) = zradtr_i(ji,jj,nlay_i)   ! record radiation transmitted below the ice
         ENDIF
      END_2D
      !
      !
      iconv = 0          ! number of iterations
      !
      l_T_converged(:,:) = .FALSE.
      DO_2D( 0, 0, 0, 0 )
         IF( .NOT. l_ice_present(ji,jj) ) THEN
            l_T_converged(ji,jj) = .TRUE. ! Initialise l_T_converged to TRUE if no ice present
         ENDIF
      END_2D
      ! Convergence calculated until all sub-domain grid points have converged
      ! Calculations keep going for all grid points until sub-domain convergence (vectorisation optimisation)
      ! but values are not taken into account (results independant of MPI partitioning)
      !
      !                                                                            !============================!
      DO WHILE ( ( .NOT. ALL (l_T_converged(:,:)) ) .AND. iconv < iconv_max )   ! Iterative procedure begins !
         !                                                                         !============================!
         iconv = iconv + 1
         !
         !--------------------------------
         ! 3) Sea ice thermal conductivity
         !--------------------------------
         IF( ln_cndi_U64 ) THEN         !-- Untersteiner (1964) formula: k = k0 + beta.S/T
            !
            DO_2D( 0, 0, 0, 0 )
               ztcond_i_cp(ji,jj,0)      = rcnd_i + zbeta * sz_i(ji,jj,1,jl_cat)      / MIN( -epsi10, t_i(ji,jj,1,jl_cat) - rt0 )
               ztcond_i_cp(ji,jj,nlay_i) = rcnd_i + zbeta * sz_i(ji,jj,nlay_i,jl_cat) / MIN( -epsi10, t_bo(ji,jj)  - rt0 )
            END_2D
            DO jk = 1, nlay_i-1
               DO_2D( 0, 0, 0, 0 )
                  IF( l_ice_present(ji,jj)) THEN
                     ztcond_i_cp(ji,jj,jk) = rcnd_i + zbeta * 0.5_wp * ( sz_i(ji,jj,jk,jl_cat) + sz_i(ji,jj,jk+1,jl_cat) ) /  &
                        &                       MIN( -epsi10, 0.5_wp * (  t_i(ji,jj,jk,jl_cat) +  t_i(ji,jj,jk+1,jl_cat) ) - rt0 )
                  ENDIF
               END_2D
            END DO
            !
         ELSEIF( ln_cndi_P07 ) THEN     !-- Pringle et al formula: k = k0 + beta1.S/T - beta2.T
            !
            DO_2D( 0, 0, 0, 0 )
               ztcond_i_cp(ji,jj,0)      = rcnd_i + 0.09_wp  *  sz_i(ji,jj,1,jl_cat)      / MIN(-epsi10, t_i(ji,jj,1,jl_cat) - rt0) &
                  &                               - 0.011_wp * ( t_i(ji,jj,1,jl_cat) - rt0 )
               ztcond_i_cp(ji,jj,nlay_i) = rcnd_i + 0.09_wp  *  sz_i(ji,jj,nlay_i,jl_cat) / MIN(-epsi10, t_bo(ji,jj)  - rt0) &
                  &                               - 0.011_wp * ( t_bo(ji,jj) - rt0 )
            END_2D
            DO jk = 1, nlay_i-1
               DO_2D( 0, 0, 0, 0 )
                  ztcond_i_cp(ji,jj,jk) = rcnd_i + 0.09_wp  *   0.5_wp * ( sz_i(ji,jj,jk,jl_cat) + sz_i(ji,jj,jk+1,jl_cat) ) /    &
                     &                         MIN( -epsi10, 0.5_wp * (  t_i(ji,jj,jk,jl_cat) +  t_i(ji,jj,jk+1,jl_cat) ) - rt0 ) &
                     &                        - 0.011_wp * ( 0.5_wp * (  t_i(ji,jj,jk,jl_cat) +  t_i(ji,jj,jk+1,jl_cat) ) - rt0 )
               END_2D
            END DO
            !
         ENDIF

         ! Variable used after iterations
         ! Value must be frozen after convergence for MPP independance reason
         DO_2D( 0, 0, 0, 0 )
            IF( .NOT. l_T_converged(ji,jj) ) &
               ztcond_i(ji,jj,:) = MAX( zkimin, ztcond_i_cp(ji,jj,:) )
         END_2D
         !
         !--- G(he) : enhancement of thermal conductivity in mono-category case
         ! Computation of effective thermal conductivity G(h)
         ! Used in mono-category case only to simulate an ITD implicitly
         ! Fichefet and Morales Maqueda, JGR 1997
         zghe(:,:) = 1._wp
         !
         IF( ln_virtual_itd ) THEN
            !
            zepsilon = 0.1_wp
            DO_2D( 0, 0, 0, 0 )
               zcnd_i = SUM( ztcond_i(ji,jj,:) ) / REAL( nlay_i+1, wp )                            ! Mean sea ice thermal conductivity
               zhe = ( rcnd_s * h_i(ji,jj,jl_cat) + zcnd_i * h_s(ji,jj,jl_cat) ) / ( rcnd_s + zcnd_i )        ! Effective thickness he (zhe)
               IF( zhe >=  zepsilon * 0.5_wp * EXP(1._wp) )  &
                  &   zghe(ji,jj) = MIN( 2._wp, 0.5_wp * ( 1._wp + LOG( 2._wp * zhe / zepsilon ) ) )   ! G(he)
            END_2D
            !
         ENDIF
         !
         !-----------------
         ! 4) kappa factors
         !-----------------
         DO_2D( 0, 0, 0, 0 )
            !
            IF ( .NOT. l_T_converged(ji,jj) ) THEN
               !
               !--- Snow
               ! Variable used after iterations
               ! Value must be frozen after convergence for MPP independance reason
               DO jk = 0, nlay_s-1
                  zkappa_s(ji,jj,jk) = zghe(ji,jj) * rcnd_s * z1_h_s(ji,jj)
               END DO
               zfac = 0.5_wp * (ztcond_i(ji, jj, 0) * zh_s(ji, jj) + rcnd_s * zh_i(ji, jj))
               zkappa_s(ji,jj,nlay_s) = isnow(ji,jj) * zghe(ji,jj) * rcnd_s * ztcond_i(ji,jj,0) / zfac   ! Snow-ice interface
               !
               !--- Ice
               ! Variable used after iterations
               ! Value must be frozen after convergence for MPP independance reason
               DO jk = 0, nlay_i
                  zkappa_i(ji,jj,jk) = zghe(ji,jj) * ztcond_i(ji,jj,jk) * z1_h_i(ji,jj)
               END DO
               ! Calculate combined surface snow and ice conductivity to pass through the coupler (met-office)
               zkappa_comb(ji,jj) = isnow_comb(ji,jj) * zkappa_s(ji,jj,0) + ( 1._wp - isnow_comb(ji,jj) ) * zkappa_i(ji,jj,0)
               ! If there is snow then use the same snow-ice interface conductivity for the top layer of ice
               IF( h_s(ji,jj,jl_cat) > 0._wp )   zkappa_i(ji,jj,0) = zkappa_s(ji,jj,nlay_s)   ! Snow-ice interface
               !
            ENDIF
            !
         END_2D
         
         !--------------------------------------
         ! 5) Sea ice specific heat, eta factors
         !--------------------------------------
         DO jk = 1, nlay_i
            DO_2D( 0, 0, 0, 0 )
               IF( .NOT. l_T_converged(ji,jj) ) THEN
                  zcpi = rcpi + zgamma * sz_i(ji,jj,jk,jl_cat) / MAX(   ( t_i   (ji,jj,jk,jl_cat) - rt0 ) &
                     &                                                * ( ztiold(ji,jj,jk)        - rt0 ), epsi10 )
                  zeta_i(ji,jj,jk) = rDt_ice * r1_rhoi * z1_h_i(ji,jj) / zcpi
               ENDIF
            END_2D
         END DO
         !
         DO jk = 1, nlay_s
            DO_2D( 0, 0, 0, 0 )
               IF( .NOT. l_T_converged(ji,jj) ) &
                  & zeta_s(ji,jj,jk) = rDt_ice * r1_rhos * r1_rcpi * z1_h_s(ji,jj)
            END_2D
         END DO
         !
         !
         IF( k_cnd == np_cnd_OFF .OR. k_cnd == np_cnd_EMU ) THEN
            !----------------------------------------!
            !                                        !
            !   Conduction flux is off or emulated   !
            !                                        !
            !----------------------------------------!
            !
            ! ==> The original BL99 temperature computation is used
            !       (with qsr_ice, qns_ice and dqns_ice as inputs)
            !
            DO_2D( 0, 0, 0, 0 )
               !
               !----------------------------
               ! 6) surface flux computation
               !----------------------------
               ! update of the non solar flux according to the update in T_su
               ! Variable used after iterations
               ! Value must be frozen after convergence for MPP independance reason
               IF( .NOT. l_T_converged(ji,jj) ) THEN
                  !
                  qns_ice(ji,jj,jl_cat) = qns_ice(ji,jj,jl_cat) + dqns_ice(ji,jj,jl_cat) * ( t_su(ji,jj,jl_cat) - ztsub(ji,jj) )
                  !
                  zfnet = qsr_ice(ji,jj,jl_cat) - qtr_ice_top(ji,jj,jl_cat) + qns_ice(ji,jj,jl_cat) ! net heat flux = net - transmitted solar + non solar
                  !
                  ! before temperatures
                  ztib(:) = t_i(ji,jj,:,jl_cat)
                  ztsb(:) = t_s(ji,jj,:,jl_cat)
                  !
                  !----------------------------
                  ! 7) tridiagonal system terms
                  !----------------------------
                  ! layer denotes the number of the layer in the snow or in the ice
                  ! jm denotes the reference number of the equation in the tridiagonal
                  ! system, terms of tridiagonal system are indexed as following :
                  ! 1 is subdiagonal term, 2 is diagonal and 3 is superdiagonal one
                  
                  ! ice interior terms (top equation has the same form as the others)
                  ztrid   (:,:) = 0._wp
                  zindterm(:)   = 0._wp
                  zindtbis(:)   = 0._wp
                  zdiagbis(:)   = 0._wp

                  DO jm = nlay_s + 2, nlay_s + nlay_i
                     jk = jm - nlay_s - 1
                     ztrid   (jm,1) =       - zeta_i(ji,jj,jk) *   zkappa_i(ji,jj,jk-1)
                     ztrid   (jm,2) = 1._wp + zeta_i(ji,jj,jk) * ( zkappa_i(ji,jj,jk-1) + zkappa_i(ji,jj,jk) )
                     ztrid   (jm,3) =       - zeta_i(ji,jj,jk) *                          zkappa_i(ji,jj,jk)
                     zindterm(jm)   = ztiold(ji,jj,jk) + zeta_i(ji,jj,jk) * zradab_i(ji,jj,jk)
                  END DO

                  jm =  nlay_s + nlay_i + 1
                  ! ice bottom term
                  ztrid   (jm,1) =       - zeta_i(ji,jj,nlay_i) *   zkappa_i(ji,jj,nlay_i-1)
                  ztrid   (jm,2) = 1._wp + zeta_i(ji,jj,nlay_i) * ( zkappa_i(ji,jj,nlay_i-1) + zkappa_i(ji,jj,nlay_i) * zg1 )
                  ztrid   (jm,3) = 0._wp
                  zindterm(jm)   = ztiold(ji,jj,nlay_i) + zeta_i(ji,jj,nlay_i) *  &
                     &         ( zradab_i(ji,jj,nlay_i) + zkappa_i(ji,jj,nlay_i) * zg1 * t_bo(ji,jj) )

                  !                                      !---------------------!
                  IF( h_s(ji,jj,jl_cat) > 0._wp ) THEN   !  snow-covered cells !
                     !                                   !---------------------!
                     ! snow interior terms (bottom equation has the same form as the others)
                     DO jm = 3, nlay_s + 1
                        jk = jm - 1
                        ztrid   (jm,1) =       - zeta_s(ji,jj,jk) *   zkappa_s(ji,jj,jk-1)
                        ztrid   (jm,2) = 1._wp + zeta_s(ji,jj,jk) * ( zkappa_s(ji,jj,jk-1) + zkappa_s(ji,jj,jk) )
                        ztrid   (jm,3) =       - zeta_s(ji,jj,jk) *                          zkappa_s(ji,jj,jk)
                        zindterm(jm)   = ztsold(ji,jj,jk) + zeta_s(ji,jj,jk) * zradab_s(ji,jj,jk)
                     END DO

                     ! case of only one layer in the ice (ice equation is altered)
                     IF( nlay_i == 1 ) THEN
                        ztrid   (nlay_s+2,3) = 0._wp
                        zindterm(nlay_s+2)   = zindterm(nlay_s+2) + zeta_i(ji,jj,1) * zkappa_i(ji,jj,1) * t_bo(ji,jj)
                     ENDIF

                     IF( t_su(ji,jj,jl_cat) < rt0 ) THEN   !--  case 1 : no surface melting

                        jm_min = 1
                        jm_max = nlay_i + nlay_s + 1

                        ! surface equation
                        ztrid   (1,1) = 0._wp
                        ztrid   (1,2) = zdqns_ice_b(ji,jj) - zg1s * zkappa_s(ji,jj,0)
                        ztrid   (1,3) =                      zg1s * zkappa_s(ji,jj,0)
                        zindterm(1)   = zdqns_ice_b(ji,jj) * t_su(ji,jj,jl_cat) - zfnet

                        ! first layer of snow equation
                        ztrid   (2,1) =       - zeta_s(ji,jj,1) *                       zkappa_s(ji,jj,0) * zg1s
                        ztrid   (2,2) = 1._wp + zeta_s(ji,jj,1) * ( zkappa_s(ji,jj,1) + zkappa_s(ji,jj,0) * zg1s )
                        ztrid   (2,3) =       - zeta_s(ji,jj,1) *   zkappa_s(ji,jj,1)
                        zindterm(2)   = ztsold(ji,jj,1) + zeta_s(ji,jj,1) * zradab_s(ji,jj,1)

                     ELSE                                  !--  case 2 : surface is melting
                        !
                        jm_min = 2
                        jm_max = nlay_i + nlay_s + 1

                        ! first layer of snow equation
                        ztrid   (2,1) = 0._wp
                        ztrid   (2,2) = 1._wp + zeta_s(ji,jj,1) * ( zkappa_s(ji,jj,1) + zkappa_s(ji,jj,0) * zg1s )
                        ztrid   (2,3) =       - zeta_s(ji,jj,1) *   zkappa_s(ji,jj,1)
                        zindterm(2)   = ztsold(ji,jj,1) + zeta_s(ji,jj,1) * &
                           &                          ( zradab_s(ji,jj,1) + zkappa_s(ji,jj,0) * zg1s * t_su(ji,jj,jl_cat) )
                     ENDIF
                     !                            !---------------------!
                  ELSE                            ! cells without snow  !
                     !                            !---------------------!
                     !
                     IF( t_su(ji,jj,jl_cat) < rt0 ) THEN   !--  case 1 : no surface melting
                        !
                        jm_min = nlay_s + 1
                        jm_max = nlay_i + nlay_s + 1

                        ! surface equation
                        ztrid   (jm_min,1) = 0._wp
                        ztrid   (jm_min,2) = zdqns_ice_b(ji,jj) - zkappa_i(ji,jj,0) * zg1
                        ztrid   (jm_min,3) =                      zkappa_i(ji,jj,0) * zg1
                        zindterm(jm_min)   = zdqns_ice_b(ji,jj) * t_su(ji,jj,jl_cat) - zfnet

                        ! first layer of ice equation
                        ztrid   (jm_min+1,1) =       - zeta_i(ji,jj,1) *                       zkappa_i(ji,jj,0) * zg1
                        ztrid   (jm_min+1,2) = 1._wp + zeta_i(ji,jj,1) * ( zkappa_i(ji,jj,1) + zkappa_i(ji,jj,0) * zg1 )
                        ztrid   (jm_min+1,3) =       - zeta_i(ji,jj,1) *   zkappa_i(ji,jj,1)
                        zindterm(jm_min+1)   = ztiold(ji,jj,1) + zeta_i(ji,jj,1) * zradab_i(ji,jj,1)

                        ! case of only one layer in the ice (surface & ice equations are altered)
                        IF( nlay_i == 1 ) THEN
                           ztrid   (jm_min,1)   = 0._wp
                           ztrid   (jm_min,2)   = zdqns_ice_b(ji,jj)      -   zkappa_i(ji,jj,0) * 2._wp
                           ztrid   (jm_min,3)   =                             zkappa_i(ji,jj,0) * 2._wp
                           ztrid   (jm_min+1,1) =       - zeta_i(ji,jj,1) *   zkappa_i(ji,jj,0) * 2._wp
                           ztrid   (jm_min+1,2) = 1._wp + zeta_i(ji,jj,1) * ( zkappa_i(ji,jj,0) * 2._wp + zkappa_i(ji,jj,1) )
                           ztrid   (jm_min+1,3) = 0._wp
                           zindterm(jm_min+1)   = ztiold(ji,jj,1) + zeta_i(ji,jj,1) * &
                              &                                 ( zradab_i(ji,jj,1) + zkappa_i(ji,jj,1) * t_bo(ji,jj) )
                        ENDIF

                     ELSE                            !--  case 2 : surface is melting

                        jm_min = nlay_s + 2
                        jm_max = nlay_i + nlay_s + 1

                        ! first layer of ice equation
                        ztrid   (jm_min,1) = 0._wp
                        ztrid   (jm_min,2) = 1._wp + zeta_i(ji,jj,1) * ( zkappa_i(ji,jj,1) + zkappa_i(ji,jj,0) * zg1 )
                        ztrid   (jm_min,3) =       - zeta_i(ji,jj,1) *   zkappa_i(ji,jj,1)
                        zindterm(jm_min)   = ztiold(ji,jj,1) + zeta_i(ji,jj,1) * &
                           &                               ( zradab_i(ji,jj,1) + zkappa_i(ji,jj,0) * zg1 * t_su(ji,jj,jl_cat) )

                        ! case of only one layer in the ice (surface & ice equations are altered)
                        IF( nlay_i == 1 ) THEN
                           ztrid   (jm_min,1) = 0._wp
                           ztrid   (jm_min,2) = 1._wp + zeta_i(ji,jj,1) * ( zkappa_i(ji,jj,0) * 2._wp + zkappa_i(ji,jj,1) )
                           ztrid   (jm_min,3) = 0._wp
                           zindterm(jm_min)   = ztiold(ji,jj,1) + zeta_i(ji,jj,1) * &
                              &                               ( zradab_i(ji,jj,1) + zkappa_i(ji,jj,1) * t_bo(ji,jj) ) &
                              &            + t_su(ji,jj,jl_cat) * zeta_i(ji,jj,1) * zkappa_i(ji,jj,0) * 2._wp
                        ENDIF

                     ENDIF
                  ENDIF
                  !
                  zindtbis(jm_min) = zindterm(jm_min)
                  zdiagbis(jm_min) = ztrid   (jm_min,2)
                  !
                  !
                  !------------------------------
                  ! 8) tridiagonal system solving
                  !------------------------------
                  ! Solve the tridiagonal system with Gauss elimination method.
                  ! Thomas algorithm, from Computational fluid Dynamics, J.D. ANDERSON, McGraw-Hill 1984

                  DO jm = jm_min+1, jm_max
                     zdiagbis(jm) = ztrid   (jm,2) - ztrid(jm,1) * ztrid   (jm-1,3) / zdiagbis(jm-1)
                     zindtbis(jm) = zindterm(jm  ) - ztrid(jm,1) * zindtbis(jm-1  ) / zdiagbis(jm-1)
                  END DO

                  ! ice temperatures
                  ! Variable used after iterations
                  ! Value must be frozen after convergence for MPP independance reason
                  t_i(ji,jj,nlay_i,jl_cat) = zindtbis(jm_max) / zdiagbis(jm_max)
                  
                  DO jm = nlay_i + nlay_s, nlay_s + 2, -1
                     jk = jm - nlay_s - 1
                     t_i(ji,jj,jk,jl_cat) = ( zindtbis(jm) - ztrid(jm,3) * t_i(ji,jj,jk+1,jl_cat) ) / zdiagbis(jm)
                  END DO

                  ! snow temperatures
                  ! Variables used after iterations
                  ! Value must be frozen after convergence for MPP independance reason
                  IF ( h_s(ji,jj,jl_cat) > 0._wp ) &
                     &   t_s(ji,jj,nlay_s,jl_cat) = ( zindtbis(nlay_s+1) - ztrid(nlay_s+1,3) * t_i(ji,jj,1,jl_cat) ) &
                     &                              / zdiagbis(nlay_s+1)

                  DO jm = nlay_s, 2, -1
                     jk = jm - 1
                     IF ( h_s(ji,jj,jl_cat) > 0._wp ) &
                        &   t_s(ji,jj,jk,jl_cat) = ( zindtbis(jm) - ztrid(jm,3) * t_s(ji,jj,jk+1,jl_cat) ) / zdiagbis(jm)
                  END DO
                  
                  ! surface temperature
                  ztsub(ji,jj) = t_su(ji,jj,jl_cat)
                  IF( t_su(ji,jj,jl_cat) < rt0 ) THEN
                     t_su(ji,jj,jl_cat) = ( zindtbis(jm_min) - ztrid(jm_min,3) *  &
                        &                 ( isnow(ji,jj) * t_s(ji,jj,1,jl_cat) + ( 1._wp - isnow(ji,jj) ) * t_i(ji,jj,1,jl_cat) ) ) &
                        &                 / zdiagbis(jm_min)
                  ENDIF
                  !
                  !--------------------------------------------------------------
                  ! 9) Has the scheme converged?, end of the iterative procedure
                  !--------------------------------------------------------------
                  ! check that nowhere it has started to melt
                  ! zdti_max is a measure of error, it has to be under zdti_bnd                  

                  zdti_max = 0._wp

                  t_su(ji,jj,jl_cat) = MAX( MIN( t_su(ji,jj,jl_cat) , rt0 ) , rt0 - 100._wp )
                  zdti_max           = MAX( zdti_max, ABS( t_su(ji,jj,jl_cat) - ztsub(ji,jj) ) )

                  IF( h_s(ji,jj,jl_cat) > 0._wp ) THEN
                     DO jk = 1, nlay_s
                        t_s(ji,jj,jk,jl_cat) = MAX( MIN( t_s(ji,jj,jk,jl_cat), rt0 ), rt0 - 100._wp )
                        zdti_max             = MAX ( zdti_max , ABS( t_s(ji,jj,jk,jl_cat) - ztsb(jk) ) )
                     END DO
                  ENDIF

                  DO jk = 1, nlay_i
                     ztmelts              = -rTmlt * sz_i(ji,jj,jk,jl_cat) + rt0
                     t_i(ji,jj,jk,jl_cat) =  MAX( MIN( t_i(ji,jj,jk,jl_cat), ztmelts ), rt0 - 100._wp )
                     zdti_max             =  MAX( zdti_max, ABS( t_i(ji,jj,jk,jl_cat) - ztib(jk) ) )
                  END DO

                  ! convergence test
                  IF( ln_zdf_chkcvg ) THEN
                     ztice_cvgerr(ji,jj,jl_cat) = zdti_max
                     ztice_cvgstp(ji,jj,jl_cat) = REAL(iconv)
                  ENDIF

                  IF( zdti_max < zdti_bnd )   l_T_converged(ji,jj) = .TRUE.

               ENDIF
            END_2D     
            !
         ELSEIF( k_cnd == np_cnd_ON ) THEN
            !----------------------------------------!
            !                                        !
            !      Conduction flux is on             !
            !                                        !
            !----------------------------------------!
            !
            ! ==> we use a modified BL99 solver with conduction flux (qcn_ice) as forcing term
            !
            DO_2D( 0, 0, 0, 0 )
               !
               IF( .NOT. l_T_converged(ji,jj) ) THEN
                  ! before temperatures
                  ztib(:) = t_i(ji,jj,:,jl_cat)
                  ztsb(:) = t_s(ji,jj,:,jl_cat)
                  !
                  !----------------------------
                  ! 7) tridiagonal system terms
                  !----------------------------
                  ! layer denotes the number of the layer in the snow or in the ice
                  ! jm denotes the reference number of the equation in the tridiagonal
                  ! system, terms of tridiagonal system are indexed as following :
                  ! 1 is subdiagonal term, 2 is diagonal and 3 is superdiagonal one

                  ! ice interior terms (top equation has the same form as the others)
                  ztrid   (:,:) = 0._wp
                  zindterm(:)   = 0._wp
                  zindtbis(:)   = 0._wp
                  zdiagbis(:)   = 0._wp

                  DO jm = nlay_s + 2, nlay_s + nlay_i
                     jk = jm - nlay_s - 1
                     ztrid   (jm,1) =       - zeta_i(ji,jj,jk) *   zkappa_i(ji,jj,jk-1)
                     ztrid   (jm,2) = 1._wp + zeta_i(ji,jj,jk) * ( zkappa_i(ji,jj,jk-1) + zkappa_i(ji,jj,jk) )
                     ztrid   (jm,3) =       - zeta_i(ji,jj,jk) *                          zkappa_i(ji,jj,jk)
                     zindterm(jm)   = ztiold(ji,jj,jk) + zeta_i(ji,jj,jk) * zradab_i(ji,jj,jk)
                  END DO

                  ! ice bottom term
                  jm =  nlay_s + nlay_i + 1
                  ztrid   (jm,1) =       - zeta_i(ji,jj,nlay_i) *   zkappa_i(ji,jj,nlay_i-1)
                  ztrid   (jm,2) = 1._wp + zeta_i(ji,jj,nlay_i) * ( zkappa_i(ji,jj,nlay_i-1) + zkappa_i(ji,jj,nlay_i) * zg1 )
                  ztrid   (jm,3) = 0._wp
                  zindterm(jm)   = ztiold(ji,jj,nlay_i) + zeta_i(ji,jj,nlay_i) *  &
                     &              ( zradab_i(ji,jj,nlay_i) + zkappa_i(ji,jj,nlay_i) * zg1 * t_bo(ji,jj) )

                  !                                      !---------------------!
                  IF( h_s(ji,jj,jl_cat) > 0._wp ) THEN   !  snow-covered cells !
                     !                                   !---------------------!
                     ! snow interior terms (bottom equation has the same form as the others)
                     DO jm = 3, nlay_s + 1
                        jk = jm - 1
                        ztrid   (jm,1) =       - zeta_s(ji,jj,jk) *   zkappa_s(ji,jj,jk-1)
                        ztrid   (jm,2) = 1._wp + zeta_s(ji,jj,jk) * ( zkappa_s(ji,jj,jk-1) + zkappa_s(ji,jj,jk) )
                        ztrid   (jm,3) =       - zeta_s(ji,jj,jk) *                          zkappa_s(ji,jj,jk)
                        zindterm(jm)   = ztsold(ji,jj,jk) + zeta_s(ji,jj,jk) * zradab_s(ji,jj,jk)
                     END DO

                     ! case of only one layer in the ice (ice equation is altered)
                     IF ( nlay_i == 1 ) THEN
                        ztrid   (nlay_s+2,3) = 0._wp
                        zindterm(nlay_s+2)   = zindterm(nlay_s+2) + zeta_i(ji,jj,1) * zkappa_i(ji,jj,1) * t_bo(ji,jj)
                     ENDIF

                     jm_min = 2
                     jm_max = nlay_i + nlay_s + 1

                     ! first layer of snow equation
                     ztrid   (2,1) = 0._wp
                     ztrid   (2,2) = 1._wp + zeta_s(ji,jj,1) * zkappa_s(ji,jj,1)
                     ztrid   (2,3) =       - zeta_s(ji,jj,1) * zkappa_s(ji,jj,1)
                     zindterm(2)   = ztsold(ji,jj,1) + zeta_s(ji,jj,1) * ( zradab_s(ji,jj,1) + qcn_ice(ji,jj,jl_cat) )

                     !                                   !---------------------!
                  ELSE                                   ! cells without snow  !
                     !                                   !---------------------!
                     jm_min = nlay_s + 2
                     jm_max = nlay_i + nlay_s + 1

                     ! first layer of ice equation
                     ztrid   (jm_min,1) = 0._wp
                     ztrid   (jm_min,2) = 1._wp + zeta_i(ji,jj,1) * zkappa_i(ji,jj,1)
                     ztrid   (jm_min,3) =       - zeta_i(ji,jj,1) * zkappa_i(ji,jj,1)
                     zindterm(jm_min)   = ztiold(ji,jj,1) + zeta_i(ji,jj,1) * ( zradab_i(ji,jj,1) + qcn_ice(ji,jj,jl_cat) )

                     ! case of only one layer in the ice (surface & ice equations are altered)
                     IF( nlay_i == 1 ) THEN
                        ztrid   (jm_min,1) = 0._wp
                        ztrid   (jm_min,2) = 1._wp + zeta_i(ji,jj,1) * zkappa_i(ji,jj,1)
                        ztrid   (jm_min,3) = 0._wp
                        zindterm(jm_min)   = ztiold(ji,jj,1) + zeta_i(ji,jj,1) *  &
                           &           ( zradab_i(ji,jj,1) + zkappa_i(ji,jj,1) * t_bo(ji,jj) + qcn_ice(ji,jj,jl_cat) )
                     ENDIF

                  ENDIF
                  !
                  zindtbis(jm_min) = zindterm(jm_min)
                  zdiagbis(jm_min) = ztrid   (jm_min,2)
                  !
                  !
                  !------------------------------
                  ! 8) tridiagonal system solving
                  !------------------------------
                  ! Solve the tridiagonal system with Gauss elimination method.
                  ! Thomas algorithm, from Computational fluid Dynamics, J.D. ANDERSON, McGraw-Hill 1984

                  DO jm = jm_min+1, jm_max
                     zdiagbis(jm) = ztrid   (jm,2) - ztrid(jm,1) * ztrid   (jm-1,3) / zdiagbis(jm-1)
                     zindtbis(jm) = zindterm(jm  ) - ztrid(jm,1) * zindtbis(jm-1  ) / zdiagbis(jm-1)
                  END DO

                  ! ice temperatures
                  ! Variable used after iterations
                  ! Value must be frozen after convergence for MPP independance reason
                  t_i(ji,jj,nlay_i,jl_cat) = zindtbis(jm_max) / zdiagbis(jm_max)

                  DO jm = nlay_i + nlay_s, nlay_s + 2, -1
                     jk = jm - nlay_s - 1
                     t_i(ji,jj,jk,jl_cat) = ( zindtbis(jm) - ztrid(jm,3) * t_i(ji,jj,jk+1,jl_cat) ) / zdiagbis(jm)
                  END DO

                  ! snow temperatures
                  ! Variables used after iterations
                  ! Value must be frozen after convergence for MPP independance reason
                  IF ( h_s(ji,jj,jl_cat) > 0._wp ) &
                     &   t_s(ji,jj,nlay_s,jl_cat) = ( zindtbis(nlay_s+1) - ztrid(nlay_s+1,3) * t_i(ji,jj,1,jl_cat) ) &
                     &                              / zdiagbis(nlay_s+1)

                  DO jm = nlay_s, 2, -1
                     jk = jm - 1
                     IF ( h_s(ji,jj,jl_cat) > 0._wp ) &
                        &   t_s(ji,jj,jk,jl_cat) = ( zindtbis(jm) - ztrid(jm,3) * t_s(ji,jj,jk+1,jl_cat) ) / zdiagbis(jm)
                  END DO
                  !
                  !--------------------------------------------------------------
                  ! 9) Has the scheme converged?, end of the iterative procedure
                  !--------------------------------------------------------------
                  ! check that nowhere it has started to melt
                  ! zdti_max is a measure of error, it has to be under zdti_bnd

                  zdti_max = 0._wp

                  IF( h_s(ji,jj,jl_cat) > 0._wp ) THEN
                     DO jk = 1, nlay_s
                        t_s(ji,jj,jk,jl_cat) = MAX( MIN( t_s(ji,jj,jk,jl_cat), rt0 ), rt0 - 100._wp )
                        zdti_max             = MAX ( zdti_max , ABS( t_s(ji,jj,jk,jl_cat) - ztsb(jk) ) )
                     END DO
                  ENDIF

                  DO jk = 1, nlay_i
                     ztmelts              = -rTmlt * sz_i(ji,jj,jk,jl_cat) + rt0
                     t_i(ji,jj,jk,jl_cat) =  MAX( MIN( t_i(ji,jj,jk,jl_cat), ztmelts ), rt0 - 100._wp )
                     zdti_max             =  MAX ( zdti_max, ABS( t_i(ji,jj,jk,jl_cat) - ztib(jk) ) )
                  END DO

                  ! convergence test
                  IF( ln_zdf_chkcvg ) THEN
                     ztice_cvgerr(ji,jj,jl_cat) = zdti_max
                     ztice_cvgstp(ji,jj,jl_cat) = REAL(iconv)
                  ENDIF

                  IF( zdti_max < zdti_bnd )   l_T_converged(ji,jj) = .TRUE.

               ENDIF
            END_2D

         ENDIF ! k_cnd

      END DO  ! End of the do while iterative procedure
      !
      !-----------------------------
      ! 10) Fluxes at the interfaces
      !-----------------------------
      !
      IF( k_cnd == np_cnd_OFF .OR. k_cnd == np_cnd_EMU ) THEN
         !
         DO_2D( 0, 0, 0, 0 )
            IF( l_ice_present(ji,jj)) THEN
               ! --- ice conduction fluxes (positive downward)
               qcn_ice_bot(ji,jj,jl_cat) = - zkappa_i(ji,jj,nlay_i) * zg1 * (t_bo(ji,jj ) - t_i(ji,jj,nlay_i,jl_cat))                 ! bottom
               qcn_ice_top(ji,jj,jl_cat) = - isnow(ji,jj)   * zkappa_s(ji,jj,0) * zg1s * (t_s(ji,jj,1,jl_cat) - t_su(ji,jj,jl_cat)) & ! surface
                  &              - ( 1._wp - isnow(ji,jj) ) * zkappa_i(ji,jj,0) * zg1  * (t_i(ji,jj,1,jl_cat) - t_su(ji,jj,jl_cat))
               ! --- Diagnose the heat loss due to changing non-solar / conduction flux
               hfx_err_dif(ji,jj) = hfx_err_dif(ji,jj) - ( qns_ice(ji,jj,jl_cat) - zqns_ice_b(ji,jj) ) * a_i(ji,jj,jl_cat)           
            ENDIF
         END_2D
         !
      ELSEIF( k_cnd == np_cnd_ON ) THEN
         !
         DO_2D( 0, 0, 0, 0 )
          IF( l_ice_present(ji,jj)) THEN
            ! --- ice conduction fluxes (positive downward)
            qcn_ice_bot(ji,jj,jl_cat) = - zkappa_i(ji,jj,nlay_i) * zg1 * ( t_bo(ji,jj ) - t_i(ji,jj,nlay_i,jl_cat) ) ! bottom
            qcn_ice_top(ji,jj,jl_cat) = qcn_ice(ji,jj,jl_cat)                                                      ! surface
          ENDIF
         END_2D
         !
         ! --- surface ice temperature
         IF( ln_cndemulate ) THEN
            DO_2D( 0, 0, 0, 0 )
               IF( l_ice_present(ji,jj) ) THEN
                  t_su(ji,jj,jl_cat) = ( qcn_ice_top(ji,jj,jl_cat) + isnow(ji,jj)   * zkappa_s(ji,jj,0) * zg1s * t_s(ji,jj,1,jl_cat) &
                     &                                  +  ( 1._wp - isnow(ji,jj) ) * zkappa_i(ji,jj,0) * zg1  * t_i(ji,jj,1,jl_cat) &
                     &                 ) / MAX( epsi10, isnow(ji,jj)   * zkappa_s(ji,jj,0) * zg1s &
                     &                      + ( 1._wp - isnow(ji,jj) ) * zkappa_i(ji,jj,0) * zg1  )
                  t_su(ji,jj,jl_cat) = MAX( MIN( t_su(ji,jj,jl_cat), rt0 ), rt0 - 100._wp )  ! cap t_su
               ENDIF
            END_2D
         ENDIF
         !
      ENDIF
      !
      ! --- Diagnose the heat loss due to non-fully converged temperature solution (should not be larger than 10-4 W-m2)
      !
      IF( k_cnd == np_cnd_OFF .OR. k_cnd == np_cnd_ON ) THEN

         CALL ice_var_enthalpy( jl_cat )

         ! zhfx_err = correction on the diagnosed heat flux due to non-convergence of the algorithm used to solve heat equation
         DO_2D( 0, 0, 0, 0 )
            IF( l_ice_present(ji,jj) ) THEN
               !
               zdq = - zq_ini(ji,jj) + ( SUM( e_i(ji,jj,1:nlay_i,jl_cat) ) * h_i(ji,jj,jl_cat) * r1_nlay_i +  &
                  &                      SUM( e_s(ji,jj,1:nlay_s,jl_cat) ) * h_s(ji,jj,jl_cat) * r1_nlay_s )
               !
               IF( k_cnd == np_cnd_OFF ) THEN
                  !
                  IF( t_su(ji,jj,jl_cat) < rt0 ) THEN  ! case T_su < 0degC
                     zhfx_err = (     qns_ice(ji,jj,jl_cat) + qsr_ice(ji,jj,jl_cat)     - zradtr_i(ji,jj,nlay_i) &
                        &       - qcn_ice_bot(ji,jj,jl_cat) + zdq * r1_Dt_ice ) * a_i(ji,jj,jl_cat)
                  ELSE                          ! case T_su = 0degC
                     zhfx_err = ( qcn_ice_top(ji,jj,jl_cat) + qtr_ice_top(ji,jj,jl_cat) - zradtr_i(ji,jj,nlay_i) &
                        &       - qcn_ice_bot(ji,jj,jl_cat) + zdq * r1_Dt_ice ) * a_i(ji,jj,jl_cat)
                  ENDIF
                  !
               ELSEIF( k_cnd == np_cnd_ON ) THEN
                  !
                  zhfx_err    = ( qcn_ice_top(ji,jj,jl_cat) + qtr_ice_top(ji,jj,jl_cat) - zradtr_i(ji,jj,nlay_i) &
                     &          - qcn_ice_bot(ji,jj,jl_cat) + zdq * r1_Dt_ice ) * a_i(ji,jj,jl_cat)
                  !
               ENDIF
               !
               ! total heat sink to be sent to the ocean
               hfx_err_dif(ji,jj) = hfx_err_dif(ji,jj) + zhfx_err
               !
               ! hfx_dif = Heat flux diagnostic of sensible heat used to warm/cool ice in W.m-2
               hfx_dif(ji,jj) = hfx_dif(ji,jj) - zdq * r1_Dt_ice * a_i(ji,jj,jl_cat)
               !
            ENDIF
         END_2D
         !
      ENDIF
      !
      !--------------------------------------------------------------------
      ! 11) reset inner snow and ice temperatures, update conduction fluxes
      !--------------------------------------------------------------------
      ! effective conductivity and 1st layer temperature (needed by Met Office)
      ! this is a conductivity at mid-layer, hence the factor 2
      DO_2D( 0, 0, 0, 0 )
         IF( l_ice_present(ji,jj) ) THEN
            IF( h_i(ji,jj,jl_cat) >= zhi_ssl ) THEN
               cnd_ice(ji,jj,jl_cat) = 2._wp * zkappa_comb(ji,jj)
            ELSE
               cnd_ice(ji,jj,jl_cat) = 2._wp * ztcond_i(ji,jj,0) / zhi_ssl ! cnd_ice is capped by: cond_i/zhi_ssl
            ENDIF
            t1_ice(ji,jj,jl_cat) = isnow(ji,jj) * t_s(ji,jj,1,jl_cat) + ( 1._wp - isnow(ji,jj) ) * t_i(ji,jj,1,jl_cat)
         ENDIF
      END_2D
      !
      IF( k_cnd == np_cnd_EMU ) THEN
         DO_2D( 0, 0, 0, 0 )
            IF( l_ice_present(ji,jj) ) THEN 
               ! Restore temperatures to their initial values
               t_s    (ji,jj,:,jl_cat) = ztsold     (ji,jj,:)
               t_i    (ji,jj,:,jl_cat) = ztiold     (ji,jj,:)
               qcn_ice(ji,jj,  jl_cat) = qcn_ice_top(ji,jj,jl_cat)
            ENDIF
         END_2D
      ENDIF
      !
      ! --- SIMIP diagnostics (Snow-ice interfacial temperature)
      DO_2D( 0, 0, 0, 0 )
         IF( l_ice_present(ji,jj)) THEN
            IF( h_s(ji,jj,jl_cat) >= zhs_ssl ) THEN
               t_si(ji,jj,jl_cat) = (   rcnd_s         * h_i(ji,jj,jl_cat) * r1_nlay_i * t_s(ji,jj,nlay_s,jl_cat)   &
                  &                + ztcond_i(ji,jj,1) * h_s(ji,jj,jl_cat) * r1_nlay_s * t_i(ji,jj,1,jl_cat)      ) &
                  &                 / ( rcnd_s         * h_i(ji,jj,jl_cat) * r1_nlay_i &
                  &                + ztcond_i(ji,jj,1) * h_s(ji,jj,jl_cat) * r1_nlay_s )
            ELSE
               t_si(ji,jj,jl_cat) = t_su(ji,jj,jl_cat)
            ENDIF
         ENDIF
      END_2D
      !
   END SUBROUTINE ice_thd_zdf_BL99

#else
   !!----------------------------------------------------------------------
   !!   Default option       Dummy Module             No SI3 sea-ice model
   !!----------------------------------------------------------------------
#endif

   !!======================================================================
END MODULE icethd_zdf_BL99
