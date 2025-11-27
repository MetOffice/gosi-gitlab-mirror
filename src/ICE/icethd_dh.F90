MODULE icethd_dh
   !!======================================================================
   !!                       ***  MODULE icethd_dh ***
   !!   seaice : thermodynamic growth and melt
   !!======================================================================
   !! History :       !  2003-05  (M. Vancoppenolle) Original code in 1D
   !!                 !  2005-06  (M. Vancoppenolle) 3D version
   !!            4.0  !  2018     (many people)      SI3 [aka Sea Ice cube]
   !!----------------------------------------------------------------------
#if defined key_si3
   !!----------------------------------------------------------------------
   !!   'key_si3'                                       SI3 sea-ice model
   !!----------------------------------------------------------------------
   !!   ice_thd_dh        : vertical sea-ice growth and melt
   !!----------------------------------------------------------------------
   USE par_ice        ! SI3 parameters
   USE par_kind, ONLY : wp
   USE par_oce
   USE sbc_oce , ONLY : sss_m, sst_m
   USE phycst
   USE ice
   USE sbc_ice
   USE icevar  , ONLY : ice_var_snwblow, ice_var_vremap, snw_var_vremap

   IMPLICIT NONE
   PRIVATE

   PUBLIC   ice_thd_dh        ! called by ice_thd

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/ICE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE ice_thd_dh( jl_cat, pq_top, pq_bot, pf_tt, pevap_rema )
      !!------------------------------------------------------------------
      !!                ***  ROUTINE ice_thd_dh  ***
      !!
      !! ** Purpose :   compute ice and snow thickness changes due to growth/melting
      !!
      !! ** Method  :   Ice/Snow surface melting arises from imbalance in surface fluxes
      !!                Bottom accretion/ablation arises from flux budget
      !!                Snow thickness can increase by precipitation and decrease by sublimation
      !!                If snow load excesses Archmiede limit, snow-ice is formed by
      !!                the flooding of sea-water in the snow
      !!
      !!                - Compute available flux of heat for surface ablation
      !!                - Compute snow and sea ice enthalpies
      !!                - Surface ablation and sublimation
      !!                - Bottom accretion/ablation
      !!                - Snow ice formation
      !!
      !! ** Note     :  h=max(0,h+dh) are often used to ensure positivity of h.
      !!                very small negative values can occur otherwise (e.g. -1.e-20)
      !!
      !! References : Bitz and Lipscomb, 1999, J. Geophys. Res.
      !!              Fichefet T. and M. Maqueda 1997, J. Geophys. Res., 102(C6), 12609-12646
      !!              Vancoppenolle, Fichefet and Bitz, 2005, Geophys. Res. Let.
      !!              Vancoppenolle et al.,2009, Ocean Modelling
      !!------------------------------------------------------------------
      INTEGER  ::   ji, jj, jk       ! dummy loop indices
      INTEGER,                     INTENT(in)    ::   jl_cat
      REAL(wp), DIMENSION(A2D(0)), INTENT(inout) ::   pq_top          ! heat for surface ablation (J.m-2)
      REAL(wp), DIMENSION(A2D(0)), INTENT(inout) ::   pq_bot          ! heat for bottom ablation (J.m-2)
      REAL(wp), DIMENSION(A2D(0)), INTENT(in   ) ::   pf_tt           ! Heat budget to determine melting or freezing (W.m-2)
      REAL(wp), DIMENSION(A2D(0)), INTENT(inout) ::   pevap_rema      ! remaining of evaporation after snow sublimation (in kg/m2)
      !
      REAL(wp) ::   ztmelts      ! local scalar
      REAL(wp) ::   zdum
      REAL(wp) ::   zt_i_new     ! bottom formation temperature
      REAL(wp) ::   z1_rho       ! 1/(rhos+rho0-rhoi)
      !
      REAL(wp) ::   zQm          ! enthalpy exchanged with the ocean (J/m2), >0 towards the ocean
      REAL(wp) ::   zEi          ! specific enthalpy of sea ice (J/kg)
      REAL(wp) ::   zEw          ! specific enthalpy of exchanged water (J/kg)
      REAL(wp) ::   zdE          ! specific enthalpy difference (J/kg)
      REAL(wp) ::   zfmdt        ! exchange mass flux x time step (J/m2), >0 towards the ocean
      REAL(wp) ::   zdeltah, zs_i_new, zds, zs_sni
      REAL(wp) ::   zswitch_sal
      !
      INTEGER , DIMENSION(nlay_i)     ::   icount    ! number of layers vanishing by melting
      REAL(wp), DIMENSION(nlay_i)     ::   zs_i      ! ice salinity
      REAL(wp), DIMENSION(0:nlay_i+1) ::   zh_i      ! ice layer thickness (m)
      REAL(wp), DIMENSION(0:nlay_s  ) ::   zh_s      ! snw layer thickness (m)
      REAL(wp), DIMENSION(0:nlay_s  ) ::   ze_s      ! snw layer enthalpy (J.m-3)
      REAL(wp), DIMENSION(0:nlay_i+1) ::   zh_i_old  ! old thickness
      REAL(wp), DIMENSION(0:nlay_i+1) ::   ze_i_old  ! old enthalpy
      REAL(wp), DIMENSION(0:nlay_i+1) ::   zs_i_old  ! old salt content
      !!------------------------------------------------------------------

      ! Discriminate between time varying salinity and constant
      SELECT CASE( nn_icesal )                  ! varying salinity or not
         CASE( 1 , 3 )   ;   zswitch_sal = 0._wp   ! prescribed salinity profile
         CASE( 2 , 4 )   ;   zswitch_sal = 1._wp   ! varying salinity profile
      END SELECT
      !
      ! for snw-ice formation
      z1_rho = 1._wp / ( rhos+rho0-rhoi )
      !
      !                       ! ==================== !
      !                       ! Start main loop here !
      !                       ! ==================== !
      DO_2D( 0, 0, 0, 0 )
         !
         IF( l_ice_present(ji,jj) ) THEN
            !
            !
            ! initialize salinity
            IF( nn_icesal == 4 ) THEN   ;   zs_i(:) = sz_i(ji,jj,:,jl_cat)  ! use layer salinity if nn_icesal=4 
            ELSE                        ;   zs_i(:) = s_i (ji,jj,  jl_cat)  !     bulk  salinity otherwise (for conservation purpose)
            ENDIF
            !
            ! initialize ice layer thicknesses and enthalpies
            zs_i_old(0:nlay_i+1) = 0._wp
            ze_i_old(0:nlay_i+1) = 0._wp
            zh_i_old(0:nlay_i+1) = 0._wp
            zh_i    (0:nlay_i+1) = 0._wp
            DO jk = 1, nlay_i
               zs_i_old(jk) = h_i(ji,jj,jl_cat) * r1_nlay_i * zs_i(jk)
               ze_i_old(jk) = h_i(ji,jj,jl_cat) * r1_nlay_i * e_i (ji,jj,jk,jl_cat)
               zh_i_old(jk) = h_i(ji,jj,jl_cat) * r1_nlay_i
               zh_i    (jk) = h_i(ji,jj,jl_cat) * r1_nlay_i
            END DO
            !
            ! initialize snw layer thicknesses and enthalpies (for snow-ice)
            zh_s(0) = 0._wp
            ze_s(0) = 0._wp
            DO jk = 1, nlay_s
               zh_s(jk) = h_s(ji,jj,   jl_cat) * r1_nlay_s
               ze_s(jk) = e_s(ji,jj,jk,jl_cat)
            END DO
            !
            !
            ! Ice sublimation
            ! ---------------
            DO jk = 1, nlay_i
               zdum               = MAX( - zh_i(jk) , - pevap_rema(ji,jj) * r1_rhoi )
               !
               hfx_sub    (ji,jj) = hfx_sub    (ji,jj) + e_i(ji,jj,jk,jl_cat) * zdum * a_i(ji,jj,jl_cat) * r1_Dt_ice ! Heat flux [W.m-2], < 0
               wfx_ice_sub(ji,jj) = wfx_ice_sub(ji,jj) - rhoi                 * zdum * a_i(ji,jj,jl_cat) * r1_Dt_ice ! Mass flux > 0
               sfx_sub    (ji,jj) = sfx_sub    (ji,jj) - rhoi * zs_i(jk)      * zdum * a_i(ji,jj,jl_cat) * r1_Dt_ice ! Salt flux >0
               !                                                                                                    clem: flux is sent to the ocean for simplicity
               !                                                                                                          but salt should remain in the ice except
               !                                                                                                          if all ice is melted. => must be corrected
               ! update remaining mass flux and thickness
               pevap_rema(ji,jj)        = pevap_rema(ji,jj)              + zdum * rhoi
               zh_i      (jk)           = MAX( 0._wp, zh_i(jk)           + zdum )
               h_i       (ji,jj,jl_cat) = MAX( 0._wp,  h_i(ji,jj,jl_cat) + zdum )
               dh_i_sub  (ji,jj)        = dh_i_sub(ji,jj)                + zdum

               ! update heat content (J.m-2), salt content and layer thickness
               zs_i_old(jk) = zs_i_old(jk) + zdum * zs_i(jk)
               ze_i_old(jk) = ze_i_old(jk) + zdum * e_i(ji,jj,jk,jl_cat)
               zh_i_old(jk) = zh_i_old(jk) + zdum
            END DO
            !
            ! remaining "potential" evap is sent to ocean
            wfx_err_sub(ji,jj) = wfx_err_sub(ji,jj) - pevap_rema(ji,jj) * a_i(ji,jj,jl_cat) * r1_Dt_ice  ! <=0 (net evap for the ocean in kg.m-2.s-1)
            !
            !
            ! Surface ice melting
            !--------------------
            DO jk = 1, nlay_i
               !
               ztmelts = - rTmlt * sz_i(ji,jj,jk,jl_cat)   ! Melting point of layer k [C]
               !
               IF( t_i(ji,jj,jk,jl_cat) >= (ztmelts+rt0) ) THEN   !-- Internal melting

                  zEi             = - e_i(ji,jj,jk,jl_cat) * r1_rhoi      ! Specific enthalpy of layer k [J/kg, <0]
                  zdE             =   0._wp                               ! Specific enthalpy difference (J/kg, <0)
                  !                                                       !   set up at 0 since no energy is needed to melt water...(it is already melted)
                  zdum            = MIN( 0._wp , - zh_i(jk) )             ! internal melting occurs when the internal temperature is above freezing
                  !                                                       !   this should normally not happen, but sometimes, heat diffusion leads to this
                  zfmdt           = - zdum * rhoi                         ! Recompute mass flux [kg/m2, >0]
                  !
                  dh_i_itm(ji,jj) = dh_i_itm(ji,jj) + zdum                ! Cumulate internal melting
                  !
                  hfx_res(ji,jj)  = hfx_res(ji,jj) + zEi  * zfmdt           * a_i(ji,jj,jl_cat) * r1_Dt_ice    ! Heat flux to the ocean [W.m-2], <0
                  !                                                                                            !      ice enthalpy zEi is "sent" to the ocean
                  wfx_res(ji,jj)  = wfx_res(ji,jj) - rhoi * zdum            * a_i(ji,jj,jl_cat) * r1_Dt_ice    ! Mass flux
                  sfx_res(ji,jj)  = sfx_res(ji,jj) - rhoi * zdum * zs_i(jk) * a_i(ji,jj,jl_cat) * r1_Dt_ice    ! Salt flux
                  !
               ELSE                                               !-- Surface melting

                  zEi            = - e_i(ji,jj,jk,jl_cat) * r1_rhoi      ! Specific enthalpy of layer k [J/kg, <0]
                  zEw            =    rcp * ztmelts                      ! Specific enthalpy of resulting meltwater [J/kg, <0]
                  zdE            =    zEi - zEw                          ! Specific enthalpy difference < 0

                  zfmdt          = - pq_top(ji,jj) / zdE                 ! Mass flux to the ocean [kg/m2, >0]

                  zdum           = - zfmdt * r1_rhoi                     ! Melt of layer jk [m, <0]

                  zdum           = MIN( 0._wp , MAX( zdum , - zh_i(jk) ) )   ! Melt of layer jk cannot exceed the layer thickness [m, <0]

                  pq_top(ji,jj)  = MAX( 0._wp , pq_top(ji,jj) - zdum * rhoi * zdE ) ! update available heat

                  dh_i_sum_3d(ji,jj,jl_cat) = dh_i_sum_3d(ji,jj,jl_cat) + zdum ! Cumulate surface melt

                  zfmdt          = - rhoi * zdum                         ! Recompute mass flux [kg/m2, >0]

                  zQm            = zfmdt * zEw                           ! Energy of the melt water sent to the ocean [J/m2, <0]

                  hfx_thd(ji,jj) = hfx_thd(ji,jj) + zEw  * zfmdt           * a_i(ji,jj,jl_cat) * r1_Dt_ice    ! Heat flux [W.m-2], < 0
                  hfx_sum(ji,jj) = hfx_sum(ji,jj) - zdE  * zfmdt           * a_i(ji,jj,jl_cat) * r1_Dt_ice    ! Heat flux used in this process [W.m-2], > 0
                  wfx_sum(ji,jj) = wfx_sum(ji,jj) - rhoi * zdum            * a_i(ji,jj,jl_cat) * r1_Dt_ice    ! Mass flux
                  sfx_sum(ji,jj) = sfx_sum(ji,jj) - rhoi * zdum * zs_i(jk) * a_i(ji,jj,jl_cat) * r1_Dt_ice    ! Salt flux >0
                  !
               END IF
               ! update thickness
               zh_i(jk) = MAX( 0._wp, zh_i(jk) + zdum )
               h_i(ji,jj,jl_cat) = MAX( 0._wp, h_i(ji,jj,jl_cat) + zdum )
               !
               ! update heat content (J.m-2), salt content and layer thickness
               zs_i_old(jk) = zs_i_old(jk) + zdum * zs_i(jk)
               ze_i_old(jk) = ze_i_old(jk) + zdum * e_i(ji,jj,jk,jl_cat)
               zh_i_old(jk) = zh_i_old(jk) + zdum
               !
               ! record which layers have disappeared (for bottom melting)
               !    => icount=0 : no layer has vanished
               !    => icount=5 : 5 layers have vanished
               IF( zh_i(jk) > 0._wp ) THEN ; icount(jk) = 0
               ELSE                        ; icount(jk) = 1
               ENDIF

            END DO

            ! Ice Basal growth
            !------------------
            ! Basal growth is driven by heat imbalance at the ice-ocean interface,
            ! between the inner conductive flux  (qcn_ice_bot), from the open water heat flux
            ! (fhld) and the sensible ice-ocean flux (qsb_ice_bot).
            ! qcn_ice_bot is positive downwards. qsb_ice_bot and fhld are positive to the ice
            !
            zs_i_new = 0._wp
            !
            IF(  pf_tt(ji,jj) < 0._wp  ) THEN

               zs_i_new       = zswitch_sal * rn_sinew * sss_m(ji,jj) + ( 1. - zswitch_sal ) * zs_i(1)        ! New ice salinity

               ztmelts        = - rTmlt * zs_i_new                                                            ! New ice melting point (C)

               zt_i_new       = zswitch_sal * t_bo(ji,jj) + ( 1. - zswitch_sal) * t_i(ji,jj, nlay_i,jl_cat)

               zEi            = rcpi * ( zt_i_new - (ztmelts+rt0) ) &                                         ! Specific enthalpy of forming ice (J/kg, <0)
                  &             - rLfus * ( 1.0 - ztmelts / ( MIN( zt_i_new - rt0, -epsi10 ) ) ) + rcp * ztmelts

               zEw            = rcp  * ( t_bo(ji,jj) - rt0 )                                                  ! Specific enthalpy of seawater (J/kg, < 0)

               zdE            = zEi - zEw                                                                     ! Specific enthalpy difference (J/kg, <0)

               dh_i_bog(ji,jj) = rDt_ice * MAX( 0._wp , pf_tt(ji,jj) / ( zdE * rhoi ) )

               ! Contribution to Energy and Salt Fluxes
               zfmdt = - rhoi * dh_i_bog(ji,jj)                                                               ! Mass flux x time step (kg/m2, < 0)

               hfx_thd(ji,jj) = hfx_thd(ji,jj) + zEw  * zfmdt                      * a_i(ji,jj,jl_cat) * r1_Dt_ice   ! Heat flux to the ocean [W.m-2], >0
               hfx_bog(ji,jj) = hfx_bog(ji,jj) - zdE  * zfmdt                      * a_i(ji,jj,jl_cat) * r1_Dt_ice   ! Heat flux used in this process [W.m-2], <0
               wfx_bog(ji,jj) = wfx_bog(ji,jj) - rhoi * dh_i_bog(ji,jj)            * a_i(ji,jj,jl_cat) * r1_Dt_ice   ! Mass flux, <0
               sfx_bog(ji,jj) = sfx_bog(ji,jj) - rhoi * dh_i_bog(ji,jj) * zs_i_new * a_i(ji,jj,jl_cat) * r1_Dt_ice   ! Salt flux, <0

               ! update thickness
               zh_i(nlay_i+1) = zh_i(nlay_i+1) + dh_i_bog(ji,jj)
               h_i(ji,jj,jl_cat)     = h_i(ji,jj,jl_cat) + dh_i_bog(ji,jj)

               ! update heat content (J.m-2), salt content and layer thickness
               zs_i_old(nlay_i+1) = zs_i_old(nlay_i+1) + dh_i_bog(ji,jj) * zs_i_new
               ze_i_old(nlay_i+1) = ze_i_old(nlay_i+1) + dh_i_bog(ji,jj) * (-zEi * rhoi)
               zh_i_old(nlay_i+1) = zh_i_old(nlay_i+1) + dh_i_bog(ji,jj)

            ENDIF

            ! Ice Basal melt
            !---------------
            DO jk = nlay_i, 1, -1
               IF(  pf_tt(ji,jj)  >  0._wp  .AND. jk > icount(jk) ) THEN   ! do not calculate where layer has already disappeared by surface melting

                  ztmelts = - rTmlt * sz_i(ji,jj,jk,jl_cat)  ! Melting point of layer jk (C)

                  IF( t_i(ji,jj,jk,jl_cat) >= (ztmelts+rt0) ) THEN   !-- Internal melting

                     zEi            = - e_i(ji,jj,jk,jl_cat) * r1_rhoi     ! Specific enthalpy of melting ice (J/kg, <0)
                     zdE            = 0._wp                                ! Specific enthalpy difference   (J/kg, <0)
                     !                                                     !   set up at 0 since no energy is needed to melt water...(it is already melted)
                     zdum           = MIN( 0._wp , - zh_i(jk) )            ! internal melting occurs when the internal temperature is above freezing
                     !                                                         this should normally not happen, but sometimes, heat diffusion leads to this
                     dh_i_itm (ji,jj)  = dh_i_itm(ji,jj) + zdum
                     !
                     zfmdt          = - zdum * rhoi                        ! Mass flux x time step > 0
                     !
                     hfx_res(ji,jj) = hfx_res(ji,jj) + zEi  * zfmdt           * a_i(ji,jj,jl_cat) * r1_Dt_ice   ! Heat flux to the ocean [W.m-2], <0
                     !                                                                                              ice enthalpy zEi is "sent" to the ocean
                     wfx_res(ji,jj) = wfx_res(ji,jj) - rhoi * zdum            * a_i(ji,jj,jl_cat) * r1_Dt_ice   ! Mass flux
                     sfx_res(ji,jj) = sfx_res(ji,jj) - rhoi * zdum * zs_i(jk) * a_i(ji,jj,jl_cat) * r1_Dt_ice   ! Salt flux
                     !
                  ELSE                                               !-- Basal melting

                     zEi            = - e_i(ji,jj,jk,jl_cat) * r1_rhoi                ! Specific enthalpy of melting ice (J/kg, <0)
                     zEw            = rcp * ztmelts                                   ! Specific enthalpy of meltwater (J/kg, <0)
                     zdE            = zEi - zEw                                       ! Specific enthalpy difference   (J/kg, <0)

                     zfmdt          = - pq_bot(ji,jj) / zdE                           ! Mass flux x time step (kg/m2, >0)

                     zdum           = - zfmdt * r1_rhoi                               ! Gross thickness change

                     zdum           = MIN( 0._wp , MAX( zdum, - zh_i(jk) ) )          ! bound thickness change

                     pq_bot(ji,jj)  = MAX( 0._wp , pq_bot(ji,jj) - zdum * rhoi * zdE ) ! update available heat. MAX is necessary for roundup errors

                     dh_i_bom(ji,jj)= dh_i_bom(ji,jj) + zdum                          ! Update basal melt

                     zfmdt          = - zdum * rhoi                                   ! Mass flux x time step > 0

                     zQm            = zfmdt * zEw                                     ! Heat exchanged with ocean

                     hfx_thd(ji,jj) = hfx_thd(ji,jj) + zEw  * zfmdt           * a_i(ji,jj,jl_cat) * r1_Dt_ice   ! Heat flux to the ocean [W.m-2], <0
                     hfx_bom(ji,jj) = hfx_bom(ji,jj) - zdE  * zfmdt           * a_i(ji,jj,jl_cat) * r1_Dt_ice   ! Heat used in this process [W.m-2], >0
                     wfx_bom(ji,jj) = wfx_bom(ji,jj) - rhoi * zdum            * a_i(ji,jj,jl_cat) * r1_Dt_ice   ! Mass flux
                     sfx_bom(ji,jj) = sfx_bom(ji,jj) - rhoi * zdum * zs_i(jk) * a_i(ji,jj,jl_cat) * r1_Dt_ice   ! Salt flux
                     !
                  ENDIF
                  ! update thickness
                  zh_i  (jk) = MAX( 0._wp, zh_i  (jk) + zdum )
                  h_i(ji,jj,jl_cat) = MAX( 0._wp, h_i(ji,jj,jl_cat) + zdum )
                  !
                  ! update heat content (J.m-2), salt content and layer thickness
                  zs_i_old(jk) = zs_i_old(jk) + zdum * zs_i(jk)
                  ze_i_old(jk) = ze_i_old(jk) + zdum * e_i(ji,jj,jk,jl_cat)
                  zh_i_old(jk) = zh_i_old(jk) + zdum
               ENDIF
            END DO

            ! Remove snow if ice has melted entirely
            ! --------------------------------------
            IF( h_i(ji,jj,jl_cat) == 0._wp ) THEN
               DO jk = 1, nlay_s
                  ! mass & energy loss to the ocean
                  hfx_res(ji,jj) = hfx_res(ji,jj) - ze_s(jk) * zh_s(jk) * a_i(ji,jj,jl_cat) * r1_Dt_ice  ! heat flux to the ocean [W.m-2], < 0
                  wfx_res(ji,jj) = wfx_res(ji,jj) + rhos     * zh_s(jk) * a_i(ji,jj,jl_cat) * r1_Dt_ice  ! mass flux
                  ! update thickness and energy
                  ze_s  (jk) = 0._wp
                  zh_s  (jk) = 0._wp
               END DO
               ! update thickness
               h_s(ji,jj,jl_cat) = 0._wp
            ENDIF

            ! Snow-Ice formation
            ! ------------------
            ! When snow load exceeds Archimede's limit, snow-ice interface goes down under sea-level,
            ! flooding of seawater transforms snow into ice. Thickness that is transformed is dh_snowice (positive for the ice)
            !
            dh_snowice(ji,jj) = MAX( 0._wp , ( rhos * h_s(ji,jj,jl_cat) + (rhoi-rho0) * h_i(ji,jj,jl_cat) ) * z1_rho )

            h_i(ji,jj,jl_cat) = h_i(ji,jj,jl_cat) + dh_snowice(ji,jj)
            h_s(ji,jj,jl_cat) = h_s(ji,jj,jl_cat) - dh_snowice(ji,jj)

            ! Contribution to energy flux to the ocean [J/m2], >0 (if sst<0)
            zfmdt          = ( rhos - rhoi ) * dh_snowice(ji,jj)    ! <0
            zEw            = rcp * sst_m(ji,jj)
            zQm            = zfmdt * zEw

            hfx_thd(ji,jj) = hfx_thd(ji,jj) + zEw * zfmdt * a_i(ji,jj,jl_cat) * r1_Dt_ice ! Heat flux
            sfx_sni(ji,jj) = sfx_sni(ji,jj) + sss_m(ji,jj) * zfmdt * a_i(ji,jj,jl_cat) * r1_Dt_ice ! Salt flux

            ! Case constant salinity in time: virtual salt flux to keep salinity constant
            IF( nn_icesal == 1 .OR. nn_icesal == 3 )  THEN
               sfx_bri(ji,jj) = sfx_bri(ji,jj) - sss_m(ji,jj) * zfmdt * a_i(ji,jj,jl_cat) * r1_Dt_ice  &  ! put back sss_m     into the ocean
                  &              - zs_i(1) * dh_snowice(ji,jj) * rhoi * a_i(ji,jj,jl_cat) * r1_Dt_ice     ! and get  rn_icesal from the ocean
            ENDIF

            ! Mass flux: All snow is thrown in the ocean, and seawater is taken to replace the volume
            wfx_sni    (ji,jj) = wfx_sni    (ji,jj) - dh_snowice(ji,jj) * rhoi * a_i(ji,jj,jl_cat) * r1_Dt_ice
            wfx_snw_sni(ji,jj) = wfx_snw_sni(ji,jj) + dh_snowice(ji,jj) * rhos * a_i(ji,jj,jl_cat) * r1_Dt_ice

            ! update thickness
            zh_i(0) = zh_i(0) + dh_snowice(ji,jj)
            zdeltah =           dh_snowice(ji,jj)

            ! update heat content (J.m-2), salt content and layer thickness
            zs_i_old(0) = zs_i_old(0) - zfmdt * sss_m(ji,jj) * r1_rhoi      ! clem: s(0) could be > rn_sinew*sss
            zh_i_old(0) = zh_i_old(0) + dh_snowice(ji,jj)
            ze_i_old(0) = ze_i_old(0) + zfmdt * zEw           ! 1st part (sea water enthalpy)

            !
            DO jk = nlay_s, 1, -1   ! flooding of snow starts from the base
               zdum        = MIN( zdeltah, zh_s(jk) )         ! amount of snw that floods, > 0
               zh_s(jk)    = MAX( 0._wp, zh_s(jk) - zdum )    ! remove some snow thickness
               ze_i_old(0) = ze_i_old(0) + zdum * ze_s(jk)    ! 2nd part (snow enthalpy)
               ! update dh_snowice
               zdeltah     = MAX( 0._wp, zdeltah - zdum )
            END DO
            !
            !
!!$      ! --- Update snow diags --- !
!!$      !!clem: this is wrong. dh_s_tot is not used anyway
!!$      DO ji = 1, npti
!!$         dh_s_tot(ji) = dh_s_tot(ji) + dh_s_sum(ji) + zdeltah + zdh_s_sub(ji) - dh_snowice(ji)
!!$      END DO
            !
            ! Remapping of snw enthalpy on a regular grid
            !--------------------------------------------
            CALL snw_var_vremap( zh_s, ze_s, e_s(ji,jj,:,jl_cat) )

            ! recalculate t_s from e_s
            IF( h_s(ji,jj,jl_cat) > 0._wp ) THEN
               DO jk = 1, nlay_s
                  t_s(ji,jj,jk,jl_cat) = rt0 + ( - e_s(ji,jj,jk,jl_cat) * r1_rhos * r1_rcpi + rLfus * r1_rcpi )
               END DO
            ELSE
               DO jk = 1, nlay_s
                  t_s(ji,jj,jk,jl_cat) = rt0
               END DO
            ENDIF

            ! Remapping of ice enthalpy/salt on a regular grid
            !-------------------------------------------------
                                   CALL ice_var_vremap( zh_i_old, ze_i_old, e_i (ji,jj,:,jl_cat) )
            IF( nn_icesal == 4 )   CALL ice_var_vremap( zh_i_old, zs_i_old, sz_i(ji,jj,:,jl_cat) )
            IF( nn_icesal == 2 )   THEN ! Update ice salinity from snow-ice and bottom growth
               zs_sni = sss_m(ji,jj) * ( rhoi - rhos ) * r1_rhoi                                       ! salinity of snow ice
               zds    =       ( zs_sni   - s_i(ji,jj,jl_cat) ) * dh_snowice(ji,jj) / MAX( epsi10, h_i(ji,jj,jl_cat) ) ! snow-ice    
               zds    = zds + ( zs_i_new - s_i(ji,jj,jl_cat) ) * dh_i_bog  (ji,jj) / MAX( epsi10, h_i(ji,jj,jl_cat) ) ! bottom growth
               !
               s_i(ji,jj,jl_cat) = s_i(ji,jj,jl_cat) + zds
            ENDIF
            !
         ENDIF ! l_ice_present
         !
      END_2D
      !                       ! ================== !
      !                       ! End main loop here !
      !                       ! ================== !
      
      ! --- ensure that a_i = 0 & h_s = 0 where h_i = 0 ---
      DO_2D( 0, 0, 0, 0 )
         IF( l_ice_present(ji,jj) .AND. h_i(ji,jj,jl_cat) == 0._wp ) THEN
            a_i (ji,jj,jl_cat) = 0._wp
            h_s (ji,jj,jl_cat) = 0._wp
            t_su(ji,jj,jl_cat) = 0._wp
            l_ice_present(ji,jj) = .FALSE.
         ENDIF
      END_2D

   END SUBROUTINE ice_thd_dh

#else
   !!----------------------------------------------------------------------
   !!   Default option                                NO SI3 sea-ice model
   !!----------------------------------------------------------------------
#endif

   !!======================================================================
END MODULE icethd_dh
