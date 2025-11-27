MODULE snwthd_dh
   !!======================================================================
   !!                       ***  MODULE snwthd_dh ***
   !!   seaice : snow melt
   !!======================================================================
   !! History :  5.x  !  2025-11  (C. Rousset)    Original code 
   !!----------------------------------------------------------------------
#if defined key_si3
   !!----------------------------------------------------------------------
   !!   'key_si3'                                       SI3 sea-ice model
   !!----------------------------------------------------------------------
   !!   snw_thd_dh        : vertical snow melt
   !!----------------------------------------------------------------------
   USE par_ice        ! SI3 parameters
   USE par_kind, ONLY : wp
   USE par_oce
   USE phycst
   USE ice
   USE sbc_ice
   USE icevar  , ONLY : ice_var_snwblow, snw_var_vremap

   IMPLICIT NONE
   PRIVATE

   PUBLIC   snw_thd_dh        ! called by snw_thd

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/ICE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE snw_thd_dh( jl_cat, pq_top )
      !!------------------------------------------------------------------
      !!                ***  ROUTINE snw_thd_dh  ***
      !!
      !! ** Purpose :   compute snow thickness changes due to melting
      !!
      !!------------------------------------------------------------------
      INTEGER  ::   ji, jj, jk       ! dummy loop indices
      INTEGER ,                    INTENT(in   ) ::   jl_cat
      REAL(wp), DIMENSION(A2D(0)), INTENT(inout) ::   pq_top   ! heat for surface ablation (J.m-2)
      !
      REAL(wp) ::   zdum
      !
      REAL(wp), DIMENSION(0:nlay_s) ::   zh_s      ! snw layer thickness (m)
      REAL(wp), DIMENSION(0:nlay_s) ::   ze_s      ! snw layer enthalpy (J.m-3)
      !!------------------------------------------------------------------
      !
      DO_2D( 0, 0, 0, 0 )
         !
         IF( l_ice_present(ji,jj) ) THEN
            !
            ! initialize snw layer thicknesses and enthalpies
            zh_s(0) = 0._wp
            ze_s(0) = 0._wp
            DO jk = 1, nlay_s
               zh_s(jk) = h_s(ji,jj,   jl_cat) * r1_nlay_s
               ze_s(jk) = e_s(ji,jj,jk,jl_cat)
            END DO
            !
            ! Internal melting
            ! ----------------
            ! IF snow temperature is above freezing point, THEN snow melts (should not happen but sometimes it does)
            DO jk = 1, nlay_s
               IF( t_s(ji,jj,jk,jl_cat) > rt0 ) THEN
                  hfx_res    (ji,jj) = hfx_res    (ji,jj) - ze_s(jk) * zh_s(jk) * a_i(ji,jj,jl_cat) * r1_Dt_ice   ! heat flux to the ocean [W.m-2], < 0
                  wfx_snw_sum(ji,jj) = wfx_snw_sum(ji,jj) + rhos     * zh_s(jk) * a_i(ji,jj,jl_cat) * r1_Dt_ice   ! mass flux
                  ! updates
                  dh_s_itm(ji,jj) = dh_s_itm(ji,jj) - zh_s(jk)
                  h_s(ji,jj,jl_cat) = MAX( 0._wp, h_s(ji,jj,jl_cat) - zh_s(jk) )
                  zh_s(jk) = 0._wp
                  ze_s(jk) = 0._wp
               END IF
            END DO

            ! Snow melting
            ! ------------
            ! If heat available (pq_top > 0) then we need to melt snow before sea ice
            DO jk = 1, nlay_s
               IF( zh_s(jk) > 0._wp .AND. pq_top(ji,jj) > 0._wp ) THEN
                  !
                  zdum = - pq_top(ji,jj) / MAX( ze_s(jk), epsi20 )   ! thickness change
                  zdum = MAX( zdum , - zh_s(jk) )                    ! bound melting

                  hfx_snw    (ji,jj) = hfx_snw    (ji,jj) - ze_s(jk) * zdum * a_i(ji,jj,jl_cat) * r1_Dt_ice   ! heat used to melt snow(W.m-2, >0)
                  wfx_snw_sum(ji,jj) = wfx_snw_sum(ji,jj) - rhos     * zdum * a_i(ji,jj,jl_cat) * r1_Dt_ice   ! snow melting only = water into the ocean

                  ! updates available heat + thickness
                  dh_s_sum_3d(ji,jj,jl_cat) = dh_s_sum_3d(ji,jj,jl_cat)       + zdum
                  pq_top(ji,jj)             = MAX( 0._wp , pq_top(ji,jj)      + zdum * ze_s(jk) )
                  h_s(ji,jj,jl_cat)         = MAX( 0._wp ,  h_s(ji,jj,jl_cat) + zdum )
                  zh_s(jk)                  = MAX( 0._wp , zh_s(jk)           + zdum )
                  !
               ENDIF
            END DO

            ! Remapping of snw enthalpy on a regular grid
            !--------------------------------------------
            CALL snw_var_vremap( zh_s, ze_s, e_s(ji,jj,:,jl_cat) )
            !
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
            !
         ENDIF ! l_ice_present
         !
      END_2D

   END SUBROUTINE snw_thd_dh
   
#else
   !!----------------------------------------------------------------------
   !!   Default option                                NO SI3 sea-ice model
   !!----------------------------------------------------------------------
#endif

   !!======================================================================
END MODULE snwthd_dh
