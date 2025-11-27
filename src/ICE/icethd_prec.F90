MODULE snwthd_prec
   !!======================================================================
   !!                       ***  MODULE snwthd_prec ***
   !!   seaice : snow precipitation, sublimation and deposition
   !!======================================================================
   !! History :  5.x  !  2025-11  (C. Rousset)    Original code 
   !!----------------------------------------------------------------------
#if defined key_si3
   !!----------------------------------------------------------------------
   !!   'key_si3'                                       SI3 sea-ice model
   !!----------------------------------------------------------------------
   !!   snw_thd_prec        : vertical snow precip/sublimation
   !!----------------------------------------------------------------------
   USE par_ice        ! SI3 parameters
   USE par_kind, ONLY : wp
   USE par_oce
   USE sbc_oce , ONLY : sprecip
   USE phycst
   USE ice
   USE sbc_ice
   USE icevar  , ONLY : ice_var_snwblow, snw_var_vremap

   IMPLICIT NONE
   PRIVATE

   PUBLIC   snw_thd_prec        ! called by ice_thd

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/ICE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE snw_thd_prec( jl_cat, pevap_rema )
      !!------------------------------------------------------------------
      !!                ***  ROUTINE snw_thd_prec  ***
      !!
      !! ** Purpose :   compute snow thickness changes due to precipitation and sublimation
      !!
      !! ** Method  :   Snow thickness can increase by precipitation and deposition
      !!                               and decrease by sublimation
      !!
      !!------------------------------------------------------------------
      INTEGER  ::   ji, jj, jk       ! dummy loop indices
      INTEGER ,                    INTENT(in   ) ::   jl_cat
      REAL(wp), DIMENSION(A2D(0)), INTENT(inout) ::   pevap_rema   ! remaining of evaporation after snow sublimation (in kg/m2)
      !
      REAL(wp) ::   zdum, zdeltah
      !
      REAL(wp), DIMENSION(A2D(0))   ::   zsnw      ! distribution of snow after wind blowing
      REAL(wp), DIMENSION(0:nlay_s) ::   zh_s      ! snw layer thickness (m)
      REAL(wp), DIMENSION(0:nlay_s) ::   ze_s      ! snw layer enthalpy (J.m-3)
      !!------------------------------------------------------------------
      !
      ! snow distribution over ice after wind blowing
      CALL ice_var_snwblow( 1._wp - at_i(A2D(0)), zsnw(:,:) )
      !
      !                       ! ==================== !
      !                       ! Start main loop here !
      !                       ! ==================== !
      DO_2D( 0, 0, 0, 0 )
         !
         IF( l_ice_present(ji,jj) ) THEN
            !
            ! initialize snw layer thicknesses and enthalpies
            zh_s(0) = 0._wp
            ze_s(0) = 0._wp
            DO jk = 1, nlay_s
               zh_s(jk) = h_s(ji,jj,jl_cat) * r1_nlay_s
               ze_s(jk) = e_s(ji,jj,jk,jl_cat)
            END DO
            !
            ! Snow precipitation
            !-------------------
            IF( sprecip(ji,jj) > 0._wp ) THEN
               zh_s(0) = zsnw(ji,jj) * sprecip(ji,jj) * rDt_ice * r1_rhos / at_i(ji,jj)   ! thickness of precip
               ze_s(0) = MAX( 0._wp, - qprec_ice(ji,jj) )                                 ! enthalpy of the precip (>0, J.m-3)
               !
               hfx_spr(ji,jj) = hfx_spr(ji,jj) + ze_s(0) * zh_s(0) * a_i(ji,jj,jl_cat) * r1_Dt_ice   ! heat flux from snow precip (>0, W.m-2)
               wfx_spr(ji,jj) = wfx_spr(ji,jj) - rhos    * zh_s(0) * a_i(ji,jj,jl_cat) * r1_Dt_ice   ! mass flux, <0
               !
               ! update thickness
               h_s(ji,jj,jl_cat) = h_s(ji,jj,jl_cat) + zh_s(0)
            ENDIF
            !
            ! Snow sublimation and deposition
            !--------------------------------
            ! if qla_ice is >=0 (upwards), heat goes to the atmosphere, therefore snow sublimates
            ! else                       , there is snow deposition
            !    comment: not counted in mass/heat exchange in iceupdate.F90 since this is an exchange with atm. (not ocean)
            zdeltah           = MAX( - evap_ice(ji,jj,jl_cat) * r1_rhos * rDt_ice, - h_s(ji,jj,jl_cat) )   ! amount of snw that sublimates (<0) or deposition (>0)
            pevap_rema(ji,jj) =        evap_ice(ji,jj,jl_cat)           * rDt_ice + zdeltah * rhos         ! remaining evap in kg.m-2 (used for ice sublimation later on)
            IF( zdeltah > 0._wp .AND. ze_s(0) == 0._wp ) &
               &                        ze_s(0) = rhos * ( rLfus - rcpi * ( t_su(ji,jj,jl_cat) - rt0 ) )   ! if snow deposition and no snow precip, then estimate ze_s(0) with t_su
            !
            DO jk = 0, nlay_s
               zdum = MAX( -zh_s(jk), zdeltah ) ! snow layer thickness that sublimates (<0) or deposits (>0)
               !
               hfx_sub    (ji,jj) = hfx_sub    (ji,jj) + ze_s(jk) * zdum * a_i(ji,jj,jl_cat) * r1_Dt_ice  ! Heat flux of snw that sublimates/deposits [W.m-2], <0 or >0
               wfx_snw_sub(ji,jj) = wfx_snw_sub(ji,jj) - rhos     * zdum * a_i(ji,jj,jl_cat) * r1_Dt_ice  ! Mass flux by sublimation or deposition
               
               ! update thickness
               h_s (ji,jj,jl_cat) = MAX( 0._wp ,  h_s(ji,jj,jl_cat) + zdum )
               zh_s(jk)           = MAX( 0._wp , zh_s(jk)           + zdum )
               
               ! update sublimation left (if any)
               zdeltah = MIN( zdeltah - zdum, 0._wp )
            END DO
            !
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

   END SUBROUTINE snw_thd_prec
   
#else
   !!----------------------------------------------------------------------
   !!   Default option                                NO SI3 sea-ice model
   !!----------------------------------------------------------------------
#endif

   !!======================================================================
END MODULE snwthd_prec
