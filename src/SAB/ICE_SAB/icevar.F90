MODULE icevar
   !!======================================================================
   !!                       ***  MODULE icevar ***
   !!   sea-ice:  series of functions to transform or compute ice variables
   !!======================================================================
   !! History :   -   !  2006-01  (M. Vancoppenolle) Original code
   !!            4.0  !  2018     (many people)      SI3 [aka Sea Ice cube]
   !!----------------------------------------------------------------------
#if defined key_si3
   !!----------------------------------------------------------------------
   !!   'key_si3'                                       SI3 sea-ice model
   !!----------------------------------------------------------------------
   !!   ice_var_sshdyn    : compute equivalent ssh in lead
   !!----------------------------------------------------------------------
   !USE par_ice        ! SI3 parameters
   USE phycst         ! physical constants (ocean directory)
   USE sbc_oce , ONLY : sss_m, sst_m, ln_ice_embd, nn_fsbc
   USE ice            ! sea-ice: variables
   !USE ice1D          ! sea-ice: thermodynamics variables
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   ice_var_sshdyn

   !! * Substitutions
#  include "do_loop_substitute.h90"

   !!----------------------------------------------------------------------
   !! NEMO/ICE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS


   FUNCTION ice_var_sshdyn(pssh, psnwice_mass, psnwice_mass_b)
      !!---------------------------------------------------------------------
      !!                   ***  ROUTINE ice_var_sshdyn  ***
      !!
      !! ** Purpose :  compute the equivalent ssh in lead when sea ice is embedded
      !!
      !! ** Method  :  ssh_lead = ssh + (Mice + Msnow) / rho0
      !!
      !! ** Reference : Jean-Michel Campin, John Marshall, David Ferreira,
      !!                Sea ice-ocean coupling using a rescaled vertical coordinate z*,
      !!                Ocean Modelling, Volume 24, Issues 1-2, 2008
      !!----------------------------------------------------------------------
      !
      ! input
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pssh            !: ssh [m]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: psnwice_mass    !: mass of snow and ice at current  ice time step [Kg/m2]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: psnwice_mass_b  !: mass of snow and ice at previous ice time step [Kg/m2]
      !
      ! output
      REAL(wp), DIMENSION(jpi,jpj) :: ice_var_sshdyn  ! equivalent ssh in lead [m]
      !
      ! temporary
      REAL(wp) :: zintn, zintb                     ! time interpolation weights []
      !
      ! compute ice load used to define the equivalent ssh in lead
      IF( ln_ice_embd ) THEN
         !
         ! average interpolation coeff as used in dynspg = (1/nn_fsbc)   * {SUM[n/nn_fsbc], n=0,nn_fsbc-1}
         !                                               = (1/nn_fsbc)^2 * {SUM[n]        , n=0,nn_fsbc-1}
         zintn = REAL( nn_fsbc - 1 ) / REAL( nn_fsbc ) * 0.5_wp
         !
         ! average interpolation coeff as used in dynspg = (1/nn_fsbc)   *    {SUM[1-n/nn_fsbc], n=0,nn_fsbc-1}
         !                                               = (1/nn_fsbc)^2 * (nn_fsbc^2 - {SUM[n], n=0,nn_fsbc-1})
         zintb = REAL( nn_fsbc + 1 ) / REAL( nn_fsbc ) * 0.5_wp
         !
         ! compute equivalent ssh in lead
         ice_var_sshdyn(:,:) = pssh(:,:) + ( zintn * psnwice_mass(:,:) + zintb * psnwice_mass_b(:,:) ) * r1_rho0
         !
      ELSE
         ! compute equivalent ssh in lead
         ice_var_sshdyn(:,:) = pssh(:,:)
      ENDIF
      !
   END FUNCTION ice_var_sshdyn



#else
   !!----------------------------------------------------------------------
   !!   Default option         Dummy module           NO SI3 sea-ice model
   !!----------------------------------------------------------------------
#endif

   !!======================================================================
END MODULE icevar
